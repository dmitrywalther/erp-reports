#!/usr/bin/env bash
# Ephemeral single-container demo. Never connect to an externally configured DB.
set -Eeuo pipefail
umask 077

if [[ "$(id -u)" == 0 ]]; then
  echo "The online demo must run as a non-root user." >&2
  exit 1
fi

APP_ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd -P)"
cd "$APP_ROOT"
export PORT="${PORT:-3838}"
if [[ ! "$PORT" =~ ^[0-9]{1,5}$ ]] || (( 10#$PORT < 1024 || 10#$PORT > 65535 )); then
  echo "PORT must be an unprivileged TCP port (1024..65535)." >&2
  exit 1
fi

# pg_config can point to client-only binaries; require the actual server tools.
PG_BIN=""
if command -v pg_config >/dev/null 2>&1; then
  candidate="$(pg_config --bindir)"
  if [[ -x "$candidate/postgres" && -x "$candidate/initdb" ]]; then PG_BIN="$candidate"; fi
fi
if [[ -z "$PG_BIN" ]]; then
  for candidate in /usr/lib/postgresql/*/bin; do
    if [[ -x "$candidate/postgres" && -x "$candidate/initdb" ]]; then PG_BIN="$candidate"; fi
  done
fi
if [[ -z "$PG_BIN" ]]; then echo "PostgreSQL server binaries are missing." >&2; exit 1; fi
for tool in postgres initdb pg_ctl pg_isready psql createdb; do
  if [[ ! -x "$PG_BIN/$tool" ]]; then echo "Required PostgreSQL tool is missing: $tool" >&2; exit 1; fi
done
command -v Rscript >/dev/null 2>&1 || { echo "Rscript is missing." >&2; exit 1; }

runtime_dir=""
pg_pid=""
app_pid=""
seed_pid=""
cleanup() {
  status=$?
  trap - EXIT INT TERM
  if [[ -n "$runtime_dir" ]]; then rm -f -- "$runtime_dir/health/ready"; fi
  for pid in "$app_pid" "$seed_pid"; do
    if [[ -n "$pid" ]] && kill -0 "$pid" 2>/dev/null; then kill -TERM "$pid" 2>/dev/null || true; fi
  done
  if [[ -n "$pg_pid" ]] && kill -0 "$pg_pid" 2>/dev/null; then
    "$PG_BIN/pg_ctl" -D "$runtime_dir/data" -m fast -w -t 10 stop >/dev/null 2>&1 ||
      "$PG_BIN/pg_ctl" -D "$runtime_dir/data" -m immediate -w -t 5 stop >/dev/null 2>&1 || true
  fi
  # Do not let an unresponsive R worker hold container shutdown indefinitely.
  for attempt in {1..5}; do
    running=false
    for pid in "$app_pid" "$seed_pid" "$pg_pid"; do
      if [[ -n "$pid" ]] && kill -0 "$pid" 2>/dev/null; then running=true; fi
    done
    if [[ "$running" == false ]]; then break; fi
    sleep 1
  done
  for pid in "$app_pid" "$seed_pid" "$pg_pid"; do
    if [[ -n "$pid" ]] && kill -0 "$pid" 2>/dev/null; then kill -KILL "$pid" 2>/dev/null || true; fi
    if [[ -n "$pid" ]]; then wait "$pid" 2>/dev/null || true; fi
  done
  # Only the private directory created by this boot is disposable.
  if [[ "$runtime_dir" =~ ^/tmp/erp-reports-online\.[[:alnum:]]{6}$ && -d "$runtime_dir" && ! -L "$runtime_dir" ]]; then
    resolved_runtime="$(realpath -e -- "$runtime_dir")"
    if [[ "$resolved_runtime" == "$runtime_dir" ]]; then rm -rf -- "$resolved_runtime"; fi
  fi
  exit "$status"
}
trap cleanup EXIT
trap 'exit 143' TERM
trap 'exit 130' INT

runtime_dir="$(mktemp -d /tmp/erp-reports-online.XXXXXX)"
if [[ ! "$runtime_dir" =~ ^/tmp/erp-reports-online\.[[:alnum:]]{6}$ || "$(realpath -e -- "$runtime_dir")" != "$runtime_dir" ]]; then
  echo "The runtime directory must resolve to the private /tmp directory." >&2
  exit 1
fi
mkdir -m 700 "$runtime_dir/data" "$runtime_dir/socket" "$runtime_dir/tmp" "$runtime_dir/health"
export ERP_REPORTS_RUNTIME_DIR="$runtime_dir" TMPDIR="$runtime_dir/tmp"
export ERP_REPORTS_DEMO=true ERP_REPORTS_ONLINE=true
export REPORTS_ELASTIC_URL=http://elastic-stub ERP_BASE_URL="" SPOT_TEAM_CREATORS=""
export TZ=Europe/Moscow PGCLIENTENCODING=UTF8 PGCONNECT_TIMEOUT=5
unset R_PROFILE R_PROFILE_USER R_ENVIRON R_ENVIRON_USER R_LIBS R_LIBS_USER R_CONFIG_ACTIVE R_CONFIG_FILE
unset PGSERVICE PGSERVICEFILE PGPASSFILE PGHOST PGHOSTADDR PGPORT PGUSER PGDATABASE PGPASSWORD PGOPTIONS
export PGHOST="$runtime_dir/socket" PGPORT=5432 PGUSER=erp_bootstrap PGDATABASE=postgres

set_report_connections() {
  local user="$1" host="$2" pass="$3" service
  for service in ERP LOADBOARD PRIVATE_AUTH PROFILES; do
    export "REPORTS_${service}_PG_HOST=$host"
    export "REPORTS_${service}_PG_PORT=5432"
    export "REPORTS_${service}_PG_USER=$user"
    export "REPORTS_${service}_PG_PASSWORD=$pass"
    export "REPORTS_${service}_PG_DB=erp_demo"
  done
}

echo "Initializing a private synthetic PostgreSQL database..."
"$PG_BIN/initdb" -D "$runtime_dir/data" --username=erp_bootstrap --encoding=UTF8 \
  --locale=C.UTF-8 --auth-local=trust --auth-host=scram-sha-256 >/dev/null
"$PG_BIN/postgres" -D "$runtime_dir/data" \
  -c listen_addresses=127.0.0.1 -c port=5432 \
  -c "unix_socket_directories=$runtime_dir/socket" -c unix_socket_permissions=0700 \
  -c timezone=UTC -c shared_buffers=16MB -c work_mem=1MB -c maintenance_work_mem=16MB \
  -c max_connections=32 -c max_parallel_workers_per_gather=0 -c max_parallel_workers=0 \
  -c password_encryption=scram-sha-256 >"$runtime_dir/postgres.log" 2>&1 &
pg_pid=$!

pg_ready=false
for attempt in {1..60}; do
  if ! kill -0 "$pg_pid" 2>/dev/null; then echo "PostgreSQL exited during startup." >&2; exit 1; fi
  if "$PG_BIN/pg_isready" -q -h "$runtime_dir/socket" -p 5432 -U erp_bootstrap -d postgres; then
    pg_ready=true
    break
  fi
  sleep 1
done
if [[ "$pg_ready" != true ]]; then echo "PostgreSQL startup timed out." >&2; exit 1; fi

"$PG_BIN/createdb" --maintenance-db=postgres erp_demo
export PGDATABASE=erp_demo
"$PG_BIN/psql" --no-psqlrc --quiet --set=ON_ERROR_STOP=1 -f demo/schema.sql
set_report_connections erp_bootstrap "$runtime_dir/socket" ""
Rscript --vanilla -e "source('demo/seed_demo.R', encoding='UTF-8')" &
seed_pid=$!
wait "$seed_pid"
seed_pid=""

# This password is generated afresh and never written to the public package/logs.
reader_password=$(od -An -N32 -tx1 /dev/urandom | tr -d ' \n')
"$PG_BIN/psql" --no-psqlrc --quiet --set=ON_ERROR_STOP=1 \
  --set="reader_password=$reader_password" <<'SQL'
CREATE ROLE erp_demo_reader LOGIN NOINHERIT NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION PASSWORD :'reader_password';
REVOKE CREATE, TEMPORARY ON DATABASE erp_demo FROM PUBLIC;
REVOKE CREATE ON SCHEMA public FROM PUBLIC;
GRANT CONNECT ON DATABASE erp_demo TO erp_demo_reader;
DO $$
DECLARE item record;
BEGIN
  FOR item IN SELECT nspname FROM pg_namespace
              WHERE nspname NOT LIKE 'pg_%' AND nspname <> 'information_schema'
  LOOP
    EXECUTE format('GRANT USAGE ON SCHEMA %I TO erp_demo_reader', item.nspname);
    EXECUTE format('GRANT SELECT ON ALL TABLES IN SCHEMA %I TO erp_demo_reader', item.nspname);
  END LOOP;
END $$;
ALTER ROLE erp_demo_reader SET default_transaction_read_only = on;
ALTER ROLE erp_demo_reader SET statement_timeout = '30s';
ALTER ROLE erp_demo_reader SET lock_timeout = '5s';
ALTER ROLE erp_demo_reader SET idle_in_transaction_session_timeout = '30s';
ALTER ROLE erp_bootstrap NOLOGIN;
SQL

# Bootstrap trust is no longer needed. Only the read-only login remains usable.
cat > "$runtime_dir/data/pg_hba.conf" <<'HBA'
local erp_demo erp_demo_reader scram-sha-256
host erp_demo erp_demo_reader 127.0.0.1/32 scram-sha-256
HBA
"$PG_BIN/pg_ctl" -D "$runtime_dir/data" reload >/dev/null

set_report_connections erp_demo_reader 127.0.0.1 "$reader_password"
export PGHOST=127.0.0.1 PGUSER=erp_demo_reader PGPASSWORD="$reader_password"
export PGOPTIONS="-c default_transaction_read_only=on -c statement_timeout=30000 -c lock_timeout=5000"
unset reader_password
echo "Synthetic database ready; starting Shiny on PORT=$PORT."
Rscript --vanilla demo/online/run_app.R &
app_pid=$!

# A broken database must not leave a seemingly healthy web process behind.
set +e
wait -n "$pg_pid" "$app_pid"
status=$?
set -e
if [[ "$status" == 0 ]]; then status=1; fi
echo "A required runtime process stopped; shutting down the demo." >&2
exit "$status"
