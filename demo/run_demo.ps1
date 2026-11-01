# Запуск приложения в демо-режиме (Windows, PowerShell).
#
# Предполагается:
#   * R 4.0.x установлен (путь ниже) и пакеты стоят в библиотеке $RLib (см. demo/install_packages.R);
#   * PostgreSQL запущен на 127.0.0.1:5433 (портативные бинарники или Docker),
#     база erp_demo создана, применена demo/schema.sql и выполнен demo/seed_demo.R.
#
# Использование:
#   .\demo\run_demo.ps1                 # запустить приложение на http://127.0.0.1:3838
#   .\demo\run_demo.ps1 -StartPg        # сначала поднять портативный PostgreSQL (см. README)
#   .\demo\run_demo.ps1 -Seed           # пересоздать схему и данные, затем запустить
#   .\demo\run_demo.ps1 -Port 4000
param(
  [int]$Port = 3838,
  [switch]$Seed,
  [switch]$StartPg,   # поднять портативный PostgreSQL из $PgBin, если порт не слушается
  [string]$RHome = "$env:LOCALAPPDATA\Programs\R\R-4.0.4",
  [string]$RLib = "$env:LOCALAPPDATA\R\erp-reports-lib",
  [string]$PgBin = "$env:LOCALAPPDATA\pgsql16\pgsql\bin",
  [string]$PgHost = "127.0.0.1",
  [string]$PgPort = "5433",
  [string]$PgUser = "postgres",
  [string]$PgPassword = "",
  [string]$PgDb = "erp_demo"
)

$ErrorActionPreference = "Stop"
Set-Location (Split-Path $PSScriptRoot -Parent)

$env:ERP_REPORTS_DEMO = "true"
foreach ($svc in @("ERP", "LOADBOARD", "PRIVATE_AUTH", "PROFILES")) {
  Set-Item "env:REPORTS_${svc}_PG_HOST" $PgHost
  Set-Item "env:REPORTS_${svc}_PG_PORT" $PgPort
  Set-Item "env:REPORTS_${svc}_PG_USER" $PgUser
  Set-Item "env:REPORTS_${svc}_PG_PASSWORD" $PgPassword
  Set-Item "env:REPORTS_${svc}_PG_DB" $PgDb
}
$env:REPORTS_ELASTIC_URL = "http://elastic-stub"
$env:R_LIBS_USER = $RLib
$env:PGCLIENTENCODING = "UTF8"
# R 4.0 for Windows predates native UTF-8 locales. C.UTF-8 inherited from
# developer terminals falls back to C and breaks source() on Cyrillic text.
$env:LANG = "Russian_Russia.1251"
$env:LC_ALL = "Russian_Russia.1251"
$env:LC_CTYPE = "Russian_Russia.1251"
$rscript = Join-Path $RHome "bin\Rscript.exe"
if (-not (Test-Path -LiteralPath $rscript -PathType Leaf)) {
  throw "Rscript не найден: $rscript. Укажите -RHome (см. README)."
}

if ($StartPg) {
  $pgData = Join-Path (Split-Path $PgBin -Parent) "..\data"
  if (-not (Test-NetConnection -ComputerName $PgHost -Port $PgPort -InformationLevel Quiet -WarningAction SilentlyContinue)) {
    Write-Host "Запускаю PostgreSQL ($pgData) на порту $PgPort..."
    & (Join-Path $PgBin "pg_ctl.exe") -D $pgData -o "-p $PgPort -c listen_addresses=$PgHost -c timezone=UTC" -l (Join-Path $pgData "..\server.log") -w start
    if ($LASTEXITCODE -ne 0) { throw "Не удалось запустить PostgreSQL (код $LASTEXITCODE)." }
  }
}

if ($Seed) {
  Write-Host "Применяю demo/schema.sql и генерирую данные..."
  $previousPgPassword = [Environment]::GetEnvironmentVariable('PGPASSWORD', 'Process')
  try {
    $env:PGPASSWORD = $PgPassword
    & (Join-Path $PgBin "psql.exe") -h $PgHost -p $PgPort -U $PgUser -d $PgDb -w -v ON_ERROR_STOP=1 -q -f "demo/schema.sql"
    if ($LASTEXITCODE -ne 0) { throw "Не удалось применить демо-схему (код $LASTEXITCODE)." }
  } finally {
    [Environment]::SetEnvironmentVariable('PGPASSWORD', $previousPgPassword, 'Process')
  }
  & $rscript --vanilla -e ".libPaths(c(Sys.getenv('R_LIBS_USER'), .libPaths())); source('demo/seed_demo.R', encoding = 'UTF-8')"
  if ($LASTEXITCODE -ne 0) { throw "Не удалось наполнить демо-БД (код $LASTEXITCODE)." }
}

Write-Host "Приложение: http://127.0.0.1:$Port  (логин demo / demo)"
& $rscript --vanilla -e ".libPaths(c(Sys.getenv('R_LIBS_USER'), .libPaths())); shiny::runApp('.', port = $Port, host = '127.0.0.1', launch.browser = FALSE)"
if ($LASTEXITCODE -ne 0) { throw "Приложение завершилось с ошибкой (код $LASTEXITCODE)." }
