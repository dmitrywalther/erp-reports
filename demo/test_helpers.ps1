# Read-only diagnostic run against the existing local demo database.
# Does not start services, seed data, or use production connection settings.
param(
  [ValidateRange(1, 3650)][int]$Days = 60,
  [string]$RHome = "$env:LOCALAPPDATA\Programs\R\R-4.0.4",
  [string]$RLib = "$env:LOCALAPPDATA\R\erp-reports-lib"
)

$ErrorActionPreference = "Stop"
$rscript = Join-Path $RHome "bin\Rscript.exe"
if (-not (Test-Path -LiteralPath $rscript)) { throw "Rscript was not found: $rscript" }

# Windows R 4.0 cannot use the C.UTF-8 locale inherited from some terminals.
$savedEnvironment = @{}
foreach ($name in @("LANG", "LC_ALL", "LC_CTYPE", "R_LIBS_USER")) {
  $savedEnvironment[$name] = [Environment]::GetEnvironmentVariable($name, "Process")
}
try {
  $env:LANG = "Russian_Russia.1251"
  $env:LC_ALL = "Russian_Russia.1251"
  $env:LC_CTYPE = "Russian_Russia.1251"
  $env:R_LIBS_USER = $RLib
  & $rscript --vanilla (Join-Path $PSScriptRoot "test_helpers.R") "--days=$Days"
  $testExitCode = $LASTEXITCODE
} finally {
  foreach ($name in $savedEnvironment.Keys) {
    [Environment]::SetEnvironmentVariable($name, $savedEnvironment[$name], "Process")
  }
}
exit $testExitCode
