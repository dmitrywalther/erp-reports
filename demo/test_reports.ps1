# Regression checks do not change data. -WithDatabase also checks all loaders
# against the existing local erp_demo database in read-only sessions.
param(
  [switch]$WithDatabase,
  [string]$RHome = "$env:LOCALAPPDATA\Programs\R\R-4.0.4",
  [string]$RLib = "$env:LOCALAPPDATA\R\erp-reports-lib"
)
$ErrorActionPreference = 'Stop'
Set-Location (Split-Path $PSScriptRoot -Parent)
$env:LANG = 'Russian_Russia.1251'
$env:LC_ALL = 'Russian_Russia.1251'
$env:LC_CTYPE = 'Russian_Russia.1251'
$env:R_LIBS_USER = $RLib
$rscript = Join-Path $RHome 'bin\Rscript.exe'
foreach ($testName in @('test_report_ratios.R', 'test_calendar.R', 'test_kpi.R', 'test_spot.R', 'test_unformed_bills.R', 'test_seed_lifecycle.R', 'test_doublets.R', 'test_public_config.R', 'test_public_support.R', 'test_session_isolation.R', 'test_online_runtime.R')) {
  Write-Host "Checking $testName..."
  & $rscript --vanilla -e "source('demo/$testName', encoding='UTF-8')"
  if ($LASTEXITCODE -ne 0) { throw "$testName failed (exit $LASTEXITCODE)." }
}
if ($WithDatabase) {
  & $rscript --vanilla 'demo/test_helpers.R'
  if ($LASTEXITCODE -ne 0) { throw "Loader checks failed (exit $LASTEXITCODE)." }
}
Write-Host 'All requested checks passed.'
