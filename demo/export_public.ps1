# Build a new distributable from explicit files; never copy the source Git history.
[CmdletBinding()]
param()
$ErrorActionPreference = 'Stop'
$repoRoot = [IO.Path]::GetFullPath((Split-Path $PSScriptRoot -Parent)).TrimEnd([char[]]'\/')
$dist = Join-Path $repoRoot 'dist'
$output = Join-Path $dist 'erp-reports-public'
$archive = Join-Path $dist 'erp-reports-public.zip'
$archiveHash = "$archive.sha256"
$buildId = [Guid]::NewGuid().ToString('N')
$staging = Join-Path $dist ".erp-reports-public-build-$buildId"
$stagingZip = "$staging.zip"
$manifestName = 'PUBLIC_EXPORT_MANIFEST.json'
$utf8 = [Text.UTF8Encoding]::new($false)

function Assert-PlainTree([string]$Directory) {
  if ((Get-Item -LiteralPath $Directory -Force).Attributes -band [IO.FileAttributes]::ReparsePoint) { throw 'Refusing a linked output root.' }
  $all = @((Get-Item -LiteralPath $Directory -Force)) + @(Get-ChildItem -LiteralPath $Directory -Recurse -Force)
  if ($all | Where-Object { $_.Attributes -band [IO.FileAttributes]::ReparsePoint }) { throw 'Refusing a linked/reparse-point tree.' }
}
function Assert-OutputPath([string]$Directory) {
  $absolute = [IO.Path]::GetFullPath($Directory).TrimEnd([char[]]'\/')
  if ($absolute -ne $output -and $absolute -ne $staging) { throw 'Refusing to remove an unexpected path.' }
  if ([IO.Path]::GetDirectoryName($absolute) -ne $dist) { throw 'Output escaped the project dist directory.' }
  Assert-PlainTree $absolute
}
function Assert-ManagedOutput {
  Assert-OutputPath $output
  $previous = Join-Path $output $manifestName
  if (-not (Test-Path -LiteralPath $previous -PathType Leaf)) { throw 'Existing output has no managed manifest; preserve it and move it aside manually.' }
  $manifest = Get-Content -LiteralPath $previous -Raw | ConvertFrom-Json
  if ($manifest.format -ne 'erp-reports-public/v1') { throw 'Existing output is not managed by this exporter.' }
  $existing = @(Get-ChildItem -LiteralPath $output -Recurse -Force -File | Where-Object { $_.FullName -ne $previous })
  if ($existing.Count -ne @($manifest.files).Count) { throw 'Existing output has added/missing files; preserve it and move it aside manually.' }
  foreach ($directory in Get-ChildItem -LiteralPath $output -Recurse -Force -Directory) {
    $relativeDirectory = $directory.FullName.Substring($output.Length + 1).Replace('\', '/') + '/'
    if (-not @($manifest.files | Where-Object { $_.path.StartsWith($relativeDirectory) }).Count) { throw 'Existing output has an added directory; preserve it and move it aside manually.' }
  }
  foreach ($file in $existing) {
    $relative = $file.FullName.Substring($output.Length + 1).Replace('\', '/')
    $entry = @($manifest.files | Where-Object { $_.path -eq $relative })
    if ($entry.Count -ne 1 -or $file.Length -ne $entry[0].bytes -or (Get-FileHash -LiteralPath $file.FullName -Algorithm SHA256).Hash -ne $entry[0].sha256) {
      throw "Existing output was changed: $relative. Preserve it and move it aside manually."
    }
  }
}

$allowed = @(Get-Content -LiteralPath (Join-Path $PSScriptRoot 'public_files.txt') | Where-Object { $_ -and -not $_.StartsWith('#') })
if (@($allowed | Select-Object -Unique).Count -ne $allowed.Count) { throw 'Duplicate allowlist entry.' }
foreach ($relative in $allowed) {
  if ($relative -match '(^|/)\.\.?(/|$)|[:\\]' -or [IO.Path]::IsPathRooted($relative)) { throw 'Invalid allowlist path.' }
  $source = Join-Path $repoRoot $relative
  if (-not (Test-Path -LiteralPath $source -PathType Leaf)) { throw "Required public file missing: $relative" }
  $item = Get-Item -LiteralPath $source -Force
  while ($item.FullName -ne $repoRoot) {
    if ($item.Attributes -band [IO.FileAttributes]::ReparsePoint) { throw "Refusing linked source: $relative" }
    $item = if ($item -is [IO.DirectoryInfo]) { $item.Parent } else { $item.Directory }
    if ($null -eq $item) { throw 'Source escaped the repository.' }
  }
}
if (Test-Path -LiteralPath $dist) {
  if ((Get-Item -LiteralPath $dist -Force).Attributes -band [IO.FileAttributes]::ReparsePoint) { throw 'dist must not be a link.' }
} else { New-Item -ItemType Directory -Path $dist | Out-Null }
if (Test-Path -LiteralPath $output) { Assert-ManagedOutput }
if ((Test-Path -LiteralPath $archive) -or (Test-Path -LiteralPath $archiveHash)) {
  if (-not (Test-Path -LiteralPath $archive -PathType Leaf) -or -not (Test-Path -LiteralPath $archiveHash -PathType Leaf)) { throw 'Existing archive/hash is incomplete; preserve it and move it aside.' }
  if ((Get-Item -LiteralPath $archive -Force).Attributes -band [IO.FileAttributes]::ReparsePoint -or
      (Get-Item -LiteralPath $archiveHash -Force).Attributes -band [IO.FileAttributes]::ReparsePoint) { throw 'Archive paths must not be links.' }
  $previousHash = (Get-Content -LiteralPath $archiveHash -Raw).Trim().Split(' ')[0]
  if ((Get-FileHash -LiteralPath $archive -Algorithm SHA256).Hash -ne $previousHash) { throw 'Existing archive was changed; preserve it and move it aside.' }
}

New-Item -ItemType Directory -Path $staging | Out-Null
try {
  foreach ($relative in $allowed) {
    $target = Join-Path $staging $relative
    New-Item -ItemType Directory -Path (Split-Path $target -Parent) -Force | Out-Null
    Copy-Item -LiteralPath (Join-Path $repoRoot $relative) -Destination $target
  }
  [IO.File]::WriteAllText((Join-Path $staging '.gitignore'), ".Rproj.user/`n.Rhistory`n.RData`n.Ruserdata`nconfig.*.yml`nusers.local.csv`nrenv/library/`ndemo/.runtime/`ndist/`n", $utf8)
  $dockerFiles = @('global.R','helpers.R','server.R','ui.R','production_calendar.R','report_utils.R','config.yml','version','ui/','ui/*.R','www/','www/my_style.css','demo/','demo/*.R','demo/schema.sql','demo/Dockerfile','demo/docker-compose.yml')
  [IO.File]::WriteAllText((Join-Path $staging '.dockerignore'), "**`n" + (($dockerFiles | ForEach-Object { "!$_" }) -join "`n") + "`n", $utf8)
  $entries = @(Get-ChildItem -LiteralPath $staging -Recurse -Force -File | Sort-Object FullName | ForEach-Object {
    [ordered]@{ path = $_.FullName.Substring($staging.Length + 1).Replace('\', '/'); bytes = $_.Length; sha256 = (Get-FileHash -LiteralPath $_.FullName -Algorithm SHA256).Hash.ToLowerInvariant() }
  })
  $manifest = [ordered]@{ format = 'erp-reports-public/v1'; generated_at_utc = [DateTime]::UtcNow.ToString('o'); files = $entries }
  [IO.File]::WriteAllText((Join-Path $staging $manifestName), ($manifest | ConvertTo-Json -Depth 5) + "`n", $utf8)
  & (Join-Path $PSScriptRoot 'check_public.ps1') -Path $staging

  Add-Type -AssemblyName System.IO.Compression.FileSystem
  $zip = [IO.Compression.ZipFile]::Open($stagingZip, [IO.Compression.ZipArchiveMode]::Create)
  try {
    foreach ($file in Get-ChildItem -LiteralPath $staging -Recurse -Force -File) {
      $relative = $file.FullName.Substring($staging.Length + 1).Replace('\', '/')
      [IO.Compression.ZipFileExtensions]::CreateEntryFromFile($zip, $file.FullName, "erp-reports-public/$relative", [IO.Compression.CompressionLevel]::Optimal) | Out-Null
    }
  } finally { $zip.Dispose() }
  # Replace only a verified, unmodified previous export under this project/dist.
  if (Test-Path -LiteralPath $output) { Assert-ManagedOutput; Remove-Item -LiteralPath $output -Recurse -Force }
  Move-Item -LiteralPath $staging -Destination $output
  if (Test-Path -LiteralPath $archive) { Remove-Item -LiteralPath $archive -Force }
  Move-Item -LiteralPath $stagingZip -Destination $archive
  $hash = (Get-FileHash -LiteralPath $archive -Algorithm SHA256).Hash.ToLowerInvariant()
  [IO.File]::WriteAllText($archiveHash, "$hash  erp-reports-public.zip`n", $utf8)
  Write-Host "Public export ready: dist/erp-reports-public ($($entries.Count + 1) files)"
  Write-Host 'Archive: dist/erp-reports-public.zip; SHA-256: dist/erp-reports-public.zip.sha256'
} finally {
  if (Test-Path -LiteralPath $staging) { Assert-OutputPath $staging; Remove-Item -LiteralPath $staging -Recurse -Force }
  if (Test-Path -LiteralPath $stagingZip) { Remove-Item -LiteralPath $stagingZip -Force }
}
