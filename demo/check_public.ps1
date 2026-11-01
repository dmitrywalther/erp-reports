# Check the public package without displaying matched values or file contents.
param([string]$Path = (Join-Path (Split-Path $PSScriptRoot -Parent) 'dist/erp-reports-public'))
$ErrorActionPreference = 'Stop'
$root = [IO.Path]::GetFullPath($Path).TrimEnd([char[]]'\/')
if (-not (Test-Path -LiteralPath $root -PathType Container)) { throw 'Public package directory does not exist.' }
if ((Get-Item -LiteralPath $root -Force).Attributes -band [IO.FileAttributes]::ReparsePoint) { throw 'Public package root must not be a link.' }
$items = @((Get-Item -LiteralPath $root -Force)) + @(Get-ChildItem -LiteralPath $root -Recurse -Force)
if ($items | Where-Object { $_.Attributes -band [IO.FileAttributes]::ReparsePoint }) { throw 'Public package contains a link/reparse point.' }
$allowed = @(Get-Content -LiteralPath (Join-Path $PSScriptRoot 'public_files.txt') | Where-Object { $_ -and -not $_.StartsWith('#') })
$allowed += @('.gitignore', '.dockerignore', 'PUBLIC_EXPORT_MANIFEST.json')
$findings = [Collections.Generic.List[string]]::new()
$files = @($items | Where-Object { -not $_.PSIsContainer })
$paths = @($files | ForEach-Object { $_.FullName.Substring($root.Length + 1).Replace('\', '/') })
foreach ($directory in $items | Where-Object { $_.PSIsContainer -and $_.FullName -ne $root }) {
  $relative = $directory.FullName.Substring($root.Length + 1).Replace('\', '/')
  if ($relative -notin @('demo', 'demo/online', 'docs', 'docs/screenshots', 'ui', 'www')) { $findings.Add("non-whitelisted-directory: $relative") }
}
foreach ($required in $allowed) {
  if ($required -notin $paths) { $findings.Add("missing-file: $required") }
}

# Signatures deliberately do not contain real credentials, names or addresses.
$rules = @(
  @{ Name = 'private-key'; Pattern = '-----BEGIN (?:RSA |EC |OPENSSH )?PRIVATE KEY-----' },
  @{ Name = 'access-token'; Pattern = '(?:glpat-[A-Za-z0-9_-]{20,}|gh[pousr]_[A-Za-z0-9_]{20,}|github_pat_[A-Za-z0-9_]{20,}|AKIA[0-9A-Z]{16}|xox[baprs]-[A-Za-z0-9-]{20,}|eyJ[A-Za-z0-9_-]{10,}\.[A-Za-z0-9_-]{10,}\.[A-Za-z0-9_-]{10,})' },
  @{ Name = 'authenticated-url'; Pattern = 'https?://[^\s/:"'']+:[^\s/@"'']+@' },
  @{ Name = 'teams-webhook'; Pattern = 'https?://[^\s"'']+(?:webhookb2|IncomingWebhook)/[^\s"'']+' },
  @{ Name = 'internal-host'; Pattern = ('(?i)(?:go' + 'fura\.(?:com|ru)|gitlab\.int\.)') },
  @{ Name = 'private-ip'; Pattern = '(?<![\d.])(?:10\.(?:\d{1,3}\.){2}\d{1,3}|192\.168\.\d{1,3}\.\d{1,3}|172\.(?:1[6-9]|2\d|3[01])\.\d{1,3}\.\d{1,3})(?![\d.])' },
  @{ Name = 'personal-local-path'; Pattern = '(?i)[A-Z]:\\Users\\[^\s\\"'']+' }
)
foreach ($file in $files) {
  $relative = $file.FullName.Substring($root.Length + 1).Replace('\', '/')
  if ($relative -notin $allowed) { $findings.Add("non-whitelisted-file: $relative") }
  if ($relative -match '(?i)(^|/)(\.git|\.runtime|renv|deployment|backups?|logs?|cache|images)(/|$)|\.(xlsx?|csv|rdata|rhistory|rds|dump|bak|log|env|pem|key|pfx)$|(^|/)config\.[^/]+\.yml$') {
    $findings.Add("forbidden-path: $relative")
  }
  if ($relative -notin $allowed) { continue } # No need to read rejected private/binary files.
  if ($file.Extension -ieq '.jpg') { continue } # Screenshots require a visual review.
  $lineNumber = 0
  foreach ($line in [IO.File]::ReadAllLines($file.FullName)) {
    $lineNumber++
    foreach ($rule in $rules) {
      if ([regex]::IsMatch($line, $rule.Pattern)) { $findings.Add("$($rule.Name): ${relative}:$lineNumber") }
    }
    foreach ($email in [regex]::Matches($line, '[A-Za-z0-9._%+-]+@([A-Za-z0-9.-]+\.[A-Za-z]{2,})')) {
      if ($email.Groups[1].Value -notmatch '(?i)(^|\.)example\.(com|org|net|invalid)$|\.invalid$') {
        $findings.Add("non-example-email: ${relative}:$lineNumber")
      }
    }
    foreach ($credential in [regex]::Matches($line, '(?i)(?:password|passwd|pwd|api[_-]?key|secret[_-]?access[_-]?key|secret|token)\s*(?:=|:|<-)\s*["'']([^"'']+)["'']')) {
      $value = $credential.Groups[1].Value
      # A shell variable or psql's quoted variable binding is not a literal secret.
      $shellReference = $file.Extension -ieq '.sh' -and $value -match '^\$(?:[A-Za-z_][A-Za-z0-9_]*|\{[A-Za-z_][A-Za-z0-9_]*\})$'
      $psqlReference = $file.Extension -in @('.sh', '.sql') -and $credential.Value -match '(?i)^password\s*:\s*''[A-Za-z_][A-Za-z0-9_]*''$'
      if ($value -notin @('demo', 'postgres') -and -not $shellReference -and -not $psqlReference) { $findings.Add("literal-credential: ${relative}:$lineNumber") }
    }
    if ($line -match '(?i)^\s*[A-Z_]*(?:PASSWORD|TOKEN|SECRET|WEBHOOK_URL)\s*:\s*(\S+)') {
      if ($Matches[1] -notin @('demo', '"demo"', "'demo'", '!expr')) { $findings.Add("yaml-credential: ${relative}:$lineNumber") }
    }
    if ($file.Extension -ieq '.md') {
      foreach ($link in [regex]::Matches($line, '!?\[[^\]]*\]\(([^)]+)\)')) {
        $target = $link.Groups[1].Value.Trim().Trim('<', '>')
        if ($target -match '^(?i)(https?://|mailto:|#)') { continue }
        $target = [Uri]::UnescapeDataString(($target -split '#', 2)[0])
        if ([IO.Path]::IsPathRooted($target)) { $findings.Add("absolute-local-link: ${relative}:$lineNumber"); continue }
        $resolved = [IO.Path]::GetFullPath((Join-Path $file.DirectoryName $target))
        if (-not $resolved.StartsWith($root + [IO.Path]::DirectorySeparatorChar, [StringComparison]::OrdinalIgnoreCase) -or
            -not (Test-Path -LiteralPath $resolved)) { $findings.Add("broken-local-link: ${relative}:$lineNumber") }
      }
    }
  }
}

$manifestPath = Join-Path $root 'PUBLIC_EXPORT_MANIFEST.json'
if (Test-Path -LiteralPath $manifestPath -PathType Leaf) {
  $manifest = Get-Content -LiteralPath $manifestPath -Raw | ConvertFrom-Json
  if ($manifest.format -ne 'erp-reports-public/v1') { $findings.Add('invalid-manifest-format') }
  $manifestPaths = @($manifest.files | ForEach-Object { $_.path })
  if (@($manifestPaths | Select-Object -Unique).Count -ne $manifestPaths.Count) { $findings.Add('duplicate-manifest-path') }
  foreach ($relative in $paths | Where-Object { $_ -ne 'PUBLIC_EXPORT_MANIFEST.json' }) {
    $entry = @($manifest.files | Where-Object { $_.path -eq $relative })
    if ($entry.Count -ne 1) { $findings.Add("missing-manifest-entry: $relative"); continue }
    $actual = Get-Item -LiteralPath (Join-Path $root $relative)
    if ($actual.Length -ne $entry[0].bytes -or (Get-FileHash -LiteralPath $actual.FullName -Algorithm SHA256).Hash -ne $entry[0].sha256) {
      $findings.Add("manifest-mismatch: $relative")
    }
  }
  foreach ($relative in $manifestPaths) {
    if ($relative -notin $paths) { $findings.Add("missing-manifest-file: $relative") }
  }
}
if ($findings.Count) {
  $findings | Sort-Object -Unique | ForEach-Object { Write-Host $_ }
  throw "Public package check failed ($($findings.Count) findings; matched values are not printed)."
}
Write-Host "Public package check passed: $($files.Count) files; allowlist, signatures and SHA-256 manifest verified."
