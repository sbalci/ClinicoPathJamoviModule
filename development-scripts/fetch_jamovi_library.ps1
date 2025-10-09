# Fetch all files across OS and R versions from https://library.jamovi.org/
# Outputs jamovi_library_files.csv in the current directory.

param(
  [string]$Base = "https://library.jamovi.org/",
  [string]$Pattern = ".*",     # e.g. '\.jmo$'
  [string]$Out = "jamovi_library_files.csv"
)

function Get-Links($Url) {
  $resp = Invoke-WebRequest -Uri $Url -UseBasicParsing
  $links = @()
  foreach ($l in $resp.Links) {
    if (-not $l.href) { continue }
    if ($l.href -eq "../" -or $l.href -eq "./" -or $l.href.StartsWith("#")) { continue }
    $full = (New-Object System.Uri((New-Object System.Uri($Url)), $l.href)).AbsoluteUri
    $links += $full
  }
  return $links
}

$osDirs = Get-Links $Base | Where-Object { $_ -match "/(linux|macos|win64)/$" } | Sort-Object
$rows = @()

foreach ($osDir in $osDirs) {
  $os = ($osDir.TrimEnd('/').Split('/') | Select-Object -Last 1)
  $rDirs = Get-Links $osDir | Where-Object { $_ -match "/R\d" } | Sort-Object

  foreach ($rDir in $rDirs) {
    $rver = ($rDir.TrimEnd('/').Split('/') | Select-Object -Last 1)

    $level1 = Get-Links $rDir
    $subdirs = $level1 | Where-Object { $_ -match "/$" }
    $files1  = $level1 | Where-Object { $_ -notmatch "/$" }

    $allFiles = @($files1)
    foreach ($sd in $subdirs) {
      $allFiles += (Get-Links $sd | Where-Object { $_ -notmatch "/$" })
    }

    foreach ($f in $allFiles) {
      $fname = ($f.Split('/') | Select-Object -Last 1)
      if ($fname -match $Pattern) {
        $rows += [pscustomobject]@{
          os = $os
          r_version = $rver
          filename = $fname
          url = $f
        }
      }
    }
  }
}

$rows | Sort-Object os, r_version, filename |
  Export-Csv -NoTypeInformation -Encoding UTF8 -Path $Out

Write-Host "Wrote $($rows.Count) rows to $Out"