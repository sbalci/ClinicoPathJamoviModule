# save as fetch-jamovi-mine.ps1
param(
  [string]$Base = "https://library.jamovi.org/",
  [string]$Out = "jamovi_ClinicoPath_modules.csv"
)

$rx = '^(ClinicoPath|ClinicoPathDescriptives|meddecide|jsurvival|jjstatsplot|OncoPath)-.*\.jmo$'

function Get-Links($Url) {
  (Invoke-WebRequest -Uri $Url -UseBasicParsing).Links |
    Where-Object href | ForEach-Object {
      $full = (New-Object System.Uri((New-Object System.Uri($Url)), $_.href)).AbsoluteUri
      if ($_.href -ne "../" -and $_.href -ne "./" -and -not $_.href.StartsWith("#")) { $full }
    }
}

$osDirs = Get-Links $Base | Where-Object { $_ -match "/(linux|macos|win64)/$" } | Sort-Object
$rows = @()

foreach ($osDir in $osDirs) {
  $os = ($osDir.TrimEnd('/').Split('/') | Select-Object -Last 1)
  $rDirs = Get-Links $osDir | Where-Object { $_ -match "/R\d" } | Sort-Object

  foreach ($rDir in $rDirs) {
    $rver = ($rDir.TrimEnd('/').Split('/') | Select-Object -Last 1)
    $lvl1 = Get-Links $rDir
    $subdirs = $lvl1 | Where-Object { $_ -match "/$" }
    $files1  = $lvl1 | Where-Object { $_ -notmatch "/$" }
    $allFiles = @($files1)
    foreach ($sd in $subdirs) { $allFiles += (Get-Links $sd | Where-Object { $_ -notmatch "/$" }) }

    foreach ($f in $allFiles) {
      $fname = ($f.Split('/') | Select-Object -Last 1)
      if ($fname -match $rx) {
        $rows += [pscustomobject]@{ os=$os; r_version=$rver; filename=$fname; url=$f }
      }
    }
  }
}

$rows | Sort-Object os, r_version, filename |
  Export-Csv -NoTypeInformation -Encoding UTF8 -Path $Out
Write-Host "Wrote $($rows.Count) rows to $Out"