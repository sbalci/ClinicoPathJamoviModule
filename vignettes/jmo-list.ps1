$base = "https://library.jamovi.org/win64/R4.5.0-x64/"
(Invoke-WebRequest $base).Links |
  Where-Object { $_.href -match "\.jmo$" } |
  ForEach-Object { $base + $_.href }