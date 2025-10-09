# Change BASE to win64/macos/linux and the R folder as needed
BASE="https://library.jamovi.org/win64/R4.5.0-x64/"
curl -s "$BASE" \
| sed -n 's/.*href="\(.*\.jmo\)".*/\1/p' \
| awk -v base="$BASE" '{print base $0}'
