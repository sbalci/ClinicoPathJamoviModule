#!/bin/zsh
# Render one analysis' options panel in jamovi's REAL client bundle, headlessly,
# and print what jamovi would have swallowed: the JS exception (or the
# "Option panel errors" list) that leaves the panel stuck on its grey skeleton.
#
#   tools/ui_harness/render_ui.sh decisioncurve [more basenames...]
#
# Compiles jamovi/<name>.u.yaml straight from source with jmvtools' bundled
# jamovi-compiler (no module-wide prepare()), serves jamovi.app's client dist
# plus harness.html on 127.0.0.1:8771, and drives analysisui.html through the
# same postMessage protocol the jamovi main window uses.
# Needs: Google Chrome, /Applications/jamovi.app, jmvtools (for jamovi-compiler).
set -u
HERE=$(cd "$(dirname "$0")" && pwd); REPO=$(cd "$HERE/../.." && pwd)
WORK=${TMPDIR:-/tmp}/jamovi-ui-harness; mkdir -p "$WORK"
DIST=/Applications/jamovi.app/Contents/Resources/jamovi/client/dist
CHROME="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
ln -sfn "$DIST/assets" "$WORK/assets"; cp "$DIST/analysisui.html" "$HERE/harness.html" "$WORK/"
python3 -m http.server 8771 --bind 127.0.0.1 --directory "$WORK" >/dev/null 2>&1 & SRV=$!
trap 'kill $SRV 2>/dev/null; rm -rf "$WORK/chromeprofile"' EXIT
sleep 0.5
rc=0
for name in "$@"; do
  echo "== $name"
  node "$HERE/compile_ui.mjs" "$REPO" "$name" "$WORK" >/dev/null || { echo "   compile failed"; rc=1; continue; }
  out=$(perl -e 'alarm 25; exec @ARGV' -- "$CHROME" --headless=new --disable-gpu --no-first-run \
        --user-data-dir="$WORK/chromeprofile" --enable-logging=stderr --v=0 --window-size=1200,1000 \
        "http://127.0.0.1:8771/harness.html?ui=$name" 2>&1 >/dev/null \
        | grep -E 'HARNESS: (response .*ERR|placeholder)|Uncaught' \
        | sed 's/^\[[0-9:\/\.]* *[A-Z]*:CONSOLE[^]]*\] *//; s/, source:.*//')
  echo "$out"
  echo "$out" | grep -q 'placeholder present = false .* errors = undefined' || rc=1
done
exit $rc
