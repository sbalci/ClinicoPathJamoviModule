# /Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/.claude/completions/ingest.sh
# chmod +x .claude/completions/ingest.sh \
#         .claude/completions/review_article_stats_save.sh
#!/usr/bin/env bash
# Convert a local file or URL to Markdown and print the resulting .md path.
# Supports: PDF, HTML/HTM, MD, TXT, DOCX. Prefers markitdown; falls back to `python -m markitdown`; then pandoc for HTML/DOCX.

set -euo pipefail

err() { printf "[ingest] %s\n" "$*" >&2; }
need() { command -v "$1" >/dev/null 2>&1; }

if [ "$#" -lt 1 ]; then
  err "Usage: $0 <file-or-url>"
  exit 2
fi

INPUT="$1"

is_url() { case "$1" in http://*|https://*) return 0;; *) return 1;; esac; }
base_for_ext() { printf "%s" "$1" | sed -E 's/[?#].*$//' | sed -E 's/\/$//'; }
get_ext() { b=$(base_for_ext "$1"); e="${b##*.}"; printf "%s" "${e,,}"; }

# Repo root (for temp)
if git_root=$(git rev-parse --show-toplevel 2>/dev/null); then
  tmpdir="$git_root/.claude/tmp"
else
  tmpdir="${TMPDIR:-/tmp}/claude-ingest"
fi
mkdir -p "$tmpdir"

WORK=""
if is_url "$INPUT"; then
  need curl || { err "curl not found"; exit 3; }
  filename=$(basename "$(base_for_ext "$INPUT")"); [ -z "$filename" ] && filename="download"
  WORK="$tmpdir/$filename"
  curl -Ls "$INPUT" -o "$WORK"
else
  [ -f "$INPUT" ] || { err "Not a file: $INPUT"; exit 4; }
  WORK="$INPUT"
fi

EXT=$(get_ext "$WORK")
out_md="$tmpdir/$(basename "${WORK%.*}").md"

run_markitdown() {
  if need markitdown; then
    markitdown "$WORK" > "$out_md"; return 0
  elif need python; then
    python -m markitdown "$WORK" > "$out_md"; return 0
  fi
  return 1
}

case "$EXT" in
  md|txt)
    cp -f "$WORK" "$out_md"
    ;;
  html|htm)
    if run_markitdown; then :; elif need pandoc; then pandoc -f html -t gfm "$WORK" -o "$out_md"; else err "Need markitdown or pandoc for HTML"; exit 5; fi
    ;;
  pdf)
    if run_markitdown; then :; else err "Need markitdown (or python -m markitdown) for PDF"; exit 6; fi
    ;;
  docx)
    if need pandoc; then pandoc -f docx -t gfm "$WORK" -o "$out_md"; else err "Need pandoc for DOCX"; exit 7; fi
    ;;
  *)
    if run_markitdown; then :; else err "Unsupported extension: .$EXT"; exit 8; fi
    ;;
esac

printf "%s\n" "$out_md"