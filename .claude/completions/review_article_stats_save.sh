#!/usr/bin/env bash
# Run /review-article-stats with one or more sources, inline their Markdown,
# and save output to literature/<label>-citation-review.md

set -euo pipefail

me_dir="$(cd "$(dirname "$0")" && pwd)"
root_dir="$(cd "$me_dir/../.." && pwd)"
ingest="$me_dir/ingest.sh"

if [ "$#" -lt 1 ]; then
  echo "Usage: $0 <LABEL> [source1 ...]" >&2
  exit 2
fi

LABEL="$1"; shift || true

if [ ! -x "$ingest" ]; then
  echo "[save] missing: $ingest" >&2
  exit 3
fi

# Ensure literature dir exists
lit_dir="$root_dir/literature"
mkdir -p "$lit_dir"

# Slugify label for filename
slug=$(printf "%s" "$LABEL" | tr ' ' '-' | tr -cd '[:alnum:]-_.' | sed 's/--\+/-/g')
[ -z "$slug" ] && slug="review"
outfile="$lit_dir/${slug}-citation-review.md"

# Nounset-safe arrays
declare -a MD_FILES=()
declare -a SKIPPED=()

# Convert all sources to Markdown (best-effort). Skip failures but record why.
for s in "$@"; do
  if mdp="$("$ingest" "$s" 2>"$me_dir/.ingest.err" || true)"; then
    if [ -n "${mdp:-}" ] && [ -f "$mdp" ]; then
      MD_FILES+=("$mdp")
    else
      reason="Conversion failed"
      SKIPPED+=("$s|||$reason")
    fi
  else
    reason="$(tr -d '\r' < "$me_dir/.ingest.err" | tail -n 1 || true)"
    SKIPPED+=("$s|||${reason:-conversion error}")
  fi
  rm -f "$me_dir/.ingest.err" || true
done

# Build prompt with inline Markdown and skipped table to avoid tool_use
prompt_file="$(mktemp -t review-article-XXXX).txt"
{
  printf "/review-article-stats %s\n\n" "$LABEL"

  if [ "${#SKIPPED[@]}" -gt 0 ]; then
    printf "### Skipped Sources\n\n| Source | Reason | Suggested command |\n|---|---|---|\n"
    for item in "${SKIPPED[@]}"; do
      src="${item%%|||*}"; rest="${item#*|||}"; reason="$rest"
      esc_src=$(printf '%s' "$src" | sed 's/|/\\|/g')
      printf "| %s | %s | markitdown \"%s\" > \"%s.md\" || python -m markitdown \"%s\" > \"%s.md\" |\n" \
        "$esc_src" "${reason:-unknown}" "$src" "$src" "$src" "$src"
    done
    printf "\n"
  fi

  if [ "${#MD_FILES[@]}" -gt 0 ]; then
    i=1
    for md in "${MD_FILES[@]}"; do
      printf "### Source %d: %s\n\n" "$i" "$md"
      printf "```markdown\n"; cat "$md"; printf "\n```\n\n"; i=$((i+1))
    done
  else
    printf "_No readable sources were inlined. Provide Markdown/text or convert with markitdown/pandoc._\n\n"
  fi
} > "$prompt_file"

# Claude CLI present?
if ! command -v claude >/dev/null 2>&1; then
  echo "[save] 'claude' CLI not found in PATH" >&2
  rm -f "$prompt_file"; exit 4
fi

# Run Claude; write to literature/<label>-citation-review.md
# (Pipe stdin; do not pass unsupported flags.)
claude < "$prompt_file" | tee "$outfile"
ret=${PIPESTATUS[0]}
rm -f "$prompt_file"
echo "[save] Saved report to: $outfile" >&2
exit $ret