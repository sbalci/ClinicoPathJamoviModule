#!/usr/bin/env bash
# Prints candidate jamovi function names by scanning jamovi/*.a.yaml
# Portable across GNU/BSD find (no -printf); outputs ONLY the base name without path or extension.
# chmod +x ./.claude/completions/functions_list.sh

set -euo pipefail

root="${1:-$(pwd)}"
dir="$root/jamovi"

# Fallback: if run from inside subdir or different CWD, try git top-level
if [ ! -d "$dir" ]; then
  if git_root=$(git rev-parse --show-toplevel 2>/dev/null); then
    dir="$git_root/jamovi"
  fi
fi

# If jamovi dir still not found, exit quietly
if [ ! -d "$dir" ]; then
  exit 0
fi

# List *.a.yaml files at depth 1 and strip the suffix. Works on BSD and GNU.
# We use -exec with a tiny shell to basename each path and drop the extension.
find "$dir" -maxdepth 1 -type f -name "*.a.yaml" -exec sh -c '
  for f in "$@"; do
    bn="$(basename "$f")"
    bn_noext="${bn%.a.yaml}"
    printf "%s\n" "$bn_noext"
  done
' _ {} + | sort -u