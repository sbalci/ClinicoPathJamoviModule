#!/usr/bin/env bash
# Prints candidate jamovi function names by scanning jamovi/*.a.yaml

root="${1:-$(pwd)}"
dir="$root/jamovi"

# Fallback if run from inside subdir
if [ ! -d "$dir" ]; then
  dir="$(git rev-parse --show-toplevel 2>/dev/null)/jamovi"
fi

# List *.a.yaml and strip the suffix to get function names
# Handles filenames like: myfunc.a.yaml
if [ -d "$dir" ]; then
  find "$dir" -maxdepth 1 -type f -name "*.a.yaml" -printf "%f\n" 2>/dev/null \
    | sed -E 's/\.a\.yaml$//' \
    | sort -u
fi