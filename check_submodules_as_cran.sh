#!/usr/bin/env bash
# R CMD check --as-cran for each jamovi submodule.
# Usage:  bash check_submodules_as_cran.sh [module ...]
#   no args  -> checks all 5 submodules
#   e.g.     bash check_submodules_as_cran.sh meddecide jsurvival
#
# Notes:
#  - `env -u ELECTRON_RUN_AS_NODE` avoids VS Code's terminal hijacking the R/jamovi binary.
#  - R CMD build honours .Rbuildignore, so the .omv jamovi assets are excluded from the tarball.
#  - Results (00check.log) land in a temp dir printed per module; grep it for NOTE/WARNING/ERROR.

set -u
GH="/Users/serdarbalci/Documents/GitHub"
MODS=("$@"); [ ${#MODS[@]} -eq 0 ] && MODS=(jjstatsplot jsurvival meddecide ClinicoPathDescriptives OncoPath)

for m in "${MODS[@]}"; do
  d="$GH/$m"
  echo "==================== $m ===================="
  out=$(mktemp -d)
  ( cd "$d" && env -u ELECTRON_RUN_AS_NODE R CMD build . --no-build-vignettes --no-manual ) \
    && tgz=$(ls -t "$d"/${m}_*.tar.gz 2>/dev/null | head -1) \
    && env -u ELECTRON_RUN_AS_NODE _R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran --no-manual -o "$out" "$tgz"
  echo "  --- summary for $m (log: $out/${m}.Rcheck/00check.log) ---"
  grep -E 'NOTE|WARNING|ERROR' "$out/${m}.Rcheck/00check.log" 2>/dev/null || echo "  (no NOTE/WARNING/ERROR — clean)"
done
