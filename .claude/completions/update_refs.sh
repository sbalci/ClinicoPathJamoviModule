#!/usr/bin/env bash
set -euo pipefail

B_R="${1:?Path to .b.R required}"
R_YAML="${2:?Path to .r.yaml required}"
REFS_YAML="${3:?Path to jamovi/00refs.yaml required}"

timestamp() { date +"%Y%m%d-%H%M%S"; }

backup_file() {
  local f="$1"
  cp -f "$f" "${f}.bak.$(timestamp)"
}

ensure_file_exists() {
  local f="$1"
  if [[ ! -f "$f" ]]; then
    echo "ERROR: File not found: $f" >&2
    exit 1
  fi
}

ensure_file_exists "$B_R"
ensure_file_exists "$R_YAML"
ensure_file_exists "$REFS_YAML"

backup_file "$R_YAML"
backup_file "$REFS_YAML"

# Extract packages from .b.R
PKGS_RAW=$(
  {
    grep -Eo 'library\s*\(\s*["'"'"']?[A-Za-z0-9._]+["'"'"']?\s*\)' "$B_R" 2>/dev/null || true
    grep -Eo 'require\s*\(\s*["'"'"']?[A-Za-z0-9._]+["'"'"']?\s*(,|\))' "$B_R" 2>/dev/null | sed 's/[,) ]$//' || true
    grep -Eo '([A-Za-z0-9._]+)::' "$B_R" 2>/dev/null | sed 's/::$//' || true
  } \
  | sed -E 's/.*library\s*\(\s*["'"'"']?([A-Za-z0-9._]+).*/\1/' \
  | sed -E 's/.*require\s*\(\s*["'"'"']?([A-Za-z0-9._]+).*/\1/'
)

PKGS=()
while IFS= read -r line; do
  [[ -n "$line" ]] && PKGS+=("$line")
done < <(printf "%s\n" "$PKGS_RAW" | awk '!seen[$0]++' | sed '/^$/d')

FOUNDATION_PKGS=("jmvcore" "ggplot2" "R6" "stats" "utils" "kableExtra" "rlang" "dplyr" "tidyr" "glue" "stringr" "htmltools" "labelled" "scales" "purrr" "gt" "DiagrammeRsvg" "base" "RColorBrewer" "gridExtra" "tibble" "grDevices" "grid" "janitor" "methods")
FILTERED_PKGS=()
for pkg in "${PKGS[@]}"; do
  skip=0
  for basepkg in "${FOUNDATION_PKGS[@]}"; do
    if [[ "$pkg" == "$basepkg" ]]; then
      skip=1
      break
    fi
  done
  if [[ $skip -eq 0 ]]; then
    FILTERED_PKGS+=("$pkg")
  fi
done
PKGS=("${FILTERED_PKGS[@]}")

if [[ ${#PKGS[@]} -eq 0 ]]; then
  echo "No packages detected in $B_R (after excluding foundational packages). Nothing to do."
  exit 0
fi

echo "Detected packages in $B_R:"
for p in "${PKGS[@]}"; do echo "  - $p"; done

# Ensure 00refs.yaml has each package
for pkg in "${PKGS[@]}"; do
  if grep -Eq "^    ${pkg}:" "$REFS_YAML" 2>/dev/null; then
    continue
  fi
  echo " - Adding ref for '${pkg}' to $REFS_YAML"
  # Insert before ... line
  if grep -Eq '^\.\.\.\s*$' "$REFS_YAML"; then
    sed -i '' "/^\.\.\./i\\
\\
    ${pkg}:\\
        type: 'software'\\
        author:\\
        year:\\
        title: \"${pkg}: R package\"\\
        publisher: '[R package]. Retrieved from https://CRAN.R-project.org/package=${pkg}'\\
        url: https://CRAN.R-project.org/package=${pkg}
" "$REFS_YAML"
  else
    cat >> "$REFS_YAML" <<EOF

    ${pkg}:
        type: 'software'
        author:
        year:
        title: "${pkg}: R package"
        publisher: '[R package]. Retrieved from https://CRAN.R-project.org/package=${pkg}'
        url: https://CRAN.R-project.org/package=${pkg}
EOF
  fi
done

# Ensure r.yaml has refs section
if ! grep -Eq '^refs:\s*$' "$R_YAML"; then
  if grep -Eq '^\.\.\.\s*$' "$R_YAML"; then
    sed -i '' '/^\.\.\.$/i\
\
refs:
' "$R_YAML"
  else
    echo -e "\nrefs:" >> "$R_YAML"
  fi
fi

# Add each package to r.yaml refs list
for pkg in "${PKGS[@]}"; do
  if grep -Eq "^[[:space:]]*-[[:space:]]*['\"]?${pkg}['\"]?" "$R_YAML" 2>/dev/null; then
    continue
  fi
  echo " - Adding '${pkg}' to refs in $R_YAML"
  if grep -Eq '^\.\.\.\s*$' "$R_YAML"; then
    sed -i '' "/^\.\.\./i\\
    - ${pkg}
" "$R_YAML"
  else
    echo "    - ${pkg}" >> "$R_YAML"
  fi
done

# Ensure ClinicoPathJamoviModule is first ref
if ! grep -Eq "ClinicoPathJamoviModule" "$R_YAML" 2>/dev/null; then
  echo " - Adding 'ClinicoPathJamoviModule' to refs in $R_YAML"
  # Insert right after refs: line
  sed -i '' '/^refs:$/a\
    - ClinicoPathJamoviModule
' "$R_YAML"
fi

echo "Done."
