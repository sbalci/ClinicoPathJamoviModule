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

# --- 0) Preconditions & backups ------------------------------------------------
ensure_file_exists "$B_R"
ensure_file_exists "$R_YAML"
ensure_file_exists "$REFS_YAML"

backup_file "$R_YAML"
backup_file "$REFS_YAML"

# --- 1) Extract packages from .b.R --------------------------------------------
PKGS_RAW=$(
  {
    grep -Eo 'library\s*\(\s*["'"'"']?[A-Za-z0-9._]+["'"'"']?\s*\)' "$B_R" || true
    grep -Eo 'require\s*\(\s*["'"'"']?[A-Za-z0-9._]+["'"'"']?\s*(,|\))' "$B_R" | sed 's/[,) ]$//' || true
    grep -Eo '([A-Za-z0-9._]+)::' "$B_R" | sed 's/::$//' || true
  } \
  | sed -E 's/.*library\s*\(\s*["'"'"']?([A-Za-z0-9._]+).*/\1/' \
  | sed -E 's/.*require\s*\(\s*["'"'"']?([A-Za-z0-9._]+).*/\1/'
)

PKGS=()
while IFS= read -r line; do
  [[ -n "$line" ]] && PKGS+=("$line")
done < <(printf "%s\n" "$PKGS_RAW" | awk '!seen[$0]++' | sed '/^$/d')

FOUNDATION_PKGS=("jmvcore" "ggplot2" "R6" "stats" "utils" "kableExtra" "rlang" "dplyr" "tidyr" "glue" "stringr" "htmltools" "labelled" "scales" "purrr" "gt" "DiagrammeRsvg" "base" "RColorBrewer" "gridExtra" "tibble" "grDevices" "grid" "ClinicoPath")
FILTERED_PKGS=()
if [[ ${#PKGS[@]} -gt 0 ]]; then
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
fi
if [[ ${#FILTERED_PKGS[@]} -gt 0 ]]; then
  PKGS=("${FILTERED_PKGS[@]}")
else
  PKGS=()
fi

if [[ ${#PKGS[@]} -eq 0 ]]; then
  echo "No external packages detected in $B_R (after excluding foundational packages)."
  echo "Will still ensure ClinicoPathJamoviModule module reference is present."
fi

if [[ ${#PKGS[@]} -gt 0 ]]; then
  echo "Detected packages in $B_R:"
  for p in "${PKGS[@]}"; do echo "  - $p"; done
fi

# --- 2) Ensure jamovi/00refs.yaml contains each package key --------------------------
ensure_refs_yaml_key() {
  local key="$1"
  if grep -Eq "^ {4}${key}:" "$REFS_YAML"; then
    return 0
  fi

  if ! grep -Eq '^refs:\s*$' "$REFS_YAML"; then
    tmp="$(mktemp)"
    printf "refs:\n" > "$tmp"
    cat "$REFS_YAML" >> "$tmp"
    mv "$tmp" "$REFS_YAML"
  fi

  if grep -Eq '^\.\.\.\s*$' "$REFS_YAML"; then
    tmp="$(mktemp)"
    sed "/^\.\.\.\$/i\\
\\
    ${key}:\\
        type: 'software'\\
        author:\\
        year:\\
        title: \"${key}: R package\"\\
        publisher: '[R package]. Retrieved from https://CRAN.R-project.org/package=${key}'\\
        url: https://CRAN.R-project.org/package=${key}
" "$REFS_YAML" > "$tmp"
    mv "$tmp" "$REFS_YAML"
  else
    cat >> "$REFS_YAML" <<EOF

    ${key}:
        type: 'software'
        author:
        year:
        title: "${key}: R package"
        publisher: '[R package]. Retrieved from https://CRAN.R-project.org/package=${key}'
        url: https://CRAN.R-project.org/package=${key}
EOF
  fi

  echo " - Added placeholder ref for '${key}' to $REFS_YAML"
}

echo "Checking $REFS_YAML for missing refs…"
if [[ ${#PKGS[@]} -gt 0 ]]; then
  for pkg in "${PKGS[@]}"; do
    ensure_refs_yaml_key "$pkg"
  done
fi

# --- 3) Add package keys to .r.yaml refs list ---------------------------------
ensure_ryaml_refs_section() {
  if grep -Eq '^refs:\s*$' "$R_YAML"; then
    return 0
  fi

  if grep -Eq '^\.\.\.\s*$' "$R_YAML"; then
    tmp="$(mktemp)"
    sed "/^\.\.\.\$/i\\
\\
refs:
" "$R_YAML" > "$tmp"
    mv "$tmp" "$R_YAML"
  else
    echo -e "\nrefs:" >> "$R_YAML"
  fi
}

add_ref_to_ryaml_if_missing() {
  local key="$1"
  if grep -Eq "^[[:space:]]*-[[:space:]]*['\"]?${key}['\"]?\s*$" "$R_YAML"; then
    return 0
  fi

  if grep -Eq '^\.\.\.\s*$' "$R_YAML"; then
    tmp="$(mktemp)"
    awk -v key="$key" '
      /^\.\.\./ {
        for (i = 1; i <= nb; i++) {
          print buffer[i]
        }
        print "    - " key
        print $0
        while ((getline) > 0) {
          print
        }
        exit
      }
      {
        nb++
        buffer[nb] = $0
      }
    ' "$R_YAML" > "$tmp"
    mv "$tmp" "$R_YAML"
  else
    echo "    - ${key}" >> "$R_YAML"
  fi

  echo " - Added '${key}' to refs list in $R_YAML"
}

ensure_ryaml_refs_section

echo "Updating $R_YAML refs list…"

# --- 4) Always ensure ClinicoPathJamoviModule is the first ref (module self-reference) ---
add_ref_to_ryaml_if_missing "ClinicoPathJamoviModule"

if [[ ${#PKGS[@]} -gt 0 ]]; then
  for pkg in "${PKGS[@]}"; do
    add_ref_to_ryaml_if_missing "$pkg"
  done
fi

# --- 5) Ensure ClinicoPathJamoviModule exists in 00refs.yaml ---
ensure_refs_yaml_key "ClinicoPathJamoviModule"

echo "Done."
echo "Backups:"
echo " - ${R_YAML}.bak.*"
echo " - ${REFS_YAML}.bak.*"
