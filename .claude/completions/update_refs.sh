#!/usr/bin/env bash
set -euo pipefail

B_R="${1:?Path to .b.R required}"
R_YAML="${2:?Path to .r.yaml required}"
REFS_YAML="${3:?Path to 00refs.yaml required}"

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
# Matches:
#   library(foo) / library("foo") / library('foo')
#   require(bar) / require("bar") / require('bar')
#   baz::qux  (collects 'baz')
# Skips base:: and utils:: etc? We keep all and let refs step dedupe.
PKGS_RAW=$(
  {
    grep -Eo 'library\s*\(\s*["'"'"']?[A-Za-z0-9._]+["'"'"']?\s*\)' "$B_R" || true
    grep -Eo 'require\s*\(\s*["'"'"']?[A-Za-z0-9._]+["'"'"']?\s*(,|\))' "$B_R" | sed 's/[,) ]$//' || true
    grep -Eo '([A-Za-z0-9._]+)::' "$B_R" | sed 's/::$//' || true
  } \
  | sed -E 's/.*library\s*\(\s*["'"'"']?([A-Za-z0-9._]+).*/\1/' \
  | sed -E 's/.*require\s*\(\s*["'"'"']?([A-Za-z0-9._]+).*/\1/'
)

# Normalize: unique, keep order stable (awk trick)
readarray -t PKGS < <(printf "%s\n" "$PKGS_RAW" | awk '!seen[$0]++' | sed '/^$/d')

if [[ ${#PKGS[@]} -eq 0 ]]; then
  echo "No packages detected in $B_R. Nothing to do."
  exit 0
fi

echo "Detected packages in $B_R:"
for p in "${PKGS[@]}"; do echo "  - $p"; done

# --- 2) Ensure 00refs.yaml contains each package key --------------------------
# We assume 00refs.yaml has a top-level "refs:" mapping.
# If a package key is missing, we append a minimal, CRAN-style placeholder entry.
ensure_refs_yaml_key() {
  local key="$1"
  # Anchor on beginning of line (allow 4 spaces indent below 'refs:')
  if grep -Eq "^ {4}${key}:" "$REFS_YAML"; then
    return 0
  fi

  # If 'refs:' missing entirely, add it at file top
  if ! grep -Eq '^refs:\s*$' "$REFS_YAML"; then
    # Prepend refs: and a newline
    tmp="$(mktemp)"
    printf "refs:\n" > "$tmp"
    cat "$REFS_YAML" >> "$tmp"
    mv "$tmp" "$REFS_YAML"
  fi

  # Append a placeholder block for the missing package
  cat >> "$REFS_YAML" <<EOF

    ${key}:
        type: 'software'
        author:
        year:
        title: "${key}: R package"
        publisher: '[R package]. Retrieved from https://CRAN.R-project.org/package=${key}'
        url: https://CRAN.R-project.org/package=${key}
EOF
  echo " - Added placeholder ref for '${key}' to $REFS_YAML"
}

echo "Checking $REFS_YAML for missing refs…"
for pkg in "${PKGS[@]}"; do
  ensure_refs_yaml_key "$pkg"
done

# --- 3) Add package keys to .r.yaml refs list ---------------------------------
# Strategy:
#  - Ensure the file has a 'refs:' list (YAML sequence) section.
#  - Append '- pkg' entries that are not already present.
# We do not reorder or remove anything; we just add missing items.

ensure_ryaml_refs_section() {
  if grep -Eq '^refs:\s*$' "$R_YAML"; then
    return 0
  fi
  # If there is a refs: under items (like per-table), we still want the top-level refs block.
  # Append at end a top-level 'refs:' if missing.
  echo -e "\nrefs:" >> "$R_YAML"
}

add_ref_to_ryaml_if_missing() {
  local key="$1"
  # Check if already referenced as a list item (allow leading spaces and quotes)
  if grep -Eq "^[[:space:]]*-[[:space:]]*['\"]?${key}['\"]?\s*$" "$R_YAML"; then
    return 0
  fi
  # Append directly under the last/top-level 'refs:' block.
  # We simply add at EOF after ensuring refs: exists; jamovi tolerates refs gathered anywhere at top-level.
  echo "  - ${key}" >> "$R_YAML"
  echo " - Added '${key}' to refs list in $R_YAML"
}

ensure_ryaml_refs_section

echo "Updating $R_YAML refs list…"
for pkg in "${PKGS[@]}"; do
  add_ref_to_ryaml_if_missing "$pkg"
done

echo "Done."
echo "Backups:"
echo " - ${R_YAML}.bak.*"
echo " - ${REFS_YAML}.bak.*"

# chmod +x .claude/completions/update_refs.sh
