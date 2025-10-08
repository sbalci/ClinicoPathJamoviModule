---
name: update-refs
description: Parse jamovi .b.R to find used packages and sync them with jamovi/*.r.yaml and jamovi/00refs.yaml
interactive: true
args:
  function_name:
    description: Jamovi function name (e.g., tableone) or --all to process every R/*.b.R
    required: true
usage: /update-refs <function_name>|--all
---

# Update References for Jamovi Function

Synchronize references between `.b.R`, `jamovi/*.r.yaml`, and `jamovi/00refs.yaml`.

## Analysis Target

Function: **`$ARG_function_name`**

### File Paths

Assuming standard module layout:

```
jamovi/$ARG_function_name.r.yaml
R/$ARG_function_name.b.R
jamovi/00refs.yaml
```

### Important Notes

**Item-Level vs Top-Level References:**

- Individual items (tables, plots) in `.r.yaml` files may have their own `refs:` field (e.g., `refs: glmnet`)
- There is also a **top-level `refs:` section** at the end of the file (before `...`)
- **DO NOT remove or modify item-level refs** - they are intentional and specific to that output item
- This tool only manages the **top-level `refs:` section**

### Steps Performed

1. **Detect packages in `.b.R`**
   - Scan for `library()`, `require()`, and `pkg::` usage.
   - Extract unique package names.
   - **Filter out foundational packages**: Excludes `jmvcore`, `ggplot2`, `R6`, `stats`, `utils`, `kableExtra`, `rlang`, `dplyr`, `tidyr` from individual function refs.

2. **Ensure package refs exist in `jamovi/00refs.yaml`**
   - Check whether each package is already listed under `refs:`.
   - If not present, insert placeholder entries **BEFORE the `...` line** (YAML document terminator):

     ```yaml
       packagename:
         type: software
         author:
         year:
         title: "packagename: R package"
         publisher: "[R package]. Retrieved from https://CRAN.R-project.org/package=packagename"
         url: https://CRAN.R-project.org/package=packagename
     ```

3. **Add missing package keys to `.r.yaml`**
   - Ensure a top-level `refs:` list exists.
   - Insert `- packagename` entries **BEFORE the `...` line** if it exists, otherwise append after `refs:`.

4. **Make non-destructive backups**
   - Backups are created:

     ```
     <file>.bak.YYYYMMDD-HHMMSS
     ```

### Implementation Command

The action runs:

```bash
# Bootstrap the worker if missing, then run it
if [ ! -f ".claude/completions/update_refs.sh" ]; then
  mkdir -p ".claude/completions"
  cat > ".claude/completions/update_refs.sh" <<'BASH'
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
# Matches:
#   library(foo) / library("foo") / library('foo')
#   require(bar) / require("bar") / require('bar')
#   baz::qux  (collects 'baz')
PKGS_RAW=$(
  {
    grep -Eo 'library\s*\(\s*["'"'"']?[A-Za-z0-9._]+["'"'"']?\s*\)' "$B_R" || true
    grep -Eo 'require\s*\(\s*["'"'"']?[A-Za-z0-9._]+["'"'"']?\s*(,|\))' "$B_R" | sed 's/[,) ]$//' || true
    grep -Eo '([A-Za-z0-9._]+)::' "$B_R" | sed 's/::$//' || true
  } \
  | sed -E 's/.*library\s*\(\s*["'"'"']?([A-Za-z0-9._]+).*/\1/' \
  | sed -E 's/.*require\s*\(\s*["'"'"']?([A-Za-z0-9._]+).*/\1/'
)

# Normalize: unique, keep order stable
# Use while-read loop for macOS bash 3.x compatibility
PKGS=()
while IFS= read -r line; do
  [[ -n "$line" ]] && PKGS+=("$line")
done < <(printf "%s\n" "$PKGS_RAW" | awk '!seen[$0]++' | sed '/^$/d')

# Exclude foundational base packages that should not be referenced in individual functions
FOUNDATION_PKGS=("jmvcore" "ggplot2" "R6" "stats" "utils" "kableExtra" "rlang" "dplyr")
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

# --- 2) Ensure jamovi/00refs.yaml contains each package key --------------------------
# We assume jamovi/00refs.yaml has a top-level "refs:" mapping.
# If a package key is missing, insert it BEFORE the '...' line (YAML document terminator).
ensure_refs_yaml_key() {
  local key="$1"
  # Check if key already exists under 'refs:' (allow 4 spaces indent)
  if grep -Eq "^ {4}${key}:" "$REFS_YAML"; then
    return 0
  fi

  # If 'refs:' missing entirely, add it at file top
  if ! grep -Eq '^refs:\s*$' "$REFS_YAML"; then
    tmp="$(mktemp)"
    printf "refs:\n" > "$tmp"
    cat "$REFS_YAML" >> "$tmp"
    mv "$tmp" "$REFS_YAML"
  fi

  # Insert before '...' line if it exists, otherwise append to end
  if grep -Eq '^\.\.\.\s*$' "$REFS_YAML"; then
    # Insert before the '...' line using sed
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
    # No '...' found, append to end
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
for pkg in "${PKGS[@]}"; do
  ensure_refs_yaml_key "$pkg"
done

# --- 3) Add package keys to .r.yaml refs list ---------------------------------
# Strategy:
#  - Ensure the file has a 'refs:' list (YAML sequence) section.
#  - Insert '- pkg' entries BEFORE '...' if it exists, otherwise append.
# We do not reorder or remove anything; we just add missing items.

ensure_ryaml_refs_section() {
  if grep -Eq '^refs:\s*$' "$R_YAML"; then
    return 0
  fi

  # Insert 'refs:' before '...' if it exists, otherwise append
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
  # Check if already referenced as a list item (allow leading spaces and quotes)
  if grep -Eq "^[[:space:]]*-[[:space:]]*['\"]?${key}['\"]?\s*$" "$R_YAML"; then
    return 0
  fi

  # Insert before '...' if it exists, otherwise append after refs:
  if grep -Eq '^\.\.\.\s*$' "$R_YAML"; then
    tmp="$(mktemp)"
    # Insert before '...' with proper 4-space indentation, avoiding multiple blank lines
    awk -v key="$key" '
      /^\.\.\./ {
        # Skip any consecutive blank lines before ...
        while (nb > 0 && length(buffer[nb]) == 0) {
          nb--
        }
        # Print buffer content
        for (i = 1; i <= nb; i++) {
          print buffer[i]
        }
        # Add the new entry (no extra blank line - one already exists in refs section)
        print "    - " key
        # Print the ... line itself
        print $0
        # Print anything after ... (if any)
        while ((getline) > 0) {
          print
        }
        exit
      }
      {
        # Store in buffer
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
for pkg in "${PKGS[@]}"; do
  add_ref_to_ryaml_if_missing "$pkg"
done

echo "Done."
echo "Backups:"
echo " - ${R_YAML}.bak.*"
echo " - ${REFS_YAML}.bak.*"
BASH
  chmod +x ".claude/completions/update_refs.sh"
fi

if [ "$ARG_function_name" = "--all" ]; then
  echo "Running update-refs for all functions under R/*.b.R …"
  shopt -s nullglob
  missing_ryaml=()
  failed_funcs=()
  processed_funcs=()
  for B in R/*.b.R; do
    fn="$(basename "$B" .b.R)"
    echo "==> $fn"
    if [ ! -f "jamovi/${fn}.r.yaml" ]; then
      echo "MISSING: jamovi/${fn}.r.yaml"
      missing_ryaml+=("$fn")
      continue
    fi
    if ! bash ".claude/completions/update_refs.sh" "R/${fn}.b.R" "jamovi/${fn}.r.yaml" "jamovi/00refs.yaml"; then
      echo "WARN: failed for ${fn}"
      failed_funcs+=("$fn")
    else
      processed_funcs+=("$fn")
    fi
  done
  echo ""
  echo "=== Summary Report ==="
  echo "Processed successfully: ${#processed_funcs[@]}"
  if [ ${#processed_funcs[@]} -gt 0 ]; then printf '  - %s\n' "${processed_funcs[@]}"; fi
  echo "Missing jamovi/*.r.yaml: ${#missing_ryaml[@]}"
  if [ ${#missing_ryaml[@]} -gt 0 ]; then printf '  - %s\n' "${missing_ryaml[@]}"; fi
  echo "Failed executions: ${#failed_funcs[@]}"
  if [ ${#failed_funcs[@]} -gt 0 ]; then printf '  - %s\n' "${failed_funcs[@]}"; fi
else
  # Single-function mode: check both files exist
  missing_any=0
  if [ ! -f "R/$ARG_function_name.b.R" ]; then
    echo "MISSING: R/$ARG_function_name.b.R"; missing_any=1
  fi
  if [ ! -f "jamovi/$ARG_function_name.r.yaml" ]; then
    echo "MISSING: jamovi/$ARG_function_name.r.yaml"; missing_any=1
  fi
  if [ $missing_any -eq 1 ]; then
    echo ""
    echo "=== Summary Report ==="
    echo "Processed successfully: 0"
    echo "Missing files for: $ARG_function_name"
    exit 1
  fi
  if bash ".claude/completions/update_refs.sh" "R/$ARG_function_name.b.R" "jamovi/$ARG_function_name.r.yaml" "jamovi/00refs.yaml"; then
    echo ""
    echo "=== Summary Report ==="
    echo "Processed successfully: 1"
    echo "Function: $ARG_function_name"
  else
    echo ""
    echo "=== Summary Report ==="
    echo "Failed executions: 1"
    echo "Function: $ARG_function_name"
    exit 1
  fi
fi
```
