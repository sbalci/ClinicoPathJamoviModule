---
name: update-refs
description: Parse jamovi .b.R to find used packages and sync them with .r.yaml and 00refs.yaml
interactive: true
args:
  function_name:
    description: Jamovi function name (e.g., tableone) or --all to process every R/*.b.R
    required: true
usage: /update-refs <function_name>|--all
---

# Update References for Jamovi Function

Synchronize references between `.b.R`, `.r.yaml`, and `00refs.yaml`.

## Analysis Target

Function: **`$ARG_function_name`**

### File Paths

Assuming standard module layout:

```
jamovi/$ARG_function_name.r.yaml
R/$ARG_function_name.b.R
00refs.yaml
```

### Steps Performed

1. **Detect packages in `.b.R`**
   - Scan for `library()`, `require()`, and `pkg::` usage.
   - Extract unique package names.

2. **Ensure package refs exist in `00refs.yaml`**
   - Check whether each package is already listed under `refs:`.
   - If not present, append placeholder entries like:

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
   - Append `- packagename` if not already listed.

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
readarray -t PKGS < <(printf "%s\n" "$PKGS_RAW" | awk '!seen[$0]++' | sed '/^$/d')

if [[ ${#PKGS[@]} -eq 0 ]]; then
  echo "No packages detected in $B_R. Nothing to do."
  exit 0
fi

echo "Detected packages in $B_R:"
for p in "${PKGS[@]}"; do echo "  - $p"; done

# --- 2) Ensure 00refs.yaml contains each package key --------------------------
# We assume 00refs.yaml has a top-level "refs:" mapping.
ensure_refs_yaml_key() {
  local key="$1"
  # Look for key under up-to-4-space indent after 'refs:'
  if awk '
    BEGIN{found_refs=0}
    /^refs:[[:space:]]*$/ {found_refs=1; next}
    found_refs && $0 ~ "^[[:space:]]{2,}"key":" {exit 0}
    END{exit 1}
  ' key="$key" "$REFS_YAML"; then
    return 0
  fi

  # If 'refs:' missing entirely, prepend it
  if ! grep -Eq '^refs:\s*$' "$REFS_YAML"; then
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
ensure_ryaml_refs_section() {
  if grep -Eq '^refs:\s*$' "$R_YAML"; then
    return 0
  fi
  echo -e "\nrefs:" >> "$R_YAML"
}

add_ref_to_ryaml_if_missing() {
  local key="$1"
  if grep -Eq "^[[:space:]]*-[[:space:]]*['\"]?${key}['\"]?\s*$" "$R_YAML"; then
    return 0
  fi
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
BASH
  chmod +x ".claude/completions/update_refs.sh"
fi

if [ "$ARG_function_name" = "--all" ]; then
  echo "Running update-refs for all functions under R/*.b.R …"
  shopt -s nullglob
  for B in R/*.b.R; do
    fn="$(basename "$B" .b.R)"
    echo "==> $fn"
    bash ".claude/completions/update_refs.sh" "R/${fn}.b.R" "jamovi/${fn}.r.yaml" "00refs.yaml" || {
      echo "WARN: failed for ${fn} (continuing)"
    }
  done
else
  bash ".claude/completions/update_refs.sh" "R/$ARG_function_name.b.R" "jamovi/$ARG_function_name.r.yaml" "00refs.yaml"
fi
```

> The block above self-installs `.claude/completions/update_refs.sh` the first time you run `/update-refs`. Subsequent runs will reuse the same script.

### Output

The script reports:
- Packages detected in .b.R
- New placeholders added to 00refs.yaml
- New refs appended to .r.yaml
- Backup files created

---

### Example Usage

Run for one function:

```bash
/update-refs tableone
```

This will execute:

```bash
bash .claude/completions/update_refs.sh "R/tableone.b.R" "jamovi/tableone.r.yaml" "00refs.yaml"
```

Example output:

```
Detected packages in R/tableone.b.R:
  - gtsummary
  - arsenal
All present in 00refs.yaml ✓
Added 'gtsummary' to jamovi/tableone.r.yaml refs list ✓
Backups:
 - jamovi/tableone.r.yaml.bak.20251008-165200
 - 00refs.yaml.bak.20251008-165200
```

Run for all functions in the module:

```bash
/update-refs --all
```
