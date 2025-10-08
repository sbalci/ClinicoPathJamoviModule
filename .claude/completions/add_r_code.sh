#!/usr/bin/env bash
set -euo pipefail

FN="${1:?Function name required (e.g., pcaBiplot)}"
shift || true
FORCE=0
for arg in "$@"; do
  if [[ "$arg" == "--force" ]]; then FORCE=1; fi
done
B_R="R/${FN}.b.R"
A_YAML="jamovi/${FN}.a.yaml"
R_YAML="jamovi/${FN}.r.yaml"

timestamp() { date +"%Y%m%d-%H%M%S"; }

backup_file() {
  local f="$1"
  [[ -f "$f" ]] || return 0
  cp -f "$f" "${f}.bak.$(timestamp)"
}

ensure_file() {
  local f="$1"; local msg="$2"
  if [[ ! -f "$f" ]]; then
    echo "MISSING: $f"
    echo "$msg"
    exit 1
  fi
}

# 0) Preconditions
ensure_file "$B_R"     "Expected analysis implementation at $B_R"
ensure_file "$A_YAML"  "Expected analysis definition at $A_YAML"
ensure_file "$R_YAML"  "Expected results definition at $R_YAML"

backup_file "$B_R"
backup_file "$A_YAML"
backup_file "$R_YAML"

# Pre-parse option defaults from a.yaml into JSON (name -> default)
AOPTS_JSON="$(Rscript --vanilla - "$A_YAML" <<'RS'
suppressWarnings(suppressMessages({ library(yaml); library(jsonlite) }))
f <- commandArgs(trailingOnly=TRUE)[1]
y <- tryCatch(read_yaml(f), error=function(e) NULL)
items <- list()
# a.yaml may be: a list of options OR a map with $options
if (!is.null(y)) {
  if (is.list(y) && !is.null(y$options)) {
    items <- y$options
  } else if (is.list(y)) {
    items <- y
  }
}
# Normalize into a named list of defaults
defs <- list()
if (is.list(items)) {
  for (it in items) {
    nm <- tryCatch(it$name, error=function(e) NULL)
    if (!is.null(nm) && nchar(as.character(nm))>0) {
      defs[[as.character(nm)]] <- if (!is.null(it$default)) it$default else NULL
    }
  }
}
# Coerce logical NA/NULL to JSON null, keep vectors as-is
cat(jsonlite::toJSON(defs, auto_unbox=TRUE, null="null"))
RS
)"

# Write the code block to a temp file to avoid escaping nightmares
TEMP_BLOCK="$(mktemp)"
cat > "$TEMP_BLOCK" <<'CODEBLOCK'
.generateRCode = function() {
    if (!self$options$showRCode)
        return(invisible(NULL))

    # Collect current options from jamovi
    opts_list <- as.list(self$options)
    opts_list[c(".meta", "debug", ".allow", ".hidden")] <- NULL

    # Ensure jsonlite present for serialization
    if (!requireNamespace("jsonlite", quietly = TRUE))
        utils::install.packages("jsonlite")
    opts_json <- jsonlite::toJSON(opts_list, auto_unbox = TRUE, null = "null")

    # Build R script text (neutral scaffold)
    r_code <- sprintf("
# ============================================
# Reproducible R Scaffold (auto-generated)
# ============================================
# This script is self-contained and portable.

# Dependencies
if (!requireNamespace('jsonlite', quietly=TRUE)) install.packages('jsonlite')
if (!requireNamespace('ggplot2', quietly=TRUE)) install.packages('ggplot2')
library(jsonlite)
library(ggplot2)

# Options captured from jamovi
opts <- jsonlite::fromJSON('%s')

# Data contract: provide your data.frame as mydata
# Example: mydata <- read.csv('file.csv')

# Add your analysis code here based on opts
", opts_json)

    r_code_html <- paste0(
      "<div style='background:#f5f5f5;padding:1em;border:1px solid #ddd;border-radius:4px;'>",
      "<h4>Copy-Ready R Code</h4>",
      "<pre style='background:white;padding:1em;overflow-x:auto;border:1px solid #ccc;'><code>",
      htmltools::htmlEscape(r_code),
      "</code></pre>",
      "</div>"
    )
    self$results$rCode$setContent(r_code_html)
}
CODEBLOCK

ADD_BLOCK="$(cat "$TEMP_BLOCK")"
rm "$TEMP_BLOCK"

# ==========================================
# 1) Patch .b.R
# ==========================================

# Check if .generateRCode already exists
if grep -qE '\.generateRCode\s*=\s*function' "$B_R"; then
  if [[ "$FORCE" -eq 1 ]]; then
    echo "Found existing .generateRCode(); --force enabled -> replacing with latest template."
    tmp_rm="$(mktemp)"
    # Remove the existing .generateRCode() function block (brace-depth aware)
    awk '
      BEGIN{inblk=0; depth=0}
      {
        if (inblk==0 && $0 ~ /\.generateRCode[[:space:]]*=[[:space:]]*function/) {
          inblk=1
          line_copy = $0
          opn = gsub(/[(]/, "(", line_copy)
          line_copy = $0
          cls = gsub(/[)]/, ")", line_copy)
          depth += opn - cls
          next
        }
        if (inblk==1) {
          line_copy = $0
          opn = gsub(/\{/, "{", line_copy)
          line_copy = $0
          cls = gsub(/\}/, "}", line_copy)
          depth += opn - cls
          if (depth<=0) { inblk=0; next } else { next }
        }
        print
      }
    ' "$B_R" > "$tmp_rm"
    mv "$tmp_rm" "$B_R"
  else
    echo ".b.R already has .generateRCode(); leaving as-is (use --force to overwrite)."
    SKIP_B_INSERT=1
  fi
fi

# Insert .generateRCode inside private list and add call in .run()
: "${SKIP_B_INSERT:=0}"
if [[ "$SKIP_B_INSERT" -eq 0 ]]; then
  # Write block to temp file for awk to read
  BLOCK_FILE="$(mktemp)"
  echo "$ADD_BLOCK" > "$BLOCK_FILE"

  tmp="$(mktemp)"
  awk -v blockfile="$BLOCK_FILE" '
    BEGIN {
      inprivate=0; inrun=0; depth=0
      # Read the block from file
      while ((getline line < blockfile) > 0) {
        block = block line "\n"
      }
      close(blockfile)
      inserted_block=0
      inserted_call=0
    }
    {
      # Track when we enter private = list(
      if ($0 ~ /private[[:space:]]*=[[:space:]]*list[[:space:]]*\(/ && inprivate==0) {
        inprivate=1
        print
        next
      }

      # Track when we enter .run = function()
      if (inprivate==1 && $0 ~ /\.run[[:space:]]*=[[:space:]]*function/) {
        inrun=1
      }

      # Find the end of .run() method to insert the call
      if (inrun==1 && inserted_call==0) {
        # Look for the closing brace of .run with proper depth tracking
        line_copy = $0
        opn = gsub(/\{/, "{", line_copy)
        line_copy = $0
        cls = gsub(/\}/, "}", line_copy)
        depth += opn - cls

        # When we close .run (depth becomes 0), insert call before the closing brace
        if (depth == 0 && $0 ~ /^[[:space:]]*\}[[:space:]]*$/) {
          print ""
          print "            # Generate R code if enabled"
          print "            if (self$options$showRCode) {"
          print "                private$.generateRCode()  # TODO: Add parameters based on your analysis"
          print "            }"
          inserted_call=1
          inrun=0
        }
      }

      # Find the end of private list to insert .generateRCode
      if (inprivate==1 && inserted_block==0 && $0 ~ /^[[:space:]]*\)\s*\)?\s*$/) {
        # This is the closing of private list
        # Insert the block before it with proper comma
        print "        },"
        print ""
        printf "%s", block
        inserted_block=1
      }

      print
    }
  ' "$B_R" > "$tmp"
  mv "$tmp" "$B_R"
  rm "$BLOCK_FILE"
  echo "Inserted .generateRCode() into private list of $B_R and added call in .run()"
fi

# ==========================================
# 2) Patch .a.yaml - insert BEFORE ...
# ==========================================

ensure_show_rcode() {
  local f="$1"
  if grep -Eq 'name:[[:space:]]*showRCode' "$f"; then
    echo ".a.yaml already has showRCode"
    return 0
  fi

  # Find line number before ... marker
  line_before_dots=$(grep -n '^\.\.\.[ ]*$' "$f" | head -1 | cut -d: -f1)

  if [[ -z "$line_before_dots" ]]; then
    echo "WARNING: No ... marker found in $f. Appending showRCode at end."
    cat >> "$f" <<'YAML'

    - name: showRCode
      title: Show R Code (Reproducible)
      type: Bool
      default: false
      description:
        R: >
          Generate copy-ready R code using upstream packages (stats, mixOmics, MASS)
          instead of jamovi wrappers. Useful for reproducing analysis in R scripts,
          learning the underlying implementation, and sharing code with non-jamovi users.
YAML
    return 0
  fi

  # Insert before ... marker
  tmp="$(mktemp)"
  awk -v target="$line_before_dots" '
    NR == target {
      print ""
      print "    - name: showRCode"
      print "      title: Show R Code (Reproducible)"
      print "      type: Bool"
      print "      default: false"
      print "      description:"
      print "        R: >"
      print "          Generate copy-ready R code using upstream packages (stats, mixOmics, MASS)"
      print "          instead of jamovi wrappers. Useful for reproducing analysis in R scripts,"
      print "          learning the underlying implementation, and sharing code with non-jamovi users."
      print ""
    }
    { print }
  ' "$f" > "$tmp"
  mv "$tmp" "$f"
  echo "Inserted showRCode option before ... in $f"
}
ensure_show_rcode "$A_YAML"

# ==========================================
# 3) Patch .r.yaml - insert BEFORE refs:
# ==========================================

ensure_r_html() {
  local f="$1"
  if grep -Eq 'name:[[:space:]]*rCode' "$f"; then
    echo ".r.yaml already has rCode Html"
    return 0
  fi

  # Find line number before refs: marker
  line_before_refs=$(grep -n '^refs:[ ]*$' "$f" | head -1 | cut -d: -f1)

  if [[ -z "$line_before_refs" ]]; then
    # No refs found, check for ... marker
    line_before_dots=$(grep -n '^\.\.\.[ ]*$' "$f" | head -1 | cut -d: -f1)
    if [[ -n "$line_before_dots" ]]; then
      line_before_refs="$line_before_dots"
    else
      echo "WARNING: No refs: or ... marker found in $f. Appending rCode at end."
      cat >> "$f" <<'YAML'

    - name: rCode
      title: Reproducible R Code
      type: Html
      visible: (showRCode)
YAML
      return 0
    fi
  fi

  # Insert before refs: or ...
  tmp="$(mktemp)"
  awk -v target="$line_before_refs" '
    NR == target {
      print ""
      print "    - name: rCode"
      print "      title: Reproducible R Code"
      print "      type: Html"
      print "      visible: (showRCode)"
      print ""
    }
    { print }
  ' "$f" > "$tmp"
  mv "$tmp" "$f"
  echo "Inserted rCode output before refs: in $f"
}
ensure_r_html "$R_YAML"

echo ""
echo "=== Summary Report ==="
echo "Patched: $B_R"
echo "  - Added .generateRCode() in private list"
echo "  - Added call to private$.generateRCode() in .run()"
echo "Patched: $A_YAML"
echo "  - Added showRCode option (before ...)"
echo "Patched: $R_YAML"
echo "  - Added rCode output (before refs:)"
echo "Backups created with .bak.$(timestamp) suffix."
