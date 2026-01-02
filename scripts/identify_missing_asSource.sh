#!/bin/bash
# identify_missing_asSource.sh
# Identifies jamovi functions missing asSource() methods and their variable options

echo "=================================================="
echo "Identifying Functions Missing asSource() Methods"
echo "=================================================="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

TOTAL=0
MISSING=0
COMPLETE=0

# Get all .a.yaml files
for yaml_file in jamovi/*.a.yaml; do
    # Extract function name
    func=$(basename "$yaml_file" .a.yaml)
    b_file="R/${func}.b.R"

    # Check if .b.R file exists
    if [[ ! -f "$b_file" ]]; then
        continue
    fi

    TOTAL=$((TOTAL + 1))

    # Check if function uses variable options
    if ! grep -qE '^\s*-\s*name:\s*(vars|dep|grouping|outcome|group|rows|cols|responseVar|groupVar|raters)' "$yaml_file"; then
        continue
    fi

    # Check if asSource exists
    if grep -q "asSource.*=.*function" "$b_file"; then
        COMPLETE=$((COMPLETE + 1))
        echo -e "${GREEN}✅${NC} $func"
    else
        MISSING=$((MISSING + 1))
        echo -e "${RED}❌${NC} $func"

        # Show variable options (BSD grep compatible)
        echo -n "   Variables: "
        grep -E '^\s*-\s*name:\s*(vars|dep|grouping|outcome|group|rows|cols|responseVar|groupVar|raters|layers|covariates|factors|predictors)' "$yaml_file" | sed 's/.*name:\s*//' | head -5 | tr '\n' ', ' | sed 's/,$//'
        echo ""

        # Check file size to estimate complexity
        lines=$(wc -l < "$b_file")
        if (( lines > 2000 )); then
            echo -e "   ${YELLOW}⚠️  Large file ($lines lines)${NC}"
        fi

        echo ""
    fi
done

echo "=================================================="
echo "Summary"
echo "=================================================="
echo "Total functions with variables: $TOTAL"
echo -e "${GREEN}Complete (with asSource): $COMPLETE${NC}"
echo -e "${RED}Missing asSource: $MISSING${NC}"
echo ""
echo "Progress: $(awk "BEGIN {printf \"%.1f\", ($COMPLETE/$TOTAL)*100}")%"
echo ""
echo "Estimated time to complete: $(awk "BEGIN {printf \"%.1f\", ($MISSING*4)/60}") hours (at 4 min/function)"
