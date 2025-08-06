#!/bin/bash

# Batch Systematic Function Check Script for Claude CLI
# This script checks multiple priority functions systematically

echo "🎯 BATCH SYSTEMATIC FUNCTION CHECK"
echo "=================================="

# Priority functions to check (add/remove as needed)
PRIORITY_FUNCTIONS=(
    "lassocox"
    "survival" 
    "multisurvival"
    "simonmakuch"
    "decisiongraph"
    "decision"
    "crosstable"
    "tableone"
    "gtsummary"
    "jjbarstats"
    "jjscatterstats"
    "coxdiagnostics"
    "agreement"
    "classification"
)

# Create reports directory
mkdir -p reports/systematic_checks

TOTAL_FUNCTIONS=${#PRIORITY_FUNCTIONS[@]}
CHECKED_FUNCTIONS=0
PASSED_FUNCTIONS=0
FAILED_FUNCTIONS=0

echo "📋 Found $TOTAL_FUNCTIONS priority functions to check"
echo ""

# Function to extract status from Claude output (simplified)
extract_status() {
    local output="$1"
    if echo "$output" | grep -q "✅ PASS\|READY"; then
        echo "PASS"
    elif echo "$output" | grep -q "⚠️ MINOR ISSUES\|NEEDS MINOR FIXES"; then
        echo "MINOR_ISSUES"  
    elif echo "$output" | grep -q "❌ NEEDS WORK\|NEEDS MAJOR WORK"; then
        echo "NEEDS_WORK"
    else
        echo "UNKNOWN"
    fi
}

# Check each function
for FUNCTION_NAME in "${PRIORITY_FUNCTIONS[@]}"; do
    echo "🔍 Checking function: $FUNCTION_NAME"
    echo "$(date): Starting check for $FUNCTION_NAME" >> reports/systematic_checks/batch_log.txt
    
    # Check if required files exist
    REQUIRED_FILES=(
        "jamovi/${FUNCTION_NAME}.a.yaml"
        "R/${FUNCTION_NAME}.b.R" 
        "jamovi/${FUNCTION_NAME}.r.yaml"
        "jamovi/${FUNCTION_NAME}.u.yaml"
    )
    
    MISSING_FILES=()
    for file in "${REQUIRED_FILES[@]}"; do
        if [ ! -f "$file" ]; then
            MISSING_FILES+=("$file")
        fi
    done
    
    if [ ${#MISSING_FILES[@]} -gt 0 ]; then
        echo "  ❌ MISSING FILES - Skipping"
        echo "Missing files for $FUNCTION_NAME: ${MISSING_FILES[*]}" >> reports/systematic_checks/batch_log.txt
        ((FAILED_FUNCTIONS++))
        continue
    fi
    
    # Run the systematic check
    echo "  🤖 Running Claude analysis..."
    
    # Read the custom prompt
    PROMPT_FILE="$HOME/.config/claude-cli/prompts/systematic_function_check.md"
    if [ ! -f "$PROMPT_FILE" ]; then
        PROMPT_FILE=".claude_prompts/systematic_function_check.md"
    fi
    
    if [ -f "$PROMPT_FILE" ]; then
        SYSTEM_PROMPT=$(cat "$PROMPT_FILE")
    else
        SYSTEM_PROMPT="You are an expert jamovi module developer. Perform a comprehensive quality assessment of this jamovi function, checking file integration, unused options, unpopulated outputs, error handling, and code quality."
    fi
    
    OUTPUT=$(claude -p --append-system-prompt "$SYSTEM_PROMPT" \
        "@jamovi/${FUNCTION_NAME}.a.yaml @R/${FUNCTION_NAME}.b.R @jamovi/${FUNCTION_NAME}.r.yaml @jamovi/${FUNCTION_NAME}.u.yaml Perform a systematic quality check of the jamovi function '${FUNCTION_NAME}'. Analyze all the provided files and give me a comprehensive assessment following the systematic checklist. Focus on integration between files, unused options, unpopulated outputs, error handling, and overall quality. Be specific and actionable in your recommendations. Replace [FUNCTION_NAME] in your response with '${FUNCTION_NAME}'." 2>&1)
    
    # Save individual report
    echo "$OUTPUT" > "reports/systematic_checks/${FUNCTION_NAME}_check.md"
    
    # Extract status
    STATUS=$(extract_status "$OUTPUT")
    
    case "$STATUS" in
        "PASS")
            echo "  ✅ PASS"
            ((PASSED_FUNCTIONS++))
            ;;
        "MINOR_ISSUES")
            echo "  ⚠️ MINOR ISSUES"
            ;;
        "NEEDS_WORK") 
            echo "  ❌ NEEDS WORK"
            ((FAILED_FUNCTIONS++))
            ;;
        *)
            echo "  ❓ UNKNOWN STATUS"
            ;;
    esac
    
    ((CHECKED_FUNCTIONS++))
    echo "  📄 Report saved: reports/systematic_checks/${FUNCTION_NAME}_check.md"
    echo ""
    
    # Brief pause to avoid overwhelming the API
    sleep 2
done

# Generate summary
echo "📊 BATCH CHECK SUMMARY"
echo "====================="
echo "Total functions: $TOTAL_FUNCTIONS"
echo "Successfully checked: $CHECKED_FUNCTIONS"
echo "✅ Passed: $PASSED_FUNCTIONS"  
echo "⚠️ Issues found: $((CHECKED_FUNCTIONS - PASSED_FUNCTIONS - FAILED_FUNCTIONS))"
echo "❌ Failed/Missing: $FAILED_FUNCTIONS"
echo ""
echo "📁 Individual reports saved in: reports/systematic_checks/"
echo "📋 Batch log saved in: reports/systematic_checks/batch_log.txt"

# Create summary report
SUMMARY_FILE="reports/systematic_checks/BATCH_SUMMARY.md"
cat > "$SUMMARY_FILE" << EOF
# Batch Systematic Function Check Summary

**Date**: $(date)
**Functions Checked**: $CHECKED_FUNCTIONS / $TOTAL_FUNCTIONS

## Results Overview

- ✅ **Passed**: $PASSED_FUNCTIONS functions
- ⚠️ **Minor Issues**: $((CHECKED_FUNCTIONS - PASSED_FUNCTIONS - FAILED_FUNCTIONS)) functions  
- ❌ **Failed/Missing**: $FAILED_FUNCTIONS functions

## Functions Checked

EOF

# Add function list to summary
for FUNCTION_NAME in "${PRIORITY_FUNCTIONS[@]}"; do
    if [ -f "reports/systematic_checks/${FUNCTION_NAME}_check.md" ]; then
        STATUS=$(extract_status "$(cat reports/systematic_checks/${FUNCTION_NAME}_check.md)")
        case "$STATUS" in
            "PASS") echo "- ✅ **$FUNCTION_NAME** - Ready" >> "$SUMMARY_FILE" ;;
            "MINOR_ISSUES") echo "- ⚠️ **$FUNCTION_NAME** - Minor improvements needed" >> "$SUMMARY_FILE" ;;
            "NEEDS_WORK") echo "- ❌ **$FUNCTION_NAME** - Requires attention" >> "$SUMMARY_FILE" ;;
            *) echo "- ❓ **$FUNCTION_NAME** - Status unclear" >> "$SUMMARY_FILE" ;;
        esac
    else
        echo "- 🚫 **$FUNCTION_NAME** - Missing files or check failed" >> "$SUMMARY_FILE"
    fi
done

cat >> "$SUMMARY_FILE" << EOF

## Next Steps

1. Review individual function reports in this directory
2. Address functions marked with ❌ (high priority)
3. Improve functions marked with ⚠️ (medium priority)
4. Implement missing functions marked with 🚫

## Report Files

EOF

ls reports/systematic_checks/*.md | sed 's/^/- /' >> "$SUMMARY_FILE"

echo "📄 Summary report: $SUMMARY_FILE"
echo ""
echo "🎯 Use these reports to systematically improve your jamovi module!"