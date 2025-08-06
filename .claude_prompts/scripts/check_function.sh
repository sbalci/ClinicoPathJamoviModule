#!/bin/bash

# Systematic Function Check Script for Claude CLI
# Usage: ./check_function.sh <function_name>

if [ $# -eq 0 ]; then
    echo "Usage: $0 <function_name>"
    echo "Example: $0 lassocox"
    echo ""
    echo "This will systematically check the jamovi function using Claude CLI"
    exit 1
fi

FUNCTION_NAME="$1"

# Check if required files exist
REQUIRED_FILES=(
    "jamovi/${FUNCTION_NAME}.a.yaml"
    "R/${FUNCTION_NAME}.b.R" 
    "jamovi/${FUNCTION_NAME}.r.yaml"
    "jamovi/${FUNCTION_NAME}.u.yaml"
)

echo "🔍 Checking if required files exist for function: $FUNCTION_NAME"

MISSING_FILES=()
for file in "${REQUIRED_FILES[@]}"; do
    if [ ! -f "$file" ]; then
        MISSING_FILES+=("$file")
        echo "❌ Missing: $file"
    else
        echo "✅ Found: $file"
    fi
done

if [ ${#MISSING_FILES[@]} -gt 0 ]; then
    echo ""
    echo "❌ Cannot proceed - missing required files:"
    printf '%s\n' "${MISSING_FILES[@]}"
    exit 1
fi

echo ""
echo "🚀 Running systematic function check with Claude CLI..."
echo "Function: $FUNCTION_NAME"
echo ""

# Read the custom prompt
PROMPT_FILE="$HOME/.config/claude-cli/prompts/systematic_function_check.md"
if [ ! -f "$PROMPT_FILE" ]; then
    PROMPT_FILE=".claude_prompts/systematic_function_check.md"
fi

if [ -f "$PROMPT_FILE" ]; then
    SYSTEM_PROMPT=$(cat "$PROMPT_FILE")
else
    echo "⚠️ Custom prompt not found, using basic prompt"
    SYSTEM_PROMPT="You are an expert jamovi module developer. Perform a comprehensive quality assessment of this jamovi function, checking file integration, unused options, unpopulated outputs, error handling, and code quality."
fi

# Create the Claude CLI command with file inclusions
claude -p --append-system-prompt "$SYSTEM_PROMPT" \
    "@jamovi/${FUNCTION_NAME}.a.yaml @R/${FUNCTION_NAME}.b.R @jamovi/${FUNCTION_NAME}.r.yaml @jamovi/${FUNCTION_NAME}.u.yaml Perform a systematic quality check of the jamovi function '${FUNCTION_NAME}'. Analyze all the provided files and give me a comprehensive assessment following the systematic checklist. Focus on integration between files, unused options, unpopulated outputs, error handling, and overall quality. Be specific and actionable in your recommendations. Replace [FUNCTION_NAME] in your response with '${FUNCTION_NAME}'."

echo ""
echo "✅ Systematic check completed for: $FUNCTION_NAME"