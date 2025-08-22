# Claude Code Assistant Files

This directory contains files for working with Claude Code (claude.ai/code) and systematic function checking.

## Directory Structure

### `/guides/`
User guides and documentation for Claude Code usage:
- `CLAUDE_CODE_SLASH_COMMANDS_GUIDE.md` - Guide to Claude Code slash commands
- `CLAUDE_CLI_SYSTEMATIC_CHECK_GUIDE.md` - Guide for systematic function checking
- `SYSTEMATIC_CHECK_USAGE_GUIDE.md` - Usage instructions for systematic checks

### `/scripts/`
Shell scripts for automated function checking:
- `check_all_functions.sh` - Script to check all jamovi functions
- `check_function.sh` - Script to check a single jamovi function

### `/systematic_checks/`
R scripts and prompts for systematic function validation:
- `RUN_SYSTEMATIC_CHECKS.R` - Main R script for running systematic checks
- `systematic_check_implementation.R` - Implementation details for checks
- `SYSTEMATIC_FUNCTION_CHECK_PROMPT.md` - Prompt template for function checks

### `/function_docs/`
Function-specific documentation and improvement guides:
- `advancedraincloud_clinical_features.md` - Clinical features documentation
- `advancedraincloud_improvements.md` - Implementation improvement notes
- `advancedraincloud_testing_guide.md` - Testing guide for advancedraincloud

## Usage

These files support the development and quality assurance processes for the ClinicoPath jamovi module. They provide:

1. **Systematic Quality Checks**: Automated validation of jamovi function integration
2. **Development Guides**: Instructions for using Claude Code effectively
3. **Function Documentation**: Detailed documentation for complex functions
4. **Testing Scripts**: Automated testing and validation tools

## Integration with CLAUDE.md

These files complement the main `CLAUDE.md` file in the project root, providing specialized tools and documentation for advanced development workflows.
