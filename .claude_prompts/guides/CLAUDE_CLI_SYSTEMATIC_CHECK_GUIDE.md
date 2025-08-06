# Claude CLI Systematic Function Check Guide

## Quick Setup

The systematic function check has been set up as a custom Claude CLI prompt. Here's how to use it:

## 📁 Files Created

- `.claude_prompts/systematic_function_check.md` - The custom prompt
- `~/.config/claude-cli/prompts/systematic_function_check.md` - Claude CLI prompt
- `check_function.sh` - Single function checker script
- `check_all_functions.sh` - Batch function checker script

## 🚀 Usage

### Option 1: Claude Code Slash Commands (Recommended ⭐)
```bash
# Quick systematic check
/check-function lassocox

# Detailed code review  
/review-function lassocox

# Generate specific fixes
/fix-function lassocox schema

# Check entire module
/check-module

# Create new function
/create-function mynewfunction survival
```

### Option 2: Bash Scripts (Alternative)
```bash
# Check a specific function
./check_function.sh lassocox

# Check all priority functions
./check_all_functions.sh
```

### Option 3: Manual Claude CLI Usage
```bash
# Direct Claude CLI command (append custom prompt as system prompt)
claude -p --append-system-prompt "$(cat .claude_prompts/systematic_function_check.md)" \
    "@jamovi/lassocox.a.yaml @R/lassocox.b.R @jamovi/lassocox.r.yaml @jamovi/lassocox.u.yaml Check the lassocox function systematically following the systematic checklist"
```

## 📋 What Gets Checked

### Automatic Analysis
- ✅ **File integrity** - All required files present
- ✅ **Argument integration** - .a.yaml options used in .b.R  
- ✅ **Output population** - .r.yaml outputs implemented
- ✅ **Error handling** - Robust error management
- ✅ **Code quality** - Organization and efficiency
- ✅ **User experience** - UI design and help text

### Detailed Assessment
- **Unused options** in .a.yaml
- **Unpopulated outputs** in .r.yaml  
- **Missing error handling** patterns
- **Integration gaps** between files
- **User experience** issues
- **Code quality** problems

## 📊 Understanding Results

### Status Levels
- ✅ **PASS** - Function is production-ready
- ⚠️ **MINOR ISSUES** - Small improvements recommended
- ❌ **NEEDS WORK** - Significant issues requiring attention

### Key Sections in Output
- **Analysis Summary** - High-level metrics
- **Critical Issues** - Must fix before release
- **Minor Issues** - Recommended improvements  
- **Detailed Findings** - Specific technical analysis
- **Actionable Recommendations** - Step-by-step fixes

## 🔧 Workflow for Systematic Improvement

### 1. Initial Assessment
```bash
# Check current status
./check_function.sh lassocox
```

### 2. Address Issues
- Fix **Critical Issues** first (blocking problems)
- Implement **unused options** or remove them
- Populate **missing outputs**
- Add **error handling** where needed

### 3. Re-check After Fixes
```bash
# Verify improvements
./check_function.sh lassocox
```

### 4. Iterate Until Clean
Repeat steps 2-3 until function achieves ✅ **PASS** status

## 📈 Batch Processing Workflow

### 1. Run Batch Check
```bash
./check_all_functions.sh
```

### 2. Review Summary
```bash
# Check the summary report
cat reports/systematic_checks/BATCH_SUMMARY.md
```

### 3. Prioritize Functions
- **❌ Failed/Needs Work** - High priority
- **⚠️ Minor Issues** - Medium priority  
- **✅ Passed** - Ready for release

### 4. Work Through Functions Systematically
```bash
# Work on high-priority functions first
./check_function.sh problematic_function
# Fix issues, then re-check
./check_function.sh problematic_function
```

## 🎯 Pro Tips

### Efficient Development
- **Check early and often** - Run checks after significant changes
- **Fix incrementally** - Address one category of issues at a time
- **Focus on integration** - Most issues are between .yaml and .b.R files

### Common Issues to Watch For
- **Unused options** - Remove or implement them
- **Unpopulated outputs** - Add population code to .b.R
- **Missing error handling** - Add tryCatch blocks and validation
- **Poor user messages** - Replace cryptic R errors with helpful guidance

### Quality Gates
- **Development**: Fix all ❌ NEEDS WORK issues
- **Testing**: Address ⚠️ MINOR ISSUES  
- **Release**: Aim for ✅ PASS on all core functions

## 🛠️ Customization

### Add More Functions to Batch Check
Edit `check_all_functions.sh` and modify the `PRIORITY_FUNCTIONS` array:
```bash
PRIORITY_FUNCTIONS=(
    "lassocox"
    "your_new_function"
    # ... more functions
)
```

### Modify the Prompt
Edit `~/.config/claude-cli/prompts/systematic_function_check.md` to:
- Add domain-specific checks
- Modify output format
- Add new quality criteria

### Custom Reports
Individual reports are saved in `reports/systematic_checks/`:
- `function_name_check.md` - Detailed analysis
- `BATCH_SUMMARY.md` - Overview of all functions
- `batch_log.txt` - Processing log

## 🚨 Troubleshooting

### "Command not found: claude"
Install Claude CLI:
```bash
# Follow Claude CLI installation instructions
# Usually: pip install claude-cli or similar
```

### "Prompt not found"
Ensure the prompt is in the right location:
```bash
ls ~/.config/claude-cli/prompts/systematic_function_check.md
```

### "Missing files" errors
The scripts check for required files. If files are missing:
- Verify function name spelling
- Check file locations (jamovi/ and R/ directories)
- Ensure you're running from module root directory

### Reports not generated
Check permissions and create directories:
```bash
mkdir -p reports/systematic_checks
chmod +x check_function.sh check_all_functions.sh
```

## 📚 Advanced Usage

### Custom Analysis
```bash
# Focus on specific aspects
claude --prompt ~/.config/claude-cli/prompts/systematic_function_check.md \
    @R/lassocox.b.R \
    "Focus only on error handling patterns in this function"
```

### Integration with Git Workflow
```bash
# Check before committing
git add .
./check_function.sh modified_function
# Only commit if ✅ PASS or acceptable ⚠️ status
git commit -m "Improve function quality"
```

This systematic approach will help you maintain high-quality, robust jamovi functions that provide excellent user experiences! 🎯