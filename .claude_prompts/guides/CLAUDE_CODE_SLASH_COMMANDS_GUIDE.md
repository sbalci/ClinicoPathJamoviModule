# Claude Code Slash Commands for Jamovi Development

## 🎯 **Custom Slash Commands Created**

I've created 5 powerful slash commands specifically for your jamovi module development workflow:

### **1. `/check-function <function_name>`**
**Purpose**: Systematic quality check of a jamovi function  
**Usage**: `/check-function lassocox`

**What it does:**
- ✅ Analyzes all 4 jamovi files (.a.yaml, .b.R, .r.yaml, .u.yaml)
- ✅ Checks argument integration (.a.yaml ↔ .b.R)
- ✅ Validates output population (.r.yaml ↔ .b.R)
- ✅ Assesses error handling and code quality
- ✅ Provides actionable fixes with exact code changes

**Perfect for:** Daily development, before committing changes, debugging integration issues

### **2. `/review-function <function_name>`**
**Purpose**: Detailed code review focusing on quality and best practices  
**Usage**: `/review-function lassocox`

**What it does:**
- 🔍 Architectural analysis and design patterns
- 🔍 Performance and scalability assessment
- 🔍 Code maintainability and readability review
- 🔍 Security and robustness evaluation
- 🔍 User experience and accessibility check

**Perfect for:** Code review process, refactoring decisions, performance optimization

### **3. `/fix-function <function_name> [issue_type]`**
**Purpose**: Generate specific, implementable fixes for identified issues  
**Usage**: `/fix-function lassocox schema`

**What it does:**
- 🔧 Identifies specific issues and their root causes
- 🔧 Provides exact code changes needed
- 🔧 Gives step-by-step implementation instructions
- 🔧 Includes validation steps and testing approach
- 🔧 Prevention strategies for similar issues

**Perfect for:** Issue resolution, implementing systematic check recommendations

### **4. `/check-module`**
**Purpose**: Batch quality assessment of all priority functions  
**Usage**: `/check-module`

**What it does:**
- 📊 Systematic check of multiple key functions
- 📊 Module-wide quality dashboard
- 📊 Priority-ranked issue identification
- 📊 Release readiness assessment
- 📊 Common pattern recognition

**Perfect for:** Release preparation, module-wide quality assessment, project planning

### **5. `/create-function <function_name> [function_type]`**
**Purpose**: Scaffold a new jamovi function with all required files  
**Usage**: `/create-function mynewanalysis survival`

**What it does:**
- 🚀 Generates complete 4-file jamovi function structure
- 🚀 Follows ClinicoPath module patterns and conventions
- 🚀 Includes comprehensive error handling template
- 🚀 Provides educational content framework
- 🚀 Sets up proper R6 class architecture

**Perfect for:** Starting new functions, maintaining consistency, rapid prototyping

## 🔄 **Development Workflow with Slash Commands**

### **Daily Development Cycle**
```bash
# 1. Check function status
/check-function myfunction

# 2. Review code quality  
/review-function myfunction

# 3. Fix identified issues
/fix-function myfunction integration

# 4. Re-check after fixes
/check-function myfunction
```

### **Release Preparation**
```bash
# 1. Module-wide assessment
/check-module

# 2. Fix critical issues in priority order
/fix-function function1
/fix-function function2

# 3. Individual function reviews
/review-function critical_function

# 4. Final validation
/check-module
```

### **New Function Development**
```bash
# 1. Create function scaffold
/create-function newanalysis survival

# 2. Implement custom logic
# ... edit generated files ...

# 3. Check integration
/check-function newanalysis

# 4. Review and polish
/review-function newanalysis
```

## 🎨 **Key Advantages of Slash Commands**

### **🚀 Speed & Efficiency**
- **Instant access** - No script setup or file paths needed
- **Context-aware** - Automatically includes relevant files
- **Integrated workflow** - Works within your Claude Code session

### **🎯 Precision & Focus**
- **Targeted analysis** - Each command has specific purpose
- **Actionable output** - Provides copy-paste ready fixes
- **Consistent format** - Standardized analysis across all functions

### **📈 Quality Assurance**
- **Best practices enforcement** - Built-in ClinicoPath patterns
- **Comprehensive coverage** - All aspects from architecture to UX
- **Prevention-focused** - Helps avoid common jamovi pitfalls

### **🔄 Iterative Improvement**
- **Quick feedback loops** - Fast check → fix → recheck cycles
- **Progress tracking** - Clear before/after comparisons
- **Learning integration** - Builds knowledge of jamovi best practices

## 💡 **Pro Tips for Maximum Effectiveness**

### **Combine Commands Strategically**
```bash
# Comprehensive function assessment
/check-function myfunction     # Find issues
/fix-function myfunction       # Get specific fixes  
/review-function myfunction    # Ensure quality
/check-function myfunction     # Validate fixes
```

### **Use for Different Development Phases**

**📝 Planning Phase:**
- `/create-function` - Generate consistent scaffolds
- `/check-module` - Understand current state

**🔧 Development Phase:**
- `/check-function` - Catch issues early
- `/fix-function` - Quick resolution guidance

**🏁 Review Phase:**
- `/review-function` - Code quality assessment
- `/check-module` - Release readiness validation

### **Leverage for Learning**
- **Pattern Recognition** - See common issues across functions
- **Best Practices** - Learn jamovi-specific conventions
- **Error Prevention** - Understand integration pitfalls

## 🎯 **Getting Started**

### **1. Start with a Known Function**
```bash
/check-function lassocox
```
This will show you the command format and type of analysis provided.

### **2. Try Module Assessment**
```bash
/check-module
```
Get an overview of your entire module's health.

### **3. Experiment with Function Creation**
```bash
/create-function testfunction descriptive
```
See how a complete jamovi function should be structured.

### **4. Use for Active Development**
Pick a function you're working on and run:
```bash
/check-function yourfunction
/fix-function yourfunction
```

## 🚀 **Why This Approach Is Powerful**

### **Compared to Manual Checking:**
- ⚡ **10x faster** than manual file inspection
- 🎯 **More comprehensive** than human review alone
- 🔄 **Consistent standards** across all functions
- 📚 **Educational** - teaches best practices

### **Compared to Generic Code Review:**
- 🎨 **Jamovi-specific** - Understands .yaml ↔ .b.R integration
- 🏗️ **Architecture-aware** - Knows R6 class patterns
- 🎯 **Domain-focused** - Understands statistical/clinical context
- 🔧 **Actionable** - Provides exact fixes, not just suggestions

This system transforms jamovi module development from manual, error-prone processes into systematic, quality-assured workflows! 🎉