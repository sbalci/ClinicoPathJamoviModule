---
name: review-function
description: Detailed code review of a specific jamovi function
interactive: true
args:
  function_name:
    description: Name of the jamovi function to review
    required: true
    autocomplete: functions
usage: /review-function <function_name>
---

# Detailed Jamovi Function Code Review

You are conducting a thorough code review of the jamovi function `$ARGUMENTS`. Focus on code quality, best practices, performance, and maintainability.

## Review Target

Function: **`$ARGUMENTS`**

## Code Review Focus Areas

### 🏗️ **Architecture & Design**

- R6 class structure and inheritance
- Function modularization and separation of concerns
- Data flow and state management
- Error propagation and handling

### 🔧 **Implementation Quality**

- Algorithm efficiency and performance
- Memory usage patterns
- Code readability and maintainability
- Following jamovi and R best practices

### 🛡️ **Robustness & Security**

- Input validation completeness
- Edge case handling
- Error message quality and helpfulness
- Data sanitization

### 📚 **Documentation & UX**

- Code comments and self-documentation
- User interface clarity
- Help text and explanatory content
- Accessibility considerations

### ⚡ **Performance & Scalability**

- Computational complexity
- Memory efficiency
- Large dataset handling
- Optimization opportunities

## Review Response Format

### 🔍 CODE REVIEW: `$ARGUMENTS`

**Overall Quality**: ⭐⭐⭐⭐⭐ (1-5 stars)  

**Maintainability**: HIGH/MEDIUM/LOW  

**Performance**: EXCELLENT/GOOD/NEEDS_WORK  

**User Experience**: EXCELLENT/GOOD/NEEDS_WORK  


#### 🏆 **STRENGTHS**

1. [Specific positive findings with code references]
2. [Well-implemented patterns]
3. [Good practices observed]

#### 🚨 **CRITICAL ISSUES**

1. [Security/reliability concerns with file:line references]
2. [Performance bottlenecks]
3. [Major design flaws]

#### ⚠️ **IMPROVEMENT OPPORTUNITIES**

1. [Code quality improvements with examples]
2. [Refactoring suggestions]
3. [Performance optimizations]

#### 💡 **ENHANCEMENT SUGGESTIONS**

1. [Feature improvements]
2. [User experience enhancements]
3. [Future-proofing recommendations]

#### 🔧 **SPECIFIC RECOMMENDATIONS**

**Architecture:**

```r
# Suggested refactoring
```

**Performance:**

```r
# Optimization examples
```

**Error Handling:**

```r
# Better error handling patterns
```

**User Experience:**

```yaml
# UI improvements
```

#### 📋 **ACTION ITEMS**

**High Priority:**

- [ ] [Specific actionable item]
- [ ] [Another specific item]

**Medium Priority:**

- [ ] [Enhancement opportunity]
- [ ] [Code quality improvement]

**Nice to Have:**

- [ ] [Future enhancement]
- [ ] [Documentation improvement]

#### 📊 **METRICS & ASSESSMENT**

| Aspect | Score | Notes |
|--------|-------|-------|
| Code Quality | X/10 | [Brief note] |
| Error Handling | X/10 | [Brief note] |
| Performance | X/10 | [Brief note] |
| Documentation | X/10 | [Brief note] |
| User Experience | X/10 | [Brief note] |

**Recommendation**: APPROVE / APPROVE_WITH_CHANGES / NEEDS_REWORK

Provide specific, actionable feedback with code examples where helpful.
