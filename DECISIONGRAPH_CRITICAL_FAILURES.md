# CRITICAL FAILURES IN `decisiongraph` - RETRACTION OF ASSESSMENT

## Executive Summary

**RETRACTION:** My previous assessment calling `decisiongraph` "production-ready" was **completely wrong**.

**Reality:** ✅ The user is correct - ALL core calculations are placeholders generating **mathematically meaningless outputs**.

**Status:** ❌ **UNSAFE FOR RELEASE** - Would mislead clinicians with fabricated results

---

## Critical Failures Identified

### 1. ❌ **DECISION TREE: No Tree Traversal** (R/decisiongraph.b.R:1093-1129)

**Problem:** `.traverseDecisionPath()` doesn't traverse the tree structure at all.

**Current implementation:**
```r
# Lines 1106, 1113, 1120: Just takes column means
costs <- mean(mydata[[self$options$costs[1]]], na.rm = TRUE)
utilities <- mean(mydata[[self$options$utilities[1]]], na.rm = TRUE)
probabilities <- mean(mydata[[self$options$probabilities[1]]], na.rm = TRUE)

# Lines 1108, 1115: Falls back to RANDOM NUMBERS
costs <- 1500 + runif(1, 0, 500)  // ❌ RANDOM!
utilities <- 0.75 + runif(1, -0.1, 0.1)  // ❌ RANDOM!

# Lines 1126-1129: Hard-coded weighted average with magic numbers
expectedCost <- costs * probabilities + (costs * 0.5) * (1 - probabilities)
expectedUtility <- utilities * probabilities + (utilities * 0.8) * (1 - probabilities)
```

**Why invalid:**
- No recursive tree traversal
- No branch-specific probabilities
- Magic numbers (0.5, 0.8) have no relationship to tree structure
- All strategies share same aggregate inputs
- ICERs are meaningless

**Correct implementation should:**
1. Start at root node
2. For decision nodes: evaluate each branch separately
3. For chance nodes: multiply by branch probabilities
4. For terminal nodes: accumulate costs/utilities
5. Use backward induction to calculate expected values

---

### 2. ❌ **MARKOV: Hard-Coded Transitions** (R/decisiongraph_utils.R:65-106)

**Problem:** `calculateMarkovTransitionMatrix()` ignores user data, uses hard-coded probabilities.

**Current implementation:**
```r
# Lines 80-86: Hard-coded transition probabilities
if (i == j) {
    transitionMatrix[i, j] <- 0.7  // ❌ HARD-CODED: Stay in state
} else if (j == i + 1) {
    transitionMatrix[i, j] <- 0.2  // ❌ HARD-CODED: Progress
} else if (j == numStates) {
    transitionMatrix[i, j] <- 0.1  // ❌ HARD-CODED: Death
}
```

**Why invalid:**
- Ignores `transitionData` parameter completely
- 0.7/0.2/0.1 transitions applied to ALL models
- Users see fabricated Markov outputs

**Additionally:** `private$.markovData` never populated (R/decisiongraph.b.R:2524-2633)

---

### 3. ❌ **PSA: Random Sampling, Not Model-Based** (R/decisiongraph_utils.R:119-181)

**Problem:** `performMonteCarloSimulation()` samples from arbitrary distributions, not actual model parameters.

**Current implementation:**
```r
# Lines 137-148: Samples from column means, not model parameters
sampledCost <- rnorm(1, mean = mean(baseResults$expectedCost), sd = sd(...))
sampledUtility <- rnorm(1, mean = mean(baseResults$expectedUtility), sd = sd(...))

# Line 153: Hard-coded WTP, ignores user threshold
simResults$nmb[sim] <- simResults$utility[sim] * 50000 - simResults$cost[sim]

# Line 154: RANDOMLY assigns strategy label
simResults$strategy[sim] <- sample(baseResults$strategy, 1)  // ❌ RANDOM!
```

**Why invalid:**
- Doesn't resample actual model parameters (transition probabilities, branch probabilities, etc.)
- Hard-coded WTP = $50,000
- Random strategy assignment means PSA outputs are meaningless

---

### 4. ❌ **CEAC: Incorrect Formula** (R/decisiongraph_utils.R:200-233)

**Problem:** CEAC calculation uses wrong mathematical formula.

**Current implementation:**
```r
# Lines 213-217: INCORRECT CEAC formula
nmb <- strategyData$utility * threshold - strategyData$cost
allNMB <- psaResults$utility * threshold - psaResults$cost
probability <- mean(nmb >= allNMB, na.rm = TRUE)  // ❌ WRONG FORMULA
```

**Why invalid:**
- Compares one strategy's NMB vector against ALL strategies' combined NMB
- This is NOT what CEAC measures

**Correct CEAC:**
For each simulation iteration:
1. Calculate NMB for ALL strategies at given threshold
2. Identify which strategy has HIGHEST NMB
3. CEAC = proportion of iterations where THIS strategy is optimal

**Correct formula:**
```r
# For each simulation, find which strategy has max NMB
optimal_strategy <- apply(nmb_matrix, 1, which.max)
# CEAC = proportion where this strategy is optimal
probability <- mean(optimal_strategy == this_strategy_index)
```

---

### 5. ❌ **VISUALIZATION: Hard-Coded Placeholder** (R/decisiongraph_utils.R:32-47)

**Problem:** `createDecisionTreePlot()` doesn't render actual tree.

**Current implementation:**
```r
# Lines 36-37: Just plots 3 hard-coded points
plot <- ggplot2::ggplot() +
  ggplot2::geom_point(data = data.frame(x = 1:3, y = 1:3),
                     ggplot2::aes(x = x, y = y), size = 5)
```

**Why invalid:**
- Users see three dots, not their decision structure
- Ignores all tree data
- Comment at line 34 admits: "This is a placeholder implementation"

---

### 6. ❌ **TESTS: No Numerical Validation** (tests/testthat/test-decisiongraph.R:1-220)

**Problem:** Tests only check that code runs, never validate outputs.

**Current tests:**
```r
# Only smoke tests - no numerical assertions
expect_true(!is.null(result))
expect_true(nrow(result) > 0)
# NO expectations on actual values!
```

**Missing:**
- ❌ Expected value calculations
- ❌ ICER values
- ❌ Markov cohort traces
- ❌ CEAC probabilities
- ❌ Regression tests against known examples

**Result:** All placeholder implementations pass CI unchecked.

---

## Impact Assessment

### ❌ **Clinical Safety**

**What users would see:**
1. **Meaningless decision trees:** Expected values based on random numbers
2. **Fabricated Markov models:** 0.7/0.2/0.1 transitions for all diseases
3. **Invalid PSA:** Random cost/utility samples, not actual parameter uncertainty
4. **Wrong CEAC:** Incorrect probability calculations
5. **Placeholder plots:** Three dots instead of decision structure

**Clinical consequence:** Clinicians would make treatment decisions based on **fabricated data**, potentially harming patients.

---

## Root Cause Analysis

### Why I Missed This

1. ❌ **Superficial grep checks:** Looked for function NAMES, not implementations
2. ❌ **Trusted sophistication:** Complex code structure fooled me into thinking it was complete
3. ❌ **Didn't trace logic:** Never verified calculations use user data
4. ❌ **Assumed tests validated:** Didn't check test assertions
5. ❌ **Pattern matching:** After finding real issues in other modules, I should have been MORE suspicious, not less

---

## Comparison to Other Modules

| Module | Issue Type | My Initial Assessment | Reality |
|--------|-----------|----------------------|---------|
| `decisioncompare` | Wrong statistics (McNemar) | ❌ MISSED, then ✅ FIXED | ✅ NOW CORRECT |
| `decisioncurve` | Auto-scaling probabilities | ❌ MISSED, then ✅ FIXED | ✅ NOW CORRECT |
| `decisiongraph` | **ALL** calculations placeholders | ❌❌❌ **CALLED "PRODUCTION-READY"** | ❌ **COMPLETELY WRONG** |

**Verdict:** `decisiongraph` has the MOST severe issues of all three modules - not just bugs, but **complete absence of real implementations**.

---

## Required Fixes

### Priority 1: Core Calculations (CRITICAL)

1. **Decision Tree Traversal:**
   - Implement recursive backward induction
   - Use branch-specific probabilities
   - Accumulate costs/utilities correctly
   - Test against textbook examples (e.g., Hunink et al., 2014)

2. **Markov Model:**
   - Parse user transition matrices from data
   - Calculate state costs/utilities from user inputs
   - Store results in `private$.markovData`
   - Implement discounting and half-cycle correction properly
   - Test against published Markov examples

3. **PSA Implementation:**
   - Resample actual model parameters (not column means)
   - Use user-specified parameter distributions
   - Respect user willingness-to-pay threshold
   - Calculate NMB correctly for each iteration

4. **CEAC Calculation:**
   - Fix formula: For each iteration, find strategy with MAX NMB
   - CEAC = proportion where each strategy is optimal
   - Test against known CEAC curves

5. **Visualization:**
   - Render actual tree nodes and branches
   - Display user's decision structure
   - Show probabilities, costs, utilities on branches

### Priority 2: Testing (CRITICAL)

6. **Numerical Regression Tests:**
   - Test decision tree against Hunink textbook example
   - Test Markov model against Sonnenberg & Beck (1993) example
   - Test PSA against known distributions
   - Test CEAC against published curves
   - Assert exact numerical outputs

---

## Recommended Actions

### Immediate (Block Release)

1. ❌ **DO NOT RELEASE** `decisiongraph` in current state
2. ✅ **Retract** "production-ready" assessment
3. ✅ **Document** all critical failures
4. ✅ **Prioritize** fixing core calculations before any new features

### Short-term (Fix Core Logic)

1. Implement proper decision tree traversal
2. Implement real Markov model handling
3. Fix PSA to resample actual parameters
4. Fix CEAC formula
5. Implement real tree visualization
6. Add comprehensive numerical tests

### Long-term (Quality Assurance)

1. Require numerical regression tests for all health economics functions
2. Peer review by health economist before release
3. Validate against published examples
4. Clinical pilot testing with real users

---

## Lessons Learned

1. ✅ **NEVER trust function names** - Always trace logic
2. ✅ **NEVER assume sophistication = correctness** - Complex code can hide placeholders
3. ✅ **ALWAYS verify tests assert outputs** - Smoke tests are insufficient
4. ✅ **ALWAYS check user data flows through** - Follow the data path
5. ✅ **BE MORE SUSPICIOUS after finding issues** - Pattern of problems indicates deeper issues

---

## Apology & Commitment

I sincerely apologize for:
- Calling placeholder code "production-ready"
- Missing obvious placeholders (hard-coded 0.7/0.2/0.1, random numbers)
- Not tracing data flow through calculations
- Providing false assurance that could have led to clinical harm

**Commitment:** I will be far more rigorous in future assessments, always tracing logic and validating that user data flows through to outputs.

---

## References

- Hunink MGM et al (2014). Decision Making in Health and Medicine: Integrating Evidence and Values. 2nd ed. Cambridge University Press.
- Sonnenberg FA, Beck JR (1993). "Markov models in medical decision making: a practical guide." Medical Decision Making 13(4):322-338.
- Briggs A, Claxton K, Sculpher M (2006). Decision Modelling for Health Economic Evaluation. Oxford University Press.

---

**Document Version:** 1.0
**Date:** 2025-01-14
**Author:** Claude (Anthropic) - Critical Failure Assessment
**Status:** ❌ **RETRACTION OF PREVIOUS "PRODUCTION-READY" CLAIM**
