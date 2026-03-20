# stagemigration Improvement Plan

## Function Scale
- 392 private methods, ~200 options, 180 outputs, 28,878 lines .b.R
- 15+ analysis modules from basic contingency to SHAP explainability

---

## ALL AUDIT RESULTS (4 agents complete)

### .a.yaml Audit
- **59 dead options** (29.5% of all options never read in .b.R)
  - 12 are name mismatches with .b.R counterparts
  - 47 are truly unused
- **21 missing options** (read in .b.R but not defined in .a.yaml)
  - 14 are name mismatches, 7 need new entries
- **4 naming convention collisions:**
  - A: `forest*` (.a.yaml) vs `rf*` (.b.R) — Random Forest
  - B: `survivalTime`/`event` vs `timeVar`/`eventVar` — Win Ratio/Frailty/Clinical Utility
  - C: `survivalTime`/`event`/`oldStage` vs `outcome`/`outcomeEvent`/`explanatory` — Interval/Informative Censoring, Concordance
  - D: `winRatio*` vs `wr*` — Win Ratio

### .r.yaml Audit
- **2 unpopulated tables**: `forestSurvivalPredictions`, `forestStagingComparison`
- **41 tables missing `visible:` rules** across 6 feature groups (interval censoring, informative censoring, concordance probability, win ratio, frailty, clinical utility) — show empty tables to users
- **7 option-result name collisions** (same name used for both Bool option and Table output): `stratifiedAnalysis`, `forestStagingComparison`, `cureModelComparison`, `clinicalUtilityComparison`, `clinicalUtilityNNT`, `clinicalUtilityTimeVarying`, `clinicalUtilityBootstrap`
- **41 invalid `clearWith` references** using nonexistent option names (`vars`, `explanatory`, `outcome`, `timeVar`, `eventVar`)
- **41 `description:` schema violations** — not valid on r.yaml result items
- **1 format typo**: `zto.pvalue` should be `zto,pvalue` (willRogersEnhancedAnalysis table)

### Plot Quality Audit
- **4 of 10 plots render BLANK**: `return(p)` instead of `print(p); return(TRUE)`
  - `.plotSankeyDiagram`, `.plotWillRogersEffect`, `.plotMigrationSurvivalComparison`, `.plotCrossValidation`
- **2 HIGH protobuf risks**: raw timeROC/dca objects in setState (lines 4227, 4383)
- **2 color-blind-hostile palettes**: red/green in Sankey + CrossValidation
- **All 10 plots** ignore `ggtheme` parameter
- **`library()` calls** inside render functions (should use requireNamespace)
- **Debug `message()` calls** left in `.plotWillRogersEffect`
- **24 guard clauses** return `return()` instead of `return(FALSE)`

### Utility Extraction Audit
- `.escapeVar` duplicated across **51 files** — extract to `R/utils-common.R`
- `.safeExecute` in 4 files with inconsistent signatures
- 24 bootstrap CI patterns + 4 pseudo-R2 methods consolidatable

---

## PRIORITY FIX ORDER

### Phase 1: CRITICAL (Prevents crashes + blank output)

| # | Issue | Count | Fix |
|---|---|---|---|
| 1 | Blank plots (return(p) not print+TRUE) | 4 plots | Change return pattern in 4 render functions |
| 2 | Option name mismatches in .b.R | 14 pairs | Rename .b.R refs to match .a.yaml |
| 3 | Protobuf serialization risks | 2 setState calls | Wrap timeROC/dca in plain lists |
| 4 | Option-result name collisions | 7 items | Rename either the option or the result |
| 5 | r.yaml `description:` schema violations | 41 tables | Remove `description:` field from all |
| 6 | Format typo `zto.pvalue` | 1 column | Change to `zto,pvalue` |

### Phase 2: HIGH (Fixes misleading UI)

| # | Issue | Count | Fix |
|---|---|---|---|
| 7 | Missing `visible:` rules | 41 tables | Add visibility rules per feature toggle |
| 8 | Invalid `clearWith` references | 41 tables | Replace with valid option names |
| 9 | Add 7 genuinely missing .a.yaml options | 7 options | Add rfMtryAuto, rfBootstrapType, etc. |
| 10 | Guard clause `return()` → `return(FALSE)` | 24 lines | Fix in all render functions |
| 11 | Remove `library()` from renders | ~10 calls | Use requireNamespace or rely on NAMESPACE |
| 12 | Remove debug `message()` calls | ~6 calls | Delete from .plotWillRogersEffect |

### Phase 3: MEDIUM (Quality improvements)

| # | Issue | Count | Fix |
|---|---|---|---|
| 13 | Color-blind-hostile palettes | 2 plots | Replace red/green with Okabe-Ito |
| 14 | Add `ggtheme` support | 10 plots | Accept ggtheme parameter |
| 15 | Populate 2 empty tables | 2 tables | Wire or remove |
| 16 | Wire `confidenceLevel` option | 1 option | Replace hardcoded 0.95 |
| 17 | Remove/wire 47 truly dead options | 47 options | Decide per option |

### Phase 4: REFACTORING (Future)

| # | Issue | Count | Fix |
|---|---|---|---|
| 18 | Extract `.escapeVar` to shared utils | 51 files | Create R/utils-common.R |
| 19 | Extract bootstrap utilities | ~24 patterns | Create R/utils-bootstrap.R |
| 20 | Extract pseudo-R2 methods | 4 methods | Create R/utils-pseudor2.R |
| 21 | Consider splitting .b.R | 28,878 lines | Module-specific helper files |

---

## MODULE STATUS DESIGNATION

### PRODUCTION-READY (Tier 1-2, no changes needed)
Basic Migration, Trend Tests, Pseudo-R2, NRI/IDI, LRT,
Calibration (Binned), DCA, Time-ROC, RMST, Stage Migration Effect

### NEEDS PLOT FIX (renders blank)
Sankey Diagram, Will Rogers Effect, Migration Survival Comparison, Cross-Validation

### NEEDS OPTION FIX (template naming mismatch)
Random Forest (6 mismatches + 5 missing options),
Win Ratio (11 dead + 3 mismatches),
Frailty (6 dead + timeVar/eventVar),
Cure Fraction (5 dead),
Clinical Utility (2 dead + timeVar/eventVar)

### NEEDS r.yaml FIX (missing visibility, invalid clearWith, description violations)
Interval Censoring (6 tables),
Informative Censoring (7 tables),
Concordance Probability (7 tables),
Win Ratio (7 tables),
Frailty Models (7 tables),
Clinical Utility (7 tables)

### HARDCODED VALUES
`confidenceLevel` option exists but code hardcodes 0.95

---

## AGENT TEAM FOR IMPLEMENTATION

### Agent A: .r.yaml Schema Fix
- Remove 41 `description:` fields (schema violation)
- Fix `zto.pvalue` → `zto,pvalue` format typo
- Add `visible:` rules to 41 tables
- Fix 41 invalid `clearWith` references
- Resolve 7 option-result name collisions (rename result items)

### Agent B: .b.R Plot Fix
- Fix 4 blank plots: `return(p)` → `print(p); return(TRUE)`
- Fix 24 guard clauses: `return()` → `return(FALSE)`
- Remove `library()` calls from render functions
- Remove debug `message()` calls
- Wrap 2 setState calls in plain lists (protobuf)

### Agent C: .b.R Option Name Harmonization
- Fix 14 name mismatches across 4 patterns (rf→forest, timeVar→survivalTime, outcome→survivalTime, wr→winRatio)
- Add 7 missing options to .a.yaml if needed

### Agent D: .a.yaml + .u.yaml Cleanup
- Remove 47 truly dead options from .a.yaml
- Remove corresponding .u.yaml controls
- Wire `confidenceLevel` or document as intentionally hardcoded

### Agent E: Color-Blind + Theme
- Replace red/green palettes in 2 plots
- Add `ggtheme` parameter support to all 10 plots

### Agent F: Utility Extraction (future)
- Extract `.escapeVar` to shared R/utils-common.R
- Extract bootstrap/R2 utilities
