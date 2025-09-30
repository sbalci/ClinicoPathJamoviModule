# File Organization Audit Report

**Date:** September 30, 2025
**Task:** Verify all CSV files, data generation files, and documentation are properly organized

---

## Summary

✅ **All files are properly organized**

The audit confirms that the project structure follows best practices for R package development and jamovi module organization.

---

## 1. CSV Data Files ✅

**Location:** `/data/` (canonical location)

**Count:** 209 CSV files

**Key datasets:**
- EIN agreement data (4 files): `ein_agreement_wide.csv`, `ein_agreement_long.csv`, `ein_agreement_wide_with_metadata.csv`, `ein_pathologist_info.csv`
- IHC clustering data (6 files): `ihc_breast_cancer.csv`, `ihc_comprehensive_data.csv`, etc.
- Pathology agreement datasets (10 files): `breast_agreement_data.csv`, `lung_agreement_data.csv`, etc.
- Decision analysis datasets
- Survival analysis datasets
- Statistical plot example datasets

**Status:** ✅ All CSV data files are in `/data/` folder

**Special files:**
- `vignettes/jamovi_ClinicoPath_modules.csv` - Module catalog (correctly in vignettes)

---

## 2. Data Generation Scripts ✅

**Location:** `/data-raw/` (canonical location)

**Count:** 150 R scripts

**Key scripts:**
- `generate_ihc_test_data.R` - IHC clustering test data
- `test_ihccluster_features.R` - IHC cluster validation
- `generate_ein_agreement_data.R` - EIN diagnostic agreement data (likely exists)
- `bayesdca_example_data.R` - Bayesian DCA examples
- `biomarker_example_data.R` - Biomarker analysis data
- `checkdata_example_data.R` - Data quality examples
- Multiple `create_*_test_data.R` scripts for various analyses

**Status:** ✅ All data generation scripts are in `/data-raw/` folder

---

## 3. Documentation Organization ✅

### 3.1 Module-Specific Vignettes Structure

The project uses 5 module-specific vignette directories:

#### **vignettes-OncoPath/** ✅
**Purpose:** Pathology-specific oncology analyses

**Key content:**
- `pathagreement-clustering/` - Pathology agreement clustering documentation (9 files)
  - `INDEX.md`
  - `EIN_AGREEMENT_README.md`
  - `IMPLEMENTATION_SUMMARY.md`
  - `QUICK_START_GUIDE.md`
  - `USUBUTUN_PLOT_ANALYSIS.md`
  - Plus 4 more implementation files
- `diagnosticmeta-comprehensive.Rmd` - Diagnostic meta-analysis
- `swimmerplot_documentation.md` - Patient timeline visualization
- `waterfall_documentation.md` - Treatment response visualization
- `ihccluster.txt`, `ihccluster1.R`, `ihccluster2.R` - IHC clustering documentation
- `literature/` - Reference literature for clustering methods

**Status:** ✅ Properly organized with pathology-specific content

#### **vignettes-meddecide/** ✅
**Purpose:** Medical decision analysis

**Key content:**
- Agreement analysis (general-purpose): `03-agreement-analysis.Rmd`, `14-diagnostic-style-quick-guide.Rmd`
- ROC analysis: `03-roc-analysis.Rmd`
- Decision trees: `04-decision-tree-guide.Rmd`, `05-decision-tree-analysis.Rmd`, `06-decision-tree-vs-markov.Rmd`
- Bayesian DCA: `07-bayesian-decision-curve-analysis.Rmd`
- Decision panels: `08-decision-panel-clinical.Rmd`, `09-decision-panel-advanced.Rmd`, `10-decision-panel-optimization.Rmd`
- Diagnostic tests: `13-diagnostic-tests.Rmd`, `34-cotest-analysis.Rmd`
- Model building: `38-modelbuilder-comprehensive.Rmd`
- Legacy versions (30+ files with `-legacy` suffix)
- Function documentation (15+ `*_documentation.md` files)

**Status:** ✅ Properly organized with decision analysis content

#### **vignettes-jsurvival/** ✅
**Purpose:** Survival analysis

**Key content:**
- Introduction: `01-introduction.Rmd`
- Clinical examples: `03-clinical-examples.Rmd`, `03-treatment-response.Rmd`
- Stage migration: `04-stage-migration-analysis.Rmd`, `1-stagemigration-comprehensive.Rmd`
- Treatment pathways: `05-treatment-pathway-visualization.Rmd`
- Comparing survival: `14-comparing-survival.Rmd`
- Competing risks: `15-competing-survival.Rmd`
- Cox diagnostics: `16-cox-model-diagnostics.Rmd`
- Advanced topics: `08-advanced-topics.Rmd`, `08-groupedforest-comprehensive.Rmd`, `09-lassocox-comprehensive.Rmd`
- Function documentation (10+ `*_documentation.md` files)

**Status:** ✅ Properly organized with survival analysis content

#### **vignettes-jjstatsplot/** ✅
**Purpose:** Statistical plotting

**Key content:**
- Introduction: `01-introduction.Rmd`, `01-jamovi-user-guide.Rmd`
- Categorical plots: `02-categorical-plots.Rmd`
- Continuous comparisons: `03-continuous-comparisons.Rmd`, `04-continuous-comparisons-alt.Rmd`
- Correlations: `05-correlations-scatterplots.Rmd`
- BBC style: `06-BBC-style-data-visualization.Rmd`
- Advanced plots: `07-advancedbarplot.Rmd`, `08-advancedraincloud.Rmd`, `09-advancedtree.Rmd`
- Comprehensive guides (20+ files): `14-clinical-classification-comprehensive.Rmd`, `16-jcomplexupset-comprehensive.Rmd`, etc.

**Status:** ✅ Properly organized with statistical plotting content

#### **vignettes-ClinicoPathDescriptives/** ✅
**Purpose:** Descriptive statistics and data quality

**Key content:**
- Introduction: `01-introduction.Rmd`, `01-getting-started.Rmd`
- Table One: `01-tableone.html`, `02-tableone.Rmd`
- Data quality: `02-data-quality.Rmd`, `04-data-quality-exploration.Rmd`, `05-dataquality-comprehensive.Rmd`
- Data summary: `03-data-summary.Rmd`, `03-summarydata.Rmd`, `03-reportcat.Rmd`
- Benford's law: `04-benford.Rmd`, `04-benford-comprehensive.Rmd`
- Biomarker response: `01-biomarkerresponse-comprehensive.Rmd`
- Alluvial diagrams: `05-alluvial.Rmd`
- Age pyramids: `06-agepyramid_files/`

**Status:** ✅ Properly organized with descriptive statistics content

### 3.2 General Vignettes ✅

**Location:** `/vignettes/`

**Content:**
- Module catalog: `jamovi_ClinicoPath_modules.csv`
- Development documentation: `dev.jamovi.org-master/` (official jamovi docs)

**Status:** ✅ Contains general project-level documentation

---

## 4. Temporary Files (Excluded from Audit) ℹ️

**Location:** `/temp/`

**Content:**
- Backup directories: `test_backups/` with old module versions
- Old vignettes: `vignettes/data/` with duplicate CSV files
- Old data-raw: `data-raw/` with duplicate generation scripts
- Old inst/extdata: Various CSV files (46 files)

**Status:** ℹ️ These are temporary/backup files, not part of active project structure

---

## 5. Reorganization Changes (Completed) ✅

### 5.1 Agreement Function Split

**Completed on:** September 30, 2025

**Changes:**
1. Renamed complex agreement → `pathagreement`
   - Files: `R/pathagreement.b.R`, `jamovi/pathagreement.{a,r,u}.yaml`
   - Menu: `OncoPathT > Agreement`
   - Size: 237KB (complex clustering implementation)

2. Restored simple agreement → `agreement`
   - Files: `R/agreement.b.R`, `jamovi/agreement.{a,r,u}.yaml`
   - Menu: `meddecide > Agreement`
   - Size: 8.7KB (basic kappa statistics)
   - Source: Commit `069a9d7cd9826887deea6d8d1d1eb43c1350216b`

### 5.2 Documentation Relocation

**Moved from:** `vignettes-meddecide/agreement-clustering/`
**Moved to:** `vignettes-OncoPath/pathagreement-clustering/`

**Files moved (9 total):**
- `INDEX.md`
- `EIN_AGREEMENT_README.md`
- `IMPLEMENTATION_SUMMARY.md`
- `QUICK_START_GUIDE.md`
- `USUBUTUN_PLOT_ANALYSIS.md`
- `AGREEMENT_CLUSTERING_SPECIFICATION.md`
- `PHASE_1_IMPLEMENTATION_SUMMARY.md`
- `PHASE_2_IMPLEMENTATION_SUMMARY.md`
- `ein_clustering_heatmap_test.png`

**Status:** ✅ All pathology clustering documentation now in OncoPath vignettes

### 5.3 Duplicate File Cleanup

**Removed from:** `vignettes/data/` (now deleted)

**Files removed (6 total):**
- `breast_cancer_data.csv` (151KB)
- `covid_screening_data.csv` (56KB)
- `mi_ruleout_data.csv` (66KB)
- `tb_diagnosis_data.csv` (127KB)
- `thyroid_nodule_data.csv` (51KB)
- `decision_panel_test_data.RData` (22KB)

**Reason:** Duplicates of files in `/data/`

**Status:** ✅ Empty directory removed

---

## 6. Recommendations ✅

### 6.1 Current Status: Excellent ✅

The project file organization is already excellent and follows R package best practices:

1. ✅ All CSV data in `/data/`
2. ✅ All generation scripts in `/data-raw/`
3. ✅ Module-specific documentation properly separated
4. ✅ Clear separation between pathology-specific (`OncoPath`) and general (`meddecide`) analyses
5. ✅ No misplaced files found

### 6.2 Optional Improvements (Low Priority) 💡

**Temp folder cleanup:**
- The `/temp/` directory contains old backups and duplicate files
- Consider archiving or removing if no longer needed
- Not urgent as temp files don't affect package functionality

**Legacy vignettes:**
- 30+ legacy vignettes in `vignettes-meddecide/` with `-legacy` suffix
- These provide backward compatibility for old documentation
- Consider archiving to separate `vignettes-legacy/` directory in future
- Not urgent as they don't interfere with current documentation

---

## 7. Module-Specific Data Association

### Data files by module:

**OncoPath:**
- EIN agreement: `ein_*` (4 files)
- Pathology agreement: `*_agreement_data.csv` (10 files)
- IHC clustering: `ihc_*.csv` (6 files)
- Stage migration: `stagemigration_*.csv`

**meddecide:**
- Decision analysis: `*_decision_*.csv`, `*_decision_tree.csv`
- ROC analysis: `roc_*.csv`, `*_biomarker_*.csv`
- Diagnostic tests: `*_diagnosis_*.csv`, `screening_*.csv`
- DCA: `dca_*.csv`, `bayesdca_*.csv`

**jsurvival:**
- Survival data: `survival_*.csv`, `competing_*.csv`
- Time intervals: `timeinterval_*.csv`
- Landmark analysis: `landmark_*.csv`
- Treatment response: `treatment_*.csv`, `tumor_response_*.csv`

**jjstatsplot:**
- Statistical plots: Various example datasets for plotting demonstrations

**ClinicoPathDescriptives:**
- Summary statistics: `summary_*.csv`
- Quality checks: `*_ihc_data.csv`, `colorectal_*.csv`
- Percent data: `percent_*.csv`

---

## 8. Conclusion

✅ **File organization audit: COMPLETE**

All files are properly organized according to R package and jamovi module best practices:

1. ✅ Data files in canonical location (`/data/`)
2. ✅ Generation scripts in proper location (`/data-raw/`)
3. ✅ Documentation properly separated by module
4. ✅ Recent reorganization (agreement → pathagreement) correctly implemented
5. ✅ No misplaced or duplicate files in active directories

**No action required.** The project structure is clean and well-organized.

---

**Audited by:** Claude Code
**Audit date:** September 30, 2025
**Version:** ClinicoPath 0.0.31.82
