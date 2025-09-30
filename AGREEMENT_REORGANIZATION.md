# Agreement Function Reorganization

## Changes Made

### 1. Renamed Complex Agreement → Pathology Agreement

The complex agreement function with diagnostic style clustering and pathology-specific features has been renamed:

**Old name:** `agreement`
**New name:** `pathagreement`
**Menu location:** OncoPathT > Agreement

**Files:**
- `jamovi/pathagreement.a.yaml` - Options definition
- `jamovi/pathagreement.r.yaml` - Results definition
- `jamovi/pathagreement.u.yaml` - UI definition
- `R/pathagreement.b.R` - Backend implementation
- `R/pathagreement.h.R` - Auto-generated header

**Features:**
- Cohen's kappa, Fleiss' kappa, Krippendorff's alpha
- Hierarchical clustering for diagnostic styles (Usubutun method)
- Style group analysis (Conservative, Balanced, Sensitive)
- Discordant case identification
- Rater characteristic associations
- Metadata rows support for rater characteristics
- Advanced visualizations (heatmaps, dendrograms, silhouette plots)
- Clinical interpretation guides

### 2. Restored Original Simple Agreement

The original simple agreement function from commit `069a9d7` has been restored:

**Name:** `agreement`
**Menu location:** meddecide > Agreement  
**Source:** https://github.com/sbalci/meddecide/tree/069a9d7cd9826887deea6d8d1d1eb43c1350216b

**Files:**
- `jamovi/agreement.a.yaml`
- `jamovi/agreement.r.yaml`
- `jamovi/agreement.u.yaml`
- `R/agreement.b.R`
- `R/agreement.h.R`

**Features:**
- Cohen's kappa (2 raters)
- Fleiss' kappa (3+ raters)
- Weighted kappa options
- Basic agreement visualization
- Simple, focused functionality

## Why This Change?

The original agreement function became too specialized for pathology use cases with:
- Diagnostic style clustering
- Pathology-specific metadata (experience, specialty, institution)
- Complex clustering visualizations
- EIN (endometrial intraepithelial neoplasia) datasets

By splitting into two functions:
- **`agreement`**: General-purpose interrater reliability for any field
- **`pathagreement`**: Specialized pathology agreement with diagnostic style analysis

## Data Files

Pathology-specific datasets remain unchanged:
- `data/ein_agreement_*.csv` - EIN diagnostic agreement data
- `data/ein_agreement_wide_with_metadata.csv` - Dataset with metadata rows
- `data/*_agreement_data.csv` - Various pathology agreement datasets (breast, lung, thyroid, etc.)

These are associated with `pathagreement`, not the general `agreement` function.

## Documentation Files

Pathology agreement documentation has been moved:
- **Old location:** `vignettes-meddecide/agreement-clustering/`
- **New location:** `vignettes-OncoPath/pathagreement-clustering/`

Documentation files:
- `INDEX.md` - Navigation guide
- `EIN_AGREEMENT_README.md` - EIN dataset documentation
- `QUICK_START_GUIDE.md` - Quick start guide for jamovi
- `USUBUTUN_PLOT_ANALYSIS.md` - Detailed plot analysis
- `IMPLEMENTATION_SUMMARY.md` - Technical implementation details
- `AGREEMENT_CLUSTERING_SPECIFICATION.md` - Algorithm specifications
- `PHASE_1_IMPLEMENTATION_SUMMARY.md` - Phase 1 features
- `PHASE_2_IMPLEMENTATION_SUMMARY.md` - Phase 2 features
- `ein_clustering_heatmap_test.png` - Example visualization

## Module Compilation

Both functions compile successfully:
```
wrote: agreement.h.R
wrote: agreement.src.js
wrote: pathagreement.h.R
wrote: pathagreement.src.js
```

## Menu Organization

**meddecide menu:**
- Agreement (simple, general-purpose)

**OncoPathT menu:**
- Pathology Interrater Reliability (complex, pathology-specific)

## File Organization Audit

A comprehensive audit of all project files was conducted following this reorganization to ensure:
- All CSV files are in `/data/` folder
- All data generation scripts are in `/data-raw/` folder
- All documentation files are in appropriate module-specific vignettes folders

**Audit Result:** ✅ All files properly organized

See `FILE_ORGANIZATION_AUDIT.md` for complete details.

---

**Date:** September 30, 2025
**Version:** ClinicoPath 0.0.31.82
