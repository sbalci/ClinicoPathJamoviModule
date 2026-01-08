# ClinicoPathJamoviModule Vignettes

This directory contains comprehensive guides and catalogs for the ClinicoPathJamoviModule package.

## Available Vignettes

### 📊 Test Data Catalogs

#### 1. [Test Data Catalog](test-data-catalog.Rmd) - Overview with Key Functions
**File:** `test-data-catalog.Rmd`
**Purpose:** Curated overview of test data files organized by menu group
**Content:**
- Usage guide and download instructions
- Key functions from each menu group with descriptions
- Featured: NEW kappa sample size functions (Jan 2025)
- Download links for selected .omv files
- Quick reference tables

**Access in R:**
```r
vignette("test-data-catalog", package = "ClinicoPath")
```

#### 2. [Complete Test Data Catalog](test-data-complete-catalog.Rmd) - All 945 Files
**File:** `test-data-complete-catalog.Rmd`
**Purpose:** Comprehensive listing of ALL 945 .omv test data files
**Content:**
- Complete alphabetical listing by function
- Direct download links for every .omv file
- File counts per function
- Summary statistics

**Access in R:**
```r
vignette("test-data-complete-catalog", package = "ClinicoPath")
```

**Or browse online:**
- [GitHub Data Directory](https://github.com/sbalci/ClinicoPathJamoviModule/tree/master/data)
- [CSV Index Download](https://raw.githubusercontent.com/sbalci/ClinicoPathJamoviModule/master/data/test_data_index.csv)

### 📖 Function Reference

#### 3. [Function Reference Guide](function-reference.Rmd)
**File:** `function-reference.Rmd`
**Purpose:** Comprehensive guide to all 420+ analysis functions
**Content:**
- Functions organized by menu group (ClinicoPathDescriptives, JJStatsPlot, OncoPath, jsurvival, meddecide)
- Purpose and "when to use" guidance for each function
- Test data availability
- Featured: NEW kappa sample size functions (kappasizeci, kappasizefixedn, kappasizepower)
- Quick reference tables by clinical domain

**Access in R:**
```r
vignette("function-reference", package = "ClinicoPath")
```

---

## Quick Start

### Downloading Test Data

All .omv files can be downloaded directly from GitHub. Two methods:

**Method 1: Direct Download (Individual Files)**
```r
# Example: Download a specific test file
download.file(
  "https://raw.githubusercontent.com/sbalci/ClinicoPathJamoviModule/master/data/kappasizefixedn_scenarios_comprehensive.omv",
  destfile = "kappasizefixedn_test.omv"
)
```

**Method 2: Clone Repository (All Files)**
```bash
git clone https://github.com/sbalci/ClinicoPathJamoviModule.git
cd ClinicoPathJamoviModule/data
# All 945 .omv files are now in the data/ directory
```

### Using Test Data Files

1. **Download** the .omv file using links in vignettes
2. **Open jamovi** desktop application
3. **Load file:** File → Open → Select downloaded .omv
4. **Explore:** Data and any saved analyses will load automatically

---

## Test Data Statistics

### Total Coverage
- **Total .omv Files:** 945
- **Functions with Test Data:** 60+
- **Menu Groups:** 5 (ClinicoPathDescriptives, JJStatsPlot, OncoPath, jsurvival, meddecide)

### Most Comprehensive Functions (15+ files)

| Rank | Function | Files | Category |
|------|----------|-------|----------|
| 1 | timeinterval | 24 | jsurvival |
| 2 | ihcheterogeneity | 21 | OncoPath |
| 3 | outcomeorganizer | 20 | jsurvival |
| 4 | singlearm | 18 | jsurvival |
| 4 | nogoldstandard | 18 | meddecide |

### Recent Additions (January 2025) ✨

**meddecide Menu Group:**
- `kappasizeci` (3 files, 86 scenarios) - Precision-based sample size
- `kappasizefixedn` (3 files, 86 scenarios) - Reverse calculation for fixed n
- `kappasizepower` (3 files, 86 scenarios) - Power-based sample size
- **Total:** 9 files, 258 comprehensive scenarios

---

## File Organization

```
vignettes/
├── README.md                           # This file
├── test-data-catalog.Rmd               # Overview with key functions
├── test-data-complete-catalog.Rmd      # Complete listing (945 files)
├── function-reference.Rmd              # Function guide (420+ functions)
└── [other vignettes...]
```

```
data/
├── test_data_index.csv                 # CSV index of all .omv files
├── *.omv                               # 945 test data files
└── *.rda                               # R data format versions
```

---

## Generating Updated Catalogs

The complete test data catalog is auto-generated from the data directory.

**To regenerate:**
```r
source("data-raw/generate_test_data_vignette.R")
```

This will:
1. Scan the `data/` directory for all .omv files
2. Generate `vignettes/test-data-complete-catalog.Rmd` with download links
3. Create `data/test_data_index.csv` for reference

---

## Additional Resources

### Online Documentation
- **Main Website:** [www.serdarbalci.com/ClinicoPathJamoviModule](https://www.serdarbalci.com/ClinicoPathJamoviModule/)
- **GitHub:** [github.com/sbalci/ClinicoPathJamoviModule](https://github.com/sbalci/ClinicoPathJamoviModule)
- **Browse Data:** [GitHub Data Directory](https://github.com/sbalci/ClinicoPathJamoviModule/tree/master/data)

### Submodule Documentation
- **ClinicoPathDescriptives:** [www.serdarbalci.com/ClinicoPathDescriptives](https://www.serdarbalci.com/ClinicoPathDescriptives/)
- **jjstatsplot:** [www.serdarbalci.com/jjstatsplot](https://www.serdarbalci.com/jjstatsplot/)
- **jsurvival:** [www.serdarbalci.com/jsurvival](https://www.serdarbalci.com/jsurvival/)
- **meddecide:** [www.serdarbalci.com/meddecide](https://www.serdarbalci.com/meddecide/)

### Getting Help
- **Issues:** [Report bugs or request features](https://github.com/sbalci/ClinicoPathJamoviModule/issues)
- **Discussions:** [GitHub Discussions](https://github.com/sbalci/ClinicoPathJamoviModule/discussions)

---

## Citation

```
Balci S (2025). ClinicoPathJamoviModule: Clinicopathological Data Analysis.
R package version 0.0.31. https://github.com/sbalci/ClinicoPathJamoviModule
```

---

**Last Updated:** 2025-01-07
**Vignette Count:** 3+ catalogs and reference guides
**Test Data Files:** 945 .omv files with direct download links
