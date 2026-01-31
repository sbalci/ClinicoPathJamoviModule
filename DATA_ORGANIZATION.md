# Data Files Organization

**Updated**: 2026-01-31

## Directory Structure

```
data/
├── *.rda                    # R data files (872 files)
│   └── All .rda datasets stored in root data/ folder
│
└── nonrda/                  # Non-RDA formats (21 files)
    ├── *.csv                # CSV files (7 files)
    ├── *.omv                # Jamovi files (6 files)
    └── *.xlsx               # Excel files (8 files)
```

---

## File Types by Location

### RDA Files (data/)

**Location**: `data/*.rda`
**Count**: 872 files
**Purpose**: Native R data format, loaded with `data(dataset_name)`

**Characteristics**:
- Fastest loading in R
- Preserves R object attributes (factors, levels, classes)
- Used by package documentation
- Required for `data()` command

**Example**:
```r
data(rpasurvival_test)
data(groomecompare_test)
```

---

### Non-RDA Files (data/nonrda/)

**Location**: `data/nonrda/`
**Count**: 21 files (7 CSV + 6 OMV + 8 XLSX)
**Purpose**: Alternative formats for cross-platform use and jamovi UI testing

#### CSV Files (7 files)

**Format**: Comma-separated values
**Use cases**:
- Cross-platform compatibility
- Import into other statistical software
- Human-readable/editable
- Version control friendly

**Files**:
```
data/nonrda/
├── groomecompare_test.csv
├── groomecompare_small.csv
├── groomecompare_large.csv
├── rpasurvival_test.csv
├── rpasurvival_small.csv
├── rpasurvival_large.csv
└── test_data_index.csv
```

**Loading**:
```r
df <- read.csv("data/nonrda/groomecompare_test.csv")
```

---

#### OMV Files (6 files)

**Format**: Jamovi native format
**Use cases**:
- Testing functions in jamovi UI
- Sharing data with jamovi users
- Preserves jamovi metadata

**Files**:
```
data/nonrda/
├── groomecompare_test.omv
├── groomecompare_small.omv
├── groomecompare_large.omv
├── rpasurvival_test.omv
├── rpasurvival_small.omv
└── rpasurvival_large.omv
```

**Loading**:
- In jamovi: File → Open → Select .omv file
- In R: `jmvReadWrite::read_omv("data/nonrda/groomecompare_test.omv")`

---

#### XLSX Files (8 files)

**Format**: Microsoft Excel
**Use cases**:
- Sharing with clinical collaborators
- Viewing/editing in Excel
- Multi-sheet workbooks

**Files**:
```
data/nonrda/
├── groomecompare_test.xlsx
├── groomecompare_small.xlsx
├── groomecompare_large.xlsx
├── groomecompare_all_scenarios.xlsx    # Multi-sheet
├── rpasurvival_test.xlsx
├── rpasurvival_small.xlsx
├── rpasurvival_large.xlsx
└── rpasurvival_all_formats.xlsx        # Multi-sheet
```

**Loading**:
```r
# Single sheet
df <- readxl::read_excel("data/nonrda/groomecompare_test.xlsx")

# Multi-sheet
sheets <- readxl::excel_sheets("data/nonrda/groomecompare_all_scenarios.xlsx")
df_list <- lapply(sheets, function(s) {
  readxl::read_excel("data/nonrda/groomecompare_all_scenarios.xlsx", sheet = s)
})
```

---

## Data Generation Scripts

**Location**: `data-raw/`

All data generation scripts have been updated to save files in the correct locations:

### rpasurvival_test_data.R
```r
# RDA files → data/
save(rpasurvival_test, file = here::here("data", "rpasurvival_test.rda"))

# CSV files → data/nonrda/
write.csv(rpasurvival_test,
          file = here::here("data", "nonrda", "rpasurvival_test.csv"))

# XLSX files → data/nonrda/
writexl::write_xlsx(rpasurvival_test,
                    path = here::here("data", "nonrda", "rpasurvival_test.xlsx"))

# OMV files → data/nonrda/
jmvReadWrite::write_omv(rpasurvival_test,
                        here::here("data", "nonrda", "rpasurvival_test.omv"))
```

### groomecompare_test_data.R
```r
# Same structure as above
# RDA → data/
# CSV, XLSX, OMV → data/nonrda/
```

---

## Why This Structure?

### Benefits

1. **R Package Standards**
   - RDA files in `data/` is R package convention
   - Allows `data()` command to work properly
   - Package documentation can reference datasets

2. **Clean Organization**
   - Separates native R format from export formats
   - Easier to manage and find files
   - Clear distinction between source and derived formats

3. **Version Control**
   - RDA files are binary (harder to diff)
   - Non-RDA files can be excluded from git if needed
   - Smaller repository size if excluding nonrda/

4. **Cross-Platform Compatibility**
   - Non-R users can access CSV/XLSX
   - Jamovi users can access OMV
   - All users can access appropriate format

---

## Git Considerations

### .gitignore Recommendations

You may want to add to `.gitignore`:

```gitignore
# Optionally exclude non-RDA files (they can be regenerated)
data/nonrda/*.csv
data/nonrda/*.xlsx
# data/nonrda/*.omv  # Keep OMV for jamovi UI testing
```

**Rationale**:
- RDA files are source of truth
- CSV/XLSX can be regenerated from RDA
- OMV files useful for testing in jamovi UI

---

## File Sizes

| Format | Total Size | Avg per file |
|--------|------------|--------------|
| RDA    | ~50 KB     | 1-20 KB      |
| CSV    | ~120 KB    | 2-46 KB      |
| OMV    | ~30 KB     | 2-21 KB      |
| XLSX   | ~160 KB    | 6-46 KB      |

**Total**: ~360 KB (all test data files)

---

## Usage Examples

### In R Package Development

```r
# Load RDA (standard)
data(rpasurvival_test)

# Load CSV (manual)
rpa_csv <- read.csv("data/nonrda/rpasurvival_test.csv")

# Load Excel
rpa_xlsx <- readxl::read_excel("data/nonrda/rpasurvival_test.xlsx")

# Load OMV
rpa_omv <- jmvReadWrite::read_omv("data/nonrda/rpasurvival_test.omv")
```

### In jamovi UI

1. Launch jamovi
2. File → Open
3. Navigate to: `ClinicoPathJamoviModule/data/nonrda/`
4. Select: `rpasurvival_test.omv` or `groomecompare_test.omv`
5. Run analysis

---

## Regenerating Data

To regenerate all test data files:

```r
# From package root
source("data-raw/rpasurvival_test_data.R")
source("data-raw/groomecompare_test_data.R")
```

This will:
1. Generate RDA files in `data/`
2. Generate CSV, XLSX, OMV in `data/nonrda/`
3. Create multi-sheet Excel workbooks

---

## Summary

| Directory | File Type | Count | Purpose |
|-----------|-----------|-------|---------|
| `data/` | RDA | 872 | R package datasets |
| `data/nonrda/` | CSV | 7 | Cross-platform |
| `data/nonrda/` | OMV | 6 | Jamovi testing |
| `data/nonrda/` | XLSX | 8 | Excel/clinical |
| **Total** | | **893** | |

---

## Maintenance

When adding new test data:

1. **Create data generation script**: `data-raw/{function}_test_data.R`
2. **Follow structure**:
   - Save RDA to: `data/`
   - Save CSV to: `data/nonrda/`
   - Save OMV to: `data/nonrda/`
   - Save XLSX to: `data/nonrda/`
3. **Document**: Update `R/data-{function}.R` with roxygen2 documentation
4. **Test**: Verify all formats can be loaded

---

**Last Updated**: 2026-01-31
**Organization**: Completed for rpasurvival and groomecompare test data
**Status**: ✅ All files organized correctly
