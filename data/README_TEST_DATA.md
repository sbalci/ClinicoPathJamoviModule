# Test Data for Date/DateTime Modules

This directory contains comprehensive test datasets for validating the `datevalidator` and `datetimeconverter` jamovi modules.

## 📁 Files in This Directory

### Test Data Files

1. **`test_datevalidator.csv`** (100 rows × 13 columns)
   - Comprehensive date format testing
   - Covers ISO, US, EU, text months, Excel serial, ambiguous formats
   - Includes edge cases, invalid dates, missing data
   - Use with: **Date/DateTime Validator** module

2. **`test_datetimeconverter.csv`** (150 rows × 15 columns)
   - Comprehensive datetime format testing
   - Covers ISO, 12h/24h, timezones, subseconds
   - Includes Unix timestamps, Excel serial with time
   - Use with: **DateTime Converter** module

3. **`test_expected_results.csv`**
   - Reference file with expected outcomes for key test cases
   - Use for validation: compare your results against these benchmarks
   - Includes expected status (Success/Failed/Warning)
   - Shows expected component extraction values

### Documentation Files

4. **`TEST_DATA_GUIDE.md`** (Full Guide)
   - Comprehensive testing procedures
   - Detailed test case descriptions
   - Expected success rates
   - Troubleshooting guidance
   - **Read this first for complete understanding**

5. **`QUICK_TEST_REFERENCE.md`** (Quick Reference)
   - One-page summary for quick testing
   - Priority test scenarios
   - Expected results matrix
   - **Print this for desk reference**

6. **`README_TEST_DATA.md`** (This File)
   - Overview of test data structure
   - Quick start guide

---

## 🚀 Quick Start

### Test the Date Validator (5 minutes)

1. Open `test_datevalidator.csv` in jamovi
2. Go to: **Analyses** → **SurvivalT** → **Data Preparation** → **Date/DateTime Validator**
3. Move to "Date Variables to Validate": `date_iso`, `date_us`, `date_eu`
4. Set **Validation Method**: Multi-Method Consensus
5. Set **Date Format**: Auto-detect
6. Enable all Display Options checkboxes
7. Click Run

**Expected Results:**
- Overall success rate: 80-90%
- Validated Data table shows all conversions
- Quality Assessment shows success breakdown

### Test the DateTime Converter (5 minutes)

1. Open `test_datetimeconverter.csv` in jamovi
2. Go to: **Analyses** → **SurvivalT** → **Data Preparation** → **DateTime Converter**
3. Select **DateTime Variable**: `datetime_iso`
4. Enable: Extract Year, Month, Day, Hour, Minute, Second
5. Click Run

**Expected Results:**
- New columns added: `year`, `month`, `day`, `hour`, `minute`, `second`
- Row 1 values: 2023, 3, 15, 14, 30, 45
- Success rate: >95%

---

## 📊 Test Data Structure

### `test_datevalidator.csv` Columns

| Column | Description | Example Values |
|--------|-------------|----------------|
| `patient_id` | Row identifier | 1, 2, 3, ... |
| `scenario` | Test description | "ISO Standard", "US Format" |
| `date_iso` | ISO format dates | 2023-03-15 |
| `date_us` | US format dates | 03/15/2023 |
| `date_eu` | European format | 15/03/2023 |
| `date_mixed` | Mixed separators | 2023.03.15 |
| `date_text_month` | Month names | March 15 2023 |
| `date_short_year` | 2-digit years | 03/15/23 |
| `date_excel` | Excel serial numbers | 44927, 45000 |
| `date_ambiguous` | Ambiguous DMY/MDY | 03/04/2023 |
| `date_missing_day` | Missing day component | 2023-03 |
| `date_missing_month` | Missing month | 2023-*-15 |
| `date_invalid` | Invalid dates | 2023-02-30, "not-a-date" |
| `date_edge_case` | Boundary cases | Leap year, pre-1900, future |

### `test_datetimeconverter.csv` Columns

| Column | Description | Example Values |
|--------|-------------|----------------|
| `patient_id` | Row identifier | 1, 2, 3, ... |
| `scenario` | Test description | "ISO Standard DateTime" |
| `datetime_iso` | ISO datetime | 2023-03-15 14:30:45 |
| `datetime_us` | US format datetime | 03/15/2023 14:30:45 |
| `datetime_eu` | EU format datetime | 15/03/2023 14:30:45 |
| `datetime_24h` | 24-hour time | 2023-03-15 14:30:45 |
| `datetime_12h_am` | 12-hour AM | 2023-03-15 02:30:45 AM |
| `datetime_12h_pm` | 12-hour PM | 2023-03-15 02:30:45 PM |
| `datetime_text_month` | Month names | March 15 2023 14:30:45 |
| `datetime_excel` | Excel serial + time | 44927.604688 |
| `datetime_unix` | Unix timestamp | 1678889445 |
| `datetime_millisec` | Millisecond timestamp | 1678889445000 |
| `datetime_no_seconds` | HH:MM only | 2023-03-15 14:30 |
| `datetime_no_minutes` | Hour only | 2023-03-15 14:00 |
| `datetime_edge_case` | Boundary cases | Midnight, leap second |
| `datetime_timezone` | With timezone | 2023-03-15 14:30:45+00:00 |

---

## 🎯 Key Test Scenarios

### Must Pass ✅
- ISO standard formats (row 1 in both files)
- Common US/EU formats (rows 2-3)
- Valid leap year dates (row 16/57)
- Excel serial with proper settings
- 12h AM/PM conversion to 24h

### Must Fail ❌
- Invalid dates like Feb 30
- Invalid times like hour 25
- Completely malformed input
- Out-of-range components

### Should Warn ⚠️
- Ambiguous date formats
- Pre-1900 dates
- >1 year future dates
- Leap seconds (parser-dependent)

---

## 📈 Expected Performance

### datevalidator Success Rates

| Method | Expected % | Best For |
|--------|-----------|----------|
| datefixR | 60-70% | Messy, mixed formats |
| anytime | 65-75% | Timestamps, numeric |
| lubridate | 75-85% | Known formats |
| **consensus** | **80-90%** | **Recommended** ✨ |

### datetimeconverter Success Rates

| Format | Expected % | Notes |
|--------|-----------|-------|
| ISO | 95-100% | Standard format |
| Regional | 85-95% | US/EU common patterns |
| Excel/Unix | 90-100% | With proper handling |
| Text months | 80-90% | English only |
| Timezones | 70-80% | Parser-dependent |

---

## 🔍 Using Expected Results File

The `test_expected_results.csv` file contains benchmark results for key test cases.

**How to use:**

1. Run your test (e.g., validate `date_iso` column)
2. Compare results with `test_expected_results.csv`
3. Check:
   - Does `expected_status` match your result?
   - Do extracted components match `expected_year`, `expected_month`, etc.?
   - Are failures occurring where expected?

**Example validation:**

```r
# Your result for row 1, date_iso
your_result <- "2023-03-15"  # From validator output

# Expected result
expected <- read.csv("data/test_expected_results.csv")
expected_row1 <- expected[expected$row_num == 1 &
                           expected$column_name == "date_iso", ]

# Compare
identical(your_result, expected_row1$expected_result)  # Should be TRUE
```

---

## 🐛 Common Issues

### Issue: Low success rate (<50%)

**Possible causes:**
- Wrong validation method selected
- `handle_excel` not enabled for numeric dates
- Column selection error
- Data not loaded correctly

**Solutions:**
- Try consensus method
- Enable Excel handling for numeric columns
- Verify column names
- Reload data file

### Issue: All rows failing

**Possible causes:**
- Missing required packages
- Incorrect column selection
- Data format corruption

**Solutions:**
```r
# Check packages installed
install.packages(c("datefixR", "anytime", "lubridate"))

# Verify data loaded
str(test_data)
head(test_data$date_iso)

# Check for errors in jamovi notices panel
```

### Issue: Component extraction not working

**Possible causes:**
- Using datevalidator instead of datetimeconverter
- Extraction options not enabled
- Column has date only (no time)

**Solutions:**
- Use **DateTime Converter** module (not Validator)
- Check extraction option checkboxes
- Verify input has time component

---

## 📝 Adding New Test Cases

When you discover new edge cases or format problems:

1. Add row to appropriate test file
2. Document in `scenario` column
3. Add expected result to `test_expected_results.csv`
4. Update `TEST_DATA_GUIDE.md` with new scenario
5. Submit as issue/PR on GitHub

**Template for new test case:**

```csv
patient_id,scenario,date_iso,expected_year,expected_month,expected_day
XXX,Description of edge case,YYYY-MM-DD,YYYY,M,D
```

---

## 🎓 Learning Resources

### Understanding Date/DateTime Parsing

- **ISO 8601:** International standard (YYYY-MM-DD, YYYY-MM-DDTHH:MM:SS)
- **Unix Epoch:** Seconds since 1970-01-01 00:00:00 UTC
- **Excel Serial:** Days since 1900-01-01 (with 1900 leap year bug)
- **Y2038 Problem:** 32-bit signed integer overflow at 2038-01-19 03:14:07

### Common Gotchas

1. **12:00 AM vs 12:00 PM**
   - 12:00 AM = midnight = 00:00 (24h)
   - 12:00 PM = noon = 12:00 (24h)

2. **Leap Years**
   - Divisible by 4: leap year (except...)
   - Divisible by 100: NOT leap year (except...)
   - Divisible by 400: IS leap year
   - Examples: 2000 (leap), 1900 (not), 2024 (leap), 2023 (not)

3. **Timezones**
   - UTC = +00:00 = Z in ISO format
   - EST = UTC-5, PST = UTC-8, CET = UTC+1
   - DST adds 1 hour in summer (e.g., EDT = UTC-4)

4. **Excel Date Bug**
   - Excel incorrectly treats 1900 as leap year
   - Dates before March 1, 1900 may be off by 1 day
   - This test dataset uses dates after 1900-03-01

---

## 📞 Support

### If Tests Are Failing Unexpectedly

1. **Check Documentation:** Read `TEST_DATA_GUIDE.md` section for that test
2. **Compare Expected:** Review `test_expected_results.csv`
3. **Verify Settings:** Ensure correct method/options selected
4. **Report Issue:** If bug found, file issue with:
   - Test file name
   - Row number
   - Expected vs actual result
   - jamovi version
   - Module version

### If You Have Questions

- **Module Functionality:** See jamovi module help (? button)
- **Test Procedures:** See `TEST_DATA_GUIDE.md`
- **Quick Reference:** See `QUICK_TEST_REFERENCE.md`
- **Bug Reports:** GitHub issues

---

## 📦 Test Data Version History

### Version 1.0 (2025-12-25)
- Initial release
- 100 date test cases
- 150 datetime test cases
- Comprehensive edge case coverage
- Expected results benchmark file
- Full documentation suite

---

## ✅ Validation Checklist

Before reporting test results:

- [ ] Loaded correct test file
- [ ] Selected appropriate column(s)
- [ ] Used recommended method (consensus for datevalidator)
- [ ] Enabled all display options
- [ ] Reviewed audit table
- [ ] Compared with expected results
- [ ] Documented success rate
- [ ] Noted any unexpected failures
- [ ] Checked edge case handling
- [ ] Verified component extraction (datetimeconverter)

---

**Test Data Maintainer:** ClinicoPath Development Team
**Last Updated:** 2025-12-25
**Compatible Modules:** datevalidator v0.0.3+, datetimeconverter v0.0.1+
**Questions:** File issue on GitHub repository
