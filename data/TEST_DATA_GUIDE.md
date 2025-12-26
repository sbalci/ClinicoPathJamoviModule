# Test Data Guide for Date/DateTime Modules

This guide explains the comprehensive test datasets created for validating the `datevalidator` and `datetimeconverter` jamovi modules.

---

## 📁 Test Files

### 1. `test_datevalidator.csv` (100 test cases)
**Purpose:** Test date validation and quality assessment
**Columns:** 13 different date format columns + scenario descriptions
**Module:** Date/DateTime Validator

### 2. `test_datetimeconverter.csv` (150 test cases)
**Purpose:** Test datetime conversion and component extraction
**Columns:** 15 different datetime format columns + scenario descriptions
**Module:** DateTime Converter

---

## 🎯 Test Coverage Overview

### `test_datevalidator.csv` - Date Formats Tested

#### Standard Format Categories (Rows 1-10)
| Row | Scenario | Format | Example | Expected Behavior |
|-----|----------|--------|---------|-------------------|
| 1 | ISO Standard | YYYY-MM-DD | 2023-03-15 | ✅ Should parse perfectly |
| 2 | US Format | MM/DD/YYYY | 03/15/2023 | ✅ Should parse with MDY |
| 3 | European Format | DD/MM/YYYY | 15/03/2023 | ✅ Should parse with DMY |
| 4 | Mixed Separators | YYYY.MM.DD | 2023.03.15 | ⚠️ Should parse, may need lubridate |
| 5-6 | Text Months | Month DD YYYY | March 15 2023 | ✅ Should parse with text month support |
| 7 | Short Year | MM/DD/YY | 03/15/23 | ✅ Should parse, assume 2000s |
| 8-9 | Excel Serial | Numeric | 44927, 45000 | ✅ Should parse with handle_excel=TRUE |
| 10 | Ambiguous | DD/MM/YYYY or MM/DD/YYYY | 03/04/2023 | ⚠️ Ambiguity warning expected |

#### Edge Cases & Validation (Rows 11-20)
| Row | Scenario | Test Purpose | Expected Result |
|-----|----------|--------------|-----------------|
| 11 | Missing Day | 2023-03 | ⚠️ Should impute day (default: 1st) |
| 12 | Missing Month | 2023-*-15 | ❌ Should fail or impute month (default: July) |
| 13 | Invalid Date | 2023-02-30 | ❌ Should fail - Feb doesn't have 30 days |
| 14 | Invalid Format | "not-a-date" | ❌ Should fail validation |
| 15 | Empty/NA | NA | ⚠️ Should report as missing |
| 16 | Leap Year Valid | 2024-02-29 | ✅ Should parse - valid leap year |
| 17 | Leap Year Invalid | 2023-02-29 | ❌ Should fail - 2023 not leap year |
| 18 | End of Month | 2023-01-31 | ✅ Should parse - valid date |
| 19 | Future Date | 2025-12-25 | ⚠️ Plausibility flag if >1 year future |
| 20 | Pre-1900 Date | 1899-12-31 | ⚠️ Plausibility flag for pre-1900 |

#### Separator & Format Variations (Rows 21-40)
- **Different Separators:** slash (/), dash (-), dot (.), space, mixed
- **Zero Padding:** With/without leading zeros
- **Numeric Formats:** 6-digit, 8-digit compact formats
- **Special Formats:** Timestamps, reversed order, tabs

#### Excel & Numeric Representations (Rows 41-47)
| Type | Example | Notes |
|------|---------|-------|
| Unix Timestamp | 1678838400 | Seconds since 1970-01-01 |
| Millisecond Timestamp | 1678838400000 | JavaScript-style timestamp |
| Excel Serial (recent) | 45500 | Days since 1900-01-01 |
| Excel Serial (old) | 25000 | Historical dates |
| Scientific Notation | 4.4927e4 | Excel export format |
| Negative Numbers | -100 | Invalid, should fail |
| Decimal Numbers | 45000.5 | Excel with time component |

#### Missing Data Representations (Rows 56-65)
Tests various ways missing data might appear:
- `null`, `NULL`, `NA`, `N/A`
- Empty strings, whitespace
- Special characters: `.`, `?`, `-`, `***`

#### International & Regional Formats (Rows 66-90)
- UK format: DD-Mon-YYYY
- Australian format: DD/M/YY
- Canadian format: MM/DD/YYYY
- Asian formats: YYYY/MM/DD
- Various separators: comma, semicolon, pipe, backslash

#### Boundary & Special Cases (Rows 91-100)
- Excel date boundaries (min/max reasonable values)
- Y2K boundary (2000-01-01)
- Century transitions
- DST transitions
- Leap seconds

---

### `test_datetimeconverter.csv` - DateTime Formats Tested

#### Basic DateTime Formats (Rows 1-15)
| Row | Scenario | Format | Example | Expected Components |
|-----|----------|--------|---------|---------------------|
| 1 | ISO Standard | YYYY-MM-DD HH:MM:SS | 2023-03-15 14:30:45 | Y:2023, M:3, D:15, h:14, m:30, s:45 |
| 2-3 | Regional | US/EU + time | 03/15/2023 14:30:45 | Same components, different parse |
| 5-6 | 12-Hour Time | AM/PM format | 02:30:45 AM/PM | Should convert to 24h correctly |
| 7-8 | Text Months | Full/abbreviated | March 15 2023 14:30:45 | Month name parsing |
| 9 | Excel Serial | Number | 44927.604688 | Both date AND time components |
| 10-11 | Unix Time | Seconds/milliseconds | 1678889445 | Epoch conversion |
| 12-13 | Incomplete Time | Missing components | No seconds/minutes | Should handle gracefully |

#### Time Precision & Special Values (Rows 14-19)
| Row | Scenario | Value | Test Purpose |
|-----|----------|-------|--------------|
| 14 | Midnight | 00:00:00 | Boundary time value |
| 15 | Last Second | 23:59:59 | End of day boundary |
| 16 | Noon | 12:00:00 | 12h/24h ambiguity |
| 17 | End of Day | 23:59:59 | Same as row 15 |
| 18 | Microseconds | .123456 | Subsecond precision |
| 19 | Milliseconds | .123 | Subsecond precision |

#### Timezone Handling (Rows 20-27)
Tests datetime with explicit timezone offsets:
- UTC (+00:00)
- US timezones (EST -05:00, PST -08:00)
- European (CET +01:00)
- Asian (JST +09:00)
- ISO separators (T, Z)

#### ISO 8601 Variations (Rows 25-31)
- `2023-03-15T14:30:45` - T separator
- `2023-03-15T14:30:45Z` - UTC indicator
- `2023-03-15T14:30:45+00:00` - With timezone
- `20230315143045` - Compact format

#### 12-Hour Format Variations (Rows 32-38)
Tests various AM/PM representations:
- With/without space before AM/PM
- Uppercase/lowercase am/pm
- Invalid values (hour 13 in 12h format)
- Hour 24 (should be invalid)

#### Invalid & Edge Cases (Rows 37-44)
| Row | Invalid Value | Why Invalid | Expected |
|-----|---------------|-------------|----------|
| 37 | 24:00:00 | Hour must be 0-23 | Should fail |
| 38 | 13:30:45 PM | Hour 13 in 12h format | Should fail |
| 39 | Second 60 | Leap second (rare, valid) | ⚠️ May fail in some parsers |
| 40 | Second 61 | Invalid second | Should fail |
| 41 | Minute 60 | Invalid minute | Should fail |
| 42-44 | Negative values | Negative h/m/s | Should fail |

#### Separator Variations (Rows 45-48)
- Mixed date/time separators
- Comma, dot, semicolon as time separators
- Non-standard punctuation

#### Precision & Padding (Rows 49-50)
- Missing leading zeros (2:3:5)
- Extra leading zeros (014:030:045)

#### Temporal Boundaries (Rows 51-58)
- DST transitions (Spring forward, Fall back)
- New Year's midnight
- Leap seconds (June 30, December 31)
- Leap year Feb 29 (valid vs invalid)

#### Excel DateTime (Rows 59-62)
Excel serial number with time component:
- Morning (0.104167 = ~02:30)
- Afternoon (0.604167 = ~14:30)
- Midnight (0.000000)
- Near midnight (0.999988)

#### Unix Epoch Boundaries (Rows 63-67)
| Row | Timestamp | Date | Notes |
|-----|-----------|------|-------|
| 63 | 0 | 1970-01-01 00:00:00 | Unix epoch start |
| 64 | 2147483647 | 2038-01-19 03:14:07 | 32-bit overflow (Y2038) |
| 65 | -100000 | Pre-1970 | Negative timestamps |
| 66-67 | Recent | 2023+ | Modern timestamps |

#### Subsecond Precision (Rows 68-69, 137-140)
Tests different decimal precision levels:
- Microseconds (6 digits): .123456
- Milliseconds (3 digits): .123
- Nanoseconds (9 digits): .123456789
- Variable precision (1, 2, 4, 7 digits)

#### Partial DateTime (Rows 70-74)
- Date only (no time)
- Time only (no date)
- 12:00 AM/PM ambiguity

#### Missing Data (Rows 75-78)
Same as datevalidator: NA, null, NULL, invalid strings

#### Standard Format Violations (Rows 79-88)
- Invalid hour (25)
- Invalid minute (61)
- Trailing/leading spaces
- Multiple spaces, tabs, newlines

#### RFC & Standard Formats (Rows 89-92)
- RFC 2822: Wed 15 Mar 2023 14:30:45
- RFC 3339: 2023-03-15T14:30:45.000Z
- ISO 8601 extended: With milliseconds and TZ
- ISO 8601 basic: Compact format

#### Application-Specific Formats (Rows 93-100)
- Military time (HHMM)
- Aviation time (HHMMZ)
- Medical chart format
- Log file formats (syslog, Apache, etc.)
- OS-specific (Windows, Mac)

#### ISO Extended Formats (Rows 101-104)
- ISO week date: 2023-W11-3
- Ordinal date: 2023-074
- Quarterly formats
- Fiscal year formats

#### Custom Separators (Rows 105-116)
Tests unusual but potentially valid separators:
- @ # symbols
- Mixed case AM/PM
- A.M./P.M. with periods
- Single digit hour/minute/second

#### Internationalization (Rows 117-119)
Month names in different languages:
- Spanish: marzo
- French: mars
- German: März

#### Relative & Duration (Rows 120-123)
- Relative: "2 hours ago", "in 3 hours"
- ISO duration: PT14H30M45S
- ISO duration full: P0Y0M0DT14H30M45S

#### Excel Edge Cases (Rows 124-130)
- Time only (0.604167)
- Scientific notation
- Very small/large numbers
- Negative (invalid)
- Excel 1900 boundary

#### Timezone Variants (Rows 131-136)
- Offset without colon
- Negative offsets
- City names (EST, GMT)
- GMT+offset
- Multiple TZ indicators (invalid)

#### Historical & Future Dates (Rows 141-147)
- Before Unix epoch (1969)
- Excel era (1900-1901)
- Pre-Excel (1899)
- Far future (2100)
- Y2038 problem dates

---

## 🧪 Testing Procedures

### Testing `datevalidator` Module

#### Test 1: Basic Validation
**Objective:** Verify all standard formats are recognized

```
1. Open test_datevalidator.csv in jamovi
2. Run Date/DateTime Validator with:
   - Method: Auto Detection (datefixR)
   - Format: Auto-detect
   - All display options: TRUE
3. Expected Results:
   - Rows 1-10: High success rate (80-100%)
   - Clear identification of format types
   - Audit table shows all conversions
```

#### Test 2: Method Comparison
**Objective:** Compare different validation methods

```
For each method (datefixR, anytime, lubridate, consensus):
1. Run validator on ALL columns
2. Compare success rates
3. Note which method handles which formats best

Expected findings:
- datefixR: Best for mixed/messy formats
- anytime: Fast, handles numeric timestamps
- lubridate: Best when format is known
- consensus: Highest overall success, identifies conflicts
```

#### Test 3: Format-Specific Validation
**Objective:** Test format specification

```
Test each format option:
1. DMY on date_eu column (rows with European dates)
2. MDY on date_us column (rows with US dates)
3. YMD on date_iso column (rows with ISO dates)

Expected: Near 100% success when format matches data
```

#### Test 4: Edge Case Handling
**Objective:** Validate error detection

```
Focus on rows 13-20, 37-47:
1. Check that invalid dates are flagged
2. Verify plausibility warnings (future/past)
3. Confirm leap year validation
4. Test missing value handling

Expected notices:
- ERROR: Invalid dates (Feb 30, invalid formats)
- WARNING: Plausibility flags (pre-1900, >1yr future)
- INFO: Successful parsing summary
```

#### Test 5: Ambiguity Detection
**Objective:** Test ambiguous format handling

```
Test row 10 and similar:
1. Use auto-detect
2. Check for ambiguity warnings
3. Verify chosen interpretation is documented

Expected: Consensus method should flag ambiguous cases
```

#### Test 6: Missing Data
**Objective:** Handle all missing data representations

```
Test rows 56-65 (missing data variants):
1. Verify all are recognized as missing
2. Check that they don't cause errors
3. Confirm proper reporting in quality assessment
```

#### Test 7: Quality Assessment
**Objective:** Validate reporting accuracy

```
1. Run on full dataset
2. Check Quality Assessment output:
   - Success rate calculation accuracy
   - Common error pattern detection
   - Method performance breakdown
3. Verify recommendations are contextually appropriate
```

#### Test 8: Excel Date Handling
**Objective:** Test Excel serial number conversion

```
Rows 8-9, 39-47 (Excel numeric dates):
1. Run with handle_excel = FALSE → Should fail
2. Run with handle_excel = TRUE → Should succeed
3. Verify converted dates are correct:
   - 44927 = 2023-01-01
   - 45000 = 2023-03-15
```

---

### Testing `datetimeconverter` Module

#### Test 1: Component Extraction
**Objective:** Verify all components are extracted correctly

```
1. Open test_datetimeconverter.csv
2. Run DateTime Converter on datetime_iso column
3. Verify extracted components:
   - Year = 2023
   - Month = 3 (numeric) / "March" (text)
   - Day = 15
   - Hour = 14
   - Minute = 30
   - Second = 45
   - Day name = "Wednesday"
   - Week number = 11
   - Quarter = 1
```

#### Test 2: 12h vs 24h Conversion
**Objective:** Test AM/PM handling

```
Test rows 5-6, 32-35:
1. Parse 12h AM times → Hour should be 2
2. Parse 12h PM times → Hour should be 14
3. Special case 12:00 AM → Hour should be 0
4. Special case 12:00 PM → Hour should be 12

Verify conversion to 24-hour format is accurate
```

#### Test 3: Timezone Handling
**Objective:** Test timezone awareness

```
Rows 20-24, 131-136:
1. Parse datetime with various TZ offsets
2. Check if output is normalized to UTC or local
3. Verify timezone offset is correctly interpreted
4. Test edge cases (no colon in offset, city names)
```

#### Test 4: Excel Serial Conversion
**Objective:** Test Excel datetime with time component

```
Rows 9, 59-62, 124-130:
1. Convert Excel serial + decimal time
2. Verify both date AND time are correct:
   - 44927.604688 → 2023-01-01 14:30:24 (approx)
   - 0.604167 → Time only: 14:30:00 (approx)
   - 44927.000000 → 2023-01-01 00:00:00
```

#### Test 5: Unix Timestamp Conversion
**Objective:** Test epoch time handling

```
Rows 10-11, 63-67:
1. Seconds timestamp: 1678889445
2. Millisecond timestamp: 1678889445000
3. Negative timestamp (pre-1970)
4. Epoch zero (1970-01-01 00:00:00)
5. Y2038 boundary (2147483647)

Verify correct conversion to readable datetime
```

#### Test 6: Subsecond Precision
**Objective:** Test fractional seconds

```
Rows 18-19, 68-69, 137-140:
1. Test various precision levels (1-9 digits)
2. Verify milliseconds are captured
3. Check microsecond handling
4. Confirm truncation vs rounding behavior
```

#### Test 7: Format Auto-Detection
**Objective:** Test parser flexibility

```
Run on ALL columns with auto-detect:
1. ISO format column → Should parse perfectly
2. US format column → Should detect and parse
3. Text month column → Should handle correctly
4. Mixed format columns → Check success rate

Note which formats are automatically recognized
```

#### Test 8: Invalid DateTime Handling
**Objective:** Test error handling

```
Rows 37-44, 79-80:
1. Hour 24 → Should fail
2. Second 61 → Should fail
3. Negative values → Should fail
4. Minute 60 → Should fail

Verify errors are reported clearly
```

#### Test 9: Incomplete DateTime
**Objective:** Handle partial information

```
Rows 12-13, 70-72:
1. Date only (no time) → Extract date components
2. Time only (no date) → Extract time components
3. No seconds → Should set seconds to 0
4. No minutes → Should set minutes to 0
```

#### Test 10: Application Format Compatibility
**Objective:** Test real-world log formats

```
Rows 89-100:
1. RFC 2822 format (email headers)
2. ISO 8601 (web APIs)
3. Syslog format (server logs)
4. Apache log format (web servers)
5. Windows/Mac OS formats

Verify compatibility with common datetime sources
```

#### Test 11: Boundary & Edge Cases
**Objective:** Test temporal boundaries

```
Rows 51-58, 141-147:
1. DST transitions → Verify correct handling
2. New Year midnight → Check date rollover
3. Leap seconds → Note if supported
4. Leap year Feb 29 → Validate vs non-leap
5. Y2038 problem date → Check if handled
6. Historical dates (pre-1900) → Verify range
```

#### Test 12: Quality vs Speed
**Objective:** Performance testing

```
1. Run on full 150 rows
2. Measure processing time
3. Compare methods:
   - Auto-detect vs specific format
   - Different parser backends
4. Check for any performance degradation

Note: Datetime parsing should complete in <10 seconds
```

---

## ✅ Expected Success Rates

### `test_datevalidator.csv`

| Method | Expected Success Rate | Notes |
|--------|----------------------|-------|
| datefixR | 60-70% | Strong with messy data, no YMD/HMS support |
| anytime | 65-75% | Fast, handles timestamps well |
| lubridate | 75-85% | Best when format is specified correctly |
| consensus | 80-90% | Highest success, combines all methods |

**Known Challenges:**
- Rows 13-14, 40-50 (invalid dates) → Should fail
- Rows 11-12 (missing components) → May fail or impute
- Rows 56-65 (missing data) → Should report as NA
- Row 10 (ambiguous) → Should warn

### `test_datetimeconverter.csv`

| Column Type | Expected Success Rate | Notes |
|-------------|----------------------|-------|
| ISO formats | 95-100% | Standard, should parse perfectly |
| US/EU formats | 85-95% | Common, well-supported |
| Text months | 80-90% | Depends on parser support |
| 12h AM/PM | 75-85% | Ambiguity at 12:00 |
| Excel serial | 90-100% | With proper Excel handling |
| Unix timestamp | 95-100% | Standard epoch conversion |
| Timezone aware | 70-80% | TZ parsing varies by method |
| Custom formats | 30-50% | Many are intentionally invalid |

**Known Challenges:**
- Rows 37-44 (invalid times) → Should fail
- Rows 75-78 (missing data) → Should report as NA
- Rows 117-119 (foreign languages) → May fail
- Rows 120-123 (relative/duration) → Likely to fail

---

## 🐛 Known Issues & Expected Failures

### datevalidator

1. **Row 12 (missing month):** The format `2023-*-15` is non-standard and will likely fail with all methods
2. **Row 50 (Roman numerals):** `III-XV-MMXXIII` will fail - not supported
3. **Row 55 (Chinese characters):** `2023年3月15日` may fail with English-only parsers
4. **Rows 73-76 (Quarter/Week/Relative):** Not standard date formats, will fail
5. **Row 98 (Leap second):** `23:59:60` may fail in some parsers

### datetimeconverter

1. **Rows 42-44 (Negative time components):** Should fail - invalid syntax
2. **Rows 117-119 (Foreign month names):** Will fail unless language-aware
3. **Rows 120-123 (Relative/Duration):** Not absolute datetimes, will fail
4. **Row 88 (Newline in string):** May fail due to CSV parsing issues
5. **Row 136 (Multiple TZ):** `2023-03-15T14:30:45Z+00:00` is contradictory, should fail

---

## 📊 Reporting Test Results

After running tests, document:

### For Each Module

1. **Overall Success Rate:** X% of total test cases
2. **Method Comparison:** Which method performed best
3. **Format Coverage:** Which formats were successfully parsed
4. **Error Patterns:** Common failure modes
5. **Performance:** Processing time for 100-150 rows
6. **Edge Case Handling:** How boundaries were managed

### Critical Validations

✅ **Must Pass:**
- Standard ISO dates/datetimes (rows 1)
- Common US/EU formats (rows 2-3)
- Valid leap year dates (row 16 / 57)
- Excel serial numbers (rows 8-9 with handle_excel=TRUE)

❌ **Must Fail:**
- Invalid dates like Feb 30 (row 13 / 58)
- Completely invalid formats (row 14 / 78)
- Out-of-range time values (rows 37-41)

⚠️ **Should Warn:**
- Ambiguous formats (row 10)
- Pre-1900 dates (row 20)
- >1 year future dates (row 19)
- Leap seconds (rows 39, 55-56)

---

## 🔄 Continuous Testing

Use these datasets as regression tests:

1. **Before Release:** Run full test suite, document results
2. **After Updates:** Compare with previous results
3. **Bug Reports:** Add new edge cases to test data
4. **User Feedback:** Incorporate real-world problematic formats

### Automated Testing Script Template

```r
# Load test data
test_data <- read.csv("data/test_datevalidator.csv")

# Run validation
results <- ClinicoPath::datevalidator(
  data = test_data,
  date_vars = colnames(test_data)[3:ncol(test_data)],
  correction_method = "consensus",
  show_quality_assessment = TRUE
)

# Extract success rate
# Compare with expected benchmark
# Flag if success rate drops below threshold
```

---

## 📝 Contributing New Test Cases

When adding new test scenarios:

1. **Document the scenario:** What format/edge case does it test?
2. **Expected outcome:** Should pass, fail, or warn?
3. **Real-world source:** Where would this format appear?
4. **Add to appropriate row:** Keep related tests together
5. **Update this guide:** Document the new test case

### Test Case Template

```csv
patient_id,scenario,date_column,expected_result,notes
XXX,Descriptive Name,YYYY-MM-DD,PASS/FAIL/WARN,Why this test matters
```

---

## 🎓 Learning Outcomes

Using these test datasets helps understand:

1. **Format Diversity:** How many ways dates can be represented
2. **Parser Limitations:** What each method can/cannot handle
3. **Data Quality:** Common problems in real-world data
4. **Best Practices:** When to use which validation method
5. **Error Messages:** How failures are communicated

---

## 📚 References

- ISO 8601: Date and time format standard
- RFC 2822: Email date format
- RFC 3339: Internet date/time format
- Excel Serial Dates: Days since 1900-01-01
- Unix Epoch: Seconds since 1970-01-01 00:00:00 UTC
- Y2038 Problem: 32-bit signed integer overflow

---

## ✨ Quick Start Testing

### 5-Minute Validator Test

```
1. Open test_datevalidator.csv
2. Select date_iso, date_us, date_eu columns
3. Run Date/DateTime Validator
4. Method: consensus, Format: auto
5. Enable all display options
6. Check success rate >80%
```

### 5-Minute Converter Test

```
1. Open test_datetimeconverter.csv
2. Select datetime_iso column
3. Run DateTime Converter
4. Enable all component extractions
5. Verify year=2023, month=3, day=15, hour=14
6. Check that new columns are added correctly
```

---

**Last Updated:** 2025-12-25
**Test Data Version:** 1.0
**Compatible Modules:** datevalidator v0.0.3, datetimeconverter v0.0.1+
