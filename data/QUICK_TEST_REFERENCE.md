# Quick Test Reference Card

## 🎯 Date/DateTime Module Testing

---

## 📋 Test Files Summary

| File | Rows | Columns | Purpose |
|------|------|---------|---------|
| `test_datevalidator.csv` | 100 | 13 | Date validation & quality assessment |
| `test_datetimeconverter.csv` | 150 | 15 | DateTime conversion & component extraction |

---

## ⚡ Quick Tests

### Test 1: Basic Validation (2 min)
```
Module: Date/DateTime Validator
Data: test_datevalidator.csv
Variables: date_iso, date_us, date_eu
Method: consensus
Expected: >80% success rate
```

### Test 2: Component Extraction (2 min)
```
Module: DateTime Converter
Data: test_datetimeconverter.csv
Variable: datetime_iso
Extract: Year, Month, Day, Hour, Minute, Second
Expected: All rows show 2023-03-15 14:30:45 components
```

### Test 3: Excel Date Handling (3 min)
```
Module: Date/DateTime Validator
Data: test_datevalidator.csv
Variable: date_excel
Options: handle_excel = TRUE
Expected: 44927→2023-01-01, 45000→2023-03-15
```

### Test 4: Timezone Awareness (3 min)
```
Module: DateTime Converter
Data: test_datetimeconverter.csv
Variable: datetime_timezone
Expected: Correctly parse +/-HH:MM offsets
```

---

## 🎨 Test Scenario Matrix

### `test_datevalidator.csv` - Key Rows

| Rows | Category | What It Tests | Pass/Fail |
|------|----------|---------------|-----------|
| 1-10 | Standard Formats | ISO, US, EU, text months | ✅ PASS |
| 11-12 | Missing Components | Day/month imputation | ⚠️ WARN |
| 13-14 | Invalid Dates | Feb 30, garbage text | ❌ FAIL |
| 15-20 | Edge Cases | NA, leap year, boundaries | Mixed |
| 21-40 | Separators | /, -, ., space, mixed | ✅ PASS |
| 41-47 | Numeric | Excel, Unix, scientific | ✅ PASS |
| 56-65 | Missing Data | NULL, NA, blank, special | ⚠️ WARN |
| 66-90 | International | UK, AU, CA, Asian formats | ✅ PASS |
| 91-100 | Boundaries | Y2K, DST, century, Excel limits | Mixed |

### `test_datetimeconverter.csv` - Key Rows

| Rows | Category | What It Tests | Pass/Fail |
|------|----------|---------------|-----------|
| 1-10 | Standard DateTime | ISO, regional, 12h/24h | ✅ PASS |
| 11-13 | Timestamp Formats | Unix, Excel serial | ✅ PASS |
| 14-19 | Time Precision | Midnight, noon, subseconds | ✅ PASS |
| 20-27 | Timezones | UTC, EST, PST, CET, JST | ✅ PASS |
| 32-38 | AM/PM Variants | Space, case, invalid combos | Mixed |
| 37-44 | Invalid Times | Hour 24, second 61, negatives | ❌ FAIL |
| 51-58 | Temporal Boundaries | DST, leap second, leap year | Mixed |
| 59-67 | Excel/Unix Edge | Epoch boundaries, Y2038 | ✅ PASS |
| 68-74 | Partial DateTime | Date only, time only, ambiguous 12:00 | Mixed |
| 75-78 | Missing Data | NA, NULL, invalid strings | ❌ FAIL |
| 89-100 | Real-World Formats | RFC, log files, OS-specific | Mixed |
| 117-119 | I18N | Spanish, French, German months | ❌ FAIL |
| 137-147 | Precision/Range | Subseconds, historical, future | ✅ PASS |

---

## 🏆 Success Criteria

### datevalidator

| Method | Expected % | What It's Good At |
|--------|-----------|-------------------|
| datefixR | 60-70% | Messy data, mixed formats |
| anytime | 65-75% | Timestamps, numeric dates |
| lubridate | 75-85% | Known formats, precision |
| **consensus** | **80-90%** | **Best overall** ✨ |

### datetimeconverter

| Format Type | Expected % | Notes |
|-------------|-----------|-------|
| ISO formats | 95-100% | Standard, reliable |
| Regional (US/EU) | 85-95% | Common patterns |
| Excel/Unix | 90-100% | With proper settings |
| Text months | 80-90% | English only |
| Timezones | 70-80% | Parser-dependent |

---

## ⚠️ Known Failure Points

### Both Modules

❌ **Will Fail:**
- Invalid dates: Feb 30 (rows 13, 58)
- Invalid times: hour 25, second 61
- Garbage text: "not-a-date"
- Foreign language months (unless supported)
- Relative dates: "yesterday", "2 hours ago"
- Duration formats: "PT14H30M45S"

⚠️ **Should Warn:**
- Ambiguous formats: 03/04/2023 (March 4 or April 3?)
- Pre-1900 dates
- >1 year future dates
- Leap seconds (parser-dependent)

✅ **Must Pass:**
- ISO standard: 2023-03-15, 2023-03-15 14:30:45
- Common regional: 03/15/2023, 15/03/2023
- Valid leap year: 2024-02-29
- Excel serial (with handle_excel=TRUE): 44927, 45000
- Unix epoch: 0, 1678889445

---

## 🔍 Validation Checklist

### Before Each Test

- [ ] Load correct test file
- [ ] Select appropriate column(s)
- [ ] Set method/options correctly
- [ ] Enable display options for review

### During Test

- [ ] Watch for error messages
- [ ] Check notices (ERROR/WARNING/INFO)
- [ ] Review audit table entries
- [ ] Verify success rate calculation

### After Test

- [ ] Document success rate
- [ ] Note which formats failed
- [ ] Check edge case handling
- [ ] Compare with expected results
- [ ] Save output for comparison

---

## 📊 Expected Component Extraction

### datetime_iso Column (Row 1)
```
Input: 2023-03-15 14:30:45
Expected Output:
  Year: 2023
  Month: 3
  Month Name: March
  Day: 15
  Day Name: Wednesday
  Week Number: 11
  Quarter: 1
  Hour: 14
  Minute: 30
  Second: 45
  Hour 12h: 2
  AM/PM: PM
```

### datetime_12h_pm Column (Row 6)
```
Input: 2023-03-15 02:30:45 PM
Expected Output:
  Hour (24h): 14
  Hour (12h): 2
  AM/PM: PM
  (Same date components as above)
```

### datetime_unix Column (Row 10)
```
Input: 1678889445
Expected Output:
  Converted to: 2023-03-15 14:30:45 (approx)
  (All components extracted from converted datetime)
```

---

## 🎯 Priority Test Scenarios

### High Priority (Must Test)

1. **ISO Standard Dates**
   - File: test_datevalidator.csv
   - Column: date_iso
   - Expected: 100% success

2. **ISO Standard DateTime**
   - File: test_datetimeconverter.csv
   - Column: datetime_iso
   - Expected: 100% success, all components extracted

3. **Invalid Date Detection**
   - File: test_datevalidator.csv
   - Rows: 13-14 (Feb 30, garbage)
   - Expected: Clear error messages

4. **12h to 24h Conversion**
   - File: test_datetimeconverter.csv
   - Columns: datetime_12h_am, datetime_12h_pm
   - Expected: Correct 24-hour conversion

5. **Excel Date Handling**
   - File: both
   - Columns: date_excel, datetime_excel
   - Expected: Correct with handle_excel=TRUE

### Medium Priority (Should Test)

6. **Ambiguous Date Handling**
   - File: test_datevalidator.csv
   - Row: 10 (03/04/2023)
   - Expected: Warning about ambiguity

7. **Timezone Parsing**
   - File: test_datetimeconverter.csv
   - Column: datetime_timezone
   - Expected: Offset correctly interpreted

8. **Missing Component Imputation**
   - File: test_datevalidator.csv
   - Rows: 11-12
   - Expected: Uses default imputation values

9. **Leap Year Validation**
   - File: both
   - Rows: 16-17, 57-58
   - Expected: 2024-02-29 valid, 2023-02-29 invalid

10. **Subsecond Precision**
    - File: test_datetimeconverter.csv
    - Rows: 18-19, 137-140
    - Expected: Fractional seconds captured

### Low Priority (Nice to Test)

11. **International Formats**
    - Various regional separator styles
    - Expected: Best-effort parsing

12. **Boundary Conditions**
    - Y2K, Y2038, DST transitions
    - Expected: Handled gracefully

13. **Application-Specific Formats**
    - Log files, RFC formats
    - Expected: Common formats recognized

---

## 🚀 Quick Command Reference

### Load Test Data (R console)
```r
test_val <- read.csv("data/test_datevalidator.csv")
test_conv <- read.csv("data/test_datetimeconverter.csv")
```

### Run Validation (R)
```r
result <- ClinicoPath::datevalidator(
  data = test_val,
  date_vars = c("date_iso", "date_us", "date_eu"),
  correction_method = "consensus",
  date_format = "auto",
  show_quality_assessment = TRUE
)
```

### Run Conversion (R)
```r
result <- ClinicoPath::datetimeconverter(
  data = test_conv,
  datetime_var = "datetime_iso",
  extract_all = TRUE
)
```

---

## 📈 Performance Benchmarks

| Operation | Rows | Expected Time |
|-----------|------|---------------|
| Validate 100 dates | 100 | <5 seconds |
| Convert 150 datetimes | 150 | <10 seconds |
| Consensus on all columns | 13 × 100 | <30 seconds |
| Full component extraction | 150 | <15 seconds |

*Times are approximate on modern hardware*

---

## 💡 Troubleshooting

### Low Success Rate (<60%)

1. Check method selection
2. Verify handle_excel setting for numeric dates
3. Try consensus method
4. Review column content - might be wrong column

### All Rows Failing

1. Verify data loaded correctly
2. Check column names match
3. Ensure packages installed (datefixR, anytime, lubridate)
4. Review error messages in notices

### Timezone Issues

1. Confirm TZ format is supported
2. Check if method supports timezones (anytime, lubridate do; datefixR doesn't)
3. Verify TZ offset syntax (+HH:MM or +HHMM)

### Component Extraction Missing

1. Ensure using datetimeconverter, not datevalidator
2. Check that extraction options are enabled
3. Verify datetime has time component (not just date)
4. Review column naming in output

---

## 📚 Related Documentation

- **Full Guide:** `TEST_DATA_GUIDE.md` (comprehensive testing procedures)
- **Module Docs:** See jamovi module help for detailed option descriptions
- **Code Examples:** Check module R documentation (?datevalidator, ?datetimeconverter)

---

**Quick Reference Version:** 1.0
**Last Updated:** 2025-12-25
**Print This Page:** For desk reference during testing
