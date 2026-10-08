# Testing Checklist: Co-Testing Analysis (`cotest`)

`cotest` is a calculator: it takes no dataset, only typed sensitivities, specificities, a
prevalence and two dependence correlations (or a worked example). Data-input scenarios such as
missing values, factor levels and column names do not apply.

## 1. Test Scenarios Matrix

| Scenario ID | Test Description | Input Conditions | Expected Outcome |
| :--- | :--- | :--- | :--- |
| **TC-01** | Defaults | Se 0.80/0.75, Sp 0.90/0.95, prevalence 0.10, phi 0.05/0.05 | Either+ 42.5%, Both+ 89.1%, Both- 0.75%; the sentence marks phi as defaults |
| **TC-02** | Independence | Same inputs, `indep = TRUE` | Exact Bayes: Both+ 120/129, Either+ 190/451 |
| **TC-03** | Truncated dependence, one group | Sp 0.99/0.90, phi- 0.40 | One row is exactly 1 with Inf odds, a "Not an estimate" footnote and a truncation notice |
| **TC-04** | Impossible in both groups | phi 0.80/0.80 | That row is NA (jamovi shows NaN) with a "Not estimable" footnote; the other rows are finite |
| **TC-05** | Below the chance diagonal | Se + Sp < 1 for one test | A reversed-entry warning, the direction words follow the ratios, and the Fagan nomogram is withheld with a reason |
| **TC-06** | Worked example from R | `preset = "psa_dre", prevalence = 0.6` | Demonstration markers on both tables, the caption and the sentence; the override notice names the discarded value |
| **TC-07** | Out-of-range entry | `test1_sens = 80` | jmvcore rejects it ("between 0.01 and 0.99"); the GUI does not clamp |
| **TC-08** | Hostile test names | `<b>`, quotes, `%s`, `Ki-67 <10%` | Cells break only tag-like `<`, the HTML is escaped, and `asSource()` parses back |

## 2. Automated Test Execution

```r
devtools::load_all()
for (f in c("test-cotest.R", "test-cotest-release-review.R", "test-cotest-validation.R"))
    testthat::test_file(file.path("tests/testthat", f))
```

`test-cotest-validation.R` checks the numbers against exact-fraction hand calculations. The
other two files cover regressions, text and wiring.

## 3. Manual Checks (jamovi app, not reachable from testthat)

- [ ] Choose a worked example, then switch back to Custom values: every box resets to its default.
- [ ] Undo straight after changing the worked example. The client runs `updateOptions` outside
      data initialisation, so this may re-run the preset handler; check what the boxes show.
- [ ] An impossible row (TC-04) shows NaN with its footnote mark.
- [ ] The Fagan nomogram follows the dark theme and its caption names the parallel rule.
