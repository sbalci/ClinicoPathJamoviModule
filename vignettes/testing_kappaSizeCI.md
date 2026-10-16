# Testing Checklist: Confidence Interval Approach for the Number of Subjects Required (`kappaSizeCI`)

`kappaSizeCI` is a planning calculator: it reads no dataset, so there are no missing-value,
variable-name or plot scenarios.

## 1. Test Scenarios Matrix

| Scenario ID | Test Description | Inputs | Expected Outcome |
| :--- | :--- | :--- | :--- |
| **TC-01** | Defaults | binary, 2 raters, 0.60 in [0.40, 0.80], props 0.20/0.80 | n = 118; answer shown before the Methodology |
| **TC-02** | Published example (Rotondi & Donner 2012) | 3 categories, 3 raters, .11/.70/.19, 0.44 in [0.34, 0.54] | n = 190 (kappaSize: 171, explained in Summary) |
| **TC-03** | One-sided | `citype = one_sided` | n = 83; kappaU ignored; Summary says "not applicable" |
| **TC-04** | Invalid kappa ordering | kappa0 = kappaL, or kappaL > kappaU | jamovi error naming the rule |
| **TC-05** | Proportion input | `0,20 0,80`; `20, 80`; `20%, 80%`; `; 0.2 0.8`; sum 1.1; 3 values for binary | decimal-comma / percentage / count / sum messages; leading separator accepted |
| **TC-06** | Too narrow to size | kappaL = 0.5999 | refused, quoting the n needed and "0.0001 away" (no scientific notation) |
| **TC-07** | Rare category | props 0.02 | n = 855; "Rare category" panel |
| **TC-08** | Sparse cells / small n | kappaL 0.10, kappaU 0.95 | n = 19; Sparse categories and Small sample panels |
| **TC-09** | Turkish | jamovi language set to Turkish | every Notes, explanation and error sentence in Turkish |

## 2. Automated Test Execution

```r
testthat::test_file("tests/testthat/test-kappaSizeCI.R")
testthat::test_file("tests/testthat/test-kappaSizeCI-validation.R")
testthat::test_file("tests/testthat/test-kappasizeci-release-review.R")
```

## 3. Still to check in the jamovi app

- [ ] Valid, invalid, valid input sequence: results clear and refill.
- [ ] Reopening a saved `.omv`.
- [ ] Turkish rendering (TC-09).
