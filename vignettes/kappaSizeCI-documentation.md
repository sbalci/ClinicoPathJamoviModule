# Confidence Interval Approach for the Number of Subjects Required - Developer Documentation

## 1. Overview

- **Function**: `kappaSizeCI` (menu: Power Analysis by meddecide; `menuGroup: PowerT` while under test)
- **Files**:
  - `jamovi/kappaSizeCI.a.yaml` - options
  - `jamovi/kappaSizeCI.u.yaml` - UI; `jamovi/js/kappaSizeCI.events.js` resets `props` when `outcome` changes
  - `jamovi/kappaSizeCI.r.yaml` - results
  - `R/kappaSizeCI.b.R` - backend; `R/utils-kappasize.R` - `kappaSizeGofCells()`, shared with
    kappaSizePower, kappaSizeFixedN and agreement
- **What it does**: the number of subjects an interobserver agreement study needs for the expected
  confidence limits around an anticipated kappa (kappa0) to reach the requested limits. It is a
  planning calculator: no dataset, no hypothesis test (`requiresData = FALSE`).

## 1a. Changelog

- **2026-10-16** (`/check-function-full`, fixes): the answer is stated before the Methodology in the
  Notes; new "Rare category" panel (a category below 5%); the Methodology says the n is for the
  goodness-of-fit interval, not Wald/Fisher-z/bootstrap; percentages are recognised; no number reaches
  the prose in scientific notation; the time-out handler no longer advises "more raters" when the
  design could not be judged; every msgid translated to Turkish.
- **2026-10-15**: closed-form n; restricted-ML re-estimation (IR-kappaSizeCI-01). See
  `development-ideas/kappaSizeCI-validation-2026-10-15.md` and
  `development-ideas/kappaSizeCI-independent-review-2026-10-15.md`.

## 2. Options (`.a.yaml`)

| Option | Type | Default | Effect |
| :--- | :--- | :--- | :--- |
| `outcome` | List 2-5 | `2` | number of categories; picks the kappaSize engine (CIBinary, CI3Cats-CI5Cats) |
| `citype` | List | `two_sided` | `one_sided` uses only kappaL, critical value `qchisq(1 - 2 alpha, 1)` |
| `kappa0` | Number | 0.60 | anticipated kappa (not a null value) |
| `kappaL` | Number | 0.40 | requested lower limit, strictly below kappa0 |
| `kappaU` | Number | 0.80 | requested upper limit, strictly above kappa0; ignored one-sided (passed as `NA`) |
| `props` | String | `0.20, 0.80` | category proportions; commas, semicolons or spaces; one value is enough for binary |
| `raters` | List 2-6 | `2` | raters per subject |
| `alpha` | Number | 0.05 | 0.001-0.20 |

## 3. Results (`.r.yaml`)

| Item | Type | Content |
| :--- | :--- | :--- |
| `notices` | Html | warnings (Sparse categories, Rare category, Small sample, very large n), then the answer, then Methodology |
| `text1` | Preformatted | the required n and a conditional sentence ("if the observed kappa comes in at kappa0") |
| `text_summary` | Preformatted | kappaSize's own `summary()`, plus a line explaining any difference from the reported n |
| `text2` | Preformatted | Study Explanation: design, the governing limit, and the arithmetic `ceiling(crit / slope)` |

Every option is in every item's `clearWith`.

## 4. How n is computed

1. `.validateInputs()` / `.validateProportions()` - kappa ordering, proportion parsing (decimal comma
   and percentage detection), renormalisation to sum exactly 1. Failures `jmvcore::reject()`.
2. `.governingSlopes()` - the expected goodness-of-fit chi-square is linear in n, so each limit has a
   slope. `fixed` holds the proportions at their anticipated values (kappaSize, Rotondi & Donner 2012);
   `rml` re-estimates them by maximum likelihood at each kappa (every design except binary with two
   raters). The governing slope is the smaller of the two at each limit.
3. `.minimumN()` = `ceiling(crit / min(slope))` is the reported n. Designs above 100,000 are refused
   with the n they would need.
4. kappaSize itself still runs, under a time budget, only to fill `text_summary`.
5. `.sparseVerdict()` applies Cochran's rule to the goodness-of-fit cells at each limit.

## 5. Change impact

- `kappaSizeGofCells()` is shared by four analyses; any change needs all four suites.
- Every reworded `.()` msgid loses its Turkish: run `jmvtools::i18nUpdate()` and refill `tr.po`.
  `test-kappasizeci-release-review.R` fails while any kappaSizeCI msgid is untranslated.
- Literal `" ["` inside a msgid is read as msgctxt; bracketed intervals are passed as values.
- Tests: `test-kappaSizeCI.R`, `test-kappaSizeCI-validation.R` (independent oracles, RD2012 tables),
  `test-kappasizeci-release-review.R` (regressions).
