# Build the survival event indicator from a raw outcome column

Single source of truth for turning a user's outcome variable into the
0/1 (or 0/1/2 competing-risk) status vector consumed by
[`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html).
Replaces five near-identical `.definemyoutcome()` blocks that had
drifted apart in their validation.

## Usage

``` r
.defineEventIndicator(
  outcome,
  outcomeLevel = NULL,
  multievent = FALSE,
  analysistype = "overall",
  dod = NULL,
  dooc = NULL,
  awd = NULL,
  awod = NULL,
  outcome_name = "outcome"
)
```

## Arguments

- outcome:

  The raw outcome vector (factor, character, numeric or logical).

- outcomeLevel:

  The level representing the event, for the single-event path. Honoured
  for numeric outcomes too, not only factors.

- multievent:

  Logical. Use the four-category mapping.

- analysistype:

  One of `"overall"`, `"cause"`, `"dfs"`, `"compete"`.

- dod, dooc, awd, awod:

  Level labels for Dead of Disease, Dead of Other Causes, Alive with
  Disease, Alive without Disease.

- outcome_name:

  Display name of the outcome variable, used in messages.

## Value

A list with `status` (0/1 or 0/1/2, `NA` preserved), `status_factor`
(Censored/Event/Competing, only when competing), the counts `n_event`,
`n_censored`, `n_competing`, `n_missing`, the labels `event_label` and
`censored_labels`, `competing_labels`, `n_levels`, `has_competing`,
`estimand`, and `error` (`NULL`, or a ready-to-display message the
caller should reject with).

## Details

Semantics, stated explicitly because they are easy to get wrong:

- On the single-event-level path every level that is *not* the selected
  event level becomes 0 (right-censored). Labels alone cannot establish
  whether this is ordinary censoring or a competing terminal event. In
  the latter case Kaplan-Meier outputs describe net/cause-specific
  survival, not absolute event risk. The caller is expected to surface
  `estimand` and `censored_labels` to the user.

- `NA` is never converted to a value. It stays `NA` and the row is
  dropped downstream by
  [`jmvcore::naOmit()`](https://rdrr.io/pkg/jmvcore/man/naOmit.html)
  (complete-case analysis). Coding it 0 would fabricate censoring.

- In `multievent` mode every observed level must be assigned to one of
  the four categories. An unmapped level is an error, never a silent
  `NA` that later gets deleted along with the patient.
