# Adjacent draft repairs prompted by the thyroid FNA review

Implemented 4 October 2026, work package 6 of the
[implementation plan](satturwar-2026-thyroid-fna-implementation-plan.md).
These are bounded correctness repairs, not promotion or release certification.
The five analyses retain draft routing with a testing suffix (`DT`).

| Analysis | Implemented behavior | Reference check |
|---|---|---|
| `geemodel` | Gamma uses a log link. Cluster records are sorted and explicit visit waves preserve missing scheduled visits. AR1/unstructured require valid, unique within-cluster visits. QIC/QICu use `geepack::QIC`; an unsupported likelihood AIC is no longer fabricated. Coefficient Wald tests show one degree of freedom. | Shuffled Gamma/log AR1 coefficients and robust covariance equal a direct `geeglm` fit; QIC/QICu equal package output. |
| `mixedmodelanova` | An explicit crossed-intercept choice includes every selected grouping factor. Single-group choices reject extra groups. Nested construction includes all groups. Type III fixed-effect tests use sum contrasts and Satterthwaite degrees of freedom. Pairwise contrasts use exported `emmeans::contrast`, with selected-confidence CIs. ICC output distinguishes group-specific variance shares. | Crossed case/reader fit equals direct `lmerTest::lmer`; marginal contrasts and CIs equal `emmeans`; both grouping factors appear. |
| `ordinalmixedmodel` | Corrected model-data construction and safe random-effect formula handling. Unsupported CLMM proportional-odds tests and conditional predictions produce explicit availability notes and no numeric results. Odds ratios are available only for the logit link; other links retain coefficient-scale CIs. | The wine example agrees with direct `ordinal::clmm`; a generated analysis rerun from logit to probit clears odds ratios and updates CIs. |
| `clinicaltrialdesign` | Paired t planning uses standardized paired differences (`d_z`) and reports complete pairs. Sample-size rounding is followed by achieved-power calculation; recruitment inflation uses pair-level dropout. Generic allocation/interim/protocol assumptions are excluded from this bounded branch. McNemar planning is explicitly unavailable because the interface lacks discordant-pair probabilities. | Sample size, power and detectable effect agree with independent `stats::power.t.test(..., type="paired", strict=TRUE)` reference calculations. With `d_z=0.5`, two-sided alpha 0.05 and power 0.80, 34 complete pairs are required. |
| `diagnosticsamplesize` | Explicit disease, consensus and method-comparator modes change interpretation and labels. Comparator modes report PPA/NPA and reference-positive proportions. Precision statements use the selected confidence level and identify independent participants and plug-in width planning. | Comparator changes preserve numeric sample sizes; generated-wrapper reruns refresh titles and narrative. Invalid planning prevalences are rejected. |

Result definitions now declare option dependencies through `clearWith`, and dynamic
tables are cleared before reruns. CLMM and trial-planning tables begin with zero rows,
so unavailable outputs do not display an empty estimate row. No new dependency was added.

## API evidence

The [geepack manual](https://cran.r-project.org/web/packages/geepack/geepack.pdf)
documents contiguous clusters, the `waves` argument, working correlations and QIC.
The implementation also checked installed `genZcor`: an unstructured fit requires a
representable visit grid, so data with a wave index exceeding maximum cluster size are
rejected rather than passed to an incompatible correlation design.

The [lmerTest manual](https://cran.r-project.org/web/packages/lmerTest/lmerTest.pdf)
documents the fitted-model ANOVA interface and denominator degrees of freedom.
The [emmeans manual](https://cran.r-project.org/web/packages/emmeans/emmeans.pdf)
documents pairwise `contrast` and inferential summaries. The scope note identifies
the first selected fixed factor and its marginal reference-grid comparison.

The [ordinal manual](https://cran.r-project.org/web/packages/ordinal/ordinal.pdf)
documents `clmm` and limits the advertised nominal-effect test/prediction APIs to
other model classes. The installed package has no `predict.clmm` or supported
`nominal_test.clmm`; these former calls are not treated as completed diagnostics.
The [pwr manual](https://cran.r-project.org/web/packages/pwr/pwr.pdf) documents
`pwr.t.test(type="paired")` and its sample-size unit. The implementation was separately
checked against the noncentral-t calculation in base R.

## Validation

`tests/testthat/test-reader-study-adjacent-drafts.R` passed 62 expectations in seven
test blocks, with no failures, warnings or skips:

```r
testthat::test_file(
  "tests/testthat/test-reader-study-adjacent-drafts.R",
  reporter = "summary", stop_on_failure = TRUE
)
```

The suite uses both isolated backend method tests and actual generated jamovi
Options/Analysis/Results classes. It checks numerical package references, ordering,
duplicate visits, invalid wave values, multiple random groups, availability notes,
paired sample-size/power transitions, and comparator/scale reruns. All five R backends
and fifteen YAML files parsed. Source-only `git diff --check` passed.

Test environment: R 4.6.0, jmvcore 2.7.38, geepack 1.3.13, lmerTest 3.2.1,
ordinal 2026.7.26, pwr 1.3.0, emmeans 2.0.4. API manuals checked 4 October 2026.
Generated wrappers were refreshed through tooling; global documentation and final
regeneration are coordinated with the encompassing change.

## Boundaries and follow-up

- These changes do not implement random-reader MRMC inference or reader-study power
  planning. Crossed continuous-outcome models are not a validated binary MRMC analysis.
  No model can distinguish modality from period when the design confounds them perfectly.
- Single-proportion precision planning retains its existing plug-in width engine. It
  does not provide width assurance, account for uncertain comparators, or account for
  repeated readers/cases. A reader-study planner needs separately validated methods.
- McNemar planning needs two discordant-pair probabilities and a specified test/power
  method before implementation. Unsupported CLMM diagnostics and predictions similarly
  need a supported, validated replacement before being offered as results.
- Other pre-existing `clinicaltrialdesign` branches, interim adjustments and its
  generic plotting/protocol machinery were not certified by this paired-only repair.
  GEE and mixed-model saved-plot lifecycle also needs its own release review.
- `RProtoBuf` is unavailable in the local environment, so protobuf serialization,
  saved-analysis reopening, and desktop resize/export were not verified. In installed
  jmvcore, `Table$deleteRows()` clears row count/data but leaves stale row-name metadata;
  lifecycle tests check `rowCount` for emptied previously populated tables because
  `asDF` itself errors on that state. No private jmvcore metadata was patched.

