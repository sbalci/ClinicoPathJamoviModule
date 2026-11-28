# Delayed-Entry Cox Regression

Open **Survival → ClinicoPath Survival → Delayed-Entry Cox Regression**.
Use one row per independent participant and a single event endpoint.

- **Entry Time:** time the participant first becomes observable, measured from
  a shared origin (such as diagnosis for a study that enrolls after diagnosis).
- **Event or Censoring Time:** time of the event or last observation measured
  from the same origin, in the same units. Do not use duration since enrollment
  as exit time. For example, enrollment 4 months after diagnosis and censoring
  after 12 additional months means entry **4**, exit **16**, event **0**.
- **Event:** 0 for right censoring, 1 for the event.
- **Baseline Covariates:** numeric or categorical predictors measured at or
  before entry. Categorical effects compare each observed level with the first
  observed level in the column's level order.

Someone who enters at month 4 is excluded from the risk set for an event at
month 4. An event or censoring at the exit time is included in the risk set
for that time. This follows the [survival package's counting-process convention
`(entry, exit]`](https://stat.ethz.ch/R-manual/R-devel/library/survival/html/Surv.html).
For [Tonje's attained-age question](https://github.com/sbalci/ClinicoPathJamoviModule/issues/122#issuecomment-7194661782),
use age at study entry and age at event or censoring (both in years) instead
of time since diagnosis. This general interface is separate from the
age-specific helper in [PR #20](https://github.com/sbalci/jsurvival/pull/20).

Outputs are the analyzed sample counts and hazard ratios with Wald 95%
confidence intervals and p-values ([Cox model and Efron ties](https://stat.ethz.ch/R-manual/R-devel/library/survival/html/coxph.html)).
The analysis rejects missing or invalid selected values rather than silently
dropping rows. It assumes that truncation and censoring are independent of
the event time as required by the model, conditional on modeled covariates
within the eligible risk sets, and that hazard ratios are proportional.
It does not verify those assumptions. Repeated intervals, competing risks,
landmarking, adjusted curves and absolute risk are not offered.

For desktop acceptance before merging, install the candidate module, open the
new menu item, and use the synthetic fixture in
`tests/testthat/test-delayed-entry-cox.R`. Check that the two estimates,
confidence limits and sample counts match R, then set an exit equal to entry
and check that the error is shown instead of a model. Confirm the preexisting
Multivariable Survival Analysis still accepts its original inputs unchanged.
