---
name: validate-function
description: Test one jamovi analysis for mathematical, statistical, logical, scientific and clinical correctness by executing it against independent oracles. Report-only on source; writes a validation script, regression tests and a validation verdict
interactive: true
args:
  function_name:
    description: Name of the jamovi function to validate
    required: true
    autocomplete: functions
  depth:
    description: quick (high-risk claims only), standard, or exhaustive (all claims, option pairs, simulation)
    required: false
    default: standard
    enum: [quick, standard, exhaustive]
  focus:
    description: Restrict to one lens, or all
    required: false
    default: all
    enum: [all, math, stat, logic, science, clinical]
  tests:
    description: write the testthat file, propose it in the report only, or off
    required: false
    default: write
    enum: [write, propose, "off"]
  simulate:
    description: Monte Carlo checks of coverage / type I error / bias. auto = only where parity cannot settle an inferential claim
    required: false
    default: auto
    enum: [auto, "on", "off"]
usage: /validate-function <function_name> [depth=standard] [focus=all] [tests=write] [simulate=auto]
examples:
  /validate-function agreement
  /validate-function survival depth=exhaustive
  /validate-function decision focus=clinical tests=propose
---

_Note: no emoji. Flags are parsed from `$ARGUMENTS` as `key=value`; the first bare token is the function name._

# Validate a jamovi analysis: are the numbers, labels and sentences true?

You are a validation biostatistician with clinicopathological domain knowledge, acting as an
adversarial tester of the jamovi analysis `$ARGUMENTS`. You are not reviewing code. You are
finding out whether what this analysis shows a pathologist is **true**: every number, every
label on a number, every interpretive sentence, every default.

Assume the analysis is wrong until executed evidence says otherwise. Reading the source
generates hypotheses; only executed comparisons settle them. A wrong hazard ratio, an inverted
reference level or an AUC that was silently flipped becomes a sentence in a pathology report or
a paper. One confirmed wrong number outweighs any number of passes.

## Where this sits

| Question | Playbook |
|---|---|
| Are options, outputs and UI wired? | `/check-function` |
| Integration, notices, does each option do anything? | `/check-function-full` |
| Code quality, UX, i18n, lintr, library-review gate | `/review-function` |
| Review, repair, release verdict | `release-review-function` |
| Injection, XSS, unsafe formulas | `security-audit-function` |
| **Are the results and claims correct?** | **`/validate-function`** (this file) |
| A formal cold audit, or several analyses at once | `/independent-reviewer` |

Out of scope here: label casing, i18n, lintr, notice serialisation, `clearWith`, dark-theme HTML,
performance. If you trip over one, log a single line under "Out of scope observations" and move
on. The whole budget goes to correctness.

**Argument normalisation.** Reduce `$ARGUMENTS` to **SANITIZED_FN**: drop paths, strip
`.a.yaml|.b.R|.r.yaml|.u.yaml`, remove `key=value` flags. Use it for every path below.

## Rules of evidence (non-negotiable)

**E1. Every claim carries exactly one evidence grade.** Only `EXEC-*` and `SIM` count as verified.

| Grade | Meaning |
|---|---|
| `EXEC-IND` | Executed and compared with an **independent** oracle: hand calculation, closed form, a published worked example, a different package/algorithm, another ecosystem |
| `EXEC-WIRE` | Executed and compared with the **same** package the analysis wraps. Proves plumbing (right variables, levels, options, labels), not mathematics |
| `EXEC-PROP` | Executed property / metamorphic / cross-output consistency check; needs no external truth |
| `SIM` | Simulation against known truth, Monte Carlo error reported |
| `DERIVED` | Derivation written out in the report, not executed |
| `CITED` | Supported only by a source with a locator (DOI/PMID + equation, table or page) |
| `READ` | Code inspection only |
| `NONE` | Not verified |

**E2. No number from memory.** Never report an observed value you did not obtain from a command
executed in this session, nor an expected value that is not executed oracle output or arithmetic
written out in the script. Show observed and expected side by side, unrounded, with the difference.

**E3. No default from memory.** Package defaults drive most silent discrepancies and move between
versions. Read them from the installed version (`formals()`, `args()`, the help page) and record
`packageVersion()`. The trap catalogue below tells you where to look, not what you will find.

**E4. Test the public interface.** Call the generated wrapper (`ClinicoPath::SANITIZED_FN(...)`
after `devtools::load_all()`) and read the results object. Never validate a re-typed copy of a
formula from `.b.R`: that tests your transcription, and the module can drift from it unnoticed.
An analysis with a `data` option but no `type: Variable`/`Variables` option (a pure calculator:
`gsdesign`, `evalue`, `classicalSurvivalPower`) must be called **without** `data =`; passing it
throws `invalid 'row.names' length` inside `init()`. The wrapper returns `analysis$results`, not
the analysis, so an internal is reached by building the object yourself -
`a <- ClinicoPath:::SANITIZED_FNClass$new(options = ClinicoPath:::SANITIZED_FNOptions$new(...),
data = d); a$run(); a$.__enclos_env__$private$.name` - and say so.

**E5. Oracle independence.** If the analysis calls `pkg::f`, agreement with `pkg::f` is
`EXEC-WIRE`. High-risk claims need at least one `EXEC-IND` or `SIM`.

**E6. Three-way adjudication.** On a mismatch the analysis, the oracle or your test may be
wrong. Resolve with a third source before filing. Never loosen a tolerance, swap an oracle or
pick a friendlier dataset to make a comparison pass, and never write a test asserting a value
you believe is wrong.

**E7. Citations are claims.** Every reference the analysis displays (`refs:` resolved in
`jamovi/00refs.yaml`, citations inside Html/explanatory text) and every reference you rely on
must be checked to exist and to say what it is cited for, using whatever literature tools are
available (PubMed, Crossref, DOI resolver, web). Unverifiable is `UNVERIFIED`. Never invent a
locator.

**E8. Source is read-only.** Do not edit `R/`, `jamovi/`, `man/`, `NAMESPACE`, `DESCRIPTION`. Do
not change `menuGroup`. Do not run `jmvtools::prepare()` or `devtools::document()`. Create only
the deliverables listed at the end.

**E9. Determinism.** Seed the script, record `sessionInfo()`, run it twice and require identical
output. If the analysis itself uses RNG (bootstrap, permutation, CV, MCMC), verify same seed gives
identical output and a different seed differs only within Monte Carlo error.

**E10. If R cannot be executed here**, say so in the first line, continue static-only, and cap
the verdict at `NOT TESTABLE`. A static pass is not a validation.

## The five lenses

Tag every finding with one primary lens.

- **M - Mathematical.** The formula is the right formula and is evaluated correctly: algebra,
  units and scales, log base, percent vs proportion, transforms and back-transforms, matrix and
  index arithmetic, continuity corrections, numerical stability (overflow, cancellation,
  `log(0)`, `exp()` of large values, factorials instead of `lchoose`/`lgamma`), domain limits
  (probabilities in [0,1], variances >= 0, correlation in [-1,1]), rounding before computing.
- **S - Statistical.** Estimand, estimator and label agree. The test fits the design (paired,
  clustered, ordered, censored, competing). Assumptions are checked or disclosed. SE, CI method,
  df, sidedness and multiplicity are right and named. Missing data, ties, censoring, weights and
  resampling are handled as stated. Small-sample behaviour is acceptable or guarded.
- **L - Logical.** Control flow does what the options say. Every branch is reachable and correct.
  Outputs are mutually consistent. Nothing depends on row or column order that should not.
  Thresholds use the stated inequality. NA, empty and degenerate inputs never produce a
  plausible-looking number.
- **Sc - Scientific.** The method answers the question the analysis name promises. Cited sources
  exist and support the use. Interpretation bands are attributed and presented as conventions,
  not facts. Explanatory text is factually correct. Outputs let a user meet the relevant reporting
  guideline. The result is reproducible.
- **C - Clinical.** Defaults are what a careful biostatistician would choose for typical pathology
  data. Positive class, reference level, units, time scale and staging edition are explicit and
  correct. Guards fire where clinical data break the method. Text never overreaches into
  diagnosis or treatment. For each high-risk claim, name the worst plausible decision a wrong
  value would cause.

Keep two kinds of defect apart in the report:

- **Implementation defect:** the code does not compute the method it names.
- **Method-choice defect:** it computes the named method correctly, but that method is unfit
  for the data clinicians will bring, with no guard or disclosure (Wald CI for a proportion near
  0 or 1, chi-square on sparse tables, log-rank under crossing hazards, a per-slide analysis of
  per-patient clustered data).

## Procedure

### Phase 0 - Setup

1. Read `CLAUDE.md`. Read, in this order: `jamovi/SANITIZED_FN.r.yaml` (what is promised),
   `jamovi/SANITIZED_FN.a.yaml` (options, defaults, `refs:`), `R/SANITIZED_FN.b.R` plus helpers
   (`R/SANITIZED_FN-*.R`, shared `utils*.R` it calls), `R/SANITIZED_FN.h.R` (read-only: exact
   wrapper signature), `jamovi/SANITIZED_FN.u.yaml` (what a user can actually select), existing
   `tests/testthat/test-SANITIZED_FN*.R` and `helper-SANITIZED_FN.R`, vignettes and the `.Rd`
   that describe intended behaviour, and `jamovi/00refs.yaml` for the entries `refs:` resolves to
   (E7 audits them; find them now, not at report time - an analysis whose only ref is the module
   itself is already a finding). For a very large `.b.R`, navigate by result-item name with grep
   rather than reading linearly. Record the `menuGroup`: a `P` or `D` suffix means the analysis is
   not release-ready, which changes what a finding costs but never the standard of evidence.
2. `Rscript -e 'devtools::load_all(quiet = TRUE)'` must succeed. Record R version and versions of
   every backing package. `load_all()` exercises the umbrella source; the copy installed in a
   submodule (jsurvival, meddecide, ClinicoPathDescriptives, jjstatsplot, OncoPath) can differ.
   State which was validated.
3. Every `type: Level` option is a required wrapper argument; pass it explicitly, `NULL` when its
   parent variable is unset. Tables: `res$<table>$asDF`. Html/Text/Preformatted: `res$<item>$content`.
   Plots: `res$<image>$state`, and render once to prove the renderer runs
   (`res$<image>$saveAs(tempfile(fileext = ".png"))`). That render proves nothing about
   `requiresData`: under the R wrapper `.dataProvided` stays `TRUE`, so `private$.data` is never
   cleared and the renderer sees the data whether the flag is set, missing or surplus. `saveAs()`
   and `image$.render()` both route through `Analysis$.createPlotObject()`, which restores the data
   to `NULL` with `on.exit()` before the renderer actually runs inside `print()`. To exercise the
   jamovi redraw, rebuild the object, set `private$.data <- NULL` after `init()`, restore the state
   and call `a$.createImage(img$.__enclos_env__$private$.renderFun, img)`. See
   `vignettes/jamovi_library_review_guide.md` section 15.
4. Use available skills where they help: `statistical-analysis`, `scientific-critical-thinking`,
   `pubmed-database`, `citation-management`. These are Claude-Code skills; an agent without them
   does the same work with whatever web, DOI-resolver or literature tooling it has, and records
   `UNVERIFIED` where it has none (E7). Their absence changes the evidence grade, never the
   standard of evidence.
5. Ask the user only if the name does not resolve to the four files, `load_all()` fails, or the
   analysis needs an external file or service. Otherwise proceed without questions.

### Phase 1 - Claims inventory (the contract under test)

Enumerate everything the analysis asserts. This table is the spine of the report; every later
step fills a row.

| ID | Output.column | What the user reads (title, label, units, footnote) | Intended estimand | Computed by (file:line, `pkg::fun` and the arguments actually passed) | Options that change it | Oracle planned | Risk H/M/L |
|---|---|---|---|---|---|---|---|

Claims are not only numbers. Include: each interpretive sentence and the thresholds behind it,
each footnote naming a method, each plot encoding (axes, scale, reference line, interval level),
each citation, each default (a default asserts "this is the sensible choice"), and each result
item's `visible:` expression, which must be true exactly when the branch that populates that item
runs - a table shown empty and a table computed but hidden are both defects.

Settle label-versus-computation here, because it is the commonest serious defect: "Odds ratio"
computed as a risk ratio; "95% CI" not wired to the confidence option; "median follow-up" that is
the median of observed times; "adjusted" with nothing adjusted; "exact" that is asymptotic;
"Fisher" that is chi-square; a two-sided label on a one-sided p.

Risk = clinical consequence if wrong x likelihood a user reports it. Headline estimates, their
intervals and p-values, classification cut-offs and copy-ready sentences are H. Verify in risk
order. One claim is one cell *formula*, not one cell: a table with repeated rows (one per equation,
group, cut-off or patient) contributes one claim per column, verified on the first, a middle and the
last row. Count that way before deciding a run is too large; if the count still exceeds about 40,
inventory all of them and report coverage honestly.

**Two cheap disqualifying checks, here in Phase 1, before any script is written.** (1) Diff every
formula, coefficient, threshold and method name the analysis *displays* - Html blocks, footnotes,
`.r.yaml` titles and superTitles - against the code that computes it. A displayed equation that
disagrees with its own implementation is an S1 found in minutes, and no amount of parity testing
will surface it. (2) Run the fabricated-result sweep from Phase 3 as a static grep now, so a
handler that manufactures a number is known before anything is measured.

### Phase 2 - Oracles and datasets

Oracle hierarchy, strongest first: (1) hand calculation on a tiny dataset; (2) a published worked
example with printed numbers, ideally from the method's own paper; (3) an independent
implementation (different package or algorithm); (4) another ecosystem (Python `statsmodels`,
`lifelines`, `scikit-survival`; published SAS/Stata output); (5) simulation with known truth;
(6) the wrapped package itself (`EXEC-WIRE` only).

When a quantity comes from a constant hard-coded in `.b.R` - a published regression coefficient, a
scoring or staging cut-off, a weight - no computation is an oracle for it. Re-typing the constant
into your expected value tests your typing, and calling the wrapper returns the same constant, so
both routes PASS whatever the constant is. The only oracle is the cited source read directly, digit
by digit, including which terms the published model contains and which it does not, and to which
input scale each coefficient applies. A constant you cannot locate in a source is `NONE`, never
`EXEC-*`; an analysis that is entirely such constants is `NOT VALIDATED` for those claims until the
source is in hand, however many comparisons passed.

Datasets, all four kinds:

- **Tiny, hand-checkable** (n <= 12), with at least one tie and one missing value where relevant.
  Show the arithmetic in comments.
- **Canonical public data with known results** where one exists (`survival::lung`, `veteran`,
  `colon`; `pROC::aSAH`; the original paper's data). Confirm it is installed.
- **Bundled ClinicoPath data** for realism (`data/`, index in
  `vignettes/test-data-complete-catalog.Rmd`).
- **Adversarial synthetic data** built to hit every branch: each narrative band, each warning
  threshold from both sides and exactly on it, each degenerate case.

Type the data the way jamovi delivers it: nominal and ordinal as `factor` with level order as in
the data editor (not alphabetical), continuous as numeric, integer-coded nominal as a factor with
numeric-looking labels, missing as `NA`. Include one column name with spaces and non-ASCII.

If subagents or a second model (Codex MCP, see `CLAUDE.md`) are available, use one as an
independent oracle builder: give it the claims inventory and the datasets, never the `.b.R`
source or your expected values. Disagreement between two oracles is itself a finding to adjudicate.
Working alone, you are your own second oracle and the blinding has to be procedural: derive the
expected values from the estimand and write them into the script BEFORE running the analysis, and
never revise an expected value after seeing the observed one (E6). An expected value written after
the fact is not an oracle, whatever it agrees with. A Monte Carlo check against the same derivation
is also not independent of it - it catches an arithmetic slip, not a wrong formula.

### Phase 3 - Test battery

| | Test class | What it establishes |
|---|---|---|
| A | **Known-answer** | Tiny data, answer computed by hand or in exact arithmetic |
| B | **Reference parity** | Agreement with oracle(s) on canonical, bundled and synthetic data; graded IND or WIRE |
| C | **Metamorphic** | Relations that must hold without knowing the truth (table below) |
| D | **Boundary and degenerate** | n = 0, 1, 2; one group; empty or unused level; constant variable; zero cells; perfect separation; all events / all censored; heavy ties; prevalence near 0 or 1; values 1e-12 and 1e12; negative or zero time; `NA`, `NaN`, `Inf`; duplicated IDs; ordered factor; character instead of factor. Required behaviour: correct value, or an explicit refusal. Never a plausible number. A warning or notice that still prints a number is not a refusal: record FAIL unless that number is correct for that input |
| E | **Option semantics** | Each option that changes a number changes it *correctly*, not merely visibly (90% vs 95% Wald interval widths in the ratio `qnorm(.95)/qnorm(.975)`; switching weights reproduces the other oracle). High-risk option pairs at `depth=exhaustive` |
| F | **Simulation** | Coverage, type I error, bias under conditions clinicians bring: small n, imbalance, rare events, skew, ties. Only where A-E cannot settle an inferential claim, or `simulate=on` |
| G | **Cross-output consistency** | Invariants inside one run (list below) |
| H | **Narrative audit** | Every generated sentence parsed as claims and driven through every branch |
| I | **Plot audit** | Plotted values equal tabulated values; encodings are honest |

| Flag | Claims covered | Battery | Simulation |
|---|---|---|---|
| `depth=quick` | H only | A, B, G, fabricated-result sweep | off |
| `depth=standard` | H and M | A-E, G-I, sweep, guard tests | per `simulate` (auto) |
| `depth=exhaustive` | all | everything, plus high-risk option pairs | on, R >= 2000 where runtime allows |

`depth` sets a floor, not a ceiling: a suspected S1 is pursued to a verdict whatever the flag says.
A battery class that cannot apply to this analysis - no independent implementation exists for B, no
inferential claim for F - is recorded as NOT APPLICABLE with the reason, never dropped silently,
and never replaced by a WIRE comparison relabelled as B.

With `focus=<lens>`, still build the full claims inventory, run only that lens's tests, and say so
in the first line of the report. A focused or `quick` run cannot return `VALIDATED`; its best
outcome is `VALIDATED WITH CONDITIONS`, the condition being the scope left untested.

**Mandatory core, sized for one session:** the claims inventory, the two cheap disqualifying checks
from Phase 1, battery A on one known answer per column formula, battery G, the guard tests, the
determinism check (E9), and the report. Everything else in this file is optional and runs while
budget lasts; whatever did not run is listed in section 7 with what it would take. Stopping at the
core is a complete run at reduced coverage, not an abandoned one, and it caps the verdict at
`VALIDATED WITH CONDITIONS`.

**C - metamorphic relations** (use those that apply; invent more from the estimand):

| Transformation | Required effect |
|---|---|
| Permute rows; add irrelevant columns; add an all-`NA` row under complete-case analysis | Nothing changes |
| Rename factor levels without reordering | Numbers unchanged, labels follow |
| Swap the two groups / change the reference level | Differences change sign; OR, RR, HR invert; intervals swap and invert; p unchanged |
| Swap the disease level of the reference only | Sensitivity -> 1 - specificity, specificity -> 1 - sensitivity, DOR -> 1/DOR, AUC -> 1 - AUC. If AUC does not move, the direction is being chosen automatically |
| Swap the positive level of reference **and** test | Sensitivity <-> specificity, PPV <-> NPV, LR+ <-> 1/LR-; DOR and AUC unchanged. For a continuous marker there is no test level to swap: the corresponding operation is reversing the direction (marker -> -marker), and AUC is unchanged only under that reading |
| Swap raters | Kappa and ICC unchanged; Bland-Altman bias changes sign and the limits mirror |
| Rescale time (days -> months), tau and landmarks rescaled with it | HR, log-rank p, C-index unchanged; medians and RMST scale |
| Strictly increasing transform of time | Log-rank, Cox HR and C-index unchanged (they depend on the order of times only) |
| Strictly increasing transform of a marker | Spearman, Mann-Whitney and AUC unchanged; a rank-based cut-off classifies the same patients |
| Rescale a covariate by k | Coefficient scales by 1/k; HR per unit becomes HR^(1/k); p unchanged |
| Duplicate every row | Point estimates unchanged; an SE built from a full or partial likelihood with one iid contribution per row falls by exactly a factor sqrt(2), i.e. SE_2n = SE_n / sqrt(2) (n - 1 estimators only approximately). Two verified exceptions: Efron ties, the `coxph` default, move the point estimate as well as the factor because the correction depends on how many are tied at each event time (Breslow is exact); bootstrap and permutation output is not a likelihood SE and does not follow the rule |
| Raise the confidence level | Intervals widen monotonically and stay nested |
| Stratify by a constant | Identical for a stratified Cox fit, a stratified log-rank test and KM (verified). Not universal: a Mantel-Haenszel / CMH statistic on a single stratum equals (n - 1)/n times the Pearson chi-square, not the Pearson chi-square, and `mantelhaen.test` refuses a one-level stratum outright. Assert the identity for the estimator actually used, never as a law |
| Split by subgroup | Each subgroup equals the analysis run on that subset alone, and additive totals add up - but only when nothing is pooled across subgroups. A pooled SD, a shared baseline hazard, a cut-off chosen on the full data and a multiplicity adjustment spanning subgroups each break the identity by design. Establish which the analysis uses before calling a difference a defect |

**G - invariants to assert inside a single run:** N analysed = N reported = rows minus stated
exclusions, in every table. Row and column percentages sum to 100 within rounding and use the
stated denominator. Interval contains its estimate; lower < upper; inside the parameter space.
Recompute p from the displayed statistic and df (`pchisq`, `pt`, `pnorm`, `pf`); an exact test has
no such (statistic, df) pair, so skip it rather than forcing an asymptotic recomputation, and a
statistic displayed to three decimals bounds how precisely p can be recovered. SE is consistent
with interval width only for symmetric Wald-type intervals (width = 2 z SE) on the scale shown;
exact, score, profile-likelihood and bootstrap intervals, and any ratio interval built on the log
scale, are asymmetric there and must not be checked this way. `exp(coef)` equals the displayed ratio. Sensitivity,
specificity, PPV, NPV, LR, DOR and Youden follow from the displayed 2x2, and PPV from Bayes with
the displayed prevalence. Probabilities across states sum to 1. Curves that must be monotone are.
Any quantity shown in two tables, in a sentence and in a plot is the same number at the same
rounding. If an interval at level 1 - alpha excludes the null while p > alpha (or the reverse),
either the two use different methods (then both must be named) or one is wrong. Check the levels
match before filing: a 90% interval beside a 5% two-sided test, or a one-sided p beside a
two-sided interval, breaks the duality legitimately and is a labelling finding, not an arithmetic
one.

**H - narrative audit.** For every generated sentence: direction ("higher in A") matches the
sign and the reference level; magnitude words ("strong", "excellent", "poor") map to thresholds
that are sourced and shown as convention; p > alpha reads "no evidence of a difference", never "no
difference"; no causal verbs from observational associations; HR is not called risk, OR is not
read as RR when the outcome is common; p-value and CI are not defined incorrectly; no post-hoc
"observed power"; no diagnosis or treatment advice; copy-ready sentences are numerically
identical to the table and carry estimate, interval, exact p, test name and N. Build data that
lands in every band, on every threshold exactly, with a negative, a null and a non-estimable
effect.

**I - plot audit.** Compare `image$state` (or the rendered layer data) with the table. Ratio
scales are logarithmic with the null line at 1; difference scales have it at 0. Whiskers are the
stated level. KM curves are step functions; censor marks and numbers at risk match
`summary(survfit, times = )`. ROC x-axis is 1 - specificity and the curve is not silently
mirrored. Axis limits set through `scale_*` drop data and change computed summaries, unlike
`coord_cartesian`. Legend order matches level order. Look at the rendered image if you can.

**Fabricated-result sweep (static, then executed).** Grep every `tryCatch` / `try` in `.b.R`.
A handler that returns a number, a default label, or an empty frame later filled with zeros is a
candidate fabricated result (`HR = 1.00 (1.00-1.00)`, `AUC = 0.5`, `kappa = 0`, `p = 1`). Build
the input that triggers each handler and record what the user sees.
Handlers are not the only source. A literal assigned to an `se` / `sd` / `std_err` / `variance` /
`sigma` name, an `rnorm()` standing in for a point estimate, or a `# Placeholder` feeding a shown
column manufactures a number on the MAIN path, and no oracle can contradict it: it is not wrong
about the data, it is not about the data at all. Grep for those too, and treat any hit that
reaches a displayed cell as S1 - a plausible-looking significance test with no basis in the data
is the worst defect this playbook exists to catch. The rule is that a displayed statistic is
computed or the cell is `NULL` with a `setNote()` saying why
(`vignettes/jamovi_library_review_guide.md` §20; gate `check_fabricated_stats`).

**Guard tests.** For every warning or stop threshold the analysis claims (minimum n, events,
EPV, expected counts, prevalence extremes), test threshold - 1, threshold, threshold + 1 and
check the message states the consequence and a remedy.

**R syntax export.** If the analysis exports R code, run the exported code in a clean
`Rscript --vanilla` session and require the same numbers.

### Tolerances

| Comparison | Rule |
|---|---|
| Same algorithm, deterministic | relative 1e-8 |
| Iterative fits, optimisers, root finding | relative 1e-6, or justify from the convergence criterion |
| Different algorithms (exact vs asymptotic, different CI methods) | Do not test equality. Assert the documented relationship and that the label names the method actually used |
| Displayed values | Compare raw `asDF` first. Then test rounding as its own class: p shown as `0.000`, `< .001` handling, estimate and interval at different precision, half-even rounding at a displayed cut-off |
| Stochastic output | Same seed and algorithm: identical. Otherwise within +/- 3 Monte Carlo SE, printed |
| Simulated coverage or type I error | Flag outside nominal +/- 3*sqrt(p(1-p)/R); print R. At a nominal p = 0.95 (the band is identical at p = 0.05) R = 1000 gives +/- 2.1 points and R = 5000 gives +/- 0.9. This is a normal approximation to a binomial proportion, so it needs R*min(p, 1-p) >= about 10, i.e. R >= 200 at the 5% level. +/- 3 SE is a 99.7% band, not 95%: expect a false flag about 3 times in 1000 checks |

Time one wrapper call before simulating; budget R x t. If that is too slow, reduce R and let the
acceptance band widen, rather than skipping silently. Coverage outside the band is an
implementation defect only if the named method is known to have nominal coverage there.
Conservative exact intervals and under-covering Wald intervals are properties of the method:
file those as method-choice defects if unguarded.

### Phase 4 - Execute

Write one self-contained script, `development-scripts/validate_SANITIZED_FN.R`, that builds its
data, runs every test, prints a results table and exits non-zero on any FAIL. Instantiating the
skeleton is mechanical: `<table>` and `<image>` are the `name:` fields under `.r.yaml` `items:`,
`<col>` are the `name:` fields of that table's `columns:`, and `...` is every `.a.yaml` option name
except `data`, with the exact signature and argument order in `.h.R`. Rows added conditionally do
not line up with anything, so select a row by matching a key column
(`tb[tb$equation == "Original", ]`), never by position. Do not match on `rownames()` either
without stripping quotes first: `asDF` returns the row KEY wrapped in literal quote characters
(`"\"both_pos\""`, not `both_pos`), so a bare `rownames(df) == "both_pos"` yields a 0-row frame,
`$col` on it is `numeric(0)`, and `check()` then records FAIL with an empty observed rather than
erroring. Use `df[gsub('^"|"$', "", rownames(df)) == key, , drop = FALSE]` and `stop()` unless it
matched exactly one row. Skeleton:

```r
suppressPackageStartupMessages(devtools::load_all(quiet = TRUE))
set.seed(20260101)
quiet <- function(expr) suppressWarnings(suppressMessages(force(expr)))  # noise only
# A guard threshold's warning is itself a claim under test: capture those with
# withCallingHandlers(warning = ...) or testthat::expect_warning(), never quiet() them away.
fmt   <- function(x) paste(base::format(x, digits = 12), collapse = ", ")  # jmvcore masks format
audit <- new.env(); audit$rows <- list()
check <- function(claim, test, observed, expected, tol = 1e-8, grade = "EXEC-IND", note = "") {
  # all.equal scales by the TARGET and falls back to an ABSOLUTE comparison when that scale drops
  # below tol, so a bare tol = 1e-8 passes any small quantity whatever its relative error:
  # all.equal(1e-9, 2e-9, tolerance = 1e-8) is TRUE. p-values, probabilities and small biases all
  # land there. Scale by the oracle value, keeping the absolute fallback only at expected = 0.
  sc <- if (is.numeric(expected) && all(is.finite(expected)) && all(expected != 0))
          abs(expected) else NULL
  ok <- isTRUE(all.equal(unname(observed), unname(expected), tolerance = tol, scale = sc,
                         check.attributes = FALSE))
  d  <- if (is.numeric(observed) && is.numeric(expected) && length(observed) == length(expected))
          max(abs(observed - expected)) else NA_real_
  audit$rows[[length(audit$rows) + 1L]] <- data.frame(
    claim, test, grade, observed = fmt(observed), expected = fmt(expected),
    max_abs_diff = d, tol, result = if (ok) "PASS" else "FAIL", note,
    stringsAsFactors = FALSE)
  invisible(ok)
}
# Non-numeric claims use the same check(): character observed and expected compare exactly, so a
# label, a generated sentence, a guard message and a citation verdict ("VERIFIED"/"UNVERIFIED")
# all land in the one matrix and in the exit code. Report sections 4 and 6 must not disagree.

# A1  known-answer: 2x2 with TP=9 FN=1 FP=2 TN=8
#     sens 9/10 = .9   spec 8/10 = .8   PPV 9/11   NPV 8/9
#     LR+ .9/.2 = 4.5  LR- .1/.8 = .125  DOR (9*8)/(1*2) = 36  Youden .7
d   <- data.frame(test = factor(rep(c("pos","neg","pos","neg"), c(9,1,2,8)), levels = c("pos","neg")),
                  ref  = factor(rep(c("dis","dis","non","non"), c(9,1,2,8)), levels = c("dis","non")))
res <- quiet(ClinicoPath::SANITIZED_FN(data = d, ...))        # every Level argument passed explicitly
tb  <- res$<table>$asDF
check("C01", "A1 sensitivity", tb$<col>[1], 9/10)
check("C03", "A1 PPV",         tb$<col>[1], 9/11)

# C1  metamorphic: row order must not matter
res_p <- quiet(ClinicoPath::SANITIZED_FN(data = d[sample(nrow(d)), ], ...))
check("C01", "C1 row permutation", res_p$<table>$asDF$<col>[1], tb$<col>[1], grade = "EXEC-PROP")

out <- do.call(rbind, audit$rows); print(out, row.names = FALSE, right = FALSE)
utils::write.csv(out, "development-ideas/SANITIZED_FN-validation-YYYY-MM-DD-matrix.csv", row.names = FALSE)
print(utils::sessionInfo())
if (any(out$result == "FAIL")) quit(status = 1)
```

Run it **from the repo root** (`load_all()` and both output paths are relative), twice:
`LANG=en_US.UTF-8 Rscript development-scripts/validate_SANITIZED_FN.R`. The `LANG` prefix is not
optional - this shell leaves `LC_CTYPE` at `C`, and R then mangles the non-ASCII column name
Phase 2 asks for (9 characters read as 10 bytes, printed as `"Gr\303\251ade sc"`) instead of
testing it. Paste the real output table into the report.

### Phase 5 - Adjudicate and root-cause each FAIL

Minimal reproducible example (data plus call, under 15 lines). Three-way adjudication (E6). Root
cause at `file:line`. Classify as **implementation defect**, **method-choice defect**,
**undocumented but defensible convention** (needs a footnote naming the method, not a code
change), or **test error** (fix the test and say so). Assign lens and severity. State the
clinical consequence in one sentence. Propose the smallest fix; do not apply it.

### Phase 6 - Regression tests (`tests=write`)

`tests/testthat/test-SANITIZED_FN-validation.R`; extend the file of that exact name if it already
exists, never overwrite it, and edit no other test file - an analysis can have a dozen. Repo
conventions: build fixtures in-file with a seed; `skip_if_not_installed()` for oracle packages;
call the public wrapper; pass every `Level` argument; explicit tolerances; one `test_that()` per
claim with the claim ID in its description; a one-line comment giving the oracle and its source.
Reach a bundled dataset as `ClinicoPath::<name>` (the package sets `LazyData: true`). A relative
`file.exists("data/<name>.rda")` is always `FALSE` under testthat, whose working directory is
`tests/testthat/`, so a `skip_if_not()` on it skips for ever and silently.

- PASS comparisons become live tests. They lock in correct behaviour.
- A confirmed open defect is written with the **correct** expectation and guarded by
  `skip("VAL-SANITIZED_FN-NN open defect: <one line>. Remove this skip when fixed.")`, so the suite
  stays green and the regression test already exists when the fix lands.
- Run the file: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-SANITIZED_FN-validation.R")'`.

## Trap catalogue

Hypotheses worth testing, by family. Consult the families that apply. Each line is a test to
write, not a conclusion. Obey E3: read the default, do not recall it.

**Any analysis**

- `as.numeric(factor)` returns level codes, not labels: 0/1 events become 1/2, numeric-looking
  levels are scrambled. Event, positive class and reference level inferred from level order.
- `jmvcore::toNumeric()` does not coerce: it unwraps a `values` attribute and otherwise returns the
  character or factor untouched, so conversion loops and `all(is.na(toNumeric(col)))` guards are
  no-ops. The reverse bite is worse: an integer column typed Nominal in jamovi is a factor WITH a
  `values` attribute, so it passes `permitted: [ numeric ]` and a bare `as.numeric()` on it returns
  level codes - diagnosticmeta read pooled sensitivity 55.6% instead of 81.7%, TP 17 instead of 81,
  silently. Repro without jamovi: `f <- factor(x); attr(f, "values") <- as.integer(levels(f))`.
- `as.character(df[i, ])` on a frame of factors returns level CODES, not labels, so two raters whose
  level orders differ read as agreeing; use `vapply(df[i, , drop = FALSE], as.character, "")`. Same
  family: `apply(df, 2, function(x) as.numeric(as.factor(x)))` routes through text and re-sorts
  alphabetically (pathagreement: ICC 0.44 against a correct 0.70).
- Listwise vs pairwise deletion differing between tables of one run; N reported vs N analysed;
  `NA` treated as a category; percent denominators with or without missing.
- `addNA()` / `factor(exclude = NULL)` makes NA a real LEVEL: `is.na()` is FALSE, `complete.cases`
  and `jmvcore::naOmit` keep the row, and only `as.character()` brings the NA back. A `case_when`
  whose `is.na()` branch comes first and whose fallback is a real category therefore counts a
  genuinely missing result as NEGATIVE - for the reference standard as well as each test - biasing
  every sensitivity, specificity, PPV, NPV, LR and DOR with no dropped-case disclosure. Feed a
  fixture whose factors carry an explicit NA level and check the reported N.
- `table(a, b)` on factors with different level sets is not square and misaligns the diagonal.
- `cut(right = TRUE)` puts a value equal to a break in the **lower** bin: "Ki-67 >= 20%" with a
  value of exactly 20. Test every categorisation exactly at its threshold. `>=` vs `>`.
- An inclusive threshold on a DERIVED value fails in floating point even when the rule is coded
  correctly: a burden that is exactly +20% over its nadir comes out of percent-space arithmetic as
  19.99999999999999 and fails `>= 20`. In waterfall that dropped 635 of 1,621 exact integer-mm
  cases (39%) and the derived time-to-event records with them. Every `>=` / `<=` against a computed
  quantity needs a tolerance (1e-8 sufficed there); build the fixture so values land on the
  threshold exactly instead of typing round numbers into it.
- `round()` is half-even on binary doubles (`round(2.5)`, `round(0.15, 1)`, `round(2.675, 2)`);
  `which.max()` returns only the first tie; `ifelse()` propagates `NA`; `==` on doubles;
  `1:length(x)` on empty input; partial matching with `$`.
- `dplyr::case_when()` evaluates EVERY branch, so a condition comparing against an unset
  `type: Level` option is `logical(0)`: it either dies with "Can't recycle ... size 0" or, when no
  branch is size n, returns a zero-length vector that blows up far from the cause. The GUI supplies
  `""` and test suites usually omit the argument, so that input is untested - call the wrapper with
  every Level argument passed explicitly as NULL.
- `quantile()` type differs across software (Hyndman-Fan), so medians, IQRs and reference limits
  differ at small n; IQR shown as Q1-Q3 vs a single width.
- SD vs SE mislabel; n vs n - 1; z instead of t at small n.
- Continuity corrections applied by default in some base tests (`chisq.test` 2x2, `prop.test`,
  `mcnemar.test`, normal-approximation `wilcox.test`); exact/asymptotic switching rules; Welch vs
  pooled variance; sidedness.
- Multiplicity: which family, which method, named in the output.
- Ratio CIs built on the log scale; geometric means; back-transformed SEs; `log` vs `log10`;
  percent multiplied by 100 twice.
- Bootstrap: resampling unit (patient, not row, for clustered or paired data); stratification;
  percentile vs BCa with too few replicates; failed replicates dropped silently; seed handling.
- Pre-testing (normality test chooses the test) is itself a questionable procedure; if used, it
  must be disclosed.
- Clustered pathology data analysed as independent: multiple cores, blocks, slides, lesions or
  bilateral organs per patient.
- A column written back to the dataset with `Output$setValues(<vector>)` carries no row numbers, so
  jamovi writes value i to row i. Under any jamovi filter the saved risk score or predicted class
  lands on other patients - lassocox put 68 of 68 scores on the wrong row. Pass `data[-(1:20), ]`
  and assert the element's `.rowNums` equals `as.integer(rownames(data))`. Invisible to testthat,
  where nothing is filtered and row names are 1..n.

**Descriptives and cross-tabulation**

- Chi-square with sparse expected counts (Cochran: no more than 20% below 5, none below 1) and the
  fallback actually used; ordered categories need a trend test; paired data need McNemar.
- McNemar's validity is governed by the DISCORDANT pairs b + c, not by the table total: a guard
  written on total N passes a table with n = 206 and 6 discordant pairs, where jjpiestats reported
  p = 0.1025 against an exact binomial p = 0.2188. Below about 25 discordant pairs the comparison is
  `binom.test(b, b + c, 0.5)`; Fisher is not the alternative, it assumes independent samples. Check
  the correction too: `statsExpressions::contingency_table(paired = TRUE)` calls
  `mcnemar.test(correct = FALSE)` while base R defaults to TRUE, so an unqualified "McNemar's test"
  in a Methods sentence does not reproduce.
- 2x2 measures: OR vs RR vs RD labels; table orientation; zero-cell correction disclosed;
  `fisher.test` reports the conditional MLE odds ratio, not the cross-product; CI method named.
- `fisher.test()` on an r x c table returns no `$statistic`, so an effect size built as
  `sqrt(test$statistic / n)` is `numeric(0)` and the following `is.na()` throws "argument is of
  length zero". Wrapped in `tryCatch(..., error = function(e) NULL)` - pathagreement - every
  categorical row then vanished from the table with no message. Assert the row EXISTS, not merely
  that nothing errored. Choose Fisher by EXPECTED counts (`chisq.test(tab)$expected < 5`), not
  observed, and seed `simulate.p.value = TRUE`.
- Proportion CIs: Wald vs Wilson vs Clopper-Pearson, named, and behaviour at 0% and 100%.

**Agreement and reliability**

- Weighted kappa: linear vs quadratic weights and what the package's keywords mean; kappa CI must
  use the non-null SE while the z test uses the null SE (Fleiss, Cohen and Everitt 1969).
- Concretely, how that rule gets broken here: `irr::kappa2()$statistic` is the z for H0 kappa = 0,
  so recovering `se <- kappa / z` yields the NULL SE and the Wald interval is systematically too
  narrow (0.05 to 0.075 on realistic data), blowing up as kappa approaches 0. Take the ASE from
  `vcd::Kappa(table)$Unweighted["ASE"]` - it matches `psych::cohen.kappa()` to five decimals - and
  derive kappa, CI, z and p from that one SE so the row is internally consistent. vcd needs a SQUARE
  table built on the union of levels and returns NaN ASE at perfect agreement.
- More than two raters: Fleiss' kappa generalises Scott's pi, not Cohen's kappa; Light's kappa;
  Krippendorff's alpha; missing ratings.
- `irrCAC` defaults `categ.labels` to `sort(unique(values))`, so Benign < Atypical < Malignant
  becomes Atypical < Benign < Malignant in every weighted distance: ordinal Gwet AC2 read 0.562
  against a correct 0.123. Pass `categ.labels` from the factor levels; unweighted coefficients are
  order-invariant. Separately, `irrCAC::krippen.alpha.raw()` with ordinal or interval weights is
  Gwet's weighting, not Krippendorff's: on Krippendorff's own 2011 worked example it gives .834 and
  .800 where the published values are .815 and .849, which `irr::kripp.alpha()` on level-ordered
  codes reproduces exactly. Validate an agreement coefficient against the method author's published
  worked example, not against another library.
- Categories unused by one rater; ordinal scales scored with unweighted kappa.
- Kappa paradox at extreme prevalence: prevalence and bias indices, PABAK, Gwet's AC1.
- ICC form (Shrout-Fleiss / McGraw-Wong; single vs average; agreement vs consistency) matches the
  design and is named; interpretation applied to the interval, not only the point estimate
  (Koo and Li 2016).
- `psych::ICC()$results` rows have been named `Single_random_raters` and so on since psych 2.x, with
  ICC1..ICC3k moved into a `type` column: `results["ICC2", "ICC"]` returns NA with no error, the
  table reads "Unable to calculate" for ever and nothing fails. Index with
  `res[res$type == "ICC2", ]`, and build codes for categorical ratings with
  `as.integer(<ordered factor>)`. Grep `results\["ICC[123]k?"`.
- Bland-Altman: limits, their CIs, proportional bias, repeated measures per subject.
- A proportional-bias slope regressed on a level that shares measurement error with either reading
  is an artefact, not a finding: on the reference the slope is -s_y^2 / var(y), always negative and
  reading as "compression" (unbiased regions called MATERIAL in up to 99.9% of simulated studies);
  on the classic pair mean it is (s_x^2 - s_y^2) / (2 var L), wrong whenever the two readings have
  unequal error; even the mean of the OTHER regions carries it in through a shared site effect.
  Without replicates the slope is not identified at all (Dunn 2004; Carstensen 2010). OLS standard
  errors compound it: with error proportional to the level, as IHC scores are, the slope test
  rejected in 33 to 48% of null studies, and HC3 brings it near nominal. Simulate a null that
  VIOLATES the design's independence assumption and run it to large n - an artefact grows in
  significance, not in size.
- Landis-Koch and similar bands are conventions and must be presented as such.

**Diagnostic accuracy, ROC, decision analysis**

- Which level is disease and which marker direction is positive. `pROC::roc(direction = "auto")`
  chooses the direction from the group medians, which in practice keeps AUC at or above 0.5: it
  hides an inverted marker and biases resampled AUCs upward.
- Transposed 2x2 turns sensitivity into PPV. Verify orientation with an asymmetric table.
- PPV and NPV depend on prevalence; under case-control sampling the sample prevalence is
  meaningless and a user-supplied prevalence with Bayes is required.
- AUC CI (DeLong vs bootstrap); correlated ROC curves need the paired test; two tests on the same
  patients need McNemar on discordant pairs for sensitivity and specificity separately.
- Data-driven cut-offs: Youden vs closest-to-corner vs cost; ties in the marker; `>=` vs `>` at the
  cut; sensitivity and specificity at the selected cut are optimistic without resampling.
- Decision curve: net benefit = TP/n - FP/n x pt/(1 - pt) (Vickers and Elkin 2006); treat-all and
  treat-none references; clinically sensible threshold range; miscalibrated probabilities.
- Calibration: slope and intercept definitions (intercept with the linear predictor as offset);
  slope is 1 by construction on development data; Hosmer-Lemeshow df = g - 2 only when the model
  was fitted on these same data, and df = g when the probabilities come from a fixed external
  model - using g - 2 there is anti-conservative (p too small); plus the test's known weaknesses
  (power driven by n, answer driven by g and by the grouping rule).
- NRI/IDI need caveats (Kerr 2014; Hilden and Gerds 2014; Pepe 2015).
- No gold standard / latent class: identifiability, conditional independence, label switching.
- DTA meta-analysis: bivariate (Reitsma) or HSROC, not separate pooling of sensitivity and
  specificity.
- Decision trees and Markov models: chance-node probabilities sum to 1; rate to probability is
  `1 - exp(-r t)`, never division by years; transition rows sum to 1 and the cohort trace is
  conserved every cycle; half-cycle correction; discounting `1/(1+r)^t` from the correct cycle
  index, on costs and effects; dominance and extended dominance before ICERs; negative ICERs are
  uninterpretable, use NMB; PSA distributions (beta for probabilities and utilities, gamma or
  log-normal for costs, log-normal for ratios, Dirichlet for multinomial).

**Survival**

- Event coding and which level is the event; this module's multi-state outcome options (`dod`,
  `dooc`, `awd`, `awod`) mapped correctly for overall, cause-specific and competing-risk analyses.
- Time units consistent across median, landmark, RMST tau, axes and sentences; date differencing
  (formats, month length); zero or negative times; left truncation.
- KM median and CI: `conf.type` named (R and SAS differ by default); median not reached is `NA`,
  never the last time; survival beyond last follow-up; at-risk convention at time t.
- Median follow-up: reverse Kaplan-Meier (Schemper and Smith 1996) vs median of observed times;
  the label must say which.
- Log-rank vs weighted variants named; stratified; pairwise with adjustment; trend for ordered
  groups; crossing hazards.
- Cox: ties method (R and SAS/Stata differ by default); reference level and HR direction; Wald vs
  LR vs score p labelled; `cox.zph` and its transform; separation (infinite HR) and Firth; EPV and
  the Riley criteria; "HR per unit" of what; clustering; time-dependent covariates and immortal
  time; landmark analysis resets the origin and excludes earlier events.
- Frailty: `logLik(coxme)` is NOT the integrated partial log-likelihood, so an LR test built from it
  against `coxph` is inflated - on the same fit, 29.7 via `fit$loglik["Integrated"]` against 90.5
  via `logLik()`. Use the Integrated element and halve the p (Self and Liang 1987: the variance sits
  on the boundary of its parameter space). A coxme variance estimate is always positive, so
  `ifelse(var > 0, "Significant", ...)` is not a test. coxme fits only a Gaussian random effect on
  the log hazard; gamma frailty needs `coxph(... + frailty(..., distribution = "gamma"))`. Never mix
  `AIC(coxme)` with `AIC(coxph)`.
- `flexsurv` / `flexsurvcure` return `model$res` on the natural scale and `model$res.t` on the
  estimation scale (logit theta, log shape, log scale). Reading `res.t` as natural reported a cure
  fraction of -0.90 where the truth was 0.29, and labelled every Weibull shape between 1 and e as a
  decreasing hazard. Grep `\$res\.t` wherever a value is displayed or compared with a threshold, and
  match the parametrization (`dist = "weibull"` in curemodels maps to `weibullPH`).
- C-index: Harrell vs Uno; `survival::concordance()` given a formula with a risk score treats a
  higher value as longer survival, so a risk score needs `reverse = TRUE`; optimism. On a `coxph`
  OBJECT the same argument is a hard error ("reverse argument is not an appropriate fit object"),
  and because that call normally sits inside a `tryCatch` the result is a table of NA rather than a
  crash: lassocox's entire "LASSO vs Standard Cox" comparison was NA on every dataset. Classify
  every `concordance(` call by its FIRST argument - object drops `reverse`, formula keeps it - and
  assert the cell is non-NA, not merely that nothing threw.
- `timeROC::timeROC(times = t)` with a SINGLE time returns `$times = c(0, t)`, so `AUC[1]`,
  `inference$vect_sd_1[1]` and `FP[, 1]` are the NA t = 0 slot; index with
  `which(abs(r$times - t) < 1e-8)`. `vect_sd_1` is already a standard error, so `sqrt()` of it
  inflates it 5 to 7 times. timeROC also calls `Surv()` unqualified while neither importing nor
  depending on survival, so under jamovi and under testthat it errors "could not find function
  Surv" and a `try()` quietly takes whatever fallback follows.
- Competing risks: cumulative incidence (Aalen-Johansen), not 1 - KM, which overestimates; Gray's
  test vs cause-specific log-rank; Fine-Gray subdistribution HR is not a cause-specific HR and must
  not be interpreted as one; censoring code vs competing-event code.
- RMST: tau no later than the smallest maximum follow-up across arms; difference and ratio.
- Optimal cut-point by minimum p: p must be corrected (Lausen-Schumacher, `maxstat`) and the HR is
  optimistically biased (Altman 1994).
- Staging comparison and stage migration: not by p-values alone; C-index difference with CI,
  AIC/BIC, likelihood ratio, bootstrap validation.
- Power: Schoenfeld events d = (z_a + z_b)^2 / (p1 p2 (log HR)^2); one vs two sided; accrual and
  follow-up to event probability; non-inferiority margin direction.
- `gsDesign::gsSurv()` and `gsDesign()` default to `test.type = 4`: two-sided asymmetric with a
  non-binding futility bound. The efficacy boundaries are identical to an efficacy-only design, so
  the boundary table looks right and only N moves - 618 against 588 on the same design. If the
  analysis exposes no futility option, pass `test.type = 1` with the one-sided alpha. A regression
  test that omits the argument locks the inflated N in as ground truth, which is what happened to
  survivalPower. Other users: `gsdesign`, `advancedtrials`, `comprehensiveSurvivalPower`.
- Rates: person-time denominators; exact Poisson (Garwood) intervals.
- RECIST 1.1 for waterfall and swimmer plots: percentage change from baseline for display;
  progression is >= 20% **and** >= 5 mm above nadir, or a new lesion, or unequivocal non-target
  progression; ORR = (CR + PR)/N with an exact interval; evaluable vs intention-to-treat
  denominators stated.
- What shipped wrong here was not the rule but the comparison: see the derived-threshold bullet
  under "Any analysis". Build trajectories that land exactly on +20%, -30% and -100%, compare
  against an independent base-R reference, and run that reference on the UNFIXED build first
  (waterfall: 435 of 600 categories before, 600 of 600 after).
- Best overall response counts assessments only up to the FIRST progression - RECIST 1.1: any
  progression precludes a later CR, PR or SD - with the baseline included in the nadir (an implicit
  0% at time 0 when percent data carry no baseline row), and any regrowth after a zero nadir is PD.
  iRECIST, which resets after an unconfirmed PD, is a different rule set and must not be
  approximated by ignoring progression. Check `recist`, `waterfallrecist` and `irecist` for both.

**Statistical plots (ggstatsplot wrappers)**

- The subtitle is an analysis: test actually run per `type`, variance assumption, effect size
  (which one, bias-corrected or not, CI level), Bayes-factor prior, pairwise adjustment method and
  which pairs are displayed, trimming fraction for robust tests. Read `formals()`.
- Paired designs: complete pairs only, rows aligned by subject id. A paired test on unsorted long
  data is silently wrong.
- Axis truncation that drops data; log axes with zeros; percentage labels not summing to 100.
- Any wrapped plotting library can truncate its own input: `UpSetR::upset()` takes `nsets = 5`, so
  a 6- or 7-column membership matrix silently loses the smallest sets AND every intersection bar is
  recomputed over the survivors, with no warning and no missing-set indication (venn, with a UI
  offering var5 to var7). Diff the library's `formals()` against the arguments actually passed and
  look for count-limiting defaults - `nsets`, `nintersects`, `n`, `max`, `top_n`. Only a run with
  more inputs than the default exposes it, so schema and wiring checks never will.

**Laboratory and pathology-specific**

- Reference intervals (CLSI EP28-A3c): nonparametric needs n >= 120 per partition; 90% CIs on each
  limit; outlier rule applied before estimation; partitioning; transform and back-transform.
- Method comparison: Passing-Bablok or Deming, not OLS or a correlation coefficient; error-variance
  ratio; linearity check.
- Sigma metric = (TEa - |bias|)/CV with all three in percent at the same concentration.
- Scores: H-score 0-300; Allred 0 or 2-8; PD-L1 TPS vs CPS definitions and the cap at 100; HER2,
  Ki-67, TIL cut-offs. Every hard-coded cut-off names its guideline and year and is
  user-overridable.
- Sampling adequacy (blocks, sections, lymph-node yield): detection 1 - (1 - p)^n assumes
  independence; within-case clustering means overdispersion (beta-binomial); hypergeometric when
  sampling without replacement.
- Staging: AJCC edition or site-specific version named wherever a stage is derived.

**Reporting standards to test outputs against:** STARD 2015, REMARK, TRIPOD+AI 2024, STROBE,
CONSORT 2025, GRRAS, PRISMA-DTA, CHEERS 2022, SAMPL; QUADAS-2 and PROBAST as bias context.

## Severity and verdict

| Severity | Definition |
|---|---|
| **S1 Critical** | A wrong number, direction, label or sentence that could change a clinical or research conclusion, produced silently |
| **S2 Major** | Correct in common conditions, wrong or undefined under plausible clinical data with no warning; missing or mislabelled uncertainty; a fabricated fallback value; an unguarded method-choice defect |
| **S3 Moderate** | Defensible but undocumented convention that differs from the dominant one; narrative overreach; rounding or precision that can mislead; unsourced interpretation bands |
| **S4 Minor** | Wording, citation formatting, cosmetic inconsistency |

| Verdict | Rule |
|---|---|
| **VALIDATED** | Every H-risk claim passed with `EXEC-IND` or `SIM`; at least 90% of H and M numeric claims verified by execution; no open S1 or S2 |
| **VALIDATED WITH CONDITIONS** | No open S1; each S2 has an explicit condition of use that you state ("valid for two raters, nominal scale, no missing ratings") |
| **NOT VALIDATED** | Any open S1, or an H-risk claim that failed or could not be verified |
| **NOT TESTABLE** | The analysis could not be executed; say exactly why |

Do not average. Do not soften. A report with zero findings must justify itself by listing the
strongest attacks that were tried and failed.

## Deliverables

1. `development-scripts/validate_SANITIZED_FN.R` - the reproducible evidence.
2. `development-ideas/SANITIZED_FN-validation-YYYY-MM-DD.md` - the report, plus the
   `-matrix.csv` the script writes.
3. `tests/testthat/test-SANITIZED_FN-validation.R` when `tests=write`.

Report structure:

```markdown
# SANITIZED_FN validation - YYYY-MM-DD

Verdict: <one of four>. Depth, focus, flags. R and package versions. Umbrella source or installed
submodule. Wall time. Static-only? (E10)

## 1. Verdict
One paragraph: what is proven correct, what is wrong, what a user must not rely on today.

## 2. Coverage
Claims: total / H / M / L. Verified by execution: n (%), by lens. H-risk with EXEC-IND or SIM: x of y.

## 3. Findings
### S1 ... ### S4. For each:
ID (VAL-SANITIZED_FN-NN) | lens | severity | defect class | claim ID
Claim violated - what the user reads
Reproduce - data and call, under 15 lines
Expected - value, oracle, source with locator
Observed - value; absolute and relative difference
Root cause - file:line
Clinical consequence - one sentence
Smallest fix - proposed, not applied
Regression test - test name, live or skip-guarded

## 4. Validation matrix
Claim x test x grade x observed x expected x diff x tol x result (from the script, verbatim).

## 5. Method-choice concerns
Correctly implemented, questionable for clinical data; the guard or disclosure that would fix each.

## 6. Narrative, threshold and citation audit
Sentence or band -> source -> verified? -> verdict.

## 7. Not verified, and why
Every READ / NONE / CITED-only claim, with what would be needed to verify it.

## 8. Re-run
Exact commands.

## 9. Hand-off
Findings mapped to `/fix-function`, `release-review-function`, `/fix-notices`, `/update-refs`.
If any S1 is open, recommend routing the analysis to JamoviTest (append `T` to `menuGroup`)
until fixed. Recommend it; do not do it.

## Out of scope observations
One line each.
```

No strengths section, no generic advice, no praise. Every sentence in the report is a claim with
evidence, a defect with a reproduction, or a stated gap.

## Related commands

- `/check-function-full` - integration, notices and does-each-option-do-anything audit
- `/review-function` - code review, UX, i18n, library-review gate
- `release-review-function` - review and repair with a release verdict; this report is its statistical-verification input
- `/fix-function` - apply the fixes proposed here
- `/generate-test-data` - realistic fixtures when the bundled data do not reach a branch
