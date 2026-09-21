---
name: independent-reviewer
description: Independently audit and experimentally test one or more jamovi analyses for mathematical, statistical, logical, scientific and clinical correctness. Audit only, no production changes; separates executed tests from static inspection and never issues an unconditional correct verdict
interactive: true
args:
  targets:
    description: Name of the jamovi function to audit, or a comma-separated list
    required: true
    autocomplete: functions
usage: /independent-reviewer <function_name>[,<function_name>...]
examples:
  /independent-reviewer agreement
  /independent-reviewer survival,multisurvival
  /independent-reviewer decisiongraph
---

_Note: no emoji. The target is read from `$ARGUMENTS`; a comma-separated list audits several analyses
and adds a consolidated summary._

# Independent review of a jamovi analysis

| Question | Playbook |
|---|---|
| Are the numbers, labels and sentences true, with a registered regression test left behind? | `/validate-function` |
| **Is this analysis correct, as judged by an independent expert auditing it cold?** | **`/independent-reviewer`** (this file) |

`/validate-function` is the narrower, artifact-producing sibling: one analysis, flags, a reproducible
validation script and a `tests/testthat/` file. Use this playbook when you want a formal independent
audit, when several analyses are in scope, or when scientific validity and claim-versus-evidence are
the question rather than a numeric parity matrix. The two overlap by design; do not run both on the
same analysis in one pass.

## Machinery borrowed from /validate-function

Read `.claude/commands/validate-function.md` before starting. This playbook does not restate its
apparatus; it uses it by name, so the rules live in one place and cannot drift apart.

| Needed here | Defined there, under |
|---|---|
| The evidence grade on every assertion: `EXEC-IND`, `EXEC-WIRE`, `EXEC-PROP`, `SIM`, `DERIVED`, `CITED`, `READ`, `NONE` | "Rules of evidence", E1 |
| No number and no package default from memory; test the public wrapper | E2, E3, E4 |
| Oracle independence, and three-way adjudication of a mismatch | E5, E6 |
| Numerical tolerance per comparison class | "Tolerances" |
| Hypotheses worth testing, by analysis family | "Trap catalogue" |
| Severity S1-S4 | "Severity and verdict" |

Do not invent a parallel grade, tolerance or severity scale. If this audit must depart from one of
those rules, name the rule and the reason in the report.

Act as an independent reviewer with expertise in mathematical statistics,
biostatistics, clinical epidemiology, pathology, and R/jamovi development.

Critically audit and experimentally test the following jamovi analyses:

TARGET: $ARGUMENTS

Your objective is to determine whether their calculations, statistical
methods, implementation logic, scientific claims, and clinical
interpretations are correct and supported by evidence.

Prioritize defects that could produce incorrect results or misleading
clinical conclusions. Successful execution is not proof of correctness.

WORKING RULES

- Read AGENTS.md, CLAUDE.md, the relevant review playbooks, and applicable
  development guides before inspecting the implementation.
- Use repository guidance for architecture and workflow. Independently
  evaluate mathematical claims and clinical thresholds appearing in that
  guidance; do not assume local examples or heuristics are authoritative.
- This is an audit: do not change production code, defaults, menuGroup,
  dependencies, or release configuration. Create reproducible audit
  scripts and synthetic fixtures separately. Run commands that regenerate
  files in an isolated copy.
- Inspect generated .h.R files where useful, but never edit them manually.
- Execute feasible checks. Clearly distinguish executed tests from static
  inspection, proposed tests, and checks blocked by the environment.
- Use synthetic or appropriately de-identified data.
- Every assertion in the report carries exactly one evidence grade (E1).
  Only `EXEC-*` and `SIM` count as verified; `READ` alone never closes a
  section, and "the code looks right" is `READ`.
- For multiple analyses, provide individual assessments and a consolidated
  summary. Explicitly identify any analyses not fully assessed. The
  consolidated summary carries what a one-analysis pass cannot see: a
  shared helper in `R/utils*.R` whose defect reaches every caller, the same
  quantity computed two different ways in two analyses, and conventions
  (event coding, reference level, time unit, positive class, interval
  method) that disagree between analyses a user reads side by side. Test
  those directly; do not infer them from the individual sections.

## 1. ESTABLISH WHAT THE ANALYSIS CLAIMS TO DO
Inspect:

- R/<function>.b.R and relevant helper functions.
- jamovi/<function>.a.yaml, .u.yaml, and .r.yaml.
- Generated interfaces, JavaScript handlers, dependencies, documentation,
  examples, references, and existing tests.

Before testing, state:

- Intended scientific question and study design.
- Target population, the unit of observation, and the unit the data are
  actually clustered on when those differ (patient, specimen, block, slide,
  lesion, rater).
- Estimand, written as one sentence naming the population, the outcome, the
  quantity (difference, ratio, rate, probability, agreement), the contrast
  or reference, the time horizon, and how missing data and intercurrent
  events are accounted for. If that sentence cannot be written from the
  code and the documentation together, that is the first finding.
- Required inputs, coding conventions, reference categories, units, and
  relevant time origins.
- Assumptions, supported data structures, and unsupported uses.
- Claimed outputs and intended interpretation.

Trace each important result from raw data through preprocessing,
calculation, transformation, and presentation.

Check whether the interface, implementation, documentation, and reported
interpretation describe the same method and estimand.

## 2. MATHEMATICAL CORRECTNESS
Verify applicable formulas and algorithms, including:

- Numerators, denominators, normalization, weights, and degrees of freedom.
- Estimates, standard errors, variance/covariance calculations, test
  statistics, p-values, and interval limits.
- One-sided versus two-sided calculations.
- Transformations, inverse transformations, logarithm bases, and units.
- Reference-category direction, sign conventions, and event coding.
- Boundary behavior, numerical stability, overflow, underflow, and rounding.
- Whether undefined or non-estimable quantities remain explicitly
  unavailable rather than becoming plausible-looking numbers.

Independently derive or calculate representative results using small
examples with known answers.

Compare against trusted reference implementations with matched settings.
Document differences in defaults, missing-data handling, tie methods,
continuity corrections, interval methods, and parameterization.

Calling the same underlying package twice is a wrapper consistency check,
not an independent mathematical validation. State this limitation.

Where a quantity comes from a constant hard-coded in the backend - a
published regression coefficient, a scoring or staging cutoff, a weight -
no computation is an oracle for it. Re-typing the constant into an
expected value tests transcription only. Verify each constant against the
cited source directly, including which terms the published model contains
and which it does not, and label any constant with no locatable source
unverified.

Do not assume every discrepancy is a bug or every agreement proves validity.

## 3. STATISTICAL CORRECTNESS
Evaluate whether the method is appropriate for the actual study design,
data structure, and estimand.

Where applicable, assess:

- Independent, paired, clustered, and repeated observations.
- Outcome distributions and model/link-function choices.
- Assumptions, diagnostics, and consequences of violations.
- Small samples, sparse cells, separation, singularity, convergence,
  identifiability, and model complexity.
- Missing-data handling and its assumptions; report exclusions and the
  effective sample size for each result.
- Sampling weights, frequency weights, and survey design.
- Confounding, interactions, nonlinear relationships, and adjustment.
- Multiple testing, subgroup exploration, and data-driven selection.
- Bootstrap/permutation resampling units and reproducibility.
- Prediction leakage, preprocessing within resampling, optimism,
  calibration, discrimination, and external validation.
- Bayesian priors, posterior computation, convergence, and interpretation
  when Bayesian methods are implemented.

Distinguish a correctly implemented calculation from a statistically
appropriate analysis.

Do not impose universal sample-size, events-per-variable, normality-test,
or performance thresholds without method-specific justification.

## 4. LOGICAL AND JAMOVI IMPLEMENTATION CORRECTNESS
Build a traceability matrix:

UI option -> option definition -> backend branch -> calculation ->
result object -> displayed table/plot/text.

Check:

- Options are correctly passed, validated, and used.
- Defaults and conditional branches implement their advertised behavior.
- Filtering, missingness, sorting, joins, and transformations preserve
  alignment between outcomes, predictors, subject IDs, and weights.
- Factor labels, unused levels, ordering, and nonstandard variable names
  are handled correctly.
- Tables, plots, exported results, and generated interpretations agree.
- Errors and warnings are visible and actionable.
- Failed calculations cannot silently return zero, stale results, or
  misleading success messages.
- Repeated option changes clear and recompute affected outputs.
- Valid -> invalid -> valid input transitions recover correctly.
- Plot resizing, saving/reopening .omv files, and exporting retain correct
  results where these behaviors can be tested.
- Package namespace/dependency behavior works in a clean R session and
  the relevant installed submodule, not only under devtools::load_all().

Test each substantive option with data capable of exposing its effect.
An unchanged result on an insensitive dataset is not sufficient evidence
that an option is broken. Presentation-only options should preserve
numerical results.

Separate backend testing from actual jamovi application testing.
Compilation, linting, and jmvtools::check() do not establish statistical
correctness.

## 5. SCIENTIFIC VALIDITY
Determine whether the implementation and documentation support the
scientific claims being made.

Check:

- Whether the design can answer the stated research question.
- Temporal ordering, selection bias, confounding, information bias,
  pseudoreplication, and data leakage where relevant.
- Whether association is incorrectly described as causation.
- Whether exploratory findings are presented as confirmatory.
- Whether internal fit is presented as external validity.
- Whether generalizability claims exceed the available evidence.
- Whether cited methods actually support the implemented procedure.
- Whether assumptions and limitations are accurately communicated.

Verify important methodological claims against primary literature,
official statistical documentation, or original method descriptions.
Record source links and relevant software versions.

If sources cannot be verified, label the claim unverified.

Record this section as a ledger, one row per claim the software makes in
text, footnote, reference, interpretation band or default:

| Claim as the user reads it | Where shown | Source and locator (DOI/PMID plus equation, table or page) | Does the source say this? | Grade |
|---|---|---|---|---|

A claim with no locator is `CITED` at best and `NONE` when the source cannot
be found. A band or threshold presented as fact rather than as convention is
a finding even when the band is the conventional one.

Distinguish limitations inherent to the method from defects in the
implementation and misuse that the software cannot detect from the data.

## 6. CLINICAL CORRECTNESS AND INTERPRETATION
Review outputs as a pathologist or clinician might read and reuse them.

Check:

- Correct outcome definitions, units, direction of benefit/harm, reference
  groups, and time horizons.
- Accurate distinction among odds, risks, rates, hazards, and probabilities.
- Appropriate absolute and relative effect reporting.
- Separation of statistical significance from clinical importance.
- Accurate communication of uncertainty.
- No unsupported interpretation of a nonsignificant result as equivalence
  or evidence of no effect.
- No unsupported diagnostic, prognostic, or treatment recommendations.
- Clinical cutoffs, categories, and decision thresholds have a documented
  rationale applicable to the intended population and setting.
- Automated report sentences remain accurate for null, adverse,
  inconclusive, boundary, and non-estimable results.

Apply relevant domain checks only:

- Diagnostic accuracy: positive-class coding, test direction, threshold
  selection, reference standard, prevalence, predictive values, and
  verification/spectrum bias.
- Survival: event/censor coding, time origin, delayed entry, ties,
  competing events, model assumptions, and prediction horizons.
- Agreement: agreement versus association, rater structure, weighting,
  prevalence effects, and the intended ICC/kappa definition.
- Decision/economic analysis: probability constraints, transition logic,
  utilities, costs, discounting, time horizon, and uncertainty.
- Prediction: intended use, calibration, validation, transportability,
  threshold consequences, and clinical utility.

Do not infer clinical validation or suitability for individual patient
care from correct code or synthetic test results.

## 7. REPRODUCIBLE TESTING

For the jamovi-specific mechanics of executing an analysis - calling the generated wrapper, the
no-variable-calculator case, reading tables, HTML and plots back out, the `asDF` quoted-rowname
trap, tolerance rules and the results-matrix harness - follow `/validate-function` Phase 0 step 3,
Phase 4 and its Tolerances table rather than reinventing them here. This playbook owns the audit;
that one owns the plumbing.
Construct and execute a test matrix containing:

A. Known-answer examples
   Small datasets with independently calculated expected results.

B. Reference comparisons
   Matched analyses using established implementations, including estimates,
   uncertainty, sample sizes, predictions, and relevant plot coordinates.

C. Property-based and metamorphic checks
   Derive applicable invariants before testing, such as row-order
   invariance, equivalent relabeling, expected reference-group reversal,
   and appropriate behavior under unit conversion. Do not impose an
   invariant where the method legitimately depends on order or scale.
   The `/validate-function` metamorphic table lists the relations already
   worked out for this module's methods; use it as the starting set.

D. Boundary, degenerate and adversarial cases
   Empty/all-missing data, tiny samples, constant variables, unused levels,
   zero cells/events, extreme imbalance, ties, invalid values, numerical
   extremes, separation, and other method-specific failure conditions.
   Required behavior is the correct value or an explicit refusal, never a
   plausible-looking number.

E. Option and lifecycle checks
   Defaults, meaningful alternative settings, important interactions,
   repeated updates, stale-result prevention, and recovery after failure.

F. Simulation checks, when justified
   Assess bias, interval coverage, type I error, or other relevant
   operating characteristics under specified data-generating mechanisms.
   Prespecify seeds, repetitions, acceptance criteria, and numerical
   tolerances. Report Monte Carlo uncertainty and simulation limitations.

For every test, record:

- Purpose and triggering input.
- Expected behavior and independent basis for that expectation.
- Actual behavior.
- Numerical tolerance and its justification.
- PASS, FAIL, BLOCKED, or NOT APPLICABLE.
- Reproduction command and relevant output.

Do not copy implementation logic into the test oracle, weaken tests to
obtain a pass, or silently change expected values after observing results.

## 8. REPORT FINDINGS AND READINESS
Lead with consequential findings, ordered by severity.

For each finding provide:

- Title, severity, and confidence.
- Exact file/line or displayed output affected.
- Minimal reproducible input and options.
- Expected versus observed behavior.
- Mathematical, statistical, or scientific justification.
- Potential effect on results and clinical interpretation.
- Recommended correction and a regression test.

Separate confirmed defects, suspected issues, methodological limitations,
unsupported claims, and optional improvements.

Include:

1. A concise verdict for each analysis.
2. An evidence table covering mathematical, statistical, logical,
   scientific, and clinical correctness, each cell carrying its evidence
   grade and the test that produced it.
3. Test results and option/output coverage.
4. Unverified areas, blocked checks, and remaining uncertainty.
5. References and reproducibility details, including sessionInfo().
6. Prioritized corrections and the evidence needed to close each finding.

Deliverables, in the same tree `/validate-function` uses. SANITIZED_FN is
the target name with paths, flags and `.a.yaml|.b.R|.r.yaml|.u.yaml`
stripped; one set per audited analysis:

1. `development-scripts/audit_SANITIZED_FN.R` - the executed checks,
   seeded, self-contained, exits non-zero on any FAIL.
2. `development-ideas/SANITIZED_FN-independent-review-YYYY-MM-DD.md` - this
   report, plus the test matrix written beside it as `-matrix.csv`.
3. For several targets, one
   `development-ideas/independent-review-YYYY-MM-DD-summary.md` holding the
   consolidated summary and the cross-analysis findings.

No `tests/testthat/` file: this playbook is audit-only and proposes the
regression test inside the finding instead. Run `/validate-function` on the
analysis to have that test written.

Assess these separately:

- Computational correctness within the tested scope.
- Statistical validity for the stated design and assumptions.
- Accuracy of scientific and clinical interpretation.
- Software release readiness.
- Evidence for any claimed clinical application.

Use PASS WITHIN TESTED SCOPE, FAIL, or INSUFFICIENT EVIDENCE, one per
dimension, on these rules:

- PASS WITHIN TESTED SCOPE: no open S1 or S2 in that dimension, every
  consequential claim in it graded `EXEC-*` or `SIM`, and the untested
  scope named in the same sentence as the verdict.
- FAIL: an open S1 or S2 in that dimension.
- INSUFFICIENT EVIDENCE: no defect found, but a consequential claim rests
  on `READ`, `CITED` or `NONE`. Absence of a failing test is not a pass.

Severity S1-S4 is the `/validate-function` scale; do not redefine it.

So that a reader of either report knows what the other means:

| This report | `/validate-function` | Both mean |
|---|---|---|
| PASS WITHIN TESTED SCOPE | VALIDATED | no open S1 or S2, scope of the evidence stated |
| PASS WITHIN TESTED SCOPE, conditions listed | VALIDATED WITH CONDITIONS | no open S1; every S2 carries an explicit condition of use |
| FAIL | NOT VALIDATED | an open S1, or a consequential claim that failed |
| INSUFFICIENT EVIDENCE | NOT TESTABLE, when nothing could be executed at all | evidence missing rather than a defect found |

Do not issue an unconditional "correct," "clinically validated," or
"ready for release" verdict when essential evidence is missing.

Finish by answering, once per analysis:
"What could make this analysis give a convincingly wrong answer, and
which tests demonstrate that those failure modes are handled?"

Name at least three concrete failure modes: a wrong input a user would
plausibly supply, a clinical data structure the method does not fit, and a
silent fallback inside the implementation. For each, give the test that
probes it, its class letter, its evidence grade and its result; where no
test exists, say so and carry it into the unverified areas. An answer that
lists only failure modes already covered by a passing test is not an answer.
