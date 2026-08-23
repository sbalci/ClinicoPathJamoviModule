################################################################################
# KAPPASIZEFIXEDN - COMPREHENSIVE USAGE EXAMPLES
################################################################################
#
# Function: kappaSizeFixedN
# Purpose:  Given a sample size you cannot change, report the LOWER LIMIT of the
#           one-sided 100(1 - alpha)% confidence interval the study can expect
#           to achieve for kappa.
#
# WHAT THE RESULT MEANS
#
#   The number returned (kappaL) is the smallest level of agreement the study
#   would still be UNABLE TO RULE OUT. Every kappa below it is excluded.
#
#   kappaL is ALWAYS BELOW kappa0 - it is a confidence limit around the
#   anticipated value, not a detectable alternative. The distance
#   (kappa0 - kappaL) is the price your fixed sample size charges in precision:
#   the wider the gap, the vaguer the study's conclusion.
#
# WHAT kappa0 IS - AND IS NOT
#
#   kappa0 here is the agreement you ANTICIPATE OBSERVING. It is NOT a null
#   hypothesis. (In kappaSizePower, and only there, kappa0 is the null being
#   tested against an alternative kappa1.) Nothing in this analysis tests a
#   hypothesis or computes power, so phrases like "minimum detectable kappa"
#   or "detectable difference" do not apply to its output.
#
# THE DECISION IT SUPPORTS
#
#   Pick, in advance, the agreement floor your claim needs - commonly 0.40
#   (moderate), 0.60 (substantial) or 0.80 (almost perfect) on the
#   Landis-Koch scale, but ideally a threshold justified by the clinical use.
#   Then ask whether kappaL clears it:
#
#     kappaL >= your floor  -> the fixed n can support the claim
#     kappaL <  your floor  -> it cannot; enrol more subjects, add raters,
#                              relax alpha, or report the limitation openly
#     kappaL <= 0           -> the study cannot even rule out chance agreement
#
#   One caveat runs through every example below: kappaL is what the study
#   reaches IF the observed agreement lands exactly on kappa0. Roughly half of
#   such studies observe less and end with a lower bound below the figure
#   shown. Treat it as a planning expectation, not a guarantee.
#
################################################################################

# Load the ClinicoPath package
library(ClinicoPath)

################################################################################
# EXAMPLE 1: Small Pilot Study - Resource Constrained (n=30)
################################################################################
# Clinical Context: Dermatology resident training evaluation
# Constraint: Limited budget allows only 30 melanoma cases
# Question: With 30 cases, how vague will the agreement estimate be?

kappaSizeFixedN(
  outcome = "2",        # Binary: melanoma vs benign
  kappa0  = 0.40,       # Agreement ANTICIPATED between two residents
  props   = "0.10, 0.90", # 10% melanoma prevalence
  raters  = "2",        # Two dermatology residents
  alpha   = 0.05,       # One-sided 95% lower bound
  n       = 30          # FIXED: only 30 cases available
)

# Result: kappaL = 0.052
# - Anticipating kappa = 0.40, 30 cases leave a lower bound of only 0.05.
# - The study would exclude nothing above chance agreement in any useful sense:
#   it cannot distinguish "slight" from "moderate" agreement.
# - The 10% prevalence is doing much of the damage; the analysis also flags
#   sparse agreement-pattern cells here, so even 0.052 is optimistic.
# - Verdict: adequate for feasibility/logistics only. Do not report a kappa
#   estimate from this study as evidence of agreement.

################################################################################
# EXAMPLE 2: Emergency Department Feasibility - Time Constrained (n=40)
################################################################################
# Clinical Context: Emergency stroke CT protocol validation
# Constraint: Can review only 40 stroke CTs in a 2-month timeframe

kappaSizeFixedN(
  outcome = "2",          # Binary: hemorrhage present vs absent
  kappa0  = 0.50,         # Anticipated moderate agreement
  props   = "0.15, 0.85", # 15% hemorrhage rate
  raters  = "2",
  alpha   = 0.05,
  n       = 40
)

# Result: kappaL = 0.186
# - A 0.31 gap below the anticipated 0.50. The published conclusion would be
#   "agreement is at least 0.19" - too weak to justify a clinical protocol.
# - Verdict: extend the review window or pool a second centre before
#   committing to protocol implementation on these data.

################################################################################
# EXAMPLE 3: Rare Pathology - Case Availability Constrained (n=35)
################################################################################
# Clinical Context: Rare tumor grading agreement
# Constraint: Only 35 cases of the tumor available per year

kappaSizeFixedN(
  outcome = "3",                  # Three grades: low, intermediate, high
  kappa0  = 0.40,
  props   = "0.30, 0.40, 0.30",   # Reasonably balanced grades
  raters  = "2",
  alpha   = 0.05,
  n       = 35
)

# Result: kappaL = 0.191
# - Note the contrast with Example 1: 35 cases across three BALANCED grades
#   buy a better bound than 30 cases with a 10% binary prevalence. Balance of
#   the categories matters as much as raw n.
# - Verdict: still short of a "moderate agreement" claim. Two or three years
#   of accrual, or a second institution, would be needed.

################################################################################
# EXAMPLE 4: Training Program - Educational Budget (n=50)
################################################################################
# Clinical Context: Three pathology trainees, post-training assessment
# Constraint: Training budget allows 50 cases for competency evaluation

kappaSizeFixedN(
  outcome = "2",
  kappa0  = 0.50,
  props   = "0.25, 0.75",
  raters  = "3",          # Three trainees rate every case
  alpha   = 0.05,
  n       = 50
)

# Result: kappaL = 0.318
# - The third rater is what rescues this design: with 2 raters and otherwise
#   identical inputs the bound would be materially lower. Extra raters buy
#   information per subject, which is often cheaper than extra subjects.
# - Verdict: usable for internal competency feedback; not enough to certify
#   "substantial" (0.60) agreement.

################################################################################
# EXAMPLE 5: Standard QA Program - Annual Protocol (n=100)
################################################################################
# Clinical Context: Annual mammography quality assurance
# Constraint: QA protocol specifies 100 cases per radiologist pair

kappaSizeFixedN(
  outcome = "2",
  kappa0  = 0.50,
  props   = "0.20, 0.80", # 20% abnormal
  raters  = "2",
  alpha   = 0.05,
  n       = 100
)

# Result: kappaL = 0.314
# - The standing QA protocol can assert only "agreement is at least 0.31".
# - Useful QA finding in itself: if the programme wants to certify moderate
#   agreement annually, the protocol's n is too small and should be revised.

################################################################################
# EXAMPLE 6: Accreditation Requirement - Regulatory Minimum (n=120)
################################################################################
# Clinical Context: Pathology accreditation tumor grading validation
# Constraint: Accreditation body requires a minimum of 120 cases

kappaSizeFixedN(
  outcome = "4",                        # Four tumor grades
  kappa0  = 0.50,
  props   = "0.25, 0.30, 0.30, 0.15",
  raters  = "2",
  alpha   = 0.05,
  n       = 120
)

# Result: kappaL = 0.398
# - A 0.10 gap - the tightest so far, because 120 cases are spread over four
#   reasonably balanced grades.
# - Verdict: comfortably supports a "fair to moderate" claim; exceeding the
#   regulatory minimum would be needed to claim 0.60.

################################################################################
# EXAMPLE 7: Clinical Trial Endpoint - Enrollment Complete (n=150)
################################################################################
# Clinical Context: Biomarker agreement in a completed clinical trial
# Constraint: Trial enrolled 150 patients; enrollment is closed

kappaSizeFixedN(
  outcome = "2",
  kappa0  = 0.60,         # Anticipated substantial agreement
  props   = "0.35, 0.65", # 35% biomarker positive
  raters  = "2",
  alpha   = 0.05,
  n       = 150
)

# Result: kappaL = 0.476
# - The trial can state "biomarker scoring agreement is at least 0.48".
# - Verdict: honest and publishable, but it falls just short of certifying the
#   0.60 threshold the anticipated value sits on. Report the bound, not
#   kappa0, in the paper.

################################################################################
# EXAMPLE 8: Cancer Registry - Annual Capacity (n=250)
################################################################################
# Clinical Context: Cancer registry staging validation
# Constraint: Registry can validate 250 cases per year

kappaSizeFixedN(
  outcome = "5",                                # Five TNM stages
  kappa0  = 0.60,
  props   = "0.20, 0.25, 0.25, 0.20, 0.10",
  raters  = "2",
  alpha   = 0.05,
  n       = 250
)

# Result: kappaL = 0.536
# - A 0.064 gap. Note that five categories did NOT hurt: with balanced
#   proportions, more categories give MORE agreement patterns to fit and a
#   slightly tighter bound than a binary outcome at the same n.
# - Verdict: one year of registry capacity nearly certifies substantial
#   agreement; two years would clear 0.60 with room to spare.

################################################################################
# EXAMPLE 9: National Screening Program - Stringent alpha (n=300)
################################################################################
# Clinical Context: National mammography screening programme QA
# Constraint: Programme specifies a 300-case annual QA sample

kappaSizeFixedN(
  outcome = "2",
  kappa0  = 0.70,
  props   = "0.25, 0.75",
  raters  = "2",
  alpha   = 0.01,         # One-sided 99% bound - public-health rigour
  n       = 300
)

# Result: kappaL = 0.574
# - alpha = 0.01 costs precision: at alpha = 0.05 the same design would return
#   a visibly higher bound. Stringency and precision trade off directly.
# - Verdict: a defensible national-programme statement - "at 99% confidence,
#   agreement is at least 0.57".

################################################################################
# EXAMPLE 10: AI Validation Study - Budget Constraint (n=400)
################################################################################
# Clinical Context: AI diagnostic algorithm vs expert reference
# Constraint: Budget allows expert labeling of 400 images

kappaSizeFixedN(
  outcome = "2",
  kappa0  = 0.60,
  props   = "0.30, 0.70",
  raters  = "2",          # Algorithm and expert treated as two raters
  alpha   = 0.05,
  n       = 400
)

# Result: kappaL = 0.524
# - 400 images support "algorithm-expert agreement is at least 0.52".
# - Verdict: adequate for a validation report. To claim 0.60 substantial
#   agreement outright, either enlarge the set or accept the bound as the
#   headline number.

################################################################################
# EXAMPLE 11: Biobank Study - Available Tissue (n=500)
################################################################################
# Clinical Context: Biobank retrospective tumor grading review
# Constraint: Adequate tissue for 500 cases

kappaSizeFixedN(
  outcome = "4",
  kappa0  = 0.65,
  props   = "0.30, 0.30, 0.25, 0.15",
  raters  = "2",
  alpha   = 0.05,
  n       = 500
)

# Result: kappaL = 0.604
# - A 0.046 gap: the bound itself clears the 0.60 "substantial" threshold.
# - Verdict: this is what an adequately sized agreement study looks like. The
#   conclusion survives without leaning on the anticipated value.

################################################################################
# EXAMPLE 12: Pharmaceutical Trial - Phase III Complete (n=200, 3 raters)
################################################################################
# Clinical Context: Phase III trial endpoint assessment, 3-rater consensus
# Constraint: Trial enrolled 200 patients; design cannot change

kappaSizeFixedN(
  outcome = "3",
  kappa0  = 0.60,
  props   = "0.35, 0.40, 0.25",
  raters  = "3",
  alpha   = 0.05,
  n       = 200
)

# Result: kappaL = 0.537
# - 200 patients with 3 raters land close to 500 cases with 2 raters
#   (Example 11) - the third rater is worth a great many subjects.
# - Verdict: supports "at least moderate, approaching substantial" agreement.

################################################################################
# EXAMPLE 13: International Consortium - Multi-Center (n=800)
################################################################################
# Clinical Context: International diagnostic criteria harmonization
# Constraint: Consortium contributed 800 cases

kappaSizeFixedN(
  outcome = "3",
  kappa0  = 0.65,
  props   = "0.30, 0.45, 0.25",
  raters  = "2",
  alpha   = 0.01,         # Stringent, as befits a standard-setting study
  n       = 800
)

# Result: kappaL = 0.594
# - Even 800 cases at alpha = 0.01 leave a 0.056 gap. Precision improves with
#   the square root of n; there is no sample size that makes the gap vanish.
# - Verdict: a strong consortium-level statement at 99% confidence.

################################################################################
# EXAMPLE 14: Large Diagnostic Survey (n=1000)
################################################################################
# Clinical Context: Nationwide diagnostic concordance survey
# Constraint: Survey capacity of 1000 cases

kappaSizeFixedN(
  outcome = "2",
  kappa0  = 0.70,
  props   = "0.50, 0.50", # Perfectly balanced - the best case for precision
  raters  = "2",
  alpha   = 0.05,
  n       = 1000
)

# Result: kappaL = 0.66
# - The smallest gap in this file (0.040): large n AND balanced categories.
# - Verdict: certifies substantial agreement with the bound alone.

################################################################################
# EXAMPLE 15: How the Bound Moves With n
################################################################################
# Educational: same design, four sample sizes. kappa0 = 0.50 throughout.

result_n20 <- kappaSizeFixedN(
  outcome = "2", kappa0 = 0.50, props = "0.50, 0.50",
  raters = "2", alpha = 0.05, n = 20
)

result_n50 <- kappaSizeFixedN(
  outcome = "2", kappa0 = 0.50, props = "0.50, 0.50",
  raters = "2", alpha = 0.05, n = 50
)

result_n100 <- kappaSizeFixedN(
  outcome = "2", kappa0 = 0.50, props = "0.50, 0.50",
  raters = "2", alpha = 0.05, n = 100
)

result_n300 <- kappaSizeFixedN(
  outcome = "2", kappa0 = 0.50, props = "0.50, 0.50",
  raters = "2", alpha = 0.05, n = 300
)

#   n =  20  ->  kappaL = 0.135   (gap 0.365)
#   n =  50  ->  kappaL = 0.276   (gap 0.224)
#   n = 100  ->  kappaL = 0.345   (gap 0.155)
#   n = 300  ->  kappaL = 0.413   (gap 0.087)
#
# Teaching point: the bound always climbs toward kappa0 and never reaches it.
# Returns diminish - the first 80 subjects buy more than the next 200.

################################################################################
# BEST PRACTICES
################################################################################

# 1. When to use kappaSizeFixedN
#
#    Use it when n is already decided by budget, time, case availability,
#    a regulator, an institutional protocol, or a completed study - and you
#    need to know how precise a conclusion that n can support.
#
#    Use kappaSizePower instead when you are TESTING a hypothesis
#    (null kappa0 vs alternative kappa1, at a given power).
#    Use kappaSizeCI instead when you can choose n to reach a target
#    confidence-interval width.

# 2. Reading the result
#
#    Report kappaL, not kappa0. kappa0 is your assumption; kappaL is what the
#    study can defend. Never describe kappaL as a "detectable" kappa or as a
#    "minimum detectable difference" - no hypothesis is tested here.
#
#      gap = kappa0 - kappaL
#      small gap (<= 0.05)  -> the sample size is generous
#      moderate gap (~0.10) -> workable; report the bound as the finding
#      large gap (>= 0.20)  -> the study will not settle the question
#      kappaL <= 0          -> chance agreement cannot be excluded at all

# 3. What actually moves the bound (all verified with this function)
#
#    Sample size - the dominant factor, with diminishing returns:
#      kappa0 = 0.50, p = 0.50, 2 raters, alpha = 0.05
#      n =  20 -> 0.135 | n =  50 -> 0.276 | n = 100 -> 0.345 | n = 300 -> 0.413
#
#    Number of raters - large gains, especially from 2 to 3:
#      kappa0 = 0.60, p = 0.30, n = 60, alpha = 0.05
#      2 -> 0.389 | 3 -> 0.445 | 4 -> 0.464 | 6 -> 0.480
#
#    Balance of the categories - a rare finding is expensive:
#      kappa0 = 0.60, 2 raters, n = 100, alpha = 0.05
#      p = 0.05 -> 0.286 | 0.10 -> 0.363 | 0.30 -> 0.440 | 0.50 -> 0.453
#
#    Significance level - stringency costs precision:
#      kappa0 = 0.60, p = 0.30, 2 raters, n = 150
#      alpha = 0.10 -> 0.502 | 0.05 -> 0.472 | 0.01 -> 0.413
#
#    Number of categories - with BALANCED proportions, more categories help
#    slightly rather than hurt (contrary to the usual intuition):
#      kappa0 = 0.60, 2 raters, n = 200, alpha = 0.05, equal proportions
#      2 cats -> 0.499 | 3 -> 0.518 | 4 -> 0.525 | 5 -> 0.529
#    What hurts is IMBALANCE, not category count. Collapsing categories helps
#    only when it removes a rare one.

# 4. Sparse agreement-pattern cells
#
#    The method is a large-sample chi-square approximation over agreement
#    patterns (how many raters called the finding present, or which category
#    they all agreed on) - not over the outcome categories themselves. With a
#    rare finding or several raters, those pattern cells go sparse long before
#    the category totals do, and the Notes panel says so using Cochran's rule.
#    When it fires, treat the bound as indicative rather than exact.

# 5. Reporting template
#
#    "Case availability fixed the sample at n = 50 rare tumours, each graded
#     independently by 2 pathologists on a 3-level scale. Anticipating
#     kappa = 0.40 with grade proportions 0.30 / 0.40 / 0.30, the study was
#     expected to yield a one-sided 95% lower confidence limit of
#     kappa = 0.19 (kappaSize; Donner & Eliasziw 1992; Rotondi & Donner 2012).
#     We therefore powered the report around excluding agreement below 0.19
#     and note that a claim of moderate agreement (kappa >= 0.40) was not
#     attainable at this sample size."
#
#    Report: the constraint that fixed n, the anticipated kappa0, the category
#    proportions, the number of raters, alpha, and the resulting lower bound -
#    plus the limitation the gap implies.

################################################################################
# REFERENCES
################################################################################

# 1. Donner A, Eliasziw M (1992). A goodness-of-fit approach to inference
#    procedures for the kappa statistic: confidence interval construction,
#    significance-testing and sample size estimation.
#    Statistics in Medicine, 11(11), 1511-1519. doi:10.1002/sim.4780111109
#    -- the method this analysis implements.
#
# 2. Rotondi MA, Donner A (2012). A confidence interval approach to sample
#    size estimation for interobserver agreement studies with multiple raters
#    and outcomes. Journal of Clinical Epidemiology, 65(7), 778-784.
#    doi:10.1016/j.jclinepi.2011.10.019
#    -- the multiple-rater, multiple-category extension used here.
#
# 3. Rotondi MA (2018). kappaSize: Sample Size Estimation Functions for
#    Studies of Interobserver Agreement. R package.
#
# 4. Landis JR, Koch GG (1977). The measurement of observer agreement for
#    categorical data. Biometrics, 33(1), 159-174.
#    -- the source of the 0.20/0.40/0.60/0.80 interpretive bands.
#
# 5. Walter SD, Eliasziw M, Donner A (1998). Sample size and optimal designs
#    for reliability studies. Statistics in Medicine, 17(1), 101-110.

################################################################################
# END OF EXAMPLES
################################################################################
