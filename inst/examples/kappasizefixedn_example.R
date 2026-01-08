################################################################################
# KAPPASIZEFIXEDN FUNCTION - COMPREHENSIVE USAGE EXAMPLES
################################################################################
#
# Function: kappaSizeFixedN
# Purpose: Determine lowest detectable kappa value given a FIXED sample size
# Approach: Reverse calculation - you have predetermined n, what kappa can you detect?
#
# Key Concept: This function answers "Given that I can only get N subjects
# (due to budget, time, or availability constraints), what is the minimum
# level of agreement I can reliably detect or estimate?"
#
################################################################################

# Load the ClinicoPath package
library(ClinicoPath)

################################################################################
# EXAMPLE 1: Small Pilot Study - Resource Constrained (n=30)
################################################################################
# Clinical Context: Dermatology resident training evaluation
# Constraint: Limited budget allows only 30 melanoma cases
# Research Question: What minimum agreement level can we detect with n=30?
# Justification: Pilot study to determine if full validation feasible

kappaSizeFixedN(
  outcome = "2",           # Binary: Melanoma vs benign
  kappa0 = 0.40,          # Null hypothesis: fair agreement
  props = "0.10, 0.90",   # 10% melanoma prevalence
  raters = "2",           # Two dermatology residents
  alpha = 0.05,           # 95% confidence
  n = 30                  # FIXED: Only 30 cases available/affordable
)

# Clinical Interpretation:
# - Result shows LOWEST kappa detectable with n=30
# - If result is (e.g.) κ=0.60, then with 30 cases you can detect
#   whether agreement is at least κ=0.60 vs the null of κ=0.40
# - Use to assess feasibility: Is this detectable difference clinically meaningful?
# - Budget Decision: If detection capability insufficient, consider increasing budget

################################################################################
# EXAMPLE 2: Emergency Department Feasibility - Time Constrained (n=40)
################################################################################
# Clinical Context: Emergency stroke CT protocol validation
# Constraint: Can review only 40 stroke CTs in 2-month timeframe
# Research Question: What agreement level detectable in 2 months?
# Justification: Time pressure for protocol implementation

kappaSizeFixedN(
  outcome = "2",           # Binary: Stroke present vs absent
  kappa0 = 0.50,          # Null hypothesis: moderate agreement
  props = "0.15, 0.85",   # 15% stroke prevalence in ED
  raters = "2",           # Two emergency radiologists
  alpha = 0.05,           # 95% confidence
  n = 40                  # FIXED: 2-month capacity = 40 cases
)

# Clinical Interpretation:
# - Time Constraint: ED can only process 40 cases in required timeframe
# - Result indicates minimum detectable improvement over κ=0.50
# - Decision Point: Is detectable difference adequate for protocol validation?
# - Alternative: If insufficient, consider extending timeline or pilot approach

################################################################################
# EXAMPLE 3: Rare Pathology - Case Availability Constrained (n=35)
################################################################################
# Clinical Context: Rare tumor grading agreement
# Constraint: Only 35 cases of rare tumor available per year
# Research Question: What 3-level grading agreement can we assess?
# Justification: Rare disease limits case availability

kappaSizeFixedN(
  outcome = "3",                     # Three grades: low/moderate/high
  kappa0 = 0.40,                    # Null hypothesis: fair agreement
  props = "0.30, 0.40, 0.30",       # Grade distribution
  raters = "2",                     # Two pathologists
  alpha = 0.05,                     # 95% confidence
  n = 35                            # FIXED: Annual case availability
)

# Clinical Interpretation:
# - Rare Disease: Only 35 cases available annually at referral center
# - Three Categories: More complex than binary, reduces detection power
# - Result shows minimum detectable 3-level grading agreement
# - Consider: Multi-center collaboration if single-center insufficient

################################################################################
# EXAMPLE 4: Training Program - Educational Budget (n=50)
################################################################################
# Clinical Context: Three pathology trainees post-training assessment
# Constraint: Training budget allows 50 cases for competency evaluation
# Research Question: What agreement detectable with 3 trainees, 50 cases?
# Justification: Educational resource limitations

kappaSizeFixedN(
  outcome = "2",           # Binary: Adequate vs inadequate specimen
  kappa0 = 0.50,          # Null hypothesis: moderate agreement
  props = "0.25, 0.75",   # 25% inadequate specimens
  raters = "3",           # Three pathology trainees
  alpha = 0.05,           # 95% confidence
  n = 50                  # FIXED: Training budget = 50 cases
)

# Clinical Interpretation:
# - Three Raters: Multiple trainees provide more information per case
# - Educational Budget: 50 cases represents available training resources
# - Result shows competency threshold detectable with available resources
# - Practical: More raters (3) helps compensate for limited sample size

################################################################################
# EXAMPLE 5: Standard QA Program - Annual Protocol (n=100)
################################################################################
# Clinical Context: Annual mammography quality assurance
# Constraint: Standard QA protocol specifies 100 cases per radiologist pair
# Research Question: What BIRADS agreement detectable with standard n=100?
# Justification: Institutional QA protocol requirement

kappaSizeFixedN(
  outcome = "2",           # Binary: Suspicious vs benign
  kappa0 = 0.50,          # Null hypothesis: moderate agreement
  props = "0.20, 0.80",   # 20% suspicious findings
  raters = "2",           # Two breast radiologists
  alpha = 0.05,           # 95% confidence
  n = 100                 # FIXED: Standard annual QA sample
)

# Clinical Interpretation:
# - Standard Protocol: Institutional policy fixes n=100 annually
# - Result indicates QA sensitivity: What disagreement can be detected?
# - Quality Monitoring: Determines if protocol adequate for QA goals
# - Policy Decision: If insufficient, propose protocol modification

################################################################################
# EXAMPLE 6: Accreditation Requirement - Regulatory Minimum (n=120)
################################################################################
# Clinical Context: Pathology accreditation tumor grading validation
# Constraint: Accreditation body requires minimum 120 cases
# Research Question: What 4-level grading agreement detectable at minimum n?
# Justification: Regulatory compliance requirement

kappaSizeFixedN(
  outcome = "4",                          # Four grades
  kappa0 = 0.50,                         # Null hypothesis
  props = "0.25, 0.30, 0.30, 0.15",      # Grade distribution
  raters = "2",                          # Two pathologists
  alpha = 0.05,                          # 95% confidence
  n = 120                                # FIXED: Regulatory minimum
)

# Clinical Interpretation:
# - Regulatory Minimum: Must meet n=120 for accreditation
# - Four Categories: Complex grading system
# - Result shows whether minimum n adequate for validation goals
# - Accreditation Decision: Exceed minimum if detection insufficient

################################################################################
# EXAMPLE 7: Clinical Trial Endpoint - Enrollment Complete (n=150)
################################################################################
# Clinical Context: Biomarker agreement in completed clinical trial
# Constraint: Trial enrolled 150 patients (enrollment closed)
# Research Question: What biomarker scoring agreement detectable post-hoc?
# Justification: Trial complete, retrospective agreement assessment

kappaSizeFixedN(
  outcome = "2",           # Binary: Biomarker positive vs negative
  kappa0 = 0.60,          # Null hypothesis: good agreement
  props = "0.35, 0.65",   # 35% positive biomarker
  raters = "2",           # Central vs local pathologist
  alpha = 0.05,           # 95% confidence
  n = 150                 # FIXED: Trial enrollment complete
)

# Clinical Interpretation:
# - Trial Complete: Cannot increase n, enrollment closed
# - Post-hoc Assessment: Determine if existing n adequate for agreement
# - Result informs whether central review agreement assessment valid
# - Publication: Result critical for reporting central review validity

################################################################################
# EXAMPLE 8: Cancer Registry - Annual Capacity (n=250)
################################################################################
# Clinical Context: Cancer registry staging validation
# Constraint: Registry has capacity to validate 250 cases per year
# Research Question: What 5-stage TNM agreement detectable with capacity?
# Justification: Operational resource limitation

kappaSizeFixedN(
  outcome = "5",                                # Five TNM stages
  kappa0 = 0.60,                               # Null hypothesis
  props = "0.20, 0.25, 0.25, 0.20, 0.10",      # Stage distribution
  raters = "2",                                # Two oncologists
  alpha = 0.05,                                # 95% confidence
  n = 250                                      # FIXED: Annual registry capacity
)

# Clinical Interpretation:
# - Five Categories: Maximum complexity (stages 0-IV)
# - Annual Capacity: Registry staff can validate 250 cases/year
# - Result shows validation capability with available resources
# - Resource Planning: Determine if capacity expansion needed

################################################################################
# EXAMPLE 9: National Screening Program - QA Sample (n=300, Stringent α)
################################################################################
# Clinical Context: National mammography screening program QA
# Constraint: National program specifies 300-case annual QA sample
# Research Question: What agreement detectable with stringent confidence?
# Justification: Public health program requires rigorous standards

kappaSizeFixedN(
  outcome = "2",           # Binary: Recall vs routine
  kappa0 = 0.70,          # Null hypothesis: good agreement
  props = "0.25, 0.75",   # 25% recall rate
  raters = "2",           # Two screening radiologists
  alpha = 0.01,           # 99% confidence (STRINGENT)
  n = 300                 # FIXED: National program QA sample
)

# Clinical Interpretation:
# - Stringent Alpha: Public health program requires 99% confidence
# - Large Fixed Sample: n=300 reflects national program resources
# - Result shows detection capability at high confidence level
# - Policy Impact: Informs national screening quality standards

################################################################################
# EXAMPLE 10: AI Validation Study - Budget Constraint (n=400)
################################################################################
# Clinical Context: AI diagnostic algorithm validation
# Constraint: Budget allows expert labeling of 400 images
# Research Question: What AI-human agreement detectable with budget?
# Justification: Expert labeling expensive, limits sample size

kappaSizeFixedN(
  outcome = "2",           # Binary: AI vs human classification
  kappa0 = 0.60,          # Null hypothesis: moderate-good agreement
  props = "0.30, 0.70",   # 30% positive findings
  raters = "2",           # AI algorithm vs radiologist
  alpha = 0.05,           # 95% confidence
  n = 400                 # FIXED: Expert labeling budget
)

# Clinical Interpretation:
# - Budget Constraint: Expert labeling costs limit n to 400
# - Validation Goal: Determine if AI-human agreement adequate
# - Result shows minimum detectable AI performance improvement
# - Investment Decision: If insufficient, justify additional budget

################################################################################
# EXAMPLE 11: Biobank Study - Available Tissue (n=500)
################################################################################
# Clinical Context: Biobank retrospective tumor grading review
# Constraint: Biobank has adequate tissue for 500 cases
# Research Question: What 4-grade agreement detectable with biobank?
# Justification: Tissue availability constraint

kappaSizeFixedN(
  outcome = "4",                          # Four tumor grades
  kappa0 = 0.65,                         # Null hypothesis
  props = "0.30, 0.30, 0.25, 0.15",      # Grade distribution
  raters = "2",                          # Two pathologists
  alpha = 0.05,                          # 95% confidence
  n = 500                                # FIXED: Available tissue samples
)

# Clinical Interpretation:
# - Biobank Resource: 500 cases with adequate tissue for review
# - Retrospective Study: Cannot collect more samples, n fixed
# - Large Sample: n=500 provides good detection capability
# - Result validates whether biobank resource sufficient for goals

################################################################################
# EXAMPLE 12: Pharmaceutical Trial - Phase III Complete (n=200, 3 Raters)
################################################################################
# Clinical Context: Phase III trial endpoint assessment complete
# Constraint: Trial enrolled 200 patients (cannot change)
# Research Question: What endpoint agreement detectable with 3 raters?
# Justification: Trial design specified 3-rater consensus, n fixed

kappaSizeFixedN(
  outcome = "3",                     # Three endpoint levels
  kappa0 = 0.60,                    # Null hypothesis
  props = "0.35, 0.40, 0.25",       # Endpoint distribution
  raters = "3",                     # Three independent assessors
  alpha = 0.05,                     # 95% confidence
  n = 200                           # FIXED: Trial enrollment
)

# Clinical Interpretation:
# - Three Raters: Trial design specified multiple assessors
# - More Raters Help: 3 raters provide more information per subject
# - Trial Complete: n=200 cannot be changed post-hoc
# - Regulatory Submission: Result supports endpoint reliability claim

################################################################################
# EXAMPLE 13: International Consortium - Multi-Center (n=800)
################################################################################
# Clinical Context: 8 centers each contribute 100 cases (n=800 total)
# Constraint: Each center commits to 100 cases, total n fixed
# Research Question: What 3-level agreement detectable consortium-wide?
# Justification: Multi-center collaboration with predetermined contributions

kappaSizeFixedN(
  outcome = "3",                     # Three severity levels
  kappa0 = 0.65,                    # Null hypothesis: good agreement
  props = "0.30, 0.45, 0.25",       # Severity distribution
  raters = "2",                     # Two pathologists
  alpha = 0.01,                     # 99% confidence (stringent for consortium)
  n = 800                           # FIXED: 8 centers × 100 cases each
)

# Clinical Interpretation:
# - Multi-Center: International collaboration with 8 participating sites
# - Large Sample: n=800 provides substantial detection power
# - Stringent Alpha: Consortium standards require 99% confidence
# - Collaboration: Result validates adequacy of center commitments

################################################################################
# EXAMPLE 14: National Health Survey - Survey Design (n=1000)
################################################################################
# Clinical Context: National health survey with predetermined sample
# Constraint: Survey design specifies n=1000 participants
# Research Question: What diagnostic agreement detectable in large survey?
# Justification: Survey methodology fixes sample size a priori

kappaSizeFixedN(
  outcome = "2",           # Binary: Disease present vs absent
  kappa0 = 0.70,          # Null hypothesis: good agreement
  props = "0.50, 0.50",   # Balanced prevalence
  raters = "2",           # Two survey physicians
  alpha = 0.05,           # 95% confidence
  n = 1000                # FIXED: Survey design specification
)

# Clinical Interpretation:
# - Large National Survey: n=1000 predetermined by survey design
# - High Detection Power: Large sample detects small differences
# - Balanced Prevalence: Optimal efficiency
# - Survey Quality: Result validates survey diagnostic methodology

################################################################################
# EXAMPLE 15: Small Sample Comparison - Understanding n Impact
################################################################################
# Clinical Context: Comparing detection capability across sample sizes
# Educational Purpose: Demonstrate how fixed n affects detectable kappa

# Very Small Sample (n=20)
result_n20 <- kappaSizeFixedN(
  outcome = "2", kappa0 = 0.50, props = "0.50, 0.50",
  raters = "2", alpha = 0.05, n = 20
)

# Small Sample (n=50)
result_n50 <- kappaSizeFixedN(
  outcome = "2", kappa0 = 0.50, props = "0.50, 0.50",
  raters = "2", alpha = 0.05, n = 50
)

# Moderate Sample (n=100)
result_n100 <- kappaSizeFixedN(
  outcome = "2", kappa0 = 0.50, props = "0.50, 0.50",
  raters = "2", alpha = 0.05, n = 100
)

# Large Sample (n=300)
result_n300 <- kappaSizeFixedN(
  outcome = "2", kappa0 = 0.50, props = "0.50, 0.50",
  raters = "2", alpha = 0.05, n = 300
)

# Educational Interpretation:
# - Compare Results: See how detectable kappa improves with larger n
# - n=20: Very limited detection, only large differences
# - n=50: Moderate detection capability
# - n=100: Good detection for most purposes
# - n=300: High detection, can find small differences
# - Teaching Point: Larger n allows detecting smaller kappa values

################################################################################
# BEST PRACTICES: FIXED SAMPLE SIZE CONSIDERATIONS
################################################################################

# 1. When to Use kappaSizeFixedN:
#
#    Use when sample size is PREDETERMINED by:
#    - Budget constraints (limited funding for cases/raters)
#    - Time constraints (must complete in fixed timeframe)
#    - Case availability (rare diseases, limited cases)
#    - Regulatory requirements (minimum n specified)
#    - Completed studies (retrospective power assessment)
#    - Institutional protocols (QA programs with fixed n)
#    - Resource capacity (staffing, equipment limitations)
#
#    DO NOT use when:
#    - You can determine sample size freely
#    - Use kappaSizePower for power-based planning
#    - Use kappaSizeCI for precision-based planning

# 2. Understanding the Result:
#
#    The function returns the LOWEST kappa value detectable/estimable
#
#    Example: If result is κ=0.65 with kappa0=0.50:
#    - With your fixed n, you can detect whether agreement reaches κ=0.65
#    - Differences smaller than (0.65 - 0.50) = 0.15 may not be detectable
#    - Clinical Question: Is detecting κ≥0.65 adequate for your needs?
#
#    Interpretation Framework:
#    - Result much higher than kappa0 → Limited detection, consider more cases
#    - Result moderately above kappa0 → Reasonable detection capability
#    - Result slightly above kappa0 → Good detection, adequate sample

# 3. Sample Size Constraints Assessment:
#
#    Pilot Studies (n=20-50):
#    - Limited precision, only detect large effects
#    - Use for feasibility, preliminary assessment
#    - Plan larger study if promising results
#
#    Small Studies (n=50-100):
#    - Moderate detection capability
#    - Adequate for many QA applications
#    - Consider if differences of interest are moderate-large
#
#    Standard Studies (n=100-250):
#    - Good detection for most purposes
#    - Typical for clinical validation studies
#    - Balance of feasibility and precision
#
#    Large Studies (n=250-500):
#    - High detection capability
#    - Can detect smaller differences
#    - Justified for important clinical decisions
#
#    Very Large Studies (n>500):
#    - Maximum detection sensitivity
#    - Appropriate for national programs, registries
#    - Diminishing returns beyond certain point

# 4. Parameter Impact on Detection:
#
#    Factors that IMPROVE detection (allow detecting lower kappa):
#    - Larger n (most important factor)
#    - More raters (provides more information per subject)
#    - Fewer categories (binary simplest)
#    - Balanced proportions (50-50 optimal)
#    - Liberal alpha (90% vs 99% confidence)
#
#    Factors that REDUCE detection (require higher kappa):
#    - Smaller n (fundamental constraint)
#    - Fewer raters (standard 2 raters)
#    - Many categories (5 categories most complex)
#    - Imbalanced proportions (rare events especially)
#    - Stringent alpha (99% vs 95% confidence)

# 5. Decision Framework After Results:
#
#    Adequate Detection:
#    - Lowest detectable kappa is clinically meaningful
#    - Proceed with study using fixed n
#    - Results will be interpretable and useful
#
#    Marginal Detection:
#    - Detectable kappa borderline for clinical needs
#    - Consider: Can you increase n slightly?
#    - Or: Accept limitations, proceed with caution
#    - Report limitations transparently
#
#    Inadequate Detection:
#    - Detectable kappa too high for study goals
#    - Options:
#       a) Increase n if possible (seek more resources)
#       b) Add more raters (if feasible and cheaper than more subjects)
#       c) Simplify categories (if clinically appropriate)
#       d) Use liberal alpha (if acceptable)
#       e) Reconsider study feasibility
#
#    Cannot Change n:
#    - Report detectable kappa in methods/limitations
#    - Frame results appropriately
#    - Acknowledge what differences can/cannot be detected
#    - Consider pilot for larger future study

# 6. Comparing to Other Kappa Sample Size Functions:
#
#    kappaSizeFixedN (this function):
#    - Input: Fixed n
#    - Output: Lowest detectable kappa
#    - Use: When n predetermined by constraints
#    - Question: "What can I detect with my available n?"
#
#    kappaSizePower:
#    - Input: Desired kappa0, kappa1, power
#    - Output: Required n
#    - Use: When testing hypothesis (improvement)
#    - Question: "How many subjects needed to detect improvement?"
#
#    kappaSizeCI:
#    - Input: Desired CI width
#    - Output: Required n
#    - Use: When estimating with precision
#    - Question: "How many subjects for precise estimate?"
#
#    Choose Based On:
#    - n fixed → Use kappaSizeFixedN
#    - Testing hypothesis → Use kappaSizePower
#    - Estimating precisely → Use kappaSizeCI

# 7. Reporting Requirements:
#
#    Always report in methods:
#    - Sample size and how it was determined/constrained
#    - Null hypothesis kappa (kappa0)
#    - All input parameters
#    - Resulting lowest detectable kappa
#    - Clinical interpretation of detection capability
#    - Any limitations due to fixed n
#
#    Example methods text:
#    "Due to limited case availability, sample size was fixed at n=50
#    rare tumor cases. With this sample size, 2 raters, binary grading,
#    and assuming κ0=0.40, we could detect agreement of κ=0.65 or higher
#    (α=0.05). This detection capability was deemed adequate for
#    preliminary validation of the grading system."

# 8. Multi-Rater Advantage:
#
#    When n is constrained, adding raters can help:
#    - More raters → More information per subject
#    - May improve detectable kappa
#    - Trade-off: Coordination complexity, cost
#
#    Example Comparison:
#    - 2 raters, n=60: Detects κ=X
#    - 3 raters, n=60: Detects κ=X-0.05 (better)
#
#    Consider when:
#    - Cannot increase n
#    - Detection inadequate with 2 raters
#    - Rater availability not limiting factor
#    - Benefit justifies coordination complexity

# 9. Category Simplification:
#
#    If detection inadequate and n fixed:
#    - Consider collapsing categories
#    - Example: 4 tumor grades → 2 categories (low vs high)
#    - Simpler classification improves detection
#    - Must be clinically justifiable
#    - Report in methods with rationale

# 10. Practical Workflow:
#
#     Step 1: Determine constraint
#     - Identify what fixes your sample size
#     - Document constraint clearly
#
#     Step 2: Run kappaSizeFixedN
#     - Input: Your fixed n and parameters
#     - Output: Lowest detectable kappa
#
#     Step 3: Evaluate result
#     - Is detectable kappa clinically meaningful?
#     - Compare to your study goals
#
#     Step 4: Make decision
#     - Adequate: Proceed with study
#     - Inadequate: Explore alternatives
#       * Increase n if possible
#       * Add raters
#       * Simplify categories
#       * Reconsider feasibility
#
#     Step 5: Report transparently
#     - Document constraint and decision
#     - Report detection capability
#     - Acknowledge limitations

################################################################################
# REFERENCES
################################################################################

# 1. Cantor AB (1996). Sample-size calculations for Cohen's kappa.
#    Psychological Methods, 1(2), 150-153.
#
# 2. Donner A, Eliasziw M (1992). A goodness-of-fit approach to inference
#    procedures for the kappa statistic. Statistics in Medicine, 11(11),
#    1511-1519.
#
# 3. Flack VF, Afifi AA, Lachenbruch PA, Schouten HJA (1988). Sample size
#    determinations for the two rater kappa statistic. Psychometrika, 53(3),
#    321-325.
#
# 4. Walter SD, Eliasziw M, Donner A (1998). Sample size and optimal designs
#    for reliability studies. Statistics in Medicine, 17(1), 101-110.
#
# 5. Shoukri MM, Asyali MH, Donner A (2004). Sample size requirements for the
#    design of reliability study: review and new results. Statistical Methods
#    in Medical Research, 13(4), 251-271.

################################################################################
# END OF EXAMPLES
################################################################################
