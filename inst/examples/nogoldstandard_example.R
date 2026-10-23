# ═══════════════════════════════════════════════════════════
# Analysis Without Gold Standard Examples
# ═══════════════════════════════════════════════════════════
library(ClinicoPath)

# The R wrapper requires every positive-level argument; use NULL for unused tests.
# Latent methods need at least three tests and assume conditional independence.
# The high-positive class is a naming convention, not an established disease class.

# ═══ EXAMPLE 1: Basic Two-Test Analysis ═══
data(nogoldstandard_test)
nogoldstandard(
  data = nogoldstandard_test,
  test1 = "Test1",
  test1Positive = "Positive",
  test2 = "Test2",
  test2Positive = "Positive",
  test3Positive = NULL,
  test4Positive = NULL,
  test5Positive = NULL,
  method = "all_positive"
)

# ═══ EXAMPLE 2: Latent Class Analysis (Three Tests) ═══
data(nogoldstandard_validation)
nogoldstandard(
  data = nogoldstandard_validation,
  test1 = "New_Test",
  test1Positive = "Positive",
  test2 = "Reference1",
  test2Positive = "Positive",
  test3 = "Reference2",
  test3Positive = "Positive",
  test4Positive = NULL,
  test5Positive = NULL,
  method = "latent_class"
)

# ═══ EXAMPLE 3: Saved GUI Teaching Configuration ═══
# Simulated diagnostic tests, not reader agreement. Neither reference test is perfect.
# The configured .omv lives in meddecide/data/nogoldstandard_validation.omv.
data(nogoldstandard_validation)
nogoldstandard(
  data = nogoldstandard_validation,
  test1 = "New_Test", test1Positive = "Positive",
  test2 = "Reference1", test2Positive = "Positive",
  test3 = "Reference2", test3Positive = "Positive",
  test4Positive = NULL, test5Positive = NULL,
  method = "latent_class", bootstrap = TRUE, nboot = 1000, alpha = 0.05,
  seed = 42, verbose = TRUE, showSummary = TRUE
)

# ═══ EXAMPLE 4: Tumor Marker Evaluation (Four Tests) ═══
data(nogoldstandard_tumormarker)
nogoldstandard(
  data = nogoldstandard_tumormarker,
  test1 = "CA125",
  test1Positive = "Elevated",
  test2 = "HE4",
  test2Positive = "Elevated",
  test3 = "CEA",
  test3Positive = "Elevated",
  test4 = "AFP",
  test4Positive = "Elevated",
  test5Positive = NULL,
  method = "composite",
  clinicalPreset = "tumor_markers"
)

# ═══ EXAMPLE 5: Cancer Screening (Five Tests) ═══
data(nogoldstandard_screening)
nogoldstandard(
  data = nogoldstandard_screening,
  test1 = "Imaging",
  test1Positive = "Abnormal",
  test2 = "ClinicalExam",
  test2Positive = "Abnormal",
  test3 = "Biomarker",
  test3Positive = "Abnormal",
  test4 = "Questionnaire",
  test4Positive = "Positive",
  test5 = "AI_Algorithm",
  test5Positive = "Positive",
  method = "all_positive",
  clinicalPreset = "screening_evaluation"
)

# ═══ EXAMPLE 6: Composite Reference Standard ═══
data(nogoldstandard_test)
nogoldstandard(
  data = nogoldstandard_test,
  test1 = "Test1",
  test1Positive = "Positive",
  test2 = "Test2",
  test2Positive = "Positive",
  test3Positive = NULL,
  test4Positive = NULL,
  test5Positive = NULL,
  method = "composite"
)

# ═══ EXAMPLE 7: Any Positive Rule (Descriptive, Not Diagnostic Accuracy) ═══
data(nogoldstandard_validation)
nogoldstandard(
  data = nogoldstandard_validation,
  test1 = "New_Test", test1Positive = "Positive",
  test2 = "Reference1", test2Positive = "Positive",
  test3 = "Reference2", test3Positive = "Positive",
  test4Positive = NULL, test5Positive = NULL,
  method = "any_positive"
)

# ═══ EXAMPLE 8: Penalized EM (Fixed-Prior MAP) ═══
data(nogoldstandard_validation)
nogoldstandard(
  data = nogoldstandard_validation,
  test1 = "New_Test",
  test1Positive = "Positive",
  test2 = "Reference1",
  test2Positive = "Positive",
  test3 = "Reference2",
  test3Positive = "Positive",
  test4Positive = NULL,
  test5Positive = NULL,
  method = "bayesian"
)

# ═══ EXAMPLE 9: Latent Class with Bootstrap CI ═══
data(nogoldstandard_validation)
nogoldstandard(
  data = nogoldstandard_validation,
  test1 = "New_Test",
  test1Positive = "Positive",
  test2 = "Reference1",
  test2Positive = "Positive",
  test3 = "Reference2",
  test3Positive = "Positive",
  test4Positive = NULL,
  test5Positive = NULL,
  method = "latent_class",
  bootstrap = TRUE,
  nboot = 500,
  verbose = TRUE
)

# ═══ EXAMPLE 10: Diagnostic Test Validation ═══
data(nogoldstandard_validation)
nogoldstandard(
  data = nogoldstandard_validation,
  test1 = "New_Test",
  test1Positive = "Positive",
  test2 = "Reference1",
  test2Positive = "Positive",
  test3 = "Reference2",
  test3Positive = "Positive",
  test4Positive = NULL,
  test5Positive = NULL,
  method = "latent_class",
  clinicalPreset = "diagnostic_validation",
  bootstrap = TRUE,
  nboot = 1000,
  alpha = 0.05
)
