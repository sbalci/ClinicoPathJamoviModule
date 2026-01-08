# ═══════════════════════════════════════════════════════════
# Odds Ratio Examples
# ═══════════════════════════════════════════════════════════
library(ClinicoPath)

# ═══ EXAMPLE 1: Basic Odds Ratio ═══
data(oddsratio_test)
oddsratio(
  data = oddsratio_test,
  explanatory = "stage",
  outcome = "outcome",
  outcomeLevel = "Dead"
)

# ═══ EXAMPLE 2: Multiple Predictors ═══
oddsratio(
  data = oddsratio_test,
  explanatory = c("stage", "treatment", "biomarker_status"),
  outcome = "outcome",
  outcomeLevel = "Dead"
)

# ═══ EXAMPLE 3: Diagnostic Test Analysis ═══
data(oddsratio_diagnostic)
oddsratio(
  data = oddsratio_diagnostic,
  explanatory = "test_result",
  outcome = "disease_status",
  outcomeLevel = "Diseased",
  showNomogram = TRUE
)

# ═══ EXAMPLE 4: Case-Control Study ═══
data(oddsratio_casecontrol)
oddsratio(
  data = oddsratio_casecontrol,
  explanatory = c("exposure", "age_group", "smoking"),
  outcome = "case_status",
  outcomeLevel = "Case"
)

# ═══ EXAMPLE 5: Continuous and Categorical ═══
oddsratio(
  data = oddsratio_test,
  explanatory = c("age", "tumor_size", "stage"),
  outcome = "outcome",
  showExplanations = TRUE
)
