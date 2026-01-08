# ═══════════════════════════════════════════════════════════
# Single Arm Survival Examples
# ═══════════════════════════════════════════════════════════
library(ClinicoPath)

# ═══ EXAMPLE 1: Basic Overall Survival ═══
data(singlearm_test)
singlearm(
  data = singlearm_test,
  elapsedtime = "time_months",
  outcome = "outcome",
  outcomeLevel = "Dead",
  analysistype = "overall",
  sc = TRUE
)

# ═══ EXAMPLE 2: Date-Based Time Calculation ═══
data(singlearm_dates)
singlearm(
  data = singlearm_dates,
  tint = TRUE,
  dxdate = "diagnosis_date",
  fudate = "followup_date",
  outcome = "outcome",
  outcomeLevel = "Dead",
  timetypedata = "ymd",
  timetypeoutput = "months",
  cutp = "12, 24, 36, 48, 60"
)

# ═══ EXAMPLE 3: Competing Risks Analysis ═══
data(singlearm_compete)
singlearm(
  data = singlearm_compete,
  elapsedtime = "time_months",
  outcome = "outcome",
  analysistype = "compete",
  dod = "Dead_Disease",
  dooc = "Dead_Other",
  awd = "Alive_Disease",
  awod = "Alive_NED",
  sc = TRUE,
  ci95 = TRUE
)

# ═══ EXAMPLE 4: Cause-Specific Survival ═══
data(singlearm_causespecific)
singlearm(
  data = singlearm_causespecific,
  elapsedtime = "time_months",
  outcome = "outcome",
  analysistype = "cause",
  dod = "Dead_Cancer",
  sc = TRUE,
  risktable = TRUE
)

# ═══ EXAMPLE 5: Landmark Analysis ═══
data(singlearm_landmark)
singlearm(
  data = singlearm_landmark,
  elapsedtime = "time_months",
  outcome = "outcome",
  outcomeLevel = "Dead",
  uselandmark = TRUE,
  landmark = 6,
  sc = TRUE,
  censored = TRUE
)

# ═══ EXAMPLE 6: KMunicate-Style Plot ═══
data(singlearm_test)
singlearm(
  data = singlearm_test,
  elapsedtime = "time_months",
  outcome = "outcome",
  outcomeLevel = "Dead",
  kmunicate = TRUE,
  risktable = TRUE
)

# ═══ EXAMPLE 7: Person-Time Metrics ═══
data(singlearm_persontime)
singlearm(
  data = singlearm_persontime,
  elapsedtime = "time_months",
  outcome = "outcome",
  outcomeLevel = "Dead",
  person_time = TRUE,
  time_intervals = "12, 36, 60",
  rate_multiplier = 100
)

# ═══ EXAMPLE 8: Cumulative Events and Hazards ═══
data(singlearm_test)
singlearm(
  data = singlearm_test,
  elapsedtime = "time_months",
  outcome = "outcome",
  outcomeLevel = "Dead",
  ce = TRUE,
  ch = TRUE,
  endplot = 60
)

# ═══ EXAMPLE 9: Baseline Hazard Analysis ═══
data(singlearm_test)
singlearm(
  data = singlearm_test,
  elapsedtime = "time_months",
  outcome = "outcome",
  outcomeLevel = "Dead",
  baseline_hazard = TRUE,
  hazard_smoothing = TRUE
)

# ═══ EXAMPLE 10: Complete Analysis with All Options ═══
data(singlearm_test)
singlearm(
  data = singlearm_test,
  elapsedtime = "time_months",
  outcome = "outcome",
  outcomeLevel = "Dead",
  analysistype = "overall",
  cutp = "6, 12, 24, 36, 48, 60",
  sc = TRUE,
  kmunicate = TRUE,
  ce = TRUE,
  ch = TRUE,
  ci95 = TRUE,
  risktable = TRUE,
  censored = TRUE,
  medianline = "hv",
  person_time = TRUE,
  baseline_hazard = TRUE,
  showExplanations = TRUE,
  showSummaries = TRUE
)
