# ═══════════════════════════════════════════════════════════
# Outcome Organizer Examples
# ═══════════════════════════════════════════════════════════
library(ClinicoPath)

# ═══ EXAMPLE 1: Overall Survival (OS) ═══
data(outcomeorganizer_os)
outcomeorganizer(
  data = outcomeorganizer_os,
  outcome = "vital_status",
  time = "time_months",
  dead = "Dead",
  alive = "Alive",
  outputTable = TRUE
)

# ═══ EXAMPLE 2: Competing Risks ═══
data(outcomeorganizer_compete)
outcomeorganizer(
  data = outcomeorganizer_compete,
  outcome = "outcome_status",
  time = "time",
  diagnostics = TRUE
)

# ═══ EXAMPLE 3: Progression-Free Survival (PFS) ═══
data(outcomeorganizer_pfs)
outcomeorganizer(
  data = outcomeorganizer_pfs,
  outcome = "progression",
  time = "time_months",
  outputTable = TRUE,
  diagnostics = TRUE
)

# ═══ EXAMPLE 4: Recurrence-Free Survival (RFS) ═══
data(outcomeorganizer_rfs)
outcomeorganizer(
  data = outcomeorganizer_rfs,
  outcome = "recurrence",
  time = "fu_time"
)

# ═══ EXAMPLE 5: Cause-Specific Survival ═══
data(outcomeorganizer_causespecific)
outcomeorganizer(
  data = outcomeorganizer_causespecific,
  outcome = "death_status",
  time = "time",
  outputTable = TRUE
)

# ═══ EXAMPLE 6: Multistate Models ═══
data(outcomeorganizer_multistate)
outcomeorganizer(
  data = outcomeorganizer_multistate,
  outcome = "current_state",
  time = "time",
  diagnostics = TRUE
)

# ═══ EXAMPLE 7: Disease-Free Survival (DFS) ═══
data(outcomeorganizer_dfs)
outcomeorganizer(
  data = outcomeorganizer_dfs,
  outcome = "status",
  time = "time_years",
  outputTable = TRUE
)
