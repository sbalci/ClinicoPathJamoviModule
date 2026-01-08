# ═══════════════════════════════════════════════════════════
# Example Usage: jjscatterstats
# ═══════════════════════════════════════════════════════════
#
# Comprehensive examples for the jjscatterstats jamovi function
# Generated: 2026-01-06

library(ClinicoPath)

# Load test datasets
data(jjscatterstats_test)
data(jjscatterstats_clinical)
data(jjscatterstats_treatment)
data(jjscatterstats_expression)
data(jjscatterstats_survival)

# ═══════════════════════════════════════════════════════════
# BASIC SCATTER PLOTS
# ═══════════════════════════════════════════════════════════

# Example 1: Simple scatter plot with correlation
jjscatterstats(
  data = jjscatterstats_test,
  dep = "ki67_index",
  group = "tumor_size"
)

# Example 2: With statistical results subtitle
jjscatterstats(
  data = jjscatterstats_test,
  dep = "ki67_index",
  group = "tumor_size",
  typestatistics = "parametric",
  resultssubtitle = TRUE
)

# Example 3: With custom titles
jjscatterstats(
  data = jjscatterstats_test,
  dep = "ki67_index",
  group = "tumor_size",
  mytitle = "Ki67 Index vs Tumor Size",
  xtitle = "Ki67 Proliferation Index (%)",
  ytitle = "Tumor Size (mm)"
)

# ═══════════════════════════════════════════════════════════
# MARGINAL DISTRIBUTIONS
# ═══════════════════════════════════════════════════════════

# Example 4: With histogram marginals
jjscatterstats(
  data = jjscatterstats_clinical,
  dep = "glucose",
  group = "hemoglobin_a1c",
  marginalType = "histogram"
)

# Example 5: With density marginals
jjscatterstats(
  data = jjscatterstats_test,
  dep = "protein_expression",
  group = "mutation_burden",
  marginalType = "density"
)

# Example 6: With boxplot marginals
jjscatterstats(
  data = jjscatterstats_clinical,
  dep = "bmi",
  group = "systolic_bp",
  marginalType = "boxplot"
)

# Example 7: Custom marginal colors
jjscatterstats(
  data = jjscatterstats_test,
  dep = "ki67_index",
  group = "tumor_size",
  marginal = TRUE,
  xsidefill = "#1f77b4",
  ysidefill = "#ff7f0e"
)

# ═══════════════════════════════════════════════════════════
# STATISTICAL TESTS
# ═══════════════════════════════════════════════════════════

# Example 8: Parametric test (Pearson correlation)
jjscatterstats(
  data = jjscatterstats_clinical,
  dep = "glucose",
  group = "hemoglobin_a1c",
  typestatistics = "parametric",
  resultssubtitle = TRUE,
  conflevel = 0.95,
  k = 3
)

# Example 9: Nonparametric test (Spearman correlation)
jjscatterstats(
  data = jjscatterstats_survival,
  dep = "ki67_index",
  group = "survival_months",
  typestatistics = "nonparametric",
  resultssubtitle = TRUE
)

# Example 10: Robust test (percentage bend correlation)
jjscatterstats(
  data = jjscatterstats_test,
  dep = "tumor_size",
  group = "lymph_nodes",
  typestatistics = "robust",
  resultssubtitle = TRUE
)

# Example 11: Bayesian test
jjscatterstats(
  data = jjscatterstats_expression,
  dep = "gene_a_expression",
  group = "gene_b_expression",
  typestatistics = "bayes",
  resultssubtitle = TRUE,
  bfmessage = TRUE
)

# ═══════════════════════════════════════════════════════════
# SMOOTH METHODS
# ═══════════════════════════════════════════════════════════

# Example 12: Linear smooth (default)
jjscatterstats(
  data = jjscatterstats_test,
  dep = "ki67_index",
  group = "tumor_size",
  smoothMethod = "lm",
  smoothlinesize = 2,
  smoothlinecolor = "blue"
)

# Example 13: LOESS smooth (non-linear)
jjscatterstats(
  data = jjscatterstats_treatment,
  dep = "drug_dose",
  group = "response_score",
  smoothMethod = "loess",
  smoothlinecolor = "red"
)

# Example 14: GAM smooth (generalized additive model)
jjscatterstats(
  data = jjscatterstats_survival,
  dep = "ki67_index",
  group = "survival_months",
  smoothMethod = "gam",
  smoothlinesize = 2.5,
  smoothlinecolor = "darkgreen"
)

# ═══════════════════════════════════════════════════════════
# GROUPED SCATTER PLOTS
# ═══════════════════════════════════════════════════════════

# Example 15: Grouped by 2-level variable
jjscatterstats(
  data = jjscatterstats_test,
  dep = "ki67_index",
  group = "tumor_size",
  grvar = "receptor_status"
)

# Example 16: Grouped by 3-level variable
jjscatterstats(
  data = jjscatterstats_test,
  dep = "protein_expression",
  group = "mutation_burden",
  grvar = "tumor_grade",
  typestatistics = "parametric",
  resultssubtitle = TRUE
)

# Example 17: Grouped by 4-level variable
jjscatterstats(
  data = jjscatterstats_test,
  dep = "ki67_index",
  group = "tumor_size",
  grvar = "tumor_stage",
  marginalType = "histogram"
)

# ═══════════════════════════════════════════════════════════
# POINT CUSTOMIZATION
# ═══════════════════════════════════════════════════════════

# Example 18: Custom point size and transparency
jjscatterstats(
  data = jjscatterstats_test,
  dep = "ki67_index",
  group = "tumor_size",
  pointsize = 5,
  pointalpha = 0.7
)

# Example 19: Minimal points with prominent smooth line
jjscatterstats(
  data = jjscatterstats_clinical,
  dep = "cholesterol",
  group = "triglycerides",
  pointsize = 2,
  pointalpha = 0.3,
  smoothlinesize = 3,
  smoothlinecolor = "darkred"
)

# ═══════════════════════════════════════════════════════════
# GGPUBR INTEGRATION
# ═══════════════════════════════════════════════════════════

# Example 20: ggpubr plot with JCO palette
jjscatterstats(
  data = jjscatterstats_test,
  dep = "ki67_index",
  group = "tumor_size",
  addGGPubrPlot = TRUE,
  ggpubrPalette = "jco"
)

# Example 21: ggpubr with correlation statistics
jjscatterstats(
  data = jjscatterstats_clinical,
  dep = "glucose",
  group = "hemoglobin_a1c",
  addGGPubrPlot = TRUE,
  ggpubrAddCorr = TRUE,
  ggpubrCorrMethod = "pearson",
  ggpubrPalette = "npg"
)

# Example 22: ggpubr with smooth line
jjscatterstats(
  data = jjscatterstats_treatment,
  dep = "drug_dose",
  group = "response_score",
  addGGPubrPlot = TRUE,
  ggpubrAddSmooth = TRUE,
  ggpubrPalette = "aaas"
)

# Example 23: ggpubr complete (correlation + smooth + Lancet palette)
jjscatterstats(
  data = jjscatterstats_test,
  dep = "protein_expression",
  group = "mutation_burden",
  addGGPubrPlot = TRUE,
  ggpubrAddCorr = TRUE,
  ggpubrCorrMethod = "spearman",
  ggpubrAddSmooth = TRUE,
  ggpubrPalette = "lancet"
)

# ═══════════════════════════════════════════════════════════
# CLINICAL LABORATORY DATA
# ═══════════════════════════════════════════════════════════

# Example 24: Glucose vs HbA1c correlation
jjscatterstats(
  data = jjscatterstats_clinical,
  dep = "glucose",
  group = "hemoglobin_a1c",
  typestatistics = "parametric",
  marginalType = "histogram",
  resultssubtitle = TRUE,
  mytitle = "Glucose-HbA1c Correlation",
  xtitle = "Fasting Glucose (mg/dL)",
  ytitle = "HbA1c (%)"
)

# Example 25: Cholesterol vs Triglycerides
jjscatterstats(
  data = jjscatterstats_clinical,
  dep = "cholesterol",
  group = "triglycerides",
  typestatistics = "parametric",
  marginalType = "density",
  resultssubtitle = TRUE,
  mytitle = "Lipid Profile Correlation"
)

# Example 26: BMI vs Blood Pressure
jjscatterstats(
  data = jjscatterstats_clinical,
  dep = "bmi",
  group = "systolic_bp",
  grvar = "diagnosis",
  typestatistics = "parametric",
  marginalType = "boxplot",
  resultssubtitle = TRUE,
  mytitle = "BMI vs Blood Pressure by Diagnosis"
)

# Example 27: HDL vs Triglycerides (negative correlation)
jjscatterstats(
  data = jjscatterstats_clinical,
  dep = "hdl_cholesterol",
  group = "triglycerides",
  typestatistics = "nonparametric",
  marginalType = "density",
  resultssubtitle = TRUE,
  mytitle = "HDL-Triglyceride Inverse Relationship"
)

# ═══════════════════════════════════════════════════════════
# DOSE-RESPONSE ANALYSIS
# ═══════════════════════════════════════════════════════════

# Example 28: Basic dose-response
jjscatterstats(
  data = jjscatterstats_treatment,
  dep = "drug_dose",
  group = "response_score",
  smoothMethod = "loess",
  marginalType = "histogram",
  mytitle = "Dose-Response Relationship"
)

# Example 29: Dose vs tumor size reduction
jjscatterstats(
  data = jjscatterstats_treatment,
  dep = "drug_dose",
  group = "tumor_size",
  grvar = "timepoint",
  smoothMethod = "loess",
  marginalType = "boxplot",
  mytitle = "Drug Dose Effect on Tumor Size Over Time"
)

# Example 30: Dose vs adverse events
jjscatterstats(
  data = jjscatterstats_treatment,
  dep = "drug_dose",
  group = "adverse_events",
  smoothMethod = "loess",
  typestatistics = "nonparametric",
  resultssubtitle = TRUE,
  mytitle = "Dose-Toxicity Relationship"
)

# ═══════════════════════════════════════════════════════════
# GENE/PROTEIN EXPRESSION
# ═══════════════════════════════════════════════════════════

# Example 31: Co-regulated genes
jjscatterstats(
  data = jjscatterstats_expression,
  dep = "gene_a_expression",
  group = "gene_b_expression",
  typestatistics = "parametric",
  marginalType = "histogram",
  resultssubtitle = TRUE,
  mytitle = "Co-regulated Gene Expression",
  xtitle = "Gene A (log2 expression)",
  ytitle = "Gene B (log2 expression)"
)

# Example 32: Oncogene vs tumor suppressor (negative correlation)
jjscatterstats(
  data = jjscatterstats_expression,
  dep = "oncogene_expression",
  group = "tumor_suppressor_expression",
  typestatistics = "parametric",
  marginalType = "density",
  resultssubtitle = TRUE,
  mytitle = "Oncogene vs Tumor Suppressor Expression"
)

# Example 33: PD-L1 vs TILs correlation
jjscatterstats(
  data = jjscatterstats_expression,
  dep = "pdl1_expression",
  group = "til_score",
  grvar = "cancer_type",
  typestatistics = "nonparametric",
  marginalType = "histogram",
  resultssubtitle = TRUE,
  mytitle = "PD-L1 Expression vs TILs by Cancer Type"
)

# Example 34: Proliferation markers (Ki67 vs PCNA)
jjscatterstats(
  data = jjscatterstats_expression,
  dep = "ki67_protein",
  group = "pcna_protein",
  typestatistics = "parametric",
  marginalType = "density",
  resultssubtitle = TRUE,
  mytitle = "Proliferation Marker Correlation"
)

# ═══════════════════════════════════════════════════════════
# SURVIVAL BIOMARKER ANALYSIS
# ═══════════════════════════════════════════════════════════

# Example 35: Ki67 vs survival time
jjscatterstats(
  data = jjscatterstats_survival,
  dep = "ki67_index",
  group = "survival_months",
  typestatistics = "nonparametric",
  marginalType = "density",
  resultssubtitle = TRUE,
  smoothMethod = "loess",
  mytitle = "Ki67 Index vs Overall Survival"
)

# Example 36: Ki67 vs survival by treatment
jjscatterstats(
  data = jjscatterstats_survival,
  dep = "ki67_index",
  group = "survival_months",
  grvar = "treatment",
  typestatistics = "nonparametric",
  marginalType = "boxplot",
  resultssubtitle = TRUE,
  mytitle = "Ki67 vs Survival by Treatment Modality"
)

# Example 37: PD-L1 vs response score
jjscatterstats(
  data = jjscatterstats_survival,
  dep = "pdl1_score",
  group = "response_score",
  typestatistics = "parametric",
  marginalType = "histogram",
  resultssubtitle = TRUE,
  mytitle = "PD-L1 Score vs Treatment Response"
)

# Example 38: Ki67 vs PFS (progression-free survival)
jjscatterstats(
  data = jjscatterstats_survival,
  dep = "ki67_index",
  group = "pfs_months",
  grvar = "disease_stage",
  typestatistics = "nonparametric",
  smoothMethod = "loess",
  mytitle = "Ki67 vs Progression-Free Survival by Stage"
)

# ═══════════════════════════════════════════════════════════
# PUBLICATION-READY COMPLETE EXAMPLES
# ═══════════════════════════════════════════════════════════

# Example 39: Complete biomarker analysis
jjscatterstats(
  data = jjscatterstats_test,
  dep = "ki67_index",
  group = "tumor_size",
  grvar = "tumor_grade",
  typestatistics = "parametric",
  marginalType = "histogram",
  resultssubtitle = TRUE,
  smoothMethod = "lm",
  conflevel = 0.95,
  k = 2,
  pointsize = 3.5,
  pointalpha = 0.6,
  smoothlinesize = 2,
  smoothlinecolor = "blue",
  xsidefill = "#009E73",
  ysidefill = "#D55E00",
  mytitle = "Ki67 Proliferation Index vs Tumor Size by Grade",
  xtitle = "Ki67 Proliferation Index (%)",
  ytitle = "Tumor Size (mm)",
  addGGPubrPlot = TRUE,
  ggpubrPalette = "jco",
  ggpubrAddCorr = TRUE,
  ggpubrCorrMethod = "pearson"
)

# Example 40: Complete clinical lab analysis
jjscatterstats(
  data = jjscatterstats_clinical,
  dep = "glucose",
  group = "hemoglobin_a1c",
  grvar = "diagnosis",
  typestatistics = "parametric",
  marginalType = "density",
  resultssubtitle = TRUE,
  conflevel = 0.95,
  k = 3,
  pointsize = 4,
  pointalpha = 0.5,
  smoothlinesize = 2.5,
  smoothlinecolor = "darkblue",
  mytitle = "Glucose-HbA1c Correlation by Diagnostic Category",
  xtitle = "Fasting Glucose (mg/dL)",
  ytitle = "Hemoglobin A1c (%)",
  addGGPubrPlot = TRUE,
  ggpubrPalette = "npg",
  ggpubrAddCorr = TRUE,
  ggpubrCorrMethod = "pearson",
  ggpubrAddSmooth = TRUE
)

# Example 41: Complete dose-response analysis
jjscatterstats(
  data = jjscatterstats_treatment,
  dep = "drug_dose",
  group = "response_score",
  grvar = "timepoint",
  smoothMethod = "loess",
  marginalType = "boxplot",
  pointsize = 4,
  pointalpha = 0.5,
  smoothlinesize = 2.5,
  smoothlinecolor = "darkred",
  mytitle = "Dose-Response Relationship Over Time",
  xtitle = "Drug Dose (mg)",
  ytitle = "Response Score (0-100)"
)
