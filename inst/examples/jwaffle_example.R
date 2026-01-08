# ═══════════════════════════════════════════════════════════
# Example Usage: jwaffle
# ═══════════════════════════════════════════════════════════
#
# Comprehensive examples for the jwaffle jamovi function
# Generated: 2026-01-06

library(ClinicoPath)

# Load test datasets
data(jwaffle_test)
data(jwaffle_disease)
data(jwaffle_pathology)
data(jwaffle_demographics)
data(jwaffle_quality)

# ═══════════════════════════════════════════════════════════
# BASIC WAFFLE CHARTS
# ═══════════════════════════════════════════════════════════

# Example 1: Simple treatment response distribution
jwaffle(
  data = jwaffle_test,
  groups = "response_category"
)

# Example 2: Disease subtype distribution
jwaffle(
  data = jwaffle_disease,
  groups = "disease_subtype"
)

# Example 3: Tumor grade distribution
jwaffle(
  data = jwaffle_pathology,
  groups = "tumor_grade"
)

# ═══════════════════════════════════════════════════════════
# ROWS PARAMETER
# ═══════════════════════════════════════════════════════════

# Example 4: 3 rows (30 squares)
jwaffle(
  data = jwaffle_pathology,
  groups = "tumor_grade",
  rows = 3
)

# Example 5: 5 rows (50 squares, default)
jwaffle(
  data = jwaffle_test,
  groups = "response_category",
  rows = 5
)

# Example 6: 10 rows (100 squares, full percentage)
jwaffle(
  data = jwaffle_demographics,
  groups = "age_group",
  rows = 10
)

# ═══════════════════════════════════════════════════════════
# FLIP ORIENTATION
# ═══════════════════════════════════════════════════════════

# Example 7: Standard orientation
jwaffle(
  data = jwaffle_quality,
  groups = "quality_grade",
  flip = FALSE
)

# Example 8: Flipped orientation
jwaffle(
  data = jwaffle_demographics,
  groups = "smoking_status",
  flip = TRUE
)

# ═══════════════════════════════════════════════════════════
# COLOR PALETTES
# ═══════════════════════════════════════════════════════════

# Example 9: Default colors
jwaffle(
  data = jwaffle_test,
  groups = "response_category",
  color_palette = "default"
)

# Example 10: Colorblind-friendly palette
jwaffle(
  data = jwaffle_demographics,
  groups = "age_group",
  color_palette = "colorblind"
)

# Example 11: Professional palette
jwaffle(
  data = jwaffle_disease,
  groups = "disease_stage",
  color_palette = "professional"
)

# Example 12: Presentation palette
jwaffle(
  data = jwaffle_pathology,
  groups = "differentiation",
  color_palette = "presentation"
)

# Example 13: Journal palette
jwaffle(
  data = jwaffle_quality,
  groups = "risk_category",
  color_palette = "journal"
)

# Example 14: Pastel colors
jwaffle(
  data = jwaffle_demographics,
  groups = "ethnicity",
  color_palette = "pastel"
)

# Example 15: Dark colors
jwaffle(
  data = jwaffle_disease,
  groups = "molecular_subtype",
  color_palette = "dark"
)

# ═══════════════════════════════════════════════════════════
# LEGEND OPTIONS
# ═══════════════════════════════════════════════════════════

# Example 16: With legend
jwaffle(
  data = jwaffle_test,
  groups = "response_category",
  show_legend = TRUE
)

# Example 17: With custom legend title
jwaffle(
  data = jwaffle_pathology,
  groups = "tumor_grade",
  show_legend = TRUE,
  legendtitle = "Tumor Grade"
)

# ═══════════════════════════════════════════════════════════
# TITLES
# ═══════════════════════════════════════════════════════════

# Example 18: With custom title
jwaffle(
  data = jwaffle_test,
  groups = "response_category",
  mytitle = "Treatment Response Distribution"
)

# Example 19: With both custom titles
jwaffle(
  data = jwaffle_quality,
  groups = "quality_grade",
  show_legend = TRUE,
  mytitle = "Quality Metrics Distribution",
  legendtitle = "Quality Grade"
)

# ═══════════════════════════════════════════════════════════
# FACETING
# ═══════════════════════════════════════════════════════════

# Example 20: Facet by treatment (3 levels)
jwaffle(
  data = jwaffle_test,
  groups = "response_category",
  facet = "treatment"
)

# Example 21: Facet by gender (2 levels)
jwaffle(
  data = jwaffle_demographics,
  groups = "age_group",
  facet = "gender"
)

# Example 22: Facet by disease stage (4 levels)
jwaffle(
  data = jwaffle_disease,
  groups = "disease_subtype",
  facet = "disease_stage"
)

# Example 23: Facet by hospital (3 levels)
jwaffle(
  data = jwaffle_pathology,
  groups = "tumor_grade",
  facet = "hospital"
)

# Example 24: Facet by quarter (4 levels)
jwaffle(
  data = jwaffle_quality,
  groups = "quality_grade",
  facet = "quarter"
)

# ═══════════════════════════════════════════════════════════
# SUMMARIES AND EXPLANATIONS
# ═══════════════════════════════════════════════════════════

# Example 25: With analysis summary
jwaffle(
  data = jwaffle_test,
  groups = "response_category",
  showSummaries = TRUE
)

# Example 26: With explanations
jwaffle(
  data = jwaffle_pathology,
  groups = "tumor_grade",
  showExplanations = TRUE
)

# Example 27: With both summaries and explanations
jwaffle(
  data = jwaffle_disease,
  groups = "disease_stage",
  showSummaries = TRUE,
  showExplanations = TRUE
)

# ═══════════════════════════════════════════════════════════
# TREATMENT RESPONSE ANALYSIS
# ═══════════════════════════════════════════════════════════

# Example 28: Basic treatment response
jwaffle(
  data = jwaffle_test,
  groups = "response_category",
  color_palette = "colorblind",
  mytitle = "Treatment Response Distribution"
)

# Example 29: Response by treatment type
jwaffle(
  data = jwaffle_test,
  groups = "response_category",
  facet = "treatment",
  color_palette = "colorblind",
  show_legend = TRUE,
  mytitle = "Treatment Response by Therapy Type",
  legendtitle = "Response Category"
)

# Example 30: Response by timepoint
jwaffle(
  data = jwaffle_test,
  groups = "response_category",
  facet = "timepoint",
  rows = 5,
  color_palette = "professional",
  show_legend = TRUE,
  mytitle = "Treatment Response Over Time",
  legendtitle = "Response"
)

# ═══════════════════════════════════════════════════════════
# PATHOLOGY DISTRIBUTIONS
# ═══════════════════════════════════════════════════════════

# Example 31: Tumor grade distribution
jwaffle(
  data = jwaffle_pathology,
  groups = "tumor_grade",
  color_palette = "journal",
  mytitle = "Tumor Grade Distribution"
)

# Example 32: Grade by hospital
jwaffle(
  data = jwaffle_pathology,
  groups = "tumor_grade",
  facet = "hospital",
  rows = 3,
  color_palette = "journal",
  show_legend = TRUE,
  mytitle = "Tumor Grade Distribution by Hospital",
  legendtitle = "Grade"
)

# Example 33: Differentiation status
jwaffle(
  data = jwaffle_pathology,
  groups = "differentiation",
  rows = 5,
  color_palette = "professional",
  show_legend = TRUE,
  mytitle = "Tumor Differentiation Status",
  legendtitle = "Differentiation"
)

# Example 34: Lymphovascular invasion status
jwaffle(
  data = jwaffle_pathology,
  groups = "lvi_status",
  facet = "hospital",
  color_palette = "colorblind",
  show_legend = TRUE,
  mytitle = "Lymphovascular Invasion by Hospital",
  legendtitle = "LVI Status"
)

# Example 35: Surgical margin status
jwaffle(
  data = jwaffle_pathology,
  groups = "margin_status",
  facet = "hospital",
  color_palette = "presentation",
  show_legend = TRUE,
  mytitle = "Surgical Margin Status by Hospital",
  legendtitle = "Margin Status"
)

# ═══════════════════════════════════════════════════════════
# DEMOGRAPHICS ANALYSIS
# ═══════════════════════════════════════════════════════════

# Example 36: Age distribution
jwaffle(
  data = jwaffle_demographics,
  groups = "age_group",
  color_palette = "professional",
  mytitle = "Age Distribution"
)

# Example 37: Age by gender
jwaffle(
  data = jwaffle_demographics,
  groups = "age_group",
  facet = "gender",
  rows = 5,
  color_palette = "colorblind",
  show_legend = TRUE,
  mytitle = "Age Distribution by Gender",
  legendtitle = "Age Group"
)

# Example 38: Ethnicity distribution
jwaffle(
  data = jwaffle_demographics,
  groups = "ethnicity",
  rows = 5,
  color_palette = "pastel",
  show_legend = TRUE,
  mytitle = "Ethnicity Distribution",
  legendtitle = "Ethnicity"
)

# Example 39: Ethnicity by region
jwaffle(
  data = jwaffle_demographics,
  groups = "ethnicity",
  facet = "region",
  color_palette = "colorblind",
  show_legend = TRUE,
  mytitle = "Ethnicity Distribution by Region",
  legendtitle = "Ethnicity"
)

# Example 40: Smoking status
jwaffle(
  data = jwaffle_demographics,
  groups = "smoking_status",
  color_palette = "presentation",
  mytitle = "Smoking Status Distribution"
)

# Example 41: Smoking status by age
jwaffle(
  data = jwaffle_demographics,
  groups = "smoking_status",
  facet = "age_group",
  rows = 3,
  color_palette = "colorblind",
  show_legend = TRUE,
  mytitle = "Smoking Status by Age Group",
  legendtitle = "Smoking Status"
)

# ═══════════════════════════════════════════════════════════
# DISEASE SUBTYPE ANALYSIS
# ═══════════════════════════════════════════════════════════

# Example 42: Histological subtypes
jwaffle(
  data = jwaffle_disease,
  groups = "disease_subtype",
  rows = 5,
  color_palette = "journal",
  show_legend = TRUE,
  mytitle = "Histological Subtype Distribution",
  legendtitle = "Subtype"
)

# Example 43: Subtypes by stage
jwaffle(
  data = jwaffle_disease,
  groups = "disease_subtype",
  facet = "disease_stage",
  color_palette = "professional",
  show_legend = TRUE,
  mytitle = "Histological Subtypes by Disease Stage",
  legendtitle = "Subtype"
)

# Example 44: Molecular subtypes
jwaffle(
  data = jwaffle_disease,
  groups = "molecular_subtype",
  rows = 5,
  color_palette = "dark",
  show_legend = TRUE,
  mytitle = "Molecular Subtype Distribution",
  legendtitle = "Molecular Subtype"
)

# Example 45: Molecular subtypes by stage
jwaffle(
  data = jwaffle_disease,
  groups = "molecular_subtype",
  facet = "disease_stage",
  rows = 5,
  color_palette = "colorblind",
  show_legend = TRUE,
  mytitle = "Molecular Subtypes by Disease Stage",
  legendtitle = "Molecular Subtype"
)

# ═══════════════════════════════════════════════════════════
# QUALITY METRICS ANALYSIS
# ═══════════════════════════════════════════════════════════

# Example 46: Quality grades
jwaffle(
  data = jwaffle_quality,
  groups = "quality_grade",
  color_palette = "presentation",
  mytitle = "Quality Grade Distribution"
)

# Example 47: Quality by institution type
jwaffle(
  data = jwaffle_quality,
  groups = "quality_grade",
  facet = "institution_type",
  flip = TRUE,
  color_palette = "professional",
  show_legend = TRUE,
  mytitle = "Quality Grades by Institution Type",
  legendtitle = "Quality Grade"
)

# Example 48: Compliance levels
jwaffle(
  data = jwaffle_quality,
  groups = "compliance",
  rows = 5,
  color_palette = "journal",
  show_legend = TRUE,
  mytitle = "Compliance Level Distribution",
  legendtitle = "Compliance"
)

# Example 49: Risk categories over time
jwaffle(
  data = jwaffle_quality,
  groups = "risk_category",
  facet = "quarter",
  color_palette = "colorblind",
  show_legend = TRUE,
  mytitle = "Risk Category Distribution Over Time",
  legendtitle = "Risk Level"
)

# Example 50: Accreditation status
jwaffle(
  data = jwaffle_quality,
  groups = "accreditation",
  facet = "institution_type",
  color_palette = "presentation",
  show_legend = TRUE,
  mytitle = "Accreditation Status by Institution Type",
  legendtitle = "Accreditation"
)
