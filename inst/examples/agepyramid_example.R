# ═══════════════════════════════════════════════════════════
# Example Usage: agepyramid
# ═══════════════════════════════════════════════════════════
#
# Age Pyramid (Population Pyramid) Visualization
# Displays the age and gender distribution of a population
#
# Generated: 2026-01-03

library(ClinicoPath)

# ═══════════════════════════════════════════════════════════
# Example 1: Basic Age Pyramid
# ═══════════════════════════════════════════════════════════

# Load standard test data
data(agepyramid_test)

# Create basic age pyramid
agepyramid(
  data = agepyramid_test,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male"
)

# ═══════════════════════════════════════════════════════════
# Example 2: Custom Bin Width
# ═══════════════════════════════════════════════════════════

# Use 10-year age bins for broader grouping
agepyramid(
  data = agepyramid_test,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male",
  bin_width = 10,
  plot_title = "Population Age Distribution (10-Year Bins)"
)

# Use 1-year bins for detailed distribution
agepyramid(
  data = agepyramid_test,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male",
  bin_width = 1,
  plot_title = "Detailed Age Distribution (1-Year Bins)"
)

# ═══════════════════════════════════════════════════════════
# Example 3: Cancer Patient Population
# ═══════════════════════════════════════════════════════════

# Typical cancer patient age distribution (older population)
data(agepyramid_cancer)

agepyramid(
  data = agepyramid_cancer,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male",
  plot_title = "Cancer Patient Age Distribution",
  color_palette = "accessible"  # Colorblind-friendly
)

# ═══════════════════════════════════════════════════════════
# Example 4: Pediatric Population
# ═══════════════════════════════════════════════════════════

data(agepyramid_pediatric)

agepyramid(
  data = agepyramid_pediatric,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male",
  age_groups = "pediatric",
  plot_title = "Pediatric Patient Age Distribution"
)

# ═══════════════════════════════════════════════════════════
# Example 5: Geriatric Population
# ═══════════════════════════════════════════════════════════

data(agepyramid_geriatric)

agepyramid(
  data = agepyramid_geriatric,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male",
  age_groups = "geriatric",
  plot_title = "Geriatric Patient Age Distribution",
  bin_width = 5
)

# ═══════════════════════════════════════════════════════════
# Example 6: Reproductive Age Population
# ═══════════════════════════════════════════════════════════

data(agepyramid_reproductive)

agepyramid(
  data = agepyramid_reproductive,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male",
  age_groups = "reproductive",
  plot_title = "Reproductive Age Population (15-50 years)"
)

# ═══════════════════════════════════════════════════════════
# Example 7: Custom Colors
# ═══════════════════════════════════════════════════════════

# Use custom colors for gender
agepyramid(
  data = agepyramid_test,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male",
  color_palette = "custom",
  color1 = "#E69F00",  # Orange for first gender
  color2 = "#56B4E9",  # Blue for second gender
  plot_title = "Custom Color Age Pyramid"
)

# ═══════════════════════════════════════════════════════════
# Example 8: Different Plot Engines
# ═══════════════════════════════════════════════════════════

# Using ggcharts (default, better pyramid styling)
agepyramid(
  data = agepyramid_test,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male",
  plot_engine = "ggcharts",
  plot_title = "Age Pyramid (ggcharts)"
)

# Using ggplot2 (more customization options)
agepyramid(
  data = agepyramid_test,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male",
  plot_engine = "ggplot2",
  plot_title = "Age Pyramid (ggplot2)"
)

# ═══════════════════════════════════════════════════════════
# Example 9: Colorblind-Friendly Palette
# ═══════════════════════════════════════════════════════════

# Use accessible color palette for colorblind viewers
agepyramid(
  data = agepyramid_test,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male",
  color_palette = "accessible",
  plot_title = "Colorblind-Friendly Age Pyramid"
)

# ═══════════════════════════════════════════════════════════
# Example 10: Unbalanced Gender Distribution
# ═══════════════════════════════════════════════════════════

# Example: Breast cancer cohort (predominantly female)
data(agepyramid_unbalanced)

agepyramid(
  data = agepyramid_unbalanced,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male",
  plot_title = "Breast Cancer Patient Age Distribution",
  color_palette = "standard"
)

# ═══════════════════════════════════════════════════════════
# Example 11: Life Course Analysis
# ═══════════════════════════════════════════════════════════

# Use life course preset for comprehensive age analysis
agepyramid(
  data = agepyramid_test,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male",
  age_groups = "lifecourse",
  plot_title = "Life Course Age Distribution"
)

# ═══════════════════════════════════════════════════════════
# Example 12: Creating Your Own Data
# ═══════════════════════════════════════════════════════════

# Create sample data
my_data <- data.frame(
  patient_age = c(25, 30, 35, 40, 45, 50, 55, 60, 65, 70,
                  28, 33, 38, 43, 48, 53, 58, 63, 68, 73),
  sex = c(rep("Female", 10), rep("Male", 10))
)

# Create pyramid
agepyramid(
  data = my_data,
  age = "patient_age",
  gender = "sex",
  female = "Female",
  male = "Male",
  bin_width = 5,
  plot_title = "Custom Dataset Age Pyramid"
)

# ═══════════════════════════════════════════════════════════
# Example 13: Complete Customization
# ═══════════════════════════════════════════════════════════

# Use all customization options together
agepyramid(
  data = agepyramid_cancer,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male",
  bin_width = 5,
  plot_title = "Comprehensive Age Analysis",
  color_palette = "custom",
  color1 = "#D55E00",  # Vermillion
  color2 = "#0072B2",  # Blue
  age_groups = "custom",
  plot_engine = "ggcharts"
)

# ═══════════════════════════════════════════════════════════
# Clinical Applications
# ═══════════════════════════════════════════════════════════

# Age pyramids are useful for:
#
# 1. Demographic Analysis
#    - Understanding patient population structure
#    - Comparing clinical trial enrollment to general population
#    - Identifying age-gender imbalances in studies
#
# 2. Epidemiological Studies
#    - Visualizing disease incidence by age and gender
#    - Comparing patient cohorts
#    - Planning healthcare resource allocation
#
# 3. Clinical Research
#    - Describing study populations
#    - Quality checks for recruitment bias
#    - Publication-ready demographic figures
#
# 4. Public Health
#    - Population health assessments
#    - Disease surveillance
#    - Healthcare planning

# ═══════════════════════════════════════════════════════════
# Tips for Effective Age Pyramids
# ═══════════════════════════════════════════════════════════

# Choosing bin width:
# - Small populations: 5-10 years
# - Large populations: 1-5 years
# - Pediatric: 1-3 years
# - Geriatric: 5-10 years

# Choosing colors:
# - Standard: Traditional gender colors (familiar to audience)
# - Accessible: Colorblind-friendly (better for publications)
# - Custom: Match institutional branding or publication theme

# Age group presets:
# - Custom: Maximum flexibility with bin_width
# - Pediatric: Optimized for ages 0-18
# - Reproductive: Focus on ages 15-50
# - Geriatric: Optimized for ages 65+
# - Life Course: Comprehensive all-age analysis

# ═══════════════════════════════════════════════════════════
