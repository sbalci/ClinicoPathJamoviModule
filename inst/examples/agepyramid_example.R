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
  plot_title = "Cancer Patient Age Distribution"
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
  plot_title = "Geriatric Patient Age Distribution"
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
# Example 7: Unbalanced Gender Distribution
# ═══════════════════════════════════════════════════════════

# Example: Breast cancer cohort (predominantly female)
data(agepyramid_unbalanced)

agepyramid(
  data = agepyramid_unbalanced,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male",
  plot_title = "Breast Cancer Patient Age Distribution"
)

# ═══════════════════════════════════════════════════════════
# Example 8: Life Course Analysis
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
# Example 9: Creating Your Own Data
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
# Example 10: Custom Age Breaks
# ═══════════════════════════════════════════════════════════

# Define your own age boundaries
agepyramid(
  data = agepyramid_test,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male",
  age_groups = "custom",
  custom_breaks = "0,18,25,50,65,100",
  plot_title = "Custom Age Boundaries"
)

# ═══════════════════════════════════════════════════════════
# Example 11: Colorblind-Friendly Palette
# ═══════════════════════════════════════════════════════════

# Use colorblind-friendly colors (orange/blue)
agepyramid(
  data = agepyramid_test,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male",
  color_palette = "colorblind",
  plot_title = "Colorblind-Friendly Age Pyramid"
)

# ═══════════════════════════════════════════════════════════
# Example 12: Grayscale for Print Publications
# ═══════════════════════════════════════════════════════════

# Use grayscale palette for print publications
agepyramid(
  data = agepyramid_cancer,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male",
  age_groups = "geriatric",
  color_palette = "grayscale",
  plot_title = "Geriatric Population (Grayscale)"
)

# ═══════════════════════════════════════════════════════════
# Example 13: Custom Colors
# ═══════════════════════════════════════════════════════════

# Use custom brand colors
agepyramid(
  data = agepyramid_test,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male",
  color_palette = "custom",
  female_color = "#9C27B0",  # Purple
  male_color = "#4CAF50",    # Green
  plot_title = "Custom Brand Colors"
)

# ═══════════════════════════════════════════════════════════
# Example 14: Complete Customization
# ═══════════════════════════════════════════════════════════

# Use all customization options together
agepyramid(
  data = agepyramid_cancer,
  age = "age",
  gender = "gender",
  female = "Female",
  male = "Male",
  age_groups = "custom",
  custom_breaks = "0,55,65,75,85,100",
  color_palette = "custom",
  female_color = "#D55E00",  # Vermillion
  male_color = "#0072B2",    # Blue
  plot_title = "Comprehensive Custom Age Analysis"
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

# Age group presets:
# - Custom: Maximum flexibility with bin_width (default: 5 years)
#   * Small populations: use 5-10 year bins
#   * Large populations: use 1-5 year bins
# - Pediatric: Optimized for ages 0-18 with developmental milestones
#   * Breaks: 0, 1, 2, 5, 10, 15, 18, Inf
# - Reproductive: Focus on reproductive ages 15-50 with 5-year intervals
#   * Breaks: 0, 15, 20, 25, 30, 35, 40, 45, 50, Inf
# - Geriatric: Optimized for ages 65+ with 5-year intervals
#   * Breaks: 0, 65, 70, 75, 80, 85, 90, 95, Inf
# - Life Course: Comprehensive all-age analysis with key life stages
#   * Breaks: 0, 5, 15, 25, 45, 65, 75, 85, Inf

# Custom age breaks:
# - Define exact boundaries: "0,18,25,50,65,100"
# - Automatically adds Inf at the end
# - Useful for specific clinical classifications
# - Falls back to bin_width if parsing fails

# Color palettes:
# - Standard: Traditional pink (#E91E63) and blue (#2196F3)
#   * Familiar to most audiences
# - Colorblind-friendly: Orange (#E69F00) and blue (#0072B2)
#   * Recommended for publications and presentations
#   * Accessible to ~8% of males with color vision deficiency
# - Grayscale: Dark (#666666) and light (#CCCCCC) gray
#   * Perfect for print publications
#   * Professional appearance in black and white
# - Custom: Specify your own hex colors
#   * Match institutional branding
#   * Create unique visual identity

# Gender level selection:
# - Female and Male level selectors allow flexible data structures
# - Smart defaults: automatically uses first two levels if not specified
# - Single-gender cohorts: set only Female or Male level
# - The analysis will automatically detect and handle single-gender data

# Plot customization:
# - Use descriptive plot titles to communicate the population being analyzed
# - Age groups are displayed with readable labels (e.g., "1-5", "6-10", "86+")
# - Tables include both counts (n) and percentages (%) for each age-gender group
# - Colors update instantly without recomputing data

# ═══════════════════════════════════════════════════════════
