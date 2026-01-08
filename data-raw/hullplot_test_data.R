# ═══════════════════════════════════════════════════════════
# Test Data Generation: hullplot
# ═══════════════════════════════════════════════════════════
#
# This script generates realistic test data for the hullplot jamovi function
# which creates scatter plots with hull polygons around groups.
#
# Generated: 2026-01-05
# Seed: 42
# Test data: hullplot_test, hullplot_clusters, hullplot_overlap,
#            hullplot_outliers, hullplot_clinical, hullplot_small,
#            hullplot_fourgroup, hullplot_unbalanced

library(tibble)
library(dplyr)
library(here)
library(writexl)
library(jmvReadWrite)

set.seed(42)

# ═══════════════════════════════════════════════════════════
# Dataset 1: Basic Two-Group Scatter (hullplot_test)
# ═══════════════════════════════════════════════════════════
# Well-separated groups for testing basic functionality

n_test <- 120

# Group A: centered at (20, 30)
group_a <- tibble(
  x = rnorm(60, mean = 20, sd = 5),
  y = rnorm(60, mean = 30, sd = 6),
  group = "Group A"
)

# Group B: centered at (50, 60)
group_b <- tibble(
  x = rnorm(60, mean = 50, sd = 5),
  y = rnorm(60, mean = 60, sd = 6),
  group = "Group B"
)

hullplot_test <- bind_rows(group_a, group_b) %>%
  mutate(
    patient_id = 1:n(),

    # Add size variable (varies with position)
    size_var = sqrt((x - mean(x))^2 + (y - mean(y))^2) + rnorm(n(), 0, 2),

    # Add color variable (categorical)
    color_category = factor(sample(c("Type 1", "Type 2", "Type 3"),
                                   n(), replace = TRUE)),

    # Convert group to factor
    group = factor(group, levels = c("Group A", "Group B"))
  ) %>%
  select(patient_id, x, y, group, size_var, color_category)


# ═══════════════════════════════════════════════════════════
# Dataset 2: Three Distinct Clusters (hullplot_clusters)
# ═══════════════════════════════════════════════════════════
# Well-separated clusters in different regions

n_clusters <- 150

# Cluster 1: Low X, Low Y (bottom-left)
cluster1 <- tibble(
  x = rnorm(50, mean = 15, sd = 4),
  y = rnorm(50, mean = 20, sd = 4),
  cluster = "Cluster 1"
)

# Cluster 2: High X, Low Y (bottom-right)
cluster2 <- tibble(
  x = rnorm(50, mean = 65, sd = 4),
  y = rnorm(50, mean = 25, sd = 4),
  cluster = "Cluster 2"
)

# Cluster 3: Mid X, High Y (top-center)
cluster3 <- tibble(
  x = rnorm(50, mean = 40, sd = 4),
  y = rnorm(50, mean = 70, sd = 4),
  cluster = "Cluster 3"
)

hullplot_clusters <- bind_rows(cluster1, cluster2, cluster3) %>%
  mutate(
    sample_id = 1:n(),

    # Add biomarker level (varies by cluster)
    biomarker = case_when(
      cluster == "Cluster 1" ~ rnorm(n(), 10, 3),
      cluster == "Cluster 2" ~ rnorm(n(), 30, 4),
      cluster == "Cluster 3" ~ rnorm(n(), 50, 5),
      TRUE ~ NA_real_
    ),

    # Add response category
    response = factor(sample(c("Responder", "Non-responder", "Partial"),
                            n(), replace = TRUE)),

    # Convert cluster to factor
    cluster = factor(cluster, levels = c("Cluster 1", "Cluster 2", "Cluster 3"))
  ) %>%
  select(sample_id, x, y, cluster, biomarker, response)


# ═══════════════════════════════════════════════════════════
# Dataset 3: Overlapping Groups (hullplot_overlap)
# ═══════════════════════════════════════════════════════════
# Groups with significant overlap for testing hull boundaries

n_overlap <- 140

# Group A: centered at (40, 40)
overlap_a <- tibble(
  x = rnorm(70, mean = 40, sd = 10),
  y = rnorm(70, mean = 40, sd = 10),
  treatment = "Treatment A"
)

# Group B: centered at (50, 50) - overlaps with A
overlap_b <- tibble(
  x = rnorm(70, mean = 50, sd = 10),
  y = rnorm(70, mean = 50, sd = 10),
  treatment = "Treatment B"
)

hullplot_overlap <- bind_rows(overlap_a, overlap_b) %>%
  mutate(
    patient_id = 1:n(),

    # Add tumor size
    tumor_size = abs(rnorm(n(), mean = 30, sd = 12)),

    # Add stage
    stage = factor(sample(c("Early", "Advanced"), n(), replace = TRUE)),

    # Convert treatment to factor
    treatment = factor(treatment, levels = c("Treatment A", "Treatment B"))
  ) %>%
  select(patient_id, x, y, treatment, tumor_size, stage)


# ═══════════════════════════════════════════════════════════
# Dataset 4: Groups with Outliers (hullplot_outliers)
# ═══════════════════════════════════════════════════════════
# Data with clear outliers to test outlier detection

n_outliers <- 100

# Main group data
outlier_main <- tibble(
  x = c(
    rnorm(40, mean = 30, sd = 5),  # Disease A
    rnorm(40, mean = 60, sd = 5)   # Disease B
  ),
  y = c(
    rnorm(40, mean = 35, sd = 5),  # Disease A
    rnorm(40, mean = 65, sd = 5)   # Disease B
  ),
  disease_type = factor(rep(c("Disease A", "Disease B"), each = 40))
)

# Add outliers
outlier_points <- tibble(
  x = c(10, 80, 45, 75, 20, 55, 15, 70, 40, 62, 25, 68, 35, 58, 48, 52, 22, 66, 38, 60),
  y = c(80, 10, 75, 20, 70, 15, 65, 25, 10, 75, 68, 18, 72, 22, 12, 78, 66, 20, 70, 12),
  disease_type = factor(sample(c("Disease A", "Disease B"), 20, replace = TRUE))
)

hullplot_outliers <- bind_rows(outlier_main, outlier_points) %>%
  mutate(
    observation_id = 1:n(),

    # Add severity score
    severity = abs(rnorm(n(), mean = 50, sd = 20)),

    # Add outcome
    outcome = factor(sample(c("Good", "Poor"), n(), replace = TRUE,
                           prob = c(0.6, 0.4))),

    # Reorder
    disease_type = factor(disease_type, levels = c("Disease A", "Disease B"))
  ) %>%
  select(observation_id, x, y, disease_type, severity, outcome)


# ═══════════════════════════════════════════════════════════
# Dataset 5: Clinical Trial Data (hullplot_clinical)
# ═══════════════════════════════════════════════════════════
# Realistic clinical trial scenario

n_clinical <- 180

hullplot_clinical <- tibble(
  patient_id = 1:n_clinical,

  # Tumor volume (cm³)
  tumor_volume = exp(rnorm(n_clinical, mean = log(25), sd = 0.6)),

  # Ki-67 index (%)
  ki67_index = pmin(95, pmax(5, rnorm(n_clinical, mean = 35, sd = 20))),

  # Treatment arm
  treatment_arm = factor(sample(c("Control", "Drug A", "Drug B"),
                               n_clinical, replace = TRUE),
                        levels = c("Control", "Drug A", "Drug B")),

  # Response status
  response_status = factor(sample(c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"),
                                 n_clinical, replace = TRUE,
                                 prob = c(0.2, 0.3, 0.3, 0.2))),

  # Tumor stage
  tumor_stage = factor(sample(c("Stage I", "Stage II", "Stage III", "Stage IV"),
                             n_clinical, replace = TRUE,
                             prob = c(0.2, 0.3, 0.3, 0.2)),
                      levels = c("Stage I", "Stage II", "Stage III", "Stage IV"))
) %>%
  # Add correlations
  mutate(
    # Higher Ki-67 tends to have larger tumors
    tumor_volume = case_when(
      ki67_index > 60 ~ tumor_volume * 1.3,
      ki67_index > 40 ~ tumor_volume * 1.1,
      TRUE ~ tumor_volume
    )
  )

# Ensure reasonable ranges
hullplot_clinical$tumor_volume <- pmin(100, pmax(5, hullplot_clinical$tumor_volume))


# ═══════════════════════════════════════════════════════════
# Dataset 6: Small Sample Size (hullplot_small)
# ═══════════════════════════════════════════════════════════
# Small dataset for edge case testing

n_small <- 30

hullplot_small <- tibble(
  id = 1:n_small,

  # X variable
  measurement_x = c(
    rnorm(15, mean = 25, sd = 5),
    rnorm(15, mean = 45, sd = 5)
  ),

  # Y variable
  measurement_y = c(
    rnorm(15, mean = 30, sd = 6),
    rnorm(15, mean = 55, sd = 6)
  ),

  # Group
  category = factor(rep(c("Category A", "Category B"), each = 15)),

  # Size variable
  score = abs(rnorm(n_small, mean = 50, sd = 15))
)


# ═══════════════════════════════════════════════════════════
# Dataset 7: Four-Group Comparison (hullplot_fourgroup)
# ═══════════════════════════════════════════════════════════
# Four distinct groups arranged in quadrants

n_fourgroup <- 200

hullplot_fourgroup <- tibble(
  sample_id = 1:n_fourgroup,

  # Gene expression A
  gene_a = c(
    rnorm(50, mean = 20, sd = 4),  # Low A, Low B
    rnorm(50, mean = 60, sd = 4),  # High A, Low B
    rnorm(50, mean = 20, sd = 4),  # Low A, High B
    rnorm(50, mean = 60, sd = 4)   # High A, High B
  ),

  # Gene expression B
  gene_b = c(
    rnorm(50, mean = 20, sd = 4),  # Low A, Low B
    rnorm(50, mean = 20, sd = 4),  # High A, Low B
    rnorm(50, mean = 60, sd = 4),  # Low A, High B
    rnorm(50, mean = 60, sd = 4)   # High A, High B
  ),

  # Molecular subtype
  subtype = factor(rep(c("Subtype 1", "Subtype 2", "Subtype 3", "Subtype 4"), each = 50),
                  levels = c("Subtype 1", "Subtype 2", "Subtype 3", "Subtype 4")),

  # Survival months
  survival_months = abs(rnorm(n_fourgroup, mean = 36, sd = 18)),

  # Grade
  histologic_grade = factor(sample(c("Grade 1", "Grade 2", "Grade 3"),
                                  n_fourgroup, replace = TRUE))
)


# ═══════════════════════════════════════════════════════════
# Dataset 8: Unbalanced Groups (hullplot_unbalanced)
# ═══════════════════════════════════════════════════════════
# Groups with very different sample sizes

n_unbalanced <- 160

hullplot_unbalanced <- tibble(
  observation_id = 1:n_unbalanced,

  # Biomarker 1
  biomarker1 = c(
    rnorm(100, mean = 35, sd = 8),  # Majority group
    rnorm(40, mean = 60, sd = 8),   # Moderate group
    rnorm(20, mean = 80, sd = 6)    # Small group
  ),

  # Biomarker 2
  biomarker2 = c(
    rnorm(100, mean = 40, sd = 10),  # Majority group
    rnorm(40, mean = 65, sd = 10),   # Moderate group
    rnorm(20, mean = 25, sd = 8)     # Small group
  ),

  # Risk group
  risk_group = factor(c(
    rep("Low Risk", 100),
    rep("Intermediate Risk", 40),
    rep("High Risk", 20)
  ), levels = c("Low Risk", "Intermediate Risk", "High Risk")),

  # Patient age
  age = c(
    round(rnorm(100, mean = 55, sd = 12)),
    round(rnorm(40, mean = 62, sd = 10)),
    round(rnorm(20, mean = 68, sd = 8))
  ),

  # Comorbidity count
  comorbidities = sample(0:5, n_unbalanced, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.1, 0.05, 0.05))
)

# Ensure reasonable age range
hullplot_unbalanced$age <- pmin(90, pmax(18, hullplot_unbalanced$age))


# ═══════════════════════════════════════════════════════════
# Save All Datasets in Multiple Formats
# ═══════════════════════════════════════════════════════════

datasets <- list(
  hullplot_test = hullplot_test,
  hullplot_clusters = hullplot_clusters,
  hullplot_overlap = hullplot_overlap,
  hullplot_outliers = hullplot_outliers,
  hullplot_clinical = hullplot_clinical,
  hullplot_small = hullplot_small,
  hullplot_fourgroup = hullplot_fourgroup,
  hullplot_unbalanced = hullplot_unbalanced
)

for (dataset_name in names(datasets)) {
  dataset <- datasets[[dataset_name]]

  # 1. RDA format
  assign(dataset_name, dataset)
  save(list = dataset_name,
       file = here::here("data", paste0(dataset_name, ".rda")))

  # 2. CSV format
  write.csv(dataset,
            file = here::here("data", paste0(dataset_name, ".csv")),
            row.names = FALSE)

  # 3. Excel format
  write_xlsx(dataset,
             path = here::here("data", paste0(dataset_name, ".xlsx")))

  # 4. Jamovi format (OMV)
  write_omv(dataset,
            here::here("data", paste0(dataset_name, ".omv")))

  cat("✓ Generated", dataset_name, "in all formats\n")
}


# ═══════════════════════════════════════════════════════════
# Generate Summary Documentation
# ═══════════════════════════════════════════════════════════

summary_doc <- "
# Test Data Summary: hullplot

Generated: 2026-01-05
Seed: 42
Total datasets: 8

## Dataset 1: hullplot_test (n=120)
Basic two-group scatter data with well-separated groups

Variables:
- patient_id: Patient identifier
- x: Continuous X-axis variable (Group A: mean=20, Group B: mean=50)
- y: Continuous Y-axis variable (Group A: mean=30, Group B: mean=60)
- group: Grouping variable (Group A, Group B)
- size_var: Optional size variable (distance from center)
- color_category: Optional color variable (Type 1, Type 2, Type 3)

Use for: Basic hull plot testing, two-group visualization


## Dataset 2: hullplot_clusters (n=150)
Three distinct clusters in different spatial regions

Variables:
- sample_id: Sample identifier
- x: X-axis position (Cluster 1: 15, Cluster 2: 65, Cluster 3: 40)
- y: Y-axis position (Cluster 1: 20, Cluster 2: 25, Cluster 3: 70)
- cluster: Cluster assignment (Cluster 1, Cluster 2, Cluster 3)
- biomarker: Biomarker level (varies by cluster: 10, 30, 50)
- response: Response category (Responder, Non-responder, Partial)

Use for: Three-group hull plots, cluster visualization


## Dataset 3: hullplot_overlap (n=140)
Two groups with significant spatial overlap

Variables:
- patient_id: Patient identifier
- x: X-axis measurement (Treatment A: mean=40, Treatment B: mean=50, large SD)
- y: Y-axis measurement (Treatment A: mean=40, Treatment B: mean=50, large SD)
- treatment: Treatment group (Treatment A, Treatment B)
- tumor_size: Tumor size measurement
- stage: Disease stage (Early, Advanced)

Use for: Overlapping hull testing, boundary detection


## Dataset 4: hullplot_outliers (n=100)
Two groups with added outlier points

Variables:
- observation_id: Observation identifier
- x: X-axis variable with outliers
- y: Y-axis variable with outliers
- disease_type: Disease classification (Disease A, Disease B)
- severity: Severity score
- outcome: Clinical outcome (Good, Poor)

Use for: Outlier detection testing, robust hull boundaries


## Dataset 5: hullplot_clinical (n=180)
Clinical trial data with three treatment arms

Variables:
- patient_id: Patient identifier
- tumor_volume: Tumor volume in cm³ (log-normal, 5-100)
- ki67_index: Ki-67 proliferation index (%, 5-95)
- treatment_arm: Treatment assignment (Control, Drug A, Drug B)
- response_status: Response classification (CR, PR, SD, PD)
- tumor_stage: Tumor stage (Stage I-IV)

Correlation: Higher Ki-67 → larger tumor volume

Use for: Clinical trial visualization, treatment comparison


## Dataset 6: hullplot_small (n=30)
Small sample size for edge case testing

Variables:
- id: Observation identifier
- measurement_x: X-axis measurement (Category A: 25, Category B: 45)
- measurement_y: Y-axis measurement (Category A: 30, Category B: 55)
- category: Grouping variable (Category A, Category B)
- score: Size variable

Use for: Small sample testing, minimal hull plots


## Dataset 7: hullplot_fourgroup (n=200)
Four groups arranged in quadrants based on gene expression

Variables:
- sample_id: Sample identifier
- gene_a: Gene A expression (low: 20, high: 60)
- gene_b: Gene B expression (low: 20, high: 60)
- subtype: Molecular subtype (Subtype 1-4, based on gene expression patterns)
- survival_months: Survival duration (months)
- histologic_grade: Histologic grade (Grade 1-3)

Arrangement: Four quadrants (Low A/Low B, High A/Low B, Low A/High B, High A/High B)

Use for: Four-group hull plots, quadrant visualization


## Dataset 8: hullplot_unbalanced (n=160)
Three groups with unbalanced sample sizes

Variables:
- observation_id: Observation identifier
- biomarker1: First biomarker (Low Risk: 35, Intermediate: 60, High: 80)
- biomarker2: Second biomarker (Low Risk: 40, Intermediate: 65, High: 25)
- risk_group: Risk classification (Low: n=100, Intermediate: n=40, High: n=20)
- age: Patient age (varies by risk group)
- comorbidities: Number of comorbidities (0-5)

Use for: Unbalanced group testing, different hull sizes


## File Formats

Each dataset is available in 4 formats:
- .rda: Native R format (fastest loading)
- .csv: Universal text format
- .xlsx: Excel format
- .omv: Jamovi native format

## Usage Example

```r
# Load package
library(ClinicoPath)

# Load test data
data(hullplot_test)

# Basic hull plot
hullplot(
  data = hullplot_test,
  x_var = \"x\",
  y_var = \"y\",
  group_var = \"group\"
)

# With customization
hullplot(
  data = hullplot_test,
  x_var = \"x\",
  y_var = \"y\",
  group_var = \"group\",
  size_var = \"size_var\",
  hull_concavity = 1.5,
  hull_alpha = 0.3,
  show_labels = TRUE,
  plot_title = \"Patient Groups Visualization\"
)

# Clinical trial data
data(hullplot_clinical)
hullplot(
  data = hullplot_clinical,
  x_var = \"tumor_volume\",
  y_var = \"ki67_index\",
  group_var = \"treatment_arm\",
  color_palette = \"clinical\",
  show_statistics = TRUE
)

# Four-group comparison
data(hullplot_fourgroup)
hullplot(
  data = hullplot_fourgroup,
  x_var = \"gene_a\",
  y_var = \"gene_b\",
  group_var = \"subtype\",
  size_var = \"survival_months\",
  hull_concavity = 2,
  show_labels = TRUE
)
```

## Spatial Arrangements

- **hullplot_test**: Diagonal separation (bottom-left vs top-right)
- **hullplot_clusters**: Triangular arrangement (3 corners)
- **hullplot_overlap**: Overlapping central regions
- **hullplot_outliers**: Separated groups with scattered outliers
- **hullplot_clinical**: Three overlapping clouds
- **hullplot_small**: Simple diagonal separation (small n)
- **hullplot_fourgroup**: Four-quadrant arrangement
- **hullplot_unbalanced**: Three groups with different densities
"

cat(summary_doc)
writeLines(summary_doc, here::here("HULLPLOT_TEST_DATA_SUMMARY.md"))

cat("\n✓ All datasets generated successfully!\n")
cat("✓ Summary documentation created: HULLPLOT_TEST_DATA_SUMMARY.md\n")
