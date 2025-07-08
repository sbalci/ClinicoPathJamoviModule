# Create test data for jscattermore function
# High-performance scatter plots using scattermore package

# Load required libraries
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# 1. Basic scatter plot data with linear relationship
jscattermore_basic <- data.frame(
    x = rnorm(1000, mean = 10, sd = 3),
    y = 2 * rnorm(1000, mean = 10, sd = 3) + 5 + rnorm(1000, sd = 1),
    id = 1:1000
)

# 2. Large dataset for performance testing (10,000 points)
jscattermore_large <- data.frame(
    x = rnorm(10000),
    y = rnorm(10000) + 0.3 * rnorm(10000),
    group = factor(sample(c("Low", "Medium", "High"), 10000, replace = TRUE, prob = c(0.3, 0.4, 0.3))),
    size_variable = runif(10000, 0.5, 2.5),
    color_continuous = runif(10000, 0, 100),
    performance_test = "large_dataset"
)

# 3. Multi-group data with different characteristics
n_per_group <- 300
jscattermore_groups <- rbind(
    data.frame(
        x = rnorm(n_per_group, mean = 0, sd = 1),
        y = rnorm(n_per_group, mean = 0, sd = 1),
        group = "Cluster_A",
        treatment = factor("Control"),
        outcome = rnorm(n_per_group, mean = 50, sd = 10),
        size_var = runif(n_per_group, 0.8, 1.2)
    ),
    data.frame(
        x = rnorm(n_per_group, mean = 3, sd = 1.2),
        y = rnorm(n_per_group, mean = 2, sd = 0.8),
        group = "Cluster_B",
        treatment = factor("Treatment_1"),
        outcome = rnorm(n_per_group, mean = 65, sd = 12),
        size_var = runif(n_per_group, 1.2, 1.8)
    ),
    data.frame(
        x = rnorm(n_per_group, mean = -2, sd = 0.8),
        y = rnorm(n_per_group, mean = 3, sd = 1.5),
        group = "Cluster_C",
        treatment = factor("Treatment_2"),
        outcome = rnorm(n_per_group, mean = 45, sd = 8),
        size_var = runif(n_per_group, 0.5, 1.0)
    )
)

# 4. Time series scatter data
jscattermore_timeseries <- data.frame(
    time = 1:500,
    signal = sin(seq(0, 4*pi, length.out = 500)) + rnorm(500, sd = 0.2),
    noise = rnorm(500),
    trend = seq(0, 10, length.out = 500) + rnorm(500, sd = 0.5),
    phase = factor(rep(c("Phase1", "Phase2", "Phase3", "Phase4"), each = 125)),
    intensity = abs(rnorm(500, mean = 1, sd = 0.3))
)

# 5. Non-linear relationships data
x_vals <- seq(-3, 3, length.out = 800)
jscattermore_nonlinear <- data.frame(
    x = x_vals,
    quadratic = x_vals^2 + rnorm(800, sd = 0.5),
    exponential = exp(x_vals/2) + rnorm(800, sd = 0.3),
    logarithmic = log(abs(x_vals) + 1) + rnorm(800, sd = 0.2),
    sinusoidal = sin(2 * x_vals) + rnorm(800, sd = 0.1),
    relationship_type = factor(rep(c("Quadratic", "Exponential", "Logarithmic", "Sinusoidal"), each = 200))
)

# 6. Real-world simulation: Medical research data
jscattermore_medical <- data.frame(
    patient_id = 1:600,
    age = round(rnorm(600, mean = 55, sd = 15)),
    bmi = rnorm(600, mean = 26, sd = 4),
    blood_pressure_systolic = round(rnorm(600, mean = 130, sd = 20)),
    blood_pressure_diastolic = round(rnorm(600, mean = 85, sd = 10)),
    cholesterol = rnorm(600, mean = 200, sd = 30),
    glucose = rnorm(600, mean = 110, sd = 25),
    treatment_group = factor(sample(c("Placebo", "Drug_A", "Drug_B"), 600, replace = TRUE)),
    gender = factor(sample(c("Male", "Female"), 600, replace = TRUE, prob = c(0.45, 0.55))),
    severity_score = runif(600, 1, 10),
    response_time = abs(rnorm(600, mean = 30, sd = 10))
) %>%
    mutate(
        # Create realistic relationships
        blood_pressure_systolic = blood_pressure_systolic + 0.5 * age + 2 * bmi,
        cholesterol = cholesterol + 0.3 * age + 1.5 * bmi,
        glucose = glucose + 0.2 * age + 0.8 * bmi
    )

# 7. High-dimensional scatter data (many variables)
jscattermore_highdim <- data.frame(
    replicate(20, rnorm(400))
) %>%
    setNames(paste0("var", 1:20)) %>%
    mutate(
        cluster = factor(kmeans(., centers = 4)$cluster),
        primary_component = var1 + var2 + var3,
        secondary_component = var4 - var5 + var6,
        interaction_term = var1 * var2,
        composite_score = rowSums(.[1:10]) / 10
    )

# 8. Extreme values and outliers data
jscattermore_outliers <- data.frame(
    x = c(rnorm(450), rnorm(25, mean = 10, sd = 1), rnorm(25, mean = -8, sd = 1)),
    y = c(rnorm(450), rnorm(25, mean = 12, sd = 1), rnorm(25, mean = -10, sd = 1)),
    outlier_type = factor(c(rep("Normal", 450), rep("High_Outlier", 25), rep("Low_Outlier", 25))),
    magnitude = c(rnorm(450, mean = 1, sd = 0.2), 
                  runif(25, 3, 5), 
                  runif(25, 3, 5)),
    influence = c(rep("Standard", 450), rep("High_Influence", 50))
)

# 9. Missing values scenario
jscattermore_missing <- jscattermore_basic
# Introduce missing values in different patterns
jscattermore_missing$x[sample(1:1000, 50)] <- NA  # Random missing
jscattermore_missing$y[sample(1:1000, 30)] <- NA  # Random missing
jscattermore_missing$both_missing <- jscattermore_missing$x
jscattermore_missing$both_missing[sample(1:1000, 20)] <- NA
jscattermore_missing$y[is.na(jscattermore_missing$both_missing)] <- NA

# 10. Perfect correlation data (for testing edge cases)
jscattermore_perfect <- data.frame(
    x = 1:200,
    y_perfect_pos = 2 * (1:200) + 5,  # Perfect positive correlation
    y_perfect_neg = -1.5 * (1:200) + 100,  # Perfect negative correlation
    y_no_corr = sample(1:200),  # No correlation (shuffled)
    constant_x = rep(5, 200),  # Constant X
    constant_y = rep(10, 200),  # Constant Y
    correlation_type = factor(rep(c("Perfect_Positive", "Perfect_Negative", "No_Correlation", "Constant"), each = 50))
)

# 11. Very large dataset for stress testing (50,000 points)
if (interactive()) {  # Only create if in interactive session to avoid memory issues
    jscattermore_stress <- data.frame(
        x = rnorm(50000),
        y = 0.8 * rnorm(50000) + 0.2 * rnorm(50000),
        batch = factor(rep(1:100, each = 500)),
        random_group = factor(sample(LETTERS[1:10], 50000, replace = TRUE)),
        size_factor = runif(50000, 0.1, 3.0),
        color_gradient = seq(1, 100, length.out = 50000) + rnorm(50000, sd = 5),
        test_type = "stress_test"
    )
} else {
    # Smaller version for automated testing
    jscattermore_stress <- data.frame(
        x = rnorm(5000),
        y = 0.8 * rnorm(5000) + 0.2 * rnorm(5000),
        batch = factor(rep(1:20, each = 250)),
        random_group = factor(sample(LETTERS[1:5], 5000, replace = TRUE)),
        size_factor = runif(5000, 0.1, 3.0),
        color_gradient = seq(1, 100, length.out = 5000) + rnorm(5000, sd = 5),
        test_type = "stress_test_reduced"
    )
}

# 12. Color palette testing data
jscattermore_colors <- data.frame(
    x = rep(1:10, each = 10),
    y = rep(1:10, times = 10),
    viridis_test = seq(0, 1, length.out = 100),
    plasma_test = seq(0, 1, length.out = 100),
    categorical = factor(rep(LETTERS[1:10], each = 10)),
    gradient_blue = seq(0, 100, length.out = 100),
    gradient_red = seq(100, 0, length.out = 100),
    palette_type = factor(rep(c("Sequential", "Diverging"), each = 50))
)

# Create comprehensive test descriptions
test_descriptions <- data.frame(
    dataset_name = c(
        "jscattermore_basic",
        "jscattermore_large", 
        "jscattermore_groups",
        "jscattermore_timeseries",
        "jscattermore_nonlinear",
        "jscattermore_medical",
        "jscattermore_highdim",
        "jscattermore_outliers",
        "jscattermore_missing",
        "jscattermore_perfect",
        "jscattermore_stress",
        "jscattermore_colors"
    ),
    description = c(
        "Basic scatter plot data with linear relationship - 1,000 points",
        "Large dataset for performance testing - 10,000 points with groups",
        "Multi-group data with different characteristics - 900 points across 3 clusters",
        "Time series scatter data with periodic patterns - 500 time points",
        "Non-linear relationships (quadratic, exponential, log, sin) - 800 points",
        "Medical research simulation with realistic variable relationships - 600 patients",
        "High-dimensional data with 20 variables and clustering - 400 points",
        "Data with extreme values and outliers for robustness testing - 500 points",
        "Dataset with various missing value patterns - 1,000 points with NAs",
        "Perfect correlation scenarios and edge cases - 200 points",
        "Very large dataset for stress testing - up to 50,000 points",
        "Color palette and visualization testing data - 100 points in grid"
    ),
    use_case = c(
        "Basic functionality testing",
        "Performance and scalability testing",
        "Group comparison and faceting",
        "Time series and trend analysis", 
        "Non-linear relationship detection",
        "Real-world medical research scenarios",
        "High-dimensional data visualization",
        "Outlier detection and robustness",
        "Missing data handling",
        "Edge case and correlation testing",
        "Stress testing and memory usage",
        "Color scheme and aesthetic testing"
    ),
    points = c(1000, 10000, 900, 500, 800, 600, 400, 500, 1000, 200, 
               ifelse(interactive(), 50000, 5000), 100),
    variables = c(3, 6, 6, 6, 6, 11, 25, 5, 4, 7, 7, 8)
)

# Print summary
cat("Created", nrow(test_descriptions), "test datasets for jscattermore function:\n\n")
print(test_descriptions)

cat("\n\nDataset memory usage summary:\n")
for (dataset_name in test_descriptions$dataset_name) {
    if (exists(dataset_name)) {
        obj_size <- format(object.size(get(dataset_name)), units = "MB")
        cat(sprintf("%-25s: %s\n", dataset_name, obj_size))
    }
}

cat("\n\nExample usage:\n")
cat("library(ClinicoPath)\n")
cat("result <- jscattermore(\n")
cat("    data = jscattermore_basic,\n")
cat("    x_var = 'x',\n")
cat("    y_var = 'y'\n")
cat(")\n")

# Save datasets if requested
if (exists("save_datasets") && save_datasets) {
    save_dir <- "data"
    if (!dir.exists(save_dir)) dir.create(save_dir)
    
    datasets_to_save <- list(
        jscattermore_basic = jscattermore_basic,
        jscattermore_large = jscattermore_large,
        jscattermore_groups = jscattermore_groups,
        jscattermore_timeseries = jscattermore_timeseries,
        jscattermore_nonlinear = jscattermore_nonlinear,
        jscattermore_medical = jscattermore_medical,
        jscattermore_highdim = jscattermore_highdim,
        jscattermore_outliers = jscattermore_outliers,
        jscattermore_missing = jscattermore_missing,
        jscattermore_perfect = jscattermore_perfect,
        jscattermore_stress = jscattermore_stress,
        jscattermore_colors = jscattermore_colors,
        jscattermore_test_descriptions = test_descriptions
    )
    
    save(list = names(datasets_to_save), 
         file = file.path(save_dir, "jscattermore_test_data.RData"))
    
    cat("\nTest datasets saved to:", file.path(save_dir, "jscattermore_test_data.RData"), "\n")
}

# Clean up large objects if not in interactive session
if (!interactive()) {
    rm(jscattermore_stress)
    gc()
}