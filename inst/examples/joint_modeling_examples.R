# Joint Modeling Examples for ClinicoPath
# These examples demonstrate how to use the joint modeling datasets
# and provide template code for testing and validation

# Load required packages
suppressPackageStartupMessages({
  library(ClinicoPath)
  library(dplyr)
  library(ggplot2)
  library(survival)
})

# ============================================================================
# Example 1: Basic Data Exploration
# ============================================================================

example_basic_exploration <- function() {
  cat("=== Example 1: Basic Data Exploration ===\n\n")
  
  # Load simple cancer data (best for learning)
  data(simple_cancer_data)
  
  # Basic summary
  cat("Dataset Overview:\n")
  print(str(simple_cancer_data))
  
  # Patient-level summary
  patient_summary <- simple_cancer_data %>%
    group_by(patient_id, treatment) %>%
    summarise(
      n_visits = n(),
      baseline_marker = first(tumor_marker),
      final_marker = last(tumor_marker),
      marker_change = final_marker - baseline_marker,
      survival_time = first(survival_time),
      event = first(progression_status),
      .groups = 'drop'
    )
  
  cat("\nPatient-level Summary:\n")
  print(summary(patient_summary))
  
  # Treatment comparison
  treatment_comparison <- patient_summary %>%
    group_by(treatment) %>%
    summarise(
      patients = n(),
      mean_visits = round(mean(n_visits), 1),
      baseline_marker_mean = round(mean(baseline_marker), 1),
      marker_change_mean = round(mean(marker_change), 1),
      event_rate = round(mean(event) * 100, 1),
      median_survival = round(median(survival_time), 1),
      .groups = 'drop'
    )
  
  cat("\nTreatment Comparison:\n")
  print(treatment_comparison)
  
  return(patient_summary)
}

# ============================================================================
# Example 2: Data Visualization Templates
# ============================================================================

example_visualization_templates <- function() {
  cat("\n=== Example 2: Data Visualization Templates ===\n\n")
  
  # Load PSA data for more interesting patterns
  data(psa_joint_data)
  
  # 1. Individual trajectory plot
  create_trajectory_plot <- function(data, id_col, time_col, biomarker_col, 
                                   group_col = NULL, n_sample = 20) {
    
    set.seed(123)
    sample_ids <- sample(unique(data[[id_col]]), min(n_sample, length(unique(data[[id_col]]))))
    sample_data <- data %>% filter(!!sym(id_col) %in% sample_ids)
    
    p <- ggplot(sample_data, aes_string(x = time_col, y = biomarker_col))
    
    if (!is.null(group_col)) {
      p <- p + geom_line(aes_string(group = id_col, color = group_col), alpha = 0.7) +
               geom_point(aes_string(color = group_col), alpha = 0.8)
    } else {
      p <- p + geom_line(aes_string(group = id_col), alpha = 0.7, color = "steelblue") +
               geom_point(alpha = 0.8, color = "steelblue")
    }
    
    p <- p + 
      labs(title = paste("Individual", tools::toTitleCase(biomarker_col), "Trajectories"),
           subtitle = paste("Sample of", n_sample, "patients"),
           x = "Time (months)", 
           y = tools::toTitleCase(gsub("_", " ", biomarker_col))) +
      theme_bw() +
      theme(legend.position = "bottom")
    
    return(p)
  }
  
  # 2. Population trajectory plot  
  create_population_plot <- function(data, time_col, biomarker_col, group_col = NULL) {
    
    p <- ggplot(data, aes_string(x = time_col, y = biomarker_col))
    
    if (!is.null(group_col)) {
      p <- p + geom_smooth(aes_string(color = group_col), method = "loess", se = TRUE, size = 1.2)
    } else {
      p <- p + geom_smooth(method = "loess", se = TRUE, size = 1.2, color = "steelblue")
    }
    
    p <- p + 
      labs(title = paste("Population", tools::toTitleCase(biomarker_col), "Trajectory"),
           x = "Time (months)", 
           y = tools::toTitleCase(gsub("_", " ", biomarker_col))) +
      theme_bw() +
      theme(legend.position = "bottom")
    
    return(p)
  }
  
  # 3. Event-time distribution plot
  create_event_plot <- function(data, id_col, survival_col, event_col, group_col = NULL) {
    
    # Get unique patients
    surv_data <- data %>%
      select(all_of(c(id_col, survival_col, event_col, group_col))) %>%
      distinct()
    
    # Create survival object
    surv_obj <- survival::Surv(surv_data[[survival_col]], surv_data[[event_col]])
    
    if (!is.null(group_col)) {
      km_fit <- survival::survfit(surv_obj ~ surv_data[[group_col]])
      
      # Extract survival data for plotting
      surv_summary <- summary(km_fit)
      plot_data <- data.frame(
        time = surv_summary$time,
        surv = surv_summary$surv,
        group = surv_summary$strata
      )
      
      p <- ggplot(plot_data, aes(x = time, y = surv, color = group)) +
        geom_step(size = 1.2) +
        labs(title = "Kaplan-Meier Survival Curves",
             x = "Time (months)", y = "Survival Probability") +
        theme_bw()
    } else {
      km_fit <- survival::survfit(surv_obj ~ 1)
      plot_data <- data.frame(time = km_fit$time, surv = km_fit$surv)
      
      p <- ggplot(plot_data, aes(x = time, y = surv)) +
        geom_step(size = 1.2, color = "steelblue") +
        labs(title = "Kaplan-Meier Survival Curve",
             x = "Time (months)", y = "Survival Probability") +
        theme_bw()
    }
    
    return(p)
  }
  
  # Generate example plots
  cat("Creating trajectory plots...\n")
  p1 <- create_trajectory_plot(psa_joint_data, "patient_id", "visit_time", "psa_level", "stage")
  p2 <- create_population_plot(psa_joint_data, "visit_time", "psa_level", "stage")
  p3 <- create_event_plot(psa_joint_data, "patient_id", "survival_time", "death_status", "stage")
  
  # Display plots
  print(p1)
  print(p2)  
  print(p3)
  
  cat("✅ Visualization templates created successfully!\n")
}

# ============================================================================
# Example 3: Data Quality Assessment Function
# ============================================================================

assess_joint_data_quality <- function(data, id_col, time_col, biomarker_col, 
                                     survival_col, event_col, covariates = NULL) {
  
  cat("=== Joint Modeling Data Quality Assessment ===\n\n")
  
  # Basic structure
  n_obs <- nrow(data)
  n_patients <- length(unique(data[[id_col]]))
  avg_visits <- round(n_obs / n_patients, 1)
  
  cat("1. Data Structure:\n")
  cat("   - Total observations:", n_obs, "\n")
  cat("   - Unique patients:", n_patients, "\n")
  cat("   - Average visits per patient:", avg_visits, "\n\n")
  
  # Missing values
  cat("2. Missing Values:\n")
  missing_counts <- data %>%
    summarise(
      across(all_of(c(id_col, time_col, biomarker_col, survival_col, event_col)), 
             ~ sum(is.na(.)), .names = "missing_{.col}")
    )
  
  for (col in names(missing_counts)) {
    var_name <- gsub("missing_", "", col)
    missing_count <- missing_counts[[col]]
    missing_pct <- round(missing_count / n_obs * 100, 1)
    cat("   -", var_name, ":", missing_count, "(", missing_pct, "%)\n")
  }
  cat("\n")
  
  # Visit patterns
  visit_dist <- data %>%
    count(!!sym(id_col)) %>%
    count(n, name = "patients")
  
  cat("3. Visit Distribution:\n")
  for (i in 1:nrow(visit_dist)) {
    cat("   -", visit_dist$n[i], "visits:", visit_dist$patients[i], "patients\n")
  }
  cat("\n")
  
  # Biomarker characteristics
  biomarker_stats <- data %>%
    summarise(
      min_value = round(min(!!sym(biomarker_col), na.rm = TRUE), 2),
      max_value = round(max(!!sym(biomarker_col), na.rm = TRUE), 2),
      median_value = round(median(!!sym(biomarker_col), na.rm = TRUE), 2),
      n_negative = sum(!!sym(biomarker_col) < 0, na.rm = TRUE),
      n_zero = sum(!!sym(biomarker_col) == 0, na.rm = TRUE)
    )
  
  cat("4. Biomarker Characteristics:\n")
  cat("   - Range:", biomarker_stats$min_value, "-", biomarker_stats$max_value, "\n")
  cat("   - Median:", biomarker_stats$median_value, "\n")
  cat("   - Negative values:", biomarker_stats$n_negative, "\n")
  cat("   - Zero values:", biomarker_stats$n_zero, "\n\n")
  
  # Survival data
  surv_data <- data %>%
    select(all_of(c(id_col, survival_col, event_col))) %>%
    distinct()
  
  event_rate <- round(mean(surv_data[[event_col]]) * 100, 1)
  median_followup <- round(median(surv_data[[survival_col]]), 1)
  max_followup <- round(max(surv_data[[survival_col]]), 1)
  
  cat("5. Survival Data:\n")
  cat("   - Event rate:", event_rate, "%\n")
  cat("   - Median follow-up:", median_followup, "months\n")
  cat("   - Maximum follow-up:", max_followup, "months\n\n")
  
  # Time consistency check
  time_issues <- data %>%
    group_by(!!sym(id_col)) %>%
    summarise(
      max_visit = max(!!sym(time_col), na.rm = TRUE),
      survival_time = first(!!sym(survival_col)),
      .groups = 'drop'
    ) %>%
    filter(max_visit > survival_time)
  
  cat("6. Time Consistency:\n")
  if (nrow(time_issues) > 0) {
    cat("   ⚠️ Warning:", nrow(time_issues), "patients have visits after survival time\n")
  } else {
    cat("   ✅ All visit times are before or equal to survival time\n")
  }
  cat("\n")
  
  # Adequacy assessment
  cat("7. Adequacy for Joint Modeling:\n")
  
  adequate <- TRUE
  
  if (n_patients < 50) {
    cat("   ⚠️ Small sample size (<50 patients) - consider larger study\n")
    adequate <- FALSE
  }
  
  if (event_rate < 10) {
    cat("   ⚠️ Low event rate (<10%) - consider longer follow-up\n")
    adequate <- FALSE  
  }
  
  if (avg_visits < 3) {
    cat("   ⚠️ Few visits per patient (<3) - limited trajectory information\n")
    adequate <- FALSE
  }
  
  missing_biomarker_pct <- sum(is.na(data[[biomarker_col]])) / n_obs * 100
  if (missing_biomarker_pct > 15) {
    cat("   ⚠️ High missing biomarker rate (>15%) - consider imputation\n")
    adequate <- FALSE
  }
  
  if (adequate) {
    cat("   ✅ Data appears adequate for joint modeling\n")
  }
  
  cat("\n")
  
  # Return summary
  return(list(
    n_patients = n_patients,
    n_observations = n_obs,
    avg_visits = avg_visits,
    event_rate = event_rate,
    adequate = adequate,
    issues = nrow(time_issues)
  ))
}

# ============================================================================
# Example 4: Simulated Analysis Results  
# ============================================================================

simulate_joint_analysis_results <- function(dataset_name = "simple_cancer_data") {
  
  cat("=== Simulated Joint Analysis Results ===\n")
  cat("(This shows what you might expect from the joint modeling analysis)\n\n")
  
  # Load appropriate dataset
  if (dataset_name == "simple_cancer_data") {
    data(simple_cancer_data)
    biomarker_name <- "tumor_marker"
    outcome_name <- "cancer progression"
  } else if (dataset_name == "psa_joint_data") {
    data(psa_joint_data)  
    biomarker_name <- "PSA"
    outcome_name <- "prostate cancer death"
  }
  
  cat("Dataset:", dataset_name, "\n")
  cat("Biomarker:", biomarker_name, "\n")
  cat("Outcome:", outcome_name, "\n\n")
  
  # Simulated results based on typical joint modeling output
  cat("LONGITUDINAL MODEL RESULTS:\n")
  cat("Fixed Effects:\n")
  cat("  Intercept:", ifelse(dataset_name == "simple_cancer_data", "45.2", "8.1"), "± 2.3\n")
  cat("  Time slope:", ifelse(dataset_name == "simple_cancer_data", "2.8", "0.12"), "± 0.4\n")
  cat("  Treatment effect:", ifelse(dataset_name == "simple_cancer_data", "-1.9 ± 0.8 (p=0.02)", "N/A"), "\n")
  cat("Random Effects SD:\n")
  cat("  Intercept:", ifelse(dataset_name == "simple_cancer_data", "12.4", "3.2"), "\n")
  cat("  Slope:", ifelse(dataset_name == "simple_cancer_data", "1.8", "0.08"), "\n")
  cat("Model Fit: AIC =", ifelse(dataset_name == "simple_cancer_data", "4567", "8234"), "\n\n")
  
  cat("SURVIVAL MODEL RESULTS:\n")
  cat("Baseline Hazard Model: Cox Proportional Hazards\n")
  if (dataset_name == "simple_cancer_data") {
    cat("Age HR: 1.02 (95% CI: 0.98-1.06, p=0.31)\n")
  } else {
    cat("Age HR: 1.04 (95% CI: 1.01-1.08, p=0.02)\n")
    cat("Stage T3 vs T1 HR: 2.1 (95% CI: 1.2-3.7, p=0.01)\n")
    cat("Gleason 8+ vs 6-7 HR: 1.8 (95% CI: 1.1-3.0, p=0.02)\n")
  }
  cat("\n")
  
  cat("🎯 JOINT MODEL KEY RESULT:\n")
  association <- ifelse(dataset_name == "simple_cancer_data", "0.31", "0.18")
  association_se <- ifelse(dataset_name == "simple_cancer_data", "0.09", "0.06")
  association_p <- ifelse(dataset_name == "simple_cancer_data", "0.001", "0.003")
  hr <- round(exp(as.numeric(association)), 2)
  
  cat("Association Parameter (α):", association, "± ", association_se, "(p =", association_p, ")\n")
  cat("Hazard Ratio per unit biomarker: ", hr, "\n")
  cat("Interpretation: Each 1-unit increase in", biomarker_name, "\n")
  cat("                is associated with a", round((hr - 1) * 100), "% increase\n")
  cat("                in the hazard of", outcome_name, "\n\n")
  
  cat("DYNAMIC PREDICTIONS:\n")
  cat("Individual risk profiles calculated for all patients\n")
  cat("Prediction horizons: 6 months, 1 year, 2 years\n")
  cat("Risk stratification: Low (<10%), Moderate (10-30%), High (>30%)\n\n")
  
  cat("MODEL VALIDATION:\n")
  cat("Concordance Index (C-index):", ifelse(dataset_name == "simple_cancer_data", "0.73", "0.68"), "\n")
  cat("Time-dependent AUC at 1 year:", ifelse(dataset_name == "simple_cancer_data", "0.71", "0.66"), "\n")
  cat("Cross-validated C-index:", ifelse(dataset_name == "simple_cancer_data", "0.70", "0.64"), "\n")
  cat("Integrated Brier Score:", ifelse(dataset_name == "simple_cancer_data", "0.12", "0.08"), "\n\n")
  
  cat("CLINICAL INTERPRETATION:\n")
  if (dataset_name == "simple_cancer_data") {
    cat("• Strong positive association between tumor marker and progression risk\n")
    cat("• Experimental treatment appears to slow marker increase\n")
    cat("• Dynamic predictions can guide monitoring frequency\n")
    cat("• Model shows good discriminative ability (C-index = 0.73)\n")
  } else {
    cat("• Moderate positive association between PSA and death risk\n")
    cat("• Higher grade and stage remain important prognostic factors\n")
    cat("• PSA trajectory provides additional prognostic information\n")
    cat("• Model performance is adequate (C-index = 0.68)\n")
  }
  
  cat("\n✅ Analysis completed successfully!\n")
}

# ============================================================================
# Example 5: Running All Examples
# ============================================================================

run_all_examples <- function() {
  
  cat("🚀 Running All Joint Modeling Examples\n")
  cat("=====================================\n\n")
  
  # Example 1: Basic exploration
  tryCatch({
    patient_data <- example_basic_exploration()
    cat("✅ Example 1 completed\n\n")
  }, error = function(e) {
    cat("❌ Example 1 failed:", e$message, "\n\n")
  })
  
  # Example 2: Visualizations  
  tryCatch({
    example_visualization_templates()
    cat("✅ Example 2 completed\n\n")
  }, error = function(e) {
    cat("❌ Example 2 failed:", e$message, "\n\n")
  })
  
  # Example 3: Data quality assessment
  tryCatch({
    data(simple_cancer_data)
    quality_report <- assess_joint_data_quality(
      simple_cancer_data, "patient_id", "visit_time", "tumor_marker",
      "survival_time", "progression_status", "treatment"
    )
    cat("✅ Example 3 completed\n\n")
  }, error = function(e) {
    cat("❌ Example 3 failed:", e$message, "\n\n")
  })
  
  # Example 4: Simulated results
  tryCatch({
    simulate_joint_analysis_results("simple_cancer_data")
    cat("✅ Example 4 completed\n\n")
  }, error = function(e) {
    cat("❌ Example 4 failed:", e$message, "\n\n")
  })
  
  cat("🎉 All examples completed!\n")
  cat("You can now proceed with your own joint modeling analysis.\n")
}

# ============================================================================
# Main execution (uncomment to run examples)
# ============================================================================

if (interactive()) {
  cat("Joint Modeling Examples for ClinicoPath\n")
  cat("=======================================\n\n")
  cat("Available examples:\n")
  cat("1. example_basic_exploration()\n")
  cat("2. example_visualization_templates()\n") 
  cat("3. assess_joint_data_quality(data, ...)\n")
  cat("4. simulate_joint_analysis_results(dataset)\n")
  cat("5. run_all_examples()\n\n")
  cat("Type any function name to run, or run_all_examples() for everything.\n")
}

# Uncomment the next line to run all examples automatically
# run_all_examples()