# Run Systematic Checks for ClinicoPath Module Functions
# 
# This script demonstrates how to use the systematic checking system
# to evaluate all functions in the module for quality and completeness.

# Load the systematic check functions
source("systematic_check_implementation.R")

# Install required packages if not available
required_packages <- c("yaml")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if(length(missing_packages) > 0) {
  cat("Installing required packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages)
}

# 1. SINGLE FUNCTION CHECK EXAMPLE
cat("=== SINGLE FUNCTION CHECK EXAMPLE ===\n")
cat("Checking lassocox function...\n\n")

# Run systematic check on lassocox
lassocox_results <- systematic_function_check("lassocox")

# Generate detailed report
lassocox_report <- generate_detailed_report("lassocox", lassocox_results, "reports/lassocox_detailed_report.md")

# 2. PRIORITY FUNCTIONS TO CHECK SYSTEMATICALLY
# Based on the module, here are key functions to prioritize:

priority_functions <- c(
  # Survival Analysis Functions
  "lassocox",           # LASSO Cox Regression
  "survival",           # Basic Survival Analysis  
  "multisurvival",      # Multi-group Survival
  "simonmakuch",        # Time-dependent Survival
  "competingsurvival",  # Competing Risks
  
  # Decision Analysis Functions
  "decisiongraph",      # Decision Trees/Markov
  "decision",           # Decision Analysis
  "decisioncurve",      # Decision Curves
  
  # Descriptive Functions
  "crosstable",         # Cross Tables
  "tableone",           # Table One
  "gtsummary",          # GT Summary Tables
  
  # Plotting Functions
  "jjbarstats",         # Bar Plots with Stats
  "jjscatterstats",     # Scatter Plots with Stats
  "survivalplot",       # Survival Plots
  
  # Diagnostic Functions
  "coxdiagnostics",     # Cox Model Diagnostics
  "agreement",          # Agreement Analysis
  "classification"      # Classification Analysis
)

# 3. BATCH CHECK PRIORITY FUNCTIONS
cat("\n=== BATCH CHECKING PRIORITY FUNCTIONS ===\n")

priority_results <- list()
priority_summary <- data.frame(
  Function = character(),
  Status = character(),
  Issues = numeric(),
  FilesOK = logical(),
  OptionsUsed = character(),
  OutputsPopulated = character(),
  ErrorHandling = numeric(),
  stringsAsFactors = FALSE
)

for(func_name in priority_functions) {
  cat("\n", strrep("-", 30), "\n")
  cat("Checking:", func_name, "\n")
  
  # Check if function files exist
  files_exist <- all(file.exists(c(
    file.path("jamovi", paste0(func_name, ".a.yaml")),
    file.path("R", paste0(func_name, ".b.R")),
    file.path("jamovi", paste0(func_name, ".r.yaml"))
  )))
  
  if(files_exist) {
    tryCatch({
      results <- systematic_function_check(func_name)
      priority_results[[func_name]] <- results
      
      # Add to summary
      status <- if(length(results$recommendations) == 0) "PASS" else 
                if(length(results$recommendations) <= 2) "MINOR_ISSUES" else "NEEDS_WORK"
      
      options_ratio <- if(!is.null(results$option_usage)) {
        paste0(sum(results$option_usage), "/", length(results$option_usage))
      } else "N/A"
      
      outputs_ratio <- if(!is.null(results$output_population)) {
        paste0(sum(results$output_population), "/", length(results$output_population))
      } else "N/A"
      
      error_count <- if(!is.null(results$error_handling)) sum(results$error_handling) else 0
      
      priority_summary <- rbind(priority_summary, data.frame(
        Function = func_name,
        Status = status,
        Issues = length(results$recommendations),
        FilesOK = all(results$file_integrity[1:4]),
        OptionsUsed = options_ratio,
        OutputsPopulated = outputs_ratio,
        ErrorHandling = error_count,
        stringsAsFactors = FALSE
      ))
      
    }, error = function(e) {
      cat("  ❌ Error checking", func_name, ":", e$message, "\n")
      priority_summary <<- rbind(priority_summary, data.frame(
        Function = func_name,
        Status = "ERROR",
        Issues = NA,
        FilesOK = FALSE,
        OptionsUsed = "ERROR",
        OutputsPopulated = "ERROR", 
        ErrorHandling = 0,
        stringsAsFactors = FALSE
      ))
    })
  } else {
    cat("  ⚠️ Missing files for", func_name, "\n")
    priority_summary <- rbind(priority_summary, data.frame(
      Function = func_name,
      Status = "MISSING_FILES",
      Issues = NA,
      FilesOK = FALSE,
      OptionsUsed = "N/A",
      OutputsPopulated = "N/A",
      ErrorHandling = 0,
      stringsAsFactors = FALSE
    ))
  }
}

# 4. GENERATE COMPREHENSIVE SUMMARY
cat("\n=== COMPREHENSIVE SUMMARY ===\n")

# Create reports directory
if(!dir.exists("reports")) dir.create("reports")

# Save summary table
write.csv(priority_summary, "reports/function_check_summary.csv", row.names = FALSE)

# Generate summary statistics
total_functions <- nrow(priority_summary)
passed_functions <- sum(priority_summary$Status == "PASS", na.rm = TRUE)
minor_issues <- sum(priority_summary$Status == "MINOR_ISSUES", na.rm = TRUE)
needs_work <- sum(priority_summary$Status == "NEEDS_WORK", na.rm = TRUE)
missing_or_error <- sum(priority_summary$Status %in% c("MISSING_FILES", "ERROR"), na.rm = TRUE)

cat("Total functions checked:", total_functions, "\n")
cat("✅ Passed:", passed_functions, "\n")
cat("⚠️ Minor issues:", minor_issues, "\n")
cat("❌ Needs work:", needs_work, "\n")
cat("🚫 Missing/Error:", missing_or_error, "\n")

# Identify functions needing attention
needs_attention <- priority_summary[priority_summary$Status %in% c("NEEDS_WORK", "MINOR_ISSUES"), ]
if(nrow(needs_attention) > 0) {
  cat("\nFunctions needing attention:\n")
  for(i in 1:nrow(needs_attention)) {
    cat("  -", needs_attention$Function[i], "(", needs_attention$Status[i], ")\n")
  }
}

# 5. GENERATE NEXT STEPS REPORT
next_steps <- c(
  "# ClinicoPath Module Quality Assessment Report",
  "",
  paste("**Date:**", Sys.Date()),
  paste("**Functions Checked:**", total_functions),
  "",
  "## Summary Statistics",
  paste("- ✅ **Passed:**", passed_functions, "functions"),
  paste("- ⚠️ **Minor Issues:**", minor_issues, "functions"),
  paste("- ❌ **Needs Work:**", needs_work, "functions"),
  paste("- 🚫 **Missing/Error:**", missing_or_error, "functions"),
  "",
  "## Priority Actions",
  ""
)

if(needs_work > 0) {
  needs_work_funcs <- priority_summary[priority_summary$Status == "NEEDS_WORK", "Function"]
  next_steps <- c(next_steps, "### High Priority (Needs Work)", "")
  for(func in needs_work_funcs) {
    next_steps <- c(next_steps, paste("- [ ] Fix", func, "- critical issues found"))
  }
  next_steps <- c(next_steps, "")
}

if(minor_issues > 0) {
  minor_issue_funcs <- priority_summary[priority_summary$Status == "MINOR_ISSUES", "Function"]
  next_steps <- c(next_steps, "### Medium Priority (Minor Issues)", "")
  for(func in minor_issue_funcs) {
    next_steps <- c(next_steps, paste("- [ ] Improve", func, "- minor enhancements needed"))
  }
  next_steps <- c(next_steps, "")
}

if(missing_or_error > 0) {
  missing_funcs <- priority_summary[priority_summary$Status %in% c("MISSING_FILES", "ERROR"), "Function"]
  next_steps <- c(next_steps, "### Implementation Required", "")
  for(func in missing_funcs) {
    next_steps <- c(next_steps, paste("- [ ] Implement", func, "- missing or broken"))
  }
}

writeLines(next_steps, "reports/quality_assessment_report.md")

cat("\nReports generated:\n")
cat("- reports/function_check_summary.csv\n")
cat("- reports/quality_assessment_report.md\n") 
cat("- reports/lassocox_detailed_report.md\n")

cat("\n🎯 Use these reports to systematically improve module quality!\n")

# Return summary for further analysis
invisible(list(
  summary = priority_summary,
  detailed_results = priority_results,
  next_steps = next_steps
))