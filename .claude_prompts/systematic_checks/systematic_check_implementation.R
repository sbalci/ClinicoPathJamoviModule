# Systematic Function Check Implementation Script
# This script provides automated checks for jamovi module functions

#' Systematic Function Checker for Jamovi Modules
#'
#' @param function_name Name of the function to check (e.g., "lassocox")
#' @param module_path Path to the module directory
#' @return List with check results
systematic_function_check <- function(function_name, module_path = ".") {

  cat("=== SYSTEMATIC FUNCTION CHECK ===\n")
  cat("Function:", function_name, "\n")
  cat("Date:", Sys.Date(), "\n\n")

  results <- list()

  # 1. FILE INTEGRITY CHECK
  cat("1. FILE INTEGRITY CHECK\n")
  files <- list(
    a_yaml = file.path(module_path, "jamovi", paste0(function_name, ".a.yaml")),
    b_r = file.path(module_path, "R", paste0(function_name, ".b.R")),
    r_yaml = file.path(module_path, "jamovi", paste0(function_name, ".r.yaml")),
    u_yaml = file.path(module_path, "jamovi", paste0(function_name, ".u.yaml")),
    h_r = file.path(module_path, "R", paste0(function_name, ".h.R"))
  )

  file_status <- sapply(files, file.exists)
  results$file_integrity <- file_status

  for(i in seq_along(file_status)) {
    status_symbol <- if(file_status[i]) "✅" else "❌"
    cat("  ", status_symbol, names(file_status)[i], "\n")
  }
  cat("\n")

  if(!all(file_status[1:4])) {
    cat("❌ CRITICAL: Missing required files. Cannot continue.\n")
    return(results)
  }

  # 2. LOAD AND PARSE FILES
  cat("2. PARSING CONFIGURATION FILES\n")

  # Parse YAML files
  if(requireNamespace("yaml", quietly = TRUE)) {
    a_yaml <- yaml::yaml.load_file(files$a_yaml)
    r_yaml <- yaml::yaml.load_file(files$r_yaml)
    u_yaml <- yaml::yaml.load_file(files$u_yaml)

    # Extract options from .a.yaml
    options_list <- a_yaml$options
    if(!is.null(options_list)) {
      option_names <- sapply(options_list, function(x) x$name)
      results$defined_options <- option_names
      cat("  ✅ Found", length(option_names), "options in .a.yaml\n")
    } else {
      cat("  ❌ No options found in .a.yaml\n")
    }

    # Extract outputs from .r.yaml
    items_list <- r_yaml$items
    if(!is.null(items_list)) {
      output_names <- sapply(items_list, function(x) x$name)
      results$defined_outputs <- output_names
      cat("  ✅ Found", length(output_names), "outputs in .r.yaml\n")
    } else {
      cat("  ❌ No outputs found in .r.yaml\n")
    }

  } else {
    cat("  ⚠️ yaml package not available. Install with: install.packages('yaml')\n")
  }

  # 3. CHECK .b.R FILE
  cat("\n3. BACKEND IMPLEMENTATION CHECK\n")

  if(file.exists(files$b_r)) {
    b_r_content <- readLines(files$b_r)
    results$b_r_lines <- length(b_r_content)
    cat("  ✅ .b.R file has", length(b_r_content), "lines\n")

    # Check for option usage
    if(exists("option_names")) {
      option_usage <- sapply(option_names, function(opt) {
        pattern <- paste0("self\\$options\\$", opt)
        any(grepl(pattern, b_r_content))
      })
      results$option_usage <- option_usage

      used_count <- sum(option_usage)
      cat("  📊 Options used in .b.R:", used_count, "/", length(option_names), "\n")

      if(used_count < length(option_names)) {
        unused_options <- option_names[!option_usage]
        cat("  ⚠️ Unused options:", paste(unused_options, collapse = ", "), "\n")
      }
    }

    # Check for output population
    if(exists("output_names")) {
      output_population <- sapply(output_names, function(out) {
        pattern <- paste0("self\\$results\\$", out)
        any(grepl(pattern, b_r_content))
      })
      results$output_population <- output_population

      populated_count <- sum(output_population)
      cat("  📊 Outputs populated in .b.R:", populated_count, "/", length(output_names), "\n")

      if(populated_count < length(output_names)) {
        unpopulated_outputs <- output_names[!output_population]
        cat("  ⚠️ Unpopulated outputs:", paste(unpopulated_outputs, collapse = ", "), "\n")
      }
    }

    # Check for explanatory functions
    explanatory_patterns <- c(
      "showExplanations", "showMethodologyNotes", "includeClinicalGuidance",
      "Explanation", "explanation", "Methodology", "Clinical"
    )

    explanatory_found <- sapply(explanatory_patterns, function(pattern) {
      any(grepl(pattern, b_r_content, ignore.case = TRUE))
    })

    results$explanatory_features <- explanatory_found
    cat("  📚 Explanatory features detected:", sum(explanatory_found), "/", length(explanatory_patterns), "\n")

  }

  # 4. ERROR HANDLING CHECK
  cat("\n4. ERROR HANDLING CHECK\n")

  error_patterns <- c("tryCatch", "stop\\(", "warning\\(", "if.*is.null", "if.*length.*== 0")
  error_handling <- sapply(error_patterns, function(pattern) {
    sum(grepl(pattern, b_r_content))
  })
  results$error_handling <- error_handling

  total_error_handling <- sum(error_handling)
  cat("  🛡️ Error handling patterns found:", total_error_handling, "\n")

  if(total_error_handling < 3) {
    cat("  ⚠️ Consider adding more error handling\n")
  }

  # 5. GENERATE RECOMMENDATIONS
  cat("\n5. RECOMMENDATIONS\n")

  recommendations <- character(0)

  if(exists("option_usage") && any(!option_usage)) {
    recommendations <- c(recommendations, "Remove unused options or implement their functionality")
  }

  if(exists("output_population") && any(!output_population)) {
    recommendations <- c(recommendations, "Implement population code for all defined outputs")
  }

  if(sum(explanatory_found) < 3) {
    recommendations <- c(recommendations, "Add more explanatory outputs for better user guidance")
  }

  if(total_error_handling < 3) {
    recommendations <- c(recommendations, "Implement more comprehensive error handling")
  }

  if(length(recommendations) == 0) {
    cat("  ✅ No critical issues found!\n")
  } else {
    for(i in seq_along(recommendations)) {
      cat("  ", i, ".", recommendations[i], "\n")
    }
  }

  results$recommendations <- recommendations

  # 6. SUMMARY
  cat("\n=== SUMMARY ===\n")
  cat("Function:", function_name, "\n")
  cat("Status:", if(length(recommendations) == 0) "✅ PASS" else if(length(recommendations) <= 2) "⚠️ MINOR ISSUES" else "❌ NEEDS WORK", "\n")
  cat("Issues found:", length(recommendations), "\n")

  return(invisible(results))
}

#' Check all functions in the module
#'
#' @param module_path Path to the module directory
#' @return List with results for all functions
check_all_functions <- function(module_path = ".") {

  # Find all .a.yaml files
  yaml_files <- list.files(file.path(module_path, "jamovi"), pattern = "\\.a\\.yaml$", full.names = FALSE)
  function_names <- gsub("\\.a\\.yaml$", "", yaml_files)

  cat("Found", length(function_names), "functions to check:\n")
  cat(paste(function_names, collapse = ", "), "\n\n")

  all_results <- list()

  for(func_name in function_names) {
    cat("\\n", strrep("=", 50), "\n")
    all_results[[func_name]] <- systematic_function_check(func_name, module_path)
    cat(strrep("=", 50), "\n")
  }

  return(all_results)
}

#' Generate detailed report for a function
#'
#' @param function_name Name of the function
#' @param check_results Results from systematic_function_check
#' @param output_file Optional file to save the report
generate_detailed_report <- function(function_name, check_results, output_file = NULL) {

  report <- c()
  report <- c(report, paste("# Detailed Function Report:", function_name))
  report <- c(report, paste("Date:", Sys.Date()))
  report <- c(report, paste("Time:", Sys.time()))
  report <- c(report, "")

  # File integrity
  report <- c(report, "## File Integrity")
  for(i in seq_along(check_results$file_integrity)) {
    status <- if(check_results$file_integrity[i]) "✅ Present" else "❌ Missing"
    report <- c(report, paste("-", names(check_results$file_integrity)[i], ":", status))
  }
  report <- c(report, "")

  # Options analysis
  if(!is.null(check_results$option_usage)) {
    report <- c(report, "## Options Analysis")
    report <- c(report, paste("Total options defined:", length(check_results$option_usage)))
    report <- c(report, paste("Options used:", sum(check_results$option_usage)))

    if(any(!check_results$option_usage)) {
      unused <- names(check_results$option_usage)[!check_results$option_usage]
      report <- c(report, "### Unused Options:")
      for(opt in unused) {
        report <- c(report, paste("- [ ]", opt))
      }
    }
    report <- c(report, "")
  }

  # Outputs analysis
  if(!is.null(check_results$output_population)) {
    report <- c(report, "## Outputs Analysis")
    report <- c(report, paste("Total outputs defined:", length(check_results$output_population)))
    report <- c(report, paste("Outputs populated:", sum(check_results$output_population)))

    if(any(!check_results$output_population)) {
      unpopulated <- names(check_results$output_population)[!check_results$output_population]
      report <- c(report, "### Unpopulated Outputs:")
      for(out in unpopulated) {
        report <- c(report, paste("- [ ]", out))
      }
    }
    report <- c(report, "")
  }

  # Recommendations
  if(length(check_results$recommendations) > 0) {
    report <- c(report, "## Recommendations")
    for(i in seq_along(check_results$recommendations)) {
      report <- c(report, paste(i, ".", check_results$recommendations[i]))
    }
    report <- c(report, "")
  }

  # Summary
  status <- if(length(check_results$recommendations) == 0) "PASS ✅" else
            if(length(check_results$recommendations) <= 2) "MINOR ISSUES ⚠️" else "NEEDS WORK ❌"

  report <- c(report, "## Summary")
  report <- c(report, paste("**Status:**", status))
  report <- c(report, paste("**Issues found:**", length(check_results$recommendations)))

  if(!is.null(output_file)) {
    writeLines(report, output_file)
    cat("Report saved to:", output_file, "\n")
  }

  return(report)
}

# Example usage:
# results <- systematic_function_check("lassocox")
# report <- generate_detailed_report("lassocox", results, "lassocox_check_report.md")
# all_results <- check_all_functions()
