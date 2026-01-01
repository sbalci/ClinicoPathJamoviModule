
# Auto-generator for Jamovi Module Tests
# Usage: source("tests/generate_tests.R"); generate_test_for_module("jamovi/adaptivelasso.a.yaml")

library(yaml)
library(stringr)

generate_test_for_module <- function(yaml_path, output_dir = "tests/testthat") {
  
  if (!file.exists(yaml_path)) stop("YAML file not found: ", yaml_path)
  
  # 1. Parse YAML
  mod_def <- yaml::read_yaml(yaml_path)
  module_name <- mod_def$name
  options <- mod_def$options
  
  print(paste("Generating test for:", module_name))
  
  # 2. Analyze Options to build Synthetic Data
  validation_code <- c()
  data_gen_code <- c()
  analysis_args <- c()
  
  # Start data generation
  data_gen_code <- c(data_gen_code,
                     "  # Synthetic data generation",
                     "  set.seed(123)",
                     "  n <- 50",
                     "  data <- data.frame(")
  
  df_cols <- c()
  
  # Helper to determine R type from YAML type
  get_r_type <- function(opt) {
    if (is.null(opt$permitted)) return("numeric") # default
    if ("factor" %in% opt$permitted) return("factor")
    if ("numeric" %in% opt$permitted) return("numeric")
    return("numeric")
  }
  
  for (opt in options) {
    name <- opt$name
    type <- opt$type
    
    # Handle Data options
    if (type == "Data") {
      # The main data argument for the function
      # Usually 'data = data'
    } else if (type == "Variable" || type == "Variables") {
      # Add to analysis arguments
      if (type == "Variables") {
         # Create multiple columns for Variables
         col_names <- paste0(name, 1:3)
         for (cn in col_names) {
            r_type <- get_r_type(opt)
            if (r_type == "factor") {
               df_cols <- c(df_cols, paste0("    ", cn, " = sample(c('A', 'B'), n, replace = TRUE)"))
            } else {
               df_cols <- c(df_cols, paste0("    ", cn, " = runif(n, 1, 100)"))
            }
         }
         # Pass vector of names
         # e.g. predictors = c('predictors1', 'predictors2', 'predictors3')
         val <- paste0("c(", paste0("'", col_names, "'", collapse = ", "), ")")
         analysis_args <- c(analysis_args, paste0("    ", name, " = ", val))
         
      } else {
         # Single Variable
         col_name <- name
         r_type <- get_r_type(opt)
         if (r_type == "factor") {
            df_cols <- c(df_cols, paste0("    ", col_name, " = sample(c('A', 'B'), n, replace = TRUE)"))
         } else {
            df_cols <- c(df_cols, paste0("    ", col_name, " = runif(n, 1, 100)"))
         }
         analysis_args <- c(analysis_args, paste0("    ", name, " = '", col_name, "'"))
      }
      
    } else if (type == "Bool") {
      # Use default value
      val <- ifelse(is.null(opt$default), "FALSE", as.character(opt$default))
      val <- toupper(val)
      analysis_args <- c(analysis_args, paste0("    ", name, " = ", val))
      
    } else if (type == "List") {
      # Use default or first option
      val <- opt$default
      if (is.null(val)) {
        if (!is.null(opt$options) && length(opt$options) > 0) {
           first_opt <- opt$options[[1]]
           if (is.list(first_opt)) val <- first_opt$name else val <- first_opt
        } else {
           val <- "NULL"
        }
      }
      if (!is.null(val) && val != "NULL") val <- paste0("'", val, "'")
      analysis_args <- c(analysis_args, paste0("    ", name, " = ", val))
      
    } else if (type == "Integer" || type == "Number") {
       val <- opt$default
       if (is.null(val)) val <- 1
       
       # Cap heavy computations for testing, but respect minimums
       capped_val <- val
       if (name %in% c("n_lambda", "bootstrap_samples", "max_iterations", "n_simulations", "n_cores")) {
          if (capped_val > 10) capped_val <- 10 # Cap at 10 for speed
          if (name == "n_cores") capped_val <- 1
       }
       
       # Ensure we respect the minimum valid value
       if (!is.null(opt$min)) {
          if (capped_val < opt$min) capped_val <- opt$min
       }
       
       analysis_args <- c(analysis_args, paste0("    ", name, " = ", capped_val))
    }
  }
  
  # Finalize data frame
  data_gen_code <- c(data_gen_code, paste(df_cols, collapse = ",\n"))
  data_gen_code <- c(data_gen_code, "  )")
  
  # 3. Construct the Test File Content
  test_file_path <- file.path(output_dir, paste0("test-", module_name, ".R"))
  
  content <- c(
    "",
    paste0("test_that('", module_name, " analysis works', {"),
    "  skip_if_not_installed('jmvReadWrite')",
    "  devtools::load_all()",
    "",
    paste(data_gen_code, collapse = "\n"),
    "",
    "  # Run analysis",
    "  expect_no_error({",
    paste0("    model <- ", module_name, "("),
    "      data = data,",
    paste(analysis_args, collapse = ",\n"),
    "    )",
    "  })",
    "",
    "  # Verify and Export OMV",
    "  expect_true(is.list(model))",
    "  expect_true(inherits(model, 'jmvcoreClass'))",
    "",
    "  # Define output path",
    paste0("  omv_path <- file.path('omv_output', '", module_name, ".omv')"),
    "  if (!dir.exists('omv_output')) dir.create('omv_output')",
    "",
    "  # Attempt to write OMV",
    "  expect_no_error({",
    "    jmvReadWrite::write_omv(model, omv_path)",
    "  })",
    "",
    "  expect_true(file.exists(omv_path))",
    "})",
    ""
  )
  
  writeLines(content, test_file_path)
  print(paste("Created:", test_file_path))
}

# Example wrapper to run for specific list
# lapply(list_of_yamls, generate_test_for_module)
