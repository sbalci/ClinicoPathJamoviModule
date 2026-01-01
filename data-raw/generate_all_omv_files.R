# Generate .omv files for all existing .rda and .csv files in the data directory
# This script uses the jmvReadWrite package to create jamovi data files

library(jmvReadWrite)

# Function to safely create .omv file from .rda file
create_omv_from_rda <- function(rda_file) {
  # Get base name without extension
  base_name <- tools::file_path_sans_ext(basename(rda_file))
  omv_file <- file.path("data", paste0(base_name, ".omv"))

  # Skip if .omv already exists
  if (file.exists(omv_file)) {
    message(sprintf("SKIP: %s.omv already exists", base_name))
    return(invisible(NULL))
  }

  # Load the .rda file
  env <- new.env()
  tryCatch({
    load(rda_file, envir = env)

    # Get the object name (should match the base name)
    obj_names <- ls(envir = env)

    if (length(obj_names) == 0) {
      warning(sprintf("WARN: No objects found in %s", rda_file))
      return(invisible(NULL))
    }

    # Get the data object
    data_obj <- get(obj_names[1], envir = env)

    # Check if it's a data frame or can be coerced to one
    if (!is.data.frame(data_obj)) {
      if (is.list(data_obj)) {
        warning(sprintf("WARN: %s is a list, not a data frame - skipping", base_name))
        return(invisible(NULL))
      } else if (is.matrix(data_obj) || is.array(data_obj)) {
        warning(sprintf("WARN: %s is %s, not a data frame - attempting conversion", base_name, class(data_obj)[1]))
        data_obj <- as.data.frame(data_obj)
      } else {
        warning(sprintf("WARN: %s is %s, cannot convert to data frame - skipping", base_name, class(data_obj)[1]))
        return(invisible(NULL))
      }
    }

    # Write to .omv file
    jmvReadWrite::write_omv(data_obj, omv_file)
    message(sprintf("SUCCESS: Created %s.omv (%d rows, %d cols)", base_name, nrow(data_obj), ncol(data_obj)))

  }, error = function(e) {
    warning(sprintf("ERROR: Failed to create %s.omv: %s", base_name, e$message))
  })

  invisible(NULL)
}

# Function to safely create .omv file from .csv file
create_omv_from_csv <- function(csv_file) {
  # Get base name without extension
  base_name <- tools::file_path_sans_ext(basename(csv_file))
  omv_file <- file.path("data", paste0(base_name, ".omv"))

  # Skip if .omv already exists
  if (file.exists(omv_file)) {
    message(sprintf("SKIP: %s.omv already exists", base_name))
    return(invisible(NULL))
  }

  # Read the .csv file
  tryCatch({
    data_obj <- read.csv(csv_file, stringsAsFactors = FALSE)

    # Write to .omv file
    jmvReadWrite::write_omv(data_obj, omv_file)
    message(sprintf("SUCCESS: Created %s.omv from CSV (%d rows, %d cols)", base_name, nrow(data_obj), ncol(data_obj)))

  }, error = function(e) {
    warning(sprintf("ERROR: Failed to create %s.omv from CSV: %s", base_name, e$message))
  })

  invisible(NULL)
}

# Main execution
cat("==========================================\n")
cat("Generating .omv files for all data files\n")
cat("==========================================\n\n")

# Find all .rda files
rda_files <- list.files("data", pattern = "\\.rda$", full.names = TRUE)
cat(sprintf("Found %d .rda files\n", length(rda_files)))

# Find all .csv files
csv_files <- list.files("data", pattern = "\\.csv$", full.names = TRUE)
cat(sprintf("Found %d .csv files\n\n", length(csv_files)))

# Process .rda files
cat("Processing .rda files...\n")
cat("------------------------\n")
invisible(lapply(rda_files, create_omv_from_rda))

cat("\n")

# Process .csv files
cat("Processing .csv files...\n")
cat("------------------------\n")
invisible(lapply(csv_files, create_omv_from_csv))

# Summary
cat("\n")
cat("==========================================\n")
cat("Summary\n")
cat("==========================================\n")

# Count .omv files after generation
omv_files_after <- list.files("data", pattern = "\\.omv$")
cat(sprintf("Total .omv files now: %d\n", length(omv_files_after)))
cat(sprintf("Total .rda files: %d\n", length(rda_files)))
cat(sprintf("Total .csv files: %d\n", length(csv_files)))
cat(sprintf("Total data files: %d\n", length(rda_files) + length(csv_files)))

cat("\nDone!\n")