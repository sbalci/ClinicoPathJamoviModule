# Update all data generation scripts to use multi-format saving
# This script automatically updates existing data generation scripts to:
# 1. Source the data_save_helpers.R file
# 2. Replace usethis::use_data() with use_data_multi_format()
# 3. Add .omv file generation for save() calls

library(stringr)

# Function to check if a file already sources data_save_helpers.R
has_helper_source <- function(file_content) {
  any(grepl("source\\(.*data_save_helpers\\.R", file_content))
}

# Function to add helper source to file
add_helper_source <- function(file_content) {
  # Find the first non-comment, non-empty line
  first_code_line <- which(!grepl("^\\s*#|^\\s*$", file_content))[1]

  if (is.na(first_code_line)) {
    # File has only comments/empty lines
    return(c(file_content, "", "# Load helper functions for multi-format data saving", "source(\"data-raw/data_save_helpers.R\")", ""))
  }

  # Insert before the first code line
  helper_lines <- c(
    "# Load helper functions for multi-format data saving",
    "source(\"data-raw/data_save_helpers.R\")",
    ""
  )

  c(
    file_content[1:(first_code_line - 1)],
    helper_lines,
    file_content[first_code_line:length(file_content)]
  )
}

# Function to replace usethis::use_data() calls
replace_use_data <- function(file_content) {
  modified <- FALSE

  for (i in seq_along(file_content)) {
    line <- file_content[i]

    # Check for usethis::use_data() pattern
    if (grepl("usethis::use_data\\s*\\(", line)) {
      # Extract the dataset name(s)
      # Pattern: usethis::use_data(dataset_name, overwrite = TRUE)

      # Replace usethis::use_data with use_data_multi_format
      new_line <- gsub(
        "usethis::use_data\\s*\\(",
        "use_data_multi_format(",
        line
      )

      # Ensure overwrite and save_csv parameters are present
      if (!grepl("save_csv\\s*=", new_line)) {
        # Add save_csv parameter before closing parenthesis
        new_line <- gsub(
          "\\s*\\)\\s*$",
          ", save_csv = TRUE)",
          new_line
        )
      }

      file_content[i] <- new_line
      modified <- TRUE
    }
  }

  list(content = file_content, modified = modified)
}

# Function to add .omv saving for save() calls
add_omv_for_save <- function(file_content) {
  modified <- FALSE
  new_content <- character()
  i <- 1

  while (i <= length(file_content)) {
    line <- file_content[i]
    new_content <- c(new_content, line)

    # Check for save() pattern that's not inside use_data_multi_format
    if (grepl("^\\s*save\\s*\\(", line) && !grepl("use_data_multi_format", line)) {
      # Extract the dataset name and file path
      # Pattern: save(dataset_name, file = "data/dataset_name.rda")

      dataset_match <- str_match(line, "save\\s*\\(\\s*([^,]+)\\s*,")[1, 2]
      file_match <- str_match(line, "file\\s*=\\s*\"([^\"]+)\"")[1, 2]

      if (!is.na(dataset_match) && !is.na(file_match)) {
        dataset_name <- trimws(dataset_match)
        rda_file <- trimws(file_match)

        # Create .omv filename
        omv_file <- gsub("\\.rda$", ".omv", rda_file)

        # Add .omv saving code after the save() line
        omv_code <- c(
          "",
          "# Also save as .omv for jamovi",
          "if (requireNamespace(\"jmvReadWrite\", quietly = TRUE)) {",
          sprintf("  jmvReadWrite::write_omv(%s, \"%s\")", dataset_name, omv_file),
          sprintf("  message(\"✓ Created %s\")", basename(omv_file)),
          "}"
        )

        new_content <- c(new_content, omv_code)
        modified <- TRUE
      }
    }

    i <- i + 1
  }

  list(content = new_content, modified = modified)
}

# Main function to update a single file
update_data_script <- function(filepath) {
  cat(sprintf("\nProcessing: %s\n", basename(filepath)))
  cat(paste(rep("-", 70), collapse = ""), "\n")

  # Read file
  tryCatch({
    file_content <- readLines(filepath, warn = FALSE)
    original_content <- file_content
    changes_made <- character()

    # 1. Add helper source if not present
    if (!has_helper_source(file_content)) {
      file_content <- add_helper_source(file_content)
      changes_made <- c(changes_made, "Added data_save_helpers.R source")
      cat("  ✓ Added helper source\n")
    } else {
      cat("  ⊙ Helper source already present\n")
    }

    # 2. Replace usethis::use_data() calls
    result <- replace_use_data(file_content)
    file_content <- result$content
    if (result$modified) {
      changes_made <- c(changes_made, "Replaced usethis::use_data() calls")
      cat("  ✓ Updated usethis::use_data() calls\n")
    } else {
      cat("  ⊙ No usethis::use_data() calls found\n")
    }

    # 3. Add .omv saving for save() calls
    result <- add_omv_for_save(file_content)
    file_content <- result$content
    if (result$modified) {
      changes_made <- c(changes_made, "Added .omv generation for save() calls")
      cat("  ✓ Added .omv generation for save() calls\n")
    } else {
      cat("  ⊙ No direct save() calls found\n")
    }

    # Write back if changes were made
    if (length(changes_made) > 0) {
      writeLines(file_content, filepath)
      cat("  ✓ File updated successfully\n")
      return(list(updated = TRUE, changes = changes_made))
    } else {
      cat("  ⊙ No changes needed\n")
      return(list(updated = FALSE, changes = NULL))
    }

  }, error = function(e) {
    cat(sprintf("  ✗ ERROR: %s\n", e$message))
    return(list(updated = FALSE, error = e$message))
  })
}

# Main execution
cat("==========================================\n")
cat("Updating Data Generation Scripts\n")
cat("==========================================\n\n")

# Find all R scripts in data-raw
r_scripts <- list.files(
  "data-raw",
  pattern = "\\.R$",
  full.names = TRUE,
  ignore.case = TRUE
)

# Exclude the helper scripts and this script
exclude_patterns <- c(
  "data_save_helpers.R",
  "update_all_data_scripts.R",
  "generate_all_omv_files.R",
  "TEMPLATE_"
)

r_scripts <- r_scripts[!basename(r_scripts) %in% exclude_patterns]
r_scripts <- r_scripts[!grepl("^TEMPLATE_", basename(r_scripts))]

cat(sprintf("Found %d data generation scripts to check\n", length(r_scripts)))
cat(paste(rep("=", 70), collapse = ""), "\n")

# Track results
results <- list()
updated_count <- 0
error_count <- 0

# Update each script
for (script in r_scripts) {
  result <- update_data_script(script)
  results[[basename(script)]] <- result

  if (!is.null(result$updated) && result$updated) {
    updated_count <- updated_count + 1
  }

  if (!is.null(result$error)) {
    error_count <- error_count + 1
  }
}

# Summary
cat("\n")
cat("==========================================\n")
cat("Summary\n")
cat("==========================================\n")
cat(sprintf("Total scripts checked: %d\n", length(r_scripts)))
cat(sprintf("Scripts updated: %d\n", updated_count))
cat(sprintf("Scripts with errors: %d\n", error_count))
cat(sprintf("Scripts unchanged: %d\n", length(r_scripts) - updated_count - error_count))

if (updated_count > 0) {
  cat("\n✓ All scripts have been updated to use multi-format data saving!\n")
  cat("\nNext steps:\n")
  cat("  1. Review the updated scripts to ensure correctness\n")
  cat("  2. Run individual scripts to test they work correctly\n")
  cat("  3. Verify .omv files are created in the data/ directory\n")
} else {
  cat("\n⊙ No scripts needed updating - all already use multi-format saving!\n")
}

cat("\nDone!\n")
