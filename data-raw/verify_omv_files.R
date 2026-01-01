# Verify that all .rda and .csv files have corresponding .omv files
# This script checks data integrity and reports any missing .omv files

cat("==========================================\n")
cat("Verifying .omv File Coverage\n")
cat("==========================================\n\n")

# Get all data files
rda_files <- list.files("data", pattern = "\\.rda$", full.names = FALSE)
csv_files <- list.files("data", pattern = "\\.csv$", full.names = FALSE)
omv_files <- list.files("data", pattern = "\\.omv$", full.names = FALSE)

# Remove file extensions to get base names
rda_bases <- tools::file_path_sans_ext(rda_files)
csv_bases <- tools::file_path_sans_ext(csv_files)
omv_bases <- tools::file_path_sans_ext(omv_files)

# Get unique base names from .rda and .csv files
all_data_bases <- unique(c(rda_bases, csv_bases))

# Check for missing .omv files
missing_omv <- setdiff(all_data_bases, omv_bases)

# Report statistics
cat("File Statistics:\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
cat(sprintf("  .rda files: %d\n", length(rda_files)))
cat(sprintf("  .csv files: %d\n", length(csv_files)))
cat(sprintf("  .omv files: %d\n", length(omv_files)))
cat(sprintf("  Unique datasets: %d\n", length(all_data_bases)))
cat("\n")

# Report coverage
coverage_pct <- (length(omv_bases) / length(all_data_bases)) * 100
cat(sprintf("Coverage: %.1f%% (%d of %d datasets have .omv files)\n\n",
           coverage_pct, length(omv_bases), length(all_data_bases)))

# Report missing files
if (length(missing_omv) > 0) {
  cat("WARNING: Missing .omv files for the following datasets:\n")
  cat(paste(rep("-", 70), collapse = ""), "\n")

  for (base in sort(missing_omv)) {
    # Check which formats exist
    has_rda <- base %in% rda_bases
    has_csv <- base %in% csv_bases

    formats <- character()
    if (has_rda) formats <- c(formats, ".rda")
    if (has_csv) formats <- c(formats, ".csv")

    cat(sprintf("  ✗ %s (exists as: %s)\n", base, paste(formats, collapse = ", ")))
  }

  cat("\n")
  cat("To generate missing .omv files, run:\n")
  cat("  Rscript data-raw/generate_all_omv_files.R\n\n")
} else {
  cat("✓ All datasets have corresponding .omv files!\n\n")
}

# Report datasets with all three formats
complete_datasets <- intersect(intersect(rda_bases, csv_bases), omv_bases)
cat(sprintf("Complete datasets (have .rda, .csv, AND .omv): %d\n", length(complete_datasets)))

# Report datasets with only some formats
partial_datasets <- setdiff(all_data_bases, complete_datasets)
if (length(partial_datasets) > 0) {
  cat(sprintf("Partial datasets (missing at least one format): %d\n\n", length(partial_datasets)))

  if (length(partial_datasets) <= 20) {
    cat("Datasets with missing formats:\n")
    cat(paste(rep("-", 70), collapse = ""), "\n")

    for (base in sort(partial_datasets)) {
      has_rda <- base %in% rda_bases
      has_csv <- base %in% csv_bases
      has_omv <- base %in% omv_bases

      status <- sprintf("  %s: .rda[%s] .csv[%s] .omv[%s]",
                       base,
                       ifelse(has_rda, "✓", "✗"),
                       ifelse(has_csv, "✓", "✗"),
                       ifelse(has_omv, "✓", "✗"))
      cat(status, "\n")
    }
    cat("\n")
  }
}

# Summary
cat("==========================================\n")
cat("Summary\n")
cat("==========================================\n")

if (coverage_pct == 100) {
  cat("✓ PASS: All datasets have .omv files\n")
  cat("\nThe package is ready for jamovi distribution!\n")
} else {
  cat(sprintf("⚠ INCOMPLETE: %.1f%% coverage\n", coverage_pct))
  cat(sprintf("   Missing .omv files for %d dataset(s)\n", length(missing_omv)))
  cat("\nRun generate_all_omv_files.R to create missing files.\n")
}

cat("\nDone!\n")

# Return invisible status
invisible(list(
  total_datasets = length(all_data_bases),
  omv_count = length(omv_bases),
  missing_count = length(missing_omv),
  coverage_pct = coverage_pct,
  complete = coverage_pct == 100
))
