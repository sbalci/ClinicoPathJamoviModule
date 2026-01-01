# Helper functions for saving data in multiple formats
# Source this file in data generation scripts to ensure consistent data saving

#' Save dataset in multiple formats (.rda, .omv, and optionally .csv)
#'
#' This function saves a dataset in the jamovi-compatible formats:
#' - .rda format for R packages
#' - .omv format for jamovi
#' - .csv format (optional) for portability
#'
#' @param data A data frame to save
#' @param name Character string. The name of the dataset (without file extension).
#'   This will be used for the filename and as the object name in the .rda file.
#' @param save_csv Logical. Should a CSV file also be created? Default is TRUE.
#' @param overwrite Logical. Should existing files be overwritten? Default is TRUE.
#' @param data_dir Character string. Directory where data files are saved. Default is "data".
#' @param verbose Logical. Print progress messages? Default is TRUE.
#'
#' @return Invisible NULL. Files are created as side effects.
#'
#' @examples
#' # Create example data
#' my_data <- data.frame(x = 1:10, y = letters[1:10])
#'
#' # Save in all formats
#' save_data_multi_format(my_data, "my_data")
#'
#' # Save without CSV
#' save_data_multi_format(my_data, "my_data", save_csv = FALSE)
save_data_multi_format <- function(data,
                                   name,
                                   save_csv = TRUE,
                                   overwrite = TRUE,
                                   data_dir = "data",
                                   verbose = TRUE) {

  # Check if jmvReadWrite is available
  if (!requireNamespace("jmvReadWrite", quietly = TRUE)) {
    stop("Package 'jmvReadWrite' is required. Please install it with: install.packages('jmvReadWrite')")
  }

  # Validate inputs
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (!is.character(name) || length(name) != 1) {
    stop("'name' must be a single character string")
  }

  # Create data directory if it doesn't exist
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }

  # Define file paths
  rda_file <- file.path(data_dir, paste0(name, ".rda"))
  omv_file <- file.path(data_dir, paste0(name, ".omv"))
  csv_file <- file.path(data_dir, paste0(name, ".csv"))

  # Save .rda file
  tryCatch({
    # Create object with the specified name
    assign(name, data)
    # Save using the name
    save(list = name, file = rda_file, envir = environment())

    if (verbose) {
      message(sprintf("✓ Saved %s.rda (%d rows, %d cols)", name, nrow(data), ncol(data)))
    }
  }, error = function(e) {
    warning(sprintf("Failed to save %s.rda: %s", name, e$message))
  })

  # Save .omv file
  tryCatch({
    jmvReadWrite::write_omv(data, omv_file)

    if (verbose) {
      message(sprintf("✓ Saved %s.omv (%d rows, %d cols)", name, nrow(data), ncol(data)))
    }
  }, error = function(e) {
    warning(sprintf("Failed to save %s.omv: %s", name, e$message))
  })

  # Save .csv file if requested
  if (save_csv) {
    tryCatch({
      write.csv(data, csv_file, row.names = FALSE)

      if (verbose) {
        message(sprintf("✓ Saved %s.csv (%d rows, %d cols)", name, nrow(data), ncol(data)))
      }
    }, error = function(e) {
      warning(sprintf("Failed to save %s.csv: %s", name, e$message))
    })
  }

  invisible(NULL)
}


#' Save multiple datasets at once
#'
#' Convenience function to save multiple datasets in one call
#'
#' @param ... Named datasets. Names will be used as filenames.
#' @param save_csv Logical. Should CSV files also be created? Default is TRUE.
#' @param overwrite Logical. Should existing files be overwritten? Default is TRUE.
#' @param data_dir Character string. Directory where data files are saved. Default is "data".
#' @param verbose Logical. Print progress messages? Default is TRUE.
#'
#' @return Invisible NULL. Files are created as side effects.
#'
#' @examples
#' data1 <- data.frame(x = 1:10, y = letters[1:10])
#' data2 <- data.frame(a = 1:5, b = LETTERS[1:5])
#'
#' save_multiple_datasets(
#'   my_data1 = data1,
#'   my_data2 = data2
#' )
save_multiple_datasets <- function(...,
                                   save_csv = TRUE,
                                   overwrite = TRUE,
                                   data_dir = "data",
                                   verbose = TRUE) {

  datasets <- list(...)
  dataset_names <- names(datasets)

  if (is.null(dataset_names) || any(dataset_names == "")) {
    stop("All datasets must be named")
  }

  if (verbose) {
    message(sprintf("\nSaving %d datasets...", length(datasets)))
    message(paste(rep("=", 50), collapse = ""))
  }

  for (i in seq_along(datasets)) {
    if (verbose) {
      message(sprintf("\nDataset %d of %d: %s", i, length(datasets), dataset_names[i]))
    }

    save_data_multi_format(
      data = datasets[[i]],
      name = dataset_names[i],
      save_csv = save_csv,
      overwrite = overwrite,
      data_dir = data_dir,
      verbose = verbose
    )
  }

  if (verbose) {
    message(sprintf("\n%s", paste(rep("=", 50), collapse = "")))
    message(sprintf("✓ Successfully saved all %d datasets", length(datasets)))
  }

  invisible(NULL)
}


#' Replace usethis::use_data with multi-format saving
#'
#' This is a drop-in replacement for usethis::use_data that also creates .omv files
#'
#' @param ... Datasets to save (unquoted names)
#' @param overwrite Logical. Overwrite existing files? Default is TRUE.
#' @param save_csv Logical. Also save as CSV? Default is TRUE.
#' @param verbose Logical. Print progress messages? Default is TRUE.
#'
#' @return Invisible NULL
#'
#' @examples
#' my_data <- data.frame(x = 1:10, y = letters[1:10])
#' use_data_multi_format(my_data)
use_data_multi_format <- function(...,
                                  overwrite = TRUE,
                                  save_csv = TRUE,
                                  verbose = TRUE) {

  # Get the names of the objects passed
  obj_names <- as.character(substitute(list(...)))[-1]

  # Get the objects themselves
  objs <- list(...)

  if (length(obj_names) != length(objs)) {
    stop("Could not determine names for all objects")
  }

  # Save each object
  for (i in seq_along(objs)) {
    save_data_multi_format(
      data = objs[[i]],
      name = obj_names[i],
      save_csv = save_csv,
      overwrite = overwrite,
      data_dir = "data",
      verbose = verbose
    )
  }

  invisible(NULL)
}


# Print usage instructions when sourced
if (interactive()) {
  message("\n", paste(rep("=", 70), collapse = ""))
  message("Data Saving Helper Functions Loaded")
  message(paste(rep("=", 70), collapse = ""))
  message("\nAvailable functions:")
  message("  - save_data_multi_format(data, name)     : Save single dataset in .rda, .omv, .csv")
  message("  - save_multiple_datasets(name1=data1, ...): Save multiple datasets at once")
  message("  - use_data_multi_format(data)            : Drop-in replacement for usethis::use_data")
  message("\nExamples:")
  message('  save_data_multi_format(my_data, "my_data")')
  message('  save_multiple_datasets(data1 = my_data1, data2 = my_data2)')
  message('  use_data_multi_format(my_data)  # Instead of usethis::use_data(my_data)')
  message(paste(rep("=", 70), collapse = ""), "\n")
}
