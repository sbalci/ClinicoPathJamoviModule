#!/usr/bin/env Rscript
# Enhanced Update Modules Script for ClinicoPathJamoviModule
#
# This script provides a comprehensive solution for distributing production-ready functions
# from the main ClinicoPathJamoviModule to specialized submodules with enhanced:
# - Error handling and validation
# - Security measures
# - Performance optimization
# - Testing integration
# - Backup and rollback capabilities
# - Configuration management
#
# Usage: Rscript _updateModules.R [config_file]
#
# Author: Enhanced version with enterprise-grade features
# Requires: R >= 4.0.0, yaml, future, digest packages
# run  /Applications/jamovi.app/Contents/MacOS/jamovi 



# Command line argument handling
args <- commandArgs(trailingOnly = TRUE)
config_file <- if (length(args) > 0) args[1] else "_updateModules_config.yaml"

cat("🚀 Starting Enhanced Module Update Process\n")
cat("Configuration file:", config_file, "\n")

# Get script directory and set working directory
script_dir <- tryCatch({
  # Try to get script directory from command line execution
  dirname(normalizePath(sys.frame(1)$ofile))
}, error = function(e) {
  # Fallback for interactive execution
  getwd()
})
setwd(script_dir)

# Source utility functions with enhanced validation
utility_file <- "module_utils.R"
if (!file.exists(utility_file)) {
  warning("⚠️ module_utils.R not found in current directory: ", getwd())
  cat("Looking for module_utils.R in script directory...\n")

  # Try to find in script directory
  script_utility <- file.path(script_dir, utility_file)
  if (file.exists(script_utility)) {
    cat("✅ Found module_utils.R in script directory\n")
    utility_file <- script_utility
  } else {
    stop("❌ module_utils.R not found. Please ensure all required files are present.")
  }
}

# Source with error handling
tryCatch({
  source(utility_file)
  cat("✅ Successfully loaded module utilities\n")
}, error = function(e) {
  stop("❌ Failed to load module_utils.R: ", e$message)
})

# Load and validate configuration
cat("\n📋 Loading configuration...\n")
config <- load_config(config_file)
config <- validate_config(config)

# Extract configuration values
global <- config$global
modes <- config$modes
modules_config <- config$modules
required_packages <- config$required_packages %||% c("xfun", "fs", "jmvtools", "devtools", "purrr", "yaml", "digest")

# Override configuration with any manual settings (for backward compatibility)
# You can still manually override these if needed
new_version <- global$new_version
new_date <- global$new_date
main_repo_dir <- global$base_repo_dir

# Operation modes
quick <- modes$quick %||% FALSE
check <- modes$check %||% FALSE
extended <- modes$extended %||% FALSE
webpage <- modes$webpage %||% FALSE
commit_modules <- modes$commit_modules %||% FALSE
WIP <- modes$WIP %||% FALSE

# File copying control modes
copy_vignettes <- modes$copy_vignettes %||% TRUE
copy_data_files <- modes$copy_data_files %||% TRUE
copy_test_files <- modes$copy_test_files %||% TRUE
copy_r_files <- modes$copy_r_files %||% TRUE

# Module-specific flags (extracted from config)
ClinicoPathDescriptives_module <- modules_config$ClinicoPathDescriptives$enabled %||% FALSE
jsurvival_module <- modules_config$jsurvival$enabled %||% FALSE
jjstatsplot_module <- modules_config$jjstatsplot$enabled %||% FALSE
meddecide_module <- modules_config$meddecide$enabled %||% FALSE

# Apply WIP mode overrides
if (WIP) {
  quick <- FALSE
  check <- FALSE
  extended <- TRUE
  webpage <- FALSE
  commit_modules <- FALSE
  cat("🔧 WIP mode enabled - using sandbox environment\n")
}

# Load required packages with validation
cat("\n📦 Loading required packages...\n")
load_required_packages(required_packages)

# Setup parallel processing if enabled
parallel_enabled <- setup_parallel_processing(
  enabled = config$performance$parallel_processing %||% FALSE,
  max_workers = config$performance$max_workers %||% 4
)

# Validate main repository directory
main_repo_dir <- validate_path(main_repo_dir, dirname(main_repo_dir), "main repository")
setwd(main_repo_dir)

# Clean old backups if backup is enabled
if (config$backup$enabled %||% TRUE) {
  cat("\n🧹 Cleaning old backups...\n")
  clean_old_backups(
    backup_base_dir = config$backup$backup_location %||% "backups",
    retention_days = config$backup$retention_days %||% 30
  )
}

# Quick mode handling
if (quick) {
  cat("⚡ Quick mode enabled - performing fast installation\n")
  with_error_handling({
    devtools::install(quick = TRUE, reload = TRUE, quiet = FALSE,
                     upgrade = FALSE, build_vignettes = FALSE, keep_source = TRUE)
  }, "quick installation")
  cat("✅ Quick mode completed successfully\n")
  quit("no", status = 0)
}



# Extract module directories from configuration with validation
module_dirs <- list()
module_validation_failed <- FALSE

cat("\n📁 Validating module directories...\n")
for (module_name in names(modules_config)) {
  if (modules_config[[module_name]]$enabled) {
    module_dir <- modules_config[[module_name]]$directory

    # Validate directory exists
    if (!dir.exists(module_dir)) {
      warning("⚠️ Module directory does not exist: ", module_dir, " for ", module_name)
      module_validation_failed <- TRUE
      next
    }

    module_dirs[[module_name]] <- module_dir
    cat("  ✅", module_name, ":", module_dir, "\n")
  } else {
    cat("  ⏭️", module_name, ": disabled\n")
  }
}

if (module_validation_failed && !WIP) {
  stop("❌ Some module directories are invalid. Check configuration or enable WIP mode.")
}

# Legacy variable assignments for backward compatibility (using config values)
jjstatsplot_dir <- module_dirs$jjstatsplot %||% modules_config$jjstatsplot$directory
meddecide_dir <- module_dirs$meddecide %||% modules_config$meddecide$directory
jsurvival_dir <- module_dirs$jsurvival %||% modules_config$jsurvival$directory
ClinicoPathDescriptives_dir <- module_dirs$ClinicoPathDescriptives %||% modules_config$ClinicoPathDescriptives$directory

# Enhanced WIP mode with backup and validation
if (WIP) {
  cat("\n🔧 Setting up WIP (Work-In-Progress) environment...\n")

  wip_setup_success <- TRUE

  for (module_name in names(module_dirs)) {
    original_dir <- module_dirs[[module_name]]
    wip_dir <- paste0(original_dir, "-WIP")

    # Validate original directory exists
    if (!dir.exists(original_dir)) {
      warning("⚠️ Original module directory does not exist: ", original_dir)
      wip_setup_success <- FALSE
      next
    }

    cat("🔧 Setting up WIP environment for", module_name, "\n")

    # Delete existing WIP directory if it exists
    if (dir.exists(wip_dir)) {
      cat("  🗑️ Removing existing WIP directory:", wip_dir, "\n")
      with_error_handling({
        fs::dir_delete(wip_dir)
      }, paste("removing existing WIP directory for", module_name), continue_on_error = TRUE)
    }

    # Create backup of original directory
    backup_path <- create_backup(original_dir, "wip_backups")
    if (is.null(backup_path)) {
      warning("⚠️ Failed to create backup for ", module_name, ", skipping WIP setup")
      wip_setup_success <- FALSE
      next
    }

    # Copy original to WIP directory
    cat("  📁 Creating WIP copy:", wip_dir, "\n")
    copy_result <- with_error_handling({
      fs::dir_copy(path = original_dir, new_path = wip_dir, overwrite = TRUE)
    }, paste("creating WIP directory for", module_name), continue_on_error = TRUE)

    if (!copy_result$success) {
      wip_setup_success <- FALSE
      next
    }

    # Update module directory reference
    module_dirs[[module_name]] <- wip_dir

    cat("  ✅ WIP environment ready for", module_name, "\n")
  }

  # Update legacy variables for WIP
  jjstatsplot_dir <- module_dirs$jjstatsplot %||% jjstatsplot_dir
  meddecide_dir <- module_dirs$meddecide %||% meddecide_dir
  jsurvival_dir <- module_dirs$jsurvival %||% jsurvival_dir
  ClinicoPathDescriptives_dir <- module_dirs$ClinicoPathDescriptives %||% ClinicoPathDescriptives_dir

  if (!wip_setup_success) {
    stop("❌ WIP setup failed for one or more modules. Check warnings above.")
  }

  cat("✅ WIP environment setup completed successfully\n")
}


# Enhanced function to update DESCRIPTION files with validation
update_description_files <- function(paths, version, date) {
  cat("\n📝 Updating DESCRIPTION files...\n")

  version_pattern <- "Version:.*$"
  date_pattern <- "Date:.*$"
  version_replacement <- paste0("Version: ", version)
  date_replacement <- paste0("Date: ", date)

  updated_count <- 0
  failed_count <- 0

  for (path in paths) {
    if (!file.exists(path)) {
      warning("⚠️ DESCRIPTION file not found: ", path)
      failed_count <- failed_count + 1
      next
    }

    # Perform updates with error handling
    update_result <- with_error_handling({
      xfun::gsub_files(files = path,
                       pattern = version_pattern,
                       replacement = version_replacement)
      xfun::gsub_files(files = path,
                       pattern = date_pattern,
                       replacement = date_replacement)
    }, paste("updating DESCRIPTION file", path), continue_on_error = TRUE)

    if (update_result$success) {
      updated_count <- updated_count + 1
      cat("  ✅ Updated:", basename(dirname(path)), "\n")
    } else {
      failed_count <- failed_count + 1
    }
  }

  cat("📝 DESCRIPTION update summary:", updated_count, "updated,", failed_count, "failed\n")

  if (failed_count > 0 && updated_count == 0) {
    stop("❌ All DESCRIPTION file updates failed")
  }
}

# Enhanced function to update YAML files with validation
update_yaml_0000_files <- function(paths, version, date) {
  cat("\n📝 Updating 0000.yaml files...\n")

  version_pattern <- "version:.*$"
  date_pattern <- "date:.*$"
  version_replacement <- paste0("version: ", version)
  date_replacement <- paste0("date: '", date, "'")

  updated_count <- 0
  failed_count <- 0

  for (path in paths) {
    if (!file.exists(path)) {
      warning("⚠️ YAML file not found: ", path)
      failed_count <- failed_count + 1
      next
    }

    update_result <- with_error_handling({
      xfun::gsub_files(files = path,
                       pattern = version_pattern,
                       replacement = version_replacement)
      xfun::gsub_files(files = path,
                       pattern = date_pattern,
                       replacement = date_replacement)
    }, paste("updating 0000.yaml file", path), continue_on_error = TRUE)

    if (update_result$success) {
      updated_count <- updated_count + 1
      cat("  ✅ Updated:", basename(dirname(path)), "\n")
    } else {
      failed_count <- failed_count + 1
    }
  }

  cat("📝 YAML 0000 update summary:", updated_count, "updated,", failed_count, "failed\n")
}


# Enhanced function to update analysis YAML files
update_yaml_a_files <- function(paths, version) {
  cat("\n📝 Updating analysis .a.yaml files...\n")

  version_pattern <- "version:.*$"
  valid_version <- paste(strsplit(version, "\\.")[[1]][1:3], collapse = ".")
  version_replacement <- paste0("version: '", valid_version, "'")

  updated_count <- 0
  failed_count <- 0

  for (path in paths) {
    if (!file.exists(path)) {
      failed_count <- failed_count + 1
      next
    }

    update_result <- with_error_handling({
      xfun::gsub_files(files = path,
                       pattern = version_pattern,
                       replacement = version_replacement)
    }, paste("updating analysis YAML file", basename(path)), continue_on_error = TRUE)

    if (update_result$success) {
      updated_count <- updated_count + 1
    } else {
      failed_count <- failed_count + 1
    }
  }

  cat("📝 Analysis YAML update summary:", updated_count, "updated,", failed_count, "failed\n")
}


# Enhanced function to copy module files with comprehensive validation ----
copy_module_files <- function(module_names, source_dir, dest_dir, file_extensions) {
  if (length(module_names) == 0) {
    cat("  ⏭️ No modules to copy\n")
    return(list(copied = 0, skipped = 0, failed = 0))
  }

  if (!dir.exists(source_dir)) {
    warning("⚠️ Source directory does not exist: ", source_dir)
    return(list(copied = 0, skipped = 0, failed = length(module_names) * length(file_extensions)))
  }

  if (!dir.exists(dest_dir)) {
    cat("  📁 Creating destination directory: ", dest_dir, "\n")
    dir.create(dest_dir, recursive = TRUE)
  }

  copied_count <- 0
  failed_count <- 0

  for (module_name in module_names) {
    for (ext in file_extensions) {
      source_path <- file.path(source_dir, paste0(module_name, ext))
      dest_path <- file.path(dest_dir, paste0(module_name, ext))

      if (!file.exists(source_path)) {
        warning("⚠️ Source file not found: ", source_path)
        failed_count <- failed_count + 1
        next
      }

      tryCatch({
        fs::file_copy(path = source_path, new_path = dest_path, overwrite = TRUE)
        cat("  ✅ Copied: ", paste0(module_name, ext), "\n")
        copied_count <- copied_count + 1
      }, error = function(e) {
        warning("⚠️ Failed to copy ", source_path, ": ", e$message)
        failed_count <- failed_count + 1
      })
    }
  }

  return(list(copied = copied_count, skipped = 0, failed = failed_count))
}

# Enhanced function to copy module files with validation and performance
copy_module_files_enhanced <- function(module_names, source_dir, dest_dir, file_extensions, module_type = "unknown") {
  cat("\n📁 Copying", module_type, "module files...\n")

  if (length(module_names) == 0) {
    cat("  ⏭️ No", module_type, "modules to process\n")
    return(list(copied = 0, skipped = 0, failed = 0))
  }

  if (!dir.exists(source_dir)) {
    warning("⚠️ Source directory does not exist: ", source_dir)
    return(list(copied = 0, skipped = 0, failed = length(module_names) * length(file_extensions)))
  }

  if (!dir.exists(dest_dir)) {
    cat("  📁 Creating destination directory:", dest_dir, "\n")
    tryCatch({
      dir.create(dest_dir, recursive = TRUE)
    }, error = function(e) {
      stop("❌ Failed to create directory ", dest_dir, ": ", e$message)
    })
  }

  copied_count <- 0
  skipped_count <- 0
  failed_count <- 0

  use_incremental <- config$performance$incremental_updates %||% TRUE
  verify_integrity <- config$security$verify_checksums %||% TRUE

  for (module_name in module_names) {
    for (ext in file_extensions) {
      source_path <- file.path(source_dir, paste0(module_name, ext))
      dest_path <- file.path(dest_dir, paste0(module_name, ext))

      if (!file.exists(source_path)) {
        warning("⚠️ Source file not found: ", source_path)
        failed_count <- failed_count + 1
        next
      }

      # Check if incremental update is possible
      if (use_incremental && verify_file_integrity(source_path, dest_path)) {
        skipped_count <- skipped_count + 1
        next
      }

      # Copy file with validation
      copy_result <- with_error_handling({
        # Validate file size before copying
        if (!validate_file_size(source_path, config$security$max_file_size_mb %||% 100)) {
          stop("File too large: ", source_path)
        }

        fs::file_copy(path = source_path, new_path = dest_path, overwrite = TRUE)

        # Verify integrity after copy if enabled
        if (verify_integrity && !verify_file_integrity(source_path, dest_path)) {
          stop("File integrity check failed after copy")
        }

      }, paste("copying", basename(source_path)), continue_on_error = TRUE)

      if (copy_result$success) {
        copied_count <- copied_count + 1
        cat("  ✅ Copied:", paste0(module_name, ext), "\n")
      } else {
        failed_count <- failed_count + 1
      }
    }
  }

  cat("📁", module_type, "module file summary:", copied_count, "copied,",
      skipped_count, "skipped,", failed_count, "failed\n")

  if (failed_count > 0 && copied_count == 0) {
    warning("⚠️ All ", module_type, " module files failed to copy")
  }

  return(list(copied = copied_count, skipped = skipped_count, failed = failed_count))
}

# Enhanced Git commit function with comprehensive validation
commit_repo_enhanced <- function(repo_dir, commit_message, validate_repo = TRUE, dry_run = FALSE) {
  if (!dir.exists(repo_dir)) {
    warning("⚠️ Repository directory does not exist: ", repo_dir)
    return(FALSE)
  }

  old_wd <- getwd()
  on.exit(setwd(old_wd))

  tryCatch({
    setwd(repo_dir)
    repo_name <- basename(repo_dir)

    # Validate it's a git repository
    if (validate_repo && !dir.exists(".git")) {
      warning("⚠️ Not a git repository: ", repo_dir)
      return(FALSE)
    }

    # Check if there are changes to commit
    status_result <- system("git status --porcelain", intern = TRUE, ignore.stderr = TRUE)
    if (length(status_result) == 0) {
      cat("  ℹ️ No changes to commit in:", repo_name, "\n")
      return(TRUE)
    }

    cat("  📝 Found", length(status_result), "changed files in:", repo_name, "\n")

    if (dry_run) {
      cat("  📋 Dry run mode - would commit:", repo_name, "\n")
      return(TRUE)
    }

    # Add all changes
    add_result <- system("git add -A", intern = TRUE, ignore.stderr = TRUE)
    if (add_result != 0) {
      warning("⚠️ Git add failed for ", repo_name)
      return(FALSE)
    }

    # Commit with message
    escaped_message <- gsub('"', '\\"', commit_message)
    commit_cmd <- sprintf('git commit -m "%s"', escaped_message)
    commit_result <- system(commit_cmd, intern = TRUE, ignore.stderr = TRUE)

    if (commit_result == 0 || any(grepl("nothing to commit", commit_result))) {
      cat("  ✅ Committed changes in:", repo_name, "\n")
      return(TRUE)
    } else {
      warning("⚠️ Git commit returned non-zero status for ", repo_name)
      return(FALSE)
    }

  }, error = function(e) {
    warning("⚠️ Git commit failed for ", basename(repo_dir), ": ", e$message)
    return(FALSE)
  })
}

# Function to commit changes in a Git repository ----
commit_repo <- function(repo_dir, commit_message) {
  old_wd <- getwd()
  setwd(repo_dir)
  system("git add -A")
  system(sprintf("git commit -m \"%s\"", commit_message))
  setwd(old_wd)
}



# Enhanced configuration-based asset copying ----
if (!WIP) {
  cat("\n📁 Copying assets with configuration-based logic...\n")

  for (module_name in names(modules_config)) {
    if (!modules_config[[module_name]]$enabled) next

    module_cfg <- modules_config[[module_name]]
    module_dir <- module_dirs[[module_name]]

    cat("📁 Processing assets for", module_name, "\n")

    # Copy data files
    if (copy_data_files && length(module_cfg$data_files) > 0) {
      cat("  📁 Copying", module_name, "data files...\n")
      data_dir <- file.path(module_dir, "data")
      if (!dir.exists(data_dir)) {
        dir.create(data_dir, recursive = TRUE)
      }

      data_sources <- file.path(main_repo_dir, "data", module_cfg$data_files)
      tryCatch({
        for (data_file in module_cfg$data_files) {
          source_path <- file.path(main_repo_dir, "data", data_file)
          if (file.exists(source_path)) {
            fs::file_copy(source_path, file.path(data_dir, data_file), overwrite = TRUE)
          } else {
            warning("⚠️ Data file not found: ", source_path)
          }
        }
      }, error = function(e) {
        warning("⚠️ Error copying data files for ", module_name, ": ", e$message)
      })
    } else if (!copy_data_files) {
      cat("  ⏭️ Skipping", module_name, "data files (copy_data_files: false)\n")
    }

    # Copy R files
    if (copy_r_files && length(module_cfg$r_files) > 0) {
      cat("  📁 Copying", module_name, "R files...\n")
      r_dir <- file.path(module_dir, "R")
      if (!dir.exists(r_dir)) {
        dir.create(r_dir, recursive = TRUE)
      }

      tryCatch({
        for (r_file in module_cfg$r_files) {
          source_path <- file.path(main_repo_dir, "R", r_file)
          if (file.exists(source_path)) {
            fs::file_copy(source_path, file.path(r_dir, r_file), overwrite = TRUE)
          } else {
            warning("⚠️ R file not found: ", source_path)
          }
        }
      }, error = function(e) {
        warning("⚠️ Error copying R files for ", module_name, ": ", e$message)
      })
    } else if (!copy_r_files) {
      cat("  ⏭️ Skipping", module_name, "R files (copy_r_files: false)\n")
    }

    # Copy test files
    if (copy_test_files && length(module_cfg$test_files) > 0) {
      cat("  📁 Copying", module_name, "test files...\n")
      test_dir <- file.path(module_dir, "tests/testthat")
      if (!dir.exists(test_dir)) {
        dir.create(test_dir, recursive = TRUE)
      }

      tryCatch({
        for (test_file in module_cfg$test_files) {
          source_path <- file.path(main_repo_dir, "tests/testthat", test_file)
          if (file.exists(source_path)) {
            fs::file_copy(source_path, file.path(test_dir, test_file), overwrite = TRUE)
          } else {
            warning("⚠️ Test file not found: ", source_path)
          }
        }
      }, error = function(e) {
        warning("⚠️ Error copying test files for ", module_name, ": ", e$message)
      })
    } else if (!copy_test_files) {
      cat("  ⏭️ Skipping", module_name, "test files (copy_test_files: false)\n")
    }

    # Copy vignette files (manual files when domain-based is disabled)
    if (copy_vignettes && length(module_cfg$vignette_files) > 0 &&
        (!config$vignette_domains$copy_settings$use_domain_based %||% FALSE)) {
      cat("  📄 Copying", module_name, "vignette files (manual)...\n")
      vignette_dir <- file.path(module_dir, "vignettes")
      if (!dir.exists(vignette_dir)) {
        dir.create(vignette_dir, recursive = TRUE)
      }

      tryCatch({
        for (vignette_file in module_cfg$vignette_files) {
          source_path <- file.path(main_repo_dir, "vignettes", vignette_file)
          if (file.exists(source_path)) {
            fs::file_copy(source_path, file.path(vignette_dir, vignette_file), overwrite = TRUE)
          } else {
            warning("⚠️ Vignette file not found: ", source_path)
          }
        }
      }, error = function(e) {
        warning("⚠️ Error copying vignette files for ", module_name, ": ", e$message)
      })
    } else if (!copy_vignettes) {
      cat("  ⏭️ Skipping", module_name, "vignette files (copy_vignettes: false)\n")
    }
  }
}

# Legacy copy example files to each module directory ----

if (!WIP) {
  ## jjstatsplot_example_files ----
  if (copy_data_files) {
    cat("📁 Copying jjstatsplot data files...\n")

    jjstatsplot_example_files <- c(
      "histopathology.rda"
      # "histopathologyGraphsPlots.omv"
    )

    # Create directories if they do not exist
    if (!dir.exists(file.path(jjstatsplot_dir, "data"))) {
      dir.create(file.path(jjstatsplot_dir, "data"), recursive = TRUE)
    }

    fs::file_copy(
      file.path(main_repo_dir, "data", jjstatsplot_example_files),
      file.path(jjstatsplot_dir, "data"),
      overwrite = TRUE
    )
  } else {
    cat("⏭️ Skipping jjstatsplot data files (copy_data_files: false)\n")
  }

  ## jjstatsplot_data_description_files ----
  if (copy_r_files) {
    cat("📁 Copying jjstatsplot R files...\n")

    jjstatsplot_data_description_files <- c(
      # "data-histopathology.R"
    )

    # Create directories if they do not exist
    if (!dir.exists(file.path(jjstatsplot_dir, "R"))) {
      dir.create(file.path(jjstatsplot_dir, "R"), recursive = TRUE)
    }

    if (length(jjstatsplot_data_description_files) > 0) {
      fs::file_copy(
        file.path(main_repo_dir, "R", jjstatsplot_data_description_files),
        file.path(jjstatsplot_dir, "R"),
        overwrite = TRUE
      )
    }
  } else {
    cat("⏭️ Skipping jjstatsplot R files (copy_r_files: false)\n")
  }




  ## jjstatsplot_vignettes ----
  if (copy_vignettes) {
    cat("📁 Copying jjstatsplot vignette files...\n")

    jjstatsplot_vignette_files <- c(
      "jjstatsplot-introduction.Rmd",
      "correlations-scatterplots.Rmd",
      "continuous-comparisons.Rmd",
      "categorical-plots.Rmd"
    )

    # Create directories if they do not exist
    if (!dir.exists(file.path(jjstatsplot_dir, "vignettes"))) {
      dir.create(file.path(jjstatsplot_dir, "vignettes"), recursive = TRUE)
    }

    tryCatch({
      fs::file_copy(
        file.path(main_repo_dir, "vignettes", jjstatsplot_vignette_files),
        file.path(jjstatsplot_dir, "vignettes"),
        overwrite = TRUE
      )
    }, error = function(e) {
      message("Error copying jjstatsplot vignette files: ", e$message)
    })
  } else {
    cat("⏭️ Skipping jjstatsplot vignette files (copy_vignettes: false)\n")
  }




  ## meddecide_example_files ----
  if (copy_data_files) {
    cat("📁 Copying meddecide data files...\n")

    meddecide_example_files <- c(
      "histopathology.rda",
      # "histopathologyMedicalDecision.omv"
      "roc_analysis_test_data.RData",
      "cancer_biomarker_data.csv",
      "cardiac_troponin_data.csv",
      "sepsis_biomarker_data.csv",
      "thyroid_function_data.csv"
    )

    # Create directories if they do not exist
    if (!dir.exists(file.path(meddecide_dir, "data"))) {
      dir.create(file.path(meddecide_dir, "data"), recursive = TRUE)
    }

    fs::file_copy(
      file.path(main_repo_dir, "data", meddecide_example_files),
      file.path(meddecide_dir, "data"),
      overwrite = TRUE
    )
  } else {
    cat("⏭️ Skipping meddecide data files (copy_data_files: false)\n")
  }


  ## meddecide_utility_data_description_files ----
  if (copy_r_files) {
    cat("📁 Copying meddecide R files...\n")

    meddecide_utility_data_description_files <- c(
      "psychopdaroc_utilities.R",
      "nomogrammer.R",
      # "meddecide-package.R",
      # "meddecide-data.R",
      "stats_utils.R",  # Replaces meddecide_stats_utils.R
      # "data-histopathology.R",
      "diagnostic_metrics.R"
    )

    # Create directories if they do not exist
    if (!dir.exists(file.path(meddecide_dir, "R"))) {
      dir.create(file.path(meddecide_dir, "R"), recursive = TRUE)
    }

    fs::file_copy(
      file.path(
        main_repo_dir,
        "R",
        meddecide_utility_data_description_files
      ),
      file.path(meddecide_dir, "R"),
      overwrite = TRUE
    )
  } else {
    cat("⏭️ Skipping meddecide R files (copy_r_files: false)\n")
  }



  ## meddecide_vignettes ----
  if (copy_vignettes) {
    cat("📁 Copying meddecide vignette files...\n")

    meddecide_vignette_files <- c(
      "nogoldstandard.Rmd",
      "diagnostic-tests.Rmd",
      "decisionpanel_advanced.Rmd",
      "decisionpanel_optimisation.Rmd",
      "decisionpanel_clinical.Rmd",
      "meddecide-vignettes.Rmd",
      "medical_decision_tree_guide.Rmd"
    )

    # Create directories if they do not exist
    if (!dir.exists(file.path(meddecide_dir, "vignettes"))) {
      dir.create(file.path(meddecide_dir, "vignettes"), recursive = TRUE)
    }

    tryCatch({
      fs::file_copy(
        file.path(main_repo_dir, "vignettes", meddecide_vignette_files),
        file.path(meddecide_dir, "vignettes"),
        overwrite = TRUE
      )
    }, error = function(e) {
      message("Error copying meddecide vignette files: ", e$message)
    })
  } else {
    cat("⏭️ Skipping meddecide vignette files (copy_vignettes: false)\n")
  }



  ## meddecide_testdata_files ----
  if (copy_test_files) {
    cat("📁 Copying meddecide test files...\n")

    meddecide_data_description_files <- c("test-decision.R", "test-roc.R")

    # Create directories if they do not exist
    if (!dir.exists(file.path(meddecide_dir, "tests/testthat"))) {
      dir.create(file.path(meddecide_dir, "tests/testthat"), recursive = TRUE)
    }

    fs::file_copy(
      file.path(
        main_repo_dir,
        "tests/testthat",
        meddecide_data_description_files
      ),
      file.path(meddecide_dir, "tests/testthat"),
      overwrite = TRUE
    )
  } else {
    cat("⏭️ Skipping meddecide test files (copy_test_files: false)\n")
  }





  ## jsurvival_example_files ----
  if (copy_data_files) {
    cat("📁 Copying jsurvival data files...\n")

    jsurvival_example_files <- c(
      "histopathology.rda"
      # "histopathologySurvival.omv"
    )

    # Create directories if they do not exist
    if (!dir.exists(file.path(jsurvival_dir, "data"))) {
      dir.create(file.path(jsurvival_dir, "data"), recursive = TRUE)
    }

    fs::file_copy(
      file.path(main_repo_dir, "data", jsurvival_example_files),
      file.path(jsurvival_dir, "data"),
      overwrite = TRUE
    )
  } else {
    cat("⏭️ Skipping jsurvival data files (copy_data_files: false)\n")
  }

  ## jsurvival_data_description_files ----
  if (copy_r_files) {
    cat("📁 Copying jsurvival R files...\n")

    jsurvival_data_description_files <- c(
      # "data-histopathology.R"
    )

    # Create directories if they do not exist
    if (!dir.exists(file.path(jsurvival_dir, "R"))) {
      dir.create(file.path(jsurvival_dir, "R"), recursive = TRUE)
    }

    if (length(jsurvival_data_description_files) > 0) {
      fs::file_copy(
        file.path(main_repo_dir, "R", jsurvival_data_description_files),
        file.path(jsurvival_dir, "R"),
        overwrite = TRUE
      )
    }
  } else {
    cat("⏭️ Skipping jsurvival R files (copy_r_files: false)\n")
  }




  ## jsurvival_vignettes ----
  if (copy_vignettes) {
    cat("📁 Copying jsurvival vignette files...\n")

    jsurvival_vignette_files <- c("jsurvival.Rmd")

    # Create directories if they do not exist
    if (!dir.exists(file.path(jsurvival_dir, "vignettes"))) {
      dir.create(file.path(jsurvival_dir, "vignettes"), recursive = TRUE)
    }

    tryCatch({
      fs::file_copy(
        file.path(main_repo_dir, "vignettes", jsurvival_vignette_files),
        file.path(jsurvival_dir, "vignettes"),
        overwrite = TRUE
      )
    }, error = function(e) {
      message("Error copying jsurvival vignette files: ", e$message)
    })
  } else {
    cat("⏭️ Skipping jsurvival vignette files (copy_vignettes: false)\n")
  }








  ## ClinicoPathDescriptives_example_files ----
  if (copy_data_files) {
    cat("📁 Copying ClinicoPathDescriptives data files...\n")

    ClinicoPathDescriptives_example_files <- c(
      "histopathology.rda",
      "histopathologyDescriptives.omv",
      "treatmentResponse.omv",
      "treatmentResponse.rda"
      # "swimmer_data_raw.omv",
      # "swimmer_data_raw.csv",
      # "swimmer_data_raw.rda",
      # "swimmer_data_date_formats.omv",
      # "swimmer_data_date_formats.csv",
      # "swimmer_data_date_formats.rda",
      # "swimmer_data.csv",
      # "swimmer_data.rda"
      # "raw_with_time.rda",
      # "raw_with_time.csv",
      # "percent_with_time.rda",
      # "percent_with_time.csv",
      # "percent_no_time.rda",
      # "percent_no_time.csv",
      # "tumor_response_examples.rda"
      # "swimmer_plot_base_data.csv",
      # "swimmer_plot_milestone_data.csv"
      # "patientTimelinesDates.rda",
      # "patientTimelines.rda",
      # "patientTimelinesDates.csv",
      # "patientTimelines.csv"
    )

    # Create directories if they do not exist
    if (!dir.exists(file.path(ClinicoPathDescriptives_dir, "data"))) {
      dir.create(file.path(ClinicoPathDescriptives_dir, "data"),
                 recursive = TRUE)
    }

    tryCatch({
      fs::file_copy(
        file.path(
          main_repo_dir,
          "data",
          ClinicoPathDescriptives_example_files
        ),
        file.path(ClinicoPathDescriptives_dir, "data"),
        overwrite = TRUE
      )
    }, error = function(e) {
      message("Error copying ClinicoPathDescriptives example files: ",
              e$message)
    })
  } else {
    cat("⏭️ Skipping ClinicoPathDescriptives data files (copy_data_files: false)\n")
  }



  ## ClinicoPathDescriptives_data_description_files ----
  if (copy_r_files) {
    cat("📁 Copying ClinicoPathDescriptives R files...\n")

    ClinicoPathDescriptives_data_description_files <- c(
      # "data-histopathology.R",
      # "data-treatmentResponse.R",
      # "tumor_response_examples.R"
      # "data-swimmer-plot-documentation.r"
      # "ClinicoPathDescriptives-package.R"
    )

    # Create directories if they do not exist
    if (!dir.exists(file.path(ClinicoPathDescriptives_dir, "R"))) {
      dir.create(file.path(ClinicoPathDescriptives_dir, "R"), recursive = TRUE)
    }

    if (length(ClinicoPathDescriptives_data_description_files) > 0) {
      fs::file_copy(
        file.path(
          main_repo_dir,
          "R",
          ClinicoPathDescriptives_data_description_files
        ),
        file.path(ClinicoPathDescriptives_dir, "R"),
        overwrite = TRUE
      )
    }
  } else {
    cat("⏭️ Skipping ClinicoPathDescriptives R files (copy_r_files: false)\n")
  }



  # ihc_test_data.csv

  # tumor_response_data.csv
  # oncology_response_data.csv
  #
  # heartdisease.omv
  # heartdisease.xlsx
  #
  #
  # BreastCancer.csv
  # BreastCancer.omv
  # BreastCancer.rda
  # colon.csv
  # colon.omv
  # colon.rda
  # melanoma.csv
  # melanoma.omv
  # melanoma.rda
  #
  # histopathology.csv
  # histopathology.omv
  # histopathology.rda
  #
  #
  #
  # pmid.omv
  # retractiondio2.omv
  # retractiondoi.omv
  #
  # rocdata.csv
  # rocdata.omv
  # rocdata.rda



  ## ClinicoPathDescriptives_vignettes ----
  if (copy_vignettes) {
    cat("📁 Copying ClinicoPathDescriptives vignette files...\n")

    ClinicoPathDescriptives_vignette_files <- c("data-summary.Rmd", "visualization.Rmd")

    # Create directories if they do not exist
    if (!dir.exists(file.path(ClinicoPathDescriptives_dir, "vignettes"))) {
      dir.create(file.path(ClinicoPathDescriptives_dir, "vignettes"),
                 recursive = TRUE)
    }

    tryCatch({
      fs::file_copy(
        file.path(
          main_repo_dir,
          "vignettes",
          ClinicoPathDescriptives_vignette_files
        ),
        file.path(ClinicoPathDescriptives_dir, "vignettes"),
        overwrite = TRUE
      )
    }, error = function(e) {
      message("Error copying ClinicoPathDescriptives vignette files: ",
              e$message)
    })
  } else {
    cat("⏭️ Skipping ClinicoPathDescriptives vignette files (copy_vignettes: false)\n")
  }




}





#  Module files ----

a_yaml_files <- list.files(
  path = "./jamovi",
  pattern = "\\.a\\.yaml$",
  recursive = TRUE,
  full.names = TRUE
)
## jjstatsplot module functions ----

jjstatsplot_a_yaml_files <- purrr::keep(a_yaml_files, function(f) {
  any(grepl("menuGroup: JJStatsPlot$", readLines(f, warn = FALSE)))
})

jjstatsplot_a_yaml_files <- gsub(pattern = "./jamovi/",
                                 replacement = "",
                                 x = jjstatsplot_a_yaml_files)
jjstatsplot_a_yaml_files <- gsub(pattern = ".a.yaml",
                                 replacement = "",
                                 x = jjstatsplot_a_yaml_files)

jjstatsplot_modules <- jjstatsplot_a_yaml_files

if (WIP) {
  jjstatsplot_a_yaml_files <- purrr::keep(a_yaml_files, function(f) {
    any(grepl("menuGroup: JJStatsPlot", readLines(f, warn = FALSE)))
  })

  jjstatsplot_a_yaml_files <- gsub(pattern = "./jamovi/",
                                   replacement = "",
                                   x = jjstatsplot_a_yaml_files)
  jjstatsplot_a_yaml_files <- gsub(pattern = ".a.yaml",
                                   replacement = "",
                                   x = jjstatsplot_a_yaml_files)

  jjstatsplot_modules <- jjstatsplot_a_yaml_files
}


## meddecide module functions ----

meddecide_a_yaml_files <- purrr::keep(a_yaml_files, function(f) {
  any(grepl("menuGroup: meddecide$", readLines(f, warn = FALSE)))
})
meddecide_a_yaml_files <- gsub(pattern = "./jamovi/",
                               replacement = "",
                               x = meddecide_a_yaml_files)
meddecide_a_yaml_files <- gsub(pattern = ".a.yaml",
                               replacement = "",
                               x = meddecide_a_yaml_files)
meddecide_modules <- meddecide_a_yaml_files

if (WIP) {
  meddecide_a_yaml_files <- purrr::keep(a_yaml_files, function(f) {
    any(grepl("menuGroup: meddecide", readLines(f, warn = FALSE)))
  })

  meddecide_a_yaml_files <- gsub(pattern = "./jamovi/",
                                 replacement = "",
                                 x = meddecide_a_yaml_files)
  meddecide_a_yaml_files <- gsub(pattern = ".a.yaml",
                                 replacement = "",
                                 x = meddecide_a_yaml_files)

  meddecide_modules <- meddecide_a_yaml_files
}



## jsurvival module functions ----

jsurvival_a_yaml_files <- purrr::keep(a_yaml_files, function(f) {
  any(grepl("menuGroup: Survival$", readLines(f, warn = FALSE)))
})
jsurvival_a_yaml_files <- gsub(pattern = "./jamovi/",
                               replacement = "",
                               x = jsurvival_a_yaml_files)
jsurvival_a_yaml_files <- gsub(pattern = ".a.yaml",
                               replacement = "",
                               x = jsurvival_a_yaml_files)
jsurvival_modules <- jsurvival_a_yaml_files

if (WIP) {
  jsurvival_a_yaml_files <- purrr::keep(a_yaml_files, function(f) {
    any(grepl("menuGroup: Survival", readLines(f, warn = FALSE)))
  })

  jsurvival_a_yaml_files <- gsub(pattern = "./jamovi/",
                                 replacement = "",
                                 x = jsurvival_a_yaml_files)
  jsurvival_a_yaml_files <- gsub(pattern = ".a.yaml",
                                 replacement = "",
                                 x = jsurvival_a_yaml_files)

  jsurvival_modules <- jsurvival_a_yaml_files
}





## ClinicoPathDescriptives module functions ----

ClinicoPathDescriptives_a_yaml_files <- purrr::keep(a_yaml_files, function(f) {
  any(grepl("menuGroup: Exploration$", readLines(f, warn = FALSE)))
})
ClinicoPathDescriptives_a_yaml_files <- gsub(pattern = "./jamovi/",
                                             replacement = "",
                                             x = ClinicoPathDescriptives_a_yaml_files)
ClinicoPathDescriptives_a_yaml_files <- gsub(pattern = ".a.yaml",
                                             replacement = "",
                                             x = ClinicoPathDescriptives_a_yaml_files)
ClinicoPathDescriptives_modules <- ClinicoPathDescriptives_a_yaml_files

if (WIP) {
  ClinicoPathDescriptives_a_yaml_files <- purrr::keep(a_yaml_files, function(f) {
    any(grepl("menuGroup: Exploration", readLines(f, warn = FALSE)))
  })

  ClinicoPathDescriptives_a_yaml_files <- gsub(pattern = "./jamovi/",
                                               replacement = "",
                                               x = ClinicoPathDescriptives_a_yaml_files)
  ClinicoPathDescriptives_a_yaml_files <- gsub(pattern = ".a.yaml",
                                               replacement = "",
                                               x = ClinicoPathDescriptives_a_yaml_files)

  ClinicoPathDescriptives_modules <- ClinicoPathDescriptives_a_yaml_files
}



# Update DESCRIPTION files ----
description_paths <- c(
  file.path(main_repo_dir, "DESCRIPTION"),
  # Main repository
  file.path(jjstatsplot_dir, "DESCRIPTION"),
  # jjstatsplot repository
  file.path(meddecide_dir, "DESCRIPTION"),
  # meddecide repository
  file.path(jsurvival_dir, "DESCRIPTION"),
  # jsurvival repository
  file.path(ClinicoPathDescriptives_dir, "DESCRIPTION")   # ClinicoPathDescriptives repository
)
update_description_files(paths = description_paths,
                         version = new_version,
                         date = new_date)


# Update YAML files ----
yaml_0000_paths <- c(
  file.path(main_repo_dir, "jamovi", "0000.yaml"),
  file.path(jjstatsplot_dir, "jamovi", "0000.yaml"),
  file.path(meddecide_dir, "jamovi", "0000.yaml"),
  file.path(jsurvival_dir, "jamovi", "0000.yaml"),
  file.path(ClinicoPathDescriptives_dir, "jamovi", "0000.yaml")
)

modules <- c(
  jjstatsplot_modules,
  meddecide_modules,
  jsurvival_modules,
  ClinicoPathDescriptives_modules
)

yaml_a_paths <- c(
  file.path(main_repo_dir, "jamovi", paste0(modules, ".a.yaml")),
  file.path(
    jjstatsplot_dir,
    "jamovi",
    paste0(jjstatsplot_modules, ".a.yaml")
  ),
  file.path(meddecide_dir, "jamovi", paste0(meddecide_modules, ".a.yaml")),
  file.path(jsurvival_dir, "jamovi", paste0(jsurvival_modules, ".a.yaml")),
  file.path(
    ClinicoPathDescriptives_dir,
    "jamovi",
    paste0(ClinicoPathDescriptives_modules, ".a.yaml")
  )
)

yaml_0000_paths <- yaml_0000_paths[file.exists(yaml_0000_paths)]
yaml_a_paths <- yaml_a_paths[file.exists(yaml_a_paths)]


# Update YAML files with new version
update_yaml_0000_files(paths = yaml_0000_paths,
                       version = new_version,
                       date = new_date)

update_yaml_a_files(paths = yaml_a_paths, version = new_version)

# Copy module files with enhanced error handling ----

cat("\n🔄 Copying jamovi module files to target repositories...\n")

# jjstatsplot_modules
if (jjstatsplot_module && length(jjstatsplot_modules) > 0) {
  cat("\n📋 Processing jjstatsplot modules...\n")

  # Copy R backend files
  copy_module_files(
    jjstatsplot_modules,
    source_dir = file.path(main_repo_dir, "R"),
    dest_dir = file.path(jjstatsplot_dir, "R"),
    file_extensions = c(".b.R")
  )

  # Ensure jamovi directory exists
  jamovi_dir <- file.path(jjstatsplot_dir, "jamovi")
  if (!dir.exists(jamovi_dir)) {
    cat("  📁 Creating jamovi directory: ", jamovi_dir, "\n")
    dir.create(jamovi_dir, recursive = TRUE)
  }

  # Copy jamovi definition files
  copy_module_files(
    jjstatsplot_modules,
    source_dir = file.path(main_repo_dir, "jamovi"),
    dest_dir = jamovi_dir,
    file_extensions = c(".a.yaml", ".r.yaml", ".u.yaml")
  )
} else {
  cat("\n⏭️ Skipping jjstatsplot modules (disabled or no modules found)\n")
}

# meddecide_modules
if (meddecide_module && length(meddecide_modules) > 0) {
  cat("\n🎩 Processing meddecide modules...\n")

  # Copy R backend files
  copy_module_files(
    meddecide_modules,
    source_dir = file.path(main_repo_dir, "R"),
    dest_dir = file.path(meddecide_dir, "R"),
    file_extensions = c(".b.R")
  )

  # Ensure jamovi directory exists
  jamovi_dir <- file.path(meddecide_dir, "jamovi")
  if (!dir.exists(jamovi_dir)) {
    cat("  📁 Creating jamovi directory: ", jamovi_dir, "\n")
    dir.create(jamovi_dir, recursive = TRUE)
  }

  # Copy jamovi definition files
  copy_module_files(
    meddecide_modules,
    source_dir = file.path(main_repo_dir, "jamovi"),
    dest_dir = jamovi_dir,
    file_extensions = c(".a.yaml", ".r.yaml", ".u.yaml")
  )
} else {
  cat("\n⏭️ Skipping meddecide modules (disabled or no modules found)\n")
}


# jsurvival_modules
if (jsurvival_module && length(jsurvival_modules) > 0) {
  cat("\n⚔️ Processing jsurvival modules...\n")

  # Copy R backend files
  copy_module_files(
    jsurvival_modules,
    source_dir = file.path(main_repo_dir, "R"),
    dest_dir = file.path(jsurvival_dir, "R"),
    file_extensions = c(".b.R")
  )

  # Ensure jamovi directory exists
  jamovi_dir <- file.path(jsurvival_dir, "jamovi")
  if (!dir.exists(jamovi_dir)) {
    cat("  📁 Creating jamovi directory: ", jamovi_dir, "\n")
    dir.create(jamovi_dir, recursive = TRUE)
  }

  # Copy jamovi definition files
  copy_module_files(
    jsurvival_modules,
    source_dir = file.path(main_repo_dir, "jamovi"),
    dest_dir = jamovi_dir,
    file_extensions = c(".a.yaml", ".r.yaml", ".u.yaml")
  )
} else {
  cat("\n⏭️ Skipping jsurvival modules (disabled or no modules found)\n")
}


# ClinicoPathDescriptives_modules
if (ClinicoPathDescriptives_module && length(ClinicoPathDescriptives_modules) > 0) {
  cat("\n🔬 Processing ClinicoPathDescriptives modules...\n")

  # Copy R backend files
  copy_module_files(
    ClinicoPathDescriptives_modules,
    source_dir = file.path(main_repo_dir, "R"),
    dest_dir = file.path(ClinicoPathDescriptives_dir, "R"),
    file_extensions = c(".b.R")
  )

  # Ensure jamovi directory exists
  jamovi_dir <- file.path(ClinicoPathDescriptives_dir, "jamovi")
  if (!dir.exists(jamovi_dir)) {
    cat("  📁 Creating jamovi directory: ", jamovi_dir, "\n")
    dir.create(jamovi_dir, recursive = TRUE)
  }

  # Copy jamovi definition files
  copy_module_files(
    ClinicoPathDescriptives_modules,
    source_dir = file.path(main_repo_dir, "jamovi"),
    dest_dir = jamovi_dir,
    file_extensions = c(".a.yaml", ".r.yaml", ".u.yaml")
  )
} else {
  cat("\n⏭️ Skipping ClinicoPathDescriptives modules (disabled or no modules found)\n")
}




## Function to replace ClinicoPath with given module name in R and Rmd files ----
replace_clinicopath_with_module <- function(base_dir, module_name) {
  if (!dir.exists(base_dir)) {
    message("Directory does not exist: ", base_dir)
    return()
  }
  # Get all R and Rmd files in the directory recursively
  files <- list.files(
    path = base_dir,
    pattern = "\\.(R|Rmd|rmd)$",
    full.names = TRUE,
    recursive = TRUE
  )
  # Perform replacements
  xfun::gsub_files(
    files = files,
    pattern = "library\\(ClinicoPath\\)",
    replacement = paste0("library(", module_name, ")")
  )
  xfun::gsub_files(
    files = files,
    pattern = "ClinicoPath::",
    replacement = paste0(module_name, "::")
  )
}

if (!WIP & webpage) {
  ## --- Replace ClinicoPath references in module code ----
  replace_clinicopath_with_module(jjstatsplot_dir, "jjstatsplot")
  replace_clinicopath_with_module(meddecide_dir, "meddecide")
  replace_clinicopath_with_module(jsurvival_dir, "jsurvival")
  replace_clinicopath_with_module(ClinicoPathDescriptives_dir, "ClinicoPathDescriptives")
}

# --- Prepare, document, and install modules ----
if (!extended) {
  jmvtools::prepare(main_repo_dir)
  devtools::document(main_repo_dir)
  jmvtools::prepare(main_repo_dir)
  devtools::document(main_repo_dir)
  # jmvtools::install(main_repo_dir)
}

# --- Commit changes in each repository ----
if (commit_modules || !WIP) {
  cat("\n📦 Committing changes to repositories...\n")

  commit_message <- sprintf("Update modules to version %s and date %s",
                            new_version, new_date)

  # Always commit main repository
  cat("📁 Committing main repository...\n")
  main_commit_success <- commit_repo_enhanced(main_repo_dir, commit_message)

  if (commit_modules) {
    cat("📁 Committing module repositories...\n")

    commit_results <- list()
    if (jjstatsplot_module) {
      commit_results$jjstatsplot <- commit_repo_enhanced(jjstatsplot_dir, commit_message)
    }
    if (meddecide_module) {
      commit_results$meddecide <- commit_repo_enhanced(meddecide_dir, commit_message)
    }
    if (jsurvival_module) {
      commit_results$jsurvival <- commit_repo_enhanced(jsurvival_dir, commit_message)
    }
    if (ClinicoPathDescriptives_module) {
      commit_results$ClinicoPathDescriptives <- commit_repo_enhanced(ClinicoPathDescriptives_dir, commit_message)
    }

    # Report commit summary
    successful_commits <- sum(unlist(commit_results), na.rm = TRUE)
    total_commits <- length(commit_results)
    cat("📦 Module commits: ", successful_commits, "/", total_commits, " successful\n")
  } else {
    cat("⏭️ Skipping module commits (commit_modules: false)\n")
  }
} else {
  cat("\n⏭️ Skipping all commits (WIP mode or commit disabled)\n")
}

# Final status report ----
cat("\n🎉 ====== UPDATE PROCESS COMPLETED ======\n")
cat("✅ Modules updated to version:", new_version, "\n")
cat("✅ Date updated to:", new_date, "\n")
cat("📁 Main repository:", main_repo_dir, "\n")

if (WIP) {
  cat("🔧 WIP mode was enabled - using sandbox directories\n")
}

active_modules <- sum(c(jjstatsplot_module, meddecide_module, jsurvival_module, ClinicoPathDescriptives_module))
cat("📊 Active modules:", active_modules, "/4\n")

if (active_modules > 0) {
  if (jjstatsplot_module) cat("  ✅ jjstatsplot\n")
  if (meddecide_module) cat("  ✅ meddecide\n")
  if (jsurvival_module) cat("  ✅ jsurvival\n")
  if (ClinicoPathDescriptives_module) cat("  ✅ ClinicoPathDescriptives\n")
}

cat("\n🎉 Module update process completed successfully!\n")
# }




# Run the update process ----

# update_modules(new_version, new_date)

# tryCatch({
#     update_modules(new_version, new_date)
# }, error = function(e) {
#     message("Error during module update: ", e$message)
# })


# Extended processing with enhanced error handling ----
if (extended) {
  cat("\n🔧 Extended processing mode enabled...\n")

  if (jjstatsplot_module) {
    cat("\n📋 Processing jjstatsplot package...\n")
    old_wd <- getwd()
    tryCatch({
      setwd(jjstatsplot_dir)
      cat("  📄 Preparing package...\n")
      jmvtools::prepare()
      cat("  📝 Documenting...\n")
      devtools::document()
      jmvtools::prepare()
      devtools::document()
      cat("  📦 Installing...\n")
      jmvtools::install()

      if (check) {
        cat("  🔍 Running R CMD check...\n")
        devtools::check()
      }
      if (webpage) {
        cat("  🌐 Building website...\n")
        pkgdown::build_site()
      }
      cat("  ✅ jjstatsplot processing completed\n")
    }, error = function(e) {
      warning("⚠️ Error processing jjstatsplot: ", e$message)
    }, finally = {
      setwd(old_wd)
    })
  }

  if (meddecide_module) {
    setwd(meddecide_dir)
    jmvtools::prepare()
    devtools::document()
    jmvtools::prepare()
    devtools::document()
    jmvtools::install()

    if (check) {
      devtools::check()
    }
    if (webpage) {
      pkgdown::build_site()
    }
  }

  if (jsurvival_module) {
    setwd(jsurvival_dir)
    jmvtools::prepare()
    devtools::document()
    jmvtools::prepare()
    devtools::document()
    jmvtools::install()

    if (check) {
      devtools::check()
    }
    if (webpage) {
      pkgdown::build_site()
    }
  }

  if (ClinicoPathDescriptives_module) {
    setwd(ClinicoPathDescriptives_dir)
    jmvtools::prepare()
    devtools::document()
    jmvtools::prepare()
    devtools::document()
    jmvtools::install()

    if (check) {
      devtools::check()
    }
    if (webpage) {
      pkgdown::build_site()
    }
  }

}

setwd(main_repo_dir)


# if (!WIP & !extended) {
#   # Update the main repository
#   jmvtools::prepare()
#   devtools::document()
#   jmvtools::prepare()
#   devtools::document()
#   jmvtools::install()

#   if (check) {
#     devtools::check()
#   }

#   if (webpage) {
#     pkgdown::build_site()
#   }
# }
