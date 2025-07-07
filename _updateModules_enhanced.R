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
# Usage: Rscript _updateModules_enhanced.R [config_file]
#
# Author: Enhanced version with enterprise-grade features
# Requires: R >= 4.0.0, yaml, future, digest packages

# Command line argument handling
args <- commandArgs(trailingOnly = TRUE)
config_file <- if (length(args) > 0) args[1] else "config.yaml"

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

# Source utility functions
if (!file.exists("module_utils.R")) {
  stop("❌ module_utils.R not found. Please ensure all required files are present.")
}

source("module_utils.R")

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

# Extract module directories from configuration
module_dirs <- list()
for (module_name in names(modules_config)) {
  if (modules_config[[module_name]]$enabled) {
    module_dirs[[module_name]] <- modules_config[[module_name]]$directory
  }
}

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

# Enhanced function to copy module files with validation and performance
copy_module_files_enhanced <- function(module_names, source_dir, dest_dir, file_extensions) {
  cat("\n📁 Copying module files...\n")

  if (!dir.exists(source_dir)) {
    stop("❌ Source directory does not exist: ", source_dir)
  }

  if (!dir.exists(dest_dir)) {
    cat("  📁 Creating destination directory:", dest_dir, "\n")
    dir.create(dest_dir, recursive = TRUE)
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

  cat("📁 Module file copy summary:", copied_count, "copied,",
      skipped_count, "skipped,", failed_count, "failed\n")

  return(list(copied = copied_count, skipped = skipped_count, failed = failed_count))
}

# Enhanced Git commit function with validation
commit_repo_enhanced <- function(repo_dir, commit_message, validate_repo = TRUE) {
  if (!dir.exists(repo_dir)) {
    warning("⚠️ Repository directory does not exist: ", repo_dir)
    return(FALSE)
  }

  old_wd <- getwd()
  on.exit(setwd(old_wd))

  tryCatch({
    setwd(repo_dir)

    # Validate it's a git repository
    if (validate_repo && !dir.exists(".git")) {
      warning("⚠️ Not a git repository: ", repo_dir)
      return(FALSE)
    }

    # Check if there are changes to commit
    status_result <- system("git status --porcelain", intern = TRUE)
    if (length(status_result) == 0) {
      cat("  ℹ️ No changes to commit in:", basename(repo_dir), "\n")
      return(TRUE)
    }

    # Add all changes
    add_result <- system("git add -A", intern = TRUE)

    # Commit with message
    commit_result <- system(sprintf("git commit -m \"%s\"", commit_message), intern = TRUE)

    cat("  ✅ Committed changes in:", basename(repo_dir), "\n")
    return(TRUE)

  }, error = function(e) {
    warning("⚠️ Git commit failed for ", repo_dir, ": ", e$message)
    return(FALSE)
  })
}

# Main execution starts here
cat("\n🎯 Starting module distribution process...\n")

# Create backups if enabled
backup_paths <- list()
if (config$backup$enabled %||% TRUE) {
  cat("\n💾 Creating backups...\n")
  for (module_name in names(module_dirs)) {
    backup_path <- create_backup(module_dirs[[module_name]], config$backup$backup_location %||% "backups")
    if (!is.null(backup_path)) {
      backup_paths[[module_name]] <- backup_path
    }
  }
}

# Validate module directories and dependencies
cat("\n🔍 Validating modules...\n")
validation_success <- TRUE

for (module_name in names(module_dirs)) {
  module_dir <- module_dirs[[module_name]]
  module_cfg <- modules_config[[module_name]]

  cat("🔍 Validating", module_name, "\n")

  # Validate module integrity
  integrity_result <- with_error_handling({
    validate_module_integrity(
      module_dir,
      module_name,
      module_cfg$required_directories
    )
  }, paste("validating module integrity for", module_name), continue_on_error = TRUE)

  if (!integrity_result$success) {
    validation_success <- FALSE
  }

  # Check dependencies
  deps_result <- with_error_handling({
    check_module_dependencies(module_dir)
  }, paste("checking dependencies for", module_name), continue_on_error = TRUE)

  if (!deps_result$success) {
    validation_success <- FALSE
  }
}

if (!validation_success) {
  stop("❌ Module validation failed. Please fix issues above before proceeding.")
}

# Discover modules by parsing YAML files
cat("\n🔍 Discovering modules...\n")

a_yaml_files <- list.files(
  path = "./jamovi",
  pattern = "\\.a\\.yaml$",
  recursive = TRUE,
  full.names = TRUE
)

# Function to extract modules for each submodule
extract_modules <- function(menu_group_pattern, wip_mode = FALSE) {
  pattern_to_use <- if (wip_mode) {
    gsub("\\$", "", menu_group_pattern)  # Remove end anchor for WIP mode
  } else {
    menu_group_pattern
  }

  matching_files <- purrr::keep(a_yaml_files, function(f) {
    any(grepl(pattern_to_use, readLines(f, warn = FALSE)))
  })

  module_names <- gsub(pattern = "./jamovi/", replacement = "", x = matching_files)
  module_names <- gsub(pattern = ".a.yaml", replacement = "", x = module_names)

  return(module_names)
}

# Extract modules for each submodule
modules <- list()
for (module_name in names(modules_config)) {
  if (modules_config[[module_name]]$enabled) {
    pattern <- if (WIP) {
      modules_config[[module_name]]$menuGroup_pattern_wip
    } else {
      modules_config[[module_name]]$menuGroup_pattern
    }

    modules[[module_name]] <- extract_modules(pattern, WIP)
    cat("📋", module_name, "modules:", length(modules[[module_name]]), "\n")
  }
}

# Update version files
description_paths <- c(file.path(main_repo_dir, "DESCRIPTION"))
yaml_0000_paths <- c(file.path(main_repo_dir, "jamovi", "0000.yaml"))
all_modules <- unlist(modules)
yaml_a_paths <- file.path(main_repo_dir, "jamovi", paste0(all_modules, ".a.yaml"))

# Add module paths
for (module_name in names(module_dirs)) {
  if (modules_config[[module_name]]$enabled) {
    description_paths <- c(description_paths, file.path(module_dirs[[module_name]], "DESCRIPTION"))
    yaml_0000_paths <- c(yaml_0000_paths, file.path(module_dirs[[module_name]], "jamovi", "0000.yaml"))

    module_yaml_paths <- file.path(module_dirs[[module_name]], "jamovi", paste0(modules[[module_name]], ".a.yaml"))
    yaml_a_paths <- c(yaml_a_paths, module_yaml_paths)
  }
}

# Filter existing files
description_paths <- description_paths[file.exists(description_paths)]
yaml_0000_paths <- yaml_0000_paths[file.exists(yaml_0000_paths)]
yaml_a_paths <- yaml_a_paths[file.exists(yaml_a_paths)]

# Update files with enhanced functions
update_description_files(description_paths, new_version, new_date)
update_yaml_0000_files(yaml_0000_paths, new_version, new_date)
update_yaml_a_files(yaml_a_paths, new_version)

# Copy assets and module files
if (!WIP) {
  cat("\n📁 Copying assets...\n")

  for (module_name in names(modules_config)) {
    if (!modules_config[[module_name]]$enabled) next

    module_cfg <- modules_config[[module_name]]
    module_dir <- module_dirs[[module_name]]

    cat("📁 Processing assets for", module_name, "\n")

    # Copy data files
    if (length(module_cfg$data_files) > 0) {
      data_dir <- file.path(module_dir, "data")
      if (!dir.exists(data_dir)) {
        dir.create(data_dir, recursive = TRUE)
      }

      data_sources <- file.path(main_repo_dir, "data", module_cfg$data_files)
      safe_copy_files(data_sources, data_dir,
                     check_integrity = config$security$verify_checksums %||% TRUE,
                     max_size_mb = config$security$max_file_size_mb %||% 100)
    }

    # Copy R files
    if (length(module_cfg$r_files) > 0) {
      r_dir <- file.path(module_dir, "R")
      if (!dir.exists(r_dir)) {
        dir.create(r_dir, recursive = TRUE)
      }

      r_sources <- file.path(main_repo_dir, "R", module_cfg$r_files)
      safe_copy_files(r_sources, r_dir)
    }

    # Copy vignette files
    if (length(module_cfg$vignette_files) > 0) {
      vignette_dir <- file.path(module_dir, "vignettes")
      if (!dir.exists(vignette_dir)) {
        dir.create(vignette_dir, recursive = TRUE)
      }

      vignette_sources <- file.path(main_repo_dir, "vignettes", module_cfg$vignette_files)
      safe_copy_files(vignette_sources, vignette_dir)
    }

    # Copy test files
    if (length(module_cfg$test_files) > 0) {
      test_dir <- file.path(module_dir, "tests/testthat")
      if (!dir.exists(test_dir)) {
        dir.create(test_dir, recursive = TRUE)
      }

      test_sources <- file.path(main_repo_dir, "tests/testthat", module_cfg$test_files)
      safe_copy_files(test_sources, test_dir)
    }
  }
}

# Copy module files (R and jamovi)
cat("\n📁 Copying module files...\n")

for (module_name in names(modules)) {
  if (!modules_config[[module_name]]$enabled) next

  module_dir <- module_dirs[[module_name]]
  module_files <- modules[[module_name]]

  cat("📁 Copying", module_name, "module files\n")

  # Copy R files
  copy_module_files_enhanced(
    module_files,
    source_dir = file.path(main_repo_dir, "R"),
    dest_dir = file.path(module_dir, "R"),
    file_extensions = c(".b.R")
  )

  # Copy jamovi files
  jamovi_dir <- file.path(module_dir, "jamovi")
  if (!dir.exists(jamovi_dir)) {
    dir.create(jamovi_dir, recursive = TRUE)
  }

  copy_module_files_enhanced(
    module_files,
    source_dir = file.path(main_repo_dir, "jamovi"),
    dest_dir = jamovi_dir,
    file_extensions = config$jamovi_extensions %||% c(".a.yaml", ".r.yaml", ".u.yaml")
  )
}

# Run tests if enabled
if (check || extended) {
  cat("\n🧪 Running module tests...\n")

  test_results <- list()

  for (module_name in names(module_dirs)) {
    if (!modules_config[[module_name]]$enabled) next

    cat("🧪 Testing", module_name, "\n")

    test_level <- if (extended) "full" else "basic"
    test_success <- run_module_tests(module_dirs[[module_name]], test_level)
    test_results[[module_name]] <- test_success

    if (!test_success) {
      warning("⚠️ Tests failed for ", module_name)
    }
  }

  failed_tests <- names(test_results)[!unlist(test_results)]
  if (length(failed_tests) > 0) {
    warning("⚠️ Tests failed for modules: ", paste(failed_tests, collapse = ", "))
  } else {
    cat("✅ All module tests passed\n")
  }
}

# Prepare, document, and install modules
if (!extended) {
  cat("\n🔧 Preparing main module...\n")

  with_error_handling({
    jmvtools::prepare(main_repo_dir)
    devtools::document(main_repo_dir)
    jmvtools::prepare(main_repo_dir)
    devtools::document(main_repo_dir)
    jmvtools::install(main_repo_dir)
  }, "preparing main module")
}

# Commit changes
cat("\n📝 Committing changes...\n")

commit_message <- sprintf("Enhanced update to version %s and date %s", new_version, new_date)

# Commit main repository
commit_repo_enhanced(main_repo_dir, commit_message)

# Commit submodules if enabled
if (commit_modules) {
  for (module_name in names(module_dirs)) {
    if (modules_config[[module_name]]$enabled) {
      commit_repo_enhanced(module_dirs[[module_name]], commit_message)
    }
  }
}

# Extended processing for individual modules
if (extended) {
  cat("\n🚀 Extended processing for individual modules...\n")

  for (module_name in names(module_dirs)) {
    if (!modules_config[[module_name]]$enabled) next

    module_dir <- module_dirs[[module_name]]
    cat("🚀 Processing", module_name, "\n")

    extended_result <- with_error_handling({
      old_wd <- getwd()
      setwd(module_dir)

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

      setwd(old_wd)

    }, paste("extended processing for", module_name), continue_on_error = TRUE)

    if (extended_result$success) {
      cat("  ✅ Extended processing completed for", module_name, "\n")
    } else {
      warning("  ⚠️ Extended processing failed for ", module_name)
    }
  }
}

# Final validation and summary
cat("\n📊 Module update summary\n")
cat("========================\n")
cat("Version:", new_version, "\n")
cat("Date:", new_date, "\n")
cat("Modules processed:", length(module_dirs), "\n")
cat("WIP mode:", if (WIP) "Yes" else "No", "\n")
cat("Backups created:", length(backup_paths), "\n")

if (length(backup_paths) > 0) {
  cat("Backup locations:\n")
  for (module_name in names(backup_paths)) {
    cat("  ", module_name, ":", backup_paths[[module_name]], "\n")
  }
}

cat("\n✅ Enhanced module update process completed successfully!\n")

# Return to main directory
setwd(main_repo_dir)
