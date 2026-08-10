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


Sys.unsetenv("ELECTRON_RUN_AS_NODE")

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
utility_file <- "_updateModules_utils.R"
if (!file.exists(utility_file)) {
  warning("⚠️ _updateModules_utils.R not found in current directory: ", getwd())
  cat("Looking for _updateModules_utils.R in script directory...\n")

  # Try to find in script directory
  script_utility <- file.path(script_dir, utility_file)
  if (file.exists(script_utility)) {
    cat("✅ Found _updateModules_utils.R in script directory\n")
    utility_file <- script_utility
  } else {
    stop("❌ _updateModules_utils.R not found. Please ensure all required files are present.")
  }
}

# Source with error handling
tryCatch({
  source(utility_file)
  cat("✅ Successfully loaded module utilities\n")
}, error = function(e) {
  stop("❌ Failed to load _updateModules_utils.R: ", e$message)
})

# Load and validate configuration
cat("\n📋 Loading configuration...\n")
config <- load_config(config_file)
config <- validate_config(config)

# Extract configuration values with backward compatibility
global <- config$global
modes <- config$modes
modules_config <- config$modules
required_packages <- config$required_packages %||% c("xfun", "fs", "jmvtools", "devtools", "purrr", "yaml", "digest")

# Handle simplified top-level configuration (new format) or nested format (old format)
new_version <- config$new_version %||% global$new_version
new_date <- config$new_date %||% global$new_date
main_repo_dir <- global$base_repo_dir %||% "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule"

# Operation modes (simplified format first, then nested format)
quick <- config$quick %||% modes$quick %||% FALSE
check <- modes$check %||% FALSE
extended <- modes$extended %||% TRUE
webpage <- modes$webpage %||% FALSE
commit_modules <- modes$commit_modules %||% FALSE
WIP <- modes$WIP %||% FALSE
TEST <- config$TEST %||% modes$TEST %||% FALSE

# File copying control modes
copy_vignettes <- modes$copy_vignettes %||% TRUE
copy_data_files <- modes$copy_data_files %||% TRUE
copy_test_files <- modes$copy_test_files %||% TRUE
copy_r_files <- modes$copy_r_files %||% TRUE

# NAMESPACE-DESCRIPTION synchronization modes
sync_namespace_description <- modes$sync_namespace_description %||% FALSE
namespace_sync_dry_run <- modes$namespace_sync_dry_run %||% FALSE

# Module-specific flags (using simplified top-level toggles)
meddecide_module <- config$meddecide %||% modes$meddecide %||% FALSE
jjstatsplot_module <- config$jjstatsplot %||% modes$jjstatsplot %||% FALSE
jsurvival_module <- config$jsurvival %||% modes$jsurvival %||% FALSE
ClinicoPathDescriptives_module <- config$ClinicoPathDescriptives %||% modes$ClinicoPathDescriptives %||% FALSE
OncoPath_module <- config$OncoPath %||% modes$OncoPath %||% FALSE

# menuGroup suffix convention - patterns MUST be anchored.
#
#   <Group>       production: shipped to the submodule
#   <Group>Extra  production overflow menu: also shipped
#   <Group>D      dev-routed:  kept out of every shipped module
#   <Group>T      test-routed: JamoviTest only (see the TEST branches below)
#
# `pattern_wip` used to be the bare, UNANCHORED group name, so in WIP mode it
# matched the D and T suffixes too and copied dev/test functions into the
# production submodules - e.g. "menuGroup: Survival" pulled in Survival (8) plus
# SurvivalD (57), SurvivalT (19) and SurvivalExtraD (1), 85 instead of 8. The
# (Extra)?$ form keeps the intended widening to the Extra menus while excluding
# the dev/test suffixes.
# Hardcoded module configurations (simplified for maintainability)
module_patterns <- list(
  meddecide = list(
    pattern = "menuGroup: meddecide$",
    pattern_wip = "menuGroup: meddecide(Extra)?[[:space:]]*$",
    data_files = c("histopathology.rda", "roc_analysis_test_data.RData", "cancer_biomarker_data.csv", 
                   "cardiac_troponin_data.csv", "sepsis_biomarker_data.csv", "thyroid_function_data.csv",
                   "bayesdca_test_data.rda", "breast_cancer_data.rda", "breast_diagnostic_styles.rda",
                   "lymphoma_diagnostic_styles.rda", "dca_test_data.csv", "thyroid_function_data.rda")
  ),
  jjstatsplot = list(
    pattern = "menuGroup: JJStatsPlot$",
    pattern_wip = "menuGroup: JJStatsPlot(Extra)?[[:space:]]*$",
    data_files = c("histopathology.rda", "groupsummary_financial_data.rda", "groupsummary_simple.rda",
                   "categorical_quality_data.rda", paste0("hullplot_", c("clinical", "customer", "experimental", "quality", "survey"), "_data.rda"),
                   paste0("jggstats_", c("clinical", "educational", "experimental", "financial", "marketing", 
                         "medical", "pharmaceutical", "psychological", "quality", "survey"), "_data.rda"))
  ),
  jsurvival = list(
    pattern = "menuGroup: Survival$",
    pattern_wip = "menuGroup: Survival(Extra)?[[:space:]]*$",
    data_files = c("histopathology.rda", "melanoma.rda", "data_longitudinal.rda",
                   paste0("stagemigration_", c("lung_cancer", "breast_cancer", "colorectal_cancer", "small_sample",
                         "large_performance", "problematic", "combined", "summary_stats"), ".rda"))
  ),
  ClinicoPathDescriptives = list(
    pattern = "menuGroup: Exploration$",
    pattern_wip = "menuGroup: Exploration(Extra)?[[:space:]]*$",
    data_files = c("histopathology.rda")
  ),
  OncoPath = list(
    pattern = "menuGroup: OncoPath$",
    pattern_wip = "menuGroup: OncoPath(Extra)?[[:space:]]*$",
    data_files = c("histopathology.rda")
  )
)

# Set enabled status for modules based on toggles and add hardcoded configurations
if (meddecide_module) {
  modules_config$meddecide$enabled <- TRUE
  modules_config$meddecide <- c(modules_config$meddecide, module_patterns$meddecide)
  cat("🔧 meddecide enabled\n")
}
if (jjstatsplot_module) {
  modules_config$jjstatsplot$enabled <- TRUE
  modules_config$jjstatsplot <- c(modules_config$jjstatsplot, module_patterns$jjstatsplot)
  cat("🔧 jjstatsplot enabled\n")
}
if (jsurvival_module) {
  modules_config$jsurvival$enabled <- TRUE
  modules_config$jsurvival <- c(modules_config$jsurvival, module_patterns$jsurvival)
  cat("🔧 jsurvival enabled\n")
}
if (ClinicoPathDescriptives_module) {
  modules_config$ClinicoPathDescriptives$enabled <- TRUE
  modules_config$ClinicoPathDescriptives <- c(modules_config$ClinicoPathDescriptives, module_patterns$ClinicoPathDescriptives)
  cat("🔧 ClinicoPathDescriptives enabled\n")
}
if (OncoPath_module) {
  modules_config$OncoPath$enabled <- TRUE
  modules_config$OncoPath <- c(modules_config$OncoPath, module_patterns$OncoPath)
  cat("🔧 OncoPath enabled\n")
}

# Apply WIP mode overrides
if (WIP) {
  quick <- FALSE
  check <- FALSE
  extended <- TRUE
  webpage <- FALSE
  commit_modules <- FALSE
  cat("🔧 WIP mode enabled - using sandbox environment\n")
}

# Apply TEST mode overrides
if (TEST) {
  quick <- FALSE
  check <- FALSE
  extended <- TRUE
  webpage <- FALSE
  commit_modules <- FALSE
  cat("🧪 TEST mode enabled - creating standalone JamoviTest module\n")
  
  # Enable JamoviTest module when TEST mode is active
  modules_config$JamoviTest$enabled <- TRUE
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
  # Check if module is enabled via top-level toggles or old enabled property
  module_enabled <- FALSE
  if (module_name == "meddecide" && meddecide_module) module_enabled <- TRUE
  if (module_name == "jjstatsplot" && jjstatsplot_module) module_enabled <- TRUE
  if (module_name == "jsurvival" && jsurvival_module) module_enabled <- TRUE
  if (module_name == "ClinicoPathDescriptives" && ClinicoPathDescriptives_module) module_enabled <- TRUE
  if (module_name == "OncoPath" && OncoPath_module) module_enabled <- TRUE
  if (module_name == "JamoviTest" && (TEST || (!is.null(modules_config[[module_name]]$enabled) && modules_config[[module_name]]$enabled))) module_enabled <- TRUE
  
  if (module_enabled) {
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

if (module_validation_failed && !WIP && !TEST) {
  stop("❌ Some module directories are invalid. Check configuration or enable WIP/TEST mode.")
}

# Handle JamoviTest module creation in TEST mode
if (TEST && modules_config$JamoviTest$enabled) {
  test_dir <- modules_config$JamoviTest$directory
  cat("\n🧪 Setting up JamoviTest module...\n")
  
  # Create JamoviTest directory if it doesn't exist
  if (!dir.exists(test_dir)) {
    cat("  📁 Creating new JamoviTest module:", test_dir, "\n")
    with_error_handling({
      jmvtools::create(path = test_dir)
    }, "creating JamoviTest module", continue_on_error = FALSE)
    cat("  ✅ JamoviTest module created successfully\n")
  } else {
    cat("  ♻️ Using existing JamoviTest module:", test_dir, "\n")
    
    # Remove NAMESPACE file for clean build
    namespace_file <- file.path(test_dir, "NAMESPACE")
    if (file.exists(namespace_file)) {
      cat("  🧹 Removing NAMESPACE file for clean build...\n")
      file.remove(namespace_file)
    }
    
    # Remove any .jmo files
    jmo_files <- list.files(test_dir, pattern = "\\.jmo$", full.names = TRUE)
    if (length(jmo_files) > 0) {
      cat("  🧹 Removing", length(jmo_files), ".jmo file(s)...\n")
      file.remove(jmo_files)
    }
    
    # Clean only the R and jamovi directories to refresh functions
    r_dir <- file.path(test_dir, "R")
    jamovi_dir <- file.path(test_dir, "jamovi")
    
    if (dir.exists(r_dir)) {
      cat("  🧹 Cleaning R directory for fresh functions...\n")
      # Remove all .b.R files but keep other R files
      r_files <- list.files(r_dir, pattern = "\\.b\\.R$", full.names = TRUE)
      if (length(r_files) > 0) {
        file.remove(r_files)
      }
    }
    
    if (dir.exists(jamovi_dir)) {
      cat("  🧹 Cleaning jamovi directory for fresh function definitions...\n")
      # Remove ALL yaml files including 0000.yaml for clean rebuild
      yaml_files <- list.files(jamovi_dir, pattern = "\\.yaml$", full.names = TRUE)
      if (length(yaml_files) > 0) {
        file.remove(yaml_files)
      }
    }
  }
  
  # Add to module_dirs
  module_dirs$JamoviTest <- test_dir
}

# Legacy variable assignments for backward compatibility (using config values)
jjstatsplot_dir <- module_dirs$jjstatsplot %||% modules_config$jjstatsplot$directory
meddecide_dir <- module_dirs$meddecide %||% modules_config$meddecide$directory
jsurvival_dir <- module_dirs$jsurvival %||% modules_config$jsurvival$directory
ClinicoPathDescriptives_dir <- module_dirs$ClinicoPathDescriptives %||% modules_config$ClinicoPathDescriptives$directory
OncoPath_dir <- module_dirs$OncoPath %||% modules_config$OncoPath$directory

# Enhanced WIP mode with backup and validation (TEST mode uses standalone JamoviTest only)
if (WIP) {
  mode_name <- "WIP (Work-In-Progress)"
  mode_suffix <- "-WIP"
  cat("\n🔧 Setting up", mode_name, "environment...\n")

  wip_setup_success <- TRUE

  for (module_name in names(module_dirs)) {
    original_dir <- module_dirs[[module_name]]
    wip_dir <- paste0(original_dir, mode_suffix)

    # Validate original directory exists
    if (!dir.exists(original_dir)) {
      warning("⚠️ Original module directory does not exist: ", original_dir)
      wip_setup_success <- FALSE
      next
    }

    cat("🔧 Setting up", mode_name, "environment for", module_name, "\n")

    # Delete existing directory if it exists
    if (dir.exists(wip_dir)) {
      cat("  🗑️ Removing existing", mode_name, "directory:", wip_dir, "\n")
      with_error_handling({
        fs::dir_delete(wip_dir)
      }, paste("removing existing", mode_name, "directory for", module_name), continue_on_error = TRUE)
    }

    # Create backup of original directory
    backup_path <- create_backup(original_dir, "wip_backups")
    if (is.null(backup_path)) {
      warning("⚠️ Failed to create backup for ", module_name, ", skipping", mode_name, "setup")
      wip_setup_success <- FALSE
      next
    }

    # Copy original to directory
    cat("  📁 Creating", mode_name, "copy:", wip_dir, "\n")
    copy_result <- with_error_handling({
      fs::dir_copy(path = original_dir, new_path = wip_dir, overwrite = TRUE)
    }, paste("creating", mode_name, "directory for", module_name), continue_on_error = TRUE)

    if (!copy_result$success) {
      wip_setup_success <- FALSE
      next
    }

    # Update module directory reference
    module_dirs[[module_name]] <- wip_dir

    cat("  ✅", mode_name, "environment ready for", module_name, "\n")
  }

  # Update legacy variables for WIP
  jjstatsplot_dir <- module_dirs$jjstatsplot %||% jjstatsplot_dir
  meddecide_dir <- module_dirs$meddecide %||% meddecide_dir
  jsurvival_dir <- module_dirs$jsurvival %||% jsurvival_dir
  ClinicoPathDescriptives_dir <- module_dirs$ClinicoPathDescriptives %||% ClinicoPathDescriptives_dir
  OncoPath_dir <- module_dirs$OncoPath %||% OncoPath_dir

  if (!wip_setup_success) {
    stop("❌", mode_name, "setup failed for one or more modules. Check warnings above.")
  }

  cat("✅", mode_name, "environment setup completed successfully\n")
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

# Enhanced copy function for JavaScript and HTML files - detects files for any function
copy_jamovi_assets <- function(module_names, source_base_dir, dest_base_dir, module_type = "unknown") {
  cat("\n🎯 Copying", module_type, "JavaScript and HTML assets...\n")

  if (length(module_names) == 0) {
    cat("  ⏭️ No", module_type, "modules to process\n")
    return(list(copied = 0, failed = 0))
  }


  copied_count <- 0
  failed_count <- 0

  # JavaScript files from jamovi/js/
  js_source_dir <- file.path(source_base_dir, "jamovi", "js")
  js_dest_dir <- file.path(dest_base_dir, "jamovi", "js")

  if (dir.exists(js_source_dir)) {
    if (!dir.exists(js_dest_dir)) {
      dir.create(js_dest_dir, recursive = TRUE)
    }

    # Enhanced detection: find ALL JavaScript files related to our modules
    copied_files <- character(0)  # Track already copied files to avoid duplicates

    for (module_name in module_names) {
      # Multiple patterns to catch various JavaScript file naming conventions:
      # 1. Direct match: moduleName.js, moduleName.events.js
      # 2. Prefix match: moduleName*.js (catches events, helpers, etc.)
      # 3. Exact event files: moduleName.events.js, moduleName.handlers.js, etc.

      js_patterns <- c(
        paste0("^", module_name, "\\.js$"),                    # moduleName.js
        paste0("^", module_name, "\\.events\\.js$"),          # moduleName.events.js
        paste0("^", module_name, "\\.handlers\\.js$"),        # moduleName.handlers.js
        paste0("^", module_name, "\\.helpers\\.js$")          # moduleName.helpers.js
      )

      for (pattern in js_patterns) {
        js_files <- list.files(js_source_dir, pattern = pattern, full.names = TRUE)

        for (js_file in js_files) {
          dest_file <- file.path(js_dest_dir, basename(js_file))

          # Skip if we already copied this file (avoid duplicates from multiple patterns)
          if (basename(js_file) %in% copied_files) {
            next
          }

          copy_result <- tryCatch({
            file.copy(js_file, dest_file, overwrite = TRUE)
            cat("  ✅ Copied JS:", basename(js_file), "for", module_name, "\n")
            copied_files <- c(copied_files, basename(js_file))
            copied_count <- copied_count + 1
            TRUE
          }, error = function(e) {
            warning("⚠️ Failed to copy JS file ", basename(js_file), ": ", e$message)
            failed_count <- failed_count + 1
            FALSE
          })
        }
      }
    }
  } else {
    cat("  ℹ️ No JavaScript source directory found:", js_source_dir, "\n")
  }

  # HTML files from jamovi/html/ (less common but included for completeness)
  html_source_dir <- file.path(source_base_dir, "jamovi", "html")
  html_dest_dir <- file.path(dest_base_dir, "jamovi", "html")

  if (dir.exists(html_source_dir)) {
    if (!dir.exists(html_dest_dir)) {
      dir.create(html_dest_dir, recursive = TRUE)
    }

    # Enhanced detection for HTML files
    copied_html_files <- character(0)  # Track already copied HTML files

    for (module_name in module_names) {
      html_patterns <- c(
        paste0("^", module_name, "\\.html$"),
        paste0("^", module_name, ".*\\.html$")
      )

      for (pattern in html_patterns) {
        html_files <- list.files(html_source_dir, pattern = pattern, full.names = TRUE)

        for (html_file in html_files) {
          dest_file <- file.path(html_dest_dir, basename(html_file))

          # Skip if we already copied this file (avoid duplicates from multiple patterns)
          if (basename(html_file) %in% copied_html_files) {
            next
          }

          copy_result <- tryCatch({
            file.copy(html_file, dest_file, overwrite = TRUE)
            cat("  ✅ Copied HTML:", basename(html_file), "for", module_name, "\n")
            copied_html_files <- c(copied_html_files, basename(html_file))
            copied_count <- copied_count + 1
            TRUE
          }, error = function(e) {
            warning("⚠️ Failed to copy HTML file ", basename(html_file), ": ", e$message)
            failed_count <- failed_count + 1
            FALSE
          })
        }
      }
    }
  } else {
    cat("  ℹ️ No HTML source directory found:", html_source_dir, "\n")
  }


  if (copied_count > 0) {
    cat("  ✅", copied_count, "asset files copied successfully\n")
  } else if (failed_count == 0) {
    cat("  ℹ️ No JavaScript/HTML assets found for", module_type, "modules\n")
  }

  return(list(copied = copied_count, failed = failed_count))
}

# Copy 00refs.yaml to module jamovi folder
# Collect every citation key referenced (refs: blocks + inline refs: [a, b]) across a
# module's jamovi/*.r.yaml, *.a.yaml and 0000.yaml. Over-inclusive by design (better to
# keep a few extra entries than to trim a genuinely-cited one).
collect_used_refs <- function(jamovi_dir) {
  files <- list.files(jamovi_dir, pattern = "(\\.r\\.yaml|\\.a\\.yaml|0000\\.yaml)$", full.names = TRUE)
  used <- character(0)
  for (f in files) {
    ln <- tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))
    in_refs <- FALSE
    for (line in ln) {
      inl <- regmatches(line, regexec("refs:\\s*\\[([^]]*)\\]", line))[[1]]
      if (length(inl) == 2) {
        used <- c(used, trimws(gsub("['\"]", "", strsplit(inl[2], ",")[[1]])))
        next
      }
      if (grepl("^\\s*refs:\\s*$", line)) { in_refs <- TRUE; next }
      if (in_refs) {
        item <- regmatches(line, regexec("^\\s*-\\s*(\\S+)\\s*$", line))[[1]]
        if (length(item) == 2) {
          used <- c(used, gsub("['\"]", "", item[2]))
        } else if (nchar(trimws(line)) > 0 && !grepl("^\\s*#", line)) {
          in_refs <- FALSE
        }
      }
    }
  }
  unique(used[nchar(used) > 0])
}

# Rewrite a copied 00refs.yaml keeping only blocks whose key is cited by the module
# (plus the always-present module self-reference). Preserves the header/footer and the
# exact text of each kept block.
trim_refs_file <- function(refs_file, used_keys) {
  ln <- readLines(refs_file, warn = FALSE)
  keep <- union(used_keys, "ClinicoPathJamoviModule")
  n <- length(ln); i <- 1; out <- character(0)
  while (i <= n) {           # copy header through the top-level `refs:` line
    out <- c(out, ln[i])
    if (grepl("^refs:\\s*$", ln[i])) { i <- i + 1; break }
    i <- i + 1
  }
  key_re <- "^    ([A-Za-z0-9._-]+):\\s*$"
  kept <- 0; dropped <- 0
  while (i <= n) {
    m <- regmatches(ln[i], regexec(key_re, ln[i]))[[1]]
    if (length(m) == 2) {
      key <- m[2]; block <- ln[i]; j <- i + 1
      while (j <= n && !grepl(key_re, ln[j]) && !grepl("^\\.\\.\\.", ln[j])) {
        block <- c(block, ln[j]); j <- j + 1
      }
      if (key %in% keep) { out <- c(out, block); kept <- kept + 1 } else dropped <- dropped + 1
      i <- j
    } else { out <- c(out, ln[i]); i <- i + 1 }
  }
  writeLines(out, refs_file)
  cat("  ✂️  Trimmed 00refs.yaml: kept", kept, "cited entries, dropped", dropped, "unused\n")
}

copy_refs_yaml <- function(dest_base_dir, module_type = "unknown") {
  cat("\n📚 Copying references file (00refs.yaml) to", module_type, "module...\n")

  source_file <- file.path(main_repo_dir, "jamovi", "00refs.yaml")
  dest_dir <- file.path(dest_base_dir, "jamovi")
  dest_file <- file.path(dest_dir, "00refs.yaml")

  if (!file.exists(source_file)) {
    warning("⚠️ Source 00refs.yaml not found: ", source_file)
    return(list(copied = 0, failed = 1))
  }

  if (!dir.exists(dest_dir)) {
    cat("  📁 Creating destination jamovi directory: ", dest_dir, "\n")
    dir.create(dest_dir, recursive = TRUE)
  }

  tryCatch({
    file.copy(source_file, dest_file, overwrite = TRUE)
    # Trim the copy to only the references this module actually cites (keeps the shared
    # main 00refs.yaml complete while each shipped submodule carries only what it uses).
    used <- tryCatch(collect_used_refs(dest_dir), error = function(e) NULL)
    if (!is.null(used) && length(used) > 0) trim_refs_file(dest_file, used)
    cat("  ✅ Copied 00refs.yaml to", module_type, "module\n")
    return(list(copied = 1, failed = 0))
  }, error = function(e) {
    warning("⚠️ Failed to copy 00refs.yaml: ", e$message)
    return(list(copied = 0, failed = 1))
  })
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



# Function to replace ClinicoPath references with module-specific names ----
replace_clinicopath_with_module <- function(base_dir, module_name) {
  if (!dir.exists(base_dir)) {
    message("Directory does not exist: ", base_dir)
    return()
  }
  # Get all R, Rmd, qmd, and md files in the directory recursively
  files <- list.files(
    path = base_dir,
    pattern = "\\.(R|Rmd|rmd|qmd|md)$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if (length(files) > 0) {
    cat("      📝 Updating", length(files), "file(s) with package references...\n")
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
    # Example data references: data(<name>, package = "ClinicoPath") -> this module
    xfun::gsub_files(
      files = files,
      pattern = 'package = "ClinicoPath"',
      replacement = paste0('package = "', module_name, '"')
    )
    xfun::gsub_files(
      files = files,
      pattern = "package = 'ClinicoPath'",
      replacement = paste0("package = '", module_name, "'")
    )
    cat("      ✅ Package references updated successfully\n")
  } else {
    cat("      ℹ️ No files found to update\n")
  }
}

# Post-process a submodule's GENERATED example docs (.h.R roxygen + man/*.Rd) ----
# jmvtools regenerates these from the copied .a.yaml on every prepare(), so the
# example `data(<name>, package = "ClinicoPath")` refs and `\donttest{}` markers
# reappear after replace_clinicopath_with_module() has already run. This normalises
# them on the *generated* files, right before install/check:
#   - rewrite the parent-package data ref to this submodule (a self-reference keeps
#     R CMD check's "unstated dependencies in examples" quiet), and
#   - convert \donttest{} -> \dontrun{} because the parent example datasets are NOT
#     shipped to submodules, so these illustrative examples must not execute under
#     R CMD check --run-donttest. Mirrors the main-repo post-process further below.
postprocess_module_examples <- function(module_dir, module_name) {
  targets <- c(
    list.files(file.path(module_dir, "R"),   pattern = "\\.h\\.R$", full.names = TRUE),
    list.files(file.path(module_dir, "man"), pattern = "\\.Rd$",    full.names = TRUE)
  )
  n <- 0L
  for (f in targets) {
    txt  <- readLines(f, warn = FALSE)
    orig <- txt
    txt <- gsub('package = "ClinicoPath"', paste0('package = "', module_name, '"'), txt, fixed = TRUE)
    txt <- gsub("package = 'ClinicoPath'", paste0("package = '", module_name, "'"), txt, fixed = TRUE)
    txt <- gsub("\\donttest{", "\\dontrun{", txt, fixed = TRUE)
    if (!identical(txt, orig)) {
      writeLines(txt, f)
      n <- n + 1L
    }
  }
  cat(sprintf("      ✅ Post-processed %d example doc(s) for %s (ClinicoPath -> %s, donttest -> dontrun)\n",
              n, module_name, module_name))
}

# Data distribution helpers (P: automatic example-data + omv distribution) ----
# Escape a string for safe use inside an .Rd \item{}{} produced by roxygen.
.rd_escape <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("%",  "\\%",  x, fixed = TRUE)
  x <- gsub("{",  "\\{",  x, fixed = TRUE)
  x <- gsub("}",  "\\}",  x, fixed = TRUE)
  x
}

# Generate a valid roxygen dataset-documentation block for one loaded object.
# Datasets in submodules were shipped WITHOUT docs -> R CMD check "undocumented data"
# NOTE; this emits a `@format`/`@keywords datasets` block + the trailing object-name
# string literal (the roxygen2 idiom that auto-adds @docType data + @name).
.gen_dataset_roxygen <- function(objname, obj, module_name, source_note = NULL) {
  hdr <- c(
    paste0("# GENERATED by _updateModules.R -- do not edit. Auto-documents the bundled"),
    paste0("# example dataset '", objname, "' so R CMD check does not flag undocumented data."),
    paste0("#' ", objname, ": example dataset for the ", module_name, " module"),
    "#'",
    paste0("#' Example dataset distributed with the ", module_name,
           " jamovi module for demonstration and testing.")
  )
  if (!is.null(source_note)) hdr <- c(hdr, "#'", paste0("#' ", source_note))
  if (is.data.frame(obj)) {
    body <- c(
      "#'",
      sprintf("#' @format A data frame with %d rows and %d variables:", nrow(obj), ncol(obj)),
      "#' \\describe{")
    for (cn in names(obj)) {
      cls <- paste(class(obj[[cn]]), collapse = "/")
      body <- c(body, sprintf("#'   \\item{%s}{%s variable}", .rd_escape(cn), .rd_escape(cls)))
    }
    body <- c(body, "#' }")
  } else {
    body <- c("#'", sprintf("#' @format An object of class %s of length %d.",
                            .rd_escape(paste(class(obj), collapse = "/")), length(obj)))
  }
  c(hdr, body,
    "#' @keywords datasets internal",
    paste0('"', objname, '"'),
    "")
}

# Ensure a submodule's .Rbuildignore excludes .omv files (jamovi assets, not R data).
.ensure_rbuildignore_omv <- function(module_dir) {
  rbi <- file.path(module_dir, ".Rbuildignore")
  want <- c("^data/.*\\.omv$", "^inst/extdata/.*\\.omv$")
  cur <- if (file.exists(rbi)) readLines(rbi, warn = FALSE) else character(0)
  add <- setdiff(want, cur)
  if (length(add) > 0) {
    writeLines(c(cur, add), rbi)
    cat("    📦 .Rbuildignore: added", length(add), "omv rule(s)\n")
  }
}

# Distribute example data to ONE submodule: (1) copy manifest .rda from main data/,
# (2) auto-generate roxygen docs for every .rda in the submodule data/, (3) copy the
# .omv referenced by the submodule's 0000.yaml `datasets:` from main inst/extdata,
# (4) ensure .Rbuildignore ignores omv. Idempotent; safe to re-run.
distribute_module_data <- function(module_name, module_cfg, module_dir, main_repo_dir,
                                   generate_docs = TRUE, distribute_omv = TRUE) {
  data_dir <- file.path(module_dir, "data")
  r_dir    <- file.path(module_dir, "R")
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  if (!dir.exists(r_dir))    dir.create(r_dir, recursive = TRUE)

  # (1) copy manifest .rda/.RData from main data/
  copied <- character(0); skipped <- character(0)
  for (df in module_cfg$data_files %||% character(0)) {
    if (!grepl("\\.(rda|RData)$", df)) next   # non-rda manifest entries handled elsewhere
    src <- file.path(main_repo_dir, "data", df)
    if (file.exists(src)) {
      fs::file_copy(src, file.path(data_dir, df), overwrite = TRUE); copied <- c(copied, df)
    } else skipped <- c(skipped, df)
  }
  if (length(skipped) > 0)
    cat("    ⚠️ not in main data/ (left as-is in submodule):", paste(skipped, collapse = ", "), "\n")

  # (2) auto-generate roxygen docs for every .rda now in the submodule data/
  n_doc <- 0L; n_pruned <- 0L
  if (generate_docs) {
    rdas <- list.files(data_dir, pattern = "\\.(rda|RData)$", full.names = TRUE)

    # Collect the object names actually present in the submodule's data/.
    present_objs <- character(0)
    for (rda in rdas) {
      pe <- new.env()
      present_objs <- c(present_objs, tryCatch(load(rda, envir = pe), error = function(err) character(0)))
    }
    present_objs <- unique(present_objs)

    # PRUNE stale dataset docs: a PURE dataset-doc file (only dataset string-literals, no
    # @export/function code, not a *-package.R) that documents ANY dataset NOT present in
    # data/ is stale -- it breaks devtools::document() ("not an exported object"). Remove it;
    # any PRESENT datasets it covered are re-generated below. Clean all-present doc files are kept.
    for (ef in list.files(r_dir, pattern = "\\.R$", full.names = TRUE)) {
      if (grepl("-package\\.R$", basename(ef))) next
      eh <- readLines(ef, warn = FALSE)
      if (any(grepl("@export", eh)) || any(grepl("<-\\s*function", eh))) next  # has code -> not a pure data doc
      docd <- gsub('"', '', grep('^"[A-Za-z0-9_.]+"[[:space:]]*$', eh, value = TRUE))
      docd <- setdiff(docd, "_PACKAGE")
      if (length(docd) == 0) next                       # not a dataset-doc file
      if (length(setdiff(docd, present_objs)) > 0) {    # documents an absent dataset -> stale
        file.remove(ef); n_pruned <- n_pruned + 1L
      }
    }

    for (rda in rdas) {
      e <- new.env()
      objs <- tryCatch(load(rda, envir = e), error = function(err) character(0))
      for (on in objs) {
        # skip if a NON-generated hand doc already documents this object
        existing <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
        hand <- FALSE
        for (ef in existing) {
          if (basename(ef) == paste0("data_", on, ".R")) next  # our own generated file
          eh <- readLines(ef, warn = FALSE)  # WHOLE file (consolidated data.R docs can be long)
          if (any(grepl(paste0("@name\\s+", on, "\\b"), eh)) ||
              any(grepl(paste0("@aliases\\b.*\\b", on, "\\b"), eh)) ||
              any(grepl(paste0('^"', on, '"\\s*$'), eh))) { hand <- TRUE; break }
        }
        if (hand) next
        doc <- .gen_dataset_roxygen(on, get(on, envir = e), module_name)
        writeLines(doc, file.path(r_dir, paste0("data_", on, ".R")))
        n_doc <- n_doc + 1L
      }
    }
  }

  # (3) copy .omv referenced in the submodule 0000.yaml `datasets:` from the canonical omv
  # source `data-raw/non-rda/` (the full 994-file example-data store; build-ignored), falling
  # back to inst/extdata/ then data/.
  n_omv <- 0L
  if (distribute_omv) {
    zero <- file.path(module_dir, "jamovi", "0000.yaml")
    if (file.exists(zero)) {
      zl <- readLines(zero, warn = FALSE)
      omv <- unique(trimws(basename(gsub(".*path:\\s*", "", grep("path:.*\\.omv", zl, value = TRUE)))))
      for (o in omv) {
        cand <- c(file.path(main_repo_dir, "data-raw", "non-rda", o),
                  file.path(main_repo_dir, "inst", "extdata", o),
                  file.path(main_repo_dir, "data", o))
        hit <- cand[file.exists(cand)]
        if (length(hit) > 0) {
          fs::file_copy(hit[1], file.path(data_dir, o), overwrite = TRUE); n_omv <- n_omv + 1L
        } else cat("    ⚠️ omv in 0000.yaml datasets not found in main (data-raw/non-rda, inst/extdata, data):", o, "\n")
      }
    }
  }

  # (4) ignore omv in the R build
  .ensure_rbuildignore_omv(module_dir)

  cat(sprintf("    📊 data: %d rda copied, %d stale doc(s) pruned, %d doc(s) generated, %d omv distributed\n",
              length(copied), n_pruned, n_doc, n_omv))
}

# omv documentation: ensure every .omv in a submodule's data/ is listed in its 0000.yaml
# `datasets:` (jamovi's dataset browser). prepare() preserves that section, so entries persist.
.omv_title_map <- c(
  agepyramid = "Age Pyramid", benford = "Benford Analysis", checkdata = "Data Quality Check",
  dataquality = "Data Quality", reportcat = "Categorical Variables Report",
  summarydata = "Continuous Variables Summary", treatmentResponse = "Treatment Response",
  tableone = "Table One", swimmerplot = "Swimmer Plot", waterfall = "Treatment Response Waterfall")

.omv_stem <- function(omv) sub("_(test|sample|example|basic|raw|longitudinal|percentage|data)([_.].*)?$", "",
                              sub("\\.omv$", "", omv))

.omv_entry <- function(omv) {
  stem <- .omv_stem(omv)
  title <- if (!is.na(.omv_title_map[stem])) unname(.omv_title_map[stem]) else tools::toTitleCase(gsub("[_-]", " ", stem))
  c(paste0("  - name: ", title), paste0("    path: ", omv),
    paste0("    description: Example dataset for the ", title, " analysis."),
    "    tags:", paste0("      - ", title))
}

# Add datasets entries for present-but-undocumented omv, SKIPPING omv owned by another module
# (documented in that module's 0000.yaml). Only appends to an EXISTING datasets: section.
document_module_omv <- function(module_dir, module_name, other_module_dirs = character(0)) {
  zero <- file.path(module_dir, "jamovi", "0000.yaml")
  data_dir <- file.path(module_dir, "data")
  if (!file.exists(zero) || !dir.exists(data_dir)) return(invisible())
  zl <- readLines(zero, warn = FALSE)
  omv_here <- basename(list.files(data_dir, pattern = "\\.omv$"))
  if (length(omv_here) == 0) return(invisible())
  path_of <- function(lines) basename(trimws(gsub(".*path:\\s*", "", grep("path:.*\\.omv", lines, value = TRUE))))
  documented <- path_of(zl)
  owned_elsewhere <- character(0)
  for (od in other_module_dirs) {
    oz <- file.path(od, "jamovi", "0000.yaml")
    if (file.exists(oz)) owned_elsewhere <- c(owned_elsewhere, path_of(readLines(oz, warn = FALSE)))
  }
  to_add <- setdiff(omv_here, unique(c(documented, owned_elsewhere)))
  if (length(to_add) == 0) return(invisible())
  ds_line <- grep("^datasets:", zl)
  if (length(ds_line) == 0) return(invisible())  # no datasets section -> don't fabricate
  ds_line <- ds_line[1]
  end <- length(zl) + 1L
  if (ds_line < length(zl)) for (i in (ds_line + 1):length(zl)) if (grepl("^[A-Za-z]", zl[i])) { end <- i; break }
  entries <- unlist(lapply(to_add, .omv_entry))
  writeLines(append(zl, entries, after = end - 1L), zero)
  cat(sprintf("    📄 documented %d omv in %s 0000.yaml datasets: %s\n",
              length(to_add), module_name, paste(to_add, collapse = ", ")))
}

# Enhanced configuration-based asset copying ----
# TEST no longer gates this loop. The per-module flags below already decide what
# runs, so gating on TEST as well made enabling a module while TEST was on a
# silent no-op: the flag said OncoPath: true and nothing was copied.
if (!WIP) {
  cat("\n📁 Copying assets with configuration-based logic...\n")

  for (module_name in names(modules_config)) {
    # Skip disabled modules - use top-level configuration flags
    module_enabled <- FALSE
    if (module_name == "meddecide" && meddecide_module) module_enabled <- TRUE
    if (module_name == "jjstatsplot" && jjstatsplot_module) module_enabled <- TRUE
    if (module_name == "jsurvival" && jsurvival_module) module_enabled <- TRUE
    if (module_name == "ClinicoPathDescriptives" && ClinicoPathDescriptives_module) module_enabled <- TRUE
    if (module_name == "OncoPath" && OncoPath_module) module_enabled <- TRUE
    # JamoviTest is built by its own dedicated pipeline further down (search
    # "JamoviTest_modules"). Excluding it here stops it being processed twice
    # now that this loop also runs while TEST is on.
    if (module_name == "JamoviTest") module_enabled <- FALSE
    
    if (!module_enabled) next

    module_cfg <- modules_config[[module_name]]
    module_dir <- module_dirs[[module_name]]

    cat("📁 Processing assets for", module_name, "\n")

    # Distribute example data: copy manifest .rda from main, auto-generate roxygen docs,
    # copy .omv referenced in the submodule 0000.yaml datasets:, and ignore omv in the build.
    if (copy_data_files) {
      cat("  📁 Distributing", module_name, "example data...\n")
      tryCatch({
        distribute_module_data(
          module_name, module_cfg, module_dir, main_repo_dir,
          generate_docs  = modes$generate_data_docs %||% TRUE,
          distribute_omv = modes$distribute_omv %||% TRUE)
      }, error = function(e) {
        warning("⚠️ Error distributing data for ", module_name, ": ", e$message)
      })
    } else {
      cat("  ⏭️ Skipping", module_name, "data files (copy_data_files: false)\n")
    }

    # Document present-but-undocumented omv in the submodule 0000.yaml datasets: (skips omv
    # owned by another production module). Keeps jamovi's dataset browser in sync with data/.
    if (copy_data_files && (modes$distribute_omv %||% TRUE)) {
      prod_mods <- c("jjstatsplot", "jsurvival", "meddecide", "ClinicoPathDescriptives", "OncoPath")
      other_dirs <- unlist(module_dirs[setdiff(prod_mods, module_name)])
      tryCatch(document_module_omv(module_dir, module_name, other_dirs),
               error = function(e) warning("⚠️ Error documenting omv for ", module_name, ": ", e$message))
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

    # Copy vignette files (folder-based copying)
    if (copy_vignettes && (config$vignette_folders$copy_settings$use_folder_based %||% TRUE)) {
      cat("  📄 Copying", module_name, "vignette files (folder-based)...\n")
      vignette_dir <- file.path(module_dir, "vignettes")
      
      # Clean existing vignettes folder for fresh copy
      if (dir.exists(vignette_dir)) {
        cat("    🧹 Cleaning existing vignettes folder...\n")
        tryCatch({
          fs::dir_delete(vignette_dir)
        }, error = function(e) {
          warning("⚠️ Error cleaning vignettes folder for ", module_name, ": ", e$message)
        })
      }
      
      # Create fresh vignettes directory
      dir.create(vignette_dir, recursive = TRUE)

      # Get folders that should be copied to this module
      target_folders <- c()
      for (folder_name in names(config$vignette_folders$folder_mapping)) {
        if (module_name %in% config$vignette_folders$folder_mapping[[folder_name]]) {
          target_folders <- c(target_folders, folder_name)
        }
      }

      # Copy files from each target folder
      for (folder_name in target_folders) {
        folder_path <- file.path(main_repo_dir, folder_name)
        if (dir.exists(folder_path)) {
          cat("    📁 Copying from", folder_name, "...\n")

          # Get all vignette files in this folder
          extensions <- config$vignette_folders$extensions %||% c(".qmd", ".Rmd", ".md")
          pattern <- paste0("\\(", paste(gsub("\\.", "\\\\.", extensions), collapse = "||"), ")$")

          vignette_files <- list.files(folder_path, pattern = pattern, recursive = FALSE)

          # Filter out excluded patterns
          exclude_patterns <- config$vignette_folders$exclude_patterns %||% c()
          for (pattern in exclude_patterns) {
            vignette_files <- vignette_files[!grepl(pattern, vignette_files)]
          }

          # Copy each file
          for (vignette_file in vignette_files) {
            source_path <- file.path(folder_path, vignette_file)
            dest_path <- file.path(vignette_dir, vignette_file)

            tryCatch({
              fs::file_copy(source_path, dest_path, overwrite = TRUE)
            }, error = function(e) {
              warning("⚠️ Error copying ", vignette_file, " from ", folder_name, ": ", e$message)
            })
          }
        } else {
          warning("⚠️ Vignette folder not found: ", folder_path)
        }
      }
      
      # Replace ClinicoPath references with module-specific names in copied vignettes
      cat("    🔄 Updating package references in vignettes...\n")
      replace_clinicopath_with_module(vignette_dir, module_name)
      
    } else if (copy_vignettes && length(module_cfg$vignette_files) > 0) {
      # Fallback to manual file copying
      cat("  📄 Copying", module_name, "vignette files (manual)...\n")
      vignette_dir <- file.path(module_dir, "vignettes")
      
      # Clean existing vignettes folder for fresh copy
      if (dir.exists(vignette_dir)) {
        cat("    🧹 Cleaning existing vignettes folder...\n")
        tryCatch({
          fs::dir_delete(vignette_dir)
        }, error = function(e) {
          warning("⚠️ Error cleaning vignettes folder for ", module_name, ": ", e$message)
        })
      }
      
      # Create fresh vignettes directory
      dir.create(vignette_dir, recursive = TRUE)

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
      
      # Replace ClinicoPath references with module-specific names in copied vignettes
      cat("    🔄 Updating package references in vignettes...\n")
      replace_clinicopath_with_module(vignette_dir, module_name)
      
    } else if (!copy_vignettes) {
      cat("  ⏭️ Skipping", module_name, "vignette files (copy_vignettes: false)\n")
    }
  }
}

# All file copying is now handled by the configuration-based system above
# This section is intentionally left empty - all file copying logic has been moved
# to the enhanced configuration-based asset copying section.
# To add files to modules, update the _updateModules_config.yaml file.

if (!WIP && !TEST) {
  # All legacy file copying sections have been removed
  # File copying is now handled by the configuration-based system above
  cat("📁 Legacy file copying sections have been removed - using configuration-based system\n")
  # All legacy hardcoded file copying has been removed
  # File copying is now handled by the configuration-based system in the main loop above
  # To modify which files are copied, edit the _updateModules_config.yaml file

}





#  Module files ----

a_yaml_files <- list.files(
  path = "./jamovi",
  pattern = "\\.a\\.yaml$",
  recursive = TRUE,
  full.names = TRUE
)

# Cross-cutting Power menuGroup support:
# Functions can use 'menuGroup: Power #<module>' (or 'PowerT #<module>' for TEST)
# to appear under the Power menu while still being distributed to their submodule.
# The YAML comment (#<module>) is invisible to jamovi but visible to readLines().

## jjstatsplot module functions ----

jjstatsplot_a_yaml_files <- purrr::keep(a_yaml_files, function(f) {
  lines <- readLines(f, warn = FALSE)
  any(grepl("menuGroup: JJStatsPlot$", lines)) ||
  any(grepl("menuGroup: Power #jjstatsplot$", lines))
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
    lines <- readLines(f, warn = FALSE)
    any(grepl("menuGroup: JJStatsPlot(Extra)?[[:space:]]*$", lines)) ||
    any(grepl("menuGroup: Power #jjstatsplot[[:space:]]*$", lines))
  })

  jjstatsplot_a_yaml_files <- gsub(pattern = "./jamovi/",
                                   replacement = "",
                                   x = jjstatsplot_a_yaml_files)
  jjstatsplot_a_yaml_files <- gsub(pattern = ".a.yaml",
                                   replacement = "",
                                   x = jjstatsplot_a_yaml_files)

  jjstatsplot_modules <- jjstatsplot_a_yaml_files
}

# NOTE: there is deliberately NO `if (TEST)` block here reassigning
# <module>_modules to the T-suffixed functions.
#
# Such a block used to exist for jjstatsplot and meddecide. It overwrote the
# submodule's own list with the *T functions, so with TEST: true the
# test-routed analyses were copied straight into the production jjstatsplot and
# meddecide repos. It was harmless only while `!TEST` gates skipped submodule
# processing in TEST mode; once those gates were removed (so test and the other
# modules could be driven independently) the overwrite became a live leak.
#
# The JamoviTest module builds its own list further down - search
# "JamoviTest_modules" - from modules_config$JamoviTest$test_patterns, which
# already covers every "<Group>T$" and "PowerT". Nothing else needs to collect
# the T functions.



## meddecide module functions ----

meddecide_a_yaml_files <- purrr::keep(a_yaml_files, function(f) {
  lines <- readLines(f, warn = FALSE)
  any(grepl("menuGroup: meddecide$", lines)) ||
  any(grepl("menuGroup: Power #meddecide$", lines))
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
    lines <- readLines(f, warn = FALSE)
    any(grepl("menuGroup: meddecide(Extra)?[[:space:]]*$", lines)) ||
    any(grepl("menuGroup: Power #meddecide[[:space:]]*$", lines))
  })

  meddecide_a_yaml_files <- gsub(pattern = "./jamovi/",
                                 replacement = "",
                                 x = meddecide_a_yaml_files)
  meddecide_a_yaml_files <- gsub(pattern = ".a.yaml",
                                 replacement = "",
                                 x = meddecide_a_yaml_files)

  meddecide_modules <- meddecide_a_yaml_files
}

# NOTE: there is deliberately NO `if (TEST)` block here reassigning
# <module>_modules to the T-suffixed functions.
#
# Such a block used to exist for jjstatsplot and meddecide. It overwrote the
# submodule's own list with the *T functions, so with TEST: true the
# test-routed analyses were copied straight into the production jjstatsplot and
# meddecide repos. It was harmless only while `!TEST` gates skipped submodule
# processing in TEST mode; once those gates were removed (so test and the other
# modules could be driven independently) the overwrite became a live leak.
#
# The JamoviTest module builds its own list further down - search
# "JamoviTest_modules" - from modules_config$JamoviTest$test_patterns, which
# already covers every "<Group>T$" and "PowerT". Nothing else needs to collect
# the T functions.




## jsurvival module functions ----

jsurvival_a_yaml_files <- purrr::keep(a_yaml_files, function(f) {
  lines <- readLines(f, warn = FALSE)
  any(grepl("menuGroup: Survival$", lines)) ||
  any(grepl("menuGroup: Power #jsurvival$", lines))
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
    lines <- readLines(f, warn = FALSE)
    any(grepl("menuGroup: Survival(Extra)?[[:space:]]*$", lines)) ||
    any(grepl("menuGroup: Power #jsurvival[[:space:]]*$", lines))
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

# Get the menuGroup pattern from config for ClinicoPathDescriptives
clinicopath_pattern <- if (WIP) {
  modules_config$ClinicoPathDescriptives$menuGroup_pattern_wip %||% "menuGroup: Exploration(Extra)?[[:space:]]*$"
} else {
  modules_config$ClinicoPathDescriptives$menuGroup_pattern %||% "menuGroup: Exploration$|menuGroup: OncoPathology$"
}

# Apply the pattern to find matching files (including Power #ClinicoPathDescriptives)
ClinicoPathDescriptives_a_yaml_files <- purrr::keep(a_yaml_files, function(f) {
  lines <- readLines(f, warn = FALSE)
  any(grepl(clinicopath_pattern, lines)) ||
  any(grepl("menuGroup: Power.*#ClinicoPathDescriptives", lines))
})

ClinicoPathDescriptives_a_yaml_files <- gsub(pattern = "./jamovi/",
                                             replacement = "",
                                             x = ClinicoPathDescriptives_a_yaml_files)
ClinicoPathDescriptives_a_yaml_files <- gsub(pattern = ".a.yaml",
                                             replacement = "",
                                             x = ClinicoPathDescriptives_a_yaml_files)
ClinicoPathDescriptives_modules <- ClinicoPathDescriptives_a_yaml_files

## OncoPath module functions ----

# Get the menuGroup pattern from config for OncoPath
oncopath_pattern <- if (WIP) {
  modules_config$OncoPath$menuGroup_pattern_wip %||% "menuGroup: OncoPath(Extra)?[[:space:]]*$"
} else {
  modules_config$OncoPath$menuGroup_pattern %||% "menuGroup: OncoPath$"
}

# Apply the pattern to find matching files (including Power #OncoPath)
OncoPath_a_yaml_files <- purrr::keep(a_yaml_files, function(f) {
  lines <- readLines(f, warn = FALSE)
  any(grepl(oncopath_pattern, lines)) ||
  any(grepl("menuGroup: Power.*#OncoPath", lines))
})

OncoPath_a_yaml_files <- gsub(pattern = "./jamovi/",
                              replacement = "",
                              x = OncoPath_a_yaml_files)
OncoPath_a_yaml_files <- gsub(pattern = ".a.yaml",
                              replacement = "",
                              x = OncoPath_a_yaml_files)
OncoPath_modules <- OncoPath_a_yaml_files

## JamoviTest module functions (TEST mode) ----
JamoviTest_modules <- c()

if (TEST) {
  cat("\n🧪 Collecting TEST functions for JamoviTest module...\n")
  
  # Collect all test functions ending with 'T' from all categories
  test_patterns <- modules_config$JamoviTest$test_patterns
  
  for (pattern in test_patterns) {
    test_files <- purrr::keep(a_yaml_files, function(f) {
      any(grepl(pattern, readLines(f, warn = FALSE)))
    })
    
    if (length(test_files) > 0) {
      # Clean file paths
      test_files_cleaned <- gsub(pattern = "./jamovi/", replacement = "", x = test_files)
      test_files_cleaned <- gsub(pattern = ".a.yaml", replacement = "", x = test_files_cleaned)
      
      JamoviTest_modules <- c(JamoviTest_modules, test_files_cleaned)
      cat("  ✅ Found", length(test_files_cleaned), "test functions matching:", pattern, "\n")
    }
  }
  
  cat("  📊 Total TEST functions collected:", length(JamoviTest_modules), "\n")
  
  if (length(JamoviTest_modules) > 0) {
    cat("  🧪 TEST functions:", paste(JamoviTest_modules, collapse = ", "), "\n")
  }
}



# Distribution coverage check (P1.6) ----
# Assert every production analysis routes to exactly one submodule. Analyses that
# route to more than one submodule are a routing bug (hard stop); analyses that
# route to none are reported (they are legitimately umbrella-only staging, e.g.
# the undocumented '…D' suffix, so this is informational unless coverage_fail_on_gap).
if (!TEST && (modes$check_distribution_coverage %||% TRUE)) {
  all_analyses <- sub("\\.a\\.yaml$", "", basename(a_yaml_files))
  module_modules <- list(
    jjstatsplot = jjstatsplot_modules,
    meddecide = meddecide_modules,
    jsurvival = jsurvival_modules,
    ClinicoPathDescriptives = ClinicoPathDescriptives_modules,
    OncoPath = OncoPath_modules
  )
  invisible(check_distribution_coverage(
    all_analyses, module_modules,
    fail_on_gap = modes$coverage_fail_on_gap %||% FALSE
  ))
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
  file.path(ClinicoPathDescriptives_dir, "DESCRIPTION"),   # ClinicoPathDescriptives repository
  file.path(OncoPath_dir, "DESCRIPTION")   # OncoPath repository
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
  file.path(ClinicoPathDescriptives_dir, "jamovi", "0000.yaml"),
  file.path(OncoPath_dir, "jamovi", "0000.yaml")
)

modules <- c(
  jjstatsplot_modules,
  meddecide_modules,
  jsurvival_modules,
  ClinicoPathDescriptives_modules,
  OncoPath_modules
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
# Skip regular module copying in TEST mode - only JamoviTest is processed
# Runs whenever any production module is enabled, independently of TEST. This
# block only ever touches the five production modules; JamoviTest is handled by
# its own pipeline below, so the two can now be driven at the same time.
if (any(c(jjstatsplot_module, meddecide_module, jsurvival_module,
          ClinicoPathDescriptives_module, OncoPath_module))) {
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
  
  # Copy JavaScript and HTML assets
  copy_jamovi_assets(
    jjstatsplot_modules,
    source_base_dir = main_repo_dir,
    dest_base_dir = jjstatsplot_dir,
    module_type = "jjstatsplot"
  )

  # Copy 00refs.yaml
  copy_refs_yaml(
    dest_base_dir = jjstatsplot_dir,
    module_type = "jjstatsplot"
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
  
  # Copy JavaScript and HTML assets
  copy_jamovi_assets(
    meddecide_modules,
    source_base_dir = main_repo_dir,
    dest_base_dir = meddecide_dir,
    module_type = "meddecide"
  )

  # Copy 00refs.yaml
  copy_refs_yaml(
    dest_base_dir = meddecide_dir,
    module_type = "meddecide"
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
  
  # Copy JavaScript and HTML assets
  copy_jamovi_assets(
    jsurvival_modules,
    source_base_dir = main_repo_dir,
    dest_base_dir = jsurvival_dir,
    module_type = "jsurvival"
  )

  # Copy 00refs.yaml
  copy_refs_yaml(
    dest_base_dir = jsurvival_dir,
    module_type = "jsurvival"
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
  
  # Copy JavaScript and HTML assets
  copy_jamovi_assets(
    ClinicoPathDescriptives_modules,
    source_base_dir = main_repo_dir,
    dest_base_dir = ClinicoPathDescriptives_dir,
    module_type = "ClinicoPathDescriptives"
  )

  # Copy 00refs.yaml
  copy_refs_yaml(
    dest_base_dir = ClinicoPathDescriptives_dir,
    module_type = "ClinicoPathDescriptives"
  )
} else {
  cat("\n⏭️ Skipping ClinicoPathDescriptives modules (disabled or no modules found)\n")
}


# OncoPath_modules
if (OncoPath_module && length(OncoPath_modules) > 0) {
  cat("\n🧬 Processing OncoPath modules...\n")

  # Copy R backend files
  copy_module_files(
    OncoPath_modules,
    source_dir = file.path(main_repo_dir, "R"),
    dest_dir = file.path(OncoPath_dir, "R"),
    file_extensions = c(".b.R")
  )

  # Ensure jamovi directory exists
  jamovi_dir <- file.path(OncoPath_dir, "jamovi")
  if (!dir.exists(jamovi_dir)) {
    dir.create(jamovi_dir, recursive = TRUE)
    cat("  📁 Created jamovi directory:", jamovi_dir, "\n")
  }

  # Copy jamovi YAML files
  copy_module_files(
    OncoPath_modules,
    source_dir = file.path(main_repo_dir, "jamovi"),
    dest_dir = jamovi_dir,
    file_extensions = c(".a.yaml", ".r.yaml", ".u.yaml")
  )

  # Copy JavaScript and HTML assets
  copy_jamovi_assets(
    OncoPath_modules,
    source_base_dir = main_repo_dir,
    dest_base_dir = OncoPath_dir,
    module_type = "OncoPath"
  )

  # Copy 00refs.yaml
  copy_refs_yaml(
    dest_base_dir = OncoPath_dir,
    module_type = "OncoPath"
  )
} else {
  cat("\n⏭️ Skipping OncoPath modules (disabled or no modules found)\n")
}


} else {
  cat("\n⏭️ No production module enabled - skipping production module processing\n")
}

# JamoviTest_modules (TEST mode only) - Process outside of the regular module block
if (TEST && modules_config$JamoviTest$enabled && length(JamoviTest_modules) > 0) {
  cat("\n🧪 Processing JamoviTest modules...\n")
  
  test_dir <- modules_config$JamoviTest$directory

  # Copy R backend files
  copy_module_files(
    JamoviTest_modules,
    source_dir = file.path(main_repo_dir, "R"),
    dest_dir = file.path(test_dir, "R"),
    file_extensions = c(".b.R")
  )

  # Ensure jamovi directory exists
  jamovi_dir <- file.path(test_dir, "jamovi")
  if (!dir.exists(jamovi_dir)) {
    cat("  📁 Creating jamovi directory: ", jamovi_dir, "\n")
    dir.create(jamovi_dir, recursive = TRUE)
  }

  # Copy jamovi definition files
  copy_module_files(
    JamoviTest_modules,
    source_dir = file.path(main_repo_dir, "jamovi"),
    dest_dir = jamovi_dir,
    file_extensions = c(".a.yaml", ".r.yaml", ".u.yaml")
  )
  
  # Copy JavaScript and HTML assets
  copy_jamovi_assets(
    JamoviTest_modules,
    source_base_dir = main_repo_dir,
    dest_base_dir = test_dir,
    module_type = "JamoviTest"
  )

  # Copy 00refs.yaml
  copy_refs_yaml(
    dest_base_dir = test_dir,
    module_type = "JamoviTest"
  )

  # Copy utils.R and other R helper files if specified in config
  if (copy_r_files && length(modules_config$JamoviTest$r_files) > 0) {
    cat("  📁 Copying helper R files...\n")
    r_dir <- file.path(test_dir, "R")
    if (!dir.exists(r_dir)) {
      dir.create(r_dir, recursive = TRUE)
    }
    
    for (r_file in modules_config$JamoviTest$r_files) {
      source_path <- file.path(main_repo_dir, "R", r_file)
      if (file.exists(source_path)) {
        fs::file_copy(source_path, file.path(r_dir, r_file), overwrite = TRUE)
        cat("    ✅ Copied:", r_file, "\n")
      } else {
        warning("⚠️ R file not found: ", source_path)
      }
    }
  }
  
  # Copy data files if specified and enabled
  if (copy_data_files && length(modules_config$JamoviTest$data_files) > 0) {
    cat("  📁 Copying data files...\n")
    data_dir <- file.path(test_dir, "data")
    if (!dir.exists(data_dir)) {
      dir.create(data_dir, recursive = TRUE)
    }
    
    for (data_file in modules_config$JamoviTest$data_files) {
      source_path <- file.path(main_repo_dir, "data", data_file)
      if (file.exists(source_path)) {
        fs::file_copy(source_path, file.path(data_dir, data_file), overwrite = TRUE)
        cat("    ✅ Copied:", data_file, "\n")
      } else {
        warning("⚠️ Data file not found: ", source_path)
      }
    }
  }
  
  cat("  ✅ JamoviTest module populated with", length(JamoviTest_modules), "test functions\n")
} else if (TEST && modules_config$JamoviTest$enabled) {
  cat("\n⚠️ No TEST functions found - JamoviTest module will be empty\n")
} else if (TEST) {
  cat("\n⏭️ Skipping JamoviTest modules (JamoviTest disabled)\n")
}





if (!WIP & webpage) {
  ## --- Replace ClinicoPath references in module code ----
  replace_clinicopath_with_module(jjstatsplot_dir, "jjstatsplot")
  replace_clinicopath_with_module(meddecide_dir, "meddecide")
  replace_clinicopath_with_module(jsurvival_dir, "jsurvival")
  replace_clinicopath_with_module(ClinicoPathDescriptives_dir, "ClinicoPathDescriptives")
  replace_clinicopath_with_module(OncoPath_dir, "OncoPath")
}

# Distribute tests to submodules (P1.4 / P1.5) ----
# Ship a self-contained dependency-guard test (the runtime twin of the dependency
# check below) plus a tests/testthat.R runner. By DEFAULT this is done only for
# submodules that have no pre-existing functional tests. The shipped guard is a
# strict dependency-policy test and will fail on undeclared package usage. The
# umbrella's per-analysis functional tests are also distributable: copy_module_tests keys
# off each submodule's function list (^test-<fn>... AND ^helper-<fn>..., so a suite's shared
# setup travels with it) and translates ClinicoPath:: -> <module>::. Distribution stays
# gated behind `copy_test_files` because the umbrella suite is not green across every
# analysis yet -- enable it once it is, so a submodule's R CMD check does not go red.
# Datasets are the other prerequisite: a copied test calling data(<x>, package = "<module>")
# needs <x> in that module's data_files manifest.
if (!TEST) {
  guard_template <- file.path(main_repo_dir, "_updateModules_test_dependency_guard.R")
  umbrella_tests <- file.path(main_repo_dir, "tests", "testthat")

  test_targets <- list()
  if (jjstatsplot_module) test_targets$jjstatsplot <- list(dir = jjstatsplot_dir, mods = jjstatsplot_modules)
  if (meddecide_module) test_targets$meddecide <- list(dir = meddecide_dir, mods = meddecide_modules)
  if (jsurvival_module) test_targets$jsurvival <- list(dir = jsurvival_dir, mods = jsurvival_modules)
  if (ClinicoPathDescriptives_module) test_targets$ClinicoPathDescriptives <- list(dir = ClinicoPathDescriptives_dir, mods = ClinicoPathDescriptives_modules)
  if (OncoPath_module) test_targets$OncoPath <- list(dir = OncoPath_dir, mods = OncoPath_modules)

  n_existing_tests <- function(module_dir) {
    td <- file.path(module_dir, "tests", "testthat")
    if (!dir.exists(td)) return(0L)
    length(setdiff(list.files(td, pattern = "^test-.*\\.R$"),
                   "test-zzz-dependency-declaration.R"))
  }

  cat("\n🧪 Distributing test infrastructure to submodules...\n")
  for (nm in names(test_targets)) {
    tt <- test_targets[[nm]]
    # The dependency-guard test is self-contained and ALWAYS-GREEN (it is the runtime
    # twin of the pkg::-vs-DESCRIPTION reconciliation check below, and skips cleanly
    # under R CMD check). Ship it to EVERY submodule -- including ones that already
    # have functional tests -- so the undeclared-dependency net (and its guarded/
    # recommended-but-undeclared WARNING pass) exists everywhere. Historically this was
    # skipped for submodules with pre-existing tests, which is exactly why meddecide
    # never got the guard and its undeclared `glmnet` (lassologistic) slipped through.
    # Only the umbrella's FULL functional suite (which can be red) stays gated behind
    # copy_test_files.
    write_dependency_guard_test(tt$dir, guard_template)
    ensure_testthat_runner(tt$dir)
    if (copy_test_files) {
      copied <- copy_module_tests(tt$mods, umbrella_tests,
                                  file.path(tt$dir, "tests", "testthat"),
                                  module_name = nm)
      cat("  🧪 ", nm, ": distributed ", length(copied),
          " functional test file(s) + dependency-guard test\n", sep = "")
    } else {
      cat("  🧪 ", nm, ": dependency-guard test shipped",
          if (n_existing_tests(tt$dir) > 0L) " (alongside pre-existing tests)" else "",
          "\n", sep = "")
    }
  }
}

# Dependency reconciliation check (P0.2) ----
# Now that each submodule's R/ has been refreshed from the umbrella, assert every
# package used via `pkg::` in the distributed code is declared in that submodule's
# DESCRIPTION. The existing NAMESPACE->DESCRIPTION sync is driven by the NAMESPACE
# file and CANNOT see `pkg::` calls, so this is the net that catches hard-crash gaps
# (e.g. cmprsk in jsurvival; vcd/lme4 in meddecide; haven in jjstatsplot; viridis
# in ClinicoPathDescriptives). Runs before the slow prepare/install so it fails fast.
if (!TEST && (modes$check_module_dependencies %||% TRUE)) {
  dep_specs <- list()
  if (jjstatsplot_module) dep_specs$jjstatsplot <- jjstatsplot_dir
  if (meddecide_module) dep_specs$meddecide <- meddecide_dir
  if (jsurvival_module) dep_specs$jsurvival <- jsurvival_dir
  if (ClinicoPathDescriptives_module) dep_specs$ClinicoPathDescriptives <- ClinicoPathDescriptives_dir
  if (OncoPath_module) dep_specs$OncoPath <- OncoPath_dir
  if (length(dep_specs) > 0) {
    check_all_modules_dependencies(dep_specs,
                                   fail_on_error = modes$deps_fail_on_error %||% TRUE)
  }
}

# --- Prepare, document, and install modules ----
if (!extended) {
  jmvtools::prepare(main_repo_dir)
  # Post-process: jmvtools generates \donttest{} from dontrun:true in YAML,

  # but R CMD check --run-donttest still runs these. Convert to \dontrun{}.
  h_files <- list.files(file.path(main_repo_dir, "R"), pattern = "\\.h\\.R$", full.names = TRUE)
  for (hf in h_files) {
    txt <- readLines(hf, warn = FALSE)
    if (any(grepl("donttest", txt))) {
      txt <- gsub("\\\\donttest\\{", "\\\\dontrun{", txt)
      writeLines(txt, hf)
    }
  }
  cat(sprintf("  Post-processed %d .h.R files (donttest -> dontrun)\n", length(h_files)))
  devtools::document(main_repo_dir)
  jmvtools::prepare(main_repo_dir)
  # Post-process again after second prepare
  for (hf in h_files) {
    txt <- readLines(hf, warn = FALSE)
    if (any(grepl("donttest", txt))) {
      txt <- gsub("\\\\donttest\\{", "\\\\dontrun{", txt)
      writeLines(txt, hf)
    }
  }
  devtools::document(main_repo_dir)
  # jmvtools::install(main_repo_dir)
}

# --- Commit changes in each repository ----
if ((commit_modules || !WIP) && !TEST) {
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
  cat("\n⏭️ Skipping all commits (WIP/TEST mode or commit disabled)\n")
}

# Final status report ----
cat("\n🎉 ====== UPDATE PROCESS COMPLETED ======\n")
cat("✅ Modules updated to version:", new_version, "\n")
cat("✅ Date updated to:", new_date, "\n")
cat("📁 Main repository:", main_repo_dir, "\n")

if (WIP) {
  cat("🔧 WIP mode was enabled - using sandbox directories\n")
}

if (TEST) {
  cat("🧪 TEST mode was enabled - using standalone JamoviTest module\n")
}

active_modules <- sum(c(jjstatsplot_module, meddecide_module, jsurvival_module, ClinicoPathDescriptives_module, OncoPath_module))
cat("📊 Active modules:", active_modules, "/5\n")

# Show active and disabled modules
if (jjstatsplot_module) {
  cat("  ✅ jjstatsplot\n")
} else {
  cat("  ⏭️ jjstatsplot (disabled)\n")
}

if (meddecide_module) {
  cat("  ✅ meddecide\n")
} else {
  cat("  ⏭️ meddecide (disabled)\n")
}

if (jsurvival_module) {
  cat("  ✅ jsurvival\n")
} else {
  cat("  ⏭️ jsurvival (disabled)\n")
}

if (ClinicoPathDescriptives_module) {
  cat("  ✅ ClinicoPathDescriptives\n")
} else {
  cat("  ⏭️ ClinicoPathDescriptives (disabled)\n")
}

if (OncoPath_module) {
  cat("  ✅ OncoPath\n")
} else {
  cat("  ⏭️ OncoPath (disabled)\n")
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
# Only processes enabled modules (respects enabled: false in config)
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
      
      # NAMESPACE-DESCRIPTION synchronization for jjstatsplot
      if (sync_namespace_description) {
        cat("  🔄 Syncing NAMESPACE with DESCRIPTION...\n")
        sync_namespace_with_description(jjstatsplot_dir, namespace_sync_dry_run)
      }
      
      jmvtools::prepare()
      devtools::document()
      postprocess_module_examples(getwd(), basename(getwd()))
      cat("  📦 Installing...\n")
      jmvtools::install()

      if (check) {
        cat("  🔍 Running R CMD check...\n")
        devtools::check()
      }
      if (webpage) {
        cat("  🌐 Building website...\n")
        # Build pkgdown with dev/agent root .md hidden, then restore. pkgdown
        # renders every root .md, has no exclude option, ignores .Rbuildignore,
        # and hard-fails on ones with pandoc-invalid YAML (e.g. TODO.md).
        (function() {
          .dev <- c("AGENTS.md", "CLAUDE.md", "GEMINI.md", "TODO.md")
          .have <- .dev[file.exists(.dev)]
          .stash <- tempfile("pkgdown-dev-"); dir.create(.stash)
          for (.f in .have) file.rename(.f, file.path(.stash, .f))
          on.exit({
            for (.f in .have) if (file.exists(file.path(.stash, .f)))
              file.rename(file.path(.stash, .f), .f)
            unlink(.stash, recursive = TRUE)
          }, add = TRUE)
          pkgdown::build_site()
        })()
      }
      cat("  ✅ jjstatsplot processing completed\n")
    }, error = function(e) {
      warning("⚠️ Error processing jjstatsplot: ", e$message)
    }, finally = {
      setwd(old_wd)
    })
  } else {
    cat("\n⏭️ Skipping jjstatsplot package (disabled)\n")
  }

  if (meddecide_module) {
    cat("\n🎩 Processing meddecide package...\n")
    old_wd <- getwd()
    tryCatch({
      setwd(meddecide_dir)
      cat("  📄 Preparing package...\n")
      jmvtools::prepare()
      cat("  📝 Documenting...\n")
      devtools::document()
      
      # NAMESPACE-DESCRIPTION synchronization for meddecide
      if (sync_namespace_description) {
        cat("  🔄 Syncing NAMESPACE with DESCRIPTION...\n")
        sync_namespace_with_description(meddecide_dir, namespace_sync_dry_run)
      }
      
      jmvtools::prepare()
      devtools::document()
      postprocess_module_examples(getwd(), basename(getwd()))
      cat("  📦 Installing...\n")
      jmvtools::install()

      if (check) {
        cat("  🔍 Running R CMD check...\n")
        devtools::check()
      }
      if (webpage) {
        cat("  🌐 Building website...\n")
        # Build pkgdown with dev/agent root .md hidden, then restore. pkgdown
        # renders every root .md, has no exclude option, ignores .Rbuildignore,
        # and hard-fails on ones with pandoc-invalid YAML (e.g. TODO.md).
        (function() {
          .dev <- c("AGENTS.md", "CLAUDE.md", "GEMINI.md", "TODO.md")
          .have <- .dev[file.exists(.dev)]
          .stash <- tempfile("pkgdown-dev-"); dir.create(.stash)
          for (.f in .have) file.rename(.f, file.path(.stash, .f))
          on.exit({
            for (.f in .have) if (file.exists(file.path(.stash, .f)))
              file.rename(file.path(.stash, .f), .f)
            unlink(.stash, recursive = TRUE)
          }, add = TRUE)
          pkgdown::build_site()
        })()
      }
      cat("  ✅ meddecide processing completed\n")
    }, error = function(e) {
      warning("⚠️ Error processing meddecide: ", e$message)
    }, finally = {
      setwd(old_wd)
    })
  } else {
    cat("\n⏭️ Skipping meddecide package (disabled)\n")
  }

  if (jsurvival_module) {
    cat("\n⚰️ Processing jsurvival package...\n")
    old_wd <- getwd()
    tryCatch({
      setwd(jsurvival_dir)
      cat("  📄 Preparing package...\n")
      jmvtools::prepare()
      cat("  📝 Documenting...\n")
      devtools::document()
      
      # NAMESPACE-DESCRIPTION synchronization for jsurvival
      if (sync_namespace_description) {
        cat("  🔄 Syncing NAMESPACE with DESCRIPTION...\n")
        sync_namespace_with_description(jsurvival_dir, namespace_sync_dry_run)
      }
      
      jmvtools::prepare()
      devtools::document()
      postprocess_module_examples(getwd(), basename(getwd()))
      cat("  📦 Installing...\n")
      jmvtools::install()

      if (check) {
        cat("  🔍 Running R CMD check...\n")
        devtools::check()
      }
      if (webpage) {
        cat("  🌐 Building website...\n")
        # Build pkgdown with dev/agent root .md hidden, then restore. pkgdown
        # renders every root .md, has no exclude option, ignores .Rbuildignore,
        # and hard-fails on ones with pandoc-invalid YAML (e.g. TODO.md).
        (function() {
          .dev <- c("AGENTS.md", "CLAUDE.md", "GEMINI.md", "TODO.md")
          .have <- .dev[file.exists(.dev)]
          .stash <- tempfile("pkgdown-dev-"); dir.create(.stash)
          for (.f in .have) file.rename(.f, file.path(.stash, .f))
          on.exit({
            for (.f in .have) if (file.exists(file.path(.stash, .f)))
              file.rename(file.path(.stash, .f), .f)
            unlink(.stash, recursive = TRUE)
          }, add = TRUE)
          pkgdown::build_site()
        })()
      }
      cat("  ✅ jsurvival processing completed\n")
    }, error = function(e) {
      warning("⚠️ Error processing jsurvival: ", e$message)
    }, finally = {
      setwd(old_wd)
    })
  } else {
    cat("\n⏭️ Skipping jsurvival package (disabled)\n")
  }

  if (ClinicoPathDescriptives_module) {
    cat("\n🔬 Processing ClinicoPathDescriptives package...\n")
    old_wd <- getwd()
    tryCatch({
      setwd(ClinicoPathDescriptives_dir)
      cat("  📄 Preparing package...\n")
      jmvtools::prepare()
      cat("  📝 Documenting...\n")
      devtools::document()
      
      # NAMESPACE-DESCRIPTION synchronization for ClinicoPathDescriptives
      if (sync_namespace_description) {
        cat("  🔄 Syncing NAMESPACE with DESCRIPTION...\n")
        sync_namespace_with_description(ClinicoPathDescriptives_dir, namespace_sync_dry_run)
      }
      
      jmvtools::prepare()
      devtools::document()
      postprocess_module_examples(getwd(), basename(getwd()))
      cat("  📦 Installing...\n")
      jmvtools::install()

      if (check) {
        cat("  🔍 Running R CMD check...\n")
        devtools::check()
      }
      if (webpage) {
        cat("  🌐 Building website...\n")
        # Build pkgdown with dev/agent root .md hidden, then restore. pkgdown
        # renders every root .md, has no exclude option, ignores .Rbuildignore,
        # and hard-fails on ones with pandoc-invalid YAML (e.g. TODO.md).
        (function() {
          .dev <- c("AGENTS.md", "CLAUDE.md", "GEMINI.md", "TODO.md")
          .have <- .dev[file.exists(.dev)]
          .stash <- tempfile("pkgdown-dev-"); dir.create(.stash)
          for (.f in .have) file.rename(.f, file.path(.stash, .f))
          on.exit({
            for (.f in .have) if (file.exists(file.path(.stash, .f)))
              file.rename(file.path(.stash, .f), .f)
            unlink(.stash, recursive = TRUE)
          }, add = TRUE)
          pkgdown::build_site()
        })()
      }
      cat("  ✅ ClinicoPathDescriptives processing completed\n")
    }, error = function(e) {
      warning("⚠️ Error processing ClinicoPathDescriptives: ", e$message)
    }, finally = {
      setwd(old_wd)
    })
  } else {
    cat("\n⏭️ Skipping ClinicoPathDescriptives package (disabled)\n")
  }

  if (OncoPath_module) {
    cat("\n🧬 Processing OncoPath package...\n")
    old_wd <- getwd()
    tryCatch({
      setwd(OncoPath_dir)
      cat("  📄 Preparing package...\n")
      jmvtools::prepare()
      cat("  📝 Documenting...\n")
      devtools::document()

      # NAMESPACE-DESCRIPTION synchronization for OncoPath
      if (sync_namespace_description) {
        cat("  🔄 Syncing NAMESPACE with DESCRIPTION...\n")
        sync_namespace_with_description(OncoPath_dir, namespace_sync_dry_run)
      }

      jmvtools::prepare()
      devtools::document()
      postprocess_module_examples(getwd(), basename(getwd()))
      cat("  📦 Installing...\n")
      jmvtools::install()
      if (check) {
        cat("  🔍 Running R CMD check...\n")
        devtools::check()
      }
      if (webpage) {
        cat("  🌐 Building website...\n")
        # Build pkgdown with dev/agent root .md hidden, then restore. pkgdown
        # renders every root .md, has no exclude option, ignores .Rbuildignore,
        # and hard-fails on ones with pandoc-invalid YAML (e.g. TODO.md).
        (function() {
          .dev <- c("AGENTS.md", "CLAUDE.md", "GEMINI.md", "TODO.md")
          .have <- .dev[file.exists(.dev)]
          .stash <- tempfile("pkgdown-dev-"); dir.create(.stash)
          for (.f in .have) file.rename(.f, file.path(.stash, .f))
          on.exit({
            for (.f in .have) if (file.exists(file.path(.stash, .f)))
              file.rename(file.path(.stash, .f), .f)
            unlink(.stash, recursive = TRUE)
          }, add = TRUE)
          pkgdown::build_site()
        })()
      }
      cat("  ✅ OncoPath processing completed\n")
    }, error = function(e) {
      warning("⚠️ Error processing OncoPath: ", e$message)
    }, finally = {
      setwd(old_wd)
    })
  } else {
    cat("\n⏭️ Skipping OncoPath package (disabled)\n")
  }

  # Process JamoviTest in TEST mode
  if (TEST && modules_config$JamoviTest$enabled && length(JamoviTest_modules) > 0) {
    cat("\n🧪 Processing JamoviTest package...\n")
    old_wd <- getwd()
    test_dir <- modules_config$JamoviTest$directory
    
    tryCatch({
      setwd(test_dir)
      cat("  📄 Preparing package...\n")
      jmvtools::prepare()
      cat("  📝 Documenting...\n")
      devtools::document()
      
      # NAMESPACE-DESCRIPTION synchronization for JamoviTest
      if (sync_namespace_description) {
        cat("  🔄 Syncing NAMESPACE with DESCRIPTION...\n")
        sync_namespace_with_description(test_dir, namespace_sync_dry_run)
      }
      
      jmvtools::prepare()
      devtools::document()
      postprocess_module_examples(getwd(), basename(getwd()))
      cat("  📦 Installing...\n")
      jmvtools::install()

      if (check) {
        cat("  🔍 Running R CMD check...\n")
        devtools::check()
      }
      cat("  ✅ JamoviTest processing completed with", length(JamoviTest_modules), "test functions\n")
    }, error = function(e) {
      warning("⚠️ Error processing JamoviTest: ", e$message)
    }, finally = {
      setwd(old_wd)
    })
  } else if (TEST) {
    cat("\n⏭️ Skipping JamoviTest package (no test functions found or disabled)\n")
  }

}

setwd(main_repo_dir)

# NAMESPACE-DESCRIPTION synchronization now occurs within each module's processing block
# This ensures updated DESCRIPTION files are used during the second jmvtools::prepare() and installation


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
