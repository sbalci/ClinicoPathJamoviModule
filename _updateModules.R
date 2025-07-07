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

# Command line argument handling
args <- commandArgs(trailingOnly = TRUE)
config_file <- if (length(args) > 0) args[1] else "updateModules_config.yaml"

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



# Extract module directories from configuration
module_dirs <- list()
for (module_name in names(modules_config)) {
  if (modules_config[[module_name]]$enabled) {
    module_dirs[[module_name]] <- modules_config[[module_name]]$directory
  }
}

# Legacy variable assignments for backward compatibility
jjstatsplot_dir <- module_dirs$jjstatsplot %||% "/Users/serdarbalci/Documents/GitHub/jjstatsplot"
meddecide_dir <- module_dirs$meddecide %||% "/Users/serdarbalci/Documents/GitHub/meddecide"
jsurvival_dir <- module_dirs$jsurvival %||% "/Users/serdarbalci/Documents/GitHub/jsurvival"
ClinicoPathDescriptives_dir <- module_dirs$ClinicoPathDescriptives %||% "/Users/serdarbalci/Documents/GitHub/ClinicoPathDescriptives"

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


# Function to update DESCRIPTION files with new version and date ----
update_description_files <- function(paths, version, date) {
  version_pattern <- "Version:.*$"
  date_pattern <- "Date:.*$"
  version_replacement <- paste0("Version: ", version)
  date_replacement <- paste0("Date: ", date)

  xfun::gsub_files(files = paths,
                   pattern = version_pattern,
                   replacement = version_replacement)
  xfun::gsub_files(files = paths,
                   pattern = date_pattern,
                   replacement = date_replacement)
}

# Function to update YAML files with new version and date ----
update_yaml_0000_files <- function(paths, version, date) {
  version_pattern <- "version:.*$"
  date_pattern <- "date:.*$"
  version_replacement <- paste0("version: ", version)
  date_replacement <- paste0("date: '", date, "'")

  xfun::gsub_files(files = paths,
                   pattern = version_pattern,
                   replacement = version_replacement)
  xfun::gsub_files(files = paths,
                   pattern = date_pattern,
                   replacement = date_replacement)
}


update_yaml_a_files <- function(paths, version) {
  version_pattern <- "version:.*$"
  valid_version <- paste(strsplit(version, "\\.")[[1]][1:3], collapse = ".")
  version_replacement <- paste0("version: '", valid_version, "'")

  xfun::gsub_files(files = paths,
                   pattern = version_pattern,
                   replacement = version_replacement)

}


# Function to copy module files from source to destination directories ----
copy_module_files <- function(module_names,
                              source_dir,
                              dest_dir,
                              file_extensions) {
  for (module_name in module_names) {
    for (ext in file_extensions) {
      source_path <- file.path(source_dir, paste0(module_name, ext))
      dest_path <- file.path(dest_dir, paste0(module_name, ext))
      message("Copying ", source_path, " -> ", dest_path)
      fs::file_copy(path = source_path,
                    new_path = dest_path,
                    overwrite = TRUE)
    }
  }
}

# Function to commit changes in a Git repository ----
commit_repo <- function(repo_dir, commit_message) {
  old_wd <- getwd()
  setwd(repo_dir)
  system("git add -A")
  system(sprintf("git commit -m \"%s\"", commit_message))
  setwd(old_wd)
}



# Copy example files to each module directory ----

if (!WIP) {
  ## jjstatsplot_example_files ----

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

  ## jjstatsplot_data_description_files ----

  jjstatsplot_data_description_files <- c(
    # "data-histopathology.R"
  )


  # Create directories if they do not exist
  if (!dir.exists(file.path(jjstatsplot_dir, "R"))) {
    dir.create(file.path(jjstatsplot_dir, "R"), recursive = TRUE)
  }


  fs::file_copy(
    file.path(main_repo_dir, "R", jjstatsplot_data_description_files),
    file.path(jjstatsplot_dir, "R"),
    overwrite = TRUE
  )




  ## jjstatsplot_vignettes ----


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




  ## meddecide_example_files ----

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


  ## meddecide_utility_data_description_files ----

  meddecide_utility_data_description_files <- c(
    "psychopdaroc_utilities.R",
    "nomogrammer.R",
    "meddecide-utils.R",
    # "meddecide-package.R",
    # "meddecide-data.R",
    "meddecide_stats_utils.R",
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



  ## meddecide_vignettes ----


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



  ## meddecide_testdata_files ----

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





  ## jsurvival_example_files ----

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

  ## jsurvival_data_description_files ----

  jsurvival_data_description_files <- c(
    # "data-histopathology.R"
  )


  # Create directories if they do not exist
  if (!dir.exists(file.path(jsurvival_dir, "R"))) {
    dir.create(file.path(jsurvival_dir, "R"), recursive = TRUE)
  }

  fs::file_copy(
    file.path(main_repo_dir, "R", jsurvival_data_description_files),
    file.path(jsurvival_dir, "R"),
    overwrite = TRUE
  )




  ## jsurvival_vignettes ----


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








  ## ClinicoPathDescriptives_example_files ----

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



  ## ClinicoPathDescriptives_data_description_files ----

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


  fs::file_copy(
    file.path(
      main_repo_dir,
      "R",
      ClinicoPathDescriptives_data_description_files
    ),
    file.path(ClinicoPathDescriptives_dir, "R"),
    overwrite = TRUE
  )



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

# Copy module files ----


# jjstatsplot_modules

copy_module_files(
  jjstatsplot_modules,
  source_dir = file.path(main_repo_dir, "R"),
  dest_dir = file.path(jjstatsplot_dir, "R"),
  file_extensions = c(".b.R")
)

if (!dir.exists(file.path(jjstatsplot_dir, "jamovi"))) {
  dir.create(file.path(jjstatsplot_dir, "jamovi"), recursive = TRUE)
}

copy_module_files(
  jjstatsplot_modules,
  source_dir = file.path(main_repo_dir, "jamovi"),
  dest_dir = file.path(jjstatsplot_dir, "jamovi"),
  file_extensions = c(".a.yaml", ".r.yaml", ".u.yaml")
)

# meddecide_modules
copy_module_files(
  meddecide_modules,
  source_dir = file.path(main_repo_dir, "R"),
  dest_dir = file.path(meddecide_dir, "R"),
  file_extensions = c(".b.R")
)

if (!dir.exists(file.path(meddecide_dir, "jamovi"))) {
  dir.create(file.path(meddecide_dir, "jamovi"), recursive = TRUE)
}

copy_module_files(
  meddecide_modules,
  source_dir = file.path(main_repo_dir, "jamovi"),
  dest_dir = file.path(meddecide_dir, "jamovi"),
  file_extensions = c(".a.yaml", ".r.yaml", ".u.yaml")
)


# jsurvival_modules
copy_module_files(
  jsurvival_modules,
  source_dir = file.path(main_repo_dir, "R"),
  dest_dir = file.path(jsurvival_dir, "R"),
  file_extensions = c(".b.R")
)

if (!dir.exists(file.path(jsurvival_dir, "jamovi"))) {
  dir.create(file.path(jsurvival_dir, "jamovi"), recursive = TRUE)
}

copy_module_files(
  jsurvival_modules,
  source_dir = file.path(main_repo_dir, "jamovi"),
  dest_dir = file.path(jsurvival_dir, "jamovi"),
  file_extensions = c(".a.yaml", ".r.yaml", ".u.yaml")
)


# ClinicoPathDescriptives_modules
copy_module_files(
  ClinicoPathDescriptives_modules,
  source_dir = file.path(main_repo_dir, "R"),
  dest_dir = file.path(ClinicoPathDescriptives_dir, "R"),
  file_extensions = c(".b.R")
)

if (!dir.exists(file.path(ClinicoPathDescriptives_dir, "jamovi"))) {
  dir.create(file.path(ClinicoPathDescriptives_dir, "jamovi"),
             recursive = TRUE)
}

copy_module_files(
  ClinicoPathDescriptives_modules,
  source_dir = file.path(main_repo_dir, "jamovi"),
  dest_dir = file.path(ClinicoPathDescriptives_dir, "jamovi"),
  file_extensions = c(".a.yaml", ".r.yaml", ".u.yaml")
)




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
  jmvtools::install(main_repo_dir)
}

# --- Commit changes in each repository ----
commit_message <- sprintf("WIP, update modules to version %s and date %s",
                          new_version,
                          new_date)
commit_repo(main_repo_dir, commit_message)

if (commit_modules) {
  commit_repo(jjstatsplot_dir, commit_message)
  commit_repo(meddecide_dir, commit_message)
  commit_repo(jsurvival_dir, commit_message)
  commit_repo(ClinicoPathDescriptives_dir, commit_message)
}

message("Modules updated to version ", new_version, " and date ", new_date)
# }




# Run the update process ----

# update_modules(new_version, new_date)

# tryCatch({
#     update_modules(new_version, new_date)
# }, error = function(e) {
#     message("Error during module update: ", e$message)
# })


if (extended) {
  if (jjstatsplot_module) {
    setwd(jjstatsplot_dir)
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


if (!WIP & !extended) {
  # Update the main repository
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
