#!/usr/bin/env Rscript
# Update modules script for ClinicoPathJamoviModule
# This script updates the ClinicoPathJamoviModule and its dependencies to a new version and date.
# It copies example files, updates DESCRIPTION and YAML files, and commits changes to the repositories.
# It also prepares, documents, and installs the modules.
# It is designed to be run in a development environment and can be set to work-in-progress (WIP) mode.
# It requires the `xfun`, `fs`, `jmvtools`, and `devtools` packages to be installed.
# # Usage:
# Rscript _updateModules.R

# Define the new version and date ----
new_version <- "0.0.3.40" # Update this to the new version you want to set
new_date <- "2024-07-05" # Update this to the new date you want to set


# Define WIP, check, extended status ----
quick <- FALSE # Set to TRUE if you want to run the script in quick mode, which skips some steps
check <- FALSE # Set to TRUE if you want to run devtools::check() on the modules
extended <- TRUE # Set to TRUE if you want to document and install submodules
ClinicoPathDescriptives_module <- TRUE # Set to TRUE if you want to update the ClinicoPathDescriptives module
jsurvival_module <- TRUE # Set to TRUE if you want to update the jsurvival module
jjstatsplot_module <- TRUE # Set to TRUE if you want to update the jjstatsplot module
meddecide_module <- TRUE # Set to TRUE if you want to update the meddecide module

webpage <- FALSE # Set to TRUE if you want to build the pkgdown website for the modules
commit_modules <- FALSE # Set to TRUE if you want to commit changes in submodule repositories
WIP <- FALSE # Set to TRUE if this is a work-in-progress update, this will prepare WIP submodules for testing. If WIP is TRUE, the script will use WIP directories for submodules.

if (WIP) {
  quick <- FALSE
  check <- FALSE
  extended <- TRUE
  ClinicoPathDescriptives_module <- TRUE
  jsurvival_module <- TRUE
  jjstatsplot_module <- TRUE
  meddecide_module <- TRUE
  webpage <- FALSE
  commit_modules <- FALSE
}



# Load required packages ----
library(xfun)
library(fs)
library(jmvtools)
library(devtools)
library(purrr)


# Define base directories (adjust these absolute paths as needed) ----
main_repo_dir <- "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule"

setwd(main_repo_dir)

if (quick) {
  devtools::install(quick = TRUE)
  stop()
}



jjstatsplot_dir <- "/Users/serdarbalci/Documents/GitHub/jjstatsplot"
meddecide_dir <- "/Users/serdarbalci/Documents/GitHub/meddecide"
jsurvival_dir <- "/Users/serdarbalci/Documents/GitHub/jsurvival"
ClinicoPathDescriptives_dir <- "/Users/serdarbalci/Documents/GitHub/ClinicoPathDescriptives"



if (WIP) {
  # Create WIP directories if they do not exist
  fs::dir_copy(path = jjstatsplot_dir,
               new_path = file.path(paste0(jjstatsplot_dir, "-WIP")),
               overwrite = TRUE)
  fs::dir_copy(path = meddecide_dir,
               new_path = file.path(paste0(meddecide_dir, "-WIP")),
               overwrite = TRUE)
  fs::dir_copy(path = jsurvival_dir,
               new_path = file.path(paste0(jsurvival_dir, "-WIP")),
               overwrite = TRUE)
  fs::dir_copy(path = ClinicoPathDescriptives_dir,
               new_path = file.path(paste0(ClinicoPathDescriptives_dir, "-WIP")),
               overwrite = TRUE)

  # Update the directories to WIP versions
  jjstatsplot_dir <- "/Users/serdarbalci/Documents/GitHub/jjstatsplot-WIP"
  meddecide_dir <- "/Users/serdarbalci/Documents/GitHub/meddecide-WIP"
  jsurvival_dir <- "/Users/serdarbalci/Documents/GitHub/jsurvival-WIP"
  ClinicoPathDescriptives_dir <- "/Users/serdarbalci/Documents/GitHub/ClinicoPathDescriptives-WIP"

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
