#!/usr/bin/env Rscript

# Define the new version and date ----
new_version <- "0.0.3.24"
new_date <- "2024-06-15"

# Define WIP, check, extended status ----
check <- FALSE
extended <- FALSE
webpage <- FALSE
WIP <- FALSE
# WIP <- FALSE  # Set to TRUE if this is a work-in-progress update


# Load required packages ----
library(xfun)
library(fs)
library(jmvtools)
library(devtools)

# Define base directories (adjust these absolute paths as needed) ----
main_repo_dir <- "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule"

setwd(main_repo_dir)

jjstatsplot_dir <- "/Users/serdarbalci/Documents/GitHub/jjstatsplot"
meddecide_dir <- "/Users/serdarbalci/Documents/GitHub/meddecide"
jsurvival_dir <- "/Users/serdarbalci/Documents/GitHub/jsurvival"
ClinicoPathDescriptives_dir <- "/Users/serdarbalci/Documents/GitHub/ClinicoPathDescriptives"



if (WIP) {
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

  xfun::gsub_files(files = paths, pattern = version_pattern, replacement = version_replacement)
  xfun::gsub_files(files = paths, pattern = date_pattern, replacement = date_replacement)
}

# Function to update YAML files with new version and date ----
update_yaml_0000_files <- function(paths, version, date) {
  version_pattern <- "version:.*$"
  date_pattern <- "date:.*$"
  version_replacement <- paste0("version: ", version)
  date_replacement <- paste0("date: '", date, "'")

  xfun::gsub_files(files = paths, pattern = version_pattern, replacement = version_replacement)
  xfun::gsub_files(files = paths, pattern = date_pattern, replacement = date_replacement)
}

update_yaml_a_files <- function(paths, version) {
    version_pattern <- "version:.*$"
    valid_version <- paste(strsplit(version, "\\.")[[1]][1:3], collapse = ".")
    version_replacement <- paste0("version: '", valid_version, "'")

  xfun::gsub_files(files = paths, pattern = version_pattern, replacement = version_replacement)

}


# Function to copy module files from source to destination directories ----
copy_module_files <- function(module_names, source_dir, dest_dir, file_extensions) {
  for (module_name in module_names) {
    for (ext in file_extensions) {
      source_path <- file.path(source_dir, paste0(module_name, ext))
      dest_path <- file.path(dest_dir, paste0(module_name, ext))
      message("Copying ", source_path, " -> ", dest_path)
      fs::file_copy(path = source_path, new_path = dest_path, overwrite = TRUE)
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

## jjstatsplot_example_files ----

jjstatsplot_example_files <- c(
  "histopathology.rda"
  # "histopathologyGraphsPlots.omv"
)

# Create directories if they do not exist
if (!dir.exists(file.path(jjstatsplot_dir, "data"))) {
  dir.create(file.path(jjstatsplot_dir, "data"), recursive = TRUE)
}


fs::file_copy(file.path(main_repo_dir, "data", jjstatsplot_example_files),
              file.path(jjstatsplot_dir, "data"),
              overwrite = TRUE)

## jjstatsplot_data_description_files ----

jjstatsplot_data_description_files <- c(
  # "data-histopathology.R"
)


# Create directories if they do not exist
if (!dir.exists(file.path(jjstatsplot_dir, "R"))) {
  dir.create(file.path(jjstatsplot_dir, "R"), recursive = TRUE)
}


fs::file_copy(file.path(main_repo_dir, "R", jjstatsplot_data_description_files),
              file.path(jjstatsplot_dir, "R"),
              overwrite = TRUE)




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

fs::file_copy(file.path(main_repo_dir, "vignettes", jjstatsplot_vignette_files),
              file.path(jjstatsplot_dir, "vignettes"),
              overwrite = TRUE)



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


fs::file_copy(file.path(main_repo_dir, "data", meddecide_example_files),
              file.path(meddecide_dir, "data"),
              overwrite = TRUE)


## meddecide_utility_data_description_files ----

meddecide_utility_data_description_files <- c(
  "psychopdaroc_utilities.R",
  "nomogrammer.R",
  "meddecide-utils.R",
  "meddecide-package.R",
  "meddecide-data.R",
  "meddecide_stats_utils.R",
  # "data-histopathology.R",
  "diagnostic_metrics.R"
)

# Create directories if they do not exist
if (!dir.exists(file.path(meddecide_dir, "R"))) {
  dir.create(file.path(meddecide_dir, "R"), recursive = TRUE)
}

fs::file_copy(file.path(main_repo_dir, "R", meddecide_utility_data_description_files),
              file.path(meddecide_dir, "R"),
              overwrite = TRUE)



## meddecide_vignettes ----


meddecide_vignette_files <- c(
  "nogoldstandard.Rmd",
  "agreement-analysis.Rmd",
  "diagnostic-tests.Rmd",
  "roc-analysis.Rmd",
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


fs::file_copy(file.path(main_repo_dir, "vignettes", meddecide_vignette_files),
              file.path(meddecide_dir, "vignettes"),
              overwrite = TRUE)



## meddecide_testdata_files ----

meddecide_data_description_files <- c(
  "test-decision.R",
  "test-roc.R"
)



# Create directories if they do not exist
if (!dir.exists(file.path(meddecide_dir, "tests/testthat"))) {
  dir.create(file.path(meddecide_dir, "tests/testthat"), recursive = TRUE)
}


fs::file_copy(file.path(main_repo_dir, "tests/testthat", meddecide_data_description_files),
              file.path(meddecide_dir, "tests/testthat"),
              overwrite = TRUE)





## jsurvival_example_files ----

jsurvival_example_files <- c(
  "histopathology.rda"
  # "histopathologySurvival.omv"
)



# Create directories if they do not exist
if (!dir.exists(file.path(jsurvival_dir, "data"))) {
  dir.create(file.path(jsurvival_dir, "data"), recursive = TRUE)
}

fs::file_copy(file.path(main_repo_dir, "data", jsurvival_example_files),
              file.path(jsurvival_dir, "data"),
              overwrite = TRUE)

## jsurvival_data_description_files ----

jsurvival_data_description_files <- c(
  # "data-histopathology.R"
)


# Create directories if they do not exist
if (!dir.exists(file.path(jsurvival_dir, "R"))) {
  dir.create(file.path(jsurvival_dir, "R"), recursive = TRUE)
}

fs::file_copy(file.path(main_repo_dir, "R", jsurvival_data_description_files),
              file.path(jsurvival_dir, "R"),
              overwrite = TRUE)




## jsurvival_vignettes ----


jsurvival_vignette_files <- c(
"jsurvival.Rmd"
)


# Create directories if they do not exist
if (!dir.exists(file.path(jsurvival_dir, "vignettes"))) {
  dir.create(file.path(jsurvival_dir, "vignettes"), recursive = TRUE)
}

fs::file_copy(file.path(main_repo_dir, "vignettes", jsurvival_vignette_files),
              file.path(jsurvival_dir, "vignettes"),
              overwrite = TRUE)








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
  dir.create(file.path(ClinicoPathDescriptives_dir, "data"), recursive = TRUE)
}


fs::file_copy(file.path(main_repo_dir, "data", ClinicoPathDescriptives_example_files),
              file.path(ClinicoPathDescriptives_dir, "data"),
              overwrite = TRUE)


## ClinicoPathDescriptives_data_description_files ----

ClinicoPathDescriptives_data_description_files <- c(
  # "data-histopathology.R",
  # "data-treatmentResponse.R",
  # "tumor_response_examples.R"
# "data-swimmer-plot-documentation.r"

"ClinicoPathDescriptives-package.R"
)


# Create directories if they do not exist
if (!dir.exists(file.path(ClinicoPathDescriptives_dir, "R"))) {
  dir.create(file.path(ClinicoPathDescriptives_dir, "R"), recursive = TRUE)
}


fs::file_copy(file.path(main_repo_dir, "R", ClinicoPathDescriptives_data_description_files),
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


ClinicoPathDescriptives_vignette_files <- c(
"clinicoPathDescriptives-introduction.Rmd",
"data-summary.Rmd",
"visualization.Rmd"
)

# Create directories if they do not exist

if (!dir.exists(file.path(ClinicoPathDescriptives_dir, "vignettes"))) {
  dir.create(file.path(ClinicoPathDescriptives_dir, "vignettes"), recursive = TRUE)
}

fs::file_copy(file.path(main_repo_dir, "vignettes", ClinicoPathDescriptives_vignette_files),
              file.path(ClinicoPathDescriptives_dir, "vignettes"),
              overwrite = TRUE)



# Function to update module files and commit changes ----

# Main update function that performs all steps ----
# update_modules <- function(new_version, new_date) {

    # --- Module files ---

    ## jjstatsplot module functions ----
    jjstatsplot_modules <- c(
        # ggstatsplot functions
        "jjhistostats",
        #
        "jjscatterstats",
        "jjcorrmat",
        #
        "jjbetweenstats",
        "jjwithinstats",
        "jjdotplotstats",
        #
        "jjbarstats",
        "jjpiestats",

        # non-ggstatsplot functions
        "waffle"
    )

    if (WIP) {

      jjstatsplot_modules <- c(
        jjstatsplot_modules,
        "jjarcdiagram",
        "jjridgestats",
        "jjstreamgraph",
        "jjtreemap",
        "jviolin",
        "lollipop",
        "parallelplot",
        "statsplot2",
        "riverplot",
        "tidyplots"
      )
    }


    ## meddecide module functions ----
    meddecide_modules <- c(
      # Decision
      "agreement",
      "decision",
      "decisioncalculator",
      "nogoldstandard",
      "decisioncompare",

      # ROC
      "psychopdaroc",

      # Decision Curve Analysis


      # Power
      "kappasizeci",
      "kappasizefixedn",
      "kappasizepower"
    )

    if (WIP) {

      meddecide_modules <- c(
        meddecide_modules,
        "bayesdca",
        "decisioncombine",
        "decisionpanel",
        "classification",
        "correlation",
        "decision2",
        "screeningcalculator",
        "cotest",
        "sequentialtests",
        "decisioncurve",
        "dendogram",
        "icccoeff",
        "modelbuilder",
        "nomogram",
        "outcomeorganizer",
        "ppv",
        "roc",
        "roc2",
        "screeningcalculator",
        "sequentialtests",
        "tree"
      )
    }



    ## jsurvival module functions ----
    jsurvival_modules <- c(
        "timeinterval",
        # "outcomeorganizer",
        "singlearm",
        "survival",
        "survivalcont",
        "multisurvival",
        "oddsratio"
    )

    if (WIP) {

      jsurvival_modules <- c(
        jsurvival_modules,
        "alluvialsurvival",
        "comparingsurvival",
        "competingsurvival",
        "lassocox",
        "powersurvival",
        "stagemigration"
      )
    }





    ## ClinicoPathDescriptives module functions ----
    ClinicoPathDescriptives_modules <- c(
        # Descriptives
        "tableone",
        "summarydata",
        "reportcat",
        "benford",
        # Plots
        "agepyramid",
        "alluvial",
        "venn",
        "vartree",

        # Patient Follow-up
        "waterfall",
        "swimmerplot",

        # Comparisons
        "crosstable"
    )

    if (WIP) {

      ClinicoPathDescriptives_modules <- c(
        ClinicoPathDescriptives_modules,
        "checkdata",
        "chisqposttest",
        "cisingle",
        "consort",
        "conttables",
        "conttablespaired",
        "groupsummary",
        "gtsummary",
        "ihcstats",
        "pairchi2",
        "retracted",
        "summarydata",
        "swimmerplot2",
        "toolssummary",
        "vtree3"
      )
    }



  # --- Update DESCRIPTION files ---
  description_paths <- c(
    file.path(main_repo_dir, "DESCRIPTION"),             # Main repository
    file.path(jjstatsplot_dir, "DESCRIPTION"),            # jjstatsplot repository
    file.path(meddecide_dir, "DESCRIPTION"),              # meddecide repository
    file.path(jsurvival_dir, "DESCRIPTION"),              # jsurvival repository
    file.path(ClinicoPathDescriptives_dir, "DESCRIPTION")   # ClinicoPathDescriptives repository
  )
  update_description_files(paths = description_paths,
                           version = new_version,
                           date = new_date)


  # --- Update YAML files ---
  yaml_0000_paths <- c(
    file.path(main_repo_dir, "jamovi", "0000.yaml"),
    file.path(jjstatsplot_dir, "jamovi", "0000.yaml"),
    file.path(meddecide_dir, "jamovi", "0000.yaml"),
    file.path(jsurvival_dir, "jamovi", "0000.yaml"),
    file.path(ClinicoPathDescriptives_dir, "jamovi", "0000.yaml")
  )

  modules <- c(jjstatsplot_modules, meddecide_modules, jsurvival_modules, ClinicoPathDescriptives_modules)

  yaml_a_paths <- c(
      file.path(main_repo_dir, "jamovi", paste0(modules, ".a.yaml")),
      file.path(jjstatsplot_dir, "jamovi", paste0(jjstatsplot_modules, ".a.yaml")),
      file.path(meddecide_dir, "jamovi", paste0(meddecide_modules, ".a.yaml")),
      file.path(jsurvival_dir, "jamovi", paste0(jsurvival_modules, ".a.yaml")),
      file.path(ClinicoPathDescriptives_dir, "jamovi", paste0(ClinicoPathDescriptives_modules, ".a.yaml"))
  )

yaml_0000_paths <- yaml_0000_paths[file.exists(yaml_0000_paths)]
yaml_a_paths <- yaml_a_paths[file.exists(yaml_a_paths)]


  # Update YAML files with new version
update_yaml_0000_files(paths = yaml_0000_paths,
                    version = new_version,
                    date = new_date)

update_yaml_a_files(paths = yaml_a_paths,
                    version = new_version)

  # --- Copy module files ---


  # jjstatsplot_modules

  copy_module_files(jjstatsplot_modules,
                    source_dir = file.path(main_repo_dir, "R"),
                    dest_dir = file.path(jjstatsplot_dir, "R"),
                    file_extensions = c(".b.R"))

  if (!dir.exists(file.path(jjstatsplot_dir, "jamovi"))) {
    dir.create(file.path(jjstatsplot_dir, "jamovi"), recursive = TRUE)
  }

    copy_module_files(jjstatsplot_modules,
                    source_dir = file.path(main_repo_dir, "jamovi"),
                    dest_dir = file.path(jjstatsplot_dir, "jamovi"),
                    file_extensions = c(".a.yaml", ".r.yaml", ".u.yaml"))

  # meddecide_modules
  copy_module_files(meddecide_modules,
                    source_dir = file.path(main_repo_dir, "R"),
                    dest_dir = file.path(meddecide_dir, "R"),
                    file_extensions = c(".b.R"))

  if (!dir.exists(file.path(meddecide_dir, "jamovi"))) {
    dir.create(file.path(meddecide_dir, "jamovi"), recursive = TRUE)
  }

  copy_module_files(meddecide_modules,
                    source_dir = file.path(main_repo_dir, "jamovi"),
                    dest_dir = file.path(meddecide_dir, "jamovi"),
                    file_extensions = c(".a.yaml", ".r.yaml", ".u.yaml"))


  # jsurvival_modules
  copy_module_files(jsurvival_modules,
                    source_dir = file.path(main_repo_dir, "R"),
                    dest_dir = file.path(jsurvival_dir, "R"),
                    file_extensions = c(".b.R"))

  if (!dir.exists(file.path(jsurvival_dir, "jamovi"))) {
    dir.create(file.path(jsurvival_dir, "jamovi"), recursive = TRUE)
  }

  copy_module_files(jsurvival_modules,
                    source_dir = file.path(main_repo_dir, "jamovi"),
                    dest_dir = file.path(jsurvival_dir, "jamovi"),
                    file_extensions = c(".a.yaml", ".r.yaml", ".u.yaml"))


  # ClinicoPathDescriptives_modules
  copy_module_files(ClinicoPathDescriptives_modules,
                    source_dir = file.path(main_repo_dir, "R"),
                    dest_dir = file.path(ClinicoPathDescriptives_dir, "R"),
                    file_extensions = c(".b.R"))

  if (!dir.exists(file.path(ClinicoPathDescriptives_dir, "jamovi"))) {
    dir.create(file.path(ClinicoPathDescriptives_dir, "jamovi"), recursive = TRUE)
  }

  copy_module_files(ClinicoPathDescriptives_modules,
                    source_dir = file.path(main_repo_dir, "jamovi"),
                    dest_dir = file.path(ClinicoPathDescriptives_dir, "jamovi"),
                    file_extensions = c(".a.yaml", ".r.yaml", ".u.yaml"))

  # --- Prepare, document, and install modules ---
  jmvtools::prepare(main_repo_dir)
  devtools::document(main_repo_dir)
  jmvtools::prepare(main_repo_dir)
  devtools::document(main_repo_dir)
  jmvtools::install(main_repo_dir)

  # --- Commit changes in each repository ---
  commit_message <- sprintf("WIP, update modules to version %s and date %s", new_version, new_date)
  commit_repo(main_repo_dir, commit_message)
  commit_repo(jjstatsplot_dir, commit_message)
  commit_repo(meddecide_dir, commit_message)
  commit_repo(jsurvival_dir, commit_message)
  commit_repo(ClinicoPathDescriptives_dir, commit_message)

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

setwd(main_repo_dir)


if (!WIP) {
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


# "This is the jjhistostats function in jjstatsplot module of jamovi. Suggest improvements to be more useful for end users. Suggest improvements for R and yaml files. Improve documentation for R and yaml files, also for DESCRIPTION and data files. Make the function more user friendly and explanatory for the end user. Add new features as necessary. When adding new features also consider original library function arguments. Improve the interface and output. Make it more visually appealing. Do not add or change color plates. Do not suggest changes for export. Give the new codes separately with explanatory texts. Then give me complete codes with recommended suggestions. Use comments for changes. Do not make breaking changes."

