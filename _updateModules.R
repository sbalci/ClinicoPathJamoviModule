#!/usr/bin/env Rscript
# Load required packages
library(xfun)
library(fs)
library(jmvtools)
library(devtools)

# Define base directories (adjust these absolute paths as needed)
main_repo_dir <- "/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule"
jjstatsplot_dir <- "/Users/serdarbalci/Documents/GitHub/jjstatsplot"
meddecide_dir <- "/Users/serdarbalci/Documents/GitHub/meddecide"
jsurvival_dir <- "/Users/serdarbalci/Documents/GitHub/jsurvival"
ClinicoPathDescriptives_dir <- "/Users/serdarbalci/Documents/GitHub/ClinicoPathDescriptives"

# Function to update DESCRIPTION files with new version and date
update_description_files <- function(paths, version, date) {
  version_pattern <- "Version:.*$"
  date_pattern <- "Date:.*$"
  version_replacement <- paste0("Version: ", version)
  date_replacement <- paste0("Date: ", date)

  xfun::gsub_files(files = paths, pattern = version_pattern, replacement = version_replacement)
  xfun::gsub_files(files = paths, pattern = date_pattern, replacement = date_replacement)
}

# Function to update YAML files with new version and date
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


# Function to copy module files from source to destination directories
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

# Function to commit changes in a Git repository
commit_repo <- function(repo_dir, commit_message) {
  old_wd <- getwd()
  setwd(repo_dir)
  system("git add -A")
  system(sprintf("git commit -m \"%s\"", commit_message))
  setwd(old_wd)
}

# Copy example files to each module directory

jjstatsplot_example_files <- c(
  "histopathology.rda",
  "histopathologyGraphsPlots.omv"
)

fs::file_copy(file.path(main_repo_dir, "data", jjstatsplot_example_files),
              file.path(jjstatsplot_dir, "data"),
              overwrite = TRUE)


meddecide_example_files <- c(
  "histopathology.rda",
  "histopathologyMedicalDecision.omv"
)

fs::file_copy(file.path(main_repo_dir, "data", meddecide_example_files),
              file.path(meddecide_dir, "data"),
              overwrite = TRUE)


jsurvival_example_files <- c(
  "histopathology.rda",
  "histopathologySurvival.omv"
)

fs::file_copy(file.path(main_repo_dir, "data", jsurvival_example_files),
              file.path(jsurvival_dir, "data"),
              overwrite = TRUE)


ClinicoPathDescriptives_example_files <- c(
  "histopathology.rda",
  "histopathologyDescriptives.omv",
  "swimmer_data_raw.omv",
  "swimmer_data_raw.csv",
  "swimmer_data_raw.rda",
  "swimmer_data_date_formats.omv",
  "swimmer_data_date_formats.csv",
  "swimmer_data_date_formats.rda",
  "swimmer_data.csv",
  "swimmer_data.rda"
)

fs::file_copy(file.path(main_repo_dir, "data", ClinicoPathDescriptives_example_files),
              file.path(ClinicoPathDescriptives_dir, "data"),
              overwrite = TRUE)

# ihc_test_data.csv
# tumor_response_examples.rda
# percent_no_time.rda
# percent_with_time.rda
# raw_with_time.rda
# percent_no_time.csv
# percent_with_time.csv
# raw_with_time.csv
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



# Main update function that performs all steps
update_modules <- function(new_version, new_date) {

    # --- Module files ---

    ## jjstatsplot modules
    jjstatsplot_modules <- c(
        # ggstatsplot functions
        # "jjhistostats",
        #
        # "jjscatterstats",
        # "jjcorrmat",
        #
        # "jjbetweenstats",
        # "jjwithinstats",
        # "jjdotplotstats",
        #
        # "jjbarstats",
        # "jjpiestats",

        # non-ggstatsplot functions
        "waffle"
    )

    ## meddecide modules
    meddecide_modules <- c(
        # "agreement",
        # "decision",
        # "decisioncalculator",
        # "kappasizeci",
        # "kappasizefixedn",
        "kappasizepower"
    )

    ## jsurvival modules
    jsurvival_modules <- c(
        # "singlearm",
        # "survival",
        # "survivalcont",
        # "multisurvival",
        "oddsratio"
    )

    ## ClinicoPathDescriptives modules
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
        # "vartree2",
        # "vartree3",
        #
        # Patient Follow-up
        # "waterfall"
        # "swimmerplot"
        # Comparisons
        "crosstable"
    )


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
  copy_module_files(jjstatsplot_modules,
                    source_dir = file.path(main_repo_dir, "jamovi"),
                    dest_dir = file.path(jjstatsplot_dir, "jamovi"),
                    file_extensions = c(".a.yaml", ".r.yaml", ".u.yaml"))

  # meddecide_modules
  copy_module_files(meddecide_modules,
                    source_dir = file.path(main_repo_dir, "R"),
                    dest_dir = file.path(meddecide_dir, "R"),
                    file_extensions = c(".b.R"))
  copy_module_files(meddecide_modules,
                    source_dir = file.path(main_repo_dir, "jamovi"),
                    dest_dir = file.path(meddecide_dir, "jamovi"),
                    file_extensions = c(".a.yaml", ".r.yaml", ".u.yaml"))


  # jsurvival_modules
  copy_module_files(jsurvival_modules,
                    source_dir = file.path(main_repo_dir, "R"),
                    dest_dir = file.path(jsurvival_dir, "R"),
                    file_extensions = c(".b.R"))
  copy_module_files(jsurvival_modules,
                    source_dir = file.path(main_repo_dir, "jamovi"),
                    dest_dir = file.path(jsurvival_dir, "jamovi"),
                    file_extensions = c(".a.yaml", ".r.yaml", ".u.yaml"))


  # ClinicoPathDescriptives_modules
  copy_module_files(ClinicoPathDescriptives_modules,
                    source_dir = file.path(main_repo_dir, "R"),
                    dest_dir = file.path(ClinicoPathDescriptives_dir, "R"),
                    file_extensions = c(".b.R"))
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
  commit_message <- sprintf("Update modules to version %s and date %s", new_version, new_date)
  commit_repo(main_repo_dir, commit_message)
  commit_repo(jjstatsplot_dir, commit_message)
  commit_repo(meddecide_dir, commit_message)
  commit_repo(jsurvival_dir, commit_message)
  commit_repo(ClinicoPathDescriptives_dir, commit_message)

  message("Modules updated to version ", new_version, " and date ", new_date)
}

# Define the new version and date
new_version <- "0.0.2.72"
new_date <- "2024-02-23"

# Run the update process
update_modules(new_version, new_date)


# jmvtools::prepare()
# devtools::document()
# jmvtools::prepare()
# devtools::document()
# jmvtools::install()


"This is the waterfall function in ClinicoPathDescriptives module of jamovi. The module is aimed to be used by medical professionals, pathologists, and oncologists. Suggest improvements to be more useful for these users and in clinical settings.
Suggest improvements for R and yaml files. Improve documentation for R and yaml files, also for DESCRIPTION and data files. Make the function more user friendly and explanatory for the end user.
Add new features as necessary. When adding new features also consider original library function arguments.
Improve the interface and output. Make it more visually appealing. Do not add or change color plates. Do not suggest changes for export.
Give me complete codes with recommended suggestions. Use comments for changes. Do not make breaking changes."
