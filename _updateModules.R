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
update_yaml_files <- function(paths, version, date) {
  version_pattern <- "version:.*$"
  date_pattern <- "date:.*$"
  version_replacement <- paste0("version: ", version)
  date_replacement <- paste0("date: '", date, "'")

  xfun::gsub_files(files = paths, pattern = version_pattern, replacement = version_replacement)
  xfun::gsub_files(files = paths, pattern = date_pattern, replacement = date_replacement)
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

# Main update function that performs all steps
update_modules <- function(new_version, new_date) {
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
  yaml_paths <- c(
    file.path(main_repo_dir, "jamovi", "0000.yaml"),
    file.path(jjstatsplot_dir, "jamovi", "0000.yaml"),
    file.path(meddecide_dir, "jamovi", "0000.yaml"),
    file.path(jsurvival_dir, "jamovi", "0000.yaml"),
    file.path(ClinicoPathDescriptives_dir, "jamovi", "0000.yaml")
  )
  update_yaml_files(paths = yaml_paths,
                    version = new_version,
                    date = new_date)

  # --- Copy module files ---
  ## jjstatsplot modules
  jjstatsplot_modules <- c(
    # ggstatsplot functions
    "jjhistostats",

    "jjscatterstats",
    "jjcorrmat",

    "jjbetweenstats",
    "jjwithinstats",
    "jjdotplotstats",

    "jjbarstats",
    "jjpiestats",

    # non-ggstatsplot functions
    "waffle"
  )
  copy_module_files(jjstatsplot_modules,
                    source_dir = file.path(main_repo_dir, "R"),
                    dest_dir = file.path(jjstatsplot_dir, "R"),
                    file_extensions = c(".b.R"))
  copy_module_files(jjstatsplot_modules,
                    source_dir = file.path(main_repo_dir, "jamovi"),
                    dest_dir = file.path(jjstatsplot_dir, "jamovi"),
                    file_extensions = c(".a.yaml", ".r.yaml", ".u.yaml"))

  ## meddecide modules
  meddecide_modules <- c(
    "agreement",
    "decision",
    "decisioncalculator",
    "kappasizeci",
    "kappasizefixedn",
    "kappasizepower"
  )
  copy_module_files(meddecide_modules,
                    source_dir = file.path(main_repo_dir, "R"),
                    dest_dir = file.path(meddecide_dir, "R"),
                    file_extensions = c(".b.R"))
  copy_module_files(meddecide_modules,
                    source_dir = file.path(main_repo_dir, "jamovi"),
                    dest_dir = file.path(meddecide_dir, "jamovi"),
                    file_extensions = c(".a.yaml", ".r.yaml", ".u.yaml"))

  ## jsurvival modules
  jsurvival_modules <- c(
    "singlearm",
    "survival",
    "survivalcont",
    "multisurvival",
    "oddsratio"
  )
  copy_module_files(jsurvival_modules,
                    source_dir = file.path(main_repo_dir, "R"),
                    dest_dir = file.path(jsurvival_dir, "R"),
                    file_extensions = c(".b.R"))
  copy_module_files(jsurvival_modules,
                    source_dir = file.path(main_repo_dir, "jamovi"),
                    dest_dir = file.path(jsurvival_dir, "jamovi"),
                    file_extensions = c(".a.yaml", ".r.yaml", ".u.yaml"))

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
    "waterfall",
    # Comparisons
    "crosstable"
  )
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
new_version <- "0.0.2.66"
new_date <- "2024-02-21"

# Run the update process
update_modules(new_version, new_date)


jmvtools::prepare()
devtools::document()
jmvtools::prepare()
devtools::document()
jmvtools::install()
