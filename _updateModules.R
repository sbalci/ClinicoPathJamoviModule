update_description_files <- function(paths, version, date) {
    version_pattern <- "Version:.*$"
    date_pattern <- "Date:.*$"
    version_replacement <- paste0("Version: ", version)
    date_replacement <- paste0("Date: ", date)

    xfun::gsub_files(files = paths, pattern = version_pattern, replacement = version_replacement)
    xfun::gsub_files(files = paths, pattern = date_pattern, replacement = date_replacement)
}

update_yaml_files <- function(paths, version, date) {
    version_pattern <- "version:.*$"
    date_pattern <- "date:.*$"
    version_replacement <- paste0("version: ", version)
    date_replacement <- paste0("date: '", date, "'")

    xfun::gsub_files(files = paths, pattern = version_pattern, replacement = version_replacement)
    xfun::gsub_files(files = paths, pattern = date_pattern, replacement = date_replacement)
}

copy_module_files <- function(module_names, source_dir, dest_dir, file_extensions) {
    for (module_name in module_names) {
        for (ext in file_extensions) {
            source_path <- paste0(source_dir, module_name, ext)
            dest_path <- paste0(dest_dir, module_name, ext)
            fs::file_copy(path = source_path, new_path = dest_path, overwrite = TRUE)
        }
    }
}

# Example usage
description_paths <-
    c("./DESCRIPTION",
    "./jjstatsplot/DESCRIPTION",
    "./meddecide/DESCRIPTION",
    "./jsurvival/DESCRIPTION",
    "./ClinicoPathDescriptives/DESCRIPTION")

update_description_files(description_paths, "0.0.2.17", "2023-12-26")

yaml_paths <-
    c("./jamovi/0000.yaml",
      "./jjstatsplot/jamovi/0000.yaml",
      "./meddecide/jamovi/0000.yaml",
      "./jsurvival/jamovi/0000.yaml",
      "./ClinicoPathDescriptives/jamovi/0000.yaml")

update_yaml_files(yaml_paths, "0.0.2.17", "2023-12-26")



jjstatsplot_modules <- c(
    "jjbarstats",
    "jjbetweenstats",
    "jjcorrmat",
    "jjdotplotstats",
    "jjhistostats",
    "jjpiestats",
    "jjscatterstats"
)


copy_module_files(jjstatsplot_modules, "./R/", "./jjstatsplot/R/", c(".b.R"))
copy_module_files(jjstatsplot_modules, "./jamovi/", "./jjstatsplot/jamovi/", c(".a.yaml", ".r.yaml", ".u.yaml"))



meddecide_modules <- c(
    "agreement",
    "decision",
    "decisioncalculator",
    "kappasizeci",
    "kappasizefixedn",
    "kappasizepower"
)

copy_module_files(meddecide_modules, "./R/", "./meddecide/R/", c(".b.R"))
copy_module_files(meddecide_modules, "./jamovi/", "./meddecide/jamovi/", c(".a.yaml", ".r.yaml", ".u.yaml"))


jsurvival_modules <- c(
    "multisurvival",
    "oddsratio",
    "singlearm",
    "survival",
    "survivalcont"
)

copy_module_files(jsurvival_modules, "./R/", "./jsurvival/R/", c(".b.R"))
copy_module_files(jsurvival_modules, "./jamovi/", "./jsurvival/jamovi/", c(".a.yaml", ".r.yaml", ".u.yaml"))


ClinicoPathDescriptives_modules <- c(
    "agepyramid",
    "alluvial",
    "benford",
    "crosstable",
    "reportcat",
    "summarydata",
    "tableone",
    "vartree",
    "venn"
)

copy_module_files(ClinicoPathDescriptives_modules, "./R/", "./ClinicoPathDescriptives/R/", c(".b.R"))
copy_module_files(ClinicoPathDescriptives_modules, "./jamovi/", "./ClinicoPathDescriptives/jamovi/", c(".a.yaml", ".r.yaml", ".u.yaml"))

