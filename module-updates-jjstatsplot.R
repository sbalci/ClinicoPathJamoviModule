# jjstatsplot ----




jmvtools::prepare(pkg = wd)
devtools::document(pkg = wd)
jmvtools::prepare(pkg = wd)
devtools::document(pkg = wd)
# devtools::check(pkg = wd)
# jmvtools::install(pkg = wd)



## update project for release


jjstatsplot_functions <- c(
    "^jjstatsplot-data",
    "^jjstatsplot-package",
    "^jjhistostats",
    "^jjbarstats",
    "^jjpiestats",
    "^jjdotplotstats",
    "^jjcorrmat",
    "^jjscatterstats",
    "^jjbetweenstats",
    "^jjwithinstats",
    "^jjwithinstats2",
    "^jjwithinstats3",
    "^statsplot2"
)

readyfunctions <- paste0(jjstatsplot_functions, collapse = "|")

files_R <-
    list.files(path = here::here("R"),
               pattern = readyfunctions,
               full.names = TRUE)

files_jamovi <-
    list.files(
        path = here::here("jamovi"),
        pattern = paste0(readyfunctions, "|^00refs"),
        full.names = TRUE
    )

# files_data <-
#     list.files(
#         path = here::here("data"),
#         full.names = TRUE
#     )
# histopathologyGraphsPlots.omv
# histopathology.csv
# histopathology.omv
# histopathology.rda


modules_path <- here::here(fs::path_home(), "Documents", "GitHub")


file.copy(from = files_R,
          to = paste0(modules_path, "/jjstatsplot", "/R"),
          overwrite = TRUE)


file.copy(from = files_jamovi,
          to = paste0(modules_path, "/jjstatsplot", "/R"),
          overwrite = TRUE)


# file.copy(from = files_data,
#           to = "~/ClinicoPath/data/",
#           overwrite = TRUE)

# file.copy(from = files_data,
#           to = "~/histopathRprojects/ClinicoPath/inst/extdata/",
#           overwrite = TRUE)


wd <- paste0(modules_path, "/jjstatsplot")

jmvtools::prepare(pkg = wd)
devtools::document(pkg = wd)
jmvtools::prepare(pkg = wd)
devtools::document(pkg = wd)
jmvtools::install(pkg = wd)

tryCatch(
    {
        devtools::check(pkg = wd)
    },
    error = function(error_message) {
        message("Below is the error message from R:")
        message(error_message)
        return(NA)
    }
)



# gitUpdateCommitPush
CommitMessage <- paste("updated on ", Sys.time(), sep = "")

gitCommand <- paste("cd ", wd, " \n git add . \n git commit --message '", CommitMessage, "' \n git push origin master \n", sep = "")
system(command = gitCommand, intern = TRUE)


