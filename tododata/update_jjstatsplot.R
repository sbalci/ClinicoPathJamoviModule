# jjstatsplot update project for release ----

# Rscript ~/histopathRprojects/ClinicoPath/tododata/update_jjstatsplot.R


options(repos = c("https://cran.microsoft.com/snapshot/2020-08-24"))

.libPaths(new = "~/histopathRprojects/ClinicoPathLibrary")


# if (!requireNamespace("ggstatsplot")) {
#     install.packages("ggstatsplot")
# }

print(paste0("copying files started at: ", Sys.time()))

readyfunctions <- c(
    "refs",
    "^data$",

    # JJ Functions
    "statsplot2",
    "^jjbarstats\\.",
    "^jjbetweenstats",
    "^jjcorrmat",
    "^jjdotplotstats",
    "^jjhistostats",
    "^jjpiestats",
    "^jjscatterstats",
    "^jjwithinstats\\."
    # "^jjwithinstats2\\."
    # jjbarstats2.b.R


)


readyfunctions <- paste0(readyfunctions, collapse = "|")

files_R <-
    list.files(path = "~/histopathRprojects/ClinicoPath/R",
               pattern = readyfunctions,
               full.names = TRUE)



files_hR <- grep(pattern = "*.h.R$", x = files_R)

files_R <- files_R[-files_hR]



files_jamovi <-
    list.files(
        path = "~/histopathRprojects/ClinicoPath/jamovi",
        pattern = readyfunctions,
        full.names = TRUE
    )


readydata <- c(
    "histopathologyGraphsPlots"
)


readydata <- paste0(readydata, collapse = "|")


files_data <-
    list.files(
        path = "~/histopathRprojects/ClinicoPath/data",
        pattern = readydata,
        full.names = TRUE
    )



file.copy(from = files_R,
          to = "~/jjstatsplot/R/",
          overwrite = TRUE)


file.copy(from = files_jamovi,
          to = "~/jjstatsplot/jamovi/",
          overwrite = TRUE)


file.copy(from = files_data,
          to = "~/jjstatsplot/data/",
          overwrite = TRUE)


file.copy(from = files_data,
          to = "~/histopathRprojects/ClinicoPath/inst/extdata/",
          overwrite = TRUE)

print(paste0("copying files ended at: ", Sys.time()))


setwd("~/jjstatsplot/")
jmvtools::prepare()
# devtools::document()
jmvtools::install()
# setwd(here::here())



CommitMessage <-
    paste("WIP at: ", Sys.time(), sep = "")

wd            <- "~/jjstatsplot/"

gitCommand    <-
    paste(
        "cd ",
        wd,
        " \n git add . \n git commit --message '",
        CommitMessage,
        "' \n",
        sep = ""
    )

system(
    command = paste(gitCommand, "\n") ,
    intern = TRUE,
    wait = TRUE
)



setwd("~/histopathRprojects/ClinicoPath/")
jmvtools::prepare()
devtools::document()
