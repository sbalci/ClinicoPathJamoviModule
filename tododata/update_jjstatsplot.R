# jjstatsplot update project for release ----

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
    "^jjwithinstats"

    # jjbarstats2.b.R


)


readyfunctions <- paste0(readyfunctions, collapse = "|")

files_R <-
    list.files(path = here::here("R"),
               pattern = readyfunctions,
               full.names = TRUE)



files_hR <- grep(pattern = "*.h.R$", x = files_R)

files_R <- files_R[-files_hR]



files_jamovi <-
    list.files(
        path = here::here("jamovi"),
        pattern = readyfunctions,
        full.names = TRUE
    )


readydata <- c(
    "histopathologyGraphsPlots"
)


readyfunctions <- paste0(readydata, collapse = "|")


files_data <-
    list.files(
        path = here::here("data"),
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

setwd("~/jjstatsplot/")
jmvtools::install()
setwd(here::here())



CommitMessage <-
    paste("Prepared for release at: ", Sys.time(), sep = "")

wd            <- "~/jjstatsplot/"

setorigin     <-
    "git remote set-url origin git@github.com:sbalci/jjstatsplot.git \n"

gitCommand    <-
    paste(
        "cd ",
        wd,
        " \n git add . \n git commit --message '",
        CommitMessage,
        "' \n",
        setorigin,
        "git push origin master \n",
        sep = ""
    )

system(
    command = paste(gitCommand, "\n") ,
    intern = TRUE,
    wait = TRUE
)
