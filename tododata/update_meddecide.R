# meddecide ClinicoPath project for release ----

# Rscript ~/histopathRprojects/ClinicoPath/tododata/update_meddecide.R

options(repos = c("https://cran.microsoft.com/snapshot/2020-08-24"))

print(paste0("copying files started at: ", Sys.time()))

readyfunctions <- c(
    "refs",
    "^data$",
    "-package",
    # "^utils-pipe"

    # Agreement
    "^agreement",
    # "^icccoeff",

    # Decision
    "^decision\\.",
    "^decisioncalculator",
    "^nomogrammer"
    # "^decision2"
    # "^roc",
    # "^tree"
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
    "histopathologyMedicalDecision"
)


readydata <- paste0(readydata, collapse = "|")


files_data <-
    list.files(
        path = "~/histopathRprojects/ClinicoPath/data",
        pattern = readydata,
        full.names = TRUE
    )



file.copy(from = files_R,
          to = "~/meddecide/R/",
          overwrite = TRUE)


file.copy(from = files_jamovi,
          to = "~/meddecide/jamovi/",
          overwrite = TRUE)


file.copy(from = files_data,
          to = "~/meddecide/data/",
          overwrite = TRUE)

file.copy(from = files_data,
          to = "~/histopathRprojects/ClinicoPath/inst/extdata/",
          overwrite = TRUE)

print(paste0("copying files ended at: ", Sys.time()))


setwd("~/meddecide/")
jmvtools::prepare()
devtools::document()
jmvtools::install()
# setwd(here::here())




CommitMessage <-
    paste("WIP at: ", Sys.time(), sep = "")

wd            <- "~/meddecide/"

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




