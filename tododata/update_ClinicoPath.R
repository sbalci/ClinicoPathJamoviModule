# ClinicoPath update project for release ----
readyfunctions <- c(
    "refs",
    "^data$",
    "-package",
    # "^utils-pipe"

    # Descriptives
    "^tableone",
    "^summarydata",
    "^reportcat",
    "^alluvial",
    # "^vartree"

    # Comparisons
    "^crosstable",
    # "^pairchi2",
    # "^correlation",

    # Survival
    "^survival",
    "^multisurvival",
    "^oddsratio",
    # "^competingsurvival",

    # Agreement
    "^agreement",
    # "^icccoeff",

    # Decision
    "^decision",
    "^decisioncalculator"
    # "^roc",
    # "^tree"
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

files_data <-
    list.files(
        path = here::here("data"),
        full.names = TRUE
    )


file.copy(from = files_R,
          to = "~/ClinicoPath/R/",
          overwrite = TRUE)


file.copy(from = files_jamovi,
          to = "~/ClinicoPath/jamovi/",
          overwrite = TRUE)


file.copy(from = files_data,
          to = "~/ClinicoPath/data/",
          overwrite = TRUE)

file.copy(from = files_data,
          to = "~/histopathRprojects/ClinicoPath/inst/extdata/",
          overwrite = TRUE)



setwd("~/ClinicoPath/")
jmvtools::install()
setwd(here::here())





CommitMessage <-
    paste("Updated for release: ", Sys.time(), sep = "")

wd            <- "~/ClinicoPath/"

setorigin     <-
    "git remote set-url origin git@github.com:sbalci/ClinicoPath.git \n"

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
