# ClinicoPath update project for release ----

# Rscript ~/histopathRprojects/ClinicoPath/tododata/update_ClinicoPath.R

options(repos = c("https://cran.microsoft.com/snapshot/2020-08-24"))

print(paste0("copying files started at: ", Sys.time()))


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
    "^agepyramid",

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

files_data <-
    list.files(
        path = "~/histopathRprojects/ClinicoPath/data",
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

print(paste0("copying files ended at: ", Sys.time()))


setwd("~/ClinicoPath/")
jmvtools::prepare()
devtools::document()
jmvtools::install()
# setwd(here::here())





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


setwd("~/histopathRprojects/ClinicoPath/")
jmvtools::prepare()
devtools::document()
