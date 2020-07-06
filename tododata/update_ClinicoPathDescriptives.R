# ClinicoPathDescriptives  update project for release ----

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
    "^crosstable"

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
    "histopathologyDescriptives"
)


readyfunctions <- paste0(readydata, collapse = "|")


files_data <-
    list.files(
        path = here::here("data"),
        pattern = readydata,
        full.names = TRUE
    )



file.copy(from = files_R,
          to = "~/ClinicoPathDescriptives/R/",
          overwrite = TRUE)


file.copy(from = files_jamovi,
          to = "~/ClinicoPathDescriptives/jamovi/",
          overwrite = TRUE)


file.copy(from = files_data,
          to = "~/ClinicoPathDescriptives/data/",
          overwrite = TRUE)

file.copy(from = files_data,
          to = "~/histopathRprojects/ClinicoPath/inst/extdata/",
          overwrite = TRUE)


setwd("~/ClinicoPathDescriptives/")
jmvtools::install()
setwd(here::here())



CommitMessage <-
    paste("Prepared for release at: ", Sys.time(), sep = "")

wd            <- "~/ClinicoPathDescriptives/"

setorigin     <-
    "git remote set-url origin git@github.com:sbalci/ClinicoPathDescriptives.git \n"

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
