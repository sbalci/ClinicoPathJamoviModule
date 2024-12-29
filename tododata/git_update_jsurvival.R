devtools::check("~/jsurvival/")


CommitMessage <-
    paste("Prepared for jamovi library 0.0.2.0041 at: ", Sys.time(), sep = "")

# CommitMessage <-
#     paste("Prepared for release at: ", Sys.time(), sep = "")

wd            <- "~/jsurvival/"

setorigin     <-
    "git remote set-url origin git@github.com:sbalci/jsurvival.git \n"

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
