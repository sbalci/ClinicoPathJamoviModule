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
