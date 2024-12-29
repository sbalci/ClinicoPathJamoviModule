devtools::check("~/jamoviTemplate/")


CommitMessage <-
    paste("Prepare for release at: ", Sys.time(), sep = "")

wd            <- "~/jamoviTemplate/"

setorigin     <-
    "git remote set-url origin git@github.com:sbalci/jamoviTemplate.git \n"

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
