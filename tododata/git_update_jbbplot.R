devtools::check("~/jbbplot/")


CommitMessage <-
    paste("Prepare for release at: ", Sys.time(), sep = "")

wd            <- "~/jbbplot/"

setorigin     <-
    "git remote set-url origin git@github.com:sbalci/jbbplot.git \n"

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


