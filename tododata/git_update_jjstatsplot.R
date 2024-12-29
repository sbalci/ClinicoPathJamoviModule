devtools::check("~/jjstatsplot/")
# rhub::validate_email(email = "drserdarbalci@gmail.com")
# rhub::check()
# rhub::check_on_ubuntu(path = "~/jjstatsplot/")
# rhub::check_on_macos(path = "~/jjstatsplot/")
# rhub::check_on_windows(path = "~/jjstatsplot/")
# rhub::check_for_cran(path = "~/jjstatsplot/")

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
