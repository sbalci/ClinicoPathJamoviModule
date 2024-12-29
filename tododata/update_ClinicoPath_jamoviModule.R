# ClinicoPath_jamoviModule update project for release ----


# remotes::install_github("sbalci/ClinicoPathDescriptives")
# remotes::install_github("sbalci/jjstatsplot")
# remotes::install_github("sbalci/jsurvival")
# remotes::install_github("sbalci/meddecide")
# remotes::install_github("sbalci/jbbplot")



# Rscript ~/histopathRprojects/ClinicoPath/tododata/update_ClinicoPath_jamoviModule.R

options(repos = c("https://cran.microsoft.com/snapshot/2020-08-24"))

setwd("~/histopathRprojects/ClinicoPath/")
jmvtools::prepare()
devtools::document()
# jmvtools::install()
# setwd(here::here())

rmarkdown::render('/Users/serdarbalciold/histopathRprojects/ClinicoPath/README.Rmd',  encoding = 'UTF-8', knit_root_dir = '~/histopathRprojects/ClinicoPath')

codemetar::write_codemeta(pkg = '~/histopathRprojects/ClinicoPath/')

devtools::check(pkg = '~/histopathRprojects/ClinicoPath/')

pkgdown::build_site(pkg = '~/histopathRprojects/ClinicoPath/')


CommitMessage <-
    paste("Prepared for release at: ", Sys.time(), sep = "")

wd            <- "~/histopathRprojects/ClinicoPath/"

setorigin     <-
    "git remote set-url origin git@github.com:sbalci/ClinicoPathJamoviModule.git \n"

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
