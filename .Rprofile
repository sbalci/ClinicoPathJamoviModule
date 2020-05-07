# source("renv/activate.R")
options(repos = c(
    # 'https://repo.jamovi.org',
                  "https://cran.microsoft.com/snapshot/2020-05-01"
                  )
)

.libPaths(new = "~/histopathRprojects/ClinicoPathLibrary")


# source(
#     "renv/activate.R"
    # "~/histopathRprojects/ClinicoPath/renv/activate.R"
#     # here::here("renv/activate.R")
# )


# https://stackoverflow.com/questions/2096473/r-determine-if-a-script-is-running-in-windows-or-linux

# https://conjugateprior.org/2015/06/identifying-the-os-from-r/


if (.Platform$OS.type == "windows") {

    library("jmvtools")

    jmvtools::check("C://Program Files//jamovi//bin")

    .libPaths(new = "C:\\ClinicoPathLibrary")

    Sys.setenv(TZ = "Europe/Istanbul")

}


.First <- function(){
    cat("\nWelcome to ClinicoPath", date(), "\n")
}


.Last <- function(){
    cat("\nGoodbye at ", date(), "\n")
}

