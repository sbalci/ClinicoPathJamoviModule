options(
    repos = "https://cran.microsoft.com/snapshot/2022-01-01"
)
# source("renv/activate.R")


# if (!requireNamespace("jmvtools", quietly = TRUE)) {
#     renv::install('jmvtools', repos = c('https://repo.jamovi.org', 'https://cran.r-project.org'))
# }


if (!requireNamespace("rlang", quietly = TRUE)) {
    install.packages("rlang")
}

if (!requireNamespace("magrittr", quietly = TRUE)) {
    install.packages("magrittr")
}

`%||%` <- rlang:::`%||%`

`%|%` <- function(x, y) {
    if (is.na(x)) y else x
}

`%notin%` <- Negate("%in%")
`%!in%` <- Negate("%in%")


library("magrittr")

cat( "ClinicoPath jamovi module",
     "Serdar Balci MD Pathologist",
     "https://www.serdarbalci.com/ClinicoPathJamoviModule/",
     sep = "\n"
)
