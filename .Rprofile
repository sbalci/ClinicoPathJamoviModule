# source("renv/activate.R")

if (!requireNamespace("jmvtools", quietly = TRUE)) {
    renv::install('jmvtools', repos = c('https://repo.jamovi.org', 'https://cran.r-project.org'))
}


options(
    repos = "https://cran.microsoft.com/snapshot/2021-04-01"
        )

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
cat("Serdar Balci, MD, Pathologist",
    "\n",
    "https://www.serdarbalci.com/",
    "\n",
    "\n"
)

