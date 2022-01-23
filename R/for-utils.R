if (!requireNamespace("rlang", quietly = TRUE)) {
    install.packages("rlang")
}
`%||%` <- rlang:::`%||%`

`%|%` <- function(x, y) {
    if (is.na(x)) y else x
}

`%notin%` <- Negate("%in%")
`%!in%` <- Negate("%in%")

if (!requireNamespace("magrittr", quietly = TRUE)) {
    install.packages("magrittr")
}
library("magrittr")

cat("Serdar Balci MD Pathologist",
    "\n",
    "https://www.serdarbalci.com/",
    "\n",
    "\n"
)
