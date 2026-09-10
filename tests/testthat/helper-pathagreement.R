# data("x", package = "ClinicoPath") only resolves against an INSTALLED build, so
# under devtools::load_all() every pathagreement test skipped with
# "{ClinicoPath} is not installed". Read the source tree's data/ first and fall
# back to the installed package.
pa_test_data <- function(name) {
    f <- testthat::test_path("..", "..", "data", paste0(name, ".rda"))
    e <- new.env()
    if (file.exists(f)) {
        load(f, envir = e)
    } else {
        utils::data(list = name, package = "ClinicoPath", envir = e)
    }
    get(name, envir = e)
}
