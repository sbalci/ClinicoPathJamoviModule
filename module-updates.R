tryCatch(
    {
jmvtools::prepare()
devtools::document()
jmvtools::prepare()
devtools::document()
# devtools::check()
# pkgdown::build_site()
jmvtools::install()
},

    error = function(error_message) {
        message("Below is the error message from R:")
        message(error_message)
        return(NA)
    }
)
