#' @title Display normality test result as a message.
#' @name normality_message
#' @description A note to the user about the validity of assumptions for the
#'   default linear model.
#'
#' @param x A numeric vector.
#' @param lab A character describing label for the variable. If `NULL`, a
#'   generic `"x"` label will be used.
#' @param ... Additional arguments (ignored).
#'
#'
#' @importFrom stats shapiro.test
#' @importFrom crayon green blue yellow red
#'
#'
#' @examples
#' # message
#' normality_message(
#'   x = anscombe$x1,
#'   lab = "x1",
#'   k = 3
#' )
#' @export

# @inheritParams specify_decimal_p
# @inherit stats::shapiro.test return value


# from https://github.com/sbalci/ClinicoPathJamoviModule/issues/3
# deleted https://github.com/cran/ipmisc/blob/master/R/helpers_messages.R
#
#
#
# function body
normality_message <- function(x,
                              lab = NULL,
                              k = 2,
                              ...) {

    # if label is not provided
    if (is.null(lab)) lab <- "x"

    # works only if sample size is greater than 3 and less than 5000
    if (length(x) > 3 && length(x) < 5000) {
        # test object
        sw_norm <- stats::shapiro.test(x)


        # exact message
        message(cat(
            crayon::green("Note: "),
            crayon::blue("Shapiro-Wilk Normality Test for "),
            crayon::yellow(lab),
            crayon::blue(": p-value = "),
            crayon::yellow(specify_decimal_p(x = sw_norm$p.value[[1]], k = k, p.value = TRUE)),
            "\n",
            sep = ""
        ))
    }
}

#' @title Display homogeneity of variance test as a message
#' @name bartlett_message
#' @description A note to the user about the validity of assumptions for the
#'   default linear model.
#'
#' @param lab A character describing label for the variable. If `NULL`, variable
#'   name will be used.
#' @inheritParams normality_message
#'
#' @importFrom rlang as_name ensym := new_formula
#' @importFrom stats bartlett.test na.omit
#' @importFrom crayon green blue yellow red
#'
#'
#' @examples
#'
#' # getting message
#' bartlett_message(
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length,
#'   lab = "Iris Species"
#' )
#' @export


# @inherit stats::bartlett.test return value
# @inheritParams long_to_wide_converter


# function body
bartlett_message <- function(data,
                             x,
                             y,
                             lab = NULL,
                             k = 2,
                             ...) {
    # make sure both quoted and unquoted arguments are allowed
    c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

    # if `lab` is not provided, use the variable `x` name
    if (is.null(lab)) lab <- rlang::as_name(x)

    # running the test
    bartlett <-
        stats::bartlett.test(
            formula = rlang::new_formula(y, x),
            data = data,
            na.action = na.omit
        )

    # display homogeneity of variances test result as a message
    message(cat(
        crayon::green("Note: "),
        crayon::blue("Bartlett's test for homogeneity of variances for factor "),
        crayon::yellow(lab),
        crayon::blue(": p-value = "),
        crayon::yellow(specify_decimal_p(x = bartlett$p.value[[1]], k = k, p.value = TRUE)),
        "\n",
        sep = ""
    ))
}
