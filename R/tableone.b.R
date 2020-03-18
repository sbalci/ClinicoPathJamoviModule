#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric
#' @importFrom tableone CreateTableOne
#' @import dplyr
#' @import tidyselect
#' @import gt
#' @import gtsummary

tableoneClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "tableoneClass",
    inherit = tableoneBase,
    private = list(
        .run = function() {

            if (length(self$options$vars) == 0) {
                todo <- "
                <br>Welcome to ClinicoPath
                          <br><br>
                          This tool will help you form a Table One, which is almost always used in clinicopathological research manuscripts.
                          <br><br>
                          Select the 'Variables' you want to include in the table. Numeric, Ordinal, and Categorical variables are allowed.
                          <br><br>
                          This tool uses tableone package. Please cite the packages and jamovi using references below.
                          "

                html <- self$results$todo
                html$setContent(todo)
                return()
            } else {

                todo <- ""
                html <- self$results$todo
                html$setContent(todo)



            mytableone <- self$data %>%
                tableone::CreateTableOne(data = .)

            # results

            self$results$text1$setContent(mytableone)

            # gtsummary

            # myvars <- jmvcore::constructFormula(terms = self$options$vars)
            # myvars <- jmvcore::decomposeFormula(formula = myvars)
            # myvars <- unlist(myvars)
            # mytableone2 <- self$data %>%
            #     dplyr::select(myvars)
            # mytableone2 <- gtsummary::tbl_summary(mytableone2)
            # self$results$text2$setContent(mytableone2)


            }
        })
)
