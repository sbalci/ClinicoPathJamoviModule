#' Cross Table
#'
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'

crosstableClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "crosstableClass",
    inherit = crosstableBase,
    private = list(

        .run = function() {

            # Error Message ----

            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")


            if (is.null(self$options$vars)) {

                # ToDo Message ----

                todo <- "
                <br>Welcome to ClinicoPath
                          <br><br>
                          This tool will help you form a Cross Table
                          Please cite the packages and jamovi using references below.
                          "

                html <- self$results$todo
                html$setContent(todo)

            } else {

                todo <- ""
                html <- self$results$todo
                html$setContent(todo)


                # Prepare Data ----

            mydata <- self$data

            # formulaR <- jmvcore::constructFormula(terms = self$options$vars)
            # formulaL <- jmvcore::constructFormula(terms = self$options$group)

            formula <- jmvcore::constructFormula(terms = self$options$vars,
                                                 dep = self$options$group)


            # formula <- paste(formulaL, '~', formulaR)
            formula2 <- as.formula(formula)



            # Exclude NA

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}


            # Select Style ----

            sty <- self$options$sty

            if (sty == "arsenal") {

                # Arsenal Table ----

                tablearsenal <- arsenal::tableby(formula = formula2,
                                                 data = mydata,
                                                 total = TRUE,
                                                 digits = 1,
                                                 digits.count = 1
                )

                tablearsenal <- summary(tablearsenal, text = 'html')


                tablearsenal <- kableExtra::kable(tablearsenal,
                                             format = "html",
                                             digits = 1,
                                             escape = FALSE)


                self$results$tablestyle1$setContent(tablearsenal)


            } else if (sty == "finalfit") {

                # Table FinalFit ----
                # https://finalfit.org/articles/tables_gallery.html#cross-tables


                myvars <- jmvcore::composeTerm(components = self$options$vars)

                myvars <- jmvcore::decomposeTerm(term = myvars)

                # myvars <- jmvcore::decomposeFormula(formula = self$options$vars)
                # myvars <- unlist(myvars)

                mydata %>%
                    summary_factorlist(dependent = self$options$group,
                                       explanatory = myvars,
                                       # column = TRUE,
                                       total_col = TRUE,
                                       p = TRUE,
                                       add_dependent_label = TRUE,
                                       na_include = FALSE
                                       # catTest = catTestfisher
                ) -> tablefinalfit


                tablefinalfit <- kableExtra::kable(tablefinalfit,
                                                  format = "html",
                                                  digits = 1,
                                                  escape = FALSE)


                self$results$tablestyle2$setContent(tablefinalfit)


            } else if (sty == "gtsummary") {


                # gtsummary ----
                # http://www.danieldsjoberg.com/gtsummary/articles/gallery.html


                tablegtsummary <-
                    gtsummary::tbl_summary(data = mydata,
                                           by = self$options$group) %>%
                    gtsummary::modify_header(stat_by =
                                                 gt::md("**{level}** N =  {n} ({style_percent(p)}%)")) %>%
                    gtsummary::add_n(x = .) %>%
                    gtsummary::bold_labels(x = .) %>%
                    gtsummary::add_p(x = .,
                                     pvalue_fun =
                                         purrr::partial(
                                             gtsummary::style_pvalue,
                                             digits = 2)
                                     ) %>%
                    gtsummary::add_q()

                tablegtsummary <- gtsummary::as_kable_extra(tablegtsummary)


                self$results$tablestyle3$setContent(tablegtsummary)


            } else if (sty %in% c("nejm", "lancet", "hmisc")) {

                sty <- jmvcore::composeTerm(components = self$options$sty)

                tabletangram <-
                    tangram::html5(
                    tangram::tangram(
                        formula2,
                        mydata,
                        id = "tbl3"
                    ),
                    style = sty,
                    caption = paste0("Cross Table for Dependent ", self$options$group)
                    )



                self$results$tablestyle4$setContent(tabletangram)

}
}


        })
)
