#' @title Table One
#'
#' @return Table
#'
#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric
#'


tableoneClass <- if (requireNamespace("jmvcore")) R6::R6Class("tableoneClass",
    inherit = tableoneBase, private = list(.run = function() {

        # Error Message ----

            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")



        if (is.null(self$options$vars)) {

            # ToDo Message ----


            todo <- "
                <br>Welcome to ClinicoPath
                          <br><br>
                          This tool will help you form a Table One, which is almost always used in clinicopathological research manuscripts.
                          <br><br>
                          Select the 'Variables' you want to include in the table. Numeric, Ordinal, and Categorical variables are allowed.
                          Select the 'Table Style' for different package outputs.
                          <br><br>
                          Missing values are excluded by default. You may reinclude them.
                          Please note that excluded cases are removed from other variables as well.
                          <br><br>
                          Please cite the packages and jamovi using references below.
                          "

            html <- self$results$todo
            html$setContent(todo)

        } else {

            todo <- ""
        html <- self$results$todo
        html$setContent(todo)


        # Prepare Data ----

            varsName <- self$options$vars

            data <- jmvcore::select(self$data, c(varsName))


            # Exclude NA

            excl <- self$options$excl

            if (excl) {data <- jmvcore::naOmit(data)}


            # Select Style ----

            sty <- self$options$sty

            # tableone ----

            if (sty == "t1") {

            mytable <- tableone::CreateTableOne(data = data)

            self$results$tablestyle1$setContent(mytable)

                # gtsummary ----

            } else if (sty == "t2") {


                mytable <- gtsummary::tbl_summary(data = data)
                mytable <- gtsummary::as_kable_extra(mytable)

                self$results$tablestyle2$setContent(mytable)

            # arsenal ----

            } else if (sty == "t3") {



                formula <- jmvcore::constructFormula(terms = self$options$vars)
                formula <- paste('~', formula)
                formula <- as.formula(formula)
                mytable <- arsenal::tableby(formula = formula,
                                            data = data,
                                            total = TRUE,
                                            digits = 1,
                                            digits.count = 0,
                                            digits.pct = 1
                )

                mytable <- summary(mytable, text = "html")

                mytable <- kableExtra::kable(mytable, format = "html",
                                               digits = 1,
                                               escape = FALSE)

                self$results$tablestyle3$setContent(mytable)


            }  else if (sty == "t4") {


                # janitor ----

                tablelist <- list()

                for (i in 1:length(varsName)) {

                    var <- varsName[i]

                    table <- data %>%
                        janitor::tabyl(dat = ., var) %>%
                        janitor::adorn_totals("row") %>%
                        janitor::adorn_pct_formatting(dat = .)

                    tablelist[[var]] <- table

                }

                mytable <- tablelist

                self$results$tablestyle4$setContent(mytable)


            }


    }
    }
    ))
