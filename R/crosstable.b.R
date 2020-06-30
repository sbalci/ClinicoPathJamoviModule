#' @title Cross Table
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



            # Select Style ----

            sty <- self$options$sty




            if (is.null(self$options$vars) || is.null(self$options$group)) {

                # ToDo Message ----

                todo <- glue::glue("
                <br>
                <br>Welcome to ClinicoPath.<br>
                This tool will help you form a Cross Table.<br>
                The functions select hypothesis tests automatically. You may see different results with different tables. Please verify your data distribution and appropriateness of the test accordingly. You may find Statkat module useful.<br>
                Please cite the packages and jamovi using references below.<br><hr>")

                html <- self$results$todo
                html$setContent(todo)
                return()

            } else {

                todo <- ""
                html <- self$results$todo
                html$setContent(todo)


                if (sty == "finalfit") {


                    todo <- glue::glue("
                    <br>
                    <b>finalfit</b> uses
                    <em>aov (analysis of variance) or t.test for Welch two sample t-test. Note continuous non-parametric test is always Kruskal Wallis (kruskal.test) which in two-group setting is equivalent to Mann-Whitney U /Wilcoxon rank sum test</em>. See full documentation <a href= 'https://finalfit.org/reference/summary_factorlist.html'>here</a>.
                    "
                    )


                    html <- self$results$todo
                    html$setContent(todo)

                }

                # Error Message ----

                if (nrow(self$data) == 0) stop("Data contains no (complete) rows")

                # Prepare Data ----

            mydata <- self$data

            # formulaR <- jmvcore::constructFormula(terms = self$options$vars)
            # formulaL <- jmvcore::constructFormula(terms = self$options$group)

            formula <- jmvcore::constructFormula(
                terms = self$options$vars,
                dep = self$options$group)


            # formula <- paste(formulaL, '~', formulaR)
            formula <- as.formula(formula)



            # Exclude NA ----

            excl <- self$options$excl

            if (excl) {mydata <- jmvcore::naOmit(mydata)}



            # tab3 <- CreateTableOne(vars = myVars, strata = "trt" , data = pbc, factorVars = catVars)


            # Arsenal Table ----

            if (sty == "arsenal") {


                tablearsenal <- arsenal::tableby(formula = formula,
                                                 data = mydata,
                                                 total = TRUE,
                                                 digits = 1,
                                                 digits.count = 1
                )

                tablearsenal <- summary(tablearsenal,
                                        text = 'html',
                                        pfootnote = 'html'
                                        )

                # tablearsenal_output <- capture.output(tablearsenal)

                # self$results$tablearsenal_output$setContent(tablearsenal_output)


                # tablearsenal <-
                #     kableExtra::kable(tablearsenal,
                #                       format = "html",
                #                       digits = 1,
                #                       escape = FALSE
                #                       )


                tablearsenal <- capture.output(tablearsenal)

                self$results$tablestyle1$setContent(tablearsenal)


            } else if (sty == "finalfit") {

                # FinalFit ----
                # https://finalfit.org/articles/tables_gallery.html#cross-tables
                # https://finalfit.org/reference/summary_factorlist.html


                myvars <- jmvcore::composeTerm(components = self$options$vars)

                myvars <- jmvcore::decomposeTerm(term = myvars)

                # myvars <- jmvcore::decomposeFormula(formula = self$options$vars)
                # myvars <- unlist(myvars)

                mydata %>%
                    finalfit::summary_factorlist(
                        .data = .,
                        dependent = self$options$group,
                        explanatory = myvars,
                        total_col = TRUE,
                        p = TRUE,
                        add_dependent_label = TRUE,
                        na_include = FALSE,
                        na_to_p = FALSE,
                        cont = self$options$cont,
                        cont_range = TRUE,


                        p_cont_para = "aov",
                        p_cat = self$options$pcat,








                        cont_nonpara = NULL,
                        cont_cut = 5,
                        dependent_label_prefix = "Dependent: ",
                        dependent_label_suffix = "",
                        row_totals_colname = "Total N",
                        row_missing_colname = "Missing N",




                        column = TRUE,
                        orderbytotal = FALSE,
                        digits = c(1, 1, 3, 1),
                        na_include_dependent = FALSE,
                        na_complete_cases = FALSE,
                        fit_id = FALSE,
                        add_col_totals = FALSE,
                        include_col_totals_percent = TRUE,
                        col_totals_rowname = NULL,
                        col_totals_prefix = "",
                        add_row_totals = FALSE,
                        include_row_missing_col = TRUE

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

                tablegtsummary <-
                    gtsummary::as_kable_extra(tablegtsummary)


                self$results$tablestyle3$setContent(tablegtsummary)


            } else if (sty %in% c("nejm", "lancet", "hmisc")) {

                sty <- jmvcore::composeTerm(components = self$options$sty)


                # tangram ----


                tabletangram <-
                    tangram::html5(
                        tangram::tangram(
                            formula,
                            mydata,
                            transform = tangram::hmisc,
                            id = "tbl3",
                            test = TRUE,
                            digits = 1,
                            include_p = TRUE
                        ),
                        fragment = TRUE,
                        style = sty,
                        caption = paste0(
                            "Cross Table for Dependent ",
                            self$options$group),
                        id = "tbl3")

                self$results$tablestyle4$setContent(tabletangram)


                export <- self$options$export

                if (export)
                {
                    write(x = tabletangram,
                          file = "~/Documents/ClinicoPathCrossTable.html"

                          )
                }


            }
                # tableone ----
                # tab3 <- CreateTableOne(vars = myVars, strata = "trt" , data = pbc, factorVars = catVars)

            }
        })
)
