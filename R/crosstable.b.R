#' @title Cross Table
#' @importFrom R6 R6Class
#' @import jmvcore
#'

crosstableClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "crosstableClass",
    inherit = crosstableBase,
    private = list(


        # labelData ----

        .labelData = function() {


            # Prepare data for analysis
            mydata <- self$data

            ## Get rownames to data
            # mydata$rownames <- rownames(mydata)

            ## Correct variable names and labels
            # Get original variable names
            original_names <- names(mydata)

            # Save original names as a named vector where the names are the original names,
            # and the values are the labels you want to set, which are also the original names.
            labels <- setNames(original_names, original_names)

            # Clean variable names
            mydata <- mydata %>% janitor::clean_names()

            # Now apply the labels to the cleaned names.
            # Since the variable names have been cleaned, you must match the labels to the cleaned names.
            # The labels vector should have names that are the cleaned names and values that are the original names.
            corrected_labels <-
                setNames(original_names, names(mydata))

            # Apply the corrected labels
            mydata <- labelled::set_variable_labels(.data = mydata,
                                                    .labels = corrected_labels)

            # Retrieve all variable labels
            all_labels <- labelled::var_label(mydata)

            # # Retrieve the variable name from the label
            # # Tek değişken için
            # dependent_variable_name_from_label <-
            #     names(all_labels)[all_labels == self$options$outcome]
            #
            # # Retrieve the variable names vector from the label vector
            # # Birden fazla değişkenler için
            # labels <- self$options$explanatory
            #
            # explanatory_variable_names <-
            #     names(all_labels)[match(labels, all_labels)]


            myvars <-  self$options$vars
            myvars <-
                names(all_labels)[match(myvars, all_labels)]


            mygroup <-
                names(all_labels)[all_labels == self$options$group]


            return(list(
                "mydata" = mydata
                , "myvars" = myvars
                , "mygroup" = mygroup
            ))


        }
        ,























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
            }


                if (sty == "finalfit") {

                    todo2 <- glue::glue("
                    <br>
                    <b>finalfit</b> uses
                    <em>aov (analysis of variance) or t.test for Welch two sample t-test. Note continuous non-parametric test is always Kruskal Wallis (kruskal.test) which in two-group setting is equivalent to Mann-Whitney U /Wilcoxon rank sum test</em>. See full documentation <a href= 'https://finalfit.org/reference/summary_factorlist.html'>here</a>.
                    "
                    )

                }


            if (sty != "finalfit") {

                    todo2 <- glue::glue("")
                }

                    html <- self$results$todo2
                    html$setContent(todo2)

                # Error Message ----

            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")





                    # Read Labelled Data ----

                    cleaneddata <- private$.labelData()

                    mydata <- cleaneddata$mydata
                    myvars <- cleaneddata$myvars
                    mygroup <- cleaneddata$mygroup



                # Prepare Data ----

            # mydata <- self$data

            # formulaR <- jmvcore::constructFormula(terms = self$options$vars)
            # formulaL <- jmvcore::constructFormula(terms = self$options$group)

            formula <- jmvcore::constructFormula(
                terms = myvars, #self$options$vars,
                dep = mygroup #self$options$group
                )


            # formula <- paste(formulaL, '~', formulaR)
            formula <- as.formula(formula)


            # self$results$r_cleaneddata$setContent(
            #     list(
            #         "mydata" = head(mydata)
            #         , "myvars" = myvars
            #         , "mygroup" = mygroup
            #         , "formula" = formula
            #         , "names" = names(mydata)
            #         , "mygroup2" = mydata[[mygroup]][1:10]
            #
            #         ))
















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


                myvars <- jmvcore::composeTerm(components = myvars #self$options$vars
                                               )

                myvars <- jmvcore::decomposeTerm(term = myvars)

                # myvars <- jmvcore::decomposeFormula(formula = self$options$vars)
                # myvars <- unlist(myvars)

                mydata %>%
                    finalfit::summary_factorlist(
                        .data = .,
                        dependent = mygroup, #self$options$group,
                        explanatory = myvars,
                        total_col = TRUE,
                        p = TRUE,
                        add_dependent_label = TRUE,
                        na_include = FALSE,
                        na_to_p = FALSE,
                        cont = self$options$cont,
                        cont_nonpara = NULL,
                        cont_cut = 5,
                        cont_range = TRUE,

                        p_cont_para = "aov",
                        p_cat = self$options$pcat,

                        dependent_label_prefix = "Dependent: ",
                        dependent_label_suffix = "",
                        row_totals_colname = "Total N",
                        row_missing_colname = "Missing N",


                        column = TRUE,

                        orderbytotal = FALSE,
                        digits = c(1, 1, 3, 1, 0),


                        na_include_dependent = FALSE,
                        na_complete_cases = FALSE,
                        fit_id = FALSE,

                        na_to_prop = TRUE,


                        add_col_totals = TRUE,
                        include_col_totals_percent = TRUE,
                        col_totals_rowname = NULL,
                        col_totals_prefix = "",
                        add_row_totals = FALSE,
                        include_row_totals_percent = TRUE,
                        include_row_missing_col = TRUE,

                        catTest = NULL,
                        weights = NULL







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
                                           by = mygroup
                    )



                #                            , #self$options$group,
                #                            statistic = list(
                #                                gtsummary::all_continuous() ~ "{mean} ({sd})",
                #                                gtsummary::all_categorical() ~ "{n} / {N} ({p}%)"
                #                            ),
                #                            digits = gtsummary::all_continuous() ~ 2,
                #                            missing_text = "(Missing)"
                #
                #                            ) %>%
                #     gtsummary::modify_header(
                #         update = gtsummary::all_stat_cols() ~ structure("**{level}** N =  {n} ({style_percent(p)}%)", class = "from_markdown")
                #         # stat_by =
                #         #                          gt::md("**{level}** N =  {n} ({style_percent(p)}%)")
                #                                   ) %>%
                #     gtsummary::add_n(x = .) %>%
                #     gtsummary::add_overall() %>%
                #     gtsummary::bold_labels(x = .) %>%
                #     gtsummary::add_p(x = .,
                #                      pvalue_fun =
                #                          purrr::partial(
                #                              gtsummary::style_pvalue,
                #                              digits = 2)
                #                      ) %>%
                #     gtsummary::add_q()
                # # %>%
                # #     gtsummary::bold_labels() %>%
                # #     gtsummary::bold_levels() %>%
                # #     gtsummary::bold_p()


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
                            mygroup #self$options$group
                            ),
                        id = "tbl3")

                self$results$tablestyle4$setContent(tabletangram)


                # export <- self$options$export
                # if (export)
                # {
                #     if (.Platform$OS.type == "windows") {
                #         stopifnot(file.exists("C:\\temp2"))
                #         write(
                #             x = tabletangram,
                #             file = "C:\\temp2\\ClinicoPathCrossTable.html"
                #         )
                #     } else {
                #     write(x = tabletangram,
                #           file = "~/Documents/ClinicoPathCrossTable.html"
                #           )
                #     }
                # }


            }
                # tableone ----
                # tab3 <- CreateTableOne(vars = myVars, strata = "trt" , data = pbc, factorVars = catVars)

            }
        )
)
