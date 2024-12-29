#' @title Cross Table
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom chisq.posthoc.test chisq.posthoc.test
#'

crosstableClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "crosstableClass",
        inherit = crosstableBase,
        private = list(
            # labelData ----

            .labelData = function() {
                # Prepare data for analysis
                mydata <- self$data

                # mydata <- as.data.frame(self$data, stringsAsFactors = TRUE)  # Ensure data is data.frame

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
                mydata <- labelled::set_variable_labels(.data = mydata, .labels = corrected_labels)

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
                    ,
                    "myvars" = myvars
                    ,
                    "mygroup" = mygroup
                ))


            }



            ,
            .run = function() {
                # Select Style ----

                sty <- self$options$sty


                if (is.null(self$options$vars) ||
                    is.null(self$options$group)) {
                    # ToDo Message ----

                    todo <- glue::glue(
                        "
                <br>
                <br>Welcome to ClinicoPath.<br>
                This tool will help you form a Cross Table.<br>
                The functions select hypothesis tests automatically. You may see different results with different tables. Please verify your data distribution and appropriateness of the test accordingly. You may find Statkat module useful.<br>
                Please cite the packages and jamovi using references below.<br><hr>"
                    )

                    html <- self$results$todo
                    html$setContent(todo)
                    return()

                } else {
                    todo <- ""
                    html <- self$results$todo
                    html$setContent(todo)
                }


                if (sty == "finalfit") {
                    todo2 <- glue::glue(
                        "
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

                if (nrow(self$data) == 0)
                    stop("Data contains no (complete) rows")





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
                    terms = myvars,
                    #self$options$vars,
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

                    if (excl) {
                        mydata <- jmvcore::naOmit(mydata)
                    }



                    # tab3 <- CreateTableOne(vars = myVars, strata = "trt" , data = pbc, factorVars = catVars)


                    ## Arsenal Table ----

                    if (sty == "arsenal") {
                        tablearsenal <- arsenal::tableby(
                            formula = formula,
                            data = mydata,
                            total = TRUE,
                            digits = 1,
                            digits.count = 1
                        )

                        tablearsenal <- summary(tablearsenal, text = 'html', pfootnote = 'html')

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
                        ## FinalFit ----
                        # https://finalfit.org/articles/tables_gallery.html#cross-tables
                        # https://finalfit.org/reference/summary_factorlist.html


                        myvars <- jmvcore::composeTerm(
                            components = myvars #self$options$vars
                            )

                            myvars <- jmvcore::decomposeTerm(term = myvars)

                            # myvars <- jmvcore::decomposeFormula(formula = self$options$vars)
                            # myvars <- unlist(myvars)

                            mydata %>%
                                finalfit::summary_factorlist(
                                    .data = .,
                                    dependent = mygroup,
                                    #self$options$group,
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








                            tablefinalfit <- kableExtra::kable(
                                tablefinalfit,
                                format = "html",
                                digits = 1,
                                escape = FALSE
                            )


                            self$results$tablestyle2$setContent(tablefinalfit)


                            } else if (sty == "gtsummary") {
                                ## gtsummary ----

                                # http://www.danieldsjoberg.com/gtsummary/articles/gallery.html


                                tablegtsummary <-
                                    gtsummary::tbl_summary(data = mydata, by = mygroup)



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


                                ## tangram ----


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
                                        caption = paste0("Cross Table for Dependent ", mygroup #self$options$group
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
                    ## tableone ----
                    # tab3 <- CreateTableOne(vars = myVars, strata = "trt" , data = pbc, factorVars = catVars)



                    # # posthoc
                    #
                    # if (self$options$perform_posthoc &&
                    #     !is.null(self$options$primary_var) &&
                    #     !is.null(self$options$secondary_var)) {
                    #
                    #     tryCatch({
                    #         # Get variable names for labels
                    #         primary_name <- self$options$primary_var
                    #         secondary_name <- self$options$secondary_var
                    #
                    #         # Create contingency table
                    #         cont_table <- table(
                    #             self$data[[primary_name]],
                    #             self$data[[secondary_name]]
                    #         )
                    #
                    #         # Check if table is valid for analysis
                    #         if(nrow(cont_table) > 0 && ncol(cont_table) > 0) {
                    #             # Perform residuals analysis
                    #             residuals_results <- chisq.posthoc.test::chisq.posthoc.test(
                    #                 x = cont_table,
                    #                 method = "bonferroni",
                    #                 round = 3
                    #             )
                    #
                    #             # Format and display results
                    #             posthoc_html <- private$.formatPostHocResults(
                    #                 residuals_results,
                    #                 variable_names = c(primary_name, secondary_name)
                    #             )
                    #
                    #             self$results$posthoc$setContent(posthoc_html)
                    #         } else {
                    #             self$results$posthoc$setContent(
                    #                 "<div class='error'>Unable to perform post-hoc analysis: Invalid contingency table</div>"
                    #             )
                    #         }
                    #     }, error = function(e) {
                    #         self$results$posthoc$setContent(
                    #             sprintf("<div class='error'>Error in post-hoc analysis: %s</div>", e$message)
                    #         )
                    #     })
                    # }


                    # # Pairwise Tests
                    #
                    # if (self$options$perform_posthoc) {
                    #     # Perform Chi-Square Test
                    #
                    #     data_posthoc <- self$data
                    #
                    #     primary_var <- self$options$primary_var
                    #     secondary_var <- self$options$secondary_var
                    #
                    #
                    #     cont_table <- table(data_posthoc[[primary_var]], data_posthoc[[secondary_var]])
                    #
                    #     chi_square_test <- chisq.test(cont_table)
                    #
                    #
                    #
                    #     basic_analysis <- private$.performCrossTableAnalysis(data_posthoc, secondary_var, primary_var)
                    #     self$results$basic_analysis$setContent(basic_analysis)
                    #
                    #
                    #
                    #
                    #
                    #     # Get number of groups
                    #     n_groups <- length(unique(data_posthoc[[secondary_var]]))
                    #
                    #
                    #     # Perform post-hoc analysis if more than 2 groups
                    #     # if (n_groups > 2 && self$options$posthoc != "none")
                    #
                    #
                    #     posthoc_html <- private$.performPostHoc(
                    #         cont_table = cont_table,
                    #         data = data_posthoc,
                    #         group_var = secondary_var,
                    #         response_var = primary_var
                    #     )
                    #
                    #     # Combine main table with post-hoc results
                    #     final_html <- paste0(
                    #         '<br><hr><br>',
                    #         '<div class="posthoc-section">',
                    #         '<h4>Post-hoc Analysis</h4>',
                    #         posthoc_html,
                    #         '</div>'
                    #     )
                    #
                    #     self$results$posthoc$setContent(final_html)
                    # }

                            }

        #     ,
        #     # formatPostHocResults
        #     .formatPostHocResults = function(residuals_results, variable_names) {
        #         # Create both views of the same data
        #         html <- paste0(
        #             '<div class="posthoc-analysis">',
        #
        #             # First view - Grade focus
        #             sprintf('<h3>Chi-square Post-hoc Analysis Results</h3>
        #         <table class="table table-striped">
        #         <thead>
        #             <tr>
        #                 <th>%s</th>
        #                 <th>Measure</th>
        #                 <th>%s Absent</th>
        #                 <th>%s Present</th>
        #             </tr>
        #         </thead>
        #         <tbody>',
        #                     variable_names[1], variable_names[2], variable_names[2]),
        #             # Note the change from self$ to private$ here:
        #             private$.formatResidualRows(residuals_results),
        #             '</tbody></table>',
        #
        #             # Interpretation guide
        #             '<h4>How to Interpret These Results:</h4>
        # <ul>
        #     <li><strong>Residuals:</strong> Show how much each combination differs from expected values
        #         <ul>
        #             <li>Values greater than 1.96 or less than -1.96 indicate significant differences</li>
        #             <li>Positive values: more cases than expected</li>
        #             <li>Negative values: fewer cases than expected</li>
        #         </ul>
        #     </li>
        #     <li><strong>P-values:</strong> Show statistical significance after Bonferroni correction
        #         <ul>
        #             <li>Values less than 0.05 would indicate significant differences</li>
        #             <li>All values = 1.000 here indicate no significant differences</li>
        #         </ul>
        #     </li>
        # </ul>',
        #
        #             # Summary
        #             '<div class="summary-box">
        # <h4>Summary of Your Results:</h4>
        # <p>',
        #             # Note the change from self$ to private$ here:
        #             private$.generateResultsSummary(residuals_results, variable_names),
        #             '</p>
        # </div>',
        #
        #             # Second view
        #             sprintf('<h3>Chi-square Post-hoc Analysis - %s Focus</h3>', variable_names[2]),
        #             # Note the change from self$ to private$ here:
        #             private$.formatAlternativeView(residuals_results, variable_names),
        #             '</div>',
        #
        #             # Styling remains the same
        #             '<style>
        # .posthoc-analysis {
        #     font-family: Arial, sans-serif;
        #     margin: 20px 0;
        # }
        # .posthoc-analysis table {
        #     width: 100%;
        #     border-collapse: collapse;
        #     margin: 15px 0;
        # }
        # .posthoc-analysis th, .posthoc-analysis td {
        #     padding: 8px;
        #     text-align: left;
        #     border: 1px solid #ddd;
        # }
        # .posthoc-analysis th {
        #     background-color: #f5f5f5;
        # }
        # .summary-box {
        #     background-color: #f8f9fa;
        #     border-radius: 5px;
        #     padding: 15px;
        #     margin: 20px 0;
        # }
        # .interpretation-box {
        #     background-color: #e9ecef;
        #     padding: 15px;
        #     margin: 10px 0;
        #     border-radius: 5px;
        # }
        # </style>'
        #         )
        #
        #         return(html)
        #     }



            # ,
            # .generateResultsSummary = function(results, variable_names) {
            #     # Extract max absolute residual
            #     max_residual <- max(abs(results$residuals))
            #
            #     summary <- sprintf(
            #         "No significant associations were found between %s and %s status. All residuals are well below the critical value of ±1.96, and all adjusted p-values are 1.000, indicating that the distribution of %s is similar across all grades.",
            #         variable_names[1],
            #         variable_names[2],
            #         variable_names[2]
            #     )
            #
            #     return(summary)
            # }

            # ,
            # .formatAlternativeView = function(residuals_results, variable_names) {
            #     # Format second view focusing on the other variable
            #     html <- paste0(
            #         '<div class="interpretation-box">',
            #         sprintf('<h4>Interpretation of %s Effects:</h4>', variable_names[2]),
            #         '<ul>',
            #         sprintf('<li><strong>%s Absent:</strong><br>', variable_names[2]),
            #         self$formatEffectInterpretation(residuals_results, "absent"),
            #         '</li>',
            #         sprintf('<li><strong>%s Present:</strong><br>', variable_names[2]),
            #         self$formatEffectInterpretation(residuals_results, "present"),
            #         '</li>',
            #         '<li><strong>Statistical Significance:</strong><br>',
            #         'All residuals are well below critical value (±1.96)<br>',
            #         'All p-values = 1.000 indicate no significant deviations<br>',
            #         sprintf(
            #             '%s distribution is similar across all grades',
            #             variable_names[2]
            #         ),
            #         '</li>',
            #         '</ul>',
            #         '</div>'
            #     )
            #     return(html)
            # }


        #     ,
        #     # .performCrossTableAnalysis
        #     .performCrossTableAnalysis = function(data, group_var, response_var) {
        #         # Create the contingency table
        #         cont_table <- table(data[[response_var]], data[[group_var]])
        #         # Get margins
        #         row_totals <- rowSums(cont_table)
        #         col_totals <- colSums(cont_table)
        #         n <- sum(cont_table)
        #
        #         # Calculate expected frequencies
        #         expected <- outer(row_totals, col_totals) / n
        #
        #         # Check conditions for Chi-Square vs Fisher's
        #         expected_less_than_5 <- sum(expected < 5)
        #         percent_less_than_5 <- (expected_less_than_5 / length(expected)) * 100
        #         min_expected <- min(expected)
        #
        #         # Perform Chi-Square test
        #         chi_result <- chisq.test(cont_table)
        #
        #         # Perform Fisher's exact test if needed
        #         fisher_needed <- (percent_less_than_5 > 20) ||
        #             (min_expected < 1)
        #         if (fisher_needed) {
        #             fisher_result <- fisher.test(cont_table, simulate.p.value = TRUE)
        #         }
        #
        #         # Create detailed HTML output
        #         html <- paste0(
        #             '<div class="analysis-results">',
        #
        #             # 1. Cross-tabulation
        #             '<section class="cross-table">',
        #             '<h4>Contingency Table Analysis</h4>',
        #             '<p>The following table shows the distribution of cases across TStage levels and groups:</p>',
        #             kableExtra::kable(cont_table, format = "html", caption = "Cross-tabulation of TStage by Group") %>%
        #                 kableExtra::kable_styling(bootstrap_options = c("striped", "hover")),
        #             '</section><br>',
        #
        #             # 2. Data description
        #             '<section class="data-description">',
        #             '<h4>Data Description</h4>',
        #             sprintf(
        #                 '<p>The analysis includes %d total observations distributed across %d rows and %d columns. ',
        #                 n,
        #                 nrow(cont_table),
        #                 ncol(cont_table)
        #             ),
        #             'The data represents the relationship between TStage and group membership.</p>',
        #             '</section><br>',
        #
        #             # 3. Chi-Square Test Results
        #             '<section class="chi-square">',
        #             '<h4>Chi-Square Test of Independence</h4>',
        #             '<p>Test results:</p>',
        #             sprintf(
        #                 '<ul>',
        #                 '<li>Chi-square statistic = %.3f</li>',
        #                 '<li>Degrees of freedom = %d</li>',
        #                 '<li>p-value = %.3f</li>',
        #                 '</ul>',
        #                 chi_result$statistic,
        #                 chi_result$parameter,
        #                 chi_result$p.value
        #             ),
        #             '<p><strong>Interpretation:</strong> ',
        #             if (chi_result$p.value < 0.05) {
        #                 'There is a significant association between TStage and group membership (p < 0.05).'
        #             } else {
        #                 'There is no significant association between TStage and group membership (p ≥ 0.05).'
        #             },
        #             '</p>',
        #             '</section><br>',
        #
        #             # 4. Assumptions Check
        #             '<section class="assumptions">',
        #             '<h4>Chi-Square Test Assumptions</h4>',
        #             sprintf(
        #                 '<ul>',
        #                 '<li>Minimum expected frequency: %.2f</li>',
        #                 '<li>Number of cells with expected frequency < 5: %d (%.1f%%)</li>',
        #                 '</ul>',
        #                 min_expected,
        #                 expected_less_than_5,
        #                 percent_less_than_5
        #             ),
        #             if (fisher_needed) {
        #                 paste0(
        #                     '<p><strong>Note:</strong> Chi-Square test assumptions are violated ',
        #                     '(>20% cells have expected frequencies <5 or minimum expected frequency <1). ',
        #                     'Fisher\'s Exact Test is recommended and provided below.</p>'
        #                 )
        #             } else {
        #                 '<p><strong>Note:</strong> Chi-Square test assumptions are met.</p>'
        #             },
        #             '</section><br>'
        #         )
        #
        #         # 5. Fisher's Exact Test if needed
        #         if (fisher_needed) {
        #             html <- paste0(
        #                 html,
        #                 '<section class="fisher">',
        #                 '<h4>Fisher\'s Exact Test</h4>',
        #                 sprintf(
        #                     '<p>Fisher\'s Exact Test p-value = %.3f</p>',
        #                     fisher_result$p.value
        #                 ),
        #                 '<p><strong>Interpretation:</strong> ',
        #                 if (fisher_result$p.value < 0.05) {
        #                     'There is a significant association between TStage and group membership (p < 0.05).'
        #                 } else {
        #                     'There is no significant association between TStage and group membership (p ≥ 0.05).'
        #                 },
        #                 '</p>',
        #                 '</section>'
        #             )
        #         }
        #
        #         html <- paste0(html, '</div>')
        #
        #         # Add some CSS styling
        #         html <- paste0(
        #             '<style>
        # .analysis-results {
        #     font-family: Arial, sans-serif;
        #     line-height: 1.6;
        #     max-width: 800px;
        #     margin: 0 auto;
        # }
        # .analysis-results section {
        #     margin-bottom: 20px;
        #     padding: 15px;
        #     background: #f8f9fa;
        #     border-radius: 5px;
        # }
        # .analysis-results h4 {
        #     color: #2c3e50;
        #     margin-bottom: 15px;
        # }
        # .analysis-results ul {
        #     margin-left: 20px;
        # }
        # </style>',
        #             html
        #         )
        #
        #         return(html)
        #     }






            # ,
            # # Format residuals results
            # .formatResiduals = function(results) {
            #     # Format residuals results as HTML table
            #     html <- kableExtra::kable(results,
            #                               format = "html",
            #                               caption = "Cell Residuals Analysis",
            #                               digits = 3) %>%
            #         kableExtra::kable_styling(
            #             bootstrap_options = c("striped", "hover", "condensed"),
            #             full_width = FALSE
            #         ) %>%
            #         kableExtra::add_footnote(
            #             c(
            #                 "Residuals > |1.96| indicate significant deviation",
            #                 "P-values adjusted using Bonferroni correction"
            #             ),
            #             notation = "symbol"
            #         )
            #
            #     return(html)
            # }



            # ,
            # # Update the .performPairwiseTests function
            # .performPairwiseTests = function(data, group_var, response_var) {
            #     # Get all valid levels (excluding NA)
            #     group_levels <- sort(unique(data[[group_var]]))
            #     group_levels <- group_levels[!is.na(group_levels)]
            #     results <- list()
            #
            #     for (i in 1:(length(group_levels) - 1)) {
            #         for (j in (i + 1):length(group_levels)) {
            #             # Create subset excluding NA values
            #             subset_data <- subset(data,
            #                                   data[[group_var]] %in% c(group_levels[i], group_levels[j]) &
            #                                       !is.na(data[[response_var]]))
            #
            #             if (nrow(subset_data) > 0) {
            #                 table_ij <- table(subset_data[[group_var]], subset_data[[response_var]])
            #
            #                 test <- suppressWarnings(chisq.test(table_ij))
            #
            #                 comparison_name <- sprintf("TStage %s vs %s", group_levels[i], group_levels[j])
            #                 results[[comparison_name]] <- list(
            #                     chi_square = test$statistic,
            #                     df = test$parameter,
            #                     p_value = test$p.value,
            #                     n1 = sum(subset_data[[group_var]] == group_levels[i]),
            #                     n2 = sum(subset_data[[group_var]] == group_levels[j])
            #                 )
            #             }
            #         }
            #     }
            #     return(results)
            # }

            # ,
            # # Update the .formatPairwise function
            # .formatPairwise = function(results) {
            #     # Convert list to data frame
            #     df <- do.call(rbind, lapply(names(results), function(name) {
            #         data.frame(
            #             Comparison = name,
            #             N1 = results[[name]]$n1,
            #             N2 = results[[name]]$n2,
            #             ChiSquare = round(results[[name]]$chi_square, 3),
            #             df = results[[name]]$df,
            #             p_value = round(results[[name]]$p_value, 3),
            #             stringsAsFactors = FALSE
            #         )
            #     }))
            #
            #     # Format as HTML table with better explanations
            #     html <- paste0(
            #         '<h4>Pairwise Chi-square Tests</h4>',
            #         '<p>Each row represents a comparison between two TStage levels:</p>',
            #         kableExtra::kable(
            #             df,
            #             format = "html",
            #             col.names = c(
            #                 "Comparison",
            #                 "N (Group 1)",
            #                 "N (Group 2)",
            #                 "Chi-Square",
            #                 "df",
            #                 "p-value"
            #             )
            #         ) %>%
            #             kableExtra::kable_styling(
            #                 bootstrap_options = c("striped", "hover", "condensed"),
            #                 full_width = FALSE
            #             ) %>%
            #             kableExtra::add_footnote(
            #                 c(
            #                     "p-values are unadjusted for multiple comparisons",
            #                     "Consider using Bonferroni correction by multiplying p-values by number of comparisons"
            #                 )
            #             )
            #     )
            #     return(html)
            # }

        #     ,
        #     # .performPostHoc function
        # .performPostHoc = function(cont_table, data, group_var, response_var) {
        #     # Get variable names for nice display
        #     var1_name <- response_var
        #     var2_name <- group_var
        #
        #     # Perform Chi-square test
        #     chi_result <- suppressWarnings(chisq.test(cont_table))
        #
        #     # Check if Fisher's exact test is needed
        #     expected <- chi_result$expected
        #     fisher_needed <- (sum(expected < 5) / length(expected)) > 0.2
        #
        #     if(fisher_needed) {
        #         fisher_result <- fisher.test(cont_table, simulate.p.value = TRUE)
        #     }
        #
        #     # Start building HTML output
        #     html <- paste0(
        #         '<div class="analysis-container">',
        #         '<h3>Statistical Analysis Results</h3>',
        #         '<div class="main-test">',
        #         sprintf('<h4>Cross-tabulation of %s by %s</h4>', var1_name, var2_name),
        #         '<table class="cross-table">',
        #         private$.formatContingencyTable(cont_table),
        #         '</table>',
        #
        #         '<div class="test-results">',
        #         sprintf('<h4>%s Results</h4>',
        #                 if(fisher_needed) "Fisher's Exact Test" else "Chi-square Test"),
        #         '<ul>',
        #         if(!fisher_needed) {
        #             sprintf(
        #                 paste(
        #                     '<li>Chi-square statistic: %.3f</li>',
        #                     '<li>Degrees of freedom: %d</li>',
        #                     '<li>p-value: %.3f</li>'
        #                 ),
        #                 chi_result$statistic,
        #                 chi_result$parameter,
        #                 chi_result$p.value
        #             )
        #         } else {
        #             sprintf('<li>Fisher\'s Exact Test p-value: %.3f</li>',
        #                     fisher_result$p.value)
        #         },
        #         '</ul>',
        #         sprintf('<p><strong>Interpretation:</strong> %s</p>',
        #                 private$.generateSummaryText(chi_result, fisher_needed,
        #                                           if(fisher_needed) fisher_result else NULL)),
        #         '</div>'
        #     )
        #
        #     # Add post-hoc analysis based on method selection
        #     if(self$options$posthoc_method %in% c("both", "residuals")) {
        #         residuals_results <- chisq.posthoc.test::chisq.posthoc.test(
        #             cont_table, method = "bonferroni")
        #
        #         html <- paste0(html,
        #                        '<div class="post-hoc-residuals">',
        #                        '<h4>Standardized Residuals Analysis</h4>',
        #                        private$.formatResiduals(residuals_results),
        #                        '</div>'
        #         )
        #     }
        #
        #     if(self$options$posthoc_method %in% c("both", "pairwise")) {
        #         pairwise_results <- private$.performPairwiseTests(
        #             data, group_var, response_var)
        #
        #         html <- paste0(html,
        #                        '<div class="post-hoc-pairwise">',
        #                        '<h4>Pairwise Comparisons</h4>',
        #                        private$.formatPairwise(pairwise_results),
        #                        '</div>'
        #         )
        #     }
        #
        #     # Add styling
        #     html <- paste0(html,
        #                    '<style>
        # .analysis-container {
        #     font-family: Arial, sans-serif;
        #     max-width: 900px;
        #     margin: 20px auto;
        #     padding: 20px;
        # }
        # .cross-table, .results-table {
        #     width: 100%;
        #     border-collapse: collapse;
        #     margin: 15px 0;
        # }
        # .cross-table th, .cross-table td,
        # .results-table th, .results-table td {
        #     padding: 8px;
        #     border: 1px solid #ddd;
        #     text-align: center;
        # }
        # .cross-table th {
        #     background-color: #f5f5f5;
        # }
        # .main-test, .post-hoc-residuals, .post-hoc-pairwise {
        #     background: #fff;
        #     padding: 20px;
        #     margin: 20px 0;
        #     border-radius: 5px;
        #     box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        # }
        # h3, h4 {
        #     color: #2c3e50;
        #     margin-bottom: 15px;
        # }
        # .test-results {
        #     background: #f8f9fa;
        #     padding: 15px;
        #     border-radius: 5px;
        #     margin: 15px 0;
        # }
        # </style>',
        #                    '</div>'
        #     )
        #
        #     return(html)
        # }

        # ,
        # # Helper function to format contingency table
        # .formatContingencyTable = function(cont_table) {
        #     # Create header row
        #     header <- sprintf(
        #         '<tr><th></th>%s</tr>',
        #         paste(sprintf('<th>%s</th>', colnames(cont_table)), collapse='')
        #     )
        #
        #     # Create data rows
        #     rows <- character()
        #     for(i in 1:nrow(cont_table)) {
        #         rows <- c(rows, sprintf(
        #             '<tr><th>%s</th>%s</tr>',
        #             rownames(cont_table)[i],
        #             paste(sprintf('<td>%d</td>', cont_table[i,]), collapse='')
        #         ))
        #     }
        #
        #     return(paste(c(header, rows), collapse='\n'))
        # }

            # ,
            # # Add these helper functions to the private list
            # .formatResidualRows = function(results) {
            #     # Get all unique levels from the results
            #     levels <- unique(rownames(results$observed))
            #
            #     rows <- character()
            #     for (level in levels) {
            #         # Get residuals and p-values for this level
            #         residuals <- results$stdres[level, ]
            #         p_values <- results$pvalues[level, ]
            #
            #         # Create row for residuals
            #         residual_row <- sprintf(
            #             '<tr><td rowspan="2">%s</td><td>Residuals</td>%s</tr>',
            #             level,
            #             paste(sprintf('<td>%.3f</td>', residuals), collapse = '')
            #         )
            #
            #         # Create row for p-values
            #         pvalue_row <- sprintf('<tr><td>p-values</td>%s</tr>',
            #                               paste(sprintf('<td>%.3f</td>', p_values), collapse = ''))
            #
            #         rows <- c(rows, residual_row, pvalue_row)
            #     }
            #
            #     return(paste(rows, collapse = '\n'))
            # }

            # ,
            # .formatEffectInterpretation = function(residuals_results, type) {
            #     interpretations <- character()
            #
            #     # Get relevant rows from results
            #     observed <- residuals_results$observed
            #     residuals <- residuals_results$stdres
            #
            #     # For each level in the primary variable
            #     for (i in 1:nrow(residuals)) {
            #         level <- rownames(residuals)[i]
            #         res <- residuals[i, ]
            #
            #         # Generate interpretation based on residual values
            #         effects <- character()
            #         for (j in 1:length(res)) {
            #             if (abs(res[j]) > 0) {
            #                 # Only mention non-zero effects
            #                 direction <- if (res[j] < 0)
            #                     "under-represented"
            #                 else
            #                     "over-represented"
            #                 strength <- if (abs(res[j]) < 0.5)
            #                     "slightly"
            #                 else
            #                     if (abs(res[j]) < 1.0)
            #                         "moderately"
            #                 else
            #                     if (abs(res[j]) < 1.96)
            #                         "notably"
            #                 else
            #                     "significantly"
            #
            #                 effects <- c(effects,
            #                              sprintf("%s %s (%.3f)", strength, direction, res[j]))
            #             }
            #         }
            #
            #         if (length(effects) > 0) {
            #             interpretations <- c(interpretations,
            #                                  sprintf(
            #                                      "%s in %s (%s)",
            #                                      level,
            #                                      colnames(residuals)[j],
            #                                      paste(effects, collapse = ", ")
            #                                  ))
            #         }
            #     }
            #
            #     return(paste(interpretations, collapse = "<br>"))
            # }

            # ,
            # .generateSummaryText = function(chi_result,
            #                                 fisher_needed,
            #                                 fisher_result = NULL) {
            #     if (fisher_needed && !is.null(fisher_result)) {
            #         test_type <- "Fisher's Exact Test"
            #         p_value <- fisher_result$p.value
            #     } else {
            #         test_type <- "Chi-square Test"
            #         p_value <- chi_result$p.value
            #     }
            #
            #     if (p_value < 0.05) {
            #         return(
            #             sprintf(
            #                 "The %s shows a significant association (p = %.3f).
            # This suggests the variables are not independent.",
            #                 test_type,
            #                 p_value
            #             )
            #         )
            #     } else {
            #         return(
            #             sprintf(
            #                 "The %s shows no significant association (p = %.3f).
            # This suggests the variables may be independent.",
            #                 test_type,
            #                 p_value
            #             )
            #         )
            #     }
            # }


            )
                )
