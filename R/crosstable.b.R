#' @title Cross Table for Clinicopathological Comparisons
#'
#' @description
#' This function generates cross tables comparing a dependent variable (rows)
#' with a grouping variable (columns) and automatically selects hypothesis tests
#' appropriate for clinical research. The output tables are rendered in various
#' styles (e.g., arsenal, finalfit, gtsummary, NEJM, Lancet, hmisc) and are intended
#' for pathologists and oncologists. In addition to visualizing associations,
#' this function now optionally provides an exportable CSV version of the cross table.
#'
#' @details
#' The function cleans variable names and applies original labels. It then builds
#' a formula based on the cleaned data and performs the appropriate statistical
#' test (e.g. chi-square or Fisher’s exact test). Detailed user guidance is provided
#' via HTML to-do messages.
#'
#' @param data A data frame containing the study data.
#' @param vars A set of variables used as the dependent variables (rows).
#' @param group A variable (factor) used as the grouping variable (columns).
#' @param sty A string indicating the desired table style.
#'            Options include: "arsenal", "finalfit", "gtsummary", "nejm", "lancet", "hmisc".
#' @param excl Logical. If TRUE, rows with missing values will be excluded.
#' @param cont A string ("mean" or "median") to specify the central tendency metric.
#' @param pcat A string ("chisq" or "fisher") to specify the primary test.
#'
#' @return The function produces an HTML table output, and if requested, an additional
#' downloadable CSV export.
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom gtsummary tbl_summary modify_header add_n add_overall bold_labels add_p add_q bold_levels bold_p all_continuous all_categorical all_stat_cols style_pvalue as_kable_extra
#' @importFrom gt md
#' @importFrom purrr partial
#' @import magrittr


crosstableClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "crosstableClass",
        inherit = crosstableBase,
        private = list(
            # .labelData ----
            # Prepare data by cleaning names and setting original labels.
            .labelData = function() {
                mydata <- self$data
                original_names <- names(mydata)
                # Save original names as labels.
                labels <- setNames(original_names, original_names)
                # Clean variable names.
                mydata <- mydata %>% janitor::clean_names()
                # Create a mapping of cleaned names to original names.
                corrected_labels <- setNames(original_names, names(mydata))
                # Apply the labels.
                mydata <- labelled::set_variable_labels(.data = mydata, .labels = corrected_labels)
                # Retrieve all labels.
                all_labels <- labelled::var_label(mydata)
                # Match the user-specified variables and grouping variable.
                myvars <- self$options$vars
                myvars <- names(all_labels)[match(myvars, all_labels)]
                mygroup <- names(all_labels)[all_labels == self$options$group]
                return(list("mydata" = mydata, "myvars" = myvars, "mygroup" = mygroup))
            },


            # .showTestInformation ----
            .showTestInformation = function() {
                test_info <- paste0(
                    "<div style='background-color: #e8f4fd; padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #2196F3;'>",
                    "<h4 style='margin-top: 0; color: #1976D2;'>Q-values and Multiple Testing Correction</h4>",
                    
                    "<p><strong>What are Q-values?</strong><br>",
                    "Q-values represent the False Discovery Rate (FDR) - the expected proportion of false positives among rejected hypotheses when testing multiple variables simultaneously.</p>",
                    
                    "<p><strong>Why use Q-values in this table?</strong><br>",
                    "When comparing multiple variables across groups (as in this cross-table), the chance of finding at least one false positive increases. Q-values control this family-wise error rate.</p>",
                    
                    "<p><strong>Interpretation Guidelines:</strong></p>",
                    "<ul>",
                    "<li><strong>Q < 0.05:</strong> Strong evidence against null hypothesis (5% FDR)</li>",
                    "<li><strong>Q < 0.10:</strong> Moderate evidence (10% FDR) - often acceptable in exploratory research</li>",
                    "<li><strong>Q < 0.20:</strong> Suggestive evidence - warrants further investigation</li>",
                    "</ul>",
                    
                    "<p><strong>Method:</strong> Benjamini-Hochberg FDR correction applied to p-values from gtsummary's automatic test selection (chi-square/Fisher for categorical, t-test/ANOVA for continuous).</p>",
                    
                    "<p><em>💡 Focus on q-values when interpreting results from tables with multiple comparisons to reduce false discoveries.</em></p>",
                    "</div>"
                )
                
                self$results$testInformation$setContent(test_info)
            },

            # .run ----
            .run = function() {
                sty <- self$options$sty
                # If required options are missing, show a welcome message with instructions.
                if (is.null(self$options$vars) || is.null(self$options$group)) {
                    todo <- glue::glue(
                        "<br><br>Welcome to ClinicoPath.<br>
                         This tool creates a Cross Table comparing your selected variables.
                         Please select the dependent variable (rows) and the grouping variable (columns).
                         The appropriate hypothesis tests are chosen automatically.
                         <br>Please ensure data distribution and test appropriateness before use.<br>
                         <hr>"
                    )
                    self$results$todo$setContent(todo)
                    return()
                } else {
                    self$results$todo$setContent("")
                }

                # Provide additional information when using 'finalfit' style.
                if (sty == "finalfit") {
                    todo2 <- glue::glue(
                        "<br>
                         <b>finalfit</b> style uses <em>aov (analysis of variance) or t.test for Welch two sample t-test</em>.
                         For continuous non-parametric tests, Kruskal Wallis is used (equivalent to Mann-Whitney U / Wilcoxon rank sum test in two-group settings).
                         See full documentation <a href='https://finalfit.org/reference/summary_factorlist.html'>here</a>.
                         "
                    )
                } else {
                    todo2 <- ""
                }
                self$results$todo2$setContent(todo2)

                # Check if data has complete rows.
                if (nrow(self$data) == 0)
                    stop("The dataset contains no complete rows. Please check your data.")

                # Read and label data.
                cleaneddata <- private$.labelData()
                mydata <- cleaneddata$mydata
                myvars <- cleaneddata$myvars
                mygroup <- cleaneddata$mygroup

                # Build formula using the cleaned variables.
                formula <- jmvcore::constructFormula(terms = myvars, dep = mygroup)
                formula <- as.formula(formula)

                # Exclude missing data if requested.
                if (self$options$excl)
                    mydata <- jmvcore::naOmit(mydata)

                # Generate table based on selected style.
                if (sty == "arsenal") {
                    tablearsenal <- arsenal::tableby(
                        formula = formula,
                        data = mydata,
                        total = TRUE,
                        digits = 1,
                        digits.count = 1
                    )
                    tablearsenal <- summary(tablearsenal, text = 'html', pfootnote = 'html')
                    tablearsenal <- capture.output(tablearsenal)
                    self$results$tablestyle1$setContent(tablearsenal)
                } else if (sty == "finalfit") {
                    myvars_term <- jmvcore::composeTerm(components = myvars)
                    myvars_term <- jmvcore::decomposeTerm(term = myvars_term)
                    private$.checkpoint()
                    # Create the finalfit summary table.
                    tablefinalfit <- mydata %>%
                        finalfit::summary_factorlist(
                            .data = .,
                            dependent = mygroup,
                            explanatory = myvars_term,
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
                        )
                    tablefinalfit <- kableExtra::kable(
                        tablefinalfit,
                        format = "html",
                        digits = 1,
                        escape = FALSE
                    )
                    self$results$tablestyle2$setContent(tablefinalfit)
                } else if (sty == "gtsummary") {
                    # tablegtsummary <- gtsummary::tbl_summary(data = mydata, by = mygroup)
                    # tablegtsummary <- gtsummary::as_kable_extra(tablegtsummary)
                    # self$results$tablestyle3$setContent(tablegtsummary)



                    # http://www.danieldsjoberg.com/gtsummary/articles/gallery.html


                # Select only the analysis variables and grouping variable
                analysis_vars <- c(myvars, mygroup)
                mydata_subset <- mydata[, analysis_vars, drop = FALSE]
                
                 tablegtsummary <-
  mydata_subset %>%
  tbl_summary(
    by = mygroup,
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{n}/{N} ({p}%)"
    ),
    digits       = all_continuous() ~ 2,
    missing_text = "(Missing)"
  ) %>%
  add_n() %>%
  add_overall() %>%
  add_p(
    # compute p‐values for all variables using gtsummary defaults
    pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = 3)
  ) %>%
  add_q(
    # compute q‐values (Benjamini–Hochberg by default)
    method = "fdr", 
    pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = 3)
  ) %>%
  modify_header(
    # re‐apply your group‐level header
    all_stat_cols() ~ "**{level}**\nN = {n} ({style_percent(p)})",
    # explicitly give p.value and q.value nice titles
    p.value      ~ "**p-value**",
    q.value      ~ "**q-value**"
  ) %>%
  bold_labels()

                tablegtsummary <-
                    gtsummary::as_kable_extra(tablegtsummary)

                self$results$tablestyle3$setContent(tablegtsummary)
                
                # Add q-value explanation
                qvalue_explanation <- paste0(
                    "<div style='background-color: #f0f8ff; padding: 15px; margin-top: 20px; border-radius: 5px; border: 1px solid #4682b4;'>",
                    "<h4 style='margin-top: 0;'>Understanding Q-values (False Discovery Rate)</h4>",
                    "<p><strong>What is a q-value?</strong><br>",
                    "The q-value is the False Discovery Rate (FDR) adjusted p-value. It estimates the proportion of false positives ",
                    "among all significant results when conducting multiple comparisons.</p>",
                    
                    "<p><strong>How to interpret:</strong></p>",
                    "<ul>",
                    "<li>A q-value of 0.05 means that 5% of significant results are expected to be false positives</li>",
                    "<li>Q-values are typically larger than p-values</li>",
                    "<li>Use q < 0.05 (or q < 0.10) as significance threshold for multiple testing</li>",
                    "</ul>",
                    
                    "<p><strong>When to use q-values:</strong></p>",
                    "<ul>",
                    "<li>✅ When testing multiple variables simultaneously (like in this table)</li>",
                    "<li>✅ In exploratory analyses with many comparisons</li>",
                    "<li>✅ In genomic/proteomic studies with thousands of tests</li>",
                    "</ul>",
                    
                    "<p><strong>Important limitations:</strong></p>",
                    "<ul>",
                    "<li>⚠️ Q-values assume all tests are independent (may not be true for correlated variables)</li>",
                    "<li>⚠️ FDR control doesn't guarantee control of Type I error for individual tests</li>",
                    "<li>⚠️ With few tests (<10), q-values may be overly conservative</li>",
                    "<li>⚠️ Should not replace careful hypothesis planning and clinical judgment</li>",
                    "</ul>",
                    
                    "<p><small><em>Method: Benjamini-Hochberg FDR correction as implemented in gtsummary::add_q()</em></small></p>",
                    "</div>"
                )
                
                self$results$qvalueExplanation$setContent(qvalue_explanation)
                
                # Show q-value focused test information automatically
                private$.showTestInformation()
                

                } else if (sty %in% c("nejm", "lancet", "hmisc")) {
                    sty_term <- jmvcore::composeTerm(components = self$options$sty)
                    tabletangram <- tangram::html5(
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
                        style = sty_term,
                        caption = paste0("Cross Table for Dependent ", mygroup),
                        id = "tbl3"
                    )
                    self$results$tablestyle4$setContent(tabletangram)
                }

            }
        )
    )
