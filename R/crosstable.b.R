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

# Helper function to escape variable names with special characters
.escapeVariableNames <- function(var_names) {
    # Check if variable names contain special characters that need escaping
    need_escaping <- grepl("[^a-zA-Z0-9._]", var_names)
    var_names[need_escaping] <- paste0("`", var_names[need_escaping], "`")
    return(var_names)
}

# Helper function to get display name from mapping
.getDisplayName <- function(cleaned_name, name_mapping) {
    # Get original display name from mapping
    original_name <- name_mapping[[cleaned_name]]
    if (is.null(original_name)) {
        return(cleaned_name)  # Fallback to cleaned name
    }
    return(original_name)
}

# Helper function to validate variable names and detect issues
.validateVariableNames <- function(original_names, cleaned_names) {
    issues <- list()
    warnings <- list()

    # Check for duplicate names after cleaning
    duplicated_cleaned <- duplicated(cleaned_names) | duplicated(cleaned_names, fromLast = TRUE)
    if (any(duplicated_cleaned)) {
        duplicate_originals <- original_names[duplicated_cleaned]
        duplicate_cleaned <- unique(cleaned_names[duplicated_cleaned])

        issues <- append(issues, paste0(
            "Duplicate variable names after cleaning: ",
            paste(duplicate_originals, collapse = ", "),
            " → ", paste(duplicate_cleaned, collapse = ", ")
        ))
    }

    # Check for very long names that might be truncated
    long_names <- original_names[nchar(original_names) > 50]
    if (length(long_names) > 0) {
        warnings <- append(warnings, paste0(
            "Very long variable names detected (>50 characters): ",
            paste(substring(long_names, 1, 30), "...", sep = "", collapse = ", ")
        ))
    }

    # Check for special characters that needed escaping
    needs_escaping <- grepl("[^a-zA-Z0-9._]", original_names)
    if (any(needs_escaping)) {
        special_names <- original_names[needs_escaping]
        warnings <- append(warnings, paste0(
            "Variable names with special characters detected: ",
            paste(special_names, collapse = ", ")
        ))
    }

    return(list(issues = issues, warnings = warnings))
}

# Helper function for intelligent test selection with clinical rationale
.selectAppropriateTest <- function(contingency_table, test_preference = "auto", min_expected = 5) {
    # Calculate expected frequencies
    tryCatch({
        chi_test <- chisq.test(contingency_table)
        expected_min <- min(chi_test$expected)
        
        if (test_preference == "auto") {
            if (expected_min < min_expected) {
                return(list(
                    test = "fisher", 
                    reason = paste0("Expected counts < ", min_expected, ". Fisher's exact test recommended."),
                    warning = TRUE,
                    expected_min = expected_min
                ))
            } else {
                return(list(
                    test = "chisq", 
                    reason = paste0("All expected counts ≥ ", min_expected, ". Chi-square test appropriate."),
                    warning = FALSE,
                    expected_min = expected_min
                ))
            }
        }
        
        # User specified test - check if appropriate
        if (test_preference == "chisq" && expected_min < min_expected) {
            return(list(
                test = test_preference,
                reason = paste0("Chi-square test as requested (Note: Expected counts < ", min_expected, ")"),
                warning = TRUE,
                expected_min = expected_min
            ))
        }
        
        return(list(
            test = test_preference, 
            reason = .("Test selected as requested"),
            warning = FALSE,
            expected_min = expected_min
        ))
    }, error = function(e) {
        return(list(
            test = "fisher", 
            reason = .("Cannot calculate expected frequencies. Using Fisher's exact test."),
            warning = TRUE,
            expected_min = NA
        ))
    })
}

# Helper function to validate sample size and data quality
.validateAnalysisAssumptions <- function(mydata, myvars, mygroup) {
    issues <- list()
    warnings <- list()
    
    # Check overall sample size
    n_total <- nrow(mydata)
    if (n_total < 20) {
        issues <- append(issues, sprintf(.("Very small sample size (n = %d). Results may be unreliable."), n_total))
    }
    
    # Check group sizes
    if (!is.null(mygroup) && mygroup %in% names(mydata)) {
        group_sizes <- table(mydata[[mygroup]])
        min_group_size <- min(group_sizes)
        
        if (min_group_size < 5) {
            warnings <- append(warnings, paste0("Small group detected (n = ", min_group_size, "). Consider combining categories or using exact tests."))
        }
        
        # Check for empty cells in cross-tabulations
        for (var in myvars) {
            if (var %in% names(mydata)) {
                cont_table <- table(mydata[[var]], mydata[[mygroup]])
                if (any(cont_table == 0)) {
                    warnings <- append(warnings, paste0("Empty cells detected in ", var, " × ", mygroup, " table. Results may be unstable."))
                }
            }
        }
    }
    
    # Check for excessive missing data
    for (var in c(myvars, mygroup)) {
        if (var %in% names(mydata)) {
            missing_pct <- mean(is.na(mydata[[var]])) * 100
            if (missing_pct > 20) {
                warnings <- append(warnings, paste0("High missing data in ", var, " (", round(missing_pct, 1), "%). Consider imputation or sensitivity analysis."))
            }
        }
    }
    
    return(list(
        critical_issues = issues,
        warnings = warnings,
        sample_size = n_total
    ))
}

# Helper function to generate clinical interpretation
.generateClinicalSummary <- function(results, myvars, mygroup, test_type = "crosstable") {
    if (is.null(results) || length(results) == 0) {
        return(.("No results available for interpretation."))
    }
    
    # Count significant associations (assuming p-value column exists)
    significant_count <- 0
    total_tests <- length(myvars)
    
    if (test_type == "crosstable") {
        summary <- sprintf(.("Cross-table analysis comparing %d variable(s) across %s groups."),
                          total_tests, mygroup)

        if (significant_count > 0) {
            summary <- paste0(summary, " ",
                sprintf(.("Found %d significant association(s) (p < 0.05)."), significant_count))
        } else {
            summary <- paste0(summary, " ",
                .("No significant associations detected (all p ≥ 0.05)."))
        }

        summary <- paste0(summary, " ",
            .("Review individual test results below for detailed findings."))
    }
    
    return(summary)
}

crosstableClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "crosstableClass",
        inherit = crosstableBase,
        private = list(
            # .labelData ----
            # Prepare data by cleaning names and setting original labels with robust handling.
            .labelData = function() {
                mydata <- self$data
                original_names <- names(mydata)

                # Validate variable names and report issues
                cleaned_names_preview <- janitor::make_clean_names(original_names)
                validation_results <- .validateVariableNames(original_names, cleaned_names_preview)

                # Report any critical issues
                if (length(validation_results$issues) > 0) {
                    stop(paste("Variable name issues detected:",
                              paste(validation_results$issues, collapse = "; ")))
                }

                # Clean variable names using janitor
                mydata <- mydata %>% janitor::clean_names()
                cleaned_names <- names(mydata)

                # Create bidirectional mappings for robust variable handling
                # original_names_mapping: cleaned_name -> original_name
                original_names_mapping <- setNames(original_names, cleaned_names)
                # cleaned_names_mapping: original_name -> cleaned_name
                cleaned_names_mapping <- setNames(cleaned_names, original_names)

                # Apply labels to preserve original names
                mydata <- labelled::set_variable_labels(
                    .data = mydata,
                    .labels = original_names_mapping
                )

                # Retrieve all variable labels
                all_labels <- labelled::var_label(mydata)

                # Robust variable matching with error handling
                tryCatch({
                    # Match user-specified variables to cleaned names
                    user_vars <- self$options$vars
                    if (length(user_vars) > 0) {
                        matched_indices <- match(user_vars, all_labels)
                        if (any(is.na(matched_indices))) {
                            missing_vars <- user_vars[is.na(matched_indices)]
                            warning(paste("Could not find variables:", paste(missing_vars, collapse = ", ")))
                        }
                        myvars <- names(all_labels)[matched_indices[!is.na(matched_indices)]]
                    } else {
                        myvars <- character(0)
                    }

                    # Match grouping variable
                    if (!is.null(self$options$group) && self$options$group != "") {
                        group_match <- which(all_labels == self$options$group)
                        if (length(group_match) > 0) {
                            mygroup <- names(all_labels)[group_match[1]]  # Take first match
                        } else {
                            warning(paste("Could not find grouping variable:", self$options$group))
                            mygroup <- character(0)
                        }
                    } else {
                        mygroup <- character(0)
                    }

                }, error = function(e) {
                    stop(paste("Variable matching failed:", e$message))
                })

                # Report warnings about variable names if any
                if (length(validation_results$warnings) > 0) {
                    warning_msg <- paste0(
                        "<div style='background-color: #fff3cd; padding: 10px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #ffc107;'>",
                        "<strong>Variable Name Warnings:</strong><br>",
                        paste(validation_results$warnings, collapse = "<br>"),
                        "</div>"
                    )
                    # Note: Store warning for later display in results
                    # TEMPORARILY DISABLED - assumptions results element not defined in .r.yaml
                    # if (exists("self") && "results" %in% names(self) && "assumptions" %in% names(self$results)) {
                    #     self$results$assumptions$setContent(warning_msg)
                    # }
                }

                return(list(
                    "mydata" = mydata,
                    "myvars" = myvars,
                    "mygroup" = mygroup,
                    "original_names_mapping" = original_names_mapping,
                    "cleaned_names_mapping" = cleaned_names_mapping
                ))
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
                    stop(.("The dataset contains no complete rows. Please check your data."))
                
                # Performance safeguards for large datasets
                n_rows <- nrow(self$data)
                n_vars <- length(self$options$vars)
                n_combinations <- n_vars * length(unique(self$data[[self$options$group]]))
                
                if (n_rows > 50000) {
                    warning(paste(.("Large dataset detected:"), n_rows, .("rows. Analysis may take longer.")))
                }
                
                if (n_combinations > 100) {
                    warning(paste(.("Large number of variable combinations:"), n_combinations, .("Consider reducing variables.")))
                }

                # Read and label data with robust variable name handling.
                cleaneddata <- private$.labelData()
                mydata <- cleaneddata$mydata
                myvars <- cleaneddata$myvars
                mygroup <- cleaneddata$mygroup
                original_names_mapping <- cleaneddata$original_names_mapping
                cleaned_names_mapping <- cleaneddata$cleaned_names_mapping

                # Build formula using escaped variable names for safety.
                escaped_myvars <- .escapeVariableNames(myvars)
                escaped_mygroup <- .escapeVariableNames(mygroup)
                formula <- jmvcore::constructFormula(terms = escaped_myvars, dep = escaped_mygroup)
                formula <- as.formula(formula)

                # Exclude missing data if requested.
                if (self$options$excl)
                    mydata <- jmvcore::naOmit(mydata)
                
                # Validate analysis assumptions and data quality
                validation_results <- .validateAnalysisAssumptions(mydata, myvars, mygroup)
                if (length(validation_results$warnings) > 0) {
                    warning_text <- paste("<strong>Data Quality Warnings:</strong><br>",
                                        paste(validation_results$warnings, collapse = "<br>"))
                    # TEMPORARILY DISABLED - assumptions results element not defined in .r.yaml
                    # self$results$assumptions$setContent(warning_text)
                }

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
                        caption = paste0("Cross Table for Dependent ", .getDisplayName(mygroup, original_names_mapping)),
                        id = "tbl3"
                    )
                    self$results$tablestyle4$setContent(tabletangram)
                }
                
                # TEMPORARILY DISABLED - pairwise and advanced options not available in .a.yaml
                # Pairwise comparisons section
                # if (self$options$pairwise && !is.null(self$options$group) && !is.null(self$options$vars)) {
                #     private$.performPairwiseComparisons()
                # }

                # Advanced post-hoc analysis section
                # if (self$options$posthoc_method != "none" || self$options$effect_size_measures ||
                #     self$options$residual_analysis || self$options$correspondence_analysis) {
                #     private$.performAdvancedPosthoc()
                # }
                
                # # Generate clinical summary with natural language interpretation
                # tryCatch({
                #     clinical_summary <- private$.generateClinicalSummary(
                #         results = list(data = mydata, vars = myvars, group = mygroup),
                #         myvars = myvars,
                #         mygroup = mygroup,
                #         test_type = "crosstable"
                #     )
                #     
                #     # Add clinical summary to results
                #     if (!is.null(clinical_summary)) {
                #         summary_html <- paste0(
                #             "<div style='background-color: #f8f9fa; padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #28a745;'>",
                #             "<h4 style='margin-top: 0; color: #28a745;'>Clinical Summary</h4>",
                #             clinical_summary,
                #             "</div>"
                #         )
                #         self$results$clinicalSummary$setContent(summary_html)
                #         
                #         # Also populate the copy-ready sentences
                #         copy_ready <- paste0(
                #             "<div style='background-color: #fff3cd; padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #ffc107;'>",
                #             "<h4 style='margin-top: 0; color: #856404;'>Copy-Ready Clinical Interpretation</h4>",
                #             "<p>", gsub("<[^>]*>", "", clinical_summary), "</p>",
                #             "</div>"
                #         )
                #         self$results$reportSentences$setContent(copy_ready)
                #     }
                # }, error = function(e) {
                #     # Handle clinical summary generation errors gracefully
                #     detailed_error <- paste(.("Clinical summary generation failed:"), e$message)
                #     warning(detailed_error)
                #     
                #     # Provide fallback guidance
                #     fallback_msg <- paste0(
                #         "<div style='background-color: #fff3cd; padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #ffc107;'>",
                #         "<h4 style='margin-top: 0; color: #856404;'>", .("Manual Interpretation Required"), "</h4>",
                #         "<p>", .("Automatic clinical summary could not be generated. Please review results manually."), "</p>",
                #         "</div>"
                #     )
                #     self$results$reportSentences$setContent(fallback_msg)
                # })

            },
            
            # TEMPORARILY DISABLED - pairwise option not available in .a.yaml
            # .performPairwiseComparisons ----
            # .performPairwiseComparisons = function() {
                # # Get the labeled data
                # # labelData <- private$.labelData()
                # mydata <- labelData$mydata
                # myvars <- labelData$myvars
                # mygroup <- labelData$mygroup
                # 
                # # Check if we have a valid group variable
                # if (length(mygroup) == 0 || !(mygroup %in% names(mydata))) {
                #     return()
                # }
                # 
                # # Get the group variable data
                # group_var <- mydata[[mygroup]]
                # 
                # # Check if group has more than 2 levels
                # if (!is.factor(group_var)) {
                #     group_var <- as.factor(group_var)
                # }
                # 
                # group_levels <- levels(group_var)
                # n_groups <- length(group_levels)
                # 
                # if (n_groups <= 2) {
                #     note <- paste0(
                #         "<div style='background-color: #fff3cd; padding: 10px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #ffc107;'>",
                #         "<strong>Note:</strong> Pairwise comparisons are only performed when the grouping variable has more than 2 levels. ",
                #         "Your grouping variable has ", n_groups, " level(s).",
                #         "</div>"
                #     )
                #     self$results$pairwiseNote$setContent(note)
                #     return()
                # }
                # 
                # # Perform pairwise comparisons for each variable
                # results_list <- list()
                # test_type <- self$options$pcat
                # 
                # for (var in myvars) {
                #     if (!(var %in% names(mydata))) next
                #     
                #     var_data <- mydata[[var]]
                #     
                #     # Skip if not categorical
                #     if (!is.factor(var_data) && !is.character(var_data)) next
                #     
                #     # Generate all pairwise combinations
                #     pairs <- utils::combn(group_levels, 2)
                #     
                #     for (i in 1:ncol(pairs)) {
                #         group1 <- pairs[1, i]
                #         group2 <- pairs[2, i]
                #         
                #         # Subset data for this pair
                #         subset_idx <- group_var %in% c(group1, group2)
                #         subset_var <- var_data[subset_idx]
                #         subset_group <- group_var[subset_idx]
                #         
                #         # Create contingency table
                #         cont_table <- table(subset_var, subset_group)
                #         
                #         # Perform test
                #         test_result <- NULL
                #         # Use intelligent test selection with clinical rationale
                #         test_selection <- private$.selectAppropriateTest(
                #             cont_table,
                #             test_preference = if (test_type == "fisher") "fisher" else "auto",
                #             min_expected = 5
                #         )
                #         
                #         test_name <- test_selection$test
                #         test_rationale <- test_selection$reason
                #         
                #         # Perform the selected test
                #         if (test_selection$test == "fisher") {
                #             tryCatch({
                #                 test_result <- fisher.test(cont_table)
                #                 test_name <- .("Fisher's Exact")
                #             }, error = function(e) {
                #                 test_result <- NULL
                #             })
                #         } else {
                #             tryCatch({
                #                 test_result <- chisq.test(cont_table)
                #                 test_name <- .("Chi-square")
                #             }, error = function(e) {
                #                 test_result <- NULL
                #             })
                #         }
                #         
                #         # Add warning if test selection was based on assumption violations
                #         if (test_selection$warning) {
                #             warning_msg <- paste(.("Test selection warning:"), test_rationale)
                #             # Store warning for later display
                #             if (is.null(self$results$assumptions$content)) {
                #                 assumption_warnings <- warning_msg
                #             } else {
                #                 assumption_warnings <- paste(self$results$assumptions$content, "<br>", warning_msg)
                #             }
                #         }
                #         
                #         if (!is.null(test_result)) {
                #             results_list <- append(results_list, list(list(
                #                 variable = var,
                #                 comparison = paste0(var, ": ", group1, " vs ", group2),
                #                 group1 = group1,
                #                 group2 = group2,
                #                 test = test_name,
                #                 statistic = if (test_name == "Chi-square") test_result$statistic else NA,
                #                 df = if (test_name == "Chi-square") test_result$parameter else NA,
                #                 p_value = test_result$p.value
                #             )))
                #         }
                #     }
                # }
                # 
                # # Apply p-value adjustment if requested
                # if (length(results_list) > 0) {
                #     p_values <- sapply(results_list, function(x) x$p_value)
                #     
                #     if (self$options$p_adjust != "none") {
                #         adjusted_p <- stats::p.adjust(p_values, method = self$options$p_adjust)
                #         for (i in seq_along(results_list)) {
                #             results_list[[i]]$p_adjusted <- adjusted_p[i]
                #         }
                #     } else {
                #         for (i in seq_along(results_list)) {
                #             results_list[[i]]$p_adjusted <- NA
                #         }
                #     }
                #     
                #     # Populate the table
                #     for (result in results_list) {
                #         self$results$pairwiseTable$addRow(
                #             rowKey = paste0(result$comparison, "_", result$test),
                #             values = list(
                #                 comparison = result$comparison,
                #                 group1 = result$group1,
                #                 group2 = result$group2,
                #                 test = result$test,
                #                 statistic = result$statistic,
                #                 df = result$df,
                #                 p_value = result$p_value,
                #                 p_adjusted = result$p_adjusted
                #             )
                #         )
                #     }
                #     
                #     # Add explanatory note
                #     note <- paste0(
                #         "<div style='background-color: #e8f4fd; padding: 10px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #2196F3;'>",
                #         "<strong>Pairwise Comparisons:</strong> All possible pairs of groups have been compared. ",
                #         "Total comparisons: ", length(results_list), ". ",
                #         if (self$options$p_adjust != "none") {
                #             paste0("P-values have been adjusted using the ", self$options$p_adjust, " method to control for multiple testing.")
                #         } else {
                #             "P-values are not adjusted for multiple comparisons."
                #         },
                #         "</div>"
                #     )
                # #     self$results$pairwiseNote$setContent(note)
                # # }
            # },
            
            # TEMPORARILY DISABLED - advanced post-hoc options not available in .a.yaml
            # Advanced Post-hoc Testing Methods
            # .performAdvancedPosthoc = function() {
                # # Get the labeled data
                # labelData <- private$.labelData()
                # mydata <- labelData$mydata
                # myvars <- labelData$myvars
                # mygroup <- labelData$mygroup
                # 
                # # Perform analyses for each variable
                # for (var in myvars) {
                #     # Create contingency table
                #     contingency_table <- table(mydata[[var]], mydata[[mygroup]])
                #     
                #     # Skip if table is empty or has single dimension
                #     if (length(contingency_table) == 0 || length(dim(contingency_table)) < 2) {
                #         next
                #     }
                #     
                #     # Effect size measures
                #     if (self$options$effect_size_measures) {
                #         private$.calculateEffectSizes(var, contingency_table)
                #     }
                #     
                #     # Residual analysis
                #     if (self$options$residual_analysis) {
                #         private$.performResidualAnalysis(var, contingency_table)
                #     }
                #     
                #     # Advanced post-hoc statistical tests
                #     if (self$options$posthoc_method != "none") {
                #         private$.performPosthocTests(var, contingency_table)
                #     }
                #     
                #     # Correspondence analysis
                #     if (self$options$correspondence_analysis) {
                #         private$.performCorrespondenceAnalysis(var, contingency_table)
                #     }
                # }
                # 
                # # TEMPORARILY DISABLED - plot options not available in .a.yaml
                # # Generate visualizations
                # # if (self$options$mosaic_plot) {
                # #     private$.prepareMosaicPlot()
                # # }

                # # if (self$options$correspondence_analysis) {
                # #     private$.prepareCorrespondencePlot()
                # # }
            # },
            
            # .calculateEffectSizes = function(var, contingency_table) {
                # # Calculate various effect size measures
                # n <- sum(contingency_table)
                #
                # # Cramér's V
                # chi_square <- chisq.test(contingency_table)$statistic
                # k <- min(nrow(contingency_table), ncol(contingency_table))
                # cramers_v <- sqrt(chi_square / (n * (k - 1)))
                # 
                # # Phi coefficient (for 2x2 tables)
                # phi <- sqrt(chi_square / n)
                # 
                # # Calculate confidence intervals (using bootstrap approximation)
                # conf_level <- self$options$confidence_level
                # 
                # # Add Cramér's V
                # self$results$effectsizes$addRow(
                #     rowKey = paste0(var, "_cramers_v"),
                #     values = list(
                #         variable = var,
                #         measure = .("Cramér's V"),
                #         value = cramers_v,
                #         ci_lower = max(0, cramers_v - 1.96 * sqrt(cramers_v / n)),
                #         ci_upper = min(1, cramers_v + 1.96 * sqrt(cramers_v / n)),
                #         interpretation = private$.interpretEffectSize(cramers_v, "cramers_v")
                #     )
                # )
                # 
                # # Add Phi coefficient for 2x2 tables
                # if (nrow(contingency_table) == 2 && ncol(contingency_table) == 2) {
                #     self$results$effectsizes$addRow(
                #         rowKey = paste0(var, "_phi"),
                #         values = list(
                #             variable = var,
                #             measure = .("Phi Coefficient"),
                #             value = phi,
                #             ci_lower = max(-1, phi - 1.96 * sqrt(phi / n)),
                #             ci_upper = min(1, phi + 1.96 * sqrt(phi / n)),
                #             interpretation = private$.interpretEffectSize(phi, "phi")
                #         )
                #     )
                # # }
            # },
            
            # .performResidualAnalysis = function(var, contingency_table) {
                # # Calculate standardized and adjusted residuals
                # chi_test <- chisq.test(contingency_table)
                # expected <- chi_test$expected
                # observed <- contingency_table
                # 
                # # Standardized residuals
                # std_residuals <- (observed - expected) / sqrt(expected)
                # 
                # # Adjusted residuals
                # row_totals <- rowSums(observed)
                # col_totals <- colSums(observed)
                # n <- sum(observed)
                # 
                # adj_residuals <- matrix(0, nrow = nrow(observed), ncol = ncol(observed))
                # 
                # for (i in 1:nrow(observed)) {
                #     for (j in 1:ncol(observed)) {
                #         variance <- expected[i,j] * (1 - row_totals[i]/n) * (1 - col_totals[j]/n)
                #         adj_residuals[i,j] <- (observed[i,j] - expected[i,j]) / sqrt(variance)
                #         
                #         # Calculate contribution to chi-square
                #         contribution <- ((observed[i,j] - expected[i,j])^2 / expected[i,j]) / chi_test$statistic * 100
                #         
                #         self$results$residuals$addRow(
                #             rowKey = paste0(var, "_", i, "_", j),
                #             values = list(
                #                 variable = var,
                #                 group_level = colnames(contingency_table)[j],
                #                 var_level = rownames(contingency_table)[i],
                #                 observed = observed[i,j],
                #                 expected = expected[i,j],
                #                 std_residual = std_residuals[i,j],
                #                 adj_residual = adj_residuals[i,j],
                #                 contribution = contribution
                #             )
                #         )
                #     }
                # # }
            # },

            # .performPosthocTests = function(var, contingency_table) {
                # method <- self$options$posthoc_method
                # 
                # if (method == "standardized_residuals") {
                #     # Test for significant standardized residuals
                #     chi_test <- chisq.test(contingency_table)
                #     
                #     self$results$posthocstats$addRow(
                #         rowKey = paste0(var, "_std_residuals"),
                #         values = list(
                #             variable = var,
                #             method = .("Standardized Residuals"),
                #             statistic = chi_test$statistic,
                #             df = chi_test$parameter,
                #             p_value = chi_test$p.value,
                #             interpretation = ifelse(chi_test$p.value < 0.05,
                #                 "Significant departure from independence",
                #                 "No significant departure from independence")
                #         )
                #     )
                #     
                # } else if (method == "cramers_v") {
                #     # Test significance of Cramér's V
                #     chi_test <- chisq.test(contingency_table)
                #     n <- sum(contingency_table)
                #     k <- min(nrow(contingency_table), ncol(contingency_table))
                #     cramers_v <- sqrt(chi_test$statistic / (n * (k - 1)))
                #     
                #     self$results$posthocstats$addRow(
                #         rowKey = paste0(var, "_cramers_v_test"),
                #         values = list(
                #             variable = var,
                #             method = .("Cramér's V Test"),
                #             statistic = cramers_v,
                #             df = chi_test$parameter,
                #             p_value = chi_test$p.value,
                #             interpretation = private$.interpretEffectSize(cramers_v, "cramers_v")
                #         )
                #     )
                #     
                # } else if (method == "freeman_halton") {
                #     # Freeman-Halton extension of Fisher's exact test
                #     if (self$options$exact_tests) {
                #         tryCatch({
                #             if (requireNamespace("RVAideMemoire", quietly = TRUE)) {
                #                 fh_test <- RVAideMemoire::fisher.multcomp(contingency_table)
                #                 
                #                 self$results$posthocstats$addRow(
                #                     rowKey = paste0(var, "_freeman_halton"),
                #                     values = list(
                #                         variable = var,
                #                         method = .("Freeman-Halton Extension"),
                #                         statistic = NA,
                #                         df = NA,
                #                         p_value = fh_test$p.value,
                #                         interpretation = ifelse(fh_test$p.value < 0.05,
                #                             "Significant association (exact test)",
                #                             "No significant association (exact test)")
                #                     )
                #                 )
                #             }
                #         }, error = function(e) {
                #             # Fallback to regular Fisher's test
                #             fisher_test <- fisher.test(contingency_table, simulate.p.value = TRUE)
                #             self$results$posthocstats$addRow(
                #                 rowKey = paste0(var, "_fisher_simulated"),
                #                 values = list(
                #                     variable = var,
                #                     method = .("Fisher's Exact (Simulated)"),
                #                     statistic = NA,
                #                     df = NA,
                #                     p_value = fisher_test$p.value,
                #                     interpretation = ifelse(fisher_test$p.value < 0.05,
                #                         "Significant association (simulated)",
                #                         "No significant association (simulated)")
                #                 )
                #             )
                #         })
                #     }
                # # }
            # },

            # .performCorrespondenceAnalysis = function(var, contingency_table) {
                # tryCatch({
                #     if (requireNamespace("ca", quietly = TRUE)) {
                #         ca_result <- ca::ca(contingency_table)
                #         
                #         # Extract eigenvalues and variance explained
                #         eigenvalues <- ca_result$sv^2
                #         total_inertia <- sum(eigenvalues)
                #         variance_explained <- eigenvalues / total_inertia * 100
                #         cumulative_variance <- cumsum(variance_explained)
                #         
                #         # Add results to table
                #         for (i in 1:min(length(eigenvalues), 3)) {  # Show first 3 dimensions
                #             self$results$correspondenceanalysis$addRow(
                #                 rowKey = paste0(var, "_dim", i),
                #                 values = list(
                #                     dimension = paste(.("Dimension"), i),
                #                     eigenvalue = eigenvalues[i],
                #                     variance_explained = variance_explained[i],
                #                     cumulative_variance = cumulative_variance[i],
                #                     inertia = eigenvalues[i] / total_inertia
                #                 )
                #             )
                #         }
                #     }
                # }, error = function(e) {
                #     # Handle CA errors gracefully
                # # })
            # },

            # .prepareMosaicPlot = function() {
                # # Prepare data for mosaic plot
                # labelData <- private$.labelData()
                # plot_data <- list(
                #     data = labelData$mydata,
                #     vars = labelData$myvars,
                #     group = labelData$mygroup
                # )
                # 
                # # self$results$mosaicplot$setState(plot_data)
            # },

            # .prepareCorrespondencePlot = function() {
                # # Prepare data for correspondence analysis plot
                # labelData <- private$.labelData()
                # plot_data <- list(
                #     data = labelData$mydata,
                #     vars = labelData$myvars,
                #     group = labelData$mygroup
                # )
                # 
                # # self$results$correspondenceplot$setState(plot_data)
            # },

            # .interpretEffectSize = function(value, type) {
                # if (type == "cramers_v") {
                #     if (value < 0.1) return("Negligible")
                #     else if (value < 0.3) return("Small")
                #     else if (value < 0.5) return("Medium")
                #     else return("Large")
                # } else if (type == "phi") {
                #     if (abs(value) < 0.1) return("Negligible")
                #     else if (abs(value) < 0.3) return("Small")
                #     else if (abs(value) < 0.5) return("Medium")
                #     else return("Large")
                # }
                # # return("Unknown")
            # },

            # Plot rendering functions
            # .mosaicplot = function(image, ggtheme, theme, ...) {
                # if (is.null(image$state)) return(FALSE)
                # 
                # plot_data <- image$state$data
                # vars <- image$state$vars
                # group <- image$state$group
                # 
                # tryCatch({
                #     if (requireNamespace("vcd", quietly = TRUE) && length(vars) >= 1) {
                #         # Create mosaic plot using vcd package
                #         var <- vars[1]  # Use first variable
                #         
                #         # Create contingency table
                #         tbl <- table(plot_data[[var]], plot_data[[group]])
                #         
                #         # Generate mosaic plot
                #         vcd::mosaic(tbl, 
                #                main = paste(.("Mosaic Plot:"), var, .("by"), group),
                #                shade = TRUE,
                #                legend = TRUE)
                #         return(TRUE)
                #     }
                # }, error = function(e) {
                #     # Fallback: create a simple bar plot
                #     if (length(vars) >= 1) {
                #         var <- vars[1]
                #         tbl <- table(plot_data[[var]], plot_data[[group]])
                #         barplot(tbl, 
                #                main = paste(.("Association:"), var, .("by"), group),
                #                beside = TRUE,
                #                legend = TRUE,
                #                col = rainbow(nrow(tbl)))
                #         return(TRUE)
                #     }
                # })
                # 
                # # return(FALSE)
            # },

            # .correspondenceplot = function(image, ggtheme, theme, ...) {
                # if (is.null(image$state)) return(FALSE)
                #
                # plot_data <- image$state$data
                # vars <- image$state$vars
                # group <- image$state$group
                #
                # tryCatch({
                #     if (requireNamespace("ca", quietly = TRUE) && length(vars) >= 1) {
                #         var <- vars[1]  # Use first variable
                #
                #         # Create contingency table
                #         tbl <- table(plot_data[[var]], plot_data[[group]])
                #
                #         # Perform correspondence analysis
                #         ca_result <- ca::ca(tbl)
                #
                #         # Create biplot
                #         plot(ca_result,
                #              main = paste(.("Correspondence Analysis:"), var, .("by"), group))
                #         return(TRUE)
                #     }
                # }, error = function(e) {
                #     # Handle errors gracefully
                # })
                #
                # # return(FALSE)
            # },

            # Dummy placeholder for last item in private list
            .dummy = function() {
                # This is a placeholder to maintain proper R6 syntax
                return(invisible(NULL))
            }
        )
    )
