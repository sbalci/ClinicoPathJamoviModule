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
#'

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
                    tablegtsummary <- gtsummary::tbl_summary(data = mydata, by = mygroup)
                    tablegtsummary <- gtsummary::as_kable_extra(tablegtsummary)
                    self$results$tablestyle3$setContent(tablegtsummary)
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
