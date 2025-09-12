#' @title Summary of Categorical Variables
#' @return Text
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom gtExtras gt_plt_summary
#' @importFrom utils packageVersion
#'
# Improved version of reportcatClass with enhanced messages and formatting
reportcatClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "reportcatClass",
    inherit = reportcatBase,
    private = list(
        .run = function() {

            # Check if any variables have been selected.
            # Enhanced welcome message with HTML formatting for a more user-friendly experience.
            if (length(self$options$vars) == 0) {
                todo <- glue::glue("
        <div style='font-family: Arial, sans-serif; color: #2c3e50;'>
          <h2>{welcome_title}</h2>
          <p>{tool_description}</p>
          <p><strong>{instructions_label}:</strong> {instructions_text}
          {variable_types_note}</p>
          <hr>
        </div>",
        welcome_title = .("Welcome to ClinicoPath"),
        tool_description = .("This tool generates a summary of your selected categorical variables."),
        instructions_label = .("Instructions"),
        instructions_text = .("Please select the Variables you wish to analyze."),
        variable_types_note = .("Only Nominal, Ordinal, or Categorical variables (factors) are allowed.")
        )
                self$results$todo$setContent(todo)
                return()
            } else {
                # Clear the to-do message if variables are selected.
                self$results$todo$setContent("")
                
                # Enhanced input validation with proper error handling
                if (nrow(self$data) == 0) {
                    self$results$error$setContent(glue::glue("<div style='padding: 15px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px; color: #721c24;'><strong>{error_label}:</strong> {error_msg}</div>",
                        error_label = .("Error"),
                        error_msg = .("Data contains no (complete) rows.")))
                    self$results$error$setVisible(TRUE)
                    return()
                }

                mydata <- self$data

                # Construct a formula from the selected variables.
                formula <- jmvcore::constructFormula(terms = self$options$vars)
                myvars <- jmvcore::decomposeFormula(formula = formula)
                myvars <- unlist(myvars)
                
                # Comprehensive validation of selected variables
                if (length(myvars) == 0) {
                    self$results$error$setContent(glue::glue("<div style='padding: 15px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px; color: #721c24;'><strong>{error_label}:</strong> {error_msg}</div>",
                        error_label = .("Error"),
                        error_msg = .("No valid variables selected.")))
                    self$results$error$setVisible(TRUE)
                    return()
                }
                
                # Check for variables that don't exist in the data
                missing_vars <- myvars[!myvars %in% names(mydata)]
                if (length(missing_vars) > 0) {
                    self$results$error$setContent(glue::glue("<div style='padding: 15px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px; color: #721c24;'><strong>{error_label}:</strong> {error_msg}: {vars}.</div>",
                        error_label = .("Error"),
                        error_msg = .("Variables not found in data"),
                        vars = paste(missing_vars, collapse = ", ")))
                    self$results$error$setVisible(TRUE)
                    return()
                }
                
                # Validate that selected variables are actually categorical
                non_categorical <- myvars[!sapply(mydata[myvars], function(x) is.factor(x) || is.character(x))]
                if (length(non_categorical) > 0) {
                    self$results$error$setContent(glue::glue("<div style='padding: 15px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px; color: #721c24;'><strong>{error_label}:</strong> {error_msg}: {vars}. {instruction}</div>",
                        error_label = .("Error"),
                        error_msg = .("Non-categorical variables detected"),
                        vars = paste(non_categorical, collapse = ", "),
                        instruction = .("Please select only categorical (factor or character) variables.")))
                    self$results$error$setVisible(TRUE)
                    return()
                }
                
                # Check for empty factor levels or all-NA variables
                empty_vars <- myvars[sapply(mydata[myvars], function(x) {
                    if (is.factor(x)) {
                        length(levels(x)) == 0 || all(is.na(x))
                    } else {
                        all(is.na(x)) || length(unique(x[!is.na(x)])) == 0
                    }
                })]
                
                if (length(empty_vars) > 0) {
                    warning_msg <- glue::glue("<div style='padding: 15px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 4px; color: #856404;'><strong>{warning_label}:</strong> {warning_msg}: {vars}. {action}</div>",
                        warning_label = .("Warning"),
                        warning_msg = .("Variables with no valid levels or all missing values"),
                        vars = paste(empty_vars, collapse = ", "),
                        action = .("These will be excluded from analysis."))
                    self$results$error$setContent(warning_msg)
                    self$results$error$setVisible(TRUE)
                    
                    # Remove empty variables from analysis
                    myvars <- myvars[!myvars %in% empty_vars]
                    if (length(myvars) == 0) {
                        self$results$error$setContent(glue::glue("<div style='padding: 15px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px; color: #721c24;'><strong>{error_label}:</strong> {error_msg}</div>",
                            error_label = .("Error"),
                            error_msg = .("No valid categorical variables remaining after validation.")))
                        return()
                    }
                }

                # Function to generate a summary for a single categorical variable.
                catsummary <- function(myvar) {
                    # Calculate total observations, missing values, and valid (non-missing) count.
                    total_obs <- length(mydata[[myvar]])
                    missing_obs <- sum(is.na(mydata[[myvar]]))
                    valid_obs <- total_obs - missing_obs
                    num_levels <- nlevels(as.factor(mydata[[myvar]]))

                    # Create a summary table for the variable.
                    tbl <- summary(as.factor(mydata[[myvar]]))
                    summar <- data.frame(
                        level = names(tbl),
                        n = as.numeric(tbl),
                        stringsAsFactors = FALSE
                    ) %>%
                        dplyr::filter(level != "NA's") %>%
                        dplyr::arrange(dplyr::desc(n))
                    summar$validtotal <- valid_obs

                    # Build a description for each level showing count and percentage.
                    description <- summar %>%
                        dplyr::mutate(
                            percent = n / validtotal,
                            level_description = glue::glue(
                                .("{level}: n = {n}, {percent} of valid cases. "),
                                level = level,
                                n = n,
                                percent = scales::percent(percent)
                            )
                        ) %>%
                        dplyr::pull(level_description)

                    # Create overall summary sentences with HTML tags for styling.
                    sentence1 <- glue::glue("<strong>{var}</strong> {has_obs} {obs} {observations} {and} {levels} {levels_text}.",
                        var = myvar,
                        has_obs = .("has"),
                        obs = total_obs,
                        observations = .("observations"),
                        and = .("and"),
                        levels = num_levels,
                        levels_text = .("levels"))
                    sentence2 <- glue::glue("{missing_label}: {count}.",
                        missing_label = .("Missing values"),
                        count = missing_obs)
                    full_description <- paste(c(sentence1, description, sentence2), collapse = "<br>")
                    return(full_description)
                }

                # Generate summaries for all selected variables and combine them.
                summaries <- purrr::map(.x = myvars, .f = catsummary)
                summary_text <- paste(summaries, collapse = "<br><br>")
                self$results$text$setContent(summary_text)

                # RESTORED: Use gtExtras as intended - it works with categorical data too
                plot_dataset <- tryCatch({
                    # Primary approach: Use gtExtras with proper categorical data handling
                    cat_vars <- myvars[sapply(mydata[myvars], function(x) is.factor(x) || is.character(x))]
                    
                    if (length(cat_vars) > 0) {
                        clean_data <- mydata[cat_vars]
                        
                        # Convert character to factor for better handling
                        clean_data <- as.data.frame(lapply(clean_data, function(x) {
                            if (is.character(x)) as.factor(x) else x
                        }))
                        
                        # Use gtExtras::gt_plt_summary with proper configuration for categorical data
                        gt_table <- clean_data %>% 
                            gtExtras::gt_plt_summary() %>%
                            gt::tab_header(
                                title = gt::md(glue::glue("**{title}**", title = .("Categorical Variables Summary"))),
                                subtitle = .("Distribution and missing value analysis")
                            ) %>%
                            # Hide irrelevant numeric columns for categorical data (if they exist)
                            {
                                tryCatch({
                                    . %>% gt::cols_hide(columns = dplyr::any_of(c("mean", "sd", "p0", "p25", "p50", "p75", "p100")))
                                }, error = function(e) {
                                    # If cols_hide fails, return table as is
                                    .
                                })
                            } %>%
                            gt::tab_options(
                                table.font.size = 12,
                                heading.title.font.size = 14,
                                heading.subtitle.font.size = 11,
                                table.width = gt::pct(100)
                            )
                        
                        # Convert to HTML using proper gt method
                        html_output <- gt::as_raw_html(gt_table)
                        return(htmltools::HTML(html_output))
                    } else {
                        return(htmltools::HTML(glue::glue("<div style='padding: 15px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px;'><p>{msg}</p></div>", 
                            msg = .("No categorical variables found."))))
                    }
                }, error = function(e) {
                    # Enhanced fallback with better styling
                    tryCatch({
                        return(private$.gtExtras_style_fallback_cat(mydata, myvars))
                    }, error = function(e2) {
                        # Final fallback to simple table
                        return(private$.create_simple_cat_summary_table(mydata, myvars))
                    })
                })
                
                self$results$text1$setContent(plot_dataset)
                
                # Add clinical interpretation
                clinical_interpretation <- private$.generateClinicalInterpretation(myvars, mydata)
                self$results$clinicalSummary$setContent(clinical_interpretation)

                # Add copy-ready report sentences
                report_sentences <- private$.generateReportSentences(myvars, mydata)
                self$results$reportSentences$setContent(report_sentences)

                # Add about section (always visible)
                about_content <- private$.generateAboutContent()
                self$results$aboutAnalysis$setContent(about_content)

                # Add assumptions and data quality guidance
                assumptions_content <- private$.generateAssumptionsContent(myvars, mydata)
                
                # Add misuse detection
                misuse_warnings <- private$.detectMisusePatterns(mydata, myvars)
                if (length(misuse_warnings) > 0) {
                    warning_content <- glue::glue(
                        "<div style='padding: 15px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 4px; color: #856404; margin-top: 10px;'>
                        <strong>{title}:</strong><br>{warnings}</div>",
                        title = .("Statistical Guidance"),
                        warnings = paste(misuse_warnings, collapse = "<br>")
                    )
                    assumptions_content <- paste(assumptions_content, warning_content, sep = "<br>")
                }
                
                self$results$assumptions$setContent(assumptions_content)
            }
        },

        # Simple categorical summary table without resource-intensive operations
        .create_simple_cat_summary_table = function(dataset, var_list) {
            # Filter to categorical/factor variables only
            cat_vars <- var_list[sapply(dataset[var_list], function(x) is.factor(x) || is.character(x))]
            
            if (length(cat_vars) == 0) {
                return(htmltools::HTML(glue::glue("<p>{msg}</p>", 
                    msg = .("No categorical variables available for summary table."))))
            }
            
            # Create simple HTML table
            html <- "<table style='border-collapse: collapse; margin: 10px 0; width: 100%;'>"
            html <- paste0(html, "<tr style='background-color: #f8f9fa;'>")
            html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>", .("Variable"), "</th>")
            html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>", .("Levels"), "</th>")
            html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>", .("N"), "</th>")
            html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>", .("Missing"), "</th>")
            html <- paste0(html, "</tr>")
            
            for (var in cat_vars) {
                data_col <- dataset[[var]]
                
                # Convert to factor if character
                if (is.character(data_col)) {
                    data_col <- factor(data_col)
                }
                
                levels_count <- length(levels(data_col))
                n_valid <- sum(!is.na(data_col))
                n_missing <- sum(is.na(data_col))
                
                html <- paste0(html, "<tr>")
                html <- paste0(html, "<td style='border: 1px solid #ccc; padding: 8px; font-weight: bold;'>", var, "</td>")
                html <- paste0(html, "<td style='border: 1px solid #ccc; padding: 8px; text-align: center;'>", levels_count, "</td>")
                html <- paste0(html, "<td style='border: 1px solid #ccc; padding: 8px; text-align: center;'>", n_valid, "</td>")
                html <- paste0(html, "<td style='border: 1px solid #ccc; padding: 8px; text-align: center;'>", n_missing, "</td>")
                html <- paste0(html, "</tr>")
            }
            
            html <- paste0(html, "</table>")
            return(htmltools::HTML(html))
        },
        
        # Generate clinical interpretation content
        .generateClinicalInterpretation = function(variables, data) {
            n_vars <- length(variables)
            n_patients <- nrow(data)
            
            # Calculate overall data quality metrics
            missing_summary <- sapply(data[variables], function(x) sum(is.na(x)) / length(x) * 100)
            avg_missing <- round(mean(missing_summary), 1)
            
            # High-level clinical interpretation
            interpretation <- glue::glue(
                "<div style='padding: 20px; background-color: #e8f5e8; border-left: 4px solid #28a745; margin: 10px 0;'>
                <h4 style='color: #155724; margin-top: 0;'>{title}</h4>
                <p><strong>{summary}:</strong> {desc}</p>
                <p><strong>{quality}:</strong> {quality_desc}</p>
                <p><strong>{clinical_use}:</strong> {use_desc}</p>
                </div>",
                title = .("Clinical Summary"),
                summary = .("Dataset Overview"),
                desc = glue::glue(.("Analysis of {n} categorical variables from {patients} patients/cases."), 
                                n = n_vars, patients = n_patients),
                quality = .("Data Quality"),
                quality_desc = if (avg_missing < 5) {
                    .("Excellent data completeness (average missing: {rate}%).") %>% 
                    glue::glue(rate = avg_missing)
                } else if (avg_missing < 15) {
                    .("Good data completeness (average missing: {rate}%).") %>% 
                    glue::glue(rate = avg_missing)
                } else {
                    .("Moderate data completeness (average missing: {rate}%). Consider impact on analysis validity.") %>% 
                    glue::glue(rate = avg_missing)
                },
                clinical_use = .("Clinical Applications"),
                use_desc = .("These summaries are suitable for baseline characteristics tables, data quality assessment, and descriptive analysis in clinical research.")
            )
            
            return(interpretation)
        },
        
        # Generate copy-ready clinical report sentences
        .generateReportSentences = function(variables, data) {
            sentences <- c()
            
            for (var in variables) {
                var_data <- data[[var]]
                n_total <- length(var_data)
                n_missing <- sum(is.na(var_data))
                n_valid <- n_total - n_missing
                
                if (n_valid > 0) {
                    # Get frequency distribution
                    freq_table <- table(var_data, useNA = "no")
                    most_common <- names(freq_table)[which.max(freq_table)]
                    most_common_n <- max(freq_table)
                    most_common_pct <- round(most_common_n / n_valid * 100, 1)
                    
                    sentence <- glue::glue(
                        .("For {variable}, the most common category was '{category}' (n = {n}, {percent}% of valid cases)."),
                        variable = var,
                        category = most_common,
                        n = most_common_n,
                        percent = most_common_pct
                    )
                    
                    if (n_missing > 0) {
                        missing_pct <- round(n_missing / n_total * 100, 1)
                        sentence <- paste(sentence, 
                                        glue::glue(.("Missing data: {n} cases ({percent}%)."), 
                                                 n = n_missing, percent = missing_pct))
                    }
                    
                    sentences <- c(sentences, sentence)
                }
            }
            
            if (length(sentences) > 0) {
                report_content <- glue::glue(
                    "<div style='padding: 15px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px;'>
                    <h5 style='color: #495057; margin-top: 0;'>{title}</h5>
                    <div style='font-family: Georgia, serif; line-height: 1.6;'>
                    {content}
                    </div>
                    <small style='color: #6c757d; margin-top: 10px; display: block;'>{note}</small>
                    </div>",
                    title = .("Copy-Ready Clinical Summary"),
                    content = paste(sentences, collapse = "<br><br>"),
                    note = .("Copy these sentences directly into clinical reports or manuscripts.")
                )
            } else {
                report_content <- glue::glue(
                    "<div style='padding: 15px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px;'>
                    <p>{msg}</p>
                    </div>",
                    msg = .("No valid categorical data available for report generation.")
                )
            }
            
            return(report_content)
        },
        
        # Generate about content explaining the analysis
        .generateAboutContent = function() {
            about_content <- glue::glue(
                "<div style='padding: 20px; background-color: #e3f2fd; border-left: 4px solid #2196f3; margin: 10px 0;'>
                <h4 style='color: #1565c0; margin-top: 0;'>{title}</h4>
                
                <h5 style='color: #1976d2;'>{what_title}</h5>
                <p>{what_desc}</p>
                
                <h5 style='color: #1976d2;'>{when_title}</h5>
                <ul>
                <li>{when_1}</li>
                <li>{when_2}</li>
                <li>{when_3}</li>
                <li>{when_4}</li>
                </ul>
                
                <h5 style='color: #1976d2;'>{how_title}</h5>
                <ol>
                <li>{how_1}</li>
                <li>{how_2}</li>
                <li>{how_3}</li>
                </ol>
                
                <h5 style='color: #1976d2;'>{output_title}</h5>
                <p>{output_desc}</p>
                </div>",
                title = .("About Categorical Variable Analysis"),
                what_title = .("What This Analysis Does"),
                what_desc = .("This tool generates comprehensive frequency distributions and descriptive statistics for categorical (nominal/ordinal) variables, including counts, percentages, missing value patterns, and data quality metrics."),
                when_title = .("When to Use This Analysis"),
                when_1 = .("Creating baseline characteristics tables for research papers"),
                when_2 = .("Assessing data quality and completeness before main analysis"),
                when_3 = .("Exploring categorical variable distributions in clinical datasets"),
                when_4 = .("Generating descriptive statistics for pathology or clinical reports"),
                how_title = .("How to Use"),
                how_1 = .("Select categorical variables (factors or text variables) from your dataset"),
                how_2 = .("Review the variable summaries and data quality metrics"),
                how_3 = .("Use the copy-ready sentences for clinical reports if needed"),
                output_title = .("Outputs Provided"),
                output_desc = .("Variable-by-variable summaries with counts and percentages, visual summary table, clinical interpretation, and copy-ready report sentences.")
            )
            
            return(about_content)
        },
        
        # Generate assumptions and data quality content
        .generateAssumptionsContent = function(variables, data) {
            assumptions_content <- glue::glue(
                "<div style='padding: 20px; background-color: #fff3e0; border-left: 4px solid #ff9800; margin: 10px 0;'>
                <h4 style='color: #e65100; margin-top: 0;'>{title}</h4>
                
                <h5 style='color: #f57c00;'>{data_title}</h5>
                <ul>
                <li>{data_1}</li>
                <li>{data_2}</li>
                <li>{data_3}</li>
                </ul>
                
                <h5 style='color: #f57c00;'>{consider_title}</h5>
                <ul>
                <li>{consider_1}</li>
                <li>{consider_2}</li>
                <li>{consider_3}</li>
                <li>{consider_4}</li>
                </ul>
                </div>",
                title = .("Data Quality & Statistical Considerations"),
                data_title = .("Data Requirements"),
                data_1 = .("Variables should be truly categorical (nominal or ordinal)"),
                data_2 = .("Each category should have sufficient sample size for reliable percentages"),
                data_3 = .("Missing data patterns should be examined for potential bias"),
                consider_title = .("Important Considerations"),
                consider_1 = .("Variables with >20 categories may need recoding for analysis"),
                consider_2 = .("Very sparse categories (<5 cases) may need combination"),
                consider_3 = .("High missing data rates (>20%) require careful interpretation"),
                consider_4 = .("Ordinal variables should maintain their natural ordering")
            )
            
            return(assumptions_content)
        },
        
        # Detect common misuse patterns
        .detectMisusePatterns = function(data, variables) {
            warnings <- c()
            
            for (var in variables) {
                var_data <- data[[var]]
                
                # Check for too many levels
                n_levels <- length(unique(var_data[!is.na(var_data)]))
                if (n_levels > 20) {
                    warnings <- c(warnings, glue::glue(
                        .("Variable '{var}' has {n} categories. Consider recoding variables with >20 categories for clearer interpretation."),
                        var = var, n = n_levels
                    ))
                }
                
                # Check for sparse categories
                if (n_levels > 1) {
                    freq_table <- table(var_data, useNA = "no")
                    sparse_categories <- sum(freq_table < 5)
                    if (sparse_categories > 0 && sparse_categories / length(freq_table) > 0.3) {
                        warnings <- c(warnings, glue::glue(
                            .("Variable '{var}' has {n} categories with <5 cases. Consider combining rare categories."),
                            var = var, n = sparse_categories
                        ))
                    }
                }
                
                # Check missing data rate
                missing_rate <- sum(is.na(var_data)) / length(var_data)
                if (missing_rate > 0.2) {
                    warnings <- c(warnings, glue::glue(
                        .("Variable '{var}' has {rate}% missing data. High missing rates may indicate data quality issues."),
                        var = var, rate = round(missing_rate * 100, 1)
                    ))
                }
            }
            
            return(warnings)
        },


        # Fallback with gtExtras-style appearance for categorical data
        .gtExtras_style_fallback_cat = function(dataset, var_list) {
            # Get categorical variables only
            cat_vars <- var_list[sapply(dataset[var_list], function(x) is.factor(x) || is.character(x))]
            
            if (length(cat_vars) == 0) {
                return(htmltools::HTML(glue::glue("<p>{msg}</p>", 
                    msg = .("No categorical variables available for summary table."))))
            }
            
            # Calculate comprehensive summary statistics for categorical data
            summary_stats <- data.frame(
                Variable = cat_vars,
                Type = rep(.("categorical"), length(cat_vars)),
                N = sapply(dataset[cat_vars], function(x) sum(!is.na(x))),
                Missing = sapply(dataset[cat_vars], function(x) sum(is.na(x))),
                Levels = sapply(dataset[cat_vars], function(x) {
                    if (is.factor(x)) nlevels(x) else length(unique(x[!is.na(x)]))
                }),
                Most_Common = sapply(dataset[cat_vars], function(x) {
                    tbl <- table(x, useNA = "no")
                    if (length(tbl) > 0) names(tbl)[which.max(tbl)] else "NA"
                }),
                Most_Common_N = sapply(dataset[cat_vars], function(x) {
                    tbl <- table(x, useNA = "no")
                    if (length(tbl) > 0) max(tbl) else 0
                }),
                stringsAsFactors = FALSE
            )
            
            # Create gtExtras-style table for categorical data
            gt_table <- summary_stats %>%
                gt::gt() %>%
                gt::tab_header(
                    title = gt::md(glue::glue("**{title}**", title = .("Categorical Variables Summary"))),
                    subtitle = gt::md(glue::glue("*{subtitle}*", subtitle = .("Comprehensive statistics for categorical variables")))
                ) %>%
                gt::cols_label(
                    Variable = .("Variable"),
                    Type = .("Type"),
                    N = .("N"),
                    Missing = .("Missing"),
                    Levels = .("Levels"),
                    Most_Common = .("Most Common"),
                    Most_Common_N = .("Count")
                ) %>%
                gt::tab_style(
                    style = gt::cell_fill(color = "#f8f9fa"),
                    locations = gt::cells_column_labels()
                ) %>%
                gt::tab_style(
                    style = gt::cell_text(weight = "bold"),
                    locations = gt::cells_column_labels()
                ) %>%
                gt::opt_stylize(style = 6, color = "blue") %>%
                gt::tab_options(
                    table.font.size = 12,
                    heading.title.font.size = 16,
                    heading.subtitle.font.size = 12
                )
            
            # Convert to HTML
            print_table <- print(gt_table)
            return(htmltools::HTML(print_table[["children"]][[2]]))
        }
    )
)



