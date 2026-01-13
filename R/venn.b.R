#' @title Venn Diagram
#' @description Generates a Venn Diagram and an Upset diagram from selected categorical variables.
#' This function converts specified variables to logical values based on a chosen "true" level.
#' Two visual outputs are produced: a Venn diagram (via ggvenn) and an Upset plot (via UpSetR or ComplexUpset).
#' Additionally, a summary table of "true" counts for each variable is provided.
#' 
#' ComplexUpset features include advanced styling, statistical annotations, custom sorting,
#' and enhanced theming options for publication-ready figures.
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom dplyr inner_join
#' @import ggvenn
#' @import ggVennDiagram
#' @import UpSetR
#' @import ComplexUpset
#' @importFrom grid grid.text
#' @importFrom ggplot2 ggtitle theme element_text
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#'
#' @return The function produces a Venn diagram and an Upset diagram.
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic 2-variable Venn diagram
#' data("mtcars")
#' mtcars$vs <- factor(mtcars$vs, levels = c(0, 1), labels = c("V-shaped", "Straight"))
#' mtcars$am <- factor(mtcars$am, levels = c(0, 1), labels = c("Automatic", "Manual"))
#'
#' # Create Venn diagram showing overlap between V-shaped engines and Manual transmission
#' venn(data = mtcars, var1 = "vs", var1true = "V-shaped",
#'      var2 = "am", var2true = "Manual")
#'
#' # Example 2: 3-variable Venn diagram with penguins data
#' library(palmerpenguins)
#' data("penguins")
#' penguins$large_bill <- factor(ifelse(penguins$bill_length_mm > 45, "Large", "Small"))
#' penguins$heavy_weight <- factor(ifelse(penguins$body_mass_g > 4000, "Heavy", "Light"))
#' penguins$adelie_species <- factor(ifelse(penguins$species == "Adelie", "Adelie", "Other"))
#'
#' venn(data = penguins,
#'      var1 = "large_bill", var1true = "Large",
#'      var2 = "heavy_weight", var2true = "Heavy",
#'      var3 = "adelie_species", var3true = "Adelie")
#'
#' # Example 3: Variable names with spaces and numbers (requires careful handling)
#' # jamovi GUI automatically handles most problematic names
#' # When calling directly in R, variable names with spaces/numbers need backticks:
#' # venn(data = mydata, var1 = "`Rater 1`", var1true = "Positive",
#' #      var2 = "`Score 2A`", var2true = "High")
#'
#' # Note: Names like "Rater 1", "Score 2A", "Item 3B" may cause parsing issues
#' # at the jamovi interface level. Solutions:
#' # 1. Use jamovi GUI for variable selection (recommended)
#' # 2. Rename variables to avoid spaces + numbers: "Rater1", "Score2A", "Item3B"
#' # 3. In R console, use backticks: `Rater 1` or quote properly
#'
#' # Example 4: Clinical biomarker analysis
#' data("biomarkers")  # Hypothetical clinical dataset
#' venn(data = biomarkers,
#'      var1 = "ER_positive", var1true = "Positive",
#'      var2 = "PR_positive", var2true = "Positive",
#'      var3 = "HER2_amplified", var3true = "Amplified",
#'      show_ggVennDiagram = TRUE,
#'      regionLabels = "both",
#'      clinicalSummary = TRUE)
#'
#' # Example 5: Medical/Clinical comorbidity analysis
#' # Create sample clinical data
#' clinical_data <- data.frame(
#'   patient_id = 1:100,
#'   diabetes = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.3, 0.7)),
#'   hypertension = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.4, 0.6)),
#'   obesity = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.25, 0.75))
#' )
#' 
#' # Analyze comorbidity patterns
#' venn(data = clinical_data,
#'      var1 = "diabetes", var1true = "Yes",
#'      var2 = "hypertension", var2true = "Yes",
#'      var3 = "obesity", var3true = "Yes")
#' 
#' # Example 4: Using ComplexUpset for advanced features
#' venn(data = clinical_data,
#'      var1 = "diabetes", var1true = "Yes",
#'      var2 = "hypertension", var2true = "Yes",
#'      var3 = "obesity", var3true = "Yes",
#'      show_complexUpset = TRUE,
#'      sortBy = "freq",
#'      minSize = 5,
#'      showAnnotations = TRUE)
#'
#' # Example 5: Advanced customization using ggVennDiagram
#' venn(data = clinical_data,
#'      var1 = "diabetes", var1true = "Yes",
#'      var2 = "hypertension", var2true = "Yes",
#'      var3 = "obesity", var3true = "Yes",
#'      show_ggVennDiagram = TRUE,
#'      regionLabels = "both",
#'      colorPalette = "Set1",
#'      labelSize = 3.5,
#'      setNameSize = 4.5)
#'
#' # Example 6: 5-variable Venn diagram using ggVennDiagram
#' # Add more clinical variables
#' clinical_data$smoking <- sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.2, 0.8))
#' clinical_data$family_history <- sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.35, 0.65))
#'
#' venn(data = clinical_data,
#'      var1 = "diabetes", var1true = "Yes",
#'      var2 = "hypertension", var2true = "Yes",
#'      var3 = "obesity", var3true = "Yes",
#'      var4 = "smoking", var4true = "Yes",
#'      var5 = "family_history", var5true = "Yes",
#'      show_ggVennDiagram = TRUE,
#'      regionLabels = "percent",
#'      colorPalette = "viridis")
#' }
#' @export vennClass

# Helper function to escape variable names with special characters for formulas
.escapeVariableNames <- function(var_names) {
    # Check if variable names contain special characters that need escaping
    need_escaping <- grepl("[^a-zA-Z0-9._]", var_names)
    var_names[need_escaping] <- paste0("`", var_names[need_escaping], "`")
    return(var_names)
}

# Helper function to validate and clean variable names for jamovi interface
.validateVennVariableNames <- function(var_names) {
    if (is.null(var_names) || length(var_names) == 0) {
        return(list(valid = TRUE, message = ""))
    }

    problematic <- c()

    # Check for names that might cause parsing issues
    for (name in var_names) {
        if (grepl("^[0-9]", name)) {
            problematic <- c(problematic, paste0("'", name, "' starts with a number"))
        }
        if (grepl("\\s+[0-9]", name)) {
            problematic <- c(problematic, paste0("'", name, "' contains space followed by number"))
        }
        if (grepl("[^a-zA-Z0-9._\\s]", name)) {
            problematic <- c(problematic, paste0("'", name, "' contains special characters"))
        }
    }

    if (length(problematic) > 0) {
        message <- paste0(
            "⚠️ Variable name format note: ", paste(problematic, collapse = "; "),
            ". If you encounter parsing errors, consider renaming these variables or using the jamovi GUI for variable selection."
        )
        return(list(valid = FALSE, message = message))
    }

    return(list(valid = TRUE, message = ""))
}

#' Venn Diagram Class
#' @name vennClass
#' @importFrom R6 R6Class
vennClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "vennClass",
        inherit = vennBase,
        private = list(
            .name_mapping = list(),
            .errors = character(0),
            .warnings = character(0),
            .info = character(0),

            .init = function() {
                # Count number of selected variables for dynamic sizing
                num_vars <- 0
                if (!is.null(self$options$var1)) num_vars <- num_vars + 1
                if (!is.null(self$options$var2)) num_vars <- num_vars + 1
                if (!is.null(self$options$var3)) num_vars <- num_vars + 1
                if (!is.null(self$options$var4)) num_vars <- num_vars + 1
                if (!is.null(self$options$var5)) num_vars <- num_vars + 1
                if (!is.null(self$options$var6)) num_vars <- num_vars + 1
                if (!is.null(self$options$var7)) num_vars <- num_vars + 1

                # Calculate dynamic dimensions based on number of variables
                # Base dimensions
                base_width <- 700
                base_height <- 450

                # Adjust dimensions based on number of variables
                if (num_vars <= 2) {
                    # 2 variables: compact size
                    plot_width <- base_width
                    plot_height <- base_height
                } else if (num_vars <= 4) {
                    # 3-4 variables: moderate increase
                    plot_width <- base_width + 100
                    plot_height <- base_height + 100
                } else {
                    # 5+ variables: larger size for UpSet plots
                    plot_width <- base_width + 200
                    plot_height <- base_height + 200
                }

                # Set dynamic sizes for all plot types
                self$results$plotGgvenn$setSize(plot_width, plot_height)
                self$results$plotGgVennDiagram$setSize(plot_width, plot_height)

                # UpSet plots need extra width for more variables
                upset_width <- plot_width + (num_vars * 50)  # Add 50px per variable
                upset_height <- plot_height + 50  # Extra height for intersections

                self$results$plotUpsetR$setSize(upset_width, upset_height)
                self$results$plotComplexUpset$setSize(upset_width, upset_height)
            },

            .run = function() {
                private$.checkpoint()

                # Reset message accumulators at the start of each run
                private$.errors <- character(0)
                private$.warnings <- character(0)
                private$.info <- character(0)

                # Validate required variables and their true levels
                if (!private$.validateVariables()) {
                    private$.displayNotices()
                    return()  # Validation failed, errors already accumulated
                }

                # If no plot type selected, default to ggvenn for user feedback
                if (!self$options$show_ggvenn && !self$options$show_ggVennDiagram &&
                    !self$options$show_upsetR && !self$options$show_complexUpset) {
                    self$options$show_ggvenn <- TRUE
                    self$results$todo$setContent(
                        paste0("<div class='alert alert-info'>",
                               .("No plot type was selected; defaulting to ggvenn output."), "</div>")
                    )
                }
                
                # Control welcome panel visibility based on variable selection
                if (is.null(self$options$var1) || is.null(self$options$var2)) {
                    # Show welcome message when no variables are selected
                    self$results$welcome$setVisible(TRUE)

                    # Extract progress info
                    has_var1 <- !is.null(self$options$var1) && length(self$options$var1) > 0
                    has_var2 <- !is.null(self$options$var2) && length(self$options$var2) > 0
                    has_var1_level <- !is.null(self$options$var1true) && length(self$options$var1true) > 0
                    has_var2_level <- !is.null(self$options$var2true) && length(self$options$var2true) > 0

                    # Count optional variables
                    optional_vars <- sum(!sapply(list(self$options$var3, self$options$var4,
                                                    self$options$var5, self$options$var6,
                                                    self$options$var7), is.null))

                    # Create professional welcome message following decisionpanel style
                    welcome_content <- paste0(
                        "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>",
                        "<div style='background: #f5f5f5; border: 2px solid #333; padding: 20px; margin-bottom: 20px;'>",
                        "<h2 style='margin: 0 0 10px 0; font-size: 20px; color: #333;'>Venn Diagram Analysis</h2>",
                        "<p style='margin: 0; font-size: 14px; color: #666;'>Visualize overlaps and intersections between categorical variables</p>",
                        "</div>",

                        "<div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>",
                        "<h3 style='margin: 0 0 10px 0; color: #333; font-size: 16px;'>Setup Progress</h3>"
                    )

                    # Progress indicators - simple and accessible
                    if (has_var1 && has_var2 && has_var1_level && has_var2_level) {
                        welcome_content <- paste0(welcome_content,
                            "<div style='font-weight: bold; margin-bottom: 10px;'>",
                            "[READY] Variables: 2 required + ", optional_vars, " optional | Levels: Selected</div>",
                            "<p style='margin: 0;'>Minimum requirements met. Analysis will begin automatically.</p>"
                        )
                    } else {
                        welcome_content <- paste0(welcome_content,
                            "<div style='margin-bottom: 10px;'>",
                            if(has_var1) "[✓]" else "[ ]", " Variable 1: ", if(has_var1) "Selected" else "Not selected",
                            if(has_var1 && has_var1_level) " + Level" else "", "</div>",
                            "<div style='margin-bottom: 10px;'>",
                            if(has_var2) "[✓]" else "[ ]", " Variable 2: ", if(has_var2) "Selected" else "Not selected",
                            if(has_var2 && has_var2_level) " + Level" else "", "</div>",
                            if(optional_vars > 0) paste0("<div style='margin-bottom: 10px;'>[+] Optional Variables: ", optional_vars, "</div>") else ""
                        )
                    }

                    welcome_content <- paste0(welcome_content,
                        "</div>",

                        "<table style='width: 100%; border-collapse: collapse; margin-bottom: 20px;'>",
                        "<tr>",
                        "<td style='width: 50%; border: 1px solid #ccc; padding: 15px; vertical-align: top;'>",
                        "<h4 style='margin: 0 0 10px 0; font-size: 15px;'>Quick Start Guide</h4>",
                        "<ol style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                        "<li>Select your <strong>Primary Variable</strong> (Variable 1)</li>",
                        "<li>Choose which level represents the <strong>'true' condition</strong></li>",
                        "<li>Add a <strong>Secondary Variable</strong> (Variable 2)</li>",
                        "<li>Select its <strong>'true' level</strong></li>",
                        "<li>Optionally add Variables 3-7 for complex analysis</li>",
                        "<li>Configure plot options and styling</li>",
                        "</ol></td>",

                        "<td style='width: 50%; border: 1px solid #ccc; padding: 15px; vertical-align: top;'>",
                        "<h4 style='margin: 0 0 10px 0; font-size: 15px;'>Visualization Options</h4>",
                        "<ul style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                        "<li><strong>ggvenn:</strong> Classic 2-3 variable Venn diagrams</li>",
                        "<li><strong>ggVennDiagram:</strong> Advanced customizable Venn plots</li>",
                        "<li><strong>UpSetR:</strong> Matrix-style intersection plots for 3+ variables</li>",
                        "<li><strong>ComplexUpset:</strong> Enhanced UpSet with annotations</li>",
                        "<li><strong>Set calculations:</strong> Detailed overlap statistics</li>",
                        "</ul></td></tr></table>",

                        "<div style='background: #f9f9f9; border: 1px solid #ccc; padding: 15px;'>",
                        "<h4 style='margin: 0 0 10px 0; font-size: 15px;'>Clinical Applications</h4>",
                        "<ul style='margin: 0; padding-left: 20px; font-size: 14px;'>",
                        "<li><strong>Biomarker overlap:</strong> Analyze multiple tumor markers or expression patterns</li>",
                        "<li><strong>Treatment response:</strong> Compare response across different therapies</li>",
                        "<li><strong>Risk factors:</strong> Examine comorbidity patterns and risk combinations</li>",
                        "<li><strong>Diagnostic concordance:</strong> Compare agreement between different tests or raters</li>",
                        "<li><strong>Variable naming:</strong> Supports names with spaces and numbers (e.g., 'Rater 3', 'Marker 2A')</li>",
                        "</ul></div></div>"
                    )

                    self$results$welcome$setContent(welcome_content)
                } else {
                    # Hide welcome message when variables are selected
                    self$results$welcome$setVisible(FALSE)
                }

                # Check if required variables (var1 and var2) are provided.
                if (is.null(self$options$var1) || is.null(self$options$var2)) {
                    # Keep the friendly welcome message in todo (non-critical guidance)
                    todo <- paste0(
                        "<br><strong>", .("Welcome to ClinicoPath Venn Diagram Tool"), "</strong>",
                        "<br><br>",
                        .("This tool helps you visualize overlaps between categorical variables using Venn and Upset diagrams."),
                        "<br><br>",
                        "<div style='background-color: #e8f4f8; padding: 12px; border-radius: 4px; border-left: 3px solid #17a2b8; font-size: 0.95em;'>",
                        "<strong>📋 ", .("Step-by-Step Variable Selection:"), "</strong>",
                        "<ol style='margin: 8px 0 0 0; padding-left: 20px;'>",
                        "<li><strong>", .("Start with Variable 1"), "</strong> - ", .("Select your first categorical variable"), "</li>",
                        "<li><strong>", .("Add Variable 2"), "</strong> - ", .("Choose a second variable (unlocks after Variable 1)"), "</li>",
                        "<li><strong>", .("Optional: Variable 3"), "</strong> - ", .("Add a third variable for 3-way analysis"), "</li>",
                        "<li><strong>", .("Optional: Variable 4"), "</strong> - ", .("Add a fourth variable for 4-way analysis"), "</li>",
                        "</ol>",
                        "<em>💡 ", .("Tip: Each variable must be categorical (factor) and you'll need to select which level represents 'true' for each."), "</em>",
                        "</div>",
                        "<hr><br>"
                    )
                    self$results$todo$setContent(todo)
                    return()
                } else {
                    # Clear welcome message once variables are selected.
                    self$results$todo$setContent("")

                    # Generate explanatory content if requested
                    if (self$options$explanatory || self$options$aboutAnalysis) {
                        private$.generateAboutAnalysis()
                    }

                    # Empty dataset check
                    if (nrow(self$data) == 0) {
                        private$.errors <- c(private$.errors,
                            'Dataset contains no complete rows. Please check your data and ensure at least one complete observation exists.')
                        private$.displayNotices()
                        return()
                    }

                    # CRITICAL FIX: Capture original data BEFORE any filtering
                    # This ensures we report actual missingness, not post-exclusion stats
                    private$.checkpoint()
                    original_data <- self$data
                    original_n <- nrow(original_data)

                    # Retrieve variable names and their corresponding "true" level selections.
                    var1 <- self$options$var1
                    var1true <- self$options$var1true
                    var2 <- self$options$var2
                    var2true <- self$options$var2true
                    var3 <- self$options$var3
                    var3true <- self$options$var3true
                    var4 <- self$options$var4
                    var4true <- self$options$var4true
                    var5 <- self$options$var5
                    var5true <- self$options$var5true
                    var6 <- self$options$var6
                    var6true <- self$options$var6true
                    var7 <- self$options$var7
                    var7true <- self$options$var7true

                    # Validate variable names for potential parsing issues
                    all_vars <- c(var1, var2, var3, var4, var5, var6, var7)
                    selected_vars <- all_vars[!sapply(all_vars, is.null)]
                    validation_result <- .validateVennVariableNames(selected_vars)

                    if (!validation_result$valid) {
                        # Display warning but continue with analysis
                        private$.warnings <- c(private$.warnings, validation_result$message)
                    }

                    # CRITICAL FIX: Select ONLY the variables needed for analysis
                    # This prevents dropping cases with NAs in unrelated columns
                    selected_data <- original_data[, selected_vars, drop = FALSE]

                    # CRITICAL FIX: Calculate missingness BEFORE exclusion for transparency
                    original_complete <- sum(complete.cases(selected_data))

                    # Apply naOmit ONLY to selected variables, not entire dataset
                    full_data <- jmvcore::naOmit(selected_data)
                    excluded_n <- original_n - nrow(full_data)

                    # CRITICAL WARNING: Report case loss if any exclusions occurred
                    if (excluded_n > 0) {
                        excluded_pct <- round(100 * excluded_n / original_n, 1)
                        private$.warnings <- c(private$.warnings, sprintf(
                            '<strong>CASE EXCLUSION:</strong> %d cases (%.1f%%) excluded due to missing values. Original N=%d, Final N=%d. Venn diagram counts and percentages reflect complete cases only. Consider implications for generalizability.',
                            excluded_n, excluded_pct, original_n, nrow(full_data)
                        ))
                    }

                    # Restore row numbers for tracking
                    row_numbers <- suppressWarnings(as.integer(rownames(full_data)))
                    if (length(row_numbers) != nrow(full_data) || any(is.na(row_numbers))) {
                        row_numbers <- seq_len(nrow(full_data))
                    }

                    # Collect only selected variables and convert to logical values
                    mydata <- data.frame(row.names = seq_len(nrow(full_data)))

                    # Create mapping between original and safe names for variables with spaces/numbers
                    name_mapping <- list()

                    # Process each variable with robust error handling for problematic names
                    if (!is.null(self$options$var1)) {
                        safe_name1 <- make.names(var1)
                        tryCatch({
                            mydata[[safe_name1]] <- ifelse(full_data[[var1]] == var1true, TRUE, FALSE)
                            name_mapping[[safe_name1]] <- var1
                        }, error = function(e) {
                            # If direct access fails, try with escaped name or provide helpful error
                            escaped_var1 <- .escapeVariableNames(var1)
                            stop(paste("Error processing variable '", var1, "': ", e$message,
                                       ". Try using backticks around the variable name: `", var1, "`", sep = ""))
                        })
                    }
                    if (!is.null(self$options$var2)) {
                        safe_name2 <- make.names(var2)
                        tryCatch({
                            mydata[[safe_name2]] <- ifelse(full_data[[var2]] == var2true, TRUE, FALSE)
                            name_mapping[[safe_name2]] <- var2
                        }, error = function(e) {
                            escaped_var2 <- .escapeVariableNames(var2)
                            stop(paste("Error processing variable '", var2, "': ", e$message,
                                       ". Try using backticks around the variable name: `", var2, "`", sep = ""))
                        })
                    }
                    if (!is.null(self$options$var3)) {
                        safe_name3 <- make.names(var3)
                        tryCatch({
                            mydata[[safe_name3]] <- ifelse(full_data[[var3]] == var3true, TRUE, FALSE)
                            name_mapping[[safe_name3]] <- var3
                        }, error = function(e) {
                            escaped_var3 <- .escapeVariableNames(var3)
                            stop(paste("Error processing variable '", var3, "': ", e$message,
                                       ". Try using backticks around the variable name: `", var3, "`", sep = ""))
                        })
                    }
                    if (!is.null(self$options$var4)) {
                        safe_name4 <- make.names(var4)
                        tryCatch({
                            mydata[[safe_name4]] <- ifelse(full_data[[var4]] == var4true, TRUE, FALSE)
                            name_mapping[[safe_name4]] <- var4
                        }, error = function(e) {
                            escaped_var4 <- .escapeVariableNames(var4)
                            stop(paste("Error processing variable '", var4, "': ", e$message,
                                       ". Try using backticks around the variable name: `", var4, "`", sep = ""))
                        })
                    }
                    if (!is.null(self$options$var5)) {
                        safe_name5 <- make.names(var5)
                        tryCatch({
                            mydata[[safe_name5]] <- ifelse(full_data[[var5]] == var5true, TRUE, FALSE)
                            name_mapping[[safe_name5]] <- var5
                        }, error = function(e) {
                            escaped_var5 <- .escapeVariableNames(var5)
                            stop(paste("Error processing variable '", var5, "': ", e$message,
                                       ". Try using backticks around the variable name: `", var5, "`", sep = ""))
                        })
                    }
                    if (!is.null(self$options$var6)) {
                        safe_name6 <- make.names(var6)
                        tryCatch({
                            mydata[[safe_name6]] <- ifelse(full_data[[var6]] == var6true, TRUE, FALSE)
                            name_mapping[[safe_name6]] <- var6
                        }, error = function(e) {
                            escaped_var6 <- .escapeVariableNames(var6)
                            stop(paste("Error processing variable '", var6, "': ", e$message,
                                       ". Try using backticks around the variable name: `", var6, "`", sep = ""))
                        })
                    }
                    if (!is.null(self$options$var7)) {
                        safe_name7 <- make.names(var7)
                        tryCatch({
                            mydata[[safe_name7]] <- ifelse(full_data[[var7]] == var7true, TRUE, FALSE)
                            name_mapping[[safe_name7]] <- var7
                        }, error = function(e) {
                            escaped_var7 <- .escapeVariableNames(var7)
                            stop(paste("Error processing variable '", var7, "': ", e$message,
                                       ". Try using backticks around the variable name: `", var7, "`", sep = ""))
                        })
                    }

                    # Store name mapping for use in plots and calculations
                    private$.name_mapping <- name_mapping

                    # Prepare data for Venn diagrams (logical values).
                    plotDataVenn <- list("mydata" = mydata,
                                         "names" = names(mydata))

                    # Set state for each plot type
                    if (self$options$show_ggvenn) {
                        self$results$plotGgvenn$setState(plotDataVenn)
                    }
                    if (self$options$show_ggVennDiagram) {
                        self$results$plotGgVennDiagram$setState(plotDataVenn)
                    }

                    # Prepare data for Upset diagrams by converting logical values to integers.
                    private$.checkpoint()
                    mydata2 <- mydata %>%
                        dplyr::mutate(dplyr::across(.cols = dplyr::everything(), ~ as.integer(.)))
                    namescolumn2 <- names(mydata2)
                    plotDataUpset <- list("mydata" = mydata2,
                                          "names" = namescolumn2)

                    if (self$options$show_upsetR) {
                        self$results$plotUpsetR$setState(plotDataUpset)
                    }
                    if (self$options$show_complexUpset) {
                        self$results$plotComplexUpset$setState(plotDataUpset)
                    }

                    # Create summary statistics for each variable using helper function
                    summaryData <- data.frame(
                        Variable = character(),
                        TrueCount = integer(),
                        FalseCount = integer(),
                        TotalCount = integer(),
                        TruePercentage = numeric(),
                        stringsAsFactors = FALSE
                    )
                    
                    # Process each variable that was selected using helper function
                    variables <- list(var1, var2, var3, var4, var5, var6, var7)
                    for (var in variables) {
                        if (!is.null(var)) {
                            # Find the safe column name that corresponds to this variable
                            safe_name <- make.names(var)
                            if (safe_name %in% names(mydata)) {
                                varStats <- private$.calculateSummaryStats(mydata, safe_name, var)
                                if (!is.null(varStats)) {
                                    summaryData <- rbind(summaryData, varStats)
                                }
                            }
                        }
                    }
                    
                    # Set the summary results
                    if (!is.null(self$results$summary)) {
                        for (i in seq_len(nrow(summaryData))) {
                            self$results$summary$addRow(rowKey = i, values = list(
                                variable = summaryData$Variable[i],
                                trueCount = summaryData$TrueCount[i],
                                falseCount = summaryData$FalseCount[i],
                                totalCount = summaryData$TotalCount[i],
                                truePercentage = summaryData$TruePercentage[i]
                            ))
                        }
                    }
                    
                    # Generate clinical interpretations if requested
                    if (self$options$explanatory || self$options$clinicalSummary) {
                        private$.generateClinicalSummary(mydata, list(var1, var2, var3, var4, var5, var6, var7), summaryData)
                    }
                    if (self$options$explanatory || self$options$reportSentences) {
                        private$.generateReportSentences(summaryData, mydata)
                    }
                    if (self$options$explanatory || self$options$assumptions) {
                        private$.generateAssumptions()
                    }
                    if (self$options$showGlossary) {
                        private$.generateGlossary()
                    }

                    # Generate set calculations if requested
                    if (self$options$showSetCalculations) {
                        private$.generateSetCalculations(mydata2, namescolumn2, summaryData)
                    }

                    # Generate membership table if requested
                    if (self$options$showSetCalculations && (self$options$showMembershipTable || self$options$membershipGroups)) {
                        private$.generateMembershipTable(mydata, names(mydata), private$.name_mapping, row_numbers)
                    }

                    # Analysis completion info
                    num_sets <- sum(!sapply(list(self$options$var1, self$options$var2, self$options$var3,
                                                 self$options$var4, self$options$var5, self$options$var6,
                                                 self$options$var7), is.null))
                    private$.info <- c(private$.info, sprintf(
                        'Venn diagram analysis completed successfully for %d categorical variables across N=%d observations.',
                        num_sets, nrow(full_data)
                    ))

                    # Display all accumulated notices
                    private$.displayNotices()
                }
            },

            .plotGgvenn = function(image, ggtheme, theme, ...) {
                private$.checkpoint()

                # Validate that the required inputs are available.
                if (is.null(self$options$var1) || is.null(self$options$var2))
                    return()
                if (nrow(self$data) == 0)
                    stop(.('Data contains no (complete) rows'))

                # Count the number of variables selected
                num_vars <- sum(!sapply(list(self$options$var1, self$options$var2, self$options$var3,
                                           self$options$var4, self$options$var5, self$options$var6,
                                           self$options$var7), is.null))

                # Check if more than 4 variables are selected
                if (num_vars > 4) {
                    # Create an informative message similar to waterfall spider plot
                    text_warning <- paste0(
                        .("ggvenn Plot: Too Many Variables"),
                        "\n\n",
                        .("ggvenn can only display up to 4 variables effectively."),
                        "\n",
                        sprintf(.("You have selected %d variables."), num_vars),
                        "\n\n",
                        .("Recommended Solution:"),
                        "\n",
                        .("• Enable 'Show ggVennDiagram Plot' instead"),
                        "\n",
                        .("• ggVennDiagram supports 5+ variables with better visualization"),
                        "\n\n",
                        .("Alternative Options:"),
                        "\n",
                        .("• Use UpSetR or ComplexUpset plots for complex intersections"),
                        "\n",
                        .("• Reduce to 4 or fewer variables for ggvenn visualization")
                    )

                    # Create a text plot with the warning message
                    p <- ggplot2::ggplot() +
                        ggplot2::annotate("text", x = 0.5, y = 0.5,
                                        label = text_warning,
                                        hjust = 0.5, vjust = 0.5,
                                        size = 4, color = "#2c3e50",
                                        lineheight = 1.2) +
                        ggplot2::theme_void() +
                        ggplot2::theme(
                            plot.background = ggplot2::element_rect(fill = "#f8f9fa", color = "#dee2e6"),
                            plot.margin = ggplot2::margin(20, 20, 20, 20)
                        ) +
                        ggplot2::xlim(0, 1) +
                        ggplot2::ylim(0, 1)

                    print(p)
                    return(TRUE)
                }

                # Retrieve the prepared data.
                results <- image$state
                mydata2 <- results$mydata
                namescolumn2 <- results$names

                # Use ggvenn (classic)
                plot <- private$.plotGgVenn(mydata2, namescolumn2, ggtheme)

                # Print the Venn Diagram.
                print(plot)
                TRUE
            },

            .plotGgVennDiagram = function(image, ggtheme, theme, ...) {
                private$.checkpoint()

                # Validate that the required inputs are available.
                if (is.null(self$options$var1) || is.null(self$options$var2))
                    return()
                if (nrow(self$data) == 0)
                    stop(.('Data contains no (complete) rows'))

                # Retrieve the prepared data.
                results <- image$state
                mydata2 <- results$mydata
                namescolumn2 <- results$names

                # Use ggVennDiagram (advanced features)
                plot <- private$.plotGgVennDiagramHelper(mydata2, namescolumn2, ggtheme, theme)

                # Print the Venn Diagram.
                print(plot)
                TRUE
            },

            .plotUpsetR = function(image, ggtheme, theme, ...) {
                private$.checkpoint()

                # Validate that the required inputs are available.
                if (is.null(self$options$var1) || is.null(self$options$var2))
                    return()
                if (nrow(self$data) == 0)
                    stop(.('Data contains no (complete) rows'))

                # Retrieve the prepared data.
                results <- image$state
                mydata2 <- results$mydata

                # Generate the UpSetR plot
                plot <- private$.plotUpsetRHelper(mydata2)

                # Print the UpSetR plot
                print(plot)
                TRUE
            },

            .plotComplexUpset = function(image, ggtheme, theme, ...) {
                private$.checkpoint()

                # Validate that the required inputs are available.
                if (is.null(self$options$var1) || is.null(self$options$var2))
                    return()
                if (nrow(self$data) == 0)
                    stop(.('Data contains no (complete) rows'))

                # Retrieve the prepared data.
                results <- image$state
                mydata2 <- results$mydata

                # Generate the ComplexUpset plot
                plot <- private$.plotComplexUpsetHelper(mydata2)

                # Print the ComplexUpset plot
                print(plot)
                TRUE
            },

            
            # Validation helper method
            .validateVariables = function() {
                # Returns TRUE if validation passes, FALSE if errors found (errors accumulated in private$.errors)

                # Check if dataset is empty
                if (nrow(self$data) == 0) {
                    private$.errors <- c(private$.errors, 'Dataset is empty. Please provide data with observations.')
                    return(FALSE)
                }

                # Validate var1 (required)
                if (!is.null(self$options$var1)) {
                    if (is.null(self$options$var1true)) {
                        private$.errors <- c(private$.errors,
                            'Variable 1 selected but "true" level not specified. Please select which level represents the positive/true condition for Variable 1.')
                        return(FALSE)
                    }
                    var1_data <- self$data[[self$options$var1]]
                    if (all(is.na(var1_data))) {
                        private$.errors <- c(private$.errors,
                            sprintf("Variable '%s' contains only missing values. Please select a different variable with valid data.", self$options$var1))
                        return(FALSE)
                    }
                    if (!self$options$var1true %in% levels(as.factor(var1_data))) {
                        available_levels <- paste(levels(as.factor(var1_data)), collapse=", ")
                        private$.errors <- c(private$.errors,
                            sprintf("Selected 'true' level '%s' not found in Variable '%s'. Available levels: %s", self$options$var1true, self$options$var1, available_levels))
                        return(FALSE)
                    }
                }

                # Validate var2 (required)
                if (!is.null(self$options$var2)) {
                    if (is.null(self$options$var2true)) {
                        private$.errors <- c(private$.errors,
                            'Variable 2 selected but "true" level not specified. Please select which level represents the positive/true condition for Variable 2.')
                        return(FALSE)
                    }
                    var2_data <- self$data[[self$options$var2]]
                    if (all(is.na(var2_data))) {
                        private$.errors <- c(private$.errors,
                            sprintf("Variable '%s' contains only missing values. Please select a different variable with valid data.", self$options$var2))
                        return(FALSE)
                    }
                    if (!self$options$var2true %in% levels(as.factor(var2_data))) {
                        available_levels <- paste(levels(as.factor(var2_data)), collapse=", ")
                        private$.errors <- c(private$.errors,
                            sprintf("Selected 'true' level '%s' not found in Variable '%s'. Available levels: %s", self$options$var2true, self$options$var2, available_levels))
                        return(FALSE)
                    }
                }

                # Validate optional variables (var3-7) - allow skipping if all NA
                for (i in 3:7) {
                    var_name <- paste0("var", i)
                    var_true_name <- paste0("var", i, "true")
                    var_value <- self$options[[var_name]]
                    var_true_value <- self$options[[var_true_name]]

                    if (!is.null(var_value)) {
                        if (is.null(var_true_value)) {
                            private$.errors <- c(private$.errors,
                                sprintf('Variable %d selected but "true" level not specified. Please select which level represents the positive/true condition.', i))
                            return(FALSE)
                        }
                        var_data <- self$data[[var_value]]
                        if (!var_true_value %in% levels(as.factor(var_data))) {
                            available_levels <- paste(levels(as.factor(var_data)), collapse=", ")
                            private$.errors <- c(private$.errors,
                                sprintf("Selected 'true' level '%s' not found in Variable '%s'. Available levels: %s", var_true_value, var_value, available_levels))
                            return(FALSE)
                        }
                    }
                }

                return(TRUE)  # Validation passed
            },
            
            # Helper function for calculating summary statistics
            .calculateSummaryStats = function(data, safe_varname, original_varname = NULL) {
                if (is.null(safe_varname)) return(NULL)

                # Use original name for display, safe name for data access
                display_name <- if (!is.null(original_varname)) original_varname else safe_varname

                # Ensure the column exists and contains logical data
                if (!safe_varname %in% names(data)) return(NULL)

                column_data <- data[[safe_varname]]

                # Ensure data is logical, convert if necessary
                if (!is.logical(column_data)) {
                    if (is.numeric(column_data)) {
                        column_data <- as.logical(column_data)
                    } else {
                        warning(paste("Column", safe_varname, "is not logical and cannot be converted"))
                        return(NULL)
                    }
                }

                true_count <- sum(column_data, na.rm = TRUE)
                false_count <- sum(!column_data, na.rm = TRUE)
                total_count <- true_count + false_count

                data.frame(
                    Variable = display_name,
                    TrueCount = true_count,
                    FalseCount = false_count,
                    TotalCount = total_count,
                    TruePercentage = round(true_count / total_count, 4),
                    stringsAsFactors = FALSE
                )
            },
            
            # Generate About This Analysis content
            .generateAboutAnalysis = function() {
                about_content <- paste0(
                    "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;'>",
                    "<h4 style='color: #2c3e50; margin-top: 0;'>", .("About Venn Diagrams"), "</h4>",
                    "<p><strong>", .("Purpose:"), "</strong> ", .("Venn diagrams visualize overlaps and intersections between categorical variables, commonly used in clinical research to analyze:"), "</p>",
                    "<ul style='margin-left: 20px;'>",
                    "<li>", .("Biomarker co-expression patterns"), "</li>",
                    "<li>", .("Treatment response combinations"), "</li>",
                    "<li>", .("Diagnostic criteria overlap"), "</li>",
                    "<li>", .("Comorbidity relationships"), "</li>",
                    "<li>", .("Risk factor associations"), "</li>",
                    "</ul>",
                    "<p><strong>", .("How to Use:"), "</strong></p>",
                    "<ol style='margin-left: 20px;'>",
                    "<li>", .("Select 2-7 categorical variables"), "</li>",
                    "<li>", .("Choose the 'true' level for each variable (e.g., 'Positive', 'Present', 'Yes')"), "</li>",
                    "<li>", .("Engine is automatically selected: classic (2-4 variables) or advanced (5+ variables)"), "</li>",
                    "<li>", .("Adjust visualization options as needed"), "</li>",
                    "<li>", .("Interpret intersections - larger overlaps indicate stronger associations"), "</li>",
                    "</ol>",
                    "<p><strong>", .("Automatic Engine Selection:"), "</strong></p>",
                    "<ul style='margin-left: 20px;'>",
                    "<li><strong>2-4 variables:</strong> ", .("Uses ggvenn (classic, simple visualization)"), "</li>",
                    "<li><strong>5+ variables:</strong> ", .("Uses ggVennDiagram (advanced features, extensive customization, publication-ready)"), "</li>",
                    "</ul>",
                    "</div>"
                )
                self$results$aboutAnalysis$setContent(about_content)
            },
            
            # Generate clinical summary of overlap patterns
            .generateClinicalSummary = function(data, variables, summaryData) {
                if (is.null(data) || nrow(summaryData) < 2) return()
                
                # Calculate key intersections
                var_names <- summaryData$Variable
                total_n <- nrow(data)
                
                # Find largest intersection
                largest_var <- var_names[which.max(summaryData$TrueCount)]
                largest_count <- max(summaryData$TrueCount, na.rm = TRUE)
                largest_pct <- round((largest_count / total_n) * 100, 1)
                
                # Calculate 2-way intersection if we have 2+ variables
                intersection_analysis <- ""
                if (length(var_names) >= 2) {
                    var1_data <- as.logical(data[[var_names[1]]])
                    var2_data <- as.logical(data[[var_names[2]]])
                    both_true <- sum(var1_data & var2_data, na.rm = TRUE)
                    both_pct <- round((both_true / total_n) * 100, 1)
                    
                    intersection_analysis <- paste0(
                        "<p><strong>", .("Key Intersection:"), "</strong> ",
                        sprintf(.("%s cases (%s%%) had both %s and %s positive."), 
                                both_true, both_pct, var_names[1], var_names[2]), "</p>"
                    )
                }
                
                # Generate clinical interpretation and statistical warnings
                clinical_interpretation <- private$.generateClinicalInterpretation(summaryData, var_names, total_n)
                statistical_warnings <- private$.validateStatisticalPower(summaryData, total_n)

                clinical_summary <- paste0(
                    "<div style='background-color: #e8f4fd; padding: 15px; border-radius: 5px; border-left: 4px solid #3498db;'>",
                    "<h4 style='color: #2980b9; margin-top: 0;'>", .("Clinical Summary"), "</h4>",
                    "<p><strong>", .("Dataset:"), "</strong> ", sprintf(.("%s cases analyzed"), total_n), "</p>",
                    "<p><strong>", .("Most Prevalent:"), "</strong> ", 
                    sprintf(.("%s was most common (%s cases, %s%%)."), largest_var, largest_count, largest_pct), "</p>",
                    intersection_analysis,
                    "<p><em>", .("Tip: Use the Venn diagram to visualize overlap patterns and the UpSet plot for detailed intersection analysis."), "</em></p>",
                    "</div>",
                    clinical_interpretation,
                    statistical_warnings
                )

                self$results$clinicalSummary$setContent(clinical_summary)
            },
            
            # Generate copy-ready report sentences
            .generateReportSentences = function(summaryData, data) {
                if (is.null(summaryData) || nrow(summaryData) == 0) return()

                var_names <- summaryData$Variable
                total_n <- nrow(data)

                # Create individual variable sentences
                individual_sentences <- sapply(1:nrow(summaryData), function(i) {
                    sprintf("%s was positive in %s of %s cases (%s%%).",
                            summaryData$Variable[i],
                            summaryData$TrueCount[i],
                            total_n,
                            round(summaryData$TruePercentage[i] * 100, 1))
                })

                # Generate intersection analysis for clinical reporting
                intersection_sentences <- ""
                if (length(var_names) >= 2) {
                    # Calculate 2-way intersection
                    var1_data <- as.logical(data[[var_names[1]]])
                    var2_data <- as.logical(data[[var_names[2]]])
                    both_positive <- sum(var1_data & var2_data, na.rm = TRUE)
                    both_pct <- round((both_positive / total_n) * 100, 1)

                    # Calculate exclusive positivity
                    var1_only <- sum(var1_data & !var2_data, na.rm = TRUE)
                    var2_only <- sum(!var1_data & var2_data, na.rm = TRUE)

                    intersection_sentences <- sprintf(
                        "Co-occurrence of %s and %s was observed in %s cases (%s%%). %s cases (%s%%) were positive for %s only, while %s cases (%s%%) were positive for %s only.",
                        var_names[1], var_names[2], both_positive, both_pct,
                        var1_only, round((var1_only/total_n)*100, 1), var_names[1],
                        var2_only, round((var2_only/total_n)*100, 1), var_names[2]
                    )
                }

                # Generate comprehensive clinical paragraph
                clinical_paragraph <- sprintf(
                    "Analysis of %s cases revealed distinct patterns of variable expression. %s The intersection analysis demonstrated %s overlap patterns, which may have clinical implications for patient stratification and treatment planning.",
                    total_n,
                    paste(individual_sentences, collapse = " "),
                    if (length(var_names) >= 2) {
                        mean_overlap <- mean(summaryData$TrueCount) / total_n
                        if (mean_overlap > 0.5) "significant" else if (mean_overlap > 0.2) "moderate" else "limited"
                    } else "individual variable"
                )

                report_content <- paste0(
                    "<div style='background-color: #f0f8f0; padding: 15px; border-radius: 5px; border-left: 4px solid #27ae60;'>",
                    "<h4 style='color: #27ae60; margin-top: 0;'>📋 Copy-Ready Clinical Summary</h4>",
                    "<div style='background-color: white; padding: 12px; border-radius: 3px; font-family: Georgia, serif; line-height: 1.6; border: 1px solid #e9ecef;'>",
                    "<h6 style='margin: 0 0 8px 0; color: #495057;'>Clinical Report Template</h6>",
                    "<p style='margin: 0 0 10px 0;'>", clinical_paragraph, "</p>",
                    if (intersection_sentences != "") paste0("<p style='margin: 0;'>", intersection_sentences, "</p>") else "",
                    "</div>",
                    "<div style='background-color: white; padding: 10px; border-radius: 3px; margin-top: 8px; border: 1px solid #e9ecef;'>",
                    "<h6 style='margin: 0 0 6px 0; color: #495057;'>Individual Variable Summary</h6>",
                    "<ul style='margin: 0; padding-left: 20px;'>",
                    paste0("<li>", individual_sentences, "</li>", collapse = ""),
                    "</ul>",
                    "</div>",
                    "<div style='margin-top: 10px; padding: 8px; background-color: #e7f3ff; border-radius: 3px;'>",
                    "<small>💡 <strong>Usage:</strong> Select and copy text from either template above. ",
                    "The clinical report template provides publication-ready prose, while the summary offers bullet-point details.</small>",
                    "</div>",
                    "</div>"
                )
                
                self$results$reportSentences$setContent(report_content)
            },
            
            # Generate assumptions and interpretation guide
            .generateAssumptions = function() {
                assumptions_content <- paste0(
                    "<div style='background-color: #fff8dc; padding: 15px; border-radius: 5px; border-left: 4px solid #f39c12;'>",
                    "<h4 style='color: #e67e22; margin-top: 0;'>", .("Interpretation Guide & Assumptions"), "</h4>",
                    
                    "<h5>", .("How to Interpret:"), "</h5>",
                    "<ul style='margin-left: 20px;'>",
                    "<li><strong>", .("Venn Diagram:"), "</strong> ", .("Circle overlaps show shared cases. Larger intersections indicate stronger associations."), "</li>",
                    "<li><strong>", .("UpSet Plot:"), "</strong> ", .("Bar heights show intersection sizes. Dots below indicate which variables are included."), "</li>",
                    "<li><strong>", .("Summary Table:"), "</strong> ", .("Shows counts and percentages for each variable individually."), "</li>",
                    "</ul>",
                    
                    "<h5>", .("Important Assumptions:"), "</h5>",
                    "<ul style='margin-left: 20px;'>",
                    "<li>", .("Variables are categorical with clearly defined 'true' levels"), "</li>",
                    "<li>", .("Cases are independent observations"), "</li>",
                    "<li>", .("Missing data is handled by exclusion"), "</li>",
                    "<li>", .("Visualization shows patterns, not statistical significance"), "</li>",
                    "</ul>",
                    
                    "<h5>", .("Clinical Considerations:"), "</h5>",
                    "<ul style='margin-left: 20px;'>",
                    "<li>", .("Consider sample size when interpreting small intersections"), "</li>",
                    "<li>", .("Large overlaps may suggest related biological pathways"), "</li>",
                    "<li>", .("Use statistical tests for formal association analysis"), "</li>",
                    "<li>", .("Consider clinical context when interpreting patterns"), "</li>",
                    "</ul>",
                    "</div>"
                )
                
                self$results$assumptions$setContent(assumptions_content)
            },

            # Helper function for ggvenn plotting (classic)
            .plotGgVenn = function(mydata2, namescolumn2, ggtheme) {
                # Generate the Venn Diagram using ggvenn.
                plot <- ggvenn::ggvenn(
                    data = mydata2,
                    columns = namescolumn2
                )

                # Enhance the plot with a title and a refined theme for improved presentation.
                plot <- plot +
                    ggtheme +
                    ggplot2::ggtitle(.("Venn Diagram of Selected Variables")) +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                        axis.line.x = ggplot2::element_blank(),
                        axis.text.x = ggplot2::element_blank(),
                        axis.ticks.x = ggplot2::element_blank(),
                        axis.title.x = ggplot2::element_blank(),
                        axis.line.y = ggplot2::element_blank(),
                        axis.text.y = ggplot2::element_blank(),
                        axis.ticks.y = ggplot2::element_blank(),
                        axis.title.y = ggplot2::element_blank()
                    )

                return(plot)
            },

            # Helper function for UpSetR plotting
            .plotUpsetRHelper = function(mydata2) {
                # Get user options
                sortBy <- self$options$sortBy
                minSize <- self$options$minSize
                showAnnotations <- self$options$showAnnotations

                # Determine order.by parameter
                orderBy <- switch(sortBy,
                    "freq" = "freq",
                    "degree" = "degree",
                    "none" = "freq",  # Default to "freq" instead of NULL to avoid xtfrm error
                    "freq"  # default
                )

                # Create UpSetR plot
                # Note: For UpSetR, showAnnotations controls text visibility and scaling
                if (showAnnotations) {
                    # Enhanced visibility: larger text and show intersection sizes
                    plot <- UpSetR::upset(
                        mydata2,
                        order.by = orderBy,
                        cutoff = minSize,
                        text.scale = c(1.5, 1.3, 1.2, 1.1, 2, 1),
                        show.numbers = "yes"
                    )
                } else {
                    # Minimal text: smaller scaling and hide numbers
                    plot <- UpSetR::upset(
                        mydata2,
                        order.by = orderBy,
                        cutoff = minSize,
                        text.scale = c(0.8, 0.8, 0.8, 0.8, 1, 0.6),
                        show.numbers = "no"
                    )
                }

                # Add a title to the Upset Diagram using grid.text.
                grid::grid.text(.("UpSetR Diagram of Selected Variables"), x = 0.5, y = 0.97,
                                gp = grid::gpar(fontsize = 14, fontface = "bold"))

                return(plot)
            },

            # Helper function for ComplexUpset plotting
            .plotComplexUpsetHelper = function(mydata2) {
                private$.checkpoint()
                # Get user options
                sortBy <- self$options$sortBy
                minSize <- self$options$minSize
                showAnnotations <- self$options$showAnnotations

                # Prepare data for ComplexUpset (convert back to logical from integer)
                upset_data <- mydata2
                for (col in names(upset_data)) {
                    upset_data[[col]] <- as.logical(upset_data[[col]])
                }

                # Determine sort mode for ComplexUpset
                sort_mode <- switch(sortBy,
                    "freq" = "descending",
                    "degree" = "ascending",
                    "none" = FALSE,
                    "descending"  # default
                )

                # Create the base ComplexUpset plot with proper annotations
                base_annotations_list <- list(
                    'Intersection size' = ComplexUpset::intersection_size(
                        text = list(size = 3)
                    )
                )

                # Add custom annotations if requested
                # Note: For ComplexUpset, showAnnotations adds percentage labels to intersection sizes
                annotations_list <- if (showAnnotations) {
                    list(
                        'Intersection percentages' = ComplexUpset::intersection_size(
                            text = list(size = 3),
                            text_mapping = ggplot2::aes(label = paste0(round(100 * !!rlang::sym('intersection_size') / sum(!!rlang::sym('intersection_size')), 1), '%'))
                        )
                    )
                } else {
                    NULL
                }

                plot <- ComplexUpset::upset(
                    data = upset_data,
                    intersect = names(upset_data),
                    min_size = minSize,
                    sort_intersections = sort_mode,
                    sort_sets = sort_mode,
                    name = .("Intersection Size"),
                    width_ratio = 0.1,
                    height_ratio = 0.8,
                    wrap = TRUE,
                    base_annotations = base_annotations_list,
                    annotations = annotations_list,
                    themes = list(
                        'intersections_matrix' = ggplot2::theme(
                            text = ggplot2::element_text(size = 10),
                            axis.text = ggplot2::element_text(size = 8)
                        ),
                        'overall_sizes' = ggplot2::theme(
                            text = ggplot2::element_text(size = 10),
                            axis.text = ggplot2::element_text(size = 8)
                        )
                    )
                )

                # Add title
                plot <- plot +
                    ggplot2::ggtitle(.("ComplexUpset Diagram of Selected Variables")) +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14)
                    )

                return(plot)
            },

            # Helper function for ggVennDiagram plotting (advanced)
            .plotGgVennDiagramHelper = function(mydata2, namescolumn2, ggtheme, theme) {
                private$.checkpoint()
                # ggVennDiagram expects a list of vectors containing row indices where each variable is TRUE
                # Convert dataframe format to list format

                # First convert to logical if needed
                if (all(sapply(mydata2, function(x) all(x %in% c(0, 1))))) {
                    mydata2 <- mydata2 %>%
                        dplyr::mutate(dplyr::across(.cols = dplyr::everything(), ~ as.logical(.)))
                }

                # Convert to list format required by ggVennDiagram
                venn_list <- list()
                for (col_name in namescolumn2) {
                    if (col_name %in% names(mydata2)) {
                        # Get row indices where this variable is TRUE
                        true_indices <- which(mydata2[[col_name]] == TRUE)
                        venn_list[[col_name]] <- true_indices
                    }
                }

                # Use the list format for ggVennDiagram
                mydata2 <- venn_list

                # Get user options for ggVennDiagram
                shapeType <- self$options$shapeType
                regionLabels <- self$options$regionLabels
                labelGeometry <- self$options$labelGeometry
                labelPrecisionDigits <- self$options$labelPrecisionDigits
                setNameSize <- self$options$setNameSize
                labelSize <- self$options$labelSize
                edgeSize <- self$options$edgeSize
                edgeColor <- self$options$edgeColor
                edgeLineType <- self$options$edgeLineType
                edgeAlpha <- self$options$edgeAlpha
                fillAlpha <- self$options$fillAlpha
                showSetLabels <- self$options$showSetLabels
                setLabelColor <- self$options$setLabelColor
                fillColorMapping <- self$options$fillColorMapping
                colorPalette <- self$options$colorPalette

                # Determine shape parameters based on number of sets and user selection
                num_sets <- length(mydata2)
                shape_params <- list()

                if (shapeType == "auto") {
                    # Let ggVennDiagram choose the best shape automatically
                    # Don't set any shape parameter
                } else if (shapeType == "circle") {
                    # Circle works for 2-4 sets
                    if (num_sets <= 4) {
                        shape_params$type <- "circle"
                    }
                } else if (shapeType == "ellipse") {
                    # Ellipse works for 2-5 sets
                    if (num_sets <= 5) {
                        shape_params$type <- "ellipse"
                    }
                } else if (shapeType == "triangle") {
                    # Triangle only works for exactly 3 sets
                    if (num_sets == 3) {
                        shape_params$type <- "triangle"
                    }
                } else if (shapeType == "polygon") {
                    # Polygon works for 4+ sets
                    if (num_sets >= 4) {
                        shape_params$type <- "polygon"
                    }
                }

                # Get original names for display using name mapping if available
                display_names <- namescolumn2
                if (!is.null(private$.name_mapping)) {
                    display_names <- sapply(namescolumn2, function(name) {
                        if (name %in% names(private$.name_mapping)) {
                            private$.name_mapping[[name]]
                        } else {
                            name
                        }
                    })
                }

                # Create the base ggVennDiagram plot with advanced options
                plot_args <- list(
                    x = mydata2,
                    category.names = if (showSetLabels) display_names else NULL,
                    label = regionLabels,
                    label_geom = labelGeometry,
                    label_percent_digit = labelPrecisionDigits,
                    label_size = labelSize,
                    set_name_size = setNameSize,
                    edge_size = edgeSize,
                    edge_lty = edgeLineType,
                    edge_alpha = edgeAlpha,
                    set_color = edgeColor  # Apply edge color to set boundaries
                )

                # Add shape parameters if specified
                if (length(shape_params) > 0) {
                    plot_args <- c(plot_args, shape_params)
                }

                # Create the plot
                plot <- do.call(ggVennDiagram::ggVennDiagram, plot_args)

                # Determine base fill colours from the jamovi theme or use defaults
                base_fill_colors <- theme$fill
                if (is.null(base_fill_colors) || length(base_fill_colors) == 0) {
                    base_fill_colors <- c("#FFFFFF", "#79A6EA")
                } else if (length(base_fill_colors) == 1) {
                    base_fill_colors <- rep(base_fill_colors[1], 2)
                } else {
                    base_fill_colors <- base_fill_colors[1:2]
                }

                build_palette_scale <- function() {
                    if (!isTRUE(fillColorMapping) || colorPalette == "default") {
                        return(NULL)
                    }

                    tryCatch({
                        if (colorPalette %in% c("viridis", "plasma", "magma", "inferno", "cividis")) {
                            palette_fun <- switch(colorPalette,
                                "viridis" = viridis::viridis,
                                "plasma" = viridis::plasma,
                                "magma" = viridis::magma,
                                "inferno" = viridis::inferno,
                                "cividis" = viridis::cividis
                            )
                            cols <- palette_fun(6)
                            cols <- grDevices::adjustcolor(cols, alpha.f = fillAlpha)
                            return(ggplot2::scale_fill_gradientn(colours = cols, guide = "none"))
                        }

                        if (requireNamespace("RColorBrewer", quietly = TRUE) &&
                            colorPalette %in% rownames(RColorBrewer::brewer.pal.info)) {
                            max_colors <- RColorBrewer::brewer.pal.info[colorPalette, "maxcolors"]
                            cols <- RColorBrewer::brewer.pal(n = min(9, max_colors), name = colorPalette)
                            cols <- grDevices::adjustcolor(cols, alpha.f = fillAlpha)
                            return(ggplot2::scale_fill_gradientn(colours = cols, guide = "none"))
                        }

                        NULL
                    }, error = function(e) NULL)
                }

                palette_scale <- build_palette_scale()
                if (is.null(palette_scale)) {
                    adjusted_colors <- grDevices::adjustcolor(base_fill_colors, alpha.f = fillAlpha)
                    palette_scale <- ggplot2::scale_fill_gradient(
                        low = adjusted_colors[1],
                        high = adjusted_colors[length(adjusted_colors)],
                        guide = "none"
                    )
                }

                if (!is.null(ggtheme)) {
                    plot <- plot + ggtheme
                }

                if (!is.null(palette_scale)) {
                    plot <- plot + palette_scale
                }

                # Apply set label colour if it differs from default
                if (setLabelColor != "black") {
                    plot <- plot +
                        ggplot2::theme(
                            text = ggplot2::element_text(color = setLabelColor)
                        )
                }

                # Add title and remove axes for a cleaner Venn diagram display
                plot <- plot +
                    ggplot2::ggtitle(.("Advanced Venn Diagram of Selected Variables")) +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                        axis.line.x = ggplot2::element_blank(),
                        axis.text.x = ggplot2::element_blank(),
                        axis.ticks.x = ggplot2::element_blank(),
                        axis.title.x = ggplot2::element_blank(),
                        axis.line.y = ggplot2::element_blank(),
                        axis.text.y = ggplot2::element_blank(),
                        axis.ticks.y = ggplot2::element_blank(),
                        axis.title.y = ggplot2::element_blank()
                    )

                return(plot)
            },

            # Generate set calculations using ggVennDiagram functions
            .generateSetCalculations = function(mydata2, namescolumn2, summaryData) {
                tryCatch({
                    # Prepare calculations
                    calculations <- list()

                    # Create venn object for calculations if ggVennDiagram is available
                    if (requireNamespace("ggVennDiagram", quietly = TRUE)) {
                        # Convert dataframe to list format required by ggVennDiagram
                        # mydata2 comes as integers (0/1), convert to logical first
                        mydata_logical <- mydata2
                        for (col in names(mydata_logical)) {
                            mydata_logical[[col]] <- as.logical(mydata_logical[[col]])
                        }

                        # Convert to list of row indices
                        venn_list <- list()
                        for (col_name in namescolumn2) {
                            if (col_name %in% names(mydata_logical)) {
                                # Get row indices where this variable is TRUE
                                true_indices <- which(mydata_logical[[col_name]] == TRUE)
                                venn_list[[col_name]] <- true_indices
                            }
                        }

                        # Create venn object with the list format
                        venn_obj <- ggVennDiagram::Venn(venn_list)

                        # Calculate overlaps if requested
                        if (self$options$calculateOverlap) {
                            tryCatch({
                                overlaps <- ggVennDiagram::overlap(venn_obj, slice = "all")
                                if (!is.null(overlaps)) {
                                    calculations$overlaps <- overlaps
                                }
                            }, error = function(e) {
                                # If overlap calculation fails, continue without it
                                NULL
                            })
                        }

                        # Calculate unique members (discern) if requested
                        if (self$options$calculateDiscern) {
                            tryCatch({
                                unique_members <- ggVennDiagram::discern(venn_obj, slice = "all")
                                if (!is.null(unique_members)) {
                                    calculations$unique_members <- unique_members
                                }
                            }, error = function(e) {
                                # If discern calculation fails, continue without it
                                NULL
                            })
                        }

                        # Calculate union if requested
                        if (self$options$calculateUnite) {
                            tryCatch({
                                union_result <- ggVennDiagram::unite(venn_obj, slice = "all")
                                if (!is.null(union_result)) {
                                    calculations$union <- union_result
                                }
                            }, error = function(e) {
                                # If unite calculation fails, continue without it
                                NULL
                            })
                        }
                    }

                    # Format results for HTML output
                    html_content <- "<div class='set-calculations'>"
                    html_content <- paste0(html_content, "<h3>Set Calculations</h3>")

                    # Always show basic information about the sets
                    html_content <- paste0(html_content, "<h4>Set Information:</h4>")
                    total_observations <- nrow(mydata2)
                    html_content <- paste0(html_content,
                        "<p><strong>Total observations:</strong> ", total_observations, "</p>")
                    html_content <- paste0(html_content,
                        "<p><strong>Number of sets:</strong> ", length(namescolumn2), "</p>")

                    if (length(calculations) > 0) {
                        if (!is.null(calculations$overlaps)) {
                            html_content <- paste0(html_content, "<h4>Overlapping Members:</h4>")
                            # CRITICAL FIX: overlap() returns a NAMED VECTOR, not a list
                            overlaps <- calculations$overlaps
                            if (!is.null(overlaps) && length(overlaps) > 0) {
                                # Check if it's a named vector or list
                                overlap_names <- names(overlaps)
                                if (!is.null(overlap_names) && length(overlap_names) > 0) {
                                    # It's a named vector - process as vector
                                    for (i in seq_along(overlaps)) {
                                        set_name <- overlap_names[i]
                                        count <- overlaps[i]
                                        # Use original name for display if available
                                        display_name <- set_name
                                        if (!is.null(private$.name_mapping) && set_name %in% names(private$.name_mapping)) {
                                            display_name <- private$.name_mapping[[set_name]]
                                        }
                                        html_content <- paste0(html_content,
                                            "<p><strong>", display_name, ":</strong> ",
                                            count, " members (",
                                            round(count/total_observations*100, 1), "%)</p>")
                                    }
                                } else if (is.list(overlaps)) {
                                    # It's a list - process as list
                                    for (i in seq_along(overlaps)) {
                                        set_name <- names(overlaps)[i]
                                        display_name <- set_name
                                        if (!is.null(private$.name_mapping) && set_name %in% names(private$.name_mapping)) {
                                            display_name <- private$.name_mapping[[set_name]]
                                        }
                                        members <- overlaps[[i]]
                                        html_content <- paste0(html_content,
                                            "<p><strong>", display_name, ":</strong> ",
                                            length(members), " members (",
                                            round(length(members)/total_observations*100, 1), "%)</p>")
                                    }
                                }
                            } else {
                                html_content <- paste0(html_content, "<p>No overlaps found.</p>")
                            }
                        }

                        if (!is.null(calculations$unique_members)) {
                            html_content <- paste0(html_content, "<h4>Unique Members per Set:</h4>")
                            # CRITICAL FIX: discern() returns a NAMED VECTOR or LIST - handle both
                            unique_members <- calculations$unique_members
                            if (!is.null(unique_members) && length(unique_members) > 0) {
                                # Check if it's a named vector or list
                                member_names <- names(unique_members)
                                if (!is.null(member_names) && length(member_names) > 0 && !is.list(unique_members)) {
                                    # It's a named vector - process as vector
                                    for (i in seq_along(unique_members)) {
                                        set_name <- member_names[i]
                                        count <- unique_members[i]
                                        # Use original name for display if available
                                        display_name <- set_name
                                        if (!is.null(private$.name_mapping) && set_name %in% names(private$.name_mapping)) {
                                            display_name <- private$.name_mapping[[set_name]]
                                        }
                                        html_content <- paste0(html_content,
                                            "<p><strong>", display_name, ":</strong> ",
                                            count, " unique members (",
                                            round(count/total_observations*100, 1), "%)</p>")
                                    }
                                } else if (is.list(unique_members)) {
                                    # It's a list - process as list
                                    for (i in seq_along(unique_members)) {
                                        set_name <- names(unique_members)[i]
                                        display_name <- set_name
                                        if (!is.null(private$.name_mapping) && set_name %in% names(private$.name_mapping)) {
                                            display_name <- private$.name_mapping[[set_name]]
                                        }
                                        members <- unique_members[[i]]
                                        html_content <- paste0(html_content,
                                            "<p><strong>", display_name, ":</strong> ",
                                            length(members), " unique members (",
                                            round(length(members)/total_observations*100, 1), "%)</p>")
                                    }
                                }
                            } else {
                                html_content <- paste0(html_content, "<p>No unique members found.</p>")
                            }
                        }

                        if (!is.null(calculations$union)) {
                            html_content <- paste0(html_content, "<h4>Union of All Sets:</h4>")
                            union_size <- length(calculations$union)
                            html_content <- paste0(html_content,
                                "<p><strong>Total unique items across all sets:</strong> ",
                                union_size, " items (",
                                round(union_size/total_observations*100, 1), "%)</p>")
                        }
                    } else {
                        html_content <- paste0(html_content,
                            "<div style='background: #fff3cd; padding: 10px; border: 1px solid #ffeaa7; border-radius: 4px; margin: 10px 0;'>",
                            "<p><strong>📊 Enable Calculations:</strong></p>",
                            "<p>To see detailed set calculations, please enable the specific options:</p>",
                            "<ul>",
                            "<li><strong>Calculate Overlaps:</strong> Shows intersection members</li>",
                            "<li><strong>Calculate Unique Members:</strong> Shows members exclusive to each set</li>",
                            "<li><strong>Calculate Unions:</strong> Shows combined membership across all sets</li>",
                            "</ul>",
                            "</div>")
                    }

                    html_content <- paste0(html_content, "</div>")

                    # Set the content
                    self$results$setCalculations$setContent(html_content)

                }, error = function(e) {
                    # If calculations fail, show error message
                    error_html <- paste0("<div class='error'>",
                        "<p>Error in set calculations: ", e$message, "</p>",
                        "<p>This feature requires ggVennDiagram package.</p>",
                        "</div>")
                    self$results$setCalculations$setContent(error_html)
                })
            },

            # Generate membership table showing which items belong to which sets
            .generateMembershipTable = function(mydata, safe_names, name_mapping, row_numbers) {
                tryCatch({
                    if (length(safe_names) < 2) {
                        return()
                    }

                    table <- self$results$membershipTable
                    if (is.null(table)) {
                        return()
                    }

                    # Build membership data with the display names that will appear in the table header
                    membership_data <- data.frame(Row = seq_len(nrow(mydata)))
                    group_labels <- NULL
                    for (safe_name in safe_names) {
                        if (safe_name %in% names(mydata)) {
                            display_name <- if (!is.null(name_mapping) && safe_name %in% names(name_mapping)) {
                                name_mapping[[safe_name]]
                            } else {
                                safe_name
                            }
                            membership_data[[display_name]] <- ifelse(mydata[[safe_name]], "Yes", "No")
                        }
                    }

                    set_columns <- names(membership_data)[names(membership_data) != "Row"]
                    if (length(set_columns) > 0) {
                        group_labels <- vapply(seq_len(nrow(membership_data)), function(i) {
                            row_values <- membership_data[i, set_columns, drop = FALSE]
                            positives <- set_columns[unlist(row_values, use.names = FALSE) == "Yes"]
                            if (length(positives) == 0) {
                                "None"
                            } else {
                                paste(positives, collapse = " & ")
                            }
                        }, FUN.VALUE = character(1))

                        membership_data$Group <- group_labels
                        membership_data <- membership_data[, c("Row", "Group", set_columns), drop = FALSE]
                    } else {
                        group_labels <- rep("None", nrow(membership_data))
                    }

                    if (nrow(membership_data) == 0L) {
                        table$deleteRows()
                        return()
                    }

                    state_data <- membership_data

                    # Map display names to safe column identifiers used internally by jamovi
                    original_names <- names(membership_data)
                    safe_col_names <- make.names(original_names, unique = TRUE)

                    # Ensure required columns exist (add only when missing to avoid duplication)
                    existing_cols <- character(0)
                    try({ existing_cols <- names(table$columns) }, silent = TRUE)

                    for (i in seq_along(original_names)) {
                        if (is.na(match(safe_col_names[i], existing_cols))) {
                            table$addColumn(
                                name = safe_col_names[i],
                                title = original_names[i],
                                type = if (original_names[i] == "Row") "integer" else "text"
                            )
                            existing_cols <- c(existing_cols, safe_col_names[i])
                        } else {
                            column <- NULL
                            try({ column <- table$getColumn(safe_col_names[i]) }, silent = TRUE)
                            if (!is.null(column)) {
                                try({ column$title <- original_names[i] }, silent = TRUE)
                            }
                        }
                    }

                    # Align data frame column names with the safe identifiers
                    names(membership_data) <- safe_col_names

                    table$setState(state_data)
                    table$deleteRows()

                    column_mapping <- stats::setNames(safe_col_names, safe_col_names)
                    private$.populateTableSafely(table, membership_data, column_mapping)

                    try({ table$setNote(key = "error", note = NULL) }, silent = TRUE)

                    if (!is.null(group_labels) && self$options$membershipGroups && self$results$membershipGroups$isNotFilled()) {
                        try({
                            output_rows <- row_numbers
                            if (length(output_rows) != length(group_labels)) {
                                output_rows <- seq_len(length(group_labels))
                            }
                            self$results$membershipGroups$setRowNums(output_rows)
                            self$results$membershipGroups$setValues(group_labels)
                        }, silent = TRUE)
                    }

                }, error = function(e) {
                    self$results$membershipTable$setNote(
                        key = "error",
                        note = paste("Error in generating membership table:", e$message)
                    )
                })
            },

            # Utility function for escaping variable names with special characters
            .escapeVar = function(x) {
                # mimic modelbuilder behavior for safe variable naming
                gsub("[^A-Za-z0-9_]+", "_", make.names(x))
            },

            # Populate jamovi tables safely using a data frame and column mapping
            .populateTableSafely = function(table_result, data_frame, column_mapping) {
                tryCatch({
                    if (is.null(data_frame) || nrow(data_frame) == 0) {
                        return(invisible(NULL))
                    }

                    for (i in seq_len(nrow(data_frame))) {
                        row_values <- list()

                        for (col_name in names(column_mapping)) {
                            source_col <- column_mapping[[col_name]]
                            if (source_col %in% names(data_frame)) {
                                row_values[[col_name]] <- data_frame[[source_col]][i]
                            } else {
                                row_values[[col_name]] <- NA
                            }
                        }

                        table_result$addRow(rowKey = i, values = row_values)

                        if (i %% 100 == 0) {
                            private$.checkpoint()
                        }
                    }
                }, error = function(e) {
                    warning(paste(.("Table population failed:"), e$message))
                })
            },

            # Generate clinical interpretation of overlap patterns
            .generateClinicalInterpretation = function(intersection_data, var_names, total_n) {
                if (is.null(intersection_data) || nrow(intersection_data) < 2) return("")

                # Find largest and most meaningful overlaps
                largest_count <- max(intersection_data$TrueCount, na.rm = TRUE)
                largest_var <- intersection_data$Variable[which.max(intersection_data$TrueCount)]
                largest_pct <- round((largest_count / total_n) * 100, 1)

                # Calculate overall overlap assessment
                mean_overlap <- mean(intersection_data$TrueCount, na.rm = TRUE)
                overlap_level <- if (mean_overlap / total_n > 0.5) "high" else if (mean_overlap / total_n > 0.2) "moderate" else "low"

                interpretation <- paste0(
                    "<div style='background: #f8f9fa; padding: 15px; border-left: 4px solid #28a745; margin: 10px 0; border-radius: 4px;'>",
                    "<h5 style='margin: 0 0 10px 0; color: #155724;'>🔬 Clinical Interpretation</h5>",
                    "<p style='margin: 0 0 8px 0;'><strong>Key Finding:</strong> In this dataset of ", total_n, " cases, ",
                    "'", largest_var, "' shows the highest prevalence with ", largest_count, " positive cases (", largest_pct, "%).</p>",
                    "<p style='margin: 0 0 8px 0;'><strong>Overlap Pattern:</strong> The variables show ", overlap_level, " levels of intersection, ",
                    if (overlap_level == "high") "suggesting strong associations between the measured characteristics."
                    else if (overlap_level == "moderate") "indicating meaningful but not dominant relationships."
                    else "suggesting the variables capture largely distinct characteristics.", "</p>",
                    "<p style='margin: 0; font-size: 0.9em; color: #6c757d;'>",
                    "💡 <em>Clinical Relevance:</em> Use Venn diagrams to identify patient subgroups, assess diagnostic overlap, ",
                    "or evaluate multi-marker patterns in pathology and oncology research.</p>",
                    "</div>"
                )

                return(interpretation)
            },

            # Validate statistical power and provide warnings
            .validateStatisticalPower = function(intersection_data, total_n) {
                warnings <- c()

                # Check for small intersection sizes
                if (!is.null(intersection_data) && nrow(intersection_data) > 0) {
                    small_counts <- sum(intersection_data$TrueCount < 5, na.rm = TRUE)
                    if (small_counts > 0) {
                        warnings <- c(warnings, paste0(
                            "⚠️ <strong>Small Sample Warning:</strong> ", small_counts, " variable(s) have fewer than 5 positive cases. ",
                            "Consider combining categories or collecting additional data for robust statistical inference."
                        ))
                    }

                    # Check for very low prevalence
                    low_prev <- sum((intersection_data$TrueCount / total_n) < 0.05, na.rm = TRUE)
                    if (low_prev > 0) {
                        warnings <- c(warnings, paste0(
                            "📊 <strong>Low Prevalence Note:</strong> ", low_prev, " variable(s) have prevalence below 5%. ",
                            "Venn diagram patterns may be difficult to interpret visually."
                        ))
                    }

                    # Check for total sample size
                    if (total_n < 30) {
                        warnings <- c(warnings, paste0(
                            "📈 <strong>Sample Size Advisory:</strong> With only ", total_n, " cases, overlap patterns should be interpreted cautiously. ",
                            "Consider this as exploratory analysis requiring validation in larger samples."
                        ))
                    }
                }

                if (length(warnings) > 0) {
                    warning_html <- paste0(
                        "<div style='background: #fff3cd; padding: 12px; border-left: 4px solid #ffc107; margin: 10px 0; border-radius: 4px;'>",
                        "<h6 style='margin: 0 0 8px 0; color: #856404;'>Statistical Considerations</h6>",
                        paste(warnings, collapse = "<br><br>"),
                        "</div>"
                    )
                    return(warning_html)
                }

                return("")
            },

            # Generate statistical glossary
            .generateGlossary = function() {
                glossary_content <- paste0(
                    "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 4px solid #6f42c1;'>",
                    "<h4 style='color: #6f42c1; margin-top: 0;'>📚 Statistical Glossary & Clinical Guide</h4>",

                    "<div style='display: grid; grid-template-columns: 1fr 1fr; gap: 15px; margin-bottom: 15px;'>",

                    # Venn Diagram Terms
                    "<div style='background: white; padding: 12px; border-radius: 6px; border: 1px solid #e9ecef;'>",
                    "<h6 style='margin: 0 0 8px 0; color: #495057; border-bottom: 1px solid #dee2e6; padding-bottom: 4px;'>Venn Diagram Terms</h6>",
                    "<p style='margin: 0 0 6px 0; font-size: 0.9em;'><strong>Intersection:</strong> Cases positive for multiple variables simultaneously (overlap regions)</p>",
                    "<p style='margin: 0 0 6px 0; font-size: 0.9em;'><strong>Union:</strong> Cases positive for any of the variables (total covered area)</p>",
                    "<p style='margin: 0 0 6px 0; font-size: 0.9em;'><strong>Exclusive:</strong> Cases positive for only one specific variable</p>",
                    "<p style='margin: 0; font-size: 0.9em;'><strong>Complement:</strong> Cases negative for all variables</p>",
                    "</div>",

                    # Clinical Applications
                    "<div style='background: white; padding: 12px; border-radius: 6px; border: 1px solid #e9ecef;'>",
                    "<h6 style='margin: 0 0 8px 0; color: #495057; border-bottom: 1px solid #dee2e6; padding-bottom: 4px;'>Clinical Applications</h6>",
                    "<p style='margin: 0 0 6px 0; font-size: 0.9em;'><strong>Biomarker Analysis:</strong> Assess multi-marker expression patterns in tumors</p>",
                    "<p style='margin: 0 0 6px 0; font-size: 0.9em;'><strong>Diagnostic Overlap:</strong> Evaluate concordance between different diagnostic methods</p>",
                    "<p style='margin: 0 0 6px 0; font-size: 0.9em;'><strong>Risk Stratification:</strong> Identify patient subgroups with multiple risk factors</p>",
                    "<p style='margin: 0; font-size: 0.9em;'><strong>Treatment Response:</strong> Compare response across different outcome measures</p>",
                    "</div>",

                    "</div>",

                    # Plot Types Explanation
                    "<div style='background: white; padding: 12px; border-radius: 6px; border: 1px solid #e9ecef; margin-bottom: 15px;'>",
                    "<h6 style='margin: 0 0 8px 0; color: #495057; border-bottom: 1px solid #dee2e6; padding-bottom: 4px;'>Plot Type Selection Guide</h6>",
                    "<div style='display: grid; grid-template-columns: 1fr 1fr; gap: 10px;'>",
                    "<div>",
                    "<p style='margin: 0 0 4px 0; font-size: 0.9em;'><strong>ggvenn:</strong> Simple, classic Venn diagrams for 2-3 variables</p>",
                    "<p style='margin: 0 0 4px 0; font-size: 0.9em;'><strong>ggVennDiagram:</strong> Advanced Venn with customization options</p>",
                    "</div>",
                    "<div>",
                    "<p style='margin: 0 0 4px 0; font-size: 0.9em;'><strong>UpSetR:</strong> Matrix-style plots for 4+ variables</p>",
                    "<p style='margin: 0; font-size: 0.9em;'><strong>ComplexUpset:</strong> Enhanced UpSet with statistical annotations</p>",
                    "</div>",
                    "</div>",
                    "</div>",

                    # Statistical Considerations
                    "<div style='background: #fff3cd; padding: 12px; border-radius: 6px; border: 1px solid #ffeaa7;'>",
                    "<h6 style='margin: 0 0 8px 0; color: #856404;'>⚠️ Statistical Considerations</h6>",
                    "<p style='margin: 0 0 6px 0; font-size: 0.9em;'><strong>Sample Size:</strong> Ensure adequate cases in each intersection for reliable interpretation</p>",
                    "<p style='margin: 0 0 6px 0; font-size: 0.9em;'><strong>Independence:</strong> Venn diagrams show overlap but don't imply causal relationships</p>",
                    "<p style='margin: 0; font-size: 0.9em;'><strong>Clinical Context:</strong> Always interpret results within appropriate clinical and biological context</p>",
                    "</div>",

                    "</div>"
                )

                self$results$glossary$setContent(glossary_content)
            },

            # Helper function to display accumulated notices as HTML
            .displayNotices = function() {
                # Display errors
                if (length(private$.errors) > 0) {
                    error_html <- paste(
                        "<div style='padding: 15px; background-color: #f8d7da; border-left: 4px solid #dc3545; border-radius: 4px;'>",
                        "<h4 style='margin-top: 0; color: #721c24;'>⛔ Validation Errors</h4>",
                        paste(sprintf("<p style='margin: 5px 0; color: #721c24;'>• %s</p>", private$.errors), collapse = ""),
                        "</div>",
                        sep = ""
                    )
                    self$results$validationErrors$setContent(error_html)
                    self$results$validationErrors$setVisible(TRUE)
                }

                # Display warnings
                if (length(private$.warnings) > 0) {
                    warning_html <- paste(
                        "<div style='padding: 15px; background-color: #fff3cd; border-left: 4px solid #ffc107; border-radius: 4px;'>",
                        "<h4 style='margin-top: 0; color: #856404;'>⚠️ Important Warnings</h4>",
                        paste(sprintf("<p style='margin: 5px 0; color: #856404;'>• %s</p>", private$.warnings), collapse = ""),
                        "</div>",
                        sep = ""
                    )
                    self$results$validationWarnings$setContent(warning_html)
                    self$results$validationWarnings$setVisible(TRUE)
                }

                # Display info messages
                if (length(private$.info) > 0) {
                    info_html <- paste(
                        "<div style='padding: 15px; background-color: #d1ecf1; border-left: 4px solid #17a2b8; border-radius: 4px;'>",
                        "<h4 style='margin-top: 0; color: #0c5460;'>ℹ️ Analysis Information</h4>",
                        paste(sprintf("<p style='margin: 5px 0; color: #0c5460;'>• %s</p>", private$.info), collapse = ""),
                        "</div>",
                        sep = ""
                    )
                    self$results$analysisInfo$setContent(info_html)
                    self$results$analysisInfo$setVisible(TRUE)
                }
            },

            # Private field to store exclusion warning message
            .excluded_warning = NULL
        ), # End of private list
        public = list(
            #' @description
            #' Generate R source code for venn diagram analysis
            #' @return Character string with R syntax for reproducible analysis
            asSource = function() {
                # Get arguments
                args <- private$.asArgs(incData = FALSE)
                if (args != '')
                    args <- paste0(',\n    ', args)

                # Get package name dynamically
                pkg_name <- utils::packageName()
                if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

                # Build complete function call
                paste0(pkg_name, '::venn(\n    data = data', args, ')')
            }
        ) # End of public list
    )
