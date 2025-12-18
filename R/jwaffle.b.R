#' @title Waffle Charts for Categorical Data Visualization
#'
#' @description
#' Creates professional waffle charts (square pie charts) to visualize categorical
#' distributions using a grid of colored squares. Each square represents a fixed
#' proportion of the total, making it ideal for showing parts-of-whole relationships
#' in clinical and pathological data.
#'
#' @param data Data frame containing categorical variables
#' @param groups Categorical grouping variable (required). Each category will be
#'   displayed as proportional colored squares in the waffle grid. Examples: Tumor
#'   grade (G1/G2/G3), Treatment outcome (Complete/Partial/No response), Risk
#'   categories (Low/Medium/High).
#' @param counts Optional numeric weight variable for pre-aggregated data. If not
#'   provided, each row counts equally (recommended for most clinical data). Only
#'   needed if your data is pre-aggregated or weighted.
#' @param facet Optional categorical variable to create separate waffle charts by
#'   subgroups (e.g., treatment arm, patient cohort, time period). Creates separate
#'   waffle charts for each level to compare distributions across subgroups.
#' @param rows Number of rows in the waffle chart grid (default: 5 for 10x10 grid).
#'   With 10 columns, this creates 100 squares where each represents approximately
#'   1% of the sample.
#' @param flip Logical, whether to flip the orientation of the waffle chart
#'   (default: FALSE). When TRUE, flips rows and columns.
#' @param color_palette Color scheme for the waffle squares. Options: "default",
#'   "colorblind" (recommended for accessibility), "professional", "presentation",
#'   "journal", "pastel", or "dark". Default uses blue-orange gradient.
#' @param show_legend Logical, whether to display the legend (default: FALSE).
#'   Enable to show category labels and colors.
#' @param mytitle Custom title for the plot (default: empty). If provided, appears
#'   at top of the chart.
#' @param legendtitle Custom title for the legend (default: empty). If not provided,
#'   uses the grouping variable name.
#' @param showSummaries Logical, generate natural language summary of waffle chart
#'   results including proportions, dominant categories, and clinical interpretation
#'   (default: FALSE).
#' @param showExplanations Logical, show detailed methodology explanations about
#'   waffle charts, when to use them, and how to interpret the results in clinical
#'   contexts (default: FALSE).
#'
#' @return A jamovi analysis object with waffle chart visualization, optional summary,
#'   and optional methodology explanation
#'
#' @details
#' **Data Requirements:**
#' - Categorical grouping variable (factor, character, or logical)
#' - Minimum 30 cases recommended for stable proportions
#' - 2-10 categories optimal for visual clarity (>10 categories may be cluttered)
#' - Haven labelled data automatically converted to factors
#'
#' **How Waffle Charts Work:**
#' A waffle chart uses a grid of colored squares (typically 10x10 = 100 squares)
#' where each square represents a fixed proportion of the total sample. This makes
#' percentages immediately intuitive - each square ≈ 1% of the sample.
#'
#' **Clinical Applications:**
#' - **Disease Classification:** Show distribution of tumor grades, cancer stages,
#'   or pathological subtypes
#' - **Treatment Outcomes:** Display response rates (complete/partial/no response)
#'   across patient cohorts
#' - **Demographic Analysis:** Present patient characteristics, risk factors, or
#'   comorbidity patterns
#' - **Quality Metrics:** Visualize compliance rates, diagnostic accuracy, or
#'   safety outcomes
#'
#' **Advantages over Other Charts:**
#' - More intuitive than pie charts for showing proportions
#' - Each square = 1% makes percentages immediately clear
#' - Handles many categories better than bar charts
#' - Effective for presentations and publications
#' - Faceting enables subgroup comparisons
#'
#' **Statistical Considerations:**
#' - Most effective with n≥30; smaller samples may show unstable proportions
#' - Works best when no single category dominates (>80%)
#' - Each square represents approximately 1% of the sample
#' - Categories with <5 cases may be statistically unreliable
#' - Chi-square tests can evaluate proportion differences between groups
#'
#' @section Performance Optimization:
#' The function implements sophisticated caching:
#' - Data aggregation cached based on variable selection and data content
#' - Color palettes cached separately to minimize recomputation
#' - Plot state management prevents unnecessary regeneration
#' - Automatic width scaling for faceted plots
#'
#' @section Clinical Validation:
#' The function performs comprehensive data quality checks:
#' - Validates categorical data types (with automatic labelled data conversion)
#' - Detects single-category data (requires multiple categories)
#' - Warns about many categories (>10 may need grouping)
#' - Alerts to small samples (<30 cases)
#' - Identifies rare categories (<5 cases)
#' - Checks for negative count values (must be non-negative)
#'
#' @examples
#' \dontrun{
#' # Basic tumor grade distribution
#' jwaffle(
#'     data = pathology_data,
#'     groups = "TumorGrade",
#'     color_palette = "colorblind",
#'     show_legend = TRUE,
#'     showSummaries = TRUE
#' )
#'
#' # Treatment response by cohort with faceting
#' jwaffle(
#'     data = clinical_data,
#'     groups = "Response",
#'     facet = "TreatmentArm",
#'     color_palette = "professional",
#'     mytitle = "Treatment Response Rates by Arm",
#'     showSummaries = TRUE
#' )
#'
#' # Weighted risk distribution
#' jwaffle(
#'     data = aggregated_data,
#'     groups = "RiskCategory",
#'     counts = "PatientCount",
#'     color_palette = "presentation",
#'     legendtitle = "Risk Level"
#' )
#' }
#'
#' @references
#' Wilke, C. O. (2019). waffle: Create Waffle Chart Visualizations in R.
#'   R package version 1.0.1.
#'
#' @seealso
#' \code{\link[waffle]{geom_waffle}} for the underlying waffle geom
#'
#' @family JJStatsPlot visualization functions
#' @keywords hplot distribution categorical
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import waffle
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom glue glue
#' @import scales
#' @importFrom rlang sym
#' @importFrom digest digest
#'
#' @export


jwaffleClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jwaffleClass",
    inherit = jwaffleBase,
    private = list(
        # Performance optimization: cache variables
        .prepared_data = NULL,
        .prepared_options = NULL,
        .data_hash = NULL,
        .options_hash = NULL,
        .cached_plot = NULL,
        .cached_palette = NULL,
        .messages = NULL,
        
        .init = function() {
            # Reset messages for new analysis
            private$.resetMessages()
            
            base_width <- 600
            base_height <- 500
            
            if (!is.null(self$options$facet) && !is.null(self$data)) {
                facet_var <- self$options$facet
                if (facet_var %in% names(self$data)) {
                    num_levels <- length(unique(self$data[[facet_var]]))
                    width <- max(base_width, num_levels * base_width)
                    self$results$plot$setSize(width, base_height)
                } else {
                    self$results$plot$setSize(base_width, base_height)
                }
            } else {
                self$results$plot$setSize(base_width, base_height)
            }
        },

        # Enhanced message management system (dual output: HTML + Notice)
        .accumulateMessage = function(message, notice_type = "WARNING") {
            if (is.null(private$.messages)) {
                private$.messages <- character()
            }
            private$.messages <- append(private$.messages, message)

            # LEGACY: Keep HTML warnings for backward compatibility
            if (!is.null(self$results$warnings)) {
                self$results$warnings$setContent(paste(private$.messages, collapse = ""))
                self$results$warnings$setVisible(TRUE)
            }

            # MODERN: Also add as jmvcore::Notice for consistent UX
            # Combine all accumulated messages into a single notice to avoid clutter
            if (length(private$.messages) > 0) {
                # Clean messages for notice display
                combined_msg <- paste(private$.messages, collapse = "\n")
                clean_msg <- gsub("<br>|<br/>|<hr>", "\n", combined_msg)
                clean_msg <- gsub("<[^>]*>", "", clean_msg)
                clean_msg <- gsub("&bull;", "•", clean_msg)
                clean_msg <- gsub("&nbsp;", " ", clean_msg)
                clean_msg <- trimws(clean_msg)

                # Remove any existing accumulated_warnings notice before adding new one
                # (jamovi will handle duplicates, but this keeps it clean)
                tryCatch({
                    private$.addNotice(
                        content = clean_msg,
                        type = notice_type,
                        name = "accumulated_warnings"
                    )
                }, error = function(e) {
                    # Silent fail if notice system unavailable
                })
            }
        },
        
        # Reset messages for new analysis run
        .resetMessages = function() {
            private$.messages <- character()
            if (!is.null(self$results$warnings)) {
                self$results$warnings$setContent("")
            }
        },

        # Add jmvcore::Notice to results (modern notice system)
        .addNotice = function(content, type = "WARNING", name = NULL) {
            # Generate unique name if not provided
            if (is.null(name)) {
                if (requireNamespace("digest", quietly = TRUE)) {
                    name <- paste0("notice_", digest::digest(content, algo = "md5"))
                } else {
                    name <- paste0("notice_", sample(10000:99999, 1))
                }
            }

            # Map string types to jmvcore::NoticeType
            notice_type <- switch(type,
                "ERROR" = jmvcore::NoticeType$ERROR,
                "STRONG_WARNING" = jmvcore::NoticeType$STRONG_WARNING,
                "WARNING" = jmvcore::NoticeType$WARNING,
                "INFO" = jmvcore::NoticeType$INFO,
                jmvcore::NoticeType$WARNING  # Default
            )

            # Create notice
            notice <- jmvcore::Notice$new(
                options = self$options,
                name = name,
                type = notice_type
            )

            # Clean content for notice (remove HTML tags since notices don't support HTML)
            clean_content <- gsub("<br>|<br/>|<hr>", "\n", content)
            clean_content <- gsub("<[^>]*>", "", clean_content)
            clean_content <- gsub("&bull;", "•", clean_content)
            clean_content <- gsub("&nbsp;", " ", clean_content)
            # Remove multiple consecutive line breaks
            clean_content <- gsub("\n\n+", "\n\n", clean_content)
            # Trim leading/trailing whitespace
            clean_content <- trimws(clean_content)

            notice$setContent(clean_content)

            # Insert at beginning of results
            self$results$insert(1, notice)

            return(notice)
        },

        # Variable name safety utility
        .escapeVar = function(var) {
            if (is.null(var)) return(NULL)
            # Use jmvcore::composeTerm for variables with spaces/special chars
            if (grepl("[^A-Za-z0-9_]", var)) {
                jmvcore::composeTerm(var)
            } else {
                var
            }
        },

        # Performance optimization methods
        .calculateDataHash = function() {
            if (is.null(self$data) || nrow(self$data) == 0) {
                return(NULL)
            }

            # Determine relevant variables
            relevant_vars <- c()
            if (!is.null(self$options$groups)) relevant_vars <- c(relevant_vars, self$options$groups)
            if (!is.null(self$options$counts)) relevant_vars <- c(relevant_vars, self$options$counts)
            if (!is.null(self$options$facet)) relevant_vars <- c(relevant_vars, self$options$facet)

            # Remove NULLs and ensure variables exist
            relevant_vars <- relevant_vars[!sapply(relevant_vars, is.null)]
            relevant_vars <- relevant_vars[relevant_vars %in% names(self$data)]

            if (length(relevant_vars) == 0) {
                return(NULL)
            }

            # Extract only relevant columns for hashing
            relevant_data <- self$data[, relevant_vars, drop = FALSE]

            # Use digest to hash actual data values, not just metadata
            # This ensures cache invalidation when data values change
            if (requireNamespace("digest", quietly = TRUE)) {
                return(digest::digest(relevant_data, algo = "md5"))
            }

            # Fallback: create hash from serialized data
            return(paste(serialize(relevant_data, NULL), collapse = ""))
        },
        
        .calculateOptionsHash = function() {
            # Create hash of all relevant options for waffle chart
            options_list <- list(
                groups = self$options$groups,
                counts = self$options$counts,
                facet = self$options$facet,
                rows = self$options$rows,
                flip = self$options$flip,
                color_palette = self$options$color_palette,
                legendtitle = self$options$legendtitle,
                show_legend = self$options$show_legend,
                mytitle = self$options$mytitle,
                showSummaries = self$options$showSummaries,
                showExplanations = self$options$showExplanations
            )
            
            return(paste(options_list, collapse = "_"))
        },
        
        .canUseCache = function() {
            current_data_hash <- private$.calculateDataHash()
            current_options_hash <- private$.calculateOptionsHash()
            
            return(!is.null(private$.cached_plot) &&
                   !is.null(private$.data_hash) &&
                   !is.null(private$.options_hash) &&
                   !is.null(current_data_hash) &&
                   !is.null(current_options_hash) &&
                   current_data_hash == private$.data_hash &&
                   current_options_hash == private$.options_hash)
        },
        
        .prepareData = function() {
            current_hash <- private$.calculateDataHash()

            if (is.null(private$.data_hash) || private$.data_hash != current_hash) {
                # Data has changed, prepare new data
                mydata <- self$data

                if (is.null(mydata) || nrow(mydata) == 0) {
                    private$.prepared_data <- NULL
                    private$.data_hash <- current_hash
                    return(NULL)
                }

                # Clean data - only remove NA in relevant columns
                # Determine relevant variables
                relevant_vars <- c()
                if (!is.null(self$options$groups) && self$options$groups != "") {
                    relevant_vars <- c(relevant_vars, self$options$groups)
                }
                if (!is.null(self$options$counts) && self$options$counts != "") {
                    relevant_vars <- c(relevant_vars, self$options$counts)
                }
                if (!is.null(self$options$facet) && self$options$facet != "") {
                    relevant_vars <- c(relevant_vars, self$options$facet)
                }

                # Only remove rows with NA in relevant columns, not all columns
                if (length(relevant_vars) > 0) {
                    original_nrow <- nrow(mydata)

                    # Identify complete rows in relevant columns only
                    complete_rows <- complete.cases(mydata[, relevant_vars, drop = FALSE])
                    mydata <- mydata[complete_rows, , drop = FALSE]

                    rows_removed <- original_nrow - nrow(mydata)

                    if (rows_removed > 0) {
                        message(sprintf(
                            "Note: %d row(s) removed due to missing values in analysis variables (%s). %d row(s) remaining.",
                            rows_removed, paste(relevant_vars, collapse = ", "), nrow(mydata)
                        ))
                    }
                }

                private$.prepared_data <- mydata
                private$.data_hash <- current_hash
            }

            return(private$.prepared_data)
        },
        
        .prepareOptions = function() {
            current_hash <- private$.calculateOptionsHash()
            
            if (is.null(private$.options_hash) || private$.options_hash != current_hash) {
                private$.prepared_options <- self$options
                private$.options_hash <- current_hash
                
                # Clear cached results when options change
                private$.cached_plot <- NULL
                private$.cached_palette <- NULL
            }
            
            return(private$.prepared_options)
        },
        
        .generateColorPalette = function(n_groups) {
            # Early return for common cases
            if (n_groups == 2 && self$options$color_palette == "default") {
                return(c("#4DA6FF", "#FFB84D"))
            }
            if (n_groups == 3 && self$options$color_palette == "default") {
                return(c("#4DA6FF", "#FF9966", "#FFB84D"))
            }
            
            current_hash <- paste(private$.calculateOptionsHash(), n_groups, sep = "_")
            
            if (is.null(private$.cached_palette) || attr(private$.cached_palette, "hash") != current_hash) {
                # Enhanced color palettes with professional styling
                palettes <- list(
                    default = colorRampPalette(c("#4DA6FF", "#FFB84D"))(n_groups),
                    colorblind = colorRampPalette(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))(n_groups),
                    professional = colorRampPalette(c("#2C3E50", "#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6", "#1ABC9C", "#34495E"))(n_groups),
                    presentation = colorRampPalette(c("#003f5c", "#bc5090", "#ffa600", "#58508d", "#ff6361", "#003f5c"))(n_groups),
                    journal = colorRampPalette(c("#334455", "#778899", "#99AABB", "#BBCCDD", "#556677", "#667788"))(n_groups),
                    pastel = colorRampPalette(c("#69b3a2", "#404080", "#FFA07A", "#98D8E8", "#F7DC6F", "#BB8FCE"))(n_groups),
                    dark = colorRampPalette(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"))(n_groups)
                )
                
                selected_palette <- palettes[[self$options$color_palette]]
                if (is.null(selected_palette)) {
                    selected_palette <- palettes$default
                }
                
                attr(selected_palette, "hash") <- current_hash
                private$.cached_palette <- selected_palette
            }
            
            return(private$.cached_palette)
        },
        
        .generateCaption = function(plotdata, total_cases, is_weighted = FALSE, facet_var = NULL, facet_level = NULL) {
            # Handle edge case where there's no data
            if (total_cases == 0 || is.na(total_cases) || is.null(total_cases)) {
                return("No data available for waffle chart")
            }

            # Calculate waffle chart statistics for caption
            n_squares <- 100  # Total number of squares in waffle chart
            units_per_square <- total_cases / n_squares
            squares_per_unit <- 100 / total_cases  # Each square represents this percentage

            # Choose appropriate terminology
            unit_label <- if (is_weighted) "weighted units" else "cases"

            # Generate base caption
            if (!is.null(facet_var) && !is.null(facet_level)) {
                # Facet-specific caption
                caption_text <- sprintf(
                    "%s = %s: Each square ~ %.1f %s (%.1f%%) (total = %d)",
                    facet_var, facet_level, units_per_square, unit_label, squares_per_unit, total_cases
                )
            } else if (!is.null(facet_var)) {
                # General faceted caption (when not specific to a level)
                caption_text <- sprintf(
                    "Each square ~ %.1f %s per facet group (values vary by %s)",
                    units_per_square, unit_label, facet_var
                )
            } else {
                # Simple caption
                caption_text <- sprintf(
                    "Each square represents %.1f %s (approximately %.1f%%) (total n=%d)",
                    units_per_square, unit_label, squares_per_unit, total_cases
                )
            }
            
            if (!is.null(facet_var)) {
                caption_text <- paste0(
                    caption_text, 
                    "\nNote: Squares represent proportions within the specified group."
                )
            }

            return(caption_text)
        },
        
        .validateInputs = function() {
            # Check for large datasets and warn user
            if (nrow(self$data) > 100000) {
                warning("Large dataset detected (", nrow(self$data), " rows). Performance may be affected. Consider sampling or aggregating your data.")
            }
            
            # Check if required groups variable exists
            if (is.null(self$options$groups) || self$options$groups == "") {
                stop("Please specify a grouping variable for the waffle chart.")
            }
            
            if (!self$options$groups %in% names(self$data)) {
                stop(paste("Grouping variable '", self$options$groups, 
                          "' not found in data. Available variables: ", 
                          paste(names(self$data), collapse = ", ")))
            }
            
            # Check optional counts variable
            if (!is.null(self$options$counts) && self$options$counts != "" &&
                !self$options$counts %in% names(self$data)) {
                stop(paste("Counts variable '", self$options$counts, 
                          "' not found in data. Available variables: ", 
                          paste(names(self$data), collapse = ", ")))
            }
            
            # Check optional facet variable  
            if (!is.null(self$options$facet) && self$options$facet != "" &&
                !self$options$facet %in% names(self$data)) {
                stop(paste("Facet variable '", self$options$facet, 
                          "' not found in data. Available variables: ", 
                          paste(names(self$data), collapse = ", ")))
            }
            
            # Validate data types and handle labelled factors
            groups_data <- self$data[[self$options$groups]]

            # Handle labelled data (preserve labels for display)
            if (inherits(groups_data, "haven_labelled")) {
                groups_data <- haven::as_factor(groups_data, levels = "both")
            }

            # Convert to factor if character/logical
            if (is.character(groups_data) || is.logical(groups_data)) {
                groups_data <- as.factor(groups_data)
            }

            if (!is.factor(groups_data)) {
                stop(paste("Grouping variable '", self$options$groups,
                          "' must be categorical (factor, character, or logical), not ",
                          class(groups_data)[1]))
            }
            
            # Enhanced clinical validation checks
            n_categories <- length(unique(groups_data))
            n_total <- nrow(self$data)
            
            if (n_categories == 1) {
                stop(paste(
                    "Only one category found in '", self$options$groups, 
                    "'. Waffle charts require multiple categories to show proportions.",
                    "Consider using a different visualization for single-category data."
                ))
            }
            
            
            if (n_categories > 10) {
                private$.accumulateMessage(glue::glue(
                    "<br>⚠️ <strong>Many Categories:</strong> {n_categories} categories detected in '{self$options$groups}'. ",
                    "Consider grouping rare categories as 'Other' for clarity detailed clinical presentation.<br>"
                ))
            }
            
            # Check sample size adequacy for clinical interpretation
            if (n_total < 30) {
                private$.accumulateMessage(glue::glue(
                    "<br>⚠️ <strong>Small Sample:</strong> Total n={n_total}. Proportions may be unstable. ",
                    "Consider combining categories or collecting more data.<br>"
                ))
            }
            
            # Check for very small category sizes
            category_counts <- table(groups_data, useNA = "no")
            min_count <- min(category_counts)
            if (min_count < 5 && n_total >= 30) {
                small_cats <- names(category_counts)[category_counts < 5]
                private$.accumulateMessage(glue::glue(
                    "<br>⚠️ <strong>Rare Categories:</strong> Some categories have <5 cases: {paste(small_cats, collapse = ', ')}. ",
                    "Consider combining rare categories for more reliable clinical interpretation.<br>"
                ))
            }
            
            # Validate counts variable if specified
            if (!is.null(self$options$counts) && self$options$counts != "") {
                counts_data <- self$data[[self$options$counts]]
                if (!is.numeric(counts_data)) {
                    stop(paste("Counts variable '", self$options$counts,
                              "' must be numeric, not ", class(counts_data)[1]))
                }
                if (any(counts_data < 0, na.rm = TRUE)) {
                    n_negative <- sum(counts_data < 0, na.rm = TRUE)
                    stop(sprintf(
                        "Counts variable '%s' contains %d negative value(s). All counts must be non-negative for waffle charts. Please check your data.",
                        self$options$counts, n_negative
                    ))
                }
            }
        },
        
        .aggregateData = function(data, groups_var, facet_var = NULL, counts_var = NULL) {
            # Pre-check for empty data
            if (is.null(data) || nrow(data) == 0) {
                stop("Cannot aggregate empty dataset. Please ensure your data contains valid rows.")
            }
            
            # Build grouping variables
            group_vars <- c(groups_var)
            if (!is.null(facet_var) && facet_var != "") {
                group_vars <- c(group_vars, facet_var)
            }
            
            # Build count expression
            if (!is.null(counts_var) && counts_var != "") {
                count_expr <- rlang::expr(sum(!!rlang::sym(counts_var), na.rm = TRUE))
            } else {
                count_expr <- rlang::expr(dplyr::n())
            }
            
            # Aggregate data with error handling
            tryCatch({
                result <- data %>%
                    dplyr::group_by(!!!rlang::syms(group_vars)) %>%
                    dplyr::summarise(count = !!count_expr, .groups = 'drop') %>%
                    dplyr::ungroup()
                
                return(result)
            }, error = function(e) {
                stop(paste("Error aggregating data:", e$message, 
                          "Please check that your variables are properly formatted."))
            })
        },
        
        .generateSummary = function(plotdata, groups_var, facet_var = NULL, total_cases, is_weighted = FALSE) {
            # Generate natural language summary of waffle chart results
            if (is.null(plotdata) || nrow(plotdata) == 0 || total_cases == 0) {
                return("<b>No data available for analysis summary.</b>")
            }

            # Choose appropriate terminology
            unit_label <- if (is_weighted) "weighted units" else "cases"
            
            if (!is.null(facet_var) && facet_var != "" && facet_var %in% names(plotdata)) {
                # Faceted summary
                summary_parts <- list()
                
                for (facet_level in unique(plotdata[[facet_var]])) {
                    facet_data <- plotdata[plotdata[[facet_var]] == facet_level, ]
                    facet_total <- sum(facet_data$count)
                    proportions <- (facet_data$count / facet_total) * 100
                    
                    # Sort the data by groups_var to ensure consistent ordering
                    facet_data <- facet_data[order(facet_data[[groups_var]]), ]
                    proportions <- (facet_data$count / facet_total) * 100
                    
                    # Create breakdown showing all categories within this facet
                    category_breakdown <- paste(
                        sprintf("%s %s: %.1f%% (n=%d)", 
                               groups_var,
                               facet_data[[groups_var]], 
                               proportions, 
                               facet_data$count), 
                        collapse = "; "
                    )
                    
                    # Find the dominant category within this facet
                    max_prop_idx <- which.max(proportions)
                    dominant_category <- facet_data[[groups_var]][max_prop_idx]
                    max_proportion <- proportions[max_prop_idx]
                    
                    summary_parts[[facet_level]] <- sprintf(
                        "<b>Among %s %s</b> (n=%d): %s → <i>%s %s predominates (%.1f%%)</i>",
                        facet_var, facet_level, facet_total, category_breakdown,
                        groups_var, dominant_category, max_proportion
                    )
                }

                summary_text <- paste0(
                    "<b>📊 Waffle Chart Summary by ", facet_var, ":</b><br><br>",
                    paste(summary_parts, collapse = "<br><br>"),
                    "<br><br><b>🔍 Overall Sample:</b> ", total_cases, " ", unit_label, " showing ", groups_var,
                    " distribution across ", length(unique(plotdata[[facet_var]])), " ", facet_var, " groups.",
                    "<br><br><b>💡 Clinical Note:</b> Compare how ", groups_var,
                    " patterns differ between ", facet_var, " groups. Each square represents a fixed proportion of ", unit_label, "."
                )
                
            } else {
                # Simple summary
                proportions <- (plotdata$count / total_cases) * 100
                max_prop_idx <- which.max(proportions)
                dominant_category <- plotdata[[groups_var]][max_prop_idx]
                max_proportion <- proportions[max_prop_idx]
                
                # Create simple breakdown
                breakdown_list <- paste(
                    sprintf("%s: %.1f%% (n=%d)", 
                           plotdata[[groups_var]], 
                           proportions, 
                           plotdata$count), 
                    collapse = ", "
                )
                
                summary_text <- sprintf(
                    "<b>📊 Waffle Chart Summary:</b><br><br>
                    The sample contains %d %s distributed as: %s.<br><br>
                    <b>Key Finding:</b> %s represents the largest proportion (%.1f%% of %s).<br><br>
                    <b>💡 Report Template:</b><br>
                    <i>\"Distribution analysis revealed %s as the most frequent category (%.1f%%, n=%d) in our sample of %d %s.\"</i>",
                    total_cases, unit_label, breakdown_list, dominant_category, max_proportion, unit_label,
                    dominant_category, max_proportion, plotdata$count[max_prop_idx], total_cases, unit_label
                )
            }
            
            return(summary_text)
        },
        
        .generateExplanation = function() {
            # Generate detailed methodology explanation for waffle charts
            explanation <- paste0(
                "<b>Waffle Chart Methodology</b><br><br>",
                
                "<b>What is a Waffle Chart?</b><br>",
                "A waffle chart is a visual representation of categorical data using a grid of colored squares, ",
                "where each square represents a fixed proportion of the total sample. The chart typically uses ",
                "a 10×10 grid (100 squares) to represent percentages intuitively.<br><br>",
                
                "<b>Clinical Applications:</b><br>",
                "• <b>Disease Classification:</b> Show distribution of tumor grades, cancer stages, or pathological subtypes<br>",
                "• <b>Treatment Outcomes:</b> Display response rates (complete/partial/no response) across patient cohorts<br>",
                "• <b>Demographic Analysis:</b> Present patient characteristics, risk factors, or comorbidity patterns<br>",
                "• <b>Quality Metrics:</b> Visualize compliance rates, diagnostic accuracy, or safety outcomes<br><br>",
                
                "<b>Advantages over Other Charts:</b><br>",
                "• <b>Intuitive Interpretation:</b> Each square = 1% makes percentages immediately clear<br>",
                "• <b>Part-to-Whole Clarity:</b> Better than pie charts for showing proportions<br>",
                "• <b>Multiple Categories:</b> Handles many categories better than bar charts<br>",
                "• <b>Visual Impact:</b> Effective for presentations and publications<br><br>",
                
                "<b>Statistical Considerations:</b><br>",
                "• <b>Sample Size:</b> Most effective with n≥30; smaller samples may show unstable proportions<br>",
                "• <b>Category Balance:</b> Works best when no single category dominates (>80%)<br>",
                "• <b>Precision:</b> Each square represents approximately 1% of the sample<br>",
                "• <b>Statistical Testing:</b> Chi-square tests can evaluate proportion differences<br><br>",
                
                "<b>Interpretation Guidelines:</b><br>",
                "• <b>Dominant Patterns:</b> Categories with >60% suggest clear predominance<br>",
                "• <b>Balanced Distributions:</b> No category >40% indicates diverse sample<br>",
                "• <b>Rare Categories:</b> <5% of sample may represent clinically significant subgroups<br>",
                "• <b>Comparative Analysis:</b> Use faceting to compare across patient groups or time periods<br><br>",
                
                "<b>Best Practices for Clinical Research:</b><br>",
                "• Use clear, standardized category labels (e.g., 'Grade 1', 'Grade 2', 'Grade 3')<br>",
                "• Choose colorblind-friendly palettes for accessibility<br>",
                "• Include sample sizes and confidence intervals when reporting<br>",
                "• Consider combining rare categories (<5 cases) for statistical stability<br>",
                "• Always report exact percentages and counts alongside visual representation"
            )
            
            return(explanation)
        },
        
        
        .run = function() {
            if (is.null(self$options$groups)) {
                todo <- paste0(
                    "<div style='background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); ",
                    "padding: 30px; border-radius: 12px; color: white; margin: 20px 0; ",
                    "box-shadow: 0 8px 16px rgba(0,0,0,0.1);'>",

                    "<h2 style='margin-top: 0; font-size: 28px; font-weight: bold;'>",
                    "📊 Welcome to Waffle Charts</h2>",

                    "<p style='font-size: 16px; line-height: 1.8; margin: 20px 0;'>",
                    "Create professional waffle charts to visualize categorical distributions ",
                    "using colored squares in a grid format.</p>",

                    "<h3 style='font-size: 20px; margin-top: 25px; margin-bottom: 15px;'>",
                    "🎯 Getting Started:</h3>",
                    "<ol style='font-size: 15px; line-height: 2; margin-left: 20px;'>",
                    "<li><strong>Required:</strong> Select a <strong>Groups</strong> variable (categorical)</li>",
                    "<li><strong>Optional:</strong> Add <strong>Counts</strong> variable for weighted data</li>",
                    "<li><strong>Optional:</strong> Use <strong>Facet By</strong> to compare across subgroups</li>",
                    "</ol>",

                    "<h3 style='font-size: 20px; margin-top: 25px; margin-bottom: 15px;'>",
                    "💡 Clinical Examples:</h3>",
                    "<ul style='font-size: 15px; line-height: 2; margin-left: 20px;'>",
                    "<li>Tumor grade distribution (G1/G2/G3)</li>",
                    "<li>Treatment response rates (Complete/Partial/None)</li>",
                    "<li>Risk category proportions (Low/Medium/High)</li>",
                    "</ul>",

                    "<p style='font-size: 14px; margin-top: 25px; opacity: 0.9;'>",
                    "<strong>💡 Tip:</strong> Each square represents ~1% of your sample, ",
                    "making percentages immediately clear.</p>",
                    "</div>"
                )
                self$results$todo$setContent(todo)
                return()
            }

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            private$.resetMessages()

            # Validate inputs before processing
            tryCatch({
                private$.validateInputs()
            }, error = function(e) {
                # Display validation error in todo and stop
                error_msg <- glue::glue(
                    "<br>❌ <b>Input Validation Error:</b><br>
                    <br>{e$message}<br>
                    <br>Please check your variable selections and try again.<br><hr>"
                )
                self$results$todo$setContent(error_msg)
                stop(e$message)
            })
            
            # Performance optimization: prepare data and options with caching
            mydata <- private$.prepareData()
            options <- private$.prepareOptions()
            
            if (is.null(mydata) || nrow(mydata) == 0) {
                stop('Data contains no (complete) rows')
            }
            
            # Generate analysis summary and explanations if requested
            if (self$options$showSummaries || self$options$showExplanations) {
                groups_var <- self$options$groups
                facet_var <- self$options$facet
                counts_var <- self$options$counts
                
                plotdata <- private$.aggregateData(mydata, groups_var, facet_var, counts_var)
                total_cases <- sum(plotdata$count)
                is_weighted <- !is.null(counts_var) && counts_var != ""

                # Generate summary if requested
                if (self$options$showSummaries) {
                    summary_content <- private$.generateSummary(plotdata, groups_var, facet_var, total_cases, is_weighted)
                    self$results$analysisSummary$setContent(summary_content)
                }
                
                # Generate explanation if requested
                if (self$options$showExplanations) {
                    explanation_content <- private$.generateExplanation()
                    self$results$methodExplanation$setContent(explanation_content)
                }
            }
            
            todo <- glue::glue("<br>✅ Waffle chart created successfully. Enable 'Analysis Summary' or 'Show Explanations' in Output Options for detailed interpretations.<br><hr>")
            self$results$todo$setContent(todo)
        },

        .plot = function(image, ...) {
            if (is.null(self$options$groups))
                return()

            # Performance optimization: use prepared data and check cache
            if (private$.canUseCache()) {
                if (!is.null(private$.cached_plot)) {
                    print(private$.cached_plot)
                    return(TRUE)
                }
            }

            # Validate inputs and prepare data using cached method
            private$.validateInputs()
            mydata <- private$.prepareData()
            
            if (is.null(mydata) || nrow(mydata) == 0)
                stop('Data contains no (complete) rows')

            groups_var <- self$options$groups
            facet_var <- self$options$facet
            counts_var <- self$options$counts

            # Use consolidated data aggregation helper
            plotdata <- private$.aggregateData(mydata, groups_var, facet_var, counts_var)

            # Calculate values for caption using helper method
            total_cases <- sum(plotdata$count)
            is_weighted <- !is.null(counts_var) && counts_var != ""
            caption_text <- private$.generateCaption(plotdata, total_cases, is_weighted, facet_var)

            # Get number of unique groups
            n_groups <- length(unique(plotdata[[groups_var]]))

            # Generate color palette using cached method
            sel_palette <- private$.generateColorPalette(n_groups)

            # CRITICAL FIX: Set plot state for efficient caching
            # This ensures plot only regenerates when data or visual options actually change
            state_data <- list(
                # Data content (convert to base data.frame to avoid serialization issues)
                data = as.data.frame(plotdata),
                # All visual options that affect plot appearance
                visual_opts = list(
                    rows = self$options$rows,
                    flip = self$options$flip,
                    color_palette = self$options$color_palette,
                    show_legend = self$options$show_legend,
                    mytitle = self$options$mytitle,
                    legendtitle = self$options$legendtitle
                )
            )

            # Set state - jamovi will only regenerate if state changes
            image$setState(state_data)

            # Create base plot
            p <- ggplot2::ggplot(
                plotdata,
                ggplot2::aes(
                    fill = !!rlang::sym(groups_var),
                    values = count
                )
            ) +
                waffle::geom_waffle(
                    n_rows = self$options$rows,
                    size = 0.5,
                    color = "white",
                    flip = self$options$flip,
                    make_proportional = TRUE
                ) +
                ggplot2::scale_fill_manual(
                    values = sel_palette,
                    name = if (self$options$legendtitle != '')
                        self$options$legendtitle
                    else
                        groups_var
                ) +
                ggplot2::coord_equal() +
                ggplot2::theme_minimal()

            # Add labels in specific order
            if (!is.null(facet_var)) {
                facet_title <- facet_var  # Store the facet variable name
                # Combine title and caption with appropriate spacing
                combined_caption <- if (self$options$mytitle != '' && nchar(self$options$mytitle) > 0) {
                    paste0(self$options$mytitle, "\n", caption_text)
                } else {
                    caption_text
                }
                
                p <- p +
                    ggplot2::labs(
                        tag = facet_title,  # Facet variable name
                        caption = combined_caption
                    ) +
                    ggplot2::facet_wrap(
                        as.formula(paste0("~", facet_var)),
                        nrow = 1,
                        strip.position = "bottom"
                    )
            } else {
                # Combine title and caption with appropriate spacing
                combined_caption <- if (self$options$mytitle != '' && nchar(self$options$mytitle) > 0) {
                    paste0(self$options$mytitle, "\n", caption_text)
                } else {
                    caption_text
                }
                
                p <- p + ggplot2::labs(caption = combined_caption)
            }

            # Handle legend
            if (!self$options$show_legend) {
                p <- p + ggplot2::theme(legend.position = "none")
            }

            # Apply final theme adjustments with enhanced professional styling
            p <- p + ggplot2::theme(
                plot.title = ggplot2::element_text(
                    hjust = 0.5,
                    size = 16,
                    face = "bold",
                    margin = ggplot2::margin(b = 20)
                ),
                plot.caption = ggplot2::element_text(
                    size = 11,
                    hjust = 0.5,
                    face = "bold",
                    margin = ggplot2::margin(t = 25)
                ),
                plot.tag = ggplot2::element_text(
                    size = 14,
                    face = "bold",
                    hjust = 0.5
                ),
                plot.tag.position = "top",
                legend.position = if(self$options$show_legend) "right" else "none",
                legend.title = ggplot2::element_text(
                    size = 12,
                    face = "bold"
                ),
                legend.text = ggplot2::element_text(
                    size = 10
                ),
                legend.margin = ggplot2::margin(l = 20),
                strip.background = ggplot2::element_rect(
                    fill = "grey95",
                    color = "grey85",
                    size = 0.5
                ),
                strip.text = ggplot2::element_text(
                    size = 12,
                    face = "bold",
                    margin = ggplot2::margin(b = 10, t = 10)
                ),
                panel.grid = ggplot2::element_blank(),
                axis.text = ggplot2::element_blank(),
                axis.title = ggplot2::element_blank(),
                panel.background = ggplot2::element_rect(
                    fill = "white",
                    color = NA
                ),
                plot.background = ggplot2::element_rect(
                    fill = "white",
                    color = NA
                )
            )

            # Cache the plot and return
            private$.cached_plot <- p
            print(p)
            TRUE
        }
    )
)
