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
#' @importFrom ComplexUpset intersection_size get_size_mode
#' @importFrom dplyr inner_join
#' @importFrom ggvenn ggvenn
#' @importFrom ggVennDiagram ggVennDiagram get_shapes
#' @importFrom grid grid.text upViewport
#' @importFrom ggplot2 ggtitle theme element_text
#' @importFrom magrittr %>%
#' @importFrom utils combn
#' @importFrom UpSetR upset
#'
#' @return The function produces a Venn diagram and an Upset diagram.
#'
#' @examples
#' \dontrun{
#' # The bundled `histopathology` dataset ships with this package.
#' data(histopathology)
#'
#' # NOTE on var1true..var7true: these are jamovi `Level` options. The compiler
#' # forbids a `default:` on a Level, so the generated wrapper declares every
#' # one of them WITHOUT a default and they are all required arguments. Pass
#' # NULL for the levels whose variable is not in play.
#'
#' # Example 1: two markers, classic ggvenn diagram
#' venn(data = histopathology,
#'      var1 = "LVI", var1true = "Present",
#'      var2 = "PNI", var2true = "Present",
#'      var3 = NULL, var3true = NULL, var4 = NULL, var4true = NULL,
#'      var5 = NULL, var5true = NULL, var6 = NULL, var6true = NULL,
#'      var7 = NULL, var7true = NULL)
#'
#' # Example 2: three markers with the advanced ggVennDiagram engine
#' venn(data = histopathology,
#'      var1 = "LVI", var1true = "Present",
#'      var2 = "PNI", var2true = "Present",
#'      var3 = "LymphNodeMetastasis", var3true = "Present",
#'      var4 = NULL, var4true = NULL, var5 = NULL, var5true = NULL,
#'      var6 = NULL, var6true = NULL, var7 = NULL, var7true = NULL,
#'      show_ggVennDiagram = TRUE,
#'      regionLabels = "both",
#'      clinicalSummary = TRUE)
#'
#' # Example 3: UpSet-style intersection plot with percentage labels
#' venn(data = histopathology,
#'      var1 = "LVI", var1true = "Present",
#'      var2 = "PNI", var2true = "Present",
#'      var3 = "PreinvasiveComponent", var3true = "Present",
#'      var4 = NULL, var4true = NULL, var5 = NULL, var5true = NULL,
#'      var6 = NULL, var6true = NULL, var7 = NULL, var7true = NULL,
#'      show_complexUpset = TRUE,
#'      sortBy = "freq",
#'      minSize = 5,
#'      showAnnotations = TRUE)
#'
#' # Example 4: five sets - ggVennDiagram handles 5+ sets, ggvenn does not
#' venn(data = histopathology,
#'      var1 = "LVI", var1true = "Present",
#'      var2 = "PNI", var2true = "Present",
#'      var3 = "PreinvasiveComponent", var3true = "Present",
#'      var4 = "LymphNodeMetastasis", var4true = "Present",
#'      var5 = "Mortality5yr", var5true = "Dead",
#'      var6 = NULL, var6true = NULL, var7 = NULL, var7true = NULL,
#'      show_ggvenn = FALSE,
#'      show_ggVennDiagram = TRUE,
#'      regionLabels = "percent",
#'      colorPalette = "viridis")
#' }
#' @name vennClass
#' @keywords internal
NULL

#' @noRd
NULL

#' Venn Diagram Class
#' @name vennClass
#' @importFrom R6 R6Class
vennClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "vennClass",
        inherit = vennBase,
        private = list(
            .name_mapping = list(),
            .safe_lookup = character(0),
            .errors = character(0),
            .warnings = character(0),
            .info = character(0),

            # Notice collection helpers. A single Preformatted (plain-text) output item:
            # avoids BOTH the jmvcore::Notice serialization error from
            # self$results$insert(999, Notice) AND any HTML in notices (project convention:
            # notice content must be plain text). ====
            .noticeList = list(),

            # TRUE when `x` is something grDevices can turn into a colour (a name from
            # colours(), "#RRGGBB", or "#RRGGBBAA"). Used to keep a typed colour option
            # from killing the whole ggplot at grob-conversion time.
            .isColour = function(x) {
                if (is.null(x) || length(x) != 1 || is.na(x) || !nzchar(x))
                    return(FALSE)
                !inherits(try(grDevices::col2rgb(x), silent = TRUE), "try-error")
            },

            .addNotice = function(type, title, content) {
                duplicate <- vapply(private$.noticeList, function(notice) {
                    identical(notice$type, type) &&
                        identical(notice$title, title) &&
                        identical(notice$content, content)
                }, logical(1))
                if (any(duplicate))
                    return()

                private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                    type = type,
                    title = title,
                    content = content
                )
                # Render immediately so early-return validation aborts still display the notice
                private$.renderNotices()
            },

            .renderNotices = function() {
                if (length(private$.noticeList) == 0) {
                    self$results$notices$setContent("")
                    return()
                }

                # Plain text only notices avoid HTML by project convention; the Preformatted
                # output item renders this literally (no markup, no injection surface).
                blocks <- vapply(private$.noticeList, function(notice) {
                    prefix <- switch(notice$type,
                        ERROR          = "ERROR: ",
                        STRONG_WARNING = "WARNING: ",
                        WARNING        = "WARNING: ",
                        "")
                    paste0(prefix, notice$title, "\n", notice$content)
                }, character(1))

                self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
            },

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


                selected_vars <- Filter(Negate(is.null), list(
                    self$options$var1, self$options$var2, self$options$var3,
                    self$options$var4, self$options$var5, self$options$var6,
                    self$options$var7))

                # membershipTable's COLUMN set is equally option-determined - Row, Group,
                # and one column per selected variable - so it is laid out here instead of
                # being assembled with addColumn() inside .run(), which made the table
                # visibly restructure on every run. .run() still adds anything missing, so
                # the two cannot get out of step.
                if (length(selected_vars) >= 2) {
                    membership_titles <- c("Row", "Group",
                        vapply(selected_vars, as.character, character(1)))
                    membership_names <- make.names(membership_titles, unique = TRUE)
                    for (i in seq_along(membership_names))
                        self$results$membershipTable$addColumn(
                            name = membership_names[i],
                            title = membership_titles[i],
                            type = if (i == 1L) "integer" else "text")
                }
            },

            .run = function() {
                private$.checkpoint()

                # Reset message accumulators at the start of each run
                private$.errors <- character(0)
                private$.warnings <- character(0)
                private$.info <- character(0)
                private$.noticeList <- list()
                private$.renderNotices()

                # .displayNotices() only ever WRITES to these three Html items, so a
                # resolved condition would otherwise stay on screen: exclude every case
                # with a row filter (red "Dataset is empty" panel), then remove the
                # filter, and the completed analysis would still be topped by the stale
                # error. Blank and hide them here; this run repaints whatever still applies.
                for (nm in c("validationErrors", "validationWarnings", "analysisInfo")) {
                    self$results[[nm]]$setContent("")
                    self$results[[nm]]$setVisible(FALSE)
                }

                # summary is a rows: 0 table filled with addRow(), and jmvcore's addRow()
                # appends without checking for a duplicate rowKey. The top-level clearWith
                # covers the variables and the plot options but not the panel toggles
                # (showGlossary, clinicalSummary, showSetCalculations, ...), so ticking one
                # of those re-entered .run() against the retained rows and printed every
                # variable twice - three runs gave 9 rows for 3 variables and
                # as.data.frame() died with "duplicate 'row.names'". Clearing here rather
                # than next to the fill loop also covers the early returns below, so a
                # validation failure cannot leave the previous run's counts on screen
                # underneath the error panel.
                self$results$summary$deleteRows()

                # Validate required variables and their true levels
                if (!private$.validateVariables()) {
                    private$.displayNotices()
                    return()  # Validation failed, errors already accumulated
                }

                # If no plot type selected, default to ggvenn for user feedback.
                # NOTE: self$options is read-only at runtime (assigning to an option
                # active-binding errors with "unused argument"), so we track the
                # fallback in a local flag and force the ggvenn plot visible below
                # instead of mutating the option.
                default_to_ggvenn <- (!self$options$show_ggvenn && !self$options$show_ggVennDiagram &&
                    !self$options$show_upsetR && !self$options$show_complexUpset)
                
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
                        "<div style='background-color: rgba(88, 88, 88, 0.06); border: 2px solid #333; padding: 20px; margin-bottom: 20px; color: inherit;'>",
                        "<h2 style='margin: 0 0 10px 0; font-size: 20px; color: inherit;'>Venn Diagram Analysis</h2>",
                        "<p style='margin: 0; font-size: 14px; color: inherit;'>Visualize overlaps and intersections between categorical variables</p>",
                        "</div>",

                        "<div style='background-color: rgba(155, 155, 155, 0.06); border-left: 4px solid #333; padding: 15px; margin-bottom: 20px; color: inherit;'>",
                        "<h3 style='margin: 0 0 10px 0; color: inherit; font-size: 16px;'>Setup Progress</h3>"
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
                            if(has_var1) "[]" else "[ ]", " Variable 1: ", if(has_var1) "Selected" else "Not selected",
                            if(has_var1 && has_var1_level) " + Level" else "", "</div>",
                            "<div style='margin-bottom: 10px;'>",
                            if(has_var2) "[]" else "[ ]", " Variable 2: ", if(has_var2) "Selected" else "Not selected",
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

                        "<div style='background-color: rgba(155, 155, 155, 0.06); border: 1px solid #ccc; padding: 15px; color: inherit;'>",
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
                    # Onboarding guidance is shown in the richer 'welcome' panel above.
                    return()
                } else {
                    # The ggvenn fallback goes through the notices channel like every
                    # other message. It used to have a results item of its own titled
                    # "To Do" - developer-speak shown to the user as a heading - which
                    # carried this one sentence and nothing else.
                    if (default_to_ggvenn) {
                        private$.addNotice(
                            "INFO",
                            .("Defaulting to the ggvenn Diagram"),
                            .("No plot type is selected, so the classic ggvenn diagram below is drawn by default. Tick ggVennDiagram, UpSetR or ComplexUpset in Plot Selection to choose a different one."))
                    }

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

                    # Column names containing spaces, leading digits or punctuation
                    # are handled below by make.names(selected_vars, unique = TRUE),
                    # so no name-format warning is raised here.
                    all_vars <- c(var1, var2, var3, var4, var5, var6, var7)
                    selected_vars <- all_vars[!sapply(all_vars, is.null)]

                    # CRITICAL FIX: Select ONLY the variables needed for analysis
                    # This prevents dropping cases with NAs in unrelated columns
                    selected_data <- original_data[, selected_vars, drop = FALSE]

                    # An explicit NA LEVEL (addNA(), factor(exclude = NULL)) is missing
                    # data wearing a level's clothes. is.na() is FALSE for those cases,
                    # so jmvcore::naOmit() keeps them; and `Ops.factor` compares level
                    # CODES, so `f == var1true` returns FALSE rather than NA for them.
                    # The case was therefore counted as a NEGATIVE: it inflated the
                    # False column and the denominator of every percentage, and the
                    # CASE EXCLUSION warning never mentioned it. Re-levelling without
                    # the NA level restores real NAs, so naOmit() drops those rows and
                    # the existing exclusion warning discloses them like any other
                    # missing value. Do NOT "fix" this with an is.na() guard on the
                    # comparison - that keeps scoring a missing marker as negative.
                    for (nm in names(selected_data)) {
                        col <- selected_data[[nm]]
                        if (is.factor(col) && anyNA(levels(col)))
                            selected_data[[nm]] <- factor(
                                col, levels = levels(col)[!is.na(levels(col))])
                    }

                    # Apply naOmit ONLY to selected variables, not entire dataset.
                    # nrow(full_data) IS the number of complete cases on the selected
                    # variables, so a separate complete.cases() count added nothing and
                    # was never read.
                    full_data <- jmvcore::naOmit(selected_data)
                    excluded_n <- original_n - nrow(full_data)

                    # Complementary missingness (var1 present where var2 is NA and vice
                    # versa) leaves NO complete case. Everything downstream then divides
                    # by zero: the summary table shows NaN in a percent-formatted column,
                    # .powerAdvisories returns early so no caution is raised, and the
                    # Analysis Information panel reports a successful run over N=0.
                    if (nrow(full_data) == 0) {
                        private$.errors <- c(private$.errors, sprintf(
                            'No case has a non-missing value for every selected variable, so there is nothing to count or draw. Original N=%d, complete cases=0. Select fewer variables, or inspect the missing-data pattern of the selected variables.',
                            original_n))
                        private$.displayNotices()
                        return()
                    }

                    # CRITICAL WARNING: Report case loss if any exclusions occurred
                    if (excluded_n > 0) {
                        excluded_pct <- round(100 * excluded_n / original_n, 1)
                        private$.warnings <- c(private$.warnings, sprintf(
                            'CASE EXCLUSION: %d cases (%.1f%%) were dropped because at least one selected variable was missing. Original N=%d, Final N=%d. Every count and percentage below, and every region of the diagrams, describes only those %d complete cases; consider whether the excluded cases differ systematically before generalising.',
                            excluded_n, excluded_pct, original_n, nrow(full_data), nrow(full_data)
                        ))
                    }

                    # Restore row numbers for tracking
                    row_numbers <- suppressWarnings(as.integer(rownames(full_data)))
                    if (length(row_numbers) != nrow(full_data) || any(is.na(row_numbers))) {
                        row_numbers <- seq_len(nrow(full_data))
                    }

                    # Two variables whose names sanitise to the SAME syntactic name
                    # ("Tumor Grade" and "Tumor.Grade" both become "Tumor.Grade")
                    # used to collide: each block called make.names() on its own
                    # variable, so the second set silently overwrote the first in
                    # `mydata` while the summary table still showed both original
                    # names. Two genuinely different sets - 30 positive and 5
                    # positive - were both reported as 5, and the diagram drew
                    # them identically. Build ONE lookup with unique = TRUE.
                    if (anyDuplicated(selected_vars)) {
                        dupes <- unique(selected_vars[duplicated(selected_vars)])
                        jmvcore::reject(
                            .("The same variable is selected more than once ({}). Each set in a Venn diagram must be a different variable."),
                            code = NULL, paste(dupes, collapse = ", "))
                    }
                    safe_lookup <- make.names(selected_vars, unique = TRUE)
                    names(safe_lookup) <- selected_vars
                    private$.safe_lookup <- safe_lookup

                    # Collect only selected variables and convert to logical values
                    mydata <- data.frame(row.names = seq_len(nrow(full_data)))

                    # Create mapping between original and safe names for variables with spaces/numbers
                    name_mapping <- list()

                    # Process each variable with robust error handling for problematic names
                    if (!is.null(self$options$var1)) {
                        safe_name1 <- safe_lookup[[var1]]
                        tryCatch({
                            mydata[[safe_name1]] <- ifelse(full_data[[var1]] == var1true, TRUE, FALSE)
                            name_mapping[[safe_name1]] <- var1
                        }, error = function(e) {
                            # Route the raw column name through jamovi's escaped error channel
                            jmvcore::reject(
                                .("Error processing variable '{}': {}. Try using backticks around the variable name: `{}`"),
                                code = NULL, var1, e$message, var1)
                        })
                    }
                    if (!is.null(self$options$var2)) {
                        safe_name2 <- safe_lookup[[var2]]
                        tryCatch({
                            mydata[[safe_name2]] <- ifelse(full_data[[var2]] == var2true, TRUE, FALSE)
                            name_mapping[[safe_name2]] <- var2
                        }, error = function(e) {
                            jmvcore::reject(
                                .("Error processing variable '{}': {}. Try using backticks around the variable name: `{}`"),
                                code = NULL, var2, e$message, var2)
                        })
                    }
                    if (!is.null(self$options$var3)) {
                        safe_name3 <- safe_lookup[[var3]]
                        tryCatch({
                            mydata[[safe_name3]] <- ifelse(full_data[[var3]] == var3true, TRUE, FALSE)
                            name_mapping[[safe_name3]] <- var3
                        }, error = function(e) {
                            jmvcore::reject(
                                .("Error processing variable '{}': {}. Try using backticks around the variable name: `{}`"),
                                code = NULL, var3, e$message, var3)
                        })
                    }
                    if (!is.null(self$options$var4)) {
                        safe_name4 <- safe_lookup[[var4]]
                        tryCatch({
                            mydata[[safe_name4]] <- ifelse(full_data[[var4]] == var4true, TRUE, FALSE)
                            name_mapping[[safe_name4]] <- var4
                        }, error = function(e) {
                            jmvcore::reject(
                                .("Error processing variable '{}': {}. Try using backticks around the variable name: `{}`"),
                                code = NULL, var4, e$message, var4)
                        })
                    }
                    if (!is.null(self$options$var5)) {
                        safe_name5 <- safe_lookup[[var5]]
                        tryCatch({
                            mydata[[safe_name5]] <- ifelse(full_data[[var5]] == var5true, TRUE, FALSE)
                            name_mapping[[safe_name5]] <- var5
                        }, error = function(e) {
                            jmvcore::reject(
                                .("Error processing variable '{}': {}. Try using backticks around the variable name: `{}`"),
                                code = NULL, var5, e$message, var5)
                        })
                    }
                    if (!is.null(self$options$var6)) {
                        safe_name6 <- safe_lookup[[var6]]
                        tryCatch({
                            mydata[[safe_name6]] <- ifelse(full_data[[var6]] == var6true, TRUE, FALSE)
                            name_mapping[[safe_name6]] <- var6
                        }, error = function(e) {
                            jmvcore::reject(
                                .("Error processing variable '{}': {}. Try using backticks around the variable name: `{}`"),
                                code = NULL, var6, e$message, var6)
                        })
                    }
                    if (!is.null(self$options$var7)) {
                        safe_name7 <- safe_lookup[[var7]]
                        tryCatch({
                            mydata[[safe_name7]] <- ifelse(full_data[[var7]] == var7true, TRUE, FALSE)
                            name_mapping[[safe_name7]] <- var7
                        }, error = function(e) {
                            jmvcore::reject(
                                .("Error processing variable '{}': {}. Try using backticks around the variable name: `{}`"),
                                code = NULL, var7, e$message, var7)
                        })
                    }

                    # Store name mapping for use in plots and calculations
                    private$.name_mapping <- name_mapping

                    # Prepare data for Venn diagrams (logical values).
                    plotDataVenn <- list("mydata" = mydata,
                                         "names" = names(mydata))

                    # Set state for each plot type
                    # Assign visibility from the option on BOTH branches. A one-way
                    # setVisible(TRUE) latched: once the fallback fired, ticking
                    # "UpSetR plot" left the ggvenn image on screen with its own
                    # checkbox cleared, until an unrelated clearWith option changed.
                    self$results$plotGgvenn$setVisible(self$options$show_ggvenn || default_to_ggvenn)
                    if (self$options$show_ggvenn || default_to_ggvenn) {
                        self$results$plotGgvenn$setState(plotDataVenn)
                    }
                    if (self$options$show_ggVennDiagram) {
                        self$results$plotGgVennDiagram$setState(plotDataVenn)
                        n_sets_selected <- length(selected_vars)
                        if (self$options$shapeType != "auto" &&
                            is.null(private$.vennShapeId(self$options$shapeType, n_sets_selected))) {
                            private$.addNotice(
                                "INFO",
                                .("Requested Venn Shape Is Not Available"),
                                sprintf(.("No %s shape is defined for %d sets, so the ggVennDiagram plot below is drawn with the automatic shape instead. Available shapes are: circle for 2-3 sets, ellipse for 4 sets, polygon for 4-7 sets, and triangle for 6 sets. Change the number of variables or set the shape back to Automatic."),
                                        self$options$shapeType, n_sets_selected))
                        }
                        # Edge color and Set label color are free-text boxes. A typo
                        # would otherwise reach ggplot2 as "Unknown colour name" and
                        # destroy the whole figure; the helper falls back to black.
                        for (nm in c("edgeColor", "setLabelColor")) {
                            if (!private$.isColour(self$options[[nm]])) {
                                private$.addNotice(
                                    "WARNING",
                                    .("Colour Not Recognised"),
                                    sprintf(.("'%s' is not a colour R recognises, so the ggVennDiagram plot below is drawn with black instead. Use a colour name from the standard R palette (for example red, steelblue, grey40) or a hex code such as #2C7FB8."),
                                            as.character(self$options[[nm]])))
                            }
                        }
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
                        if (self$options$minSize > 0) {
                            private$.addNotice(
                                "INFO",
                                .("Minimum Intersection Size Does Not Reach the UpSetR Plot"),
                                sprintf(.("Minimum intersection size is set to %d, but that filter is applied by the ComplexUpset engine only. The UpSetR plot below still draws every intersection it finds, including ones with fewer than %d cases. Switch on the ComplexUpset plot to see the filtered version, or read the UpSetR bars as unfiltered."),
                                        self$options$minSize, self$options$minSize))
                        }
                    }
                    if (self$options$show_complexUpset) {
                        self$results$plotComplexUpset$setState(plotDataUpset)
                        # Minimum intersection size larger than any intersection in the
                        # data makes ComplexUpset abort. .plotComplexUpsetHelper drops
                        # the filter so a plot is still drawn; say so, or the filter
                        # looks silently applied.
                        max_intersection <- if (nrow(mydata2) > 0)
                            max(table(apply(mydata2, 1, paste, collapse = "|"))) else 0L
                        if (self$options$minSize > max_intersection) {
                            private$.addNotice(
                                "INFO",
                                .("Minimum Intersection Size Is Larger Than Any Intersection"),
                                sprintf(.("Minimum intersection size is set to %d, but the largest group of cases sharing one membership pattern contains %d cases. Filtering at %d would leave nothing to draw, so the filter has been switched off and the ComplexUpset plot below shows every intersection, unfiltered. Set the minimum intersection size to %d or less for it to take effect."),
                                        self$options$minSize, max_intersection,
                                        self$options$minSize, max_intersection))
                        }
                    }

                    # Create summary statistics for each variable using helper function
                    summaryData <- data.frame(
                        Variable = character(),
                        Level = character(),
                        TrueCount = integer(),
                        FalseCount = integer(),
                        TotalCount = integer(),
                        TruePercentage = numeric(),
                        stringsAsFactors = FALSE
                    )

                    # Process each variable that was selected using helper function
                    variables <- list(var1, var2, var3, var4, var5, var6, var7)
                    true_levels <- list(var1true, var2true, var3true, var4true,
                                        var5true, var6true, var7true)
                    for (vi in seq_along(variables)) {
                        var <- variables[[vi]]
                        if (!is.null(var)) {
                            # Find the safe column name that corresponds to this variable
                            safe_name <- safe_lookup[[var]]
                            if (safe_name %in% names(mydata)) {
                                varStats <- private$.calculateSummaryStats(
                                    mydata, safe_name, var, true_levels[[vi]])
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
                        # "True" is whatever level the user nominated per variable, and
                        # nothing else in the output records which level that was. The
                        # word "positive" appears throughout the prose panels, so the
                        # mapping has to be visible somewhere.
                        if (nrow(summaryData) > 0 && !all(is.na(summaryData$Level)))
                            self$results$summary$setNote("levels", .fmt(
                                .("\"True\" means the variable equals the level selected for it: {levels}. Every count, percentage and sentence below calls that level positive."),
                                levels = paste(sprintf("%s = %s", summaryData$Variable,
                                              ifelse(is.na(summaryData$Level), .("(not set)"), summaryData$Level)),
                                      collapse = "; ")))
                    }

                    # Small-sample / low-prevalence advisories go to the always-visible
                    # notices panel, independently of the optional Clinical Summary.
                    for (adv in private$.powerAdvisories(summaryData, nrow(mydata)))
                        private$.addNotice("STRONG_WARNING", adv$title, adv$content)

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

                    # Generate membership table if requested.
                    # Decoupled from showSetCalculations so the membership table / data output
                    # can be produced independently (previously silently no-oped without it).
                    if (self$options$showMembershipTable || self$options$membershipGroups) {
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
                    jmvcore::reject(.('Data contains no (complete) rows'))

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
                        .("\u{2022} Enable 'Show ggVennDiagram Plot' instead"),
                        "\n",
                        .("\u{2022} ggVennDiagram supports 5+ variables with better visualization"),
                        "\n\n",
                        .("Alternative Options:"),
                        "\n",
                        .("\u{2022} Use UpSetR or ComplexUpset plots for complex intersections"),
                        "\n",
                        .("\u{2022} Reduce to 4 or fewer variables for ggvenn visualization")
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

                if (is.null(results))
                    return(FALSE)
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
                    jmvcore::reject(.('Data contains no (complete) rows'))

                # Retrieve the prepared data.
                results <- image$state

                if (is.null(results))
                    return(FALSE)
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
                    jmvcore::reject(.('Data contains no (complete) rows'))

                # Retrieve the prepared data.
                results <- image$state

                if (is.null(results))
                    return(FALSE)
                mydata2 <- results$mydata

                # Generate the UpSetR plot
                plot <- private$.plotUpsetRHelper(mydata2)

                # Print the UpSetR plot
                print(plot)

                # The title has to be drawn AFTER print(). UpSetR::upset() does not
                # draw - it returns an object - and print.upset() reaches
                # Make_base_plot(newpage = TRUE) -> grid.newpage(), which wiped any
                # title drawn beforehand. Return to the root viewport first so the
                # coordinates are page-relative and not relative to UpSetR's last
                # pushed viewport.
                grid::upViewport(0)
                grid::grid.text(.("UpSetR Diagram of Selected Variables"), x = 0.5, y = 0.97,
                                gp = grid::gpar(fontsize = 14, fontface = "bold"))
                TRUE
            },

            .plotComplexUpset = function(image, ggtheme, theme, ...) {
                private$.checkpoint()

                # Validate that the required inputs are available.
                if (is.null(self$options$var1) || is.null(self$options$var2))
                    return()
                if (nrow(self$data) == 0)
                    jmvcore::reject(.('Data contains no (complete) rows'))

                # Retrieve the prepared data.
                results <- image$state

                if (is.null(results))
                    return(FALSE)
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

                # Validate optional variables (var3-7)
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
                        # Same check var1/var2 already get. Without it an all-missing
                        # optional variable fell through to the level test below and
                        # reported "Available levels: " with an empty list, which names
                        # the symptom rather than the problem.
                        if (all(is.na(var_data))) {
                            private$.errors <- c(private$.errors,
                                sprintf("Variable '%s' contains only missing values. Please select a different variable with valid data.", var_value))
                            return(FALSE)
                        }
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
            .calculateSummaryStats = function(data, safe_varname, original_varname = NULL,
                                              true_level = NULL) {
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
                        private$.addNotice(
                            "WARNING",
                            paste0("Variable '", display_name, "' Was Left Out"),
                            paste0("Variable '", display_name, "' holds ",
                                class(column_data)[1],
                                " values, which cannot be turned into the yes/no set membership this analysis needs, so it contributes no row to the summary table and no set to the diagrams. ",
                                "Pick a nominal or ordinal variable (or a 0/1 numeric one) and set its 'true' level in the options, or recode '",
                                display_name,
                                "' into two categories first, then run the analysis again.")
                        )
                        return(NULL)
                    }
                }

                true_count <- sum(column_data, na.rm = TRUE)
                false_count <- sum(!column_data, na.rm = TRUE)
                total_count <- true_count + false_count

                data.frame(
                    Variable = display_name,
                    # The level the user nominated as "true" for this variable. Every
                    # sentence and label in the output calls that level "positive", so
                    # the level itself has to travel with the counts - it may be
                    # "Dead", "Absent" or "Grade 1" just as easily as "Positive".
                    Level = if (is.null(true_level)) NA_character_ else as.character(true_level),
                    TrueCount = true_count,
                    FalseCount = false_count,
                    TotalCount = total_count,
                    # total_count is 0 only if the column is entirely NA; 0/0 would put
                    # a NaN into a percent-formatted table column.
                    TruePercentage = if (total_count > 0) round(true_count / total_count, 4) else NA_real_,
                    stringsAsFactors = FALSE
                )
            },
            
            # Generate About This Analysis content
            .generateAboutAnalysis = function() {
                about_content <- paste0(
                    "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-radius: 5px; margin-bottom: 15px; color: inherit;'>",
                    "<h4 style='color: inherit; margin-top: 0;'>", .("About Venn Diagrams"), "</h4>",
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
                    "<li>", .("Select one or more plot engines in the Plot Selection panel"), "</li>",
                    "<li>", .("Adjust visualization options as needed"), "</li>",
                    "<li>", .("Interpret intersections by reading the number printed in each region - the Venn shapes are fixed geometry, so the drawn area of a region does not encode how many cases it holds"), "</li>",
                    "</ol>",
                    "<p><strong>", .("Choosing a Plot Engine:"), "</strong></p>",
                    "<ul style='margin-left: 20px;'>",
                    "<li><strong>ggvenn:</strong> ", .("Classic Venn diagram for up to 4 variables"), "</li>",
                    "<li><strong>ggVennDiagram:</strong> ", .("Advanced Venn with extensive customization; recommended for 5 or more variables"), "</li>",
                    "<li><strong>UpSetR / ComplexUpset:</strong> ", .("Matrix-style intersection plots for many variables"), "</li>",
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
                
                # Most prevalent set. which.max() returns only the FIRST maximum, so
                # a genuine tie was reported as if one variable were the winner and
                # the equally-common ones went unmentioned. Name all of them.
                largest_count <- max(summaryData$TrueCount, na.rm = TRUE)
                largest_vars <- summaryData$Variable[!is.na(summaryData$TrueCount) &
                                                     summaryData$TrueCount == largest_count]
                largest_var <- paste(largest_vars, collapse = ", ")
                largest_pct <- round((largest_count / total_n) * 100, 1)
                
                # Calculate 2-way intersection if we have 2+ variables
                intersection_analysis <- ""
                if (length(var_names) >= 2) {
                    # Resolve through the same unique lookup the columns were built
                    # with; make.names() here would reintroduce the collision.
                    lk <- private$.safe_lookup
                    col1 <- if (var_names[1] %in% names(lk)) lk[[var_names[1]]] else make.names(var_names[1])
                    col2 <- if (var_names[2] %in% names(lk)) lk[[var_names[2]]] else make.names(var_names[2])
                    var1_data <- as.logical(data[[col1]])
                    var2_data <- as.logical(data[[col2]])
                    both_true <- sum(var1_data & var2_data, na.rm = TRUE)
                    both_pct <- round((both_true / total_n) * 100, 1)
                    
                    intersection_analysis <- paste0(
                        "<p><strong>", .("Key Intersection:"), "</strong> ",
                        sprintf(.("%s cases (%s%%) had both %s and %s positive."), 
                                both_true, both_pct, htmltools::htmlEscape(var_names[1]), htmltools::htmlEscape(var_names[2])),
                        # Only the first two selected variables enter this sentence.
                        if (length(var_names) > 2)
                            paste0(" ", sprintf(.("This figure covers those two sets only; the other %d selected variable(s) (%s) do not enter it - read the diagram for higher-order intersections."),
                                                length(var_names) - 2,
                                                htmltools::htmlEscape(paste(var_names[-(1:2)], collapse = ", "))))
                        else "",
                        "</p>"
                    )
                }
                
                # Generate clinical interpretation and statistical warnings
                clinical_interpretation <- private$.generateClinicalInterpretation(summaryData, var_names, total_n)
                statistical_warnings <- private$.validateStatisticalPower(summaryData, total_n)

                clinical_summary <- paste0(
                    "<div style='background-color: rgba(33, 149, 236, 0.1); padding: 15px; border-radius: 5px; border-left: 4px solid #3498db; color: inherit;'>",
                    "<h4 style='color: inherit; margin-top: 0;'>", .("Clinical Summary"), "</h4>",
                    "<p><strong>", .("Dataset:"), "</strong> ", sprintf(.("%s cases analyzed"), total_n), "</p>",
                    "<p><strong>", .("Most Prevalent:"), "</strong> ", 
                    if (length(largest_vars) > 1)
                        sprintf(.("%s were equally the most common (%s cases each, %s%%)."),
                                htmltools::htmlEscape(largest_var), largest_count, largest_pct)
                    else
                        sprintf(.("%s was most common (%s cases, %s%%)."),
                                htmltools::htmlEscape(largest_var), largest_count, largest_pct), "</p>",
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

                # Create individual variable sentences. The level the user nominated as
                # "true" is named explicitly: it can be "Dead", "Absent" or "Grade 1"
                # just as easily as "Positive", and this prose is offered for pasting
                # into a manuscript.
                has_level <- "Level" %in% names(summaryData)
                individual_sentences <- sapply(seq_len(nrow(summaryData)), function(i) {
                    lvl <- if (has_level) summaryData$Level[i] else NA_character_
                    if (!is.na(lvl))
                        sprintf("%s was %s in %s of %s cases (%s%%).",
                                htmltools::htmlEscape(summaryData$Variable[i]),
                                htmltools::htmlEscape(lvl),
                                summaryData$TrueCount[i],
                                total_n,
                                round(summaryData$TruePercentage[i] * 100, 1))
                    else
                        sprintf("%s was positive in %s of %s cases (%s%%).",
                                htmltools::htmlEscape(summaryData$Variable[i]),
                                summaryData$TrueCount[i],
                                total_n,
                                round(summaryData$TruePercentage[i] * 100, 1))
                })

                # Generate intersection analysis for clinical reporting
                intersection_sentences <- ""
                if (length(var_names) >= 2) {
                    # Calculate 2-way intersection
                    # Resolve through the same unique lookup the columns were built
                    # with; make.names() here would reintroduce the collision.
                    lk <- private$.safe_lookup
                    resolve <- function(nm) if (nm %in% names(lk)) lk[[nm]] else make.names(nm)
                    col1 <- resolve(var_names[1])
                    col2 <- resolve(var_names[2])
                    var1_data <- as.logical(data[[col1]])
                    var2_data <- as.logical(data[[col2]])
                    both_positive <- sum(var1_data & var2_data, na.rm = TRUE)
                    both_pct <- round((both_positive / total_n) * 100, 1)

                    # "Only" has to mean positive for THIS set and no other selected set.
                    # var1_data & !var2_data ignores sets 3..7, so with three or more
                    # variables it over-counted and contradicted the "Unique Members per
                    # Set" figures in the Set Calculations panel, which are computed the
                    # way this now is (degree == 1 over the full membership matrix).
                    all_cols <- vapply(var_names, resolve, character(1), USE.NAMES = FALSE)
                    all_cols <- all_cols[all_cols %in% names(data)]
                    member_mat <- as.matrix(data[, all_cols, drop = FALSE]) == TRUE
                    degree <- rowSums(member_mat, na.rm = TRUE)
                    var1_only <- sum(degree == 1 & var1_data, na.rm = TRUE)
                    var2_only <- sum(degree == 1 & var2_data, na.rm = TRUE)

                    intersection_sentences <- sprintf(
                        "Co-occurrence of %s and %s was observed in %s cases (%s%%). %s cases (%s%%) were positive for %s only, while %s cases (%s%%) were positive for %s only.",
                        htmltools::htmlEscape(var_names[1]), htmltools::htmlEscape(var_names[2]), both_positive, both_pct,
                        var1_only, round((var1_only/total_n)*100, 1), htmltools::htmlEscape(var_names[1]),
                        var2_only, round((var2_only/total_n)*100, 1), htmltools::htmlEscape(var_names[2])
                    )

                    # The co-occurrence figure is pairwise; the "only" figures above are
                    # exclusive across every selected set. Say so, because with three or
                    # more sets the two clauses answer different questions.
                    if (length(var_names) > 2) {
                        intersection_sentences <- paste0(intersection_sentences, sprintf(
                            " The co-occurrence figure covers %s and %s alone, whereas \"only\" means positive for that variable and negative for the other %d selected variables; the remaining variable(s) (%s) and the higher-order intersections are shown in the diagram and, if enabled, in the Set Calculations panel.",
                            htmltools::htmlEscape(var_names[1]), htmltools::htmlEscape(var_names[2]),
                            length(var_names) - 1,
                            htmltools::htmlEscape(paste(var_names[-(1:2)], collapse = ", "))))
                    }
                }

                # Generate comprehensive clinical paragraph
                # Purely descriptive opener. This analysis performs no test of any
                # kind, so "revealed distinct patterns" was an inferential claim being
                # handed to the user as ready-to-paste manuscript prose.
                clinical_paragraph <- sprintf(
                    "Analysis of %s cases produced the following set membership counts. %s%s",
                    total_n,
                    paste(individual_sentences, collapse = " "),
                    if (length(var_names) >= 2) {
                        mean_overlap <- mean(summaryData$TrueCount) / total_n
                        sprintf(paste0(" Across the selected variables, the average share of positive cases was %s;",
                                       " overlap between variables is shown in the diagram and depends on how common each variable is."),
                                if (mean_overlap > 0.5) "high" else if (mean_overlap > 0.2) "moderate" else "low")
                    } else ""
                )

                report_content <- paste0(
                    "<div style='background-color: rgba(33, 152, 33, 0.07); padding: 15px; border-radius: 5px; border-left: 4px solid #27ae60; color: inherit;'>",
                    "<h4 style='color: inherit; margin-top: 0;'> Copy-Ready Clinical Summary</h4>",
                    "<div style='background-color: rgba(255, 255, 255, 0.06); padding: 12px; border-radius: 3px; font-family: Georgia, serif; line-height: 1.6; border: 1px solid #e9ecef; color: inherit;'>",
                    "<h6 style='margin: 0 0 8px 0; color: inherit;'>Clinical Report Template</h6>",
                    "<p style='margin: 0 0 10px 0;'>", clinical_paragraph, "</p>",
                    if (intersection_sentences != "") paste0("<p style='margin: 0;'>", intersection_sentences, "</p>") else "",
                    "</div>",
                    "<div style='background-color: rgba(255, 255, 255, 0.06); padding: 10px; border-radius: 3px; margin-top: 8px; border: 1px solid #e9ecef; color: inherit;'>",
                    "<h6 style='margin: 0 0 6px 0; color: inherit;'>Individual Variable Summary</h6>",
                    "<ul style='margin: 0; padding-left: 20px;'>",
                    paste0("<li>", individual_sentences, "</li>", collapse = ""),
                    "</ul>",
                    "</div>",
                    "<div style='margin-top: 10px; padding: 8px; background-color: rgba(33, 144, 255, 0.11); border-radius: 3px; color: inherit;'>",
                    "<small> <strong>Usage:</strong> Select and copy text from either template above. ",
                    "The clinical report template provides publication-ready prose, while the summary offers bullet-point details.</small>",
                    "</div>",
                    "</div>"
                )
                
                self$results$reportSentences$setContent(report_content)
            },
            
            # Generate assumptions and interpretation guide
            .generateAssumptions = function() {
                assumptions_content <- paste0(
                    "<div style='background-color: rgba(255, 211, 33, 0.16); padding: 15px; border-radius: 5px; border-left: 4px solid #f39c12; color: inherit;'>",
                    "<h4 style='color: inherit; margin-top: 0;'>", .("Interpretation Guide & Assumptions"), "</h4>",
                    
                    "<h5>", .("How to Interpret:"), "</h5>",
                    "<ul style='margin-left: 20px;'>",
                    "<li><strong>", .("Venn Diagram:"), "</strong> ", .("Circle overlaps show which sets a group of cases belongs to; the count printed in a region, not its drawn area, is the number of cases. How many cases fall in an intersection depends on how common each variable is and does not by itself measure association."), "</li>",
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
                    "<li>", .("Large overlaps may reflect high positivity rates rather than a shared mechanism; test association formally before interpreting them"), "</li>",
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
                # Get user options.
                # minSize is deliberately NOT read here: UpSetR's `cutoff` only takes
                # effect in its group.by = "sets" branch, which this code never selects,
                # so passing it did nothing (verified: cutoff = 0 and cutoff = 50 give
                # byte-identical Main_bar data). The ComplexUpset engine applies the
                # filter; .run() posts a notice when the two disagree.
                sortBy <- self$options$sortBy
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
                        text.scale = c(1.5, 1.3, 1.2, 1.1, 2, 1),
                        show.numbers = "yes"
                    )
                } else {
                    # Minimal text: smaller scaling and hide numbers
                    plot <- UpSetR::upset(
                        mydata2,
                        order.by = orderBy,
                        text.scale = c(0.8, 0.8, 0.8, 0.8, 1, 0.6),
                        show.numbers = "no"
                    )
                }

                # The title is drawn by .plotUpsetR AFTER print(); see the comment there.
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

                # ComplexUpset keeps the sort KEY and the sort DIRECTION in two
                # separate arguments. sort_intersections alone only ever orders by
                # cardinality, so "Sort intersections by: Degree" used to hand back a
                # size-ordered plot with no indication that the option was ignored.
                sort_mode <- switch(sortBy,
                    "freq" = "descending",
                    "degree" = "descending",
                    "none" = FALSE,
                    "descending"  # default
                )
                # Verified against ComplexUpset 1.3.3 and UpSetR 1.4.1 on 3 sets:
                # sort_intersections_by = "degree" with "descending" lists the
                # HIGHEST-degree intersections FIRST (x order: 1-2-3, 2-3, 1-3, 1-2,
                # 3, 2, 1, Outside). UpSetR agrees, because .plotUpsetRHelper passes a
                # length-1 order.by = "degree", so Counter() uses decreasing[1] = TRUE
                # (its default `decreasing = c(T, F)`); the triple intersection comes
                # out at x = 1. Do NOT flip either engine without re-checking the other:
                # order.by = c("freq", "degree") would take decreasing[2] = FALSE and
                # reverse UpSetR only.
                sort_key <- if (sortBy == "degree") "degree" else "cardinality"

                # Percentage labels ride on the intersection-size bars that are already
                # drawn, instead of stacking a second, redundant panel on top of them.
                # The label expression MUST use ComplexUpset's own size-mode helper:
                # rlang::sym('intersection_size') resolves to a non-numeric column when
                # the aesthetics are computed, and the whole plot died with
                # "non-numeric argument to binary operator" as soon as the user ticked
                # "Percentage labels". The denominator is the full analysed cohort, not
                # sum() over the DISPLAYED intersections, so the percentages keep
                # meaning "share of all cases" even when minSize hides small bars.
                # text_mapping REPLACES the default count label rather than adding to
                # it, so the count is written into the same label - the analysis tells
                # the user elsewhere to report raw counts alongside percentages.
                n_cases <- nrow(upset_data)
                base_annotations_list <- list(
                    'Intersection size' = if (showAnnotations) {
                        ComplexUpset::intersection_size(
                            text = list(size = 3),
                            text_mapping = ggplot2::aes(label = paste0(
                                !!ComplexUpset::get_size_mode('exclusive_intersection'),
                                '\n',
                                round(100 * !!ComplexUpset::get_size_mode('exclusive_intersection') / n_cases, 1),
                                '%'))
                        )
                    } else {
                        ComplexUpset::intersection_size(
                            text = list(size = 3)
                        )
                    }
                )

                # ComplexUpset::upset() ABORTS - "No intersections left after filtering:
                # the maximal size for `min_size` for this dataset is N" - as soon as
                # min_size exceeds the largest exclusive intersection, and minSize is a
                # free TextBox with no upper bound. Verified against ComplexUpset 1.3.3
                # that the maximum it compares against INCLUDES the all-negative
                # ("Outside of known sets") region, so the guard is taken over every
                # membership pattern. .run() posts a notice when the guard bites.
                # Clamping to max_intersection is NOT safe: ComplexUpset keeps
                # intersections with size >= min_size, so min_size = max keeps ONLY the
                # single largest pattern and silently drops every other bar (verified
                # with ComplexUpset::upset_data() on histopathology LVI/PNI/
                # PreinvasiveComponent: min_size = 77 leaves "Outside of known sets"
                # alone, 1 of 8 patterns). An unsatisfiable filter is therefore dropped
                # altogether, which is what the notice in .run() tells the user.
                max_intersection <- if (nrow(upset_data) > 0)
                    max(table(apply(upset_data, 1, paste, collapse = "|"))) else 0L
                effective_min_size <- if (minSize > max_intersection) 0 else minSize

                plot <- ComplexUpset::upset(
                    data = upset_data,
                    intersect = names(upset_data),
                    min_size = effective_min_size,
                    sort_intersections = sort_mode,
                    sort_intersections_by = sort_key,
                    sort_sets = sort_mode,
                    name = .("Intersection Size"),
                    width_ratio = 0.1,
                    height_ratio = 0.8,
                    wrap = TRUE,
                    base_annotations = base_annotations_list,
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

            # Map the "Venn diagram shape" option onto a real ggVennDiagram shape_id.
            # The available (family, number-of-sets) combinations are read from the
            # package rather than restated here, so the mapping cannot drift out of
            # date. Returns NULL - i.e. leave the choice automatic - when the requested
            # family has no shape for this number of sets.
            .vennShapeId = function(shapeType, num_sets) {
                if (is.null(shapeType) || shapeType == "auto")
                    return(NULL)

                tryCatch({
                    shapes <- ggVennDiagram::get_shapes()
                    match_rows <- shapes[shapes$type == shapeType & shapes$nsets == num_sets, , drop = FALSE]
                    if (nrow(match_rows) == 0)
                        return(NULL)
                    as.character(match_rows$shape_id[1])
                }, error = function(e) NULL)
            },

            # Index of the layer that draws the SET NAMES in a ggVennDiagram plot.
            # It is the geom_text layer whose data names the sets; the region-label
            # layer is distinguished by carrying a "percent" column. Returns NA when
            # the layer cannot be identified (e.g. after an upstream change), and every
            # caller treats NA as "leave the plot alone".
            .setLabelLayerIndex = function(plot) {
                if (is.null(plot$layers) || length(plot$layers) == 0)
                    return(NA_integer_)
                hits <- which(vapply(plot$layers, function(ly) {
                    inherits(ly$geom, "GeomText") &&
                        is.data.frame(ly$data) &&
                        "name" %in% names(ly$data) &&
                        !("percent" %in% names(ly$data))
                }, logical(1)))
                if (length(hits) == 0) NA_integer_ else hits[1]
            },

            # Helper function for ggVennDiagram plotting (advanced)
            .plotGgVennDiagramHelper = function(mydata2, namescolumn2, ggtheme, theme) {
                private$.checkpoint()
                # Number of analysed cases, captured before mydata2 is replaced by the
                # index-list form below. It is the denominator every other output in
                # this analysis uses for percentages.
                total_n <- nrow(mydata2)

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

                # Both colours are free-text options. An unknown name reaches ggplot2
                # as "Unknown colour name: reddd" during grob conversion and destroys
                # the whole figure. Fall back to black here; .run() names the rejected
                # string in a notice.
                if (!private$.isColour(edgeColor)) edgeColor <- "black"
                if (!private$.isColour(setLabelColor)) setLabelColor <- "black"

                # Determine the shape. ggVennDiagram selects a shape through `shape_id`,
                # which is looked up BEFORE the plot is drawn (process_data(shape_id=)).
                # The previous code built a `type` argument, which is not a formal of
                # ggVennDiagram at all: it fell into `...`, was forwarded to plot_venn(),
                # never reached shape selection, raised no error, and left the shape
                # permanently automatic. The eligibility rules it encoded were wrong too
                # (triangle exists for 6 sets, not 3), so the family/set-count table is
                # now read from the package instead of being restated here.
                num_sets <- length(mydata2)
                shape_params <- list()
                shape_id <- private$.vennShapeId(shapeType, num_sets)
                if (!is.null(shape_id))
                    shape_params$shape_id <- shape_id

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
                    # ALWAYS pass the real names. category.names = NULL does not hide the
                    # set labels - ggVennDiagram strips the names and then auto-generates
                    # "Set_1", "Set_2", ..., so unticking "Set names" used to produce a
                    # figure labelled Set_1/Set_2 with no recoverable mapping back to the
                    # variables. Hiding is done after the plot is built, further down.
                    category.names = display_names,
                    label = regionLabels,
                    label_geom = labelGeometry,
                    label_percent_digit = labelPrecisionDigits,
                    label_size = labelSize,
                    # `set_size`, not `set_name_size`: the latter is not a ggVennDiagram
                    # parameter, so it was swallowed by `...` and the slider did nothing.
                    set_size = setNameSize,
                    edge_size = edgeSize,
                    edge_lty = edgeLineType,
                    # edgeAlpha has no ggVennDiagram parameter at all (no `edge_alpha`
                    # anywhere in plot_venn's body); it is applied to the boundary layer
                    # after the plot is built, further down.
                    set_color = edgeColor  # boundary AND set-name colour in ggVennDiagram
                )

                # Add shape parameters if specified
                if (length(shape_params) > 0) {
                    plot_args <- c(plot_args, shape_params)
                }

                # Create the plot
                plot <- do.call(ggVennDiagram::ggVennDiagram, plot_args)

                # ggVennDiagram::plot_venn computes its region percentages as
                # count / sum(count) over the drawn regions, i.e. over the UNION of the
                # sets. Every other number in this analysis - the Summary of True Counts
                # table, the clinical and copy-ready prose, the ggvenn labels, the
                # ComplexUpset bar labels - is a share of all analysed cases. Verified
                # on n = 100 with A-only 28, B-only 22, both 16, neither 34: ggvenn
                # printed "28 (28.0%)" while ggVennDiagram printed 42% for the same
                # region (28/66). Recompute the label columns against total_n so one
                # denominator holds across the whole analysis. The aes is
                # aes(label = .data[[label]]) over "count"/"percent"/"both", so
                # rewriting the columns is enough; label = "none" leaves no such layer
                # and the loop below simply finds nothing.
                region_layer <- which(vapply(plot$layers, function(ly)
                    is.data.frame(ly$data) && all(c("count", "percent") %in% names(ly$data)),
                    logical(1)))
                if (length(region_layer) > 0 && total_n > 0) {
                    rd <- plot$layers[[region_layer[1]]]$data
                    rd$percent <- paste0(
                        round(rd$count * 100 / total_n, digits = labelPrecisionDigits), "%")
                    if ("both" %in% names(rd))
                        rd$both <- paste(base::format(rd$count, big.mark = ",", big.interval = 3L),
                                         paste0("(", rd$percent, ")"), sep = "\n")
                    plot$layers[[region_layer[1]]]$data <- rd
                }

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
                    # build_palette_scale() returns NULL for BOTH "no palette chosen"
                    # and "fill colour mapping switched off", and the fallback below
                    # is itself a count -> colour gradient. So unticking "Fill color
                    # mapping" - whose label promises exactly the opposite - left the
                    # regions still shaded by intersection size, and the option was
                    # observably inert unless a palette happened to be selected.
                    # Flatten the scale here so the box does what it says.
                    palette_scale <- if (!isTRUE(fillColorMapping))
                        ggplot2::scale_fill_gradient(
                            low = adjusted_colors[length(adjusted_colors)],
                            high = adjusted_colors[length(adjusted_colors)],
                            guide = "none")
                    else
                        ggplot2::scale_fill_gradient(
                            low = adjusted_colors[1],
                            high = adjusted_colors[length(adjusted_colors)],
                            guide = "none")
                }

                if (!is.null(ggtheme)) {
                    plot <- plot + ggtheme
                }

                if (!is.null(palette_scale)) {
                    plot <- plot + palette_scale
                }

                # Set-name labels and boundary transparency are layer properties, and a
                # ggplot2 theme cannot reach either: the set names are drawn by
                # plot_venn as a geom_text layer that carries its own colour, so
                # theme(text = element_text(color = ...)) recoloured the plot TITLE and
                # left the set names untouched. Operate on the layers instead.
                set_label_layer <- private$.setLabelLayerIndex(plot)

                if (!showSetLabels && !is.na(set_label_layer)) {
                    plot$layers[[set_label_layer]] <- NULL
                    set_label_layer <- NA_integer_
                } else if (!is.na(set_label_layer) &&
                           "color" %in% names(plot$layers[[set_label_layer]]$data)) {
                    # ggVennDiagram::plot_venn passes set_color to BOTH
                    # get_shape_setedge() and get_shape_setlabel(), so without this the
                    # set NAMES inherit the Edge color. Skipping the write when
                    # setLabelColor == "black" treated the default as "unset" and left
                    # the names red whenever Edge color was red; "black" is a choice the
                    # user can see in the box, so it is always honoured.
                    plot$layers[[set_label_layer]]$data$color <- setLabelColor
                }

                if (!is.null(edgeAlpha) && !is.na(edgeAlpha) && edgeAlpha < 1) {
                    edge_layer <- which(vapply(plot$layers,
                        function(ly) inherits(ly$geom, "GeomPath"), logical(1)))
                    if (length(edge_layer) > 0)
                        plot$layers[[edge_layer[1]]]$aes_params$alpha <- edgeAlpha
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

            # Generate set calculations.
            # These are computed straight from the logical membership matrix rather
            # than through ggVennDiagram's set algebra. The previous version called
            # ggVennDiagram::overlap(venn, slice = "all") and
            # ggVennDiagram::discern(venn, slice = "all"): `slice` is a prefix of BOTH
            # of discern's formals (slice1, slice2), so every discern call died with
            # "argument 2 matches multiple formal arguments" inside a silent tryCatch,
            # and overlap returned an UNNAMED vector of row indices that neither
            # rendering branch below could display. Doing the counting here is a few
            # lines, is exact, and cannot break on an upstream signature change.
            .generateSetCalculations = function(mydata2, namescolumn2, summaryData) {
                tryCatch({
                    calculations <- list()
                    # Which toggles the user asked for, so the panel can tell
                    # "nothing requested" apart from "requested but empty".
                    requested <- c(
                        overlap = isTRUE(self$options$calculateOverlap),
                        discern = isTRUE(self$options$calculateDiscern),
                        unite   = isTRUE(self$options$calculateUnite))

                    present <- namescolumn2[namescolumn2 %in% names(mydata2)]
                    member_mat <- as.matrix(mydata2[, present, drop = FALSE]) == 1
                    # Show the user's own variable names, not the sanitised ones
                    display_names <- vapply(present, function(nm) {
                        if (!is.null(private$.name_mapping) && nm %in% names(private$.name_mapping))
                            as.character(private$.name_mapping[[nm]])
                        else nm
                    }, character(1), USE.NAMES = FALSE)
                    degree <- rowSums(member_mat, na.rm = TRUE)

                    # Overlaps: every pairwise intersection, plus the all-way
                    # intersection once three or more sets are selected.
                    # EVERY intersection order from 2 up to k, listed lowest order
                    # first. The previous version reported the C(k,2) pairwise
                    # intersections plus the single k-way one and said nothing about
                    # the gap: with 4 sets the four 3-way intersections were silently
                    # absent (measured on n = 200: A&B&C 21, A&B&E 24, A&C&E 27,
                    # B&C&E 21), and with 7 sets 105 of the 120 intersections were.
                    # The row count is bounded by 2^7 - 7 - 1 = 120 because the
                    # analysis accepts at most seven variables, so no cap is needed.
                    if (requested[["overlap"]] && ncol(member_mat) >= 2) {
                        k <- ncol(member_mat)
                        combos <- unlist(
                            lapply(2:k, function(m) utils::combn(k, m, simplify = FALSE)),
                            recursive = FALSE)
                        overlap_counts <- vapply(combos, function(idx)
                            sum(rowSums(member_mat[, idx, drop = FALSE], na.rm = TRUE) ==
                                    length(idx)),
                            numeric(1))
                        names(overlap_counts) <- vapply(combos, function(idx)
                            paste(display_names[idx], collapse = " & "), character(1))
                        calculations$overlaps <- overlap_counts
                    }

                    # Unique members: cases positive for exactly one of the selected sets.
                    if (requested[["discern"]] && ncol(member_mat) >= 1) {
                        unique_counts <- vapply(seq_len(ncol(member_mat)), function(j)
                            sum(degree == 1 & member_mat[, j], na.rm = TRUE), numeric(1))
                        names(unique_counts) <- display_names
                        calculations$unique_members <- unique_counts
                    }

                    # Union: cases positive for at least one of the selected sets.
                    # Stored as row indices so the existing length() rendering still works.
                    if (requested[["unite"]]) {
                        calculations$union <- which(degree > 0)
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
                        # Both vectors are built above as NAMED numeric vectors keyed by
                        # the user's own variable names, so one rendering path covers them.
                        # round() drops a trailing zero, so a whole number printed as
                        # "26%" in a list whose other entries read "23.5%".
                        pct <- function(x) formatC(100 * x / total_observations,
                                                   format = "f", digits = 1)

                        renderCounts <- function(html, heading, counts, unit, empty_msg) {
                            html <- paste0(html, "<h4>", heading, "</h4>")
                            if (is.null(counts) || length(counts) == 0)
                                return(paste0(html, "<p>", empty_msg, "</p>"))
                            labels <- names(counts)
                            for (i in seq_along(counts)) {
                                html <- paste0(html,
                                    "<p><strong>", htmltools::htmlEscape(labels[i]), ":</strong> ",
                                    counts[[i]], " ", unit, " (",
                                    pct(counts[[i]]), "%)</p>")
                            }
                            html
                        }

                        if (!is.null(calculations$overlaps)) {
                            html_content <- renderCounts(
                                html_content,
                                "Intersection Sizes (cases positive for every set listed):",
                                calculations$overlaps, "cases",
                                "No intersections could be formed from the selected sets.")
                        }

                        if (!is.null(calculations$unique_members)) {
                            html_content <- renderCounts(
                                html_content,
                                "Unique Members per Set (cases positive for that set only):",
                                calculations$unique_members, "cases",
                                "No case is positive for exactly one of the selected sets.")
                        }

                        if (!is.null(calculations$union)) {
                            html_content <- paste0(html_content, "<h4>Union of All Sets:</h4>")
                            union_size <- length(calculations$union)
                            html_content <- paste0(html_content,
                                "<p><strong>Cases positive for at least one selected set:</strong> ",
                                union_size, " cases (",
                                pct(union_size), "%)</p>")
                        }
                    } else if (!any(requested)) {
                        html_content <- paste0(html_content,
                            "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 10px; border: 1px solid #ffeaa7; border-radius: 4px; margin: 10px 0; color: inherit;'>",
                            "<p><strong>Enable calculations:</strong></p>",
                            "<p>No set calculation is currently switched on. Tick one or more of:</p>",
                            "<ul>",
                            "<li><strong>Overlap calculations:</strong> how many cases fall in every intersection of two or more sets</li>",
                            "<li><strong>Unique member calculations:</strong> how many cases are positive for exactly one set</li>",
                            "<li><strong>Union calculations:</strong> how many cases are positive for at least one set</li>",
                            "</ul>",
                            "</div>")
                    } else {
                        # Something WAS requested but produced nothing. Say which, instead
                        # of telling the user to switch on the option they already used.
                        reasons <- character(0)
                        if (requested[["overlap"]] && length(namescolumn2) < 2)
                            reasons <- c(reasons, "Overlap calculations need at least two sets.")
                        if (requested[["overlap"]] && length(namescolumn2) >= 2)
                            reasons <- c(reasons, "Overlap calculations produced no result for these sets.")
                        if (requested[["discern"]])
                            reasons <- c(reasons, "Unique-member calculations produced no result for these sets.")
                        if (requested[["unite"]])
                            reasons <- c(reasons, "Union calculations produced no result for these sets.")
                        html_content <- paste0(html_content,
                            "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 10px; border: 1px solid #ffeaa7; border-radius: 4px; margin: 10px 0; color: inherit;'>",
                            "<p><strong>Requested calculations returned nothing:</strong></p><ul>",
                            paste0("<li>", htmltools::htmlEscape(reasons), "</li>", collapse = ""),
                            "</ul></div>")
                    }

                    html_content <- paste0(html_content, "</div>")

                    # Set the content
                    self$results$setCalculations$setContent(html_content)

                }, error = function(e) {
                    # If calculations fail, show error message
                    safe_error <- htmltools::htmlEscape(conditionMessage(e))
                    error_html <- paste0(
                        "<div style='background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; padding: 12px; border-radius: 4px; color: inherit;'>",
                        "<p><strong>Set calculations could not be completed.</strong> ",
                        "The Venn and UpSet diagrams and the Summary of True Counts table are computed separately and are unaffected. ",
                        "Switch off 'Set calculations' to hide this panel, or select fewer variables and run again.</p>",
                        "<p>Technical detail: ", safe_error, "</p>",
                        "</div>")
                    self$results$setCalculations$setContent(error_html)
                })
            },

            # Write the per-case group back into the dataset as a new column.
            # `type: Output` options are client-driven: jmvtools emits
            # OptionOutput$new("membershipGroups") with no value argument and leaves
            # the name out of the generated wrapper's formals, so this runs from
            # jamovi ONLY - no example or asSource() call can enter it. It was
            # wrapped in try(silent = TRUE), which left the single
            # unreachable-by-example path in the analysis with no safety net at all:
            # the write failed, no column appeared, and nothing said why.
            .writeMembershipGroups = function(group_labels, row_numbers) {
                if (is.null(group_labels) || !self$options$membershipGroups ||
                    !self$results$membershipGroups$isNotFilled())
                    return(invisible(NULL))

                tryCatch({
                    output_rows <- row_numbers
                    if (length(output_rows) != length(group_labels))
                        output_rows <- seq_along(group_labels)
                    self$results$membershipGroups$setRowNums(output_rows)
                    self$results$membershipGroups$setValues(group_labels)
                }, error = function(e) {
                    private$.addNotice(
                        "WARNING",
                        .("Membership Groups Column Was Not Added"),
                        sprintf(.("The membership group for each case could not be written back into the dataset, so no new column has appeared. Everything else in this analysis is computed separately and is unaffected, and the same assignment is shown in the Membership Table if you switch it on. Technical detail: %s"),
                                conditionMessage(e)))
                })
                invisible(NULL)
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
                        # Build the labels from a logical MATRIX. The previous
                        # version indexed the data frame row by row
                        # (membership_data[i, cols]), which is the expensive half
                        # of a table that took 34s at n = 800.
                        member_mat <- as.matrix(
                            membership_data[, set_columns, drop = FALSE]) == "Yes"
                        group_labels <- apply(member_mat, 1L, function(z) {
                            if (!any(z)) "None" else paste(set_columns[z], collapse = " & ")
                        })

                        membership_data$Group <- group_labels
                        membership_data <- membership_data[, c("Row", "Group", set_columns), drop = FALSE]
                    } else {
                        group_labels <- rep("None", nrow(membership_data))
                    }

                    if (nrow(membership_data) == 0L) {
                        table$deleteRows()
                        return()
                    }

                    # group_labels is all the "Add membership groups to data" output
                    # needs. Everything below renders the TABLE, which is gated by
                    # `visible: (showMembershipTable)` - so with only the data output
                    # switched on this was up to 500 addRow() calls written into an
                    # element the user cannot see.
                    if (!self$options$showMembershipTable) {
                        private$.writeMembershipGroups(group_labels, row_numbers)
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

                    # Cap the rendered rows. jmvcore's addRow is called once per
                    # patient and the cost grows quadratically: measured 3.2s at
                    # n = 200, 8.3s at n = 400, 34.0s at n = 800, and a 20000-row
                    # dataset did not finish in ten minutes. A per-patient listing
                    # is also not what this table is for - the group counts are -
                    # and the full per-row assignment is available by enabling
                    # "Add membership groups to data", which writes it back as a
                    # dataset column rather than rendering it.
                    max_rows <- 500L
                    n_all <- nrow(membership_data)
                    if (n_all > max_rows) {
                        membership_data <- membership_data[seq_len(max_rows), , drop = FALSE]
                    }

                    column_mapping <- stats::setNames(safe_col_names, safe_col_names)
                    private$.populateTableSafely(table, membership_data, column_mapping)

                    if (n_all > max_rows) {
                        try({ table$setNote("truncated", .fmt(
                            .("Showing the first {shown} of {total} cases. Enable <b>Add membership groups to data</b> to get the group for every case as a dataset column; rendering one table row per case is slow and is capped here."),
                            shown = max_rows, total = n_all)) }, silent = TRUE)
                    } else {
                        try({ table$setNote("truncated", NULL) }, silent = TRUE)
                    }

                    try({ table$setNote(key = "error", note = NULL) }, silent = TRUE)

                    # `type: Output` options are client-driven: jmvtools emits
                    # OptionOutput$new("membershipGroups") with no value argument and
                    # leaves the name out of the generated wrapper's formals, so this
                    # branch is reachable from jamovi ONLY - no test, example or
                    # asSource() call can enter it. try(silent = TRUE) therefore left
                    # the single unreachable-by-test path in the analysis with no
                    # safety net at all: the user ticks "Add membership groups to
                    # data", the write fails, no column appears, and nothing anywhere
                    # says why. Report the failure instead of swallowing it.
                    private$.writeMembershipGroups(group_labels, row_numbers)

                }, error = function(e) {
                    self$results$membershipTable$setNote(
                        key = "error",
                        note = .fmt(.("Error in generating membership table: {err}"), err = e$message)
                    )
                })
            },

            # Populate jamovi tables safely using a data frame and column mapping
            .populateTableSafely = function(table_result, data_frame, column_mapping) {
                # Declared outside tryCatch so the error handler can always report progress
                rows_added <- 0

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
                        rows_added <- rows_added + 1

                        if (i %% 100 == 0) {
                            private$.checkpoint()
                        }
                    }
                }, error = function(e) {
                    private$.addNotice(
                        "WARNING",
                        "Membership Table Is Incomplete",
                        paste0("Only ", rows_added, " of ", nrow(data_frame),
                            " case rows could be written into the membership table before an internal error stopped it, so the remaining cases are missing from that table. ",
                            "The Venn and UpSet diagrams and the summary counts are computed separately and are unaffected. ",
                            "You can switch off 'Show membership table' to hide it, or select fewer variables and run again. ",
                            "Technical detail: ", conditionMessage(e))
                    )
                })
            },

            # Generate clinical interpretation of overlap patterns
            .generateClinicalInterpretation = function(intersection_data, var_names, total_n) {
                if (is.null(intersection_data) || nrow(intersection_data) < 2) return("")

                # Find largest and most meaningful overlaps
                largest_count <- max(intersection_data$TrueCount, na.rm = TRUE)
                # every variable at the maximum, not just which.max()'s first one
                largest_vars <- intersection_data$Variable[!is.na(intersection_data$TrueCount) &
                                                           intersection_data$TrueCount == largest_count]
                largest_var <- htmltools::htmlEscape(paste(largest_vars, collapse = "', '"))
                largest_pct <- round((largest_count / total_n) * 100, 1)

                # Calculate overall overlap assessment
                mean_overlap <- mean(intersection_data$TrueCount, na.rm = TRUE)
                overlap_level <- if (mean_overlap / total_n > 0.5) "high" else if (mean_overlap / total_n > 0.2) "moderate" else "low"

                interpretation <- paste0(
                    "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-left: 4px solid #28a745; margin: 10px 0; border-radius: 4px; color: inherit;'>",
                    "<h5 style='margin: 0 0 10px 0; color: inherit;'> Clinical Interpretation</h5>",
                    "<p style='margin: 0 0 8px 0;'><strong>Key Finding:</strong> In this dataset of ", total_n, " cases, ",
                    "'", largest_var, "' ",
                    if (length(largest_vars) > 1) "are equally the most prevalent, each with " else "shows the highest prevalence with ",
                    largest_count, " positive cases (", largest_pct, "%).</p>",
                    "<p style='margin: 0 0 8px 0;'><strong>Overlap Pattern:</strong> Across the selected variables, the average share of positive cases is ", overlap_level, ". ",
                    # `intersection_data` is summaryData: one row PER VARIABLE, so
                    # mean_overlap / total_n is the average positivity ACROSS variables.
                    # Phrasing it as "each variable" was false whenever the variables
                    # differ in prevalence (e.g. 90% and 20% average to "high").
                    if (overlap_level == "high") "That is, averaged over the variables, more than half of cases are positive; single variables can sit far above or below that average, so read the True % column of the Summary of True Counts table."
                    else if (overlap_level == "moderate") "That is, averaged over the variables, between one fifth and one half of cases are positive; single variables can sit outside that range, so read the True % column of the Summary of True Counts table."
                    else "That is, averaged over the variables, under one fifth of cases are positive; single variables can sit outside that range, so read the True % column of the Summary of True Counts table.",
                    " Overlap size depends on how common each variable is and does not by itself measure association.", "</p>",
                    "<p style='margin: 0; font-size: 0.9em; color: inherit;'>",
                    " <em>Clinical Relevance:</em> Use Venn diagrams to identify patient subgroups, assess diagnostic overlap, ",
                    "or evaluate multi-marker patterns in pathology and oncology research.</p>",
                    "</div>"
                )

                return(interpretation)
            },

            # Small-sample and low-prevalence advisories.
            # Returned as structured entries so the SAME wording can be pushed to the
            # always-visible notices panel from .run() AND rendered into the optional
            # Clinical Summary HTML. Previously these three checks existed only inside
            # the Clinical Summary, which is off by default - so a two-marker analysis
            # on a 15-case series showed no caution at all.
            .powerAdvisories = function(intersection_data, total_n) {
                advisories <- list()
                if (is.null(intersection_data) || nrow(intersection_data) == 0 ||
                    is.null(total_n) || is.na(total_n) || total_n == 0)
                    return(advisories)

                add <- function(title, content) {
                    advisories[[length(advisories) + 1]] <<- list(title = title, content = content)
                }

                sparse <- !is.na(intersection_data$TrueCount) & intersection_data$TrueCount < 5
                if (any(sparse)) {
                    add(
                        .("Very Few Positive Cases"),
                        sprintf(.("%d of %d selected variables have fewer than 5 positive cases (%s). The counts and percentages printed in the regions built from those variables rest on a handful of cases and move substantially when a single case changes. Report the raw counts next to the percentages, and consider collapsing categories or adding cases before reading anything into those regions."),
                                sum(sparse), nrow(intersection_data),
                                paste(intersection_data$Variable[sparse], collapse = ", ")))
                }

                rare <- !is.na(intersection_data$TrueCount) & (intersection_data$TrueCount / total_n) < 0.05
                if (any(rare)) {
                    add(
                        .("Low Prevalence"),
                        sprintf(.("%d of %d selected variables are positive in fewer than 5%% of the %d analysed cases (%s). The Venn shapes this analysis draws are fixed geometry: a region holding one case is drawn the same size as a region holding a hundred, so a rare set is easy to over-read. Go by the counts and percentages printed inside the regions, by the Summary of True Counts table, and by the UpSet plots if you switch them on."),
                                sum(rare), nrow(intersection_data), total_n,
                                paste(intersection_data$Variable[rare], collapse = ", ")))
                }

                # Mirror image of `rare`, and the playbook's <5% OR >95% rule. A set
                # holding (nearly) every case is as uninformative as one holding
                # almost none: its circle swallows the figure, the unique and
                # "outside" regions collapse to nothing, and the diagram carries
                # almost no information. Without this a variable positive in 98% -
                # or two variables positive in 100% - drew no caution at all, while
                # the 2% mirror image drew two.
                saturated <- !is.na(intersection_data$TrueCount) &
                    (intersection_data$TrueCount / total_n) > 0.95
                if (any(saturated)) {
                    add(
                        .("Very High Prevalence"),
                        sprintf(.("%d of %d selected variables are positive in more than 95%% of the %d analysed cases (%s). A set that contains almost every case fills the diagram, leaving the regions outside it nearly empty, so the picture separates almost nothing. Check that the level nominated as positive is the one you meant, and read the Summary of True Counts table rather than the shapes."),
                                sum(saturated), nrow(intersection_data), total_n,
                                paste(intersection_data$Variable[saturated], collapse = ", ")))
                }

                if (total_n < 30) {
                    add(
                        .("Small Sample"),
                        sprintf(.("Only %d complete cases are being displayed. Every intersection count and percentage below is based on that total, so single-case changes move the percentages by more than three points. Treat the pattern as exploratory and describe it with counts rather than percentages."),
                                total_n))
                }

                advisories
            },

            # HTML rendering of the same advisories for the Clinical Summary panel
            .validateStatisticalPower = function(intersection_data, total_n) {
                advisories <- private$.powerAdvisories(intersection_data, total_n)
                if (length(advisories) == 0)
                    return("")

                items <- vapply(advisories, function(adv) paste0(
                    "<strong>", htmltools::htmlEscape(adv$title), ":</strong> ",
                    htmltools::htmlEscape(adv$content)), character(1))

                paste0(
                    "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 12px; border-left: 4px solid #ffc107; margin: 10px 0; border-radius: 4px; color: inherit;'>",
                    "<h6 style='margin: 0 0 8px 0; color: inherit;'>", .("Statistical Considerations"), "</h6>",
                    paste(items, collapse = "<br><br>"),
                    "</div>")
            },

            # Generate statistical glossary
            .generateGlossary = function() {
                glossary_content <- paste0(
                    "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; border-left: 4px solid #6f42c1; color: inherit;'>",
                    "<h4 style='color: inherit; margin-top: 0;'> Statistical Glossary & Clinical Guide</h4>",

                    "<div style='display: grid; grid-template-columns: 1fr 1fr; gap: 15px; margin-bottom: 15px;'>",

                    # Venn Diagram Terms
                    "<div style='background: rgba(255, 255, 255, 0.06); padding: 12px; border-radius: 6px; border: 1px solid #e9ecef; color: inherit;'>",
                    "<h6 style='margin: 0 0 8px 0; color: inherit; border-bottom: 1px solid #dee2e6; padding-bottom: 4px;'>Venn Diagram Terms</h6>",
                    "<p style='margin: 0 0 6px 0; font-size: 0.9em;'><strong>Intersection:</strong> Cases positive for multiple variables simultaneously (overlap regions)</p>",
                    "<p style='margin: 0 0 6px 0; font-size: 0.9em;'><strong>Union:</strong> Cases positive for any of the variables (total covered area)</p>",
                    "<p style='margin: 0 0 6px 0; font-size: 0.9em;'><strong>Exclusive:</strong> Cases positive for only one specific variable</p>",
                    "<p style='margin: 0; font-size: 0.9em;'><strong>Complement:</strong> Cases negative for all variables</p>",
                    "</div>",

                    # Clinical Applications
                    "<div style='background: rgba(255, 255, 255, 0.06); padding: 12px; border-radius: 6px; border: 1px solid #e9ecef; color: inherit;'>",
                    "<h6 style='margin: 0 0 8px 0; color: inherit; border-bottom: 1px solid #dee2e6; padding-bottom: 4px;'>Clinical Applications</h6>",
                    "<p style='margin: 0 0 6px 0; font-size: 0.9em;'><strong>Biomarker Analysis:</strong> Assess multi-marker expression patterns in tumors</p>",
                    "<p style='margin: 0 0 6px 0; font-size: 0.9em;'><strong>Diagnostic Overlap:</strong> Evaluate concordance between different diagnostic methods</p>",
                    "<p style='margin: 0 0 6px 0; font-size: 0.9em;'><strong>Risk Stratification:</strong> Identify patient subgroups with multiple risk factors</p>",
                    "<p style='margin: 0; font-size: 0.9em;'><strong>Treatment Response:</strong> Compare response across different outcome measures</p>",
                    "</div>",

                    "</div>",

                    # Plot Types Explanation
                    "<div style='background: rgba(255, 255, 255, 0.06); padding: 12px; border-radius: 6px; border: 1px solid #e9ecef; margin-bottom: 15px; color: inherit;'>",
                    "<h6 style='margin: 0 0 8px 0; color: inherit; border-bottom: 1px solid #dee2e6; padding-bottom: 4px;'>Plot Type Selection Guide</h6>",
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
                    "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 12px; border-radius: 6px; border: 1px solid #ffeaa7; color: inherit;'>",
                    "<h6 style='margin: 0 0 8px 0; color: inherit;'> Statistical Considerations</h6>",
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
                        "<div style='padding: 15px; background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; border-radius: 4px; color: inherit;'>",
                        "<h4 style='margin-top: 0; color: inherit;'> Validation Errors</h4>",
                        paste(sprintf("<p style='margin: 5px 0; color: inherit;'>\u{2022} %s</p>", htmltools::htmlEscape(private$.errors)), collapse = ""),
                        "</div>",
                        sep = ""
                    )
                    self$results$validationErrors$setContent(error_html)
                    self$results$validationErrors$setVisible(TRUE)
                }

                # Display warnings
                if (length(private$.warnings) > 0) {
                    warning_html <- paste(
                        "<div style='padding: 15px; background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ffc107; border-radius: 4px; color: inherit;'>",
                        "<h4 style='margin-top: 0; color: inherit;'> Important Warnings</h4>",
                        paste(sprintf("<p style='margin: 5px 0; color: inherit;'>\u{2022} %s</p>", htmltools::htmlEscape(private$.warnings)), collapse = ""),
                        "</div>",
                        sep = ""
                    )
                    self$results$validationWarnings$setContent(warning_html)
                    self$results$validationWarnings$setVisible(TRUE)
                }

                # Display info messages
                if (length(private$.info) > 0) {
                    info_html <- paste(
                        "<div style='padding: 15px; background-color: rgba(33, 163, 188, 0.21); border-left: 4px solid #17a2b8; border-radius: 4px; color: inherit;'>",
                        "<h4 style='margin-top: 0; color: inherit;'> Analysis Information</h4>",
                        paste(sprintf("<p style='margin: 5px 0; color: inherit;'>\u{2022} %s</p>", htmltools::htmlEscape(private$.info)), collapse = ""),
                        "</div>",
                        sep = ""
                    )
                    self$results$analysisInfo$setContent(info_html)
                    self$results$analysisInfo$setVisible(TRUE)
                }
            }
        ), # End of private list
        public = list(
            #' @description
            #' Generate R source code for venn diagram analysis
            #' @return Character string with R syntax for reproducible analysis
            asSource = function() {
                # Get arguments
                args <- private$.asArgs(incData = FALSE)
                # jmvcore's .asArgs() ALREADY returns a leading "\n    ", so the
                # copy-pasted ',\n    ' prefix produced a stray blank argument line
                # in the syntax pane ("data = data,", blank, "var1 = A,"). Harmless -
                # it still parses and round-trips - but it is what the user copies.
                if (args != '')
                    args <- paste0(',', if (startsWith(args, '\n')) '' else '\n    ', args)

                # Get package name dynamically
                pkg_name <- utils::packageName()
                if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

                # Build complete function call
                paste0(pkg_name, '::venn(\n    data = data', args, ')')
            }
        ) # End of public list
    )
