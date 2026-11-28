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
#' @param rows Number of rows in the 100-square waffle chart grid (default: 10 for
#'   a 10x10 grid). With 10 columns, this creates 100 squares where each represents
#'   approximately 1% of the sample.
#' @param flip Logical, whether to flip the orientation of the waffle chart
#'   (default: FALSE). When TRUE, flips rows and columns.
#' @param color_palette Color scheme for the waffle squares. Options: "default",
#'   "colorblind" (Okabe-Ito, recommended for accessibility), "professional",
#'   "presentation", "journal", "pastel", or "dark". Default uses blue-orange gradient.
#' @param show_legend Logical, whether to display the legend (default: TRUE).
#'   Enabled by default so category labels and colors are identifiable.
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
#' - Minimum 30 cases is a common rule of thumb for stable proportions
#' - 2-5 categories optimal for visual clarity (>10 categories may be cluttered)
#' - In jamovi / `jwaffleClass`, `haven_labelled` vectors are automatically converted
#'   to factors; R wrapper callers should pre-convert with `haven::as_factor()` so
#'   value labels are preserved before `jmvcore` factor coercion
#'
#' **How Waffle Charts Work:**
#' A waffle chart uses a grid of colored squares (default 10x10 = 100 squares)
#' where each square represents 1% of the total sample (apportioned via the
#' Hamilton largest-remainder method so squares always sum to 100 per panel).
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
#' - Works best with 2-5 categories; prefer a horizontal bar chart when there are
#'   many categories or precise comparison is required
#' - Effective for presentations and publications
#' - Faceting enables subgroup comparisons
#'
#' **Statistical Considerations:**
#' - Rule of thumb: most effective with n>=30; smaller samples show unstable
#'   proportions, and at n<100 each case spans multiple squares
#' - Works best when no single category overwhelms the display (>80%)
#' - Each square represents 1% of the panel total (exact Hamilton apportionment)
#' - Categories with <5 cases may be statistically unreliable
#' - Chi-square or Fisher exact tests can evaluate proportion differences between groups
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
#' Rudis, B., & Gandy, D. (2023). waffle: Create Waffle Chart Visualizations.
#'   R package version 1.0.2. https://doi.org/10.32614/CRAN.package.waffle
#'
#' @seealso
#' \code{\link[waffle]{geom_waffle}} for the underlying waffle geom
#'
#' @family JJStatsPlot visualization functions
#' @keywords hplot distribution categorical
#'
#' @importFrom R6 R6Class
#' @importFrom magrittr %>%
#' @importFrom glue glue
#' @importFrom rlang sym
#' @importFrom digest digest
jwaffleClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jwaffleClass",
    inherit = jwaffleBase,
    private = list(
        # Performance optimization: cache variables
        .prepared_data = NULL,
        .aggregate_cache = NULL,
        .aggregate_key = NULL,
        .prepared_options = NULL,
        .data_hash = NULL,
        .options_hash = NULL,
        .cached_plot = NULL,
        .cached_palette = NULL,
        .messages = NULL,
        # Highest severity seen across the accumulated data-quality messages.
        # .accumulateMessage() collapses them into ONE notice block, so the block's
        # type must be the MAXIMUM severity, never the latest caller's - otherwise a
        # plain WARNING raised after a STRONG_WARNING silently downgrades the whole
        # block (severity must escalate only).
        .max_severity = "",

        # Notice collection rendered into the Preformatted notices result item (plain text).
        .noticeList = list(),

        .addNotice = function(type, title, content) {
            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type, title = title, content = content
            )
            private$.renderNotices()
        },

        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                self$results$notices$setContent("")
                return()
            }
            blocks <- vapply(private$.noticeList, function(notice) {
                # STRONG_WARNING gets its own prefix: mapping it onto "WARNING: "
                # made the most consequential message in this analysis (a category
                # that is absent from the figure but present in the summary text)
                # read at exactly the same weight as "your sample is small".
                prefix <- switch(notice$type,
                    ERROR = "ERROR: ", STRONG_WARNING = "IMPORTANT: ",
                    WARNING = "WARNING: ", "")
                paste0(prefix, notice$title, "\n", notice$content)
            }, character(1))
            self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
        },

        .init = function() {
            # Reset messages for new analysis
            private$.resetMessages()
            
            base_width <- 600
            base_height <- 500
            
            if (!is.null(self$options$facet) && !is.null(self$data)) {
                facet_var <- self$options$facet
                if (facet_var %in% names(self$data)) {
                    num_levels <- max(1L, length(unique(stats::na.omit(self$data[[facet_var]]))))
                    # Cap the total width so a high-cardinality facet variable does not
                    # produce an enormous (and effectively unrenderable) image.
                    max_total_width <- 3000
                    width <- min(max(base_width, num_levels * base_width), max_total_width)
                    self$results$plot$setSize(width, base_height)
                } else {
                    self$results$plot$setSize(base_width, base_height)
                }
            } else {
                self$results$plot$setSize(base_width, base_height)
            }
        },

        # Enhanced message management system (dual output: HTML + Notice)
        .severityRank = function(type) {
            switch(type, ERROR = 4L, STRONG_WARNING = 3L, WARNING = 2L, INFO = 1L, 0L)
        },

        .accumulateMessage = function(message, notice_type = "WARNING") {
            if (is.null(private$.messages)) {
                private$.messages <- character()
            }
            private$.messages <- append(private$.messages, message)
            if (private$.severityRank(notice_type) >
                private$.severityRank(private$.max_severity))
                private$.max_severity <- notice_type

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
                # Only strip real tags: a bare "<[^>]*>" starts matching at the "<" of
                # literal text like "<5 cases" (or a factor level named "<5 cm") and runs
                # to the next ">" - the one closing a later <br> - deleting the whole
                # sentence in between.
                clean_msg <- gsub("</?[A-Za-z][^>]*>", "", clean_msg)
                # Decode HTML entities for the Preformatted plain-text notices box
                # (warnings HTML retains the escaped entities). Decode &amp; last
                # so double-encoded sequences are not over-decoded.
                clean_msg <- gsub("&nbsp;", " ", clean_msg, fixed = TRUE)
                clean_msg <- gsub("&lt;", "<", clean_msg, fixed = TRUE)
                clean_msg <- gsub("&gt;", ">", clean_msg, fixed = TRUE)
                clean_msg <- gsub("&quot;", "\"", clean_msg, fixed = TRUE)
                clean_msg <- gsub("&#0?39;", "'", clean_msg)
                clean_msg <- gsub("&amp;", "&", clean_msg, fixed = TRUE)
                # Each message is wrapped in <br> markers, so tag-stripping leaves
                # ragged leading spaces and runs of 3+ newlines in the Preformatted
                # box. Normalise to exactly one blank line between messages.
                clean_msg <- gsub("[ \t]+\n", "\n", clean_msg)
                clean_msg <- gsub("\n[ \t]+", "\n", clean_msg)
                clean_msg <- gsub("\n{3,}", "\n\n", clean_msg)
                clean_msg <- trimws(clean_msg)

                tryCatch({
                    # clean_msg already holds ALL accumulated messages combined, so
                    # reset the notice list first to avoid appending duplicate blocks.
                    private$.noticeList <- list()
                    private$.addNotice(private$.max_severity, "Data Quality Notes", clean_msg)
                }, error = function(e) {
                    # Silent fail if notice system unavailable
                })
            }
        },
        
        # Reset messages for new analysis run
        .resetMessages = function() {
            private$.messages <- character()
            private$.noticeList <- list()
            private$.max_severity <- ""
            if (!is.null(self$results$warnings)) {
                self$results$warnings$setContent("")
                # Hide the Messages box when empty so a stale/empty box is not shown;
                # .accumulateMessage() re-enables it whenever a message is produced.
                self$results$warnings$setVisible(FALSE)
            }
            # Clear stale data-quality notices so they do not persist into a run that
            # produces none but whose clearWith (groups/counts/facet/rows/color_palette)
            # has not fired (e.g. underlying data values changed without a variable
            # selection change).
            if (!is.null(self$results$notices)) {
                self$results$notices$setContent("")
            }
        },

        # Consolidated empty-data guard shared by .run (early + post-prep) and .plot,
        # so every entry point rejects with a single consistent message.
        .requireData = function(data) {
            if (is.null(data) || nrow(data) == 0)
                jmvcore::reject('Data contains no (complete) rows')
            invisible(data)
        },

        # Normalize factors carrying an explicit NA level (e.g. addNA() or
        # factor(..., exclude = NULL)) back to genuine missing values so
        # complete.cases() excludes them rather than counting "NA" as a category.
        .normalizeFactorNA = function(x) {
            if (is.factor(x) && anyNA(levels(x))) {
                keep_lvls <- levels(x)[!is.na(levels(x))]
                if (is.ordered(x)) {
                    return(ordered(as.character(x), levels = keep_lvls))
                }
                return(factor(as.character(x), levels = keep_lvls))
            }
            x
        },

        # Exact integer Hamilton (largest-remainder) apportionment to total_squares.
        # Shared by .validateInputs() and .plot() so vanishing-category diagnostics
        # and rendered tiles are guaranteed identical and immune to the IEEE 754
        # `as.integer(values * 100)` truncation bug in waffle:::StatWaffle (which
        # truncates 29%, 57%, and 58% to 28, 56, and 57 squares).
        .apportionSquares = function(counts, total_squares = 100L) {
            counts <- as.numeric(counts)
            total <- sum(counts)
            if (length(counts) == 0L || !is.finite(total) || total <= 0) {
                return(rep(0L, length(counts)))
            }
            raw <- (counts / total) * total_squares
            fl <- floor(raw)
            rem <- raw - fl
            need <- as.integer(round(total_squares - sum(fl)))
            if (need > 0L) {
                idx <- utils::tail(order(rem), need)
                fl[idx] <- fl[idx] + 1L
            }
            as.integer(round(fl))
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

            # Use digest (matching .calculateDataHash) so NULL vs empty-string options
            # produce distinct hashes. paste(list, collapse) coerces NULL away and can
            # collide two option sets onto one cache key (worst case: a stale plot).
            if (requireNamespace("digest", quietly = TRUE)) {
                return(digest::digest(options_list, algo = "md5"))
            }
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

            if (!identical(private$.data_hash, current_hash)) {
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

                # Labelled-logic parity and explicit-NA factor normalization BEFORE
                # complete.cases(), so addNA() factors and haven_labelled vectors are
                # handled consistently across validation, summaries, and plotting.
                cat_vars <- character(0)
                if (!is.null(self$options$groups) && self$options$groups != "")
                    cat_vars <- c(cat_vars, self$options$groups)
                if (!is.null(self$options$facet) && self$options$facet != "")
                    cat_vars <- c(cat_vars, self$options$facet)
                for (v in cat_vars) {
                    if (v %in% names(mydata)) {
                        if (inherits(mydata[[v]], "haven_labelled")) {
                            mydata[[v]] <- haven::as_factor(mydata[[v]], levels = "both")
                        }
                        mydata[[v]] <- private$.normalizeFactorNA(mydata[[v]])
                    }
                }

                # Only remove rows with NA in relevant columns, not all columns
                # (.validateInputs() surfaces the excluded-row count to the user in notices/warnings)
                if (length(relevant_vars) > 0) {
                    complete_rows <- complete.cases(mydata[, relevant_vars, drop = FALSE])
                    mydata <- mydata[complete_rows, , drop = FALSE]
                }

                private$.prepared_data <- mydata
                private$.data_hash <- current_hash
            }

            return(private$.prepared_data)
        },
        
        .prepareOptions = function() {
            current_hash <- private$.calculateOptionsHash()
            
            if (!identical(private$.options_hash, current_hash)) {
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

            if (is.null(private$.cached_palette) || !identical(attr(private$.cached_palette, "hash"), current_hash)) {
                # Discrete palettes subset their validated anchor colors when
                # n_groups <= length(anchors) (preserving Okabe-Ito colorblind safety
                # and avoiding endpoint collisions), interpolating only when more
                # categories than anchors are requested.
                pick_discrete <- function(anchors, n) {
                    if (n <= length(anchors)) {
                        return(anchors[seq_len(n)])
                    }
                    grDevices::colorRampPalette(anchors)(n)
                }

                pal_name <- self$options$color_palette
                selected_palette <- switch(
                    pal_name,
                    default      = grDevices::colorRampPalette(c("#4DA6FF", "#FFB84D"))(n_groups),
                    colorblind   = pick_discrete(c("#999999", "#E69F00", "#56B4E9", "#009E73",
                                                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), n_groups),
                    professional = pick_discrete(c("#2C3E50", "#E74C3C", "#3498DB", "#2ECC71",
                                                   "#F39C12", "#9B59B6", "#1ABC9C", "#34495E"), n_groups),
                    presentation = pick_discrete(c("#003f5c", "#bc5090", "#ffa600", "#58508d",
                                                   "#ff6361"), n_groups),
                    journal      = pick_discrete(c("#334455", "#778899", "#99AABB", "#BBCCDD",
                                                   "#556677", "#667788"), n_groups),
                    pastel       = pick_discrete(c("#69b3a2", "#404080", "#FFA07A", "#98D8E8",
                                                   "#F7DC6F", "#BB8FCE"), n_groups),
                    dark         = pick_discrete(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",
                                                   "#66A61E", "#E6AB02"), n_groups),
                    grDevices::colorRampPalette(c("#4DA6FF", "#FFB84D"))(n_groups)
                )

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
            # Percentage of the total that ONE SQUARE represents. The chart is
            # always drawn with n_squares squares, so this is 100/n_squares = 1%
            # by construction, whatever n is.
            #
            # This used to be `100 / total_cases`, which is the percentage per
            # CASE, not per square - so every caption reported the reciprocal:
            # "approximately 2.0%" at n = 50, "0.5%" at n = 200, "0.2%" at
            # n = 437. It happened to be right only at n = 100.
            pct_per_square <- 100 / n_squares

            # Choose appropriate terminology and adaptive precision so small totals
            # (e.g. n = 2..4 or normalized weights summing to 1) never print "0.0 cases".
            unit_label <- if (is_weighted) "weighted units" else "cases"
            units_str <- if (units_per_square >= 0.1) {
                sprintf("%.1f", units_per_square)
            } else {
                sprintf("%.2g", units_per_square)
            }

            # Generate base caption
            if (!is.null(facet_var) && !is.null(facet_level)) {
                # Facet-specific caption
                caption_text <- sprintf(
                    "%s = %s: Each square ~ %s %s (%.1f%%) (total = %g)",
                    facet_var, facet_level, units_str, unit_label, pct_per_square, total_cases
                )
            } else if (!is.null(facet_var)) {
                # General faceted caption (when not specific to a level).
                # Exact per-panel Hamilton apportionment scales EACH panel to 100 squares,
                # so a grand-total cases-per-square figure is wrong per panel. Report the
                # honest per-panel proportion plus grand total; per-panel n rides on strips.
                caption_text <- sprintf(
                    "Each panel is scaled independently; each square ~ 1%% within that %s group (total n=%g across all panels)",
                    facet_var, total_cases
                )
            } else {
                # Simple caption
                caption_text <- sprintf(
                    "Each square represents %s %s (approximately %.1f%%) (total n=%g)",
                    units_str, unit_label, pct_per_square, total_cases
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
            # jamovi does not surface base warning() output, so the slow-render
            # heads-up has to reach an element the user can actually see.
            #
            # Two constraints decide where and how:
            #   1. It lives HERE, not in .run(). .plot() calls .resetMessages()
            #      (which blanks the notices item) and then calls this method again,
            #      so only notices produced from here are re-created on the render
            #      pass; one emitted in .run() is wiped the moment the plot draws.
            #   2. It goes through .accumulateMessage(), NOT .addNotice().
            #      .accumulateMessage() REPLACES the whole notice list with a single
            #      "Data Quality Notes" block built from private$.messages, so any
            #      notice added separately via .addNotice() is destroyed as soon as
            #      the next message accumulates.
            #
            # It used to say "Rendering may be slow. Consider sampling or aggregating
            # your data." That is measurably false: make_proportional = TRUE always
            # draws exactly 100 squares, so render cost does not scale with n at all
            # (measured end-to-end: 0.16 s at 20,000 rows, 0.26 s at 150,000, 1.25 s at
            # 1,000,000). Sampling would have thrown away data to fix a non-problem.
            # What IS true at large n is that each square hides a large absolute number
            # of cases, so a visually negligible sliver can still be hundreds of patients.
            if (nrow(self$data) > 100000) {
                # base::format() explicitly: jmvcore exports its own format() generic,
                # which silently ignores big.mark and returns the bare number.
                per_square <- base::format(round(nrow(self$data) / 100), big.mark = ",",
                                           scientific = FALSE, trim = TRUE)
                private$.accumulateMessage(
                    paste0("<br> <strong>Large Dataset:</strong> ",
                           base::format(nrow(self$data), big.mark = ",",
                                        scientific = FALSE, trim = TRUE),
                           " rows. The waffle grid always holds 100 squares, so one ",
                           "square stands for about ", per_square, " cases here. Small ",
                           "but clinically important categories can be invisible at ",
                           "this scale - read the proportions from the summary text, ",
                           "not from the square count.<br>"),
                    notice_type = "WARNING")
            }

            # Check if required groups variable exists
            if (is.null(self$options$groups) || self$options$groups == "") {
                jmvcore::reject("Please specify a grouping variable for the waffle chart.")
            }

            # Defense-in-depth backstop for rows (covers R callers using run(noThrow = TRUE))
            rows_val <- self$options$rows
            if (!is.numeric(rows_val) || length(rows_val) != 1L || !is.finite(rows_val) || rows_val < 1) {
                jmvcore::reject("Number of rows must be a positive integer (>= 1).")
            }

            if (!self$options$groups %in% names(self$data)) {
                jmvcore::reject(
                    code = NULL,
                    "Grouping variable '{}' not found in data. Available variables: {}",
                    self$options$groups, paste(names(self$data), collapse = ", "))
            }

            # Check optional counts variable
            if (!is.null(self$options$counts) && self$options$counts != "" &&
                !self$options$counts %in% names(self$data)) {
                jmvcore::reject(
                    code = NULL,
                    "Counts variable '{}' not found in data. Available variables: {}",
                    self$options$counts, paste(names(self$data), collapse = ", "))
            }

            # Check optional facet variable
            if (!is.null(self$options$facet) && self$options$facet != "" &&
                !self$options$facet %in% names(self$data)) {
                jmvcore::reject(
                    code = NULL,
                    "Facet variable '{}' not found in data. Available variables: {}",
                    self$options$facet, paste(names(self$data), collapse = ", "))
            }

            if (!is.null(self$options$facet) && self$options$facet != "" &&
                identical(self$options$groups, self$options$facet)) {
                jmvcore::reject(
                    code = NULL,
                    "Grouping variable and Facet variable cannot be the same ('{}'). Please select two different variables.",
                    self$options$groups)
            }

            # Validate data types and normalize labelled / explicit-NA factors
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
                jmvcore::reject(
                    code = NULL,
                    "Grouping variable '{}' must be categorical (factor, character, or logical), not {}",
                    self$options$groups, class(groups_data)[1])
            }
            groups_data <- private$.normalizeFactorNA(groups_data)

            has_counts <- !is.null(self$options$counts) && self$options$counts != ""
            has_facet  <- !is.null(self$options$facet) && self$options$facet != ""

            # Validate counts variable type and domain (negative / Inf) before filtering
            if (has_counts) {
                counts_data <- self$data[[self$options$counts]]
                if (!is.numeric(counts_data)) {
                    jmvcore::reject(
                        code = NULL,
                        "Counts variable '{}' must be numeric, not {}",
                        self$options$counts, class(counts_data)[1])
                }
                if (any(counts_data < 0, na.rm = TRUE)) {
                    n_negative <- sum(counts_data < 0, na.rm = TRUE)
                    jmvcore::reject(
                        code = NULL,
                        "Counts variable '{}' contains {} negative value(s). All counts must be non-negative for waffle charts. Please check your data.",
                        self$options$counts, n_negative)
                }
                n_nonfinite <- sum(!is.finite(counts_data) & !is.na(counts_data))
                if (n_nonfinite > 0) {
                    jmvcore::reject(
                        code = NULL,
                        "Counts variable '{}' contains {} non-finite value(s) (Inf or -Inf). All counts must be finite numbers. Please check your data.",
                        self$options$counts, n_nonfinite)
                }
                if (sum(counts_data, na.rm = TRUE) <= 0) {
                    jmvcore::reject(
                        code = NULL,
                        "Counts variable '{}' sums to zero, so there is nothing to draw. Please check your data.",
                        self$options$counts)
                }
            }

            # Build normalized working frame of active analysis variables so every
            # structural refusal and data-quality notice below evaluates the EXACT
            # complete-case sample that .prepareData(), .generateSummary(), and .plot()
            # actually analyse (never raw unfiltered self$data).
            miss_vars <- self$options$groups
            clean_df <- data.frame(groups_col = groups_data)
            if (has_counts) {
                miss_vars <- c(miss_vars, self$options$counts)
                clean_df$counts_col <- self$data[[self$options$counts]]
            }
            if (has_facet) {
                miss_vars <- c(miss_vars, self$options$facet)
                facet_data <- self$data[[self$options$facet]]
                if (inherits(facet_data, "haven_labelled")) {
                    facet_data <- haven::as_factor(facet_data, levels = "both")
                }
                if (is.character(facet_data) || is.logical(facet_data)) {
                    facet_data <- as.factor(facet_data)
                }
                facet_data <- private$.normalizeFactorNA(facet_data)
                clean_df$facet_col <- facet_data
            }

            n_rows <- nrow(self$data)
            complete_mask <- stats::complete.cases(clean_df)
            n_dropped <- sum(!complete_mask)
            clean_df <- clean_df[complete_mask, , drop = FALSE]

            # Report excluded rows before downstream diagnostics
            if (n_dropped > 0) {
                private$.accumulateMessage(glue::glue(
                    "<br> <strong>Missing Data:</strong> {n_dropped} of {n_rows} row(s) ",
                    "excluded because of missing values in the analysis variables ",
                    "({paste(htmltools::htmlEscape(miss_vars), collapse = ', ')}). ",
                    "The chart and the summary are ",
                    "based on {nrow(clean_df)} row(s).<br>"
                ))
            }

            # Post-filtering structural guards
            if (all(is.na(groups_data))) {
                jmvcore::reject(
                    code = NULL,
                    "Grouping variable '{}' has no non-missing values, so there is nothing to chart.",
                    self$options$groups)
            }
            if (nrow(clean_df) == 0) {
                jmvcore::reject("Data contains no (complete) rows")
            }

            clean_groups <- droplevels(clean_df$groups_col)
            n_categories <- length(unique(clean_groups))

            if (n_categories == 0) {
                jmvcore::reject(
                    code = NULL,
                    "Grouping variable '{}' has no non-missing values, so there is nothing to chart.",
                    self$options$groups)
            }

            if (n_categories == 1) {
                jmvcore::reject(
                    code = NULL,
                    "Only one category found in '{}'. Waffle charts require multiple categories to show proportions. Consider using a different visualization for single-category data.",
                    self$options$groups)
            }

            # Complete-case weighted count checks (overall and per-facet panel)
            if (has_counts) {
                n_total <- sum(clean_df$counts_col)
                if (!is.finite(n_total)) {
                    jmvcore::reject(
                        code = NULL,
                        "Counts variable '{}' sum overflows to a non-finite value. Please rescale your weights and try again.",
                        self$options$counts)
                }
                if (n_total <= 0) {
                    jmvcore::reject(
                        code = NULL,
                        "Counts variable '{}' sums to zero across complete cases, so there is nothing to draw. Please check your data.",
                        self$options$counts)
                }
                if (has_facet) {
                    clean_facet <- droplevels(as.factor(clean_df$facet_col))
                    panel_totals <- tapply(clean_df$counts_col, clean_facet, sum)
                    ovf_panels <- names(panel_totals)[!is.finite(panel_totals) & !is.na(panel_totals)]
                    if (length(ovf_panels) > 0) {
                        jmvcore::reject(
                            code = NULL,
                            "Counts variable '{}' sum overflows to a non-finite value in facet level(s): {}. Please rescale your weights.",
                            self$options$counts, paste(ovf_panels, collapse = ", "))
                    }
                    zero_panels <- names(panel_totals)[is.na(panel_totals) | panel_totals <= 0]
                    if (length(zero_panels) > 0) {
                        jmvcore::reject(
                            code = NULL,
                            "Counts variable '{}' sums to zero in facet level(s): {}. Each facet panel must have positive total counts.",
                            self$options$counts, paste(zero_panels, collapse = ", "))
                    }
                }
                category_counts <- tapply(clean_df$counts_col, clean_groups, sum)
            } else {
                n_total <- nrow(clean_df)
                category_counts <- table(clean_groups, useNA = "no")
            }
            category_counts <- category_counts[!is.na(category_counts)]
            case_label <- if (has_counts) "weighted cases" else "cases"

            # User-controlled identifiers (column names, factor labels) must be HTML-escaped
            # before flowing into glue() / setContent on Html outputs.
            groups_name_safe <- htmltools::htmlEscape(self$options$groups)

            if (n_categories > 10) {
                private$.accumulateMessage(glue::glue(
                    "<br> <strong>Many Categories:</strong> {n_categories} categories detected in '{groups_name_safe}'. ",
                    "Consider grouping rare categories as 'Other' for clearer clinical presentation.<br>"
                ))
            }

            # Check sample size adequacy on complete cases
            if (n_total < 30) {
                private$.accumulateMessage(glue::glue(
                    "<br> <strong>Small Sample:</strong> Total n={n_total} {case_label}. ",
                    "Proportions may be unstable. ",
                    "Consider combining categories or collecting more data.<br>"
                ))
            }

            # Check for very small category sizes on complete cases
            min_count <- min(category_counts)
            if (min_count < 5 && n_total >= 30) {
                small_cats <- names(category_counts)[category_counts < 5]
                n_small <- length(small_cats)
                show_small <- min(n_small, 8L)
                small_cats_safe <- paste0(
                    paste(htmltools::htmlEscape(small_cats[seq_len(show_small)]), collapse = ', '),
                    if (n_small > show_small) paste0(" and ", n_small - show_small, " more") else "")
                private$.accumulateMessage(glue::glue(
                    "<br> <strong>Rare Categories:</strong> Some categories hold fewer than 5 ",
                    "{case_label}: {small_cats_safe}. Estimates for these are unreliable - ",
                    "consider combining them into an 'Other' group. ",
                    "(A separate check below reports categories too small to be DRAWN at all.)<br>"
                ))
            }

            # EXACT VANISHING-CATEGORY CHECK (overall or per-panel when faceted).
            # Uses the exact same .apportionSquares() Hamilton apportionment that
            # .plot() draws, so this notice fires if and only if a category present
            # in a panel receives 0 drawn squares in that panel (0% FN, 0% FP).
            vanished_items <- character(0)
            if (has_facet) {
                clean_facet <- droplevels(as.factor(clean_df$facet_col))
                for (flv in levels(clean_facet)) {
                    sub_idx <- clean_facet == flv
                    sub_g <- droplevels(clean_groups[sub_idx])
                    if (has_counts) {
                        sub_cnt <- tapply(clean_df$counts_col[sub_idx], sub_g, sum)
                    } else {
                        sub_cnt <- table(sub_g, useNA = "no")
                    }
                    sub_cnt <- sub_cnt[!is.na(sub_cnt)]
                    sub_tot <- sum(sub_cnt)
                    if (length(sub_cnt) > 0 && is.finite(sub_tot) && sub_tot > 0) {
                        sq <- private$.apportionSquares(sub_cnt, 100L)
                        van_idx <- which(as.numeric(sub_cnt) > 0 & sq == 0L)
                        if (length(van_idx) > 0) {
                            sub_shares <- as.numeric(sub_cnt[van_idx]) / sub_tot
                            for (j in seq_along(van_idx)) {
                                cat_nm <- htmltools::htmlEscape(names(sub_cnt)[van_idx[j]])
                                fac_nm <- htmltools::htmlEscape(flv)
                                pct_s  <- paste0(formatC(100 * sub_shares[j], format = "f", digits = 2), "%")
                                vanished_items <- c(vanished_items,
                                                    sprintf("%s (%s in %s)", cat_nm, pct_s, fac_nm))
                            }
                        }
                    }
                }
            } else {
                share_total <- sum(category_counts, na.rm = TRUE)
                if (length(category_counts) > 0 && is.finite(share_total) && share_total > 0) {
                    sq <- private$.apportionSquares(category_counts, 100L)
                    van_idx <- which(as.numeric(category_counts) > 0 & sq == 0L)
                    if (length(van_idx) > 0) {
                        sub_shares <- as.numeric(category_counts[van_idx]) / share_total
                        for (j in seq_along(van_idx)) {
                            cat_nm <- htmltools::htmlEscape(names(category_counts)[van_idx[j]])
                            pct_s  <- paste0(formatC(100 * sub_shares[j], format = "f", digits = 2), "%")
                            vanished_items <- c(vanished_items, sprintf("%s (%s)", cat_nm, pct_s))
                        }
                    }
                }
            }

            if (length(vanished_items) > 0) {
                n_vanished <- length(vanished_items)
                show_n <- min(n_vanished, 8L)
                vanished_safe <- paste0(
                    paste(vanished_items[seq_len(show_n)], collapse = ", "),
                    if (n_vanished > show_n)
                        paste0(" and ", n_vanished - show_n, " more") else "")
                scope_txt <- if (has_facet) "in that panel" else "of the total"
                private$.accumulateMessage(glue::glue(
                    "<br> <strong>Categories missing from the chart:</strong> ",
                    "{vanished_safe}. ",
                    "Each waffle grid holds 100 squares (one per percentage point via largest-remainder rounding), ",
                    "so categories with very small shares {scope_txt} round to zero squares and are not ",
                    "drawn - even though they still appear in the summary text and ",
                    "in your data. Report those categories in the text, or combine them ",
                    "into an 'Other' group so the figure and the numbers agree.<br>"
                ), notice_type = "STRONG_WARNING")
            }


        },
        
        .aggregateData = function(data, groups_var, facet_var = NULL, counts_var = NULL) {
            # Pre-check for empty data
            if (is.null(data) || nrow(data) == 0) {
                jmvcore::reject("Cannot aggregate empty dataset. Please ensure your data contains valid rows.")
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

                # Drop unused factor levels so the palette length (keyed to the observed
                # groups via length(unique(...))) matches the number of levels ggplot
                # maps in scale_fill_manual. Levels left empty by complete.cases /
                # filtering would otherwise trigger 'Insufficient values in manual scale'.
                for (gv in group_vars) {
                    if (gv %in% names(result) && is.factor(result[[gv]]))
                        result[[gv]] <- droplevels(result[[gv]])
                }

                return(result)
            }, error = function(e) {
                jmvcore::reject(
                    code = NULL,
                    "Error aggregating data: {}. Please check that your variables are properly formatted.",
                    e$message)
            })
        },
        
        # .run() (for the summary) and .plot() (for the chart) both need the same
        # aggregate; without this they grouped and summarised the data twice per cycle.
        .getAggregate = function(data, groups_var, facet_var = NULL, counts_var = NULL) {
            blank <- function(x) if (is.null(x)) "" else as.character(x)
            key <- paste(blank(private$.data_hash), blank(groups_var),
                         blank(facet_var), blank(counts_var), sep = "\r")
            if (is.null(private$.aggregate_cache) ||
                !identical(key, private$.aggregate_key)) {
                private$.aggregate_cache <- private$.aggregateData(
                    data, groups_var, facet_var, counts_var)
                private$.aggregate_key <- key
            }
            private$.aggregate_cache
        },

        # which.max() silently picks the first of a tie, so a three-way split was
        # reported as "A represents the largest proportion (33.3%)" with no hint that
        # B and C were level with it. Name the ties.
        .dominantPhrase = function(labels, proportions) {
            idx <- which.max(proportions)
            tied <- which(abs(proportions - proportions[idx]) < 1e-9)
            if (length(tied) > 1) {
                lab <- as.character(labels[tied])
                lab <- if (length(lab) == 2) paste(lab, collapse = " and ")
                       else paste0(paste(lab[-length(lab)], collapse = ", "),
                                   " and ", lab[length(lab)])
                return(list(label = lab, value = proportions[idx], tied = TRUE))
            }
            list(label = labels[idx], value = proportions[idx], tied = FALSE)
        },

        .generateSummary = function(plotdata, groups_var, facet_var = NULL, total_cases, is_weighted = FALSE) {
            # Generate natural language summary of waffle chart results
            if (is.null(plotdata) || nrow(plotdata) == 0 || total_cases == 0) {
                return("<b>No data available for analysis summary.</b>")
            }

            # HTML-escape every user-controlled identifier (column names + factor labels)
            # before interpolating into the HTML summary. Column names and factor labels
            # are arbitrary strings from the user's dataset - escape once at the boundary
            # so the rest of the function can stay readable.
            groups_var_safe <- htmltools::htmlEscape(groups_var)
            plotdata[[groups_var]] <- htmltools::htmlEscape(as.character(plotdata[[groups_var]]))
            facet_var_safe <- NULL
            if (!is.null(facet_var) && facet_var != "" && facet_var %in% names(plotdata)) {
                facet_var_safe <- htmltools::htmlEscape(facet_var)
                plotdata[[facet_var]] <- htmltools::htmlEscape(as.character(plotdata[[facet_var]]))
            }

            # Choose appropriate terminology
            unit_label <- if (is_weighted) "weighted units" else "cases"

            if (!is.null(facet_var) && facet_var != "" && facet_var %in% names(plotdata)) {
                # Faceted summary
                summary_parts <- list()

                for (facet_level in unique(plotdata[[facet_var]])) {
                    facet_data <- plotdata[plotdata[[facet_var]] == facet_level, ]
                    facet_total <- sum(facet_data$count)
                    if (!is.finite(facet_total) || facet_total <= 0) {
                        summary_parts[[facet_level]] <- sprintf(
                            "<b>Among %s %s</b> (n=0): No positive counts available.",
                            facet_var_safe, facet_level
                        )
                        next
                    }
                    # NO order() here. The escaping above turned the group column
                    # into character, so order() sorted it ALPHABETICALLY and the
                    # text listed ordinal categories out of order relative to the
                    # figure ("G1; G10; G2" for levels G1/G2/G10). .aggregateData()
                    # already returns rows sorted by factor level (dplyr group_by
                    # sorts on the grouping keys), and subsetting preserves that, so
                    # the natural row order is the chart's legend order.
                    proportions <- (facet_data$count / facet_total) * 100

                    # Create breakdown showing all categories within this facet.
                    # counts (facet_data$count) are doubles when a numeric weight
                    # variable is used, so format with %g rather than %d (sprintf
                    # "%d" errors on non-integer doubles).
                    category_breakdown <- paste(
                        sprintf("%s %s: %.1f%% (n=%g)",
                               groups_var_safe,
                               facet_data[[groups_var]],
                               proportions,
                               facet_data$count),
                        collapse = "; "
                    )

                    # Find the dominant category within this facet
                    dom <- private$.dominantPhrase(facet_data[[groups_var]], proportions)

                    summary_parts[[facet_level]] <- sprintf(
                        "<b>Among %s %s</b> (n=%g): %s \u2192 <i>%s %s %s (%.1f%%)</i>",
                        facet_var_safe, facet_level, facet_total, category_breakdown,
                        groups_var_safe, dom$label,
                        if (dom$tied) "are tied for the largest share" else "predominates",
                        dom$value
                    )
                }

                summary_text <- paste0(
                    "<b> Waffle Chart Summary by ", facet_var_safe, ":</b><br><br>",
                    paste(summary_parts, collapse = "<br><br>"),
                    "<br><br><b> Overall Sample:</b> ", total_cases, " ", unit_label, " showing ", groups_var_safe,
                    " distribution across ", length(unique(plotdata[[facet_var]])), " ", facet_var_safe, " groups.",
                    "<br><br><b> Clinical Note:</b> Compare how ", groups_var_safe,
                    " patterns differ between ", facet_var_safe, " groups. Each square represents a fixed proportion of ", unit_label, "."
                )

            } else {
                # Simple summary
                proportions <- (plotdata$count / total_cases) * 100
                dom <- private$.dominantPhrase(plotdata[[groups_var]], proportions)
                max_prop_idx <- which.max(proportions)
                dominant_category <- dom$label
                max_proportion <- dom$value
                largest_phrase <- if (dom$tied)
                    "are tied for the largest proportion" else "represents the largest proportion"
                frequent_phrase <- if (dom$tied)
                    "as the most frequent categories, tied" else "as the most frequent category"
                each_suffix <- if (dom$tied) " each" else ""

                # Create simple breakdown (counts are doubles for weighted data, so
                # format with %g rather than %d to avoid a sprintf crash).
                breakdown_list <- paste(
                    sprintf("%s: %.1f%% (n=%g)",
                           plotdata[[groups_var]],
                           proportions,
                           plotdata$count),
                    collapse = ", "
                )

                summary_text <- sprintf(
                    "<b> Waffle Chart Summary:</b><br><br>
                    The sample contains %g %s distributed as: %s.<br><br>
                    <b>Key Finding:</b> %s %s (%.1f%% of %s).<br><br>
                    <b> Report Template:</b><br>
                    <i>\"Distribution analysis revealed %s %s (%.1f%%, n=%g%s) in our sample of %g %s.\"</i>",
                    total_cases, unit_label, breakdown_list, dominant_category, largest_phrase,
                    max_proportion, unit_label,
                    dominant_category, frequent_phrase, max_proportion,
                    plotdata$count[max_prop_idx], each_suffix, total_cases, unit_label
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
                "a 10\u00d710 grid (100 squares) to represent percentages intuitively.<br><br>",
                
                "<b>Clinical Applications:</b><br>",
                "\u2022 <b>Disease Classification:</b> Show distribution of tumor grades, cancer stages, or pathological subtypes<br>",
                "\u2022 <b>Treatment Outcomes:</b> Display response rates (complete/partial/no response) across patient cohorts<br>",
                "\u2022 <b>Demographic Analysis:</b> Present patient characteristics, risk factors, or comorbidity patterns<br>",
                "\u2022 <b>Quality Metrics:</b> Visualize compliance rates, diagnostic accuracy, or safety outcomes<br><br>",
                
                "<b>Advantages and Trade-offs vs. Other Charts:</b><br>",
                "\u2022 <b>Intuitive Interpretation:</b> Each square = 1% (exact Hamilton largest-remainder apportionment to 100 squares per panel) makes proportions easy to count<br>",
                "\u2022 <b>Part-to-Whole Clarity:</b> Often easier to read than circular pie charts for estimating percentages<br>",
                "\u2022 <b>Category Count Trade-off:</b> Best suited to 2\u20135 categories; prefer a horizontal bar chart when there are many categories or when fine quantitative comparisons are required<br>",
                "\u2022 <b>Visual Impact:</b> Effective for presentations and publications<br><br>",
                
                "<b>Statistical Considerations:</b><br>",
                "\u2022 <b>Sample Size (rule of thumb):</b> Proportions are more stable at n\u226530; when n<100 each case spans multiple squares, so always report exact counts alongside the 100-square grid. When using the Counts option, sample-size notices assume frequency counts rather than normalized weights<br>",
                "\u2022 <b>Category Balance (convention):</b> Visual separation is clearest when no single category exceeds ~80%<br>",
                "\u2022 <b>Precision:</b> Each square represents 1% of its panel (rounded via largest-remainder apportionment so every panel totals exactly 100 squares)<br>",
                "\u2022 <b>Statistical Testing:</b> Chi-square or Fisher's exact tests (outside this chart) can formally evaluate proportion differences<br><br>",
                
                "<b>Interpretation Conventions (informal rules of thumb):</b><br>",
                "\u2022 <b>Dominant Patterns:</b> As a descriptive convention, a category >60% suggests predominance<br>",
                "\u2022 <b>Balanced Distributions:</b> For 3 or more categories (k\u22653), no category >40% indicates a relatively spread distribution (not applicable to binary variables, where the larger category is always \u226550%)<br>",
                "\u2022 <b>Rare Categories:</b> Three distinct floors apply \u2014 a category whose share within its panel rounds to zero squares under Hamilton largest-remainder apportionment (typically <0.5%, and occasionally up to ~0.9% when several other categories have larger remainders) is NOT DRAWN in that panel; a category with fewer than 5 cases has high sampling uncertainty; and a category under 5% is drawn but may still represent a clinically important subgroup worth reporting explicitly in text<br>",
                "\u2022 <b>Comparative Analysis:</b> Use faceting to compare across patient groups or time periods (each panel is scaled independently to 100 squares, with per-panel n shown on each strip)<br><br>",
                
                "<b>Best Practices for Clinical Research:</b><br>",
                "\u2022 Use clear, standardized category labels (e.g., 'Grade 1', 'Grade 2', 'Grade 3')<br>",
                "\u2022 Choose colorblind-friendly palettes for accessibility<br>",
                "\u2022 Include sample sizes and confidence intervals when reporting<br>",
                "\u2022 Consider combining rare categories (<5 cases) for statistical stability<br>",
                "\u2022 Always report exact percentages and counts alongside visual representation"
            )
            
            return(explanation)
        },
        
        
        .run = function() {
            private$.noticeList <- list()
            if (is.null(self$options$groups)) {
                todo <- paste0(
                    "<div style='background: rgba(138, 155, 172, 0.06); color: inherit; ",
                    "padding: 30px; border-radius: 8px; border: 1px solid rgba(138, 155, 172, 0.35); ",
                    "margin: 20px 0; '>",

                    "<h2 style='margin-top: 0; font-size: 24px; font-weight: bold; color: inherit;'>",
                    " Welcome to Waffle Charts</h2>",

                    "<p style='font-size: 15px; line-height: 1.6; margin: 15px 0; color: inherit;'>",
                    "Create professional waffle charts to visualize categorical distributions ",
                    "using colored squares in a grid format.</p>",

                    "<h3 style='font-size: 18px; margin-top: 20px; margin-bottom: 10px; color: inherit;'>",
                    " Getting Started:</h3>",
                    "<ol style='font-size: 14px; line-height: 1.8; margin-left: 20px; color: inherit;'>",
                    "<li><strong>Required:</strong> Select a <strong>Groups</strong> variable (categorical)</li>",
                    "<li><strong>Optional:</strong> Add <strong>Counts</strong> variable for weighted data</li>",
                    "<li><strong>Optional:</strong> Use <strong>Facet By</strong> to compare across subgroups</li>",
                    "</ol>",

                    "<h3 style='font-size: 18px; margin-top: 20px; margin-bottom: 10px; color: inherit;'>",
                    " Clinical Examples:</h3>",
                    "<ul style='font-size: 14px; line-height: 1.8; margin-left: 20px; color: inherit;'>",
                    "<li>Tumor grade distribution (G1/G2/G3)</li>",
                    "<li>Treatment response rates (Complete/Partial/None)</li>",
                    "<li>Risk category proportions (Low/Medium/High)</li>",
                    "</ul>",

                    "<p style='font-size: 14px; margin-top: 20px; padding: 10px; ",
                    "background: rgba(33, 144, 255, 0.11); color: inherit; border-left: 3px solid #0066cc; '>",
                    "<strong> Tip:</strong> Each square represents 1% of your sample, ",
                    "making percentages immediately clear.</p>",
                    "</div>"
                )
                self$results$todo$setContent(todo)
                if (!is.null(self$results$analysisSummary)) {
                    self$results$analysisSummary$setContent("")
                }
                if (!is.null(self$results$methodExplanation)) {
                    self$results$methodExplanation$setContent("")
                }
                private$.cached_plot <- NULL
                return()
            }

            private$.requireData(self$data)

            private$.resetMessages()

            # Validate inputs before processing. The rejection itself carries the message:
            # jamovi shows it as the analysis's error state, so it is not also written into
            # the todo panel (that showed the user the same message twice; check-module
            # 2026-11-27, notices guide section 13.6). The handler only drops what a failed
            # run must not leave behind, then re-raises the original condition.
            tryCatch({
                private$.validateInputs()
            }, error = function(e) {
                private$.cached_plot <- NULL
                self$results$todo$setContent("")
                stop(e)
            })

            # Performance optimization: prepare data and options with caching
            mydata <- private$.prepareData()
            # Side effect only: refreshes the option hash and clears the plot/palette
            # caches when an option changed. The returned value is unused, and binding
            # it to `options` shadowed base::options() for the rest of .run().
            invisible(private$.prepareOptions())

            private$.requireData(mydata)

            # Generate analysis summary and explanations if requested
            if (self$options$showSummaries || self$options$showExplanations) {
                groups_var <- self$options$groups
                facet_var <- self$options$facet
                counts_var <- self$options$counts
                
                plotdata <- private$.getAggregate(mydata, groups_var, facet_var, counts_var)
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
            
            todo <- glue::glue("<br> Waffle chart created successfully. Enable 'Analysis summary' or 'Explanations' in Output Options for detailed interpretations.<br><hr>")
            self$results$todo$setContent(todo)
        },

        .plot = function(image, ...) {
            if (is.null(self$options$groups))
                return(FALSE)

            # Performance optimization: use prepared data and check cache
            if (private$.canUseCache()) {
                if (!is.null(private$.cached_plot)) {
                    print(private$.cached_plot)
                    return(TRUE)
                }
            }

            # Reset messages first: .plot runs as a separate render pass from .run, so
            # re-validating without a reset would append the same data-quality notices a
            # second time (duplicated Messages / notices). Resetting keeps it idempotent.
            private$.resetMessages()

            # Validate inputs and prepare data using cached method
            private$.validateInputs()
            mydata <- private$.prepareData()

            private$.requireData(mydata)

            groups_var <- self$options$groups
            facet_var <- self$options$facet
            counts_var <- self$options$counts

            # Use consolidated data aggregation helper (memoized: .run() already
            # built this aggregate when the summary panel is on)
            plotdata <- private$.getAggregate(mydata, groups_var, facet_var, counts_var)

            # Calculate values for caption using helper method
            total_cases <- sum(plotdata$count)
            is_weighted <- !is.null(counts_var) && counts_var != ""
            caption_text <- private$.generateCaption(plotdata, total_cases, is_weighted, facet_var)

            # Compute exact integer 100-square Hamilton apportionment per panel (or
            # overall when unfaceted) so every panel draws exactly 100 squares and is
            # immune to waffle:::StatWaffle's IEEE 754 as.integer(values * 100)
            # truncation bug (which turned 29%, 57%, 58% into 28, 56, 57 squares).
            plot_df <- as.data.frame(plotdata)
            if (!is.null(facet_var) && facet_var != "" && facet_var %in% names(plot_df)) {
                plot_df$squares <- integer(nrow(plot_df))
                for (flv in unique(plot_df[[facet_var]])) {
                    idx <- which(plot_df[[facet_var]] == flv)
                    plot_df$squares[idx] <- private$.apportionSquares(plot_df$count[idx], 100L)
                }
            } else {
                plot_df$squares <- private$.apportionSquares(plot_df$count, 100L)
            }

            # Get group levels and map palette explicitly by level name so colors
            # remain consistent across panels even if a category rounds to 0 squares.
            group_levels <- if (is.factor(plot_df[[groups_var]])) {
                levels(plot_df[[groups_var]])
            } else {
                as.character(unique(plot_df[[groups_var]]))
            }
            n_groups <- length(group_levels)

            # Generate color palette using cached method
            sel_palette <- private$.generateColorPalette(n_groups)
            named_palette <- stats::setNames(sel_palette, group_levels)

            # Create base plot (values = exact integer squares summing to 100 per panel)
            p <- ggplot2::ggplot(
                plot_df,
                ggplot2::aes(
                    fill = !!rlang::sym(groups_var),
                    values = squares
                )
            ) +
                waffle::geom_waffle(
                    n_rows = self$options$rows,
                    size = 0.5,
                    color = "white",
                    flip = self$options$flip,
                    make_proportional = FALSE
                ) +
                ggplot2::scale_fill_manual(
                    values = named_palette,
                    name = if (self$options$legendtitle != '')
                        self$options$legendtitle
                    else
                        groups_var
                ) +
                ggplot2::coord_equal() +
                ggplot2::theme_minimal()

            # Add labels in specific order
            if (!is.null(facet_var)) {
                # make_proportional = TRUE normalises EACH panel to 100 squares
                # (verified against waffle 1.0.2), so a panel holding 10 cases is drawn
                # exactly as large as one holding 600. Without the n on the strip the
                # figure invites the reader to compare panel sizes that carry no
                # information. Put the per-panel n where it cannot be missed.
                panel_n <- tapply(plotdata$count, plotdata[[facet_var]], sum,
                                  na.rm = TRUE)
                panel_n <- panel_n[!is.na(panel_n)]
                panel_labels <- stats::setNames(
                    sprintf("%s (n=%g)", names(panel_n), as.numeric(panel_n)),
                    names(panel_n))

                p <- p +
                    ggplot2::labs(
                        tag = facet_var,  # Facet variable name
                        caption = caption_text
                    ) +
                    ggplot2::facet_wrap(
                        ggplot2::vars(!!rlang::sym(facet_var)),
                        nrow = 1,
                        strip.position = "bottom",
                        labeller = ggplot2::as_labeller(panel_labels)
                    )
            } else {
                p <- p + ggplot2::labs(caption = caption_text)
            }

            # "Chart Title" goes where the UI label and the roxygen say it goes: the top.
            # It used to be pasted onto the front of the caption, so it rendered at the
            # BOTTOM in bold caption styling, glued to the statistics line, while the
            # plot.title theme block below was dead code.
            if (!is.null(self$options$mytitle) && nzchar(self$options$mytitle))
                p <- p + ggplot2::labs(title = self$options$mytitle)

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
                    linewidth = 0.5
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
        },

        # Hardened syntax-pane option serializer: drops undeclared option names
        # (from saved .omv/.omt files) and list values, and escapes all character
        # scalars via encodeString() so no option value can break out of quotes.
        .sourcifyOption = function(option) {
            if (identical(option$name, "data"))
                return("data = data")
            if (!option$name %in% names(formals(jwaffle)))
                return("")
            value <- option$value
            if (is.list(value))
                return("")
            if (is.character(value)) {
                if (length(value) != 1L || is.na(value) || !nzchar(value) ||
                    identical(value, option$default))
                    return("")
                return(paste0(option$name, " = ", encodeString(value, quote = '"')))
            }
            super$.sourcifyOption(option)
        }
    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for jwaffle analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            groups <- self$options$groups

            if (is.null(groups) || !is.character(groups) ||
                length(groups) != 1L || is.na(groups) || !nzchar(groups))
                return('')

            # Route every option through the hardened private$.sourcifyOption()
            # so undeclared option names and list values are dropped and every
            # character scalar (variable names, free-text titles, or coerced
            # strings from a saved .omv/.omt) is properly escaped.
            args <- character(0)
            for (option in private$.options$options) {
                if (identical(option$name, 'data'))
                    next
                as <- private$.sourcifyOption(option)
                if (!identical(as, ''))
                    args <- c(args, as)
            }

            # Get package name dynamically
            pkg_name <- utils::packageName()
            if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

            # Build complete function call
            paste0(pkg_name, '::jwaffle(\n    data = data,\n    ',
                   paste(args, collapse = ',\n    '), ')')
        }
    ) # End of public list
)
