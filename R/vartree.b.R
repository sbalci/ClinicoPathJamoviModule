#' @title Variable Tree
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom DiagrammeRsvg export_svg
#' @importFrom labelled set_variable_labels var_label
#' @importFrom magrittr %>%
#' @importFrom janitor clean_names
#' @importFrom vtree vtree
#'
#' @return An \code{R6} class generator object for the \code{vartreeClass} backend; used internally by the jamovi analysis wrapper and not called directly.
# Enhanced implementation supporting current CRAN vtree version 5.6.5
# Consolidates functionality from legacy versions with modern vtree features

vartreeClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "vartreeClass",
    inherit = vartreeBase,
    private = list(

        # Notice collection helpers. A single Preformatted (plain-text) output item:
        # avoids BOTH the jmvcore::Notice serialization error from
        # self$results$insert(999, Notice) AND any HTML in notices (project convention:
        # notice content must be plain text). ====
        .noticeList = list(),

        .addNotice = function(type, title, content) {
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

            # Plain text only - notices avoid HTML by project convention; the Preformatted
            # output item renders this literally (no markup, no injection surface).
            blocks <- vapply(private$.noticeList, function(notice) {
                prefix <- switch(notice$type,
                    ERROR          = "ERROR: ",
                    STRONG_WARNING = "STRONG WARNING: ",
                    WARNING        = "WARNING: ",
                    INFO           = "INFO: ",
                    "")
                paste0(prefix, notice$title, "\n", notice$content)
            }, character(1))

            self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
        },

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

            # A lookup miss returns character(0), not NULL. Left as-is, the guards
            # downstream evaluate `TRUE && logical(0)` -> NA and `if (NA)` raises a
            # raw R error with no jamovi-level message, so collapse misses to NULL.
            orNull <- function(x) if (length(x) == 0) NULL else x
            
            # Match the user-specified variables
            myvars <- self$options$vars
            myvars <- names(all_labels)[match(myvars, all_labels)]
            
            # Handle optional variables
            percvar <- NULL
            if (!is.null(self$options$percvar)) {
                percvar <- orNull(names(all_labels)[all_labels == self$options$percvar])
            }
            
            summaryvar <- NULL
            if (!is.null(self$options$summaryvar)) {
                summaryvar <- orNull(names(all_labels)[all_labels == self$options$summaryvar])
            }
            
            prunebelow <- NULL
            if (!is.null(self$options$prunebelow)) {
                prunebelow <- orNull(names(all_labels)[all_labels == self$options$prunebelow])
            }
            
            follow <- NULL
            if (!is.null(self$options$follow)) {
                follow <- orNull(names(all_labels)[all_labels == self$options$follow])
            }
            
            return(list(
                "mydata" = mydata, 
                "myvars" = myvars, 
                "percvar" = percvar,
                "summaryvar" = summaryvar,
                "prunebelow" = prunebelow,
                "follow" = follow,
                "all_labels" = all_labels
            ))
        },

        .run = function() {
            private$.noticeList <- list()
            # NOTE: the previously commented-out `jmvcore::Notice$new(...) +
            # insert(999, notice)` blocks have been migrated to the
            # serialization-safe plain-text notices pattern via
            # private$.addNotice(type, title, content), rendered into the
            # `notices` Preformatted output item (see .addNotice/.renderNotices).
            # Initial check for variables
            if (is.null(self$options$vars) || length(self$options$vars) == 0) {
                todo <- paste0(
                    "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-radius: 5px; color: inherit;'>",
                    "<h3 style='color: inherit; margin-top: 0;'>", .("Welcome to Variable Tree Analysis"), "</h3>",
                    "<p>", .("This tool creates hierarchical visualizations of categorical variables to identify patient subgroups and clinical patterns."), "</p>",
                    
                    "<h4 style='color: inherit;'>", .("Quick Start Guide:"), "</h4>",
                    "<ol>",
                    "<li><b>", .("Select Variables:"), "</b> ", .("Choose 2-4 categorical variables (diagnosis, treatment, stage, etc.)"), "</li>",
                    "<li><b>", .("Optional Summaries:"), "</b> ", .("Add a continuous variable for statistical summaries"), "</li>",
                    "<li><b>", .("Choose Style:"), "</b> ", .("Default (colorful), Clean (white nodes), or Minimal (one flat tone per level)"), "</li>",
                    "<li><b>", .("Enable Interpretation:"), "</b> ", .("Get an automatic plain-language description of what the tree shows"), "</li>",
                    "</ol>",
                    
                    "<h4 style='color: inherit;'>", .("Typical Uses:"), "</h4>",
                    "<ul>",
                    "<li>", .("How cases are distributed across combinations of risk factors"), "</li>",
                    "<li>", .("How often each treatment-and-response combination occurs"), "</li>",
                    "<li>", .("Which combinations of candidate prognostic factors are well represented, and which are near-empty"), "</li>",
                    "<li>", .("Checking data completeness and coding before modelling"), "</li>",
                    "</ul>",
                    
                    "<p><b> ", .("Tip:"), "</b> ", .("Start with 2-3 most important variables. You can always add more complexity later."), "</p>",
                    "</div>"
                )
                html <- self$results$todo
                html$setContent(todo)
                return()
            } else {
                todo <- ""
                html <- self$results$todo
                html$setContent(todo)
            }

            # Error Message - Check for empty dataset
            if (nrow(self$data) == 0) {
                private$.addNotice('ERROR', 'Empty Dataset', 'The dataset has no rows, so there is nothing to build a tree from. Check that a data file is loaded and that any row filters are not excluding every case.')
                return()
            }

            # Read and label data
            labeledData <- private$.labelData()
            mydata <- labeledData$mydata
            myvars <- labeledData$myvars
            percvar <- labeledData$percvar
            summaryvar <- labeledData$summaryvar
            prunebelow <- labeledData$prunebelow
            follow <- labeledData$follow
            all_labels <- labeledData$all_labels
            
            # Select only columns needed for analysis to avoid dropping rows
            columns_to_select <- c(myvars)
            if (!is.null(percvar)) columns_to_select <- c(columns_to_select, percvar)
            if (!is.null(summaryvar)) columns_to_select <- c(columns_to_select, summaryvar)
            # prunebelow / follow columns are deliberately NOT added here. vtree
            # honours those settings only for variables that are already in `vars`, so
            # a prune/follow variable outside the tree contributes nothing to the
            # figure - but keeping its column would let jmvcore::naOmit() below drop
            # rows on ITS missing values, silently changing N from a control that has
            # no visible effect.
            columns_to_select <- unique(columns_to_select)
            mydata <- jmvcore::select(df = mydata, columnNames = columns_to_select)

            # Input validation using cleaned data with Notices
            if (!is.null(myvars)) {
                error_count <- 0

                # Validate variables exist and are appropriate type
                for (var in myvars) {
                    if (!var %in% names(mydata)) {
                        error_count <- error_count + 1
                        private$.addNotice('ERROR', 'Variable Not Found', sprintf("Variable '%s' not found in dataset. Please verify variable selection.", var))
                    }
                    if (!is.factor(mydata[[var]]) && !is.character(mydata[[var]])) {
                        private$.addNotice('WARNING', 'Variable Not Categorical', sprintf("Variable '%s' is not categorical (factor/character). Tree visualization may not display properly.", var))
                    }
                }

                # Stop if any errors found
                if (error_count > 0) {
                    return()
                }

                # Validate percentage variable if specified
                if (!is.null(percvar) && !percvar %in% names(mydata)) {
                    private$.addNotice('ERROR', 'Percentage Variable Not Found', 'Percentage variable not found in dataset. Please verify your variable selection.')
                    return()
                } else if (!is.null(percvar)) {
                    # Ensure requested level exists
                    if (!is.null(self$options$percvarLevel) &&
                        !self$options$percvarLevel %in% levels(as.factor(mydata[[percvar]]))) {
                        available_levels <- paste(levels(as.factor(mydata[[percvar]])), collapse=", ")
                        private$.addNotice('ERROR', 'Invalid Percentage Level', sprintf("Selected percentage level '%s' not present in percentage variable. Available levels: %s",
                                                  self$options$percvarLevel, available_levels))
                        return()
                    }
                }

                # Validate summary variable if specified
                if (!is.null(summaryvar)) {
                    if (!summaryvar %in% names(mydata)) {
                        private$.addNotice('ERROR', 'Summary Variable Not Found', 'Summary variable not found in dataset. Please verify your variable selection.')
                        return()
                    }
                    if (!is.numeric(mydata[[summaryvar]])) {
                        private$.addNotice('WARNING', 'Summary Variable Not Numeric', 'Summary variable is not numeric. Statistical summaries (mean, SD) may not be meaningful. Consider selecting a continuous variable.')
                    }
                }

                # Validate pruning variables if specified. Checked against the full
                # labelled frame (all_labels), NOT the selected columns: a prune/follow
                # variable outside `vars` is deliberately not selected, so testing
                # names(mydata) here would reject every legitimate use of the control.
                if (!is.null(prunebelow) && !prunebelow %in% names(all_labels)) {
                    private$.addNotice('ERROR', 'Prune Below Variable Not Found', 'Prune below variable not found in dataset. Please verify your variable selection.')
                    return()
                }

                # Validate follow variables if specified
                if (!is.null(follow) && !follow %in% names(all_labels)) {
                    private$.addNotice('ERROR', 'Follow Variable Not Found', 'Follow variable not found in dataset. Please verify your variable selection.')
                    return()
                }
            }

            # Read Arguments
            horizontal <- self$options$horizontal
            sline <- self$options$sline
            mytitle <- self$options$mytitle
            # myvars, percvar, summaryvar already retrieved from labeledData

            # Style handling - Enhanced feature from vtree3
            style <- self$options$style
            if (is.null(style)) {
                style <- "default"
            }

            # Handle pruning options
            xprunesmaller <- NULL
            useprunesmaller <- self$options$useprunesmaller
            if (useprunesmaller) {
                # jmvcore::OptionInteger does NOT coerce to an R integer, and the
                # .u.yaml TextBox is `format: number`, so a decimal arrives here
                # unchanged and would reach sprintf('%d') further down - a raw R error
                # with no jamovi-level message. ceiling() is exactly behaviour
                # preserving for integer node counts: `count < 2.5` and `count < 3`
                # select the same nodes.
                xprunesmaller <- suppressWarnings(as.integer(ceiling(self$options$prunesmaller)))
                if (length(xprunesmaller) != 1L || is.na(xprunesmaller) || xprunesmaller < 2L) {
                    xprunesmaller <- NULL
                    private$.addNotice('WARNING', 'Small-Node Pruning Not Applied',
                        "The 'Prune counts <' threshold has to be a whole number of 2 or more before it can hide anything, because every displayed node holds at least one case. The current setting removed no branches.")
                }
            }

            # Default Arguments - Enhanced with modern vtree parameters
            # splitspaces removed - variable names preserved by direct passing
            xsplitspaces <- FALSE
            xprunebelow <- list()
            xfollow <- list()
            xlabelvar <- NULL
            xfillcolor <- NULL
            xfillnodes <- TRUE
            xNAfillcolor <- "white"
            xrootfillcolor <- "#EFF3FF"
            xpalette <- NULL
            xgradient <- TRUE
            xrevgradient <- FALSE
            xsinglecolor <- 2
            xcolorvarlabels <- TRUE
            xshowroot <- TRUE
            xplain <- FALSE

            # Style-specific settings - Enhanced styling system
            if (style == "clean") {
                xNAfillcolor <- "#FFFFFF"
                xrootfillcolor <- "#FFFFFF"
                xfillcolor <- "#FFFFFF"
                xcolorvarlabels <- FALSE
            } else if (style == "minimal") {
                # 'minimal' used to set exactly the same four values as 'clean' plus
                # sline (which already defaults to TRUE), so the two presets produced
                # byte-identical trees while the interpretation panel claimed the
                # layout had been simplified. It now selects vtree's own "plain"
                # layout: one flat tone per level instead of the gradient. NOTE the
                # spacing goes the OTHER way - plain = TRUE moves vtree's `squeeze`
                # from 1 to 0.6 (vtree.R: `if (missing(squeeze)) squeeze <- 0.6`),
                # which raises nodesep 0.1 -> 0.46 and margin 0.1 -> 0.18, so the
                # figure is LARGER, not tighter. Pass squeeze = 1 alongside plain if
                # that is ever unwanted. vtree overrides `fillcolor` itself when
                # plain = TRUE, so none is set here.
                xplain <- TRUE
                xNAfillcolor <- "#FFFFFF"
                xrootfillcolor <- "#FFFFFF"
                xcolorvarlabels <- FALSE
                sline <- TRUE
            }

            # Missing Value Handling with Notice system
            # Capture original counts BEFORE exclusion
            original_n <- nrow(mydata)

            excl <- self$options$excl
            excluded_n <- 0

            if (excl) {
                before_n <- nrow(mydata)
                mydata <- jmvcore::naOmit(mydata)
                excluded_n <- before_n - nrow(mydata)

                # STRONG_WARNING: Report case loss to user via Notice
                if (excluded_n > 0) {
                    excluded_pct <- round(100 * excluded_n / original_n, 1)

                    private$.addNotice('STRONG_WARNING', 'Case Exclusion', sprintf(
                        'CASE EXCLUSION: %d cases (%.1f%%) excluded due to missing values. Original N=%d, Final N=%d. Tree counts and percentages reflect complete cases only. Consider implications for generalizability.',
                        excluded_n, excluded_pct, original_n, nrow(mydata)
                    ))
                }
            }

            # Guard: abort cleanly if missing-value exclusion removed every row
            if (nrow(mydata) == 0) {
                private$.addNotice('ERROR', 'No Complete Cases',
                    'All cases were removed by missing-value exclusion. No complete observations remain for the selected variables. Disable missing-value exclusion (NA) or check your data.')
                return()
            }

            # Small sample warning
            if (nrow(mydata) < 50) {
                private$.addNotice('WARNING', 'Small Sample Size', sprintf(
                    'Only %d cases are available. Each additional variable splits those cases further, so the subgroups at the bottom of the tree will hold very few cases and their percentages move substantially with a single observation. N>=50 is a common rule of thumb for describing subgroups stably. Consider using fewer variables or collapsing levels so each node keeps a usable count.',
                    nrow(mydata)
                ))
            }

            # Two different quantities, deliberately kept apart:
            #   max_combinations      - Cartesian product of the levels, i.e. how many
            #                           cells the tree COULD have. A complexity measure.
            #   observed_combinations - how many of those cells actually hold cases.
            # Only the observed count may be reported to the user as a finding; the
            # product overstates it whenever any combination is unoccupied.
            var_level_counts <- sapply(mydata[, myvars, drop=FALSE], function(x) length(unique(x)))
            max_combinations <- prod(var_level_counts)

            # Counted over the rows (O(n)), not over an allocated contingency array
            # (O(prod(levels))), so a wide selection cannot blow up here.
            combo_counts <- table(do.call(paste, c(as.list(mydata[, myvars, drop = FALSE]), sep = "\r")))
            observed_combinations <- length(combo_counts)
            smallest_node_n <- if (observed_combinations > 0) as.integer(min(combo_counts)) else 0L

            if (max_combinations > 500) {
                var_summary <- paste(sprintf("%s (%d levels)", myvars, var_level_counts), collapse=", ")
                # %s, not %d: prod() returns a double and sprintf('%d', 2176782336)
                # is an error, which would have crashed this very warning.
                private$.addNotice('WARNING', 'Large Tree Complexity', sprintf(
                    'The selected variables allow up to %s subgroup combinations (%s); %d of them contain at least one case. A tree this wide is hard to read on screen and to print. Consider: (1) using fewer variables, (2) collapsing variable levels, or (3) using the pruning options to focus on the larger branches.',
                    base::format(max_combinations, big.mark = ",", scientific = FALSE),
                    var_summary, observed_combinations
                ))
            }

            # Create label mapping for vtree display
            # This will show original variable names in the tree display
            xlabelvar <- setNames(all_labels[myvars], myvars)
            
            # Prepare Variables - Use cleaned variable names for processing
            myvars1 <- myvars

            # Prune-below / follow-below are resolved HERE, ahead of the summary
            # specs, because the small-subgroup notices below have to describe the
            # nodes that actually survive into the figure.
            xprunebelow <- private$.buildConditionalOption(
                prunebelow,
                self$options$pruneLevel1,
                self$options$pruneLevel2
            )
            xfollow <- private$.buildConditionalOption(
                follow,
                self$options$followLevel1,
                self$options$followLevel2
            )

            # vtree honours prunebelow / follow only for variables that are in `vars`.
            # A setting on any other variable leaves the tree byte-identical, with no
            # error and no message of any kind, so say so here.
            prune_follow_vars <- unique(c(names(xprunebelow), names(xfollow)))
            for (nm in prune_follow_vars) {
                if (!nm %in% myvars1) {
                    nm_label <- all_labels[[nm]]
                    if (is.null(nm_label) || !nzchar(nm_label)) nm_label <- nm
                    private$.addNotice('WARNING', 'Prune / Follow Variable Not in the Tree', sprintf(
                        "'%s' is selected as a prune-below or follow-below variable, but it is not one of the tree variables, so it has no effect on the figure. Add it to Variables, or clear the setting.",
                        nm_label))
                }
            }

            # TRUE when a prune/follow setting really does truncate the tree. The set
            # of displayed nodes is then a subset this module does not model, so the
            # smallest-node notices below cannot describe the figure and are withheld.
            tree_truncated <- any(prune_follow_vars %in% myvars1)

            # Which nodes the minimum-size setting removes, and the smallest node that
            # SURVIVES into the figure. Computed here rather than after vtree() so the
            # notices below describe the rendered tree and not the raw data.
            pruned <- NULL
            smallest_shown_n <- smallest_node_n
            if (isTRUE(useprunesmaller) && !is.null(xprunesmaller)) {
                pruned <- private$.prunedByThreshold(mydata, myvars1, xprunesmaller,
                                                     self$options$vp)
                smallest_shown_n <- pruned$min_shown
            }
            # Gate for every "this node is tiny" notice: only when the count describes
            # a node that is actually drawn.
            report_small_nodes <- !tree_truncated && smallest_shown_n > 0

            # Summary specs accumulate as a character VECTOR (one element per
            # vtree summary), not a newline-joined single string.
            xsummary <- NULL

            # TRUE only once the reference-level spec has actually been added to
            # xsummary; the interpretation panel must not claim a percentage that was
            # never drawn.
            perc_spec_drawn <- FALSE

            # Handle Percentage Variable
            if (!is.null(percvar) && !is.null(self$options$percvarLevel) && self$options$pct) {
                # vtree 5.6/5.7 mis-collects summary headings - its loop reads
                # `headingslist <- c(headinglist, result$heading)` (plural assigned,
                # singular read), so when more than one summary spec is supplied
                # EVERY spec is labelled with the last spec's heading. A bare
                # "var=level" spec alongside a mean/SD spec therefore rendered the
                # reference-level count under the continuous variable's name
                # ("age: 18 (45%)"), which reads as a statement about age.
                # Writing the label into the spec BODY bypasses the heading path and
                # renders identically to the single-spec case.
                perc_var_label <- all_labels[[percvar]]
                if (is.null(perc_var_label) || !nzchar(perc_var_label)) perc_var_label <- percvar

                # vtree::parseSummary splits a summary spec at its FIRST whitespace
                # (regex "^(\\S+)\\s(.+)$") and reads the part before it as the
                # condition. A level containing a space therefore left the condition as
                # "var=FirstWord", vtree tested `var == "FirstWord"`, and every node
                # printed 0 (0%) - silently wrong, not an error. vtree's own convention
                # is to write such a level with underscores; parseSummary converts them
                # back to spaces whenever SOME level of that variable contains a space.
                # Characters outside vtree's variable-name class cannot be encoded at
                # all - they make parseSummary stop("Unknown variable in summary: er+")
                # - so those are refused with a message instead.
                # Residual ambiguity: a level containing a genuine underscore, in a
                # variable that also has a level containing a space, is converted back
                # to a space by vtree and cannot be expressed here.
                perc_level <- self$options$percvarLevel
                perc_level_code <- gsub(" ", "_", perc_level, fixed = TRUE)
                if (grepl("[^A-Za-z0-9~@#()_|,.]", perc_level_code)) {
                    private$.addNotice('WARNING', 'Reference-Level Percentage Not Shown', sprintf(
                        "The level '%s' contains characters that cannot be used in a tree summary; only letters, digits and the symbols ~ @ # ( ) _ | , . are accepted there. No reference-level percentage is drawn in the nodes. Recode that level to a name without '+', '-', '/' or '=' to display it.",
                        perc_level))
                } else {
                    xsummary <- c(xsummary, paste0(
                        percvar, "=", perc_level_code, " \\n\\n",
                        perc_var_label, "=", perc_level, ": %npct%"))
                    perc_spec_drawn <- TRUE
                }
            } else if (!is.null(percvar) && !is.null(self$options$percvarLevel)) {
                # percvar/percvarLevel alone do nothing: the spec is only built when
                # 'Percentages' is on. Say so rather than leaving the tree silently
                # unchanged (the interpretation panel used to claim it had worked).
                private$.addNotice('WARNING', 'Reference-Level Percentage Not Shown', sprintf(
                    "A reference variable ('%s') and level ('%s') are selected, but the 'Percentages' option is switched off, so no percentage for that level is drawn in any node. Tick 'Percentages' to display it.",
                    self$options$percvar, self$options$percvarLevel))
            }

            # Handle Summary Variable - Enhanced statistical summaries
            # CRITICAL FIX: Don't overwrite percvar summary, append instead
            if (!is.null(summaryvar)) {
                # vtree recognises %leafonly% and nothing else here - showing the
                # summary at every node is its DEFAULT, i.e. the empty location code.
                # A "%allnodes%" token is not interpreted and was printed verbatim
                # inside every node of the tree.
                summarylocation1 <- if (identical(self$options$summarylocation, "leafonly"))
                    "%leafonly%" else ""

                # Node text uses the original variable label, matching the variable
                # headers (which get their labels via labelvar) instead of showing
                # the janitor::clean_names() version.
                summary_label <- all_labels[[summaryvar]]
                if (is.null(summary_label) || !nzchar(summary_label)) summary_label <- summaryvar

                summ_spec <- paste0(
                    summaryvar, " \\n\\n",
                    summary_label, "\\n",
                    "mean=%mean%", "\\n",
                    "SD=%SD%", "\\n",
                    summarylocation1, "\\n"
                )

                # Append as a separate vector element so a percentage spec (if any)
                # and this mean/SD spec both render (vtree accepts a character
                # vector of summary specs).
                xsummary <- c(xsummary, summ_spec)

                if (report_small_nodes && smallest_shown_n < 3) {
                    private$.addNotice('WARNING', 'Mean and SD Shown for Very Small Subgroups', sprintf(
                        "The smallest subgroup displayed in this tree holds %d case(s), and mean/SD for '%s' are printed there in the same form as for the large nodes. SD is undefined for a single case (vtree prints SD=NA) and a mean over two cases carries almost no precision. Mean and SD also describe only the non-missing values of '%s' within each node, independently of the missing-value exclusion setting. Read the node count next to each mean before using it.",
                        smallest_shown_n, summary_label, summary_label))
                }
            }

            # A percentage on a handful of cases is the number a reader is most likely
            # to over-read, and it is this figure's headline output. Same gate as the
            # mean/SD notice above; 5 is the conventional cell-count floor.
            if (self$options$pct && report_small_nodes && smallest_shown_n < 5) {
                private$.addNotice('WARNING', 'Percentages Shown for Very Small Subgroups', sprintf(
                    'The smallest subgroup displayed in this tree holds %d case(s). A percentage based on that many cases moves by a large amount with a single observation, and it is printed in the same form as a percentage from a large node. Read the count beside each percentage.',
                    smallest_shown_n))
            }

            # Which denominator is in force is not written anywhere in the figure, and
            # it changes with the 'Valid percentages' option. Verified against vtree
            # 5.7.0: with vp = TRUE a node under a parent of 80 non-missing cases shows
            # 40 (50%), the NA node shows its count with no percentage; with vp = FALSE
            # the same node shows 40 (50%) out of the parent's raw count and the NA node
            # carries its own percentage. The %npct% reference-level figure uses a third
            # denominator - the node's non-missing count of the reference variable
            # (vtree builds its nodeargs without a vp entry, so nAndpct() uses its own
            # vp = TRUE default) - e.g. 22 (49%) = 22/45 inside a node of 50.
            if (self$options$pct) {
                denom_note <- if (isTRUE(self$options$vp))
                    "Node percentages use the count in the node directly above as the denominator (the root node at the first split). 'Valid percentages' is ON, so cases with a missing value on the splitting variable are left out of that denominator and appear in a separate NA node with no percentage."
                else
                    "Node percentages use the count in the node directly above as the denominator (the root node at the first split). 'Valid percentages' is OFF, so cases with a missing value on the splitting variable are counted in that denominator and the NA node carries its own percentage."
                if (perc_spec_drawn)
                    denom_note <- paste0(denom_note,
                        "\nThe reference-level percentage printed inside each node uses a different denominator: that node's count of cases with a non-missing value on the reference variable. 'mv=N' beside it reports how many cases were left out.")
                private$.addNotice('INFO', 'How the Percentages Are Calculated', denom_note)
            }

            # Run vtree function - Enhanced with modern vtree API and label support.
            # Wrapped in tryCatch so an incompatible variable/option combination (too many
            # factor levels, invalid prune/follow settings) surfaces a clean jamovi-level
            # message via jmvcore::reject() instead of an opaque raw R crash.
            private$.checkpoint()
            vtree_args <- list(
                z = mydata,
                vars = myvars1,
                sameline = sline,
                title = mytitle,
                horiz = horizontal,
                showvarnames = self$options$varnames,
                showlegend = self$options$legend,
                showpct = self$options$pct,
                splitspaces = xsplitspaces,
                prunebelow = xprunebelow,
                follow = xfollow,
                prunesmaller = xprunesmaller,
                fillcolor = xfillcolor,
                fillnodes = xfillnodes,
                NAfillcolor = xNAfillcolor,
                rootfillcolor = xrootfillcolor,
                palette = xpalette,
                gradient = xgradient,
                revgradient = xrevgradient,
                singlecolor = xsinglecolor,
                colorvarlabels = xcolorvarlabels,
                plain = xplain,
                seq = self$options$sequence,
                pattern = self$options$pattern,
                showroot = xshowroot,
                shownodelabels = self$options$nodelabel,
                showcount = self$options$showcount,
                vp = self$options$vp,
                summary = xsummary,
                labelvar = xlabelvar,  # Use original variable names for display
                pngknit = FALSE
            )

            # ptable is deliberately NOT part of the shared arguments: with
            # ptable = TRUE, vtree() returns a plain data.frame instead of a grViz
            # widget, and DiagrammeRsvg::export_svg() does not error on a data.frame
            # - it returns an empty string - so ticking 'Pattern table' used to
            # publish an empty <div> and silently blank the Variable Tree panel.
            # The tree is always built with ptable = FALSE; the table comes from a
            # second call below.
            results <- tryCatch(
                do.call(vtree::vtree, c(vtree_args, list(ptable = FALSE))),
                error = function(e) jmvcore::reject(paste0(
                    "The variable tree could not be generated: ", conditionMessage(e),
                    ". Try selecting fewer variables, or adjusting the pruning / follow options.")))

            # Pruning removes nodes from the tree without saying so, which leaves
            # branch counts not summing to their parent. Report exactly what went.
            # `pruned` was computed above, before the summary specs, so that the
            # small-subgroup notices could use the surviving-node count.
            if (!is.null(pruned) && pruned$nodes > 0) {
                shown <- pruned$labels
                if (length(shown) > 8)
                    shown <- c(shown[seq_len(8)],
                               sprintf("... and %d more", length(shown) - 8))
                private$.addNotice(
                    "WARNING",
                    "Branches hidden by the minimum-size setting",
                    paste0(
                        sprintf("%d node(s) holding %d case(s) are not shown, because 'Prune nodes smaller than' is set to %d.",
                                pruned$nodes, pruned$cases, xprunesmaller),
                        "\nCounts within a branch will therefore not add up to the count above it.",
                        "\nHidden: ", paste(shown, collapse = "; "),
                        if (isTRUE(self$options$vp) && pruned$min_shown > 0 &&
                            pruned$min_shown < xprunesmaller)
                            "\nMissing-value (NA) nodes are exempt from the threshold while 'Valid percentages' is on, so an NA node holding fewer cases than the threshold is still drawn."
                        else "",
                        "\nTurn the setting off to see every branch."))
            }

            # export as svg ----
            results1 <- tryCatch(
                DiagrammeRsvg::export_svg(gv = results),
                error = function(e) jmvcore::reject(paste0(
                    "The variable tree could not be rendered to SVG: ", conditionMessage(e), ".")))

            # Cap the rendered width. Rewriting only `width` left `height` and the
            # viewBox alone, so under preserveAspectRatio the drawing kept its
            # natural size and was merely centred in a wider box - the option did
            # nothing. Scale both dimensions, and only downward: the option is a
            # MAXIMUM, so a tree narrower than the cap is left at its own size.
            maxwidth <- suppressWarnings(as.integer(self$options$maxwidth))
            if (length(maxwidth) != 1L || is.na(maxwidth)) maxwidth <- 600L
            maxwidth <- max(100L, maxwidth)

            dims <- regmatches(results1,
                               regexpr('width="[0-9.]+pt" height="[0-9.]+pt"', results1))
            if (length(dims) == 1L) {
                nums <- suppressWarnings(as.numeric(
                    regmatches(dims, gregexpr('[0-9.]+', dims))[[1]]))
                if (length(nums) == 2L && all(is.finite(nums)) && nums[1] > maxwidth) {
                    results1 <- sub(dims,
                                    sprintf('width="%dpt" height="%dpt"',
                                            maxwidth,
                                            max(1L, as.integer(round(nums[2] * maxwidth / nums[1])))),
                                    results1, fixed = TRUE)
                }
            }

            results1 <- paste0('<div style="width: 100%; overflow-x: auto; white-space: nowrap;">',
                               results1,
                               '</div>')

            # Handle pattern table - second call, because ptable = TRUE returns a
            # data.frame rather than the tree widget (see the note on vtree_args).
            if (self$options$ptable) {
                private$.checkpoint()
                ptable_df <- tryCatch(
                    do.call(vtree::vtree, c(vtree_args, list(ptable = TRUE))),
                    error = function(e) e)

                if (inherits(ptable_df, "error")) {
                    self$results$text2$setContent("")
                    private$.addNotice('WARNING', 'Pattern Table Not Available', paste0(
                        "The pattern table could not be built for this combination of variables and options (",
                        conditionMessage(ptable_df),
                        "). The tree above is unaffected. Try fewer variables, or switch the pattern table off."))
                } else {
                    self$results$text2$setContent(
                        paste(capture.output(print(ptable_df)), collapse = "\n"))
                }
            }

            # Set main content conditionally
            if (self$options$showInterpretation) {
                interpretation <- private$.generateInterpretation(
                    small_nodes = report_small_nodes && smallest_shown_n < 3,
                    perc_spec_drawn = perc_spec_drawn)
                clinical_summary <- private$.generateClinicalSummary()
                about_section <- private$.generateAboutSection()
                glossary <- private$.generateTreeGlossary()  # Enhancement 4

                # Enhancement 3: Generate copy-ready report sentence
                report_sentence <- private$.generateReportSentence(
                    n_vars = length(myvars),
                    observed_combinations = observed_combinations,
                    final_n = nrow(mydata),
                    original_n = original_n,
                    excluded_n = excluded_n,
                    pruned = pruned,
                    prune_threshold = xprunesmaller
                )
                self$results$reportSentence$setContent(report_sentence)

                enhanced_content <- paste0(
                    about_section,
                    "<br><br>", clinical_summary,
                    "<br><br>", interpretation,
                    "<br><br>", glossary  # Enhancement 4: Add glossary
                )
                self$results$interpretation$setContent(enhanced_content)
                self$results$text1$setContent(results1)
            } else {
                self$results$text1$setContent(results1)
            }

            # Analysis completion notice (INFO). Reports the OBSERVED number of
            # combinations, not the Cartesian product of the levels.
            private$.addNotice('INFO', 'Analysis Complete', sprintf(
                'Tree built from %d categorical variable(s) across N=%d observations. %d combination(s) of those variables occur in the data%s.',
                length(myvars), nrow(mydata), observed_combinations,
                if (!is.null(pruned) && pruned$nodes > 0)
                    sprintf("; %d branch(es) are hidden by the minimum-size setting", pruned$nodes)
                else ""
            ))

            # Enhancement 2: Pattern/sequence mode explanations
            if (self$options$pattern) {
                private$.addNotice('INFO', 'Pattern Mode', 'PATTERN MODE: Tree groups cases by unique variable combinations (patterns) regardless of order. Each branch represents a distinct pattern. Use this mode to identify common patient profiles or covariate combinations. Refer to pattern table for detailed counts.')
            }

            if (self$options$sequence) {
                private$.addNotice('INFO', 'Sequence Mode', 'SEQUENCE MODE: Tree preserves variable order to show progression patterns. Same combinations in different orders create separate branches. Use this mode for temporal sequences (diagnosis to treatment to outcome) or ordered clinical pathways. Particularly useful for longitudinal or staged data.')
            }
        },

        # Enhanced interpretation generation
        # small_nodes / perc_spec_drawn are passed in from .run(): both bullets below
        # state something about the RENDERED tree, and neither condition can be
        # recovered from self$options alone.
        .generateInterpretation = function(small_nodes = FALSE, perc_spec_drawn = FALSE) {
            interp_parts <- c(
                paste0("<b>", .("Variable Tree Interpretation:"), "</b><br>"),
                paste0("\u{2022} ", .("The tree displays hierarchical relationships between categorical variables"), "<br>"),
                paste0("\u{2022} ", .("Each node shows counts and percentages for variable combinations"), "<br>")
            )

            if (self$options$pct) {
                interp_parts <- c(interp_parts, paste0("\u{2022} ", .("Percentages are calculated relative to the node directly above (the root node at the first split)"), "<br>"))
            }

            if (!is.null(self$options$summaryvar)) {
                summary_bullet <- if (isTRUE(small_nodes))
                    .("Statistical summaries (mean, SD) are shown for the continuous variable, including in nodes that hold only one or two cases")
                else
                    .("Statistical summaries (mean, SD) are shown for the continuous variable")
                interp_parts <- c(interp_parts, paste0("\u{2022} ", summary_bullet, "<br>"))
            }

            # Only claim the reference-level percentage when it is actually wired.
            # .run() builds the spec under `percvar && percvarLevel && pct` AND only
            # when the level can be encoded for vtree's summary parser; pct defaults to
            # FALSE, so a panel reading the options alone asserted a percentage that
            # appears nowhere in the tree. perc_spec_drawn is set at the point the spec
            # is appended, so it is the only condition that cannot drift out of step.
            if (isTRUE(perc_spec_drawn)) {
                interp_parts <- c(interp_parts, paste0("\u{2022} ", .("Percentage calculated for '"), htmltools::htmlEscape(self$options$percvarLevel),
                                                       .("' level of '"), htmltools::htmlEscape(self$options$percvar), .("'"), "<br>"))
            }

            # Style-specific notes
            style_note <- switch(self$options$style,
                "clean" = paste0("\u{2022} ", .("Clean style applied: minimal colors, focus on data structure"), "<br>"),
                "minimal" = paste0("\u{2022} ", .("Minimal style applied: one flat tone per level instead of a gradient, no label colouring, and more space between nodes"), "<br>"),
                NULL
            )

            if (!is.null(style_note)) {
                interp_parts <- c(interp_parts, style_note)
            }

            interp_parts <- c(interp_parts, paste0("\u{2022} ", .("Tree structure helps identify patterns and relationships in categorical data"), "<br>"))

            return(paste(interp_parts, collapse = ""))
        },
        
        # Helper to build conditional options for vtree pruning/following.
        # Returns a named list of the form list(<varname> = c("Level1", "Level2")),
        # which is exactly what vtree::vtree() expects for `prunebelow` / `follow`.
        # Previously this assembled a string like "list(varname=c('A','B'))" and
        # round-tripped it through a runtime evaluator; that made attacker-supplied
        # factor labels (level1/level2) a path to arbitrary R code execution. Build
        # the list as a native R object instead.
        # Which nodes does vtree's `prunesmaller` remove, and how many cases go
        # with them?
        #
        # vtree drops any node whose count is below the threshold, and a dropped
        # node takes its descendants with it. A node at depth d is a combination
        # of the first d variables, so walking depths 1..k and skipping anything
        # already inside a pruned ancestor reproduces the rule exactly.
        #
        # This is needed because pruning is otherwise invisible: with a threshold
        # of 5 a tree showed root 60 and branches 40 + 17, the 3 remaining cases
        # having disappeared with no indication. vtree does emit a message() of its
        # own, and jamovi does surface message()/warning() - but in the generic
        # "Analysis Notes" panel, detached from this analysis and without naming the
        # branches that went. The notice below names them.
        # `vp` mirrors the option of the same name, because vtree EXEMPTS NA nodes
        # from prunesmaller when it is on:
        #     selectcount <- categoryCounts >= prunesmaller | names(categoryCounts) == "NA"
        # (vtree 5.7.0, buildCanopy). Verified on A(big=80, mid=17, rare=3, NA=2) x B
        # with prunesmaller = 10: with vp = TRUE the NA node (n=2) is still drawn and
        # only its two children go, so the hidden set is rare, mid>x, NA>x, NA>y - four
        # nodes, not the three this walk used to report while naming the NA node itself
        # as hidden.
        #
        # `min_shown` is the smallest node that survives; the small-subgroup notices in
        # .run() need a count that describes the RENDERED tree, not the raw data.
        .prunedByThreshold = function(data, vars, threshold, vp = TRUE) {
            empty <- list(nodes = 0L, cases = 0L, labels = character(0), min_shown = 0L)
            if (is.null(threshold) || !is.finite(threshold) || threshold <= 0)
                return(empty)
            vars <- vars[vars %in% names(data)]
            if (length(vars) == 0) return(empty)

            # table() below allocates prod(levels of the first d variables) cells,
            # which is unbounded for a wide selection (12 variables x 6 levels is
            # 2.2e9 cells). The tree itself is already unusable long before that -
            # the "Large Tree Complexity" warning fires above 500 - so give up on
            # the pruning report rather than trying to allocate it.
            level_counts <- vapply(data[, vars, drop = FALSE],
                                   function(x) length(unique(x)), integer(1))
            if (prod(level_counts) > 1e6) return(empty)

            pruned_prefixes <- list()   # ancestors already removed
            n_nodes <- 0L
            n_cases <- 0L
            labels <- character(0)
            min_shown <- Inf

            for (d in seq_along(vars)) {
                tab <- table(data[, vars[seq_len(d)], drop = FALSE], useNA = "ifany")
                idx <- which(tab > 0, arr.ind = TRUE)
                if (length(idx) == 0) next
                if (is.null(dim(idx))) idx <- matrix(idx, ncol = 1)

                dn <- dimnames(tab)
                for (r in seq_len(nrow(idx))) {
                    combo <- vapply(seq_len(d), function(j) dn[[j]][idx[r, j]], character(1))
                    key <- paste(combo, collapse = "\r")
                    count <- tab[matrix(idx[r, ], nrow = 1)]

                    # inside an already-pruned ancestor? then it is not counted again
                    inside <- any(vapply(pruned_prefixes, function(pp)
                        length(pp) <= d && identical(pp, combo[seq_along(pp)]), logical(1)))
                    if (inside) next

                    # An NA node at this depth is exempt when valid percentages are on
                    # (vtree labels a missing category "NA"). It stays in the figure and
                    # its children are still judged on their own counts, so do NOT add
                    # it to pruned_prefixes.
                    is_na_node <- is.na(combo[d]) || identical(combo[d], "NA")
                    if (isTRUE(vp) && is_na_node) {
                        min_shown <- min(min_shown, as.integer(count))
                        next
                    }

                    if (count < threshold) {
                        pruned_prefixes[[length(pruned_prefixes) + 1]] <- combo
                        n_nodes <- n_nodes + 1L
                        n_cases <- n_cases + as.integer(count)
                        labels <- c(labels, sprintf("%s (n=%d)",
                                                    paste(combo, collapse = " > "), count))
                    } else {
                        min_shown <- min(min_shown, as.integer(count))
                    }
                }
            }
            list(nodes = n_nodes, cases = n_cases, labels = labels,
                 min_shown = if (is.finite(min_shown)) as.integer(min_shown) else 0L)
        },

        .buildConditionalOption = function(variable, level1, level2) {
            if (is.null(variable)) {
                return(NULL)
            }

            levels_to_use <- character()
            if (!is.null(level1) && nchar(as.character(level1)) > 0) {
                levels_to_use <- c(levels_to_use, as.character(level1))
            }
            if (!is.null(level2) && nchar(as.character(level2)) > 0) {
                levels_to_use <- c(levels_to_use, as.character(level2))
            }

            if (length(levels_to_use) == 0) {
                return(NULL)
            }

            stats::setNames(list(levels_to_use), as.character(variable))
        },
        
        # NOTE: a .getRSyntax() override used to live here. jmvcore::Analysis has no
        # method of that name (the syntax pane goes through the public asSource(),
        # which calls the private .sourcifyOption()), so it was never dispatched -
        # and had anything called it, `super$.getRSyntax` would have been NULL and
        # raised "attempt to apply non-function". Removed rather than left as a
        # false assurance that non-syntactic variable names are handled specially;
        # jmvcore's own sourcify path quotes them.

        # Generate clinical summary with practical guidance
        .generateClinicalSummary = function() {
            summary_parts <- c(
                paste0("<div style='background-color: rgba(138, 155, 172, 0.06); padding: 10px; border-left: 4px solid #007bff; margin: 10px 0; color: inherit;'>"),
                paste0("<b>", .("Clinical Summary:"), "</b><br>"),
                paste0("\u{2022} ", .("Variable trees help identify patient subgroups and treatment patterns"), "<br>"),
                paste0("\u{2022} ", .("Each branch represents a unique combination of patient characteristics"), "<br>"),
                paste0("\u{2022} ", .("Patient counts (n) are shown as text within each node, not encoded by node size"), "<br>")
            )
            
            # Add specific guidance based on analysis setup
            if (!is.null(self$options$summaryvar)) {
                summary_parts <- c(summary_parts,
                    paste0("\u{2022} ", .("Statistical summaries show mean and standard deviation for continuous measures"), "<br>"))
            }

            # CRITICAL FIX: Removed false percvar claim
            # Accurate percvar message now in .generateInterpretation() only when actually working

            summary_parts <- c(summary_parts, "</div>")
            return(paste(summary_parts, collapse = ""))
        },
        
        # Generate About This Analysis section
        .generateAboutSection = function() {
            about_parts <- c(
                paste0("<div style='background-color: rgba(33, 149, 188, 0.1); padding: 10px; border-left: 4px solid #28a745; margin: 10px 0; color: inherit;'>"),
                paste0("<b>", .("About This Analysis:"), "</b><br>"),
                paste0("\u{2022} <b>", .("Purpose:"), "</b> ", .("Explore relationships between categorical variables in clinical data"), "<br>"),
                paste0("\u{2022} <b>", .("Best for:"), "</b> ", .("Showing how many cases fall into each combination of categorical variables"), "<br>"),
                paste0("\u{2022} <b>", .("What it reports:"), "</b> ", .("Counts and percentages per subgroup, and optionally the mean and SD of one continuous variable within each subgroup. It fits no model and performs no test."), "<br>"),
                paste0("\u{2022} <b>", .("Data Requirements:"), "</b> ", .("Categorical variables (diagnosis, treatment, stage, etc.)"), "<br>")
            )

            about_parts <- c(about_parts, "</div>")
            return(paste(about_parts, collapse = ""))
        },

        # Enhancement 3: Generate copy-ready report sentence
        .generateReportSentence = function(n_vars, observed_combinations, final_n, original_n, excluded_n, pruned = NULL, prune_threshold = NULL) {
            # Format variable list with original labels
            if (n_vars == 1) {
                var_text <- sprintf("'%s'", self$options$vars[1])
            } else if (n_vars == 2) {
                var_text <- sprintf("'%s' and '%s'", self$options$vars[1], self$options$vars[2])
            } else {
                var_list <- paste(sprintf("'%s'", self$options$vars[1:(n_vars-1)]), collapse=", ")
                var_text <- sprintf("%s, and '%s'", var_list, self$options$vars[n_vars])
            }

            # Base sentence. The count reported here is the number of combinations
            # OBSERVED in the data, not the Cartesian product of the variable levels
            # (which counts empty cells and is therefore not a finding).
            sentence <- sprintf(
                "Variable tree analysis examined %d categorical variable%s (%s) across N=%d observations, in which %d distinct subgroup combination%s occurred.",
                n_vars,
                ifelse(n_vars > 1, "s", ""),
                var_text,
                final_n,
                observed_combinations,
                ifelse(observed_combinations > 1, "s", "")
            )

            # Add exclusion note if applicable
            if (excluded_n > 0) {
                excluded_pct <- round(100 * excluded_n / original_n, 1)
                sentence <- paste0(
                    sentence,
                    sprintf(" Missing value exclusion removed %d cases (%.1f%%).", excluded_n, excluded_pct)
                )
            }

            # Pruning hides branches from the figure, so a sentence describing the
            # figure has to say so.
            if (!is.null(pruned) && pruned$nodes > 0) {
                # Not "all subgroups smaller than X were removed": with 'Valid
                # percentages' on, vtree exempts NA nodes from the size threshold, so
                # one can still be drawn below it. State what went, not a blanket rule.
                sentence <- paste0(
                    sentence,
                    sprintf(" %d subgroup%s holding fewer than %d cases %s not displayed (%d case%s in total).",
                            pruned$nodes, ifelse(pruned$nodes > 1, "s", ""),
                            prune_threshold,
                            ifelse(pruned$nodes > 1, "were", "was"),
                            pruned$cases, ifelse(pruned$cases > 1, "s", ""))
                )
            }

            # Add mode-specific notes
            if (self$options$pattern) {
                sentence <- paste0(sentence, " Pattern mode was used to group cases by unique variable combinations.")
            } else if (self$options$sequence) {
                sentence <- paste0(sentence, " Sequence mode preserved variable order for progression analysis.")
            }

            # Add summary variable note
            if (!is.null(self$options$summaryvar)) {
                summary_location <- ifelse(self$options$summarylocation == "leafonly", "leaf nodes only", "all nodes")
                sentence <- paste0(
                    sentence,
                    sprintf(" Statistical summaries (mean, SD) for '%s' were displayed at %s.",
                           self$options$summaryvar, summary_location)
                )
            }

            return(sentence)
        },

        # Enhancement 4: Generate tree terminology glossary
        .generateTreeGlossary = function() {
            glossary_parts <- c(
                paste0("<div style='background-color: rgba(255, 202, 33, 0.23); padding: 10px; border-left: 4px solid #ffc107; margin: 10px 0; color: inherit;'>"),
                paste0("<b>", .("Tree Terminology Guide:"), "</b><br>"),
                paste0("\u{2022} <b>", .("Root Node:"), "</b> ", .("Top of tree showing total sample size (N) and starting point for all branches"), "<br>"),
                paste0("\u{2022} <b>", .("Branch:"), "</b> ", .("Path from root to leaf representing a sequence of variable splits"), "<br>"),
                paste0("\u{2022} <b>", .("Leaf Node:"), "</b> ", .("Terminal node with no further splits, representing a final patient subgroup"), "<br>"),
                paste0("\u{2022} <b>", .("Internal Node:"), "</b> ", .("Non-terminal node that splits into further branches"), "<br>"),
                paste0("\u{2022} <b>", .("Node Count:"), "</b> ", .("Number (n) of observations in that subgroup"), "<br>"),
                paste0("\u{2022} <b>", .("Node Percentage:"), "</b> ", .("Percentage of the count in the node directly above; at the first split that node is the root, so the denominator is the whole sample"), "<br>"),
                paste0("\u{2022} <b>", .("mv=N:"), "</b> ", .("Number of cases with a missing value that were left out of the statistic printed beside it"), "<br>"),
                paste0("\u{2022} <b>", .("Pruning:"), "</b> ", .("Removing branches below certain conditions to simplify tree"), "<br>"),
                paste0("\u{2022} <b>", .("Pattern:"), "</b> ", .("Unique combination of variable values regardless of order"), "<br>"),
                paste0("\u{2022} <b>", .("Sequence:"), "</b> ", .("Ordered progression of variable values (order matters)"), "<br>"),
                "</div>"
            )
            return(paste(glossary_parts, collapse = ""))
        }
    )
)
