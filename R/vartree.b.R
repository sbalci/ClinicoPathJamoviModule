#' @title Variable Tree
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import DiagrammeR
#' @import DiagrammeRsvg
#' @import vtree
#' @import janitor
#' @import labelled
#' @importFrom magrittr %>%
#' @importFrom stats as.formula
#' @importFrom grDevices rgb
#'
#' @export vartreeClass
#'
# Enhanced implementation supporting current CRAN vtree version 5.6.5
# Consolidates functionality from legacy versions with modern vtree features

vartreeClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "vartreeClass",
    inherit = vartreeBase,
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
            
            # Match the user-specified variables
            myvars <- self$options$vars
            myvars <- names(all_labels)[match(myvars, all_labels)]
            
            # Handle optional variables
            percvar <- NULL
            if (!is.null(self$options$percvar)) {
                percvar <- names(all_labels)[all_labels == self$options$percvar]
            }
            
            summaryvar <- NULL
            if (!is.null(self$options$summaryvar)) {
                summaryvar <- names(all_labels)[all_labels == self$options$summaryvar]
            }
            
            prunebelow <- NULL
            if (!is.null(self$options$prunebelow)) {
                prunebelow <- names(all_labels)[all_labels == self$options$prunebelow]
            }
            
            follow <- NULL
            if (!is.null(self$options$follow)) {
                follow <- names(all_labels)[all_labels == self$options$follow]
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

            # Initial check for variables
            if (is.null(self$options$vars)) {
                todo <- paste0(
                    "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px;'>",
                    "<h3 style='color: #007bff; margin-top: 0;'>", .("Welcome to Variable Tree Analysis"), "</h3>",
                    "<p>", .("This tool creates hierarchical visualizations of categorical variables to identify patient subgroups and clinical patterns."), "</p>",
                    
                    "<h4 style='color: #28a745;'>", .("Quick Start Guide:"), "</h4>",
                    "<ol>",
                    "<li><b>", .("Select Variables:"), "</b> ", .("Choose 2-4 categorical variables (diagnosis, treatment, stage, etc.)"), "</li>",
                    "<li><b>", .("Optional Summaries:"), "</b> ", .("Add a continuous variable for statistical summaries"), "</li>",
                    "<li><b>", .("Choose Style:"), "</b> ", .("Default (colorful), Clean (minimal), or Minimal (text-only)"), "</li>",
                    "<li><b>", .("Enable Interpretation:"), "</b> ", .("Get automatic clinical guidance and explanations"), "</li>",
                    "</ol>",
                    
                    "<h4 style='color: #6f42c1;'>", .("Clinical Use Cases:"), "</h4>",
                    "<ul>",
                    "<li>", .("Patient stratification by risk factors"), "</li>",
                    "<li>", .("Treatment response patterns"), "</li>",
                    "<li>", .("Prognostic factor combinations"), "</li>",
                    "<li>", .("Quality improvement analysis"), "</li>",
                    "</ul>",
                    
                    "<p><b>💡 ", .("Tip:"), "</b> ", .("Start with 2-3 most important variables. You can always add more complexity later."), "</p>",
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
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'emptyDataset',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Dataset contains no complete rows. Please check your data and ensure at least one complete observation exists.')
                self$results$insert(1, notice)
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
            if (!is.null(prunebelow)) columns_to_select <- c(columns_to_select, prunebelow)
            if (!is.null(follow)) columns_to_select <- c(columns_to_select, follow)
            columns_to_select <- unique(columns_to_select)
            mydata <- jmvcore::select(df = mydata, columnNames = columns_to_select)

            # Input validation using cleaned data with Notices
            if (!is.null(myvars)) {
                error_count <- 0
                warning_count <- 0

                # Validate variables exist and are appropriate type
                for (var in myvars) {
                    if (!var %in% names(mydata)) {
                        error_count <- error_count + 1
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = paste0('varNotFound_', error_count),
                            type = jmvcore::NoticeType$ERROR
                        )
                        notice$setContent(sprintf("Variable '%s' not found in dataset. Please verify variable selection.", var))
                        self$results$insert(error_count, notice)
                    }
                    if (!is.factor(mydata[[var]]) && !is.character(mydata[[var]])) {
                        warning_count <- warning_count + 1
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = paste0('varNotCategorical_', warning_count),
                            type = jmvcore::NoticeType$WARNING
                        )
                        notice$setContent(sprintf("Variable '%s' is not categorical (factor/character). Tree visualization may not display properly.", var))
                        self$results$insert(100 + warning_count, notice)  # Mid-section
                    }
                }

                # Stop if any errors found
                if (error_count > 0) {
                    return()
                }

                # Validate percentage variable if specified
                if (!is.null(percvar) && !percvar %in% names(mydata)) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'percvarNotFound',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent('Percentage variable not found in dataset. Please verify your variable selection.')
                    self$results$insert(1, notice)
                    return()
                } else if (!is.null(percvar)) {
                    # Ensure requested level exists
                    if (!is.null(self$options$percvarLevel) &&
                        !self$options$percvarLevel %in% levels(as.factor(mydata[[percvar]]))) {
                        available_levels <- paste(levels(as.factor(mydata[[percvar]])), collapse=", ")
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'percvarLevelInvalid',
                            type = jmvcore::NoticeType$ERROR
                        )
                        notice$setContent(sprintf("Selected percentage level '%s' not present in percentage variable. Available levels: %s",
                                                  self$options$percvarLevel, available_levels))
                        self$results$insert(1, notice)
                        return()
                    }
                }

                # Validate summary variable if specified
                if (!is.null(summaryvar)) {
                    if (!summaryvar %in% names(mydata)) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'summaryvarNotFound',
                            type = jmvcore::NoticeType$ERROR
                        )
                        notice$setContent('Summary variable not found in dataset. Please verify your variable selection.')
                        self$results$insert(1, notice)
                        return()
                    }
                    if (!is.numeric(mydata[[summaryvar]])) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'summaryvarNotNumeric',
                            type = jmvcore::NoticeType$WARNING
                        )
                        notice$setContent('Summary variable is not numeric. Statistical summaries (mean, SD) may not be meaningful. Consider selecting a continuous variable.')
                        self$results$insert(101, notice)
                    }
                }

                # Validate pruning variables if specified
                if (!is.null(prunebelow) && !prunebelow %in% names(mydata)) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'prunebelowNotFound',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent('Prune below variable not found in dataset. Please verify your variable selection.')
                    self$results$insert(1, notice)
                    return()
                }

                # Validate follow variables if specified
                if (!is.null(follow) && !follow %in% names(mydata)) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'followNotFound',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent('Follow variable not found in dataset. Please verify your variable selection.')
                    self$results$insert(1, notice)
                    return()
                }
            }

            # Read Arguments
            horizontal <- self$options$horizontal
            sline <- self$options$sline
            mytitle <- self$options$mytitle
            # myvars, percvar, summaryvar already retrieved from labeledData

            # Initialize xsummary
            xsummary <- NULL  # Default initialization

            # Style handling - Enhanced feature from vtree3
            style <- self$options$style
            if (is.null(style)) {
                style <- "default"
            }

            # Handle pruning options
            xprunesmaller <- NULL
            useprunesmaller <- self$options$useprunesmaller
            if (useprunesmaller) {
                xprunesmaller <- self$options$prunesmaller
            }

            # Default Arguments - Enhanced with modern vtree parameters
            # splitspaces removed - variable names preserved by direct passing
            xsplitspaces <- FALSE
            xprune <- list()
            xprunebelow <- list()
            xkeep <- list()
            xfollow <- list()
            xlabelnode <- list()
            xtlabelnode <- NULL
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

            # Style-specific settings - Enhanced styling system
            if (style == "clean") {
                xNAfillcolor <- "#FFFFFF"
                xrootfillcolor <- "#FFFFFF"
                xfillcolor <- "#FFFFFF"
                xcolorvarlabels <- FALSE
            } else if (style == "minimal") {
                xNAfillcolor <- "#FFFFFF"
                xrootfillcolor <- "#FFFFFF"
                xfillcolor <- "#FFFFFF"
                xcolorvarlabels <- FALSE
                sline <- TRUE
            }

            # Missing Value Handling with Notice system
            # Capture original counts BEFORE exclusion
            original_n <- nrow(mydata)
            original_complete <- sum(complete.cases(mydata[, myvars, drop = FALSE]))

            excl <- self$options$excl
            excluded_n <- 0

            if (excl) {
                before_n <- nrow(mydata)
                mydata <- jmvcore::naOmit(mydata)
                excluded_n <- before_n - nrow(mydata)

                # STRONG_WARNING: Report case loss to user via Notice
                if (excluded_n > 0) {
                    excluded_pct <- round(100 * excluded_n / original_n, 1)

                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'missingValueExclusion',
                        type = jmvcore::NoticeType$STRONG_WARNING
                    )
                    notice$setContent(sprintf(
                        'CASE EXCLUSION: %d cases (%.1f%%) excluded due to missing values. Original N=%d, Final N=%d. Tree counts and percentages reflect complete cases only. Consider implications for generalizability.',
                        excluded_n, excluded_pct, original_n, nrow(mydata)
                    ))
                    self$results$insert(1, notice)  # Top priority
                }
            }

            # Small sample warning
            if (nrow(mydata) < 50) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'smallSample',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent(sprintf(
                    'Small sample size (N=%d). Tree patterns may not be reliable for clinical decisions. Recommend N≥50 for stable subgroup identification. Interpret with caution.',
                    nrow(mydata)
                ))
                self$results$insert(2, notice)  # After errors
            }

            # Enhancement 1: Large tree complexity warning
            # Cache variable level counts for later reuse (line 509 optimization)
            var_level_counts <- sapply(mydata[, myvars, drop=FALSE], function(x) length(unique(x)))
            total_combinations <- prod(var_level_counts)

            if (total_combinations > 500) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'largeTreeComplexity',
                    type = jmvcore::NoticeType$WARNING
                )
                var_summary <- paste(sprintf("%s (%d levels)", myvars, var_level_counts), collapse=", ")
                notice$setContent(sprintf(
                    'Large tree complexity detected: %d total subgroup combinations from variables: %s. Tree visualization may be difficult to interpret. Consider: (1) reducing variables, (2) collapsing variable levels, or (3) using pruning options to focus on key patterns.',
                    total_combinations, var_summary
                ))
                self$results$insert(3, notice)
            }

            # Create label mapping for vtree display
            # This will show original variable names in the tree display
            xlabelvar <- setNames(all_labels[myvars], myvars)
            
            # Prepare Variables - Use cleaned variable names for processing
            myvars1 <- myvars

            # CRITICAL FIX: Handle Summary Variables Correctly
            # Initialize xsummary as NULL
            xsummary <- NULL

            # Handle Percentage Variable - FIXED: Now properly passed to vtree
            if (!is.null(percvar) && !is.null(self$options$percvarLevel) && self$options$pct) {
                # Build percentage summary spec following vtree syntax
                # Format: "varname=level" tells vtree to show % for that level
                perc_spec <- paste0(percvar, "=", self$options$percvarLevel)

                # If we already have a summary, combine them; otherwise use perc spec
                if (!is.null(xsummary)) {
                    xsummary <- paste0(xsummary, "\n", perc_spec)
                } else {
                    xsummary <- perc_spec
                }
            }

            # Handle Summary Variable - Enhanced statistical summaries
            # CRITICAL FIX: Don't overwrite percvar summary, append instead
            if (!is.null(summaryvar)) {
                summarylocation <- self$options$summarylocation

                if (summarylocation == "leafonly") {
                    summarylocation1 <- "%leafonly%"
                } else if (summarylocation == "allnodes") {
                    summarylocation1 <- "%allnodes%"
                }

                summ_spec <- paste0(
                    summaryvar, " \\n\\n",
                    summaryvar, "\\n",
                    "mean=%mean%", "\\n",
                    "SD=%SD%", "\\n",
                    summarylocation1, "\\n"
                )

                # CRITICAL FIX: Combine with percvar summary if it exists
                if (!is.null(xsummary)) {
                    xsummary <- paste0(xsummary, "\n", summ_spec)
                } else {
                    xsummary <- summ_spec
                }
            }

            # Handle Prune Below - Enhanced pruning controls
            xprunebelow <- private$.buildConditionalOption(
                prunebelow, 
                self$options$pruneLevel1, 
                self$options$pruneLevel2
            )

            # Handle Follow Below - Enhanced follow controls
            xfollow <- private$.buildConditionalOption(
                follow, 
                self$options$followLevel1, 
                self$options$followLevel2
            )

            # Handle Interpretation - Enhanced interpretation feature
            if (self$options$showInterpretation) {
                mytitle <- paste0(mytitle, "\\n(", .("Interpretation will be shown below"), ")")
            }

            # Run vtree function - Enhanced with modern vtree API and label support
            results <- vtree::vtree(
                z = mydata,
                vars = myvars1,
                sameline = sline,
                title = mytitle,
                horiz = horizontal,
                showvarnames = self$options$varnames,
                showlegend = self$options$legend,
                showpct = self$options$pct,
                splitspaces = xsplitspaces,
                prunebelow = private$.safeEvalParse(xprunebelow),
                follow = private$.safeEvalParse(xfollow),
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
                seq = self$options$sequence,
                pattern = self$options$pattern,
                ptable = self$options$ptable,
                showroot = xshowroot,
                shownodelabels = self$options$nodelabel,
                showcount = self$options$showcount,
                vp = self$options$vp,
                summary = xsummary,
                labelvar = xlabelvar,  # Use original variable names for display
                pngknit = FALSE
            )

            # export as svg ----
            results1 <- DiagrammeRsvg::export_svg(gv = results)

            # Use maxwidth parameter or default values
            maxwidth <- self$options$maxwidth
            if (is.null(maxwidth)) {
                maxwidth <- ifelse(horizontal == TRUE, 400, 1000)
            }
            
            results1 <- base::sub('width=\"[[:digit:]pt\"]+',
                                  paste0('width=', maxwidth, 'pt '),
                                  results1)

            results1 <- paste0('<html><head><style>
                               #myDIV {width: 610px; height: 850px; overflow: auto;}
                               </style></head><body><div id="myDIV">',
                               results1,
                               '</div></body></html>')

            # Handle pattern table
            if (self$options$ptable) {
                self$results$text2$setContent(results)
            }

            # Set main content conditionally - Enhanced with clinical guidance
            if (self$options$showInterpretation) {
                interpretation <- private$.generateInterpretation(results)
                clinical_summary <- private$.generateClinicalSummary()
                about_section <- private$.generateAboutSection()
                glossary <- private$.generateTreeGlossary()  # Enhancement 4

                # Enhancement 3: Generate copy-ready report sentence
                report_sentence <- private$.generateReportSentence(
                    myvars = myvars,
                    n_vars = length(myvars),
                    total_combinations = total_combinations,
                    final_n = nrow(mydata),
                    original_n = original_n,
                    excluded_n = excluded_n
                )
                self$results$reportSentence$setContent(report_sentence)

                enhanced_content <- paste0(
                    results1,
                    "<br><br>", about_section,
                    "<br><br>", clinical_summary,
                    "<br><br>", interpretation,
                    "<br><br>", glossary  # Enhancement 4: Add glossary
                )
                self$results$text1$setContent(enhanced_content)
            } else {
                self$results$text1$setContent(results1)
            }

            # Analysis completion notice (INFO)
            success_notice <- jmvcore::Notice$new(
                options = self$options,
                name = 'analysisComplete',
                type = jmvcore::NoticeType$INFO
            )

            # Reuse cached total_combinations from Enhancement 1 (line 355-356)
            n_vars <- length(myvars)
            # total_combinations already calculated above

            success_notice$setContent(sprintf(
                'Analysis completed successfully. Tree displays %d categorical variables with %d total subgroup combinations across N=%d observations.',
                n_vars, total_combinations, nrow(mydata)
            ))
            self$results$insert(999, success_notice)  # Bottom

            # Enhancement 2: Pattern/sequence mode explanations
            if (self$options$pattern) {
                pattern_notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'patternModeExplanation',
                    type = jmvcore::NoticeType$INFO
                )
                pattern_notice$setContent('PATTERN MODE: Tree groups cases by unique variable combinations (patterns) regardless of order. Each branch represents a distinct pattern. Use this mode to identify common patient profiles or covariate combinations. Refer to pattern table for detailed counts.')
                self$results$insert(998, pattern_notice)  # Just above success notice
            }

            if (self$options$sequence) {
                sequence_notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'sequenceModeExplanation',
                    type = jmvcore::NoticeType$INFO
                )
                sequence_notice$setContent('SEQUENCE MODE: Tree preserves variable order to show progression patterns. Same combinations in different orders create separate branches. Use this mode for temporal sequences (diagnosis→treatment→outcome) or ordered clinical pathways. Particularly useful for longitudinal or staged data.')
                self$results$insert(998, sequence_notice)  # Just above success notice
            }
        },

        # Enhanced interpretation generation
        .generateInterpretation = function(results) {
            interp_parts <- c(
                paste0("<b>", .("Variable Tree Interpretation:"), "</b><br>"),
                paste0("• ", .("The tree displays hierarchical relationships between categorical variables"), "<br>"),
                paste0("• ", .("Each node shows counts and percentages for variable combinations"), "<br>")
            )

            if (self$options$pct) {
                interp_parts <- c(interp_parts, paste0("• ", .("Percentages are calculated relative to parent nodes"), "<br>"))
            }

            if (!is.null(self$options$summaryvar)) {
                interp_parts <- c(interp_parts, paste0("• ", .("Statistical summaries (mean, SD) are shown for the continuous variable"), "<br>"))
            }

            # CRITICAL FIX: Only claim percvar works if it's actually wired
            if (!is.null(self$options$percvar) && !is.null(self$options$percvarLevel)) {
                interp_parts <- c(interp_parts, paste0("• ", .("Percentage calculated for '"), self$options$percvarLevel,
                                                       .("' level of '"), self$options$percvar, .("'"), "<br>"))
            }

            # Style-specific notes
            style_note <- switch(self$options$style,
                "clean" = paste0("• ", .("Clean style applied: minimal colors, focus on data structure"), "<br>"),
                "minimal" = paste0("• ", .("Minimal style applied: simplified layout with same-line presentation"), "<br>"),
                NULL
            )

            if (!is.null(style_note)) {
                interp_parts <- c(interp_parts, style_note)
            }

            interp_parts <- c(interp_parts, paste0("• ", .("Tree structure helps identify patterns and relationships in categorical data"), "<br>"))

            return(paste(interp_parts, collapse = ""))
        },
        
        # CRITICAL FIX: Helper function to build conditional options for pruning/following
        # Fixed to avoid composeTerm() on level values and allow single-level pruning
        .buildConditionalOption = function(variable, level1, level2) {
            # If no variable selected, return NULL
            if (is.null(variable)) {
                return(NULL)
            }

            # CRITICAL FIX: Only use composeTerm() on the VARIABLE name
            # Do NOT use it on level values - they should be passed as-is
            variable_term <- jmvcore::composeTerm(variable)

            # Build level vector based on what's provided
            levels_to_use <- c()

            # CRITICAL FIX: Allow single-level pruning (level1 only)
            if (!is.null(level1) && nchar(as.character(level1)) > 0) {
                # Don't wrap level1 in composeTerm - use raw value
                levels_to_use <- c(levels_to_use, level1)
            }

            if (!is.null(level2) && nchar(as.character(level2)) > 0) {
                # Don't wrap level2 in composeTerm - use raw value
                levels_to_use <- c(levels_to_use, level2)
            }

            # If no levels specified, return NULL
            if (length(levels_to_use) == 0) {
                return(NULL)
            }

            # Build the list spec correctly:
            # list(varname = c("Level1", "Level2"))
            # NOT list(`varname` = c('`Level1`', '`Level2`'))
            levels_quoted <- paste0("'", levels_to_use, "'", collapse = ",")
            return(paste0("list(", variable_term, "=c(", levels_quoted, "))"))
        },
        
        # Custom R syntax generation to handle variable names with spaces
        .getRSyntax = function() {
            # Get the default syntax
            syntax <- super$.getRSyntax()
            
            # Fix variable names with spaces by backticking them
            if (!is.null(self$options$vars) && length(self$options$vars) > 0) {
                # Process each variable name to add backticks if they contain spaces
                vars_fixed <- sapply(self$options$vars, function(var) {
                    if (grepl(" ", var)) {
                        return(paste0("`", var, "`"))
                    } else {
                        return(var)
                    }
                })
                
                # Create properly formatted variable list
                vars_syntax <- paste0("c(", paste(paste0('"', vars_fixed, '"'), collapse = ", "), ")")
                
                # Replace the problematic vars() call with proper c() syntax
                syntax <- gsub("vars\\s*=\\s*vars\\([^)]+\\)", paste0("vars = ", vars_syntax), syntax)
            }
            
            return(syntax)
        },
        
        # Safe evaluation of parsed text with error handling
        .safeEvalParse = function(text_expression) {
            if (is.null(text_expression)) {
                return(NULL)
            }
            
            tryCatch({
                eval(parse(text = text_expression))
            }, error = function(e) {
                warning(paste(.("Error in conditional expression:"), e$message, 
                             .("Using NULL value instead.")))
                return(NULL)
            })
        },
        
        # Generate clinical summary with practical guidance
        .generateClinicalSummary = function() {
            summary_parts <- c(
                paste0("<div style='background-color: #f8f9fa; padding: 10px; border-left: 4px solid #007bff; margin: 10px 0;'>"),
                paste0("<b>", .("Clinical Summary:"), "</b><br>"),
                paste0("• ", .("Variable trees help identify patient subgroups and treatment patterns"), "<br>"),
                paste0("• ", .("Each branch represents a unique combination of patient characteristics"), "<br>"),
                paste0("• ", .("Node sizes indicate patient counts - larger nodes represent more common patterns"), "<br>")
            )
            
            # Add specific guidance based on analysis setup
            if (!is.null(self$options$summaryvar)) {
                summary_parts <- c(summary_parts,
                    paste0("• ", .("Statistical summaries show mean and standard deviation for continuous measures"), "<br>"))
            }

            # CRITICAL FIX: Removed false percvar claim
            # Accurate percvar message now in .generateInterpretation() only when actually working

            summary_parts <- c(summary_parts, "</div>")
            return(paste(summary_parts, collapse = ""))
        },
        
        # Generate About This Analysis section
        .generateAboutSection = function() {
            about_parts <- c(
                paste0("<div style='background-color: #e8f4f8; padding: 10px; border-left: 4px solid #28a745; margin: 10px 0;'>"),
                paste0("<b>", .("About This Analysis:"), "</b><br>"),
                paste0("• <b>", .("Purpose:"), "</b> ", .("Explore relationships between categorical variables in clinical data"), "<br>"),
                paste0("• <b>", .("Best for:"), "</b> ", .("Identifying patient subgroups, treatment patterns, and outcome associations"), "<br>"),
                paste0("• <b>", .("Clinical Applications:"), "</b> ", .("Risk stratification, treatment selection, prognostic factor analysis"), "<br>"),
                paste0("• <b>", .("Data Requirements:"), "</b> ", .("Categorical variables (diagnosis, treatment, stage, etc.)"), "<br>")
            )

            about_parts <- c(about_parts, "</div>")
            return(paste(about_parts, collapse = ""))
        },

        # Enhancement 3: Generate copy-ready report sentence
        .generateReportSentence = function(myvars, n_vars, total_combinations, final_n, original_n, excluded_n) {
            # Format variable list with original labels
            if (n_vars == 1) {
                var_text <- sprintf("'%s'", self$options$vars[1])
            } else if (n_vars == 2) {
                var_text <- sprintf("'%s' and '%s'", self$options$vars[1], self$options$vars[2])
            } else {
                var_list <- paste(sprintf("'%s'", self$options$vars[1:(n_vars-1)]), collapse=", ")
                var_text <- sprintf("%s, and '%s'", var_list, self$options$vars[n_vars])
            }

            # Base sentence
            sentence <- sprintf(
                "Variable tree analysis examined %d categorical variable%s (%s) across N=%d observations, identifying %d unique subgroup combinations.",
                n_vars,
                ifelse(n_vars > 1, "s", ""),
                var_text,
                final_n,
                total_combinations
            )

            # Add exclusion note if applicable
            if (excluded_n > 0) {
                excluded_pct <- round(100 * excluded_n / original_n, 1)
                sentence <- paste0(
                    sentence,
                    sprintf(" Missing value exclusion removed %d cases (%.1f%%).", excluded_n, excluded_pct)
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
                paste0("<div style='background-color: #fff3cd; padding: 10px; border-left: 4px solid #ffc107; margin: 10px 0;'>"),
                paste0("<b>", .("Tree Terminology Guide:"), "</b><br>"),
                paste0("• <b>", .("Root Node:"), "</b> ", .("Top of tree showing total sample size (N) and starting point for all branches"), "<br>"),
                paste0("• <b>", .("Branch:"), "</b> ", .("Path from root to leaf representing a sequence of variable splits"), "<br>"),
                paste0("• <b>", .("Leaf Node:"), "</b> ", .("Terminal node with no further splits, representing a final patient subgroup"), "<br>"),
                paste0("• <b>", .("Internal Node:"), "</b> ", .("Non-terminal node that splits into further branches"), "<br>"),
                paste0("• <b>", .("Node Count:"), "</b> ", .("Number (n) of observations in that subgroup"), "<br>"),
                paste0("• <b>", .("Node Percentage:"), "</b> ", .("Proportion of cases relative to parent node or total sample"), "<br>"),
                paste0("• <b>", .("Pruning:"), "</b> ", .("Removing branches below certain conditions to simplify tree"), "<br>"),
                paste0("• <b>", .("Pattern:"), "</b> ", .("Unique combination of variable values regardless of order"), "<br>"),
                paste0("• <b>", .("Sequence:"), "</b> ", .("Ordered progression of variable values (order matters)"), "<br>"),
                "</div>"
            )
            return(paste(glossary_parts, collapse = ""))
        }
    )
)
