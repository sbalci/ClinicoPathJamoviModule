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

            # Error Message
            if (nrow(self$data) == 0)
                stop(.("Dataset contains no complete rows. Please check your data and variable selections."))

            # Read and label data
            labeledData <- private$.labelData()
            mydata <- labeledData$mydata
            myvars <- labeledData$myvars
            percvar <- labeledData$percvar
            summaryvar <- labeledData$summaryvar
            prunebelow <- labeledData$prunebelow
            follow <- labeledData$follow
            all_labels <- labeledData$all_labels

            # Input validation using cleaned data
            if (!is.null(myvars)) {
                # Validate variables exist and are appropriate type
                for (var in myvars) {
                    if (!var %in% names(mydata)) {
                        stop(paste(.("Variable"), var, .("not found in dataset. Please check your variable selection.")))
                    }
                    if (!is.factor(mydata[[var]]) && !is.character(mydata[[var]])) {
                        warning(paste(.("Variable"), var, .("is not categorical and may not display properly in the tree visualization.")))
                    }
                }
                
                # Validate percentage variable if specified
                if (!is.null(percvar) && !percvar %in% names(mydata)) {
                    stop(.("Percentage variable not found in dataset."))
                }
                
                # Validate summary variable if specified
                if (!is.null(summaryvar)) {
                    if (!summaryvar %in% names(mydata)) {
                        stop(.("Summary variable not found in dataset."))
                    }
                    if (!is.numeric(mydata[[summaryvar]])) {
                        warning(.("Summary variable is not numeric. Statistical summaries may not be meaningful."))
                    }
                }
                
                # Validate pruning variables if specified
                if (!is.null(prunebelow) && !prunebelow %in% names(mydata)) {
                    stop(.("Prune below variable not found in dataset."))
                }
                
                # Validate follow variables if specified
                if (!is.null(follow) && !follow %in% names(mydata)) {
                    stop(.("Follow variable not found in dataset."))
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

            # Exclude NA handling - Enhanced data preprocessing
            excl <- self$options$excl
            if (excl) {
                mydata <- jmvcore::naOmit(mydata)
            }

            # Prepare Data
            # Select only the columns we need, handling NULL values
            columns_to_select <- c(myvars)
            if (!is.null(percvar)) columns_to_select <- c(columns_to_select, percvar)
            if (!is.null(summaryvar)) columns_to_select <- c(columns_to_select, summaryvar)
            if (!is.null(prunebelow)) columns_to_select <- c(columns_to_select, prunebelow)
            if (!is.null(follow)) columns_to_select <- c(columns_to_select, follow)
            
            mydata <- jmvcore::select(df = mydata, columnNames = unique(columns_to_select))

            # Create label mapping for vtree display
            # This will show original variable names in the tree display
            xlabelvar <- setNames(all_labels[myvars], myvars)
            
            # Prepare Variables - Use cleaned variable names for processing
            myvars1 <- myvars

            # Handle Percentage Variable - Enhanced percentage handling
            if (!is.null(percvar) && !is.null(self$options$percvarLevel)) {
                xsummary <- paste0(percvar, "=", self$options$percvarLevel)
            }

            # Handle Summary Variable - Enhanced statistical summaries
            if (!is.null(summaryvar)) {
                summarylocation <- self$options$summarylocation

                if (summarylocation == "leafonly") {
                    summarylocation1 <- "%leafonly%"
                } else if (summarylocation == "allnodes") {
                    summarylocation1 <- "%allnodes%"
                }

                xsummary <- paste0(
                    summaryvar, " \\n\\n",
                    summaryvar, "\\n",
                    "mean=%mean%", "\\n",
                    "SD=%SD%", "\\n",
                    summarylocation1, "\\n"
                )
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
                
                enhanced_content <- paste0(
                    results1, 
                    "<br><br>", about_section,
                    "<br><br>", clinical_summary, 
                    "<br><br>", interpretation
                )
                self$results$text1$setContent(enhanced_content)
            } else {
                self$results$text1$setContent(results1)
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
        
        # Helper function to build conditional options for pruning/following
        .buildConditionalOption = function(variable, level1, level2) {
            if (is.null(variable) || is.null(level1) || is.null(level2)) {
                return(NULL)
            }
            
            variable_term <- jmvcore::composeTerm(variable)
            level1_term <- jmvcore::composeTerm(level1)
            level2_term <- jmvcore::composeTerm(level2)
            
            return(paste0("list(", variable_term, "=c('", level1_term, "','", level2_term, "'))"))
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
            
            if (!is.null(self$options$percvar)) {
                summary_parts <- c(summary_parts, 
                    paste0("• ", .("Percentages calculated for the selected outcome variable"), "<br>"))
            }
            
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
            
            # Add warnings for common issues
            if (nrow(self$data) < 50) {
                about_parts <- c(about_parts, 
                    paste0("<br>⚠️ <b>", .("Note:"), "</b> ", 
                           .("Small sample size may limit interpretation reliability"), "<br>"))
            }
            
            about_parts <- c(about_parts, "</div>")
            return(paste(about_parts, collapse = ""))
        }
    )
)