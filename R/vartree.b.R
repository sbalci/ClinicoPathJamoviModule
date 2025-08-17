#' @title Variable Tree
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import DiagrammeR
#' @import DiagrammeRsvg
#' @import vtree
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
        .run = function() {

            # Initial check for variables
            if (is.null(self$options$vars)) {
                todo <- "
                <br>Welcome to ClinicoPath Descriptives Module
                <br><br>
                This tool will help you form an enhanced Variable Tree.
                <br><br>
                Enhanced features include:
                <br>• Multiple style presets (default, clean, minimal)
                <br>• Advanced color customization and gradients
                <br>• Statistical summaries in nodes
                <br>• Automatic interpretation generation
                <br>• Support for current CRAN vtree package
                "
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
                stop("Data contains no (complete) rows")

            # Read Data
            mydata <- self$data

            # Read Arguments
            horizontal <- self$options$horizontal
            sline <- self$options$sline
            mytitle <- self$options$mytitle
            myvars <- self$options$vars
            percvar <- self$options$percvar
            summaryvar <- self$options$summaryvar

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
            mydata <- jmvcore::select(df = mydata,
                                      columnNames = c(myvars, percvar, summaryvar))

            # Prepare Variables - Use raw variable names to preserve spaces
            # vtree expects a vector of variable names, so use self$options$vars directly
            myvars1 <- self$options$vars

            # Handle Percentage Variable - Enhanced percentage handling
            if (!is.null(self$options$percvar) && !is.null(self$options$percvarLevel)) {
                percvar <- self$options$percvar
                xsummary <- paste0(percvar, "=", self$options$percvarLevel)
            }

            # Handle Summary Variable - Enhanced statistical summaries
            if (!is.null(self$options$summaryvar)) {
                summaryvar <- self$options$summaryvar
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
            if (!is.null(self$options$prunebelow) &&
                !is.null(self$options$pruneLevel1) &&
                !is.null(self$options$pruneLevel2)) {

                prunebelow <- self$options$prunebelow
                prunebelow <- jmvcore::composeTerm(prunebelow)

                pruneLevel1 <- self$options$pruneLevel1
                pruneLevel1 <- jmvcore::composeTerm(pruneLevel1)

                pruneLevel2 <- self$options$pruneLevel2
                pruneLevel2 <- jmvcore::composeTerm(pruneLevel2)

                xprunebelow <- paste0("list(", prunebelow,
                                      "=c('", pruneLevel1, "','", pruneLevel2,"'))")
            }

            # Handle Follow Below - Enhanced follow controls
            if (!is.null(self$options$follow) &&
                !is.null(self$options$followLevel1) &&
                !is.null(self$options$followLevel2)) {

                follow <- self$options$follow
                follow <- jmvcore::composeTerm(follow)

                followLevel1 <- self$options$followLevel1
                followLevel1 <- jmvcore::composeTerm(followLevel1)

                followLevel2 <- self$options$followLevel2
                followLevel2 <- jmvcore::composeTerm(followLevel2)

                xfollow <- paste0("list(", follow,
                                  "=c('", followLevel1, "','", followLevel2,"'))")
            }

            # Handle Interpretation - Enhanced interpretation feature
            if (self$options$showInterpretation) {
                mytitle <- paste0(mytitle, "\\n(Interpretation will be shown below)")
            }

            # Run vtree function - Enhanced with modern vtree API
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
                prunebelow = if(!is.null(xprunebelow)) eval(parse(text = xprunebelow)) else NULL,
                follow = if(!is.null(xfollow)) eval(parse(text = xfollow)) else NULL,
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
                               '</div></script></body></html>')

            self$results$text1$setContent(results1)

            # Handle pattern table
            if (self$options$ptable) {
                self$results$text2$setContent(results)
            }

            # Add interpretation if requested - Enhanced interpretation
            if (self$options$showInterpretation) {
                interpretation <- private$.generateInterpretation(results)
                self$results$text1$setContent(paste0(results1, "<br><br>", interpretation))
            }
        },

        # Enhanced interpretation generation
        .generateInterpretation = function(results) {
            interpretation <- "<b>Variable Tree Interpretation:</b><br>"
            interpretation <- paste0(interpretation,
                                     "• The tree displays hierarchical relationships between categorical variables<br>",
                                     "• Each node shows counts and percentages for variable combinations<br>")
            if (self$options$pct) {
                interpretation <- paste0(interpretation,
                                         "• Percentages are calculated relative to parent nodes<br>")
            }
            if (!is.null(self$options$summaryvar)) {
                interpretation <- paste0(interpretation,
                                         "• Statistical summaries (mean, SD) are shown for the continuous variable<br>")
            }
            if (self$options$style == "clean") {
                interpretation <- paste0(interpretation,
                                         "• Clean style applied: minimal colors, focus on data structure<br>")
            } else if (self$options$style == "minimal") {
                interpretation <- paste0(interpretation,
                                         "• Minimal style applied: simplified layout with same-line presentation<br>")
            }
            interpretation <- paste0(interpretation,
                                     "• Tree structure helps identify patterns and relationships in categorical data<br>")
            return(interpretation)
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
        }
    )
)