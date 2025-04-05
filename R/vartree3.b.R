#' @title Variable Tree V3
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import DiagrammeR
#' @import DiagrammeRsvg

vartree3Class <- if (requireNamespace('jmvcore')) R6::R6Class(
    "vartree3Class",
    inherit = vartree3Base,
    private = list(
        .run = function() {

            # Initial check for variables
            if (is.null(self$options$vars)) {
                todo <- "
                <br>Welcome to ClinicoPath Descriptives Module
                <br><br>
                This tool will help you form an enhanced Variable Tree.
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

            # Style handling
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

            # Default Arguments
            xsplitspaces <- TRUE
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

            # Style-specific settings
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

            # Exclude NA handling
            excl <- self$options$excl
            if (excl) {
                mydata <- jmvcore::naOmit(mydata)
            }

            # Prepare Data
            mydata <- jmvcore::select(df = mydata,
                                      columnNames = c(myvars, percvar, summaryvar))

            # Prepare Formula
            formula <- jmvcore::constructFormula(terms = self$options$vars)
            myvars1 <- jmvcore::decomposeFormula(formula = formula)
            myvars1 <- unlist(myvars1)
            myvars1 <- paste0(myvars1, collapse = " ")

            # Handle Percentage Variable
            if (!is.null(self$options$percvar) && !is.null(self$options$percvarLevel)) {
                percvar <- self$options$percvar
                xsummary <- paste0(percvar, "=", self$options$percvarLevel)
            }

            # Handle Summary Variable
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

            # Handle Prune Below
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

            # Handle Follow Below
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

            # Handle Interpretation
            if (self$options$showInterpretation) {
                mytitle <- paste0(mytitle, "\\n(Interpretation will be shown below)")
            }

            # Run vtree function

            private$.checkpoint()


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

            # Convert to SVG
            results1 <- DiagrammeRsvg::export_svg(gv = results)
            self$results$text1$setContent(print(results1))

            # Handle pattern table
            if (self$options$ptable) {
                self$results$text2$setContent(results)
            }

            # Add interpretation if requested
            if (self$options$showInterpretation) {
                interpretation <- private$.generateInterpretation(results)
                self$results$text1$setContent(paste0(results1, "<br><br>", interpretation))
            }
        },

        .generateInterpretation = function(results) {
            # Helper function to generate interpretation text
            # This is a placeholder - expand based on your needs
            interpretation <- "<b>Tree Interpretation:</b><br>"
            interpretation <- paste0(interpretation,
                                     "- The tree shows the hierarchical relationship between variables<br>",
                                     "- Each node shows counts and percentages<br>")
            if (self$options$pct) {
                interpretation <- paste0(interpretation,
                                         "- Percentages are calculated relative to parent nodes<br>")
            }
            return(interpretation)
        }
    )
)
