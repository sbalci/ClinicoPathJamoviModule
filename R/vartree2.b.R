#' @title Variable Tree 2
#'
#' @description
#' An updated version of the Variable Tree function for the ClinicoPath Descriptives module.
#' This function uses the new vtree package API and is aimed for use by medical professionals,
#' pathologists, and oncologists.
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#'
vartree2Class <- if (requireNamespace("jmvcore")) R6::R6Class(
    "vartree2Class",
    inherit = vartree2Base,
    private = list(
        .run = function() {

            # If no variables are specified, display a welcome message.
            if (is.null(self$options$vars)) {
                todo <- "<br>Welcome to ClinicoPath Descriptives Module
                 <br><br>This tool will help you form a Variable Tree."
                self$results$todo$setContent(todo)
                return()
            } else {
                self$results$todo$setContent("")
            }

            # Check that the data has complete rows.
            if (nrow(self$data) == 0)
                stop("Data contains no (complete) rows")

            # Use the full data frame so that vtree can find all variables.
            mydata <- self$data

            # Build the whitespace-separated variable string.
            myvars1 <- paste(as.character(self$options$vars), collapse = " ")

            # Build the summary string if percvar or summaryvar is provided.
            xsummary <- ""
            if (!is.null(self$options$percvar)) {
                xsummary <- paste0(self$options$percvar, "=", self$options$percvarLevel)
            }
            if (!is.null(self$options$summaryvar)) {
                summarylocation <- self$options$summarylocation
                summarylocation1 <- if (summarylocation == "leafonly") "%leafonly%" else if (summarylocation == "allnodes") "%allnodes%" else ""
                xsummary <- paste0(
                    self$options$summaryvar, " \n\n",
                    self$options$summaryvar, "\n",
                    "mean=%mean%", "\n",
                    "SD=%SD%", "\n",
                    summarylocation1, "\n"
                )
            }

            # Build the prune list (if specified).
            xprunebelow <- list()
            if (!is.null(self$options$prunebelow)) {
                pb  <- jmvcore::composeTerm(self$options$prunebelow)
                pl1 <- jmvcore::composeTerm(self$options$pruneLevel1)
                pl2 <- jmvcore::composeTerm(self$options$pruneLevel2)
                xprunebelow[[ pb ]] <- c(pl1, pl2)
            }

            # Build the follow list (if specified).
            xfollow <- list()
            if (!is.null(self$options$follow)) {
                fol  <- jmvcore::composeTerm(self$options$follow)
                fol1 <- jmvcore::composeTerm(self$options$followLevel1)
                fol2 <- jmvcore::composeTerm(self$options$followLevel2)
                xfollow[[ fol ]] <- c(fol1, fol2)
            }

            # Determine the prunesmaller value if enabled.
            xprunesmaller <- if (self$options$useprunesmaller) self$options$prunesmaller else NULL

            # Call the updated vtree function.

            private$.checkpoint()


            results <- vtree::vtree(
                data              = mydata,
                vars              = myvars1,
                sameline          = self$options$sline,
                title             = self$options$mytitle,
                horiz             = self$options$horizontal,
                showvarnames      = self$options$varnames,
                showlegend        = self$options$legend,
                showpct           = self$options$pct,
                splitspaces       = TRUE,
                prunebelow        = xprunebelow,
                follow            = xfollow,
                prunesmaller      = xprunesmaller,
                seq               = self$options$sequence,
                pattern           = self$options$pattern,
                ptable            = self$options$ptable,
                shownodelabels    = self$options$nodelabel,
                showcount         = self$options$showcount,
                varnamepointsize  = 18,
                vp                = self$options$vp,
                summary           = xsummary,
                pngknit           = TRUE,
                root              = TRUE
            )

            # Export the resulting diagram as SVG and assign it to the output.
            results_svg <- DiagrammeRsvg::export_svg(gv = results)
            self$results$text1$setContent(results_svg)

            # If a pattern table was requested, set that output as well.
            if (self$options$ptable)
                self$results$text2$setContent(results)
        }
    )
)
