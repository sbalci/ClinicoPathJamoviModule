#' @title Circos Chord Diagram
#' @importFrom R6 R6Class
#' @import jmvcore
#' @export

circosClass <- R6::R6Class(
    "circosClass",
    inherit = circosBase,
    private = list(

        .init = function() {
            todo <- glue::glue(
                "<h3>Circos Chord Diagram</h3>
                <p>A chord diagram shows the flow or co-occurrence between categories on a
                circle: each category is a sector and ribbons connect categories with a
                width proportional to the strength of their relationship.</p>
                <p><b>Provide either:</b></p>
                <ul>
                  <li>an <b>edge list</b> - a <i>From</i> and a <i>To</i> variable and an
                      optional <i>Value</i> weight; or</li>
                  <li><b>two categorical variables</b> to be cross-tabulated into links.</li>
                </ul>
                <p>Use it for state transitions, gene co-mutation, referral flows, or
                movement between diagnostic categories.</p>"
            )
            self$results$todo$setContent(todo)
        },

        .run = function() {
            opt <- self$options
            if (is.null(opt$fromVar) || is.null(opt$toVar)) return()

            mat <- private$.buildMatrix()
            if (is.null(mat)) return()

            if (opt$showMatrix)
                private$.populateMatrix(mat)
            self$results$plot$setState(mat)
            if (opt$showExplanation)
                private$.populateExplanation()
        },

        .buildMatrix = function() {
            opt <- self$options
            from <- as.character(self$data[[opt$fromVar]])
            to   <- as.character(self$data[[opt$toVar]])

            if (!is.null(opt$valueVar) && opt$inputMode == "edges") {
                val <- jmvcore::toNumeric(self$data[[opt$valueVar]])
            } else {
                val <- rep(1, length(from))
            }

            ok <- !is.na(from) & !is.na(to) & !is.na(val)
            from <- from[ok]; to <- to[ok]; val <- val[ok]
            if (length(from) == 0) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>No complete links to display.</p>")
                return(NULL)
            }

            cats <- sort(unique(c(from, to)))
            if (length(cats) < 2 || length(cats) > 50) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>Need between 2 and 50 distinct categories.</p>")
                return(NULL)
            }
            mat <- matrix(0, length(cats), length(cats),
                          dimnames = list(cats, cats))
            for (i in seq_along(from))
                mat[from[i], to[i]] <- mat[from[i], to[i]] + val[i]

            if (opt$symmetric) {
                mat <- mat + t(mat)
                mat[lower.tri(mat)] <- 0   # keep upper triangle to avoid double ribbons
            }
            mat
        },

        .populateMatrix = function(mat) {
            tab <- self$results$matrixTable
            cats <- rownames(mat)
            # add a column per target once
            if (length(tab$columns) == 0) {
                tab$addColumn(name = "from", title = "From \\ To", type = "text")
                for (cn in cats)
                    tab$addColumn(name = paste0("c_", cn), title = cn, type = "number")
            }
            for (i in seq_along(cats)) {
                vals <- list(from = cats[i])
                for (j in seq_along(cats))
                    vals[[paste0("c_", cats[j])]] <- mat[i, j]
                tab$addRow(rowKey = i, values = vals)
            }
        },

        .populateExplanation = function() {
            self$results$explanation$setContent(
                "<h4>Chord diagrams (circular visualization)</h4>
                <p>Categories are arranged as sectors around a circle; a ribbon between two
                sectors represents a link, and its width is proportional to the link's
                value. Directional ribbons use a difference in height and arrowheads to
                show flow from source to target; symmetric mode combines the two directions
                into a single undirected ribbon (appropriate for co-occurrence).</p>
                <p>The diagram is drawn with the <i>circlize</i> package (Gu et al., 2014).
                It summarizes many pairwise relationships compactly, making dominant flows
                and hubs visually apparent.</p>")
        },

        .plot = function(image, ggtheme, theme, ...) {
            mat <- image$state
            if (is.null(mat)) return(FALSE)
            if (!requireNamespace("circlize", quietly = TRUE)) return(FALSE)

            opt <- self$options
            cats <- rownames(mat)
            pal <- opt$gridPalette
            ncol_needed <- length(cats)
            cols <- tryCatch(
                grDevices::hcl.colors(ncol_needed, palette = "Set2"),
                error = function(e) grDevices::rainbow(ncol_needed))
            if (requireNamespace("RColorBrewer", quietly = TRUE)) {
                maxc <- switch(pal, Set2 = 8, Dark2 = 8, Paired = 12, Spectral = 11, 8)
                base <- RColorBrewer::brewer.pal(min(max(3, ncol_needed), maxc), pal)
                cols <- grDevices::colorRampPalette(base)(ncol_needed)
            }
            grid.col <- stats::setNames(cols, cats)

            circlize::circos.clear()
            dirType <- if (opt$directional && !opt$symmetric)
                c("diffHeight", "arrows") else "diffHeight"
            circlize::chordDiagram(
                mat, grid.col = grid.col,
                transparency = opt$transparency,
                directional = if (opt$directional && !opt$symmetric) 1 else 0,
                direction.type = dirType,
                link.arr.type = "big.arrow",
                annotationTrack = if (opt$showLabels) c("name", "grid") else "grid")
            circlize::circos.clear()
            TRUE
        }
    )
)
