#' @title Fragility Index for Dichotomous Outcomes
#' @importFrom R6 R6Class
#' @import jmvcore
#' @export

fragilityindexClass <- R6::R6Class(
    "fragilityindexClass",
    inherit = fragilityindexBase,
    private = list(

        .init = function() {
            todo <- glue::glue(
                "<h3>Fragility Index for Dichotomous Outcomes</h3>
                <p>The <b>Fragility Index (FI)</b> is the minimum number of patients whose
                outcome would need to change to flip a result across the significance
                threshold. A small index means the finding rests on only a few events.
                The <b>Fragility Quotient (FQ)</b> divides the index by the total sample
                size, allowing comparison across studies.</p>
                <p><b>Provide either:</b></p>
                <ul>
                  <li><b>Summary counts</b> &mdash; events and totals for each of the two groups, or</li>
                  <li><b>Raw data</b> &mdash; a two-level group variable and a binary outcome variable.</li>
                </ul>
                <p>For a significant result, the index counts reversals toward
                non-significance; for a non-significant result, the <b>reverse fragility
                index</b> counts reversals needed to reach significance.</p>"
            )
            self$results$todo$setContent(todo)
        },

        .run = function() {
            opt <- self$options

            counts <- private$.getCounts()
            if (is.null(counts)) return()
            e1 <- counts$e1; n1 <- counts$n1; e2 <- counts$e2; n2 <- counts$n2

            if (opt$showCounts)
                private$.populateCounts(counts)

            fr <- private$.fragility(e1, n1, e2, n2, opt$alpha, opt$testType)
            if (is.null(fr)) return()

            private$.populateMain(fr)
            if (opt$showTrajectory)
                private$.populateTrajectory(fr)
            if (opt$showPlot)
                self$results$plot$setState(fr)
            if (opt$showSummary)
                private$.populateSummary(fr)
            if (opt$showExplanation)
                private$.populateExplanation()
        },

        # ---- obtain 2x2 counts from summary or raw data --------------------
        .getCounts = function() {
            opt <- self$options
            if (opt$dataFormat == "summary") {
                e1 <- opt$events1; n1 <- opt$n1; e2 <- opt$events2; n2 <- opt$n2
                if (is.null(e1) || is.null(n1) || is.null(e2) || is.null(n2))
                    return(NULL)
                if (e1 > n1 || e2 > n2) {
                    self$results$todo$setContent(
                        "<p style='color:#a33'>Events cannot exceed the group total.</p>")
                    return(NULL)
                }
                return(list(e1 = e1, n1 = n1, e2 = e2, n2 = n2,
                            g1 = "Group 1", g2 = "Group 2"))
            }
            # raw data
            if (is.null(opt$group) || is.null(opt$outcome)) return(NULL)
            g <- self$data[[opt$group]]; o <- self$data[[opt$outcome]]
            ok <- !is.na(g) & !is.na(o)
            g <- droplevels(as.factor(g[ok])); o <- as.factor(o[ok])
            if (nlevels(g) != 2) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>The group variable must have exactly two levels.</p>")
                return(NULL)
            }
            ev <- if (!is.null(opt$outcomeEvent) && opt$outcomeEvent != "")
                      opt$outcomeEvent else levels(o)[nlevels(o)]
            isEvent <- as.character(o) == ev
            gl <- levels(g)
            list(e1 = sum(isEvent[g == gl[1]]), n1 = sum(g == gl[1]),
                 e2 = sum(isEvent[g == gl[2]]), n2 = sum(g == gl[2]),
                 g1 = gl[1], g2 = gl[2])
        },

        .pval = function(a, c, n1, n2, testType) {
            m <- matrix(c(a, n1 - a, c, n2 - c), nrow = 2, byrow = TRUE)
            if (testType == "chisq")
                suppressWarnings(stats::chisq.test(m, correct = FALSE)$p.value)
            else
                stats::fisher.test(m)$p.value
        },

        # ---- fragility engine ----------------------------------------------
        .fragility = function(e1, n1, e2, n2, alpha, testType) {
            pf <- function(a, c) private$.pval(a, c, n1, n2, testType)
            p0 <- pf(e1, e2)
            sig0 <- p0 < alpha
            a <- e1; c <- e2; fi <- 0L
            traj <- list(c(step = 0L, e1 = a, e2 = c, p = p0))
            maxit <- n1 + n2
            target <- if (sig0) function(p) p >= alpha else function(p) p < alpha
            repeat {
                moves <- list()
                if (a < n1) moves[["g1u"]] <- c(a + 1, c)
                if (a > 0)  moves[["g1d"]] <- c(a - 1, c)
                if (c < n2) moves[["g2u"]] <- c(a, c + 1)
                if (c > 0)  moves[["g2d"]] <- c(a, c - 1)
                ps <- vapply(moves, function(m) pf(m[1], m[2]), numeric(1))
                if (length(ps) == 0) { fi <- NA_integer_; break }
                idx <- if (sig0) which.max(ps) else which.min(ps)
                a <- moves[[idx]][1]; c <- moves[[idx]][2]; fi <- fi + 1L
                traj[[length(traj) + 1]] <- c(step = fi, e1 = a, e2 = c, p = ps[idx])
                if (target(ps[idx])) break
                if (fi > maxit) { fi <- NA_integer_; break }
            }
            N <- n1 + n2
            list(fi = fi, fq = if (is.na(fi)) NA_real_ else fi / N,
                 p0 = p0, sig0 = sig0, alpha = alpha, N = N,
                 type = if (sig0) "Fragility Index" else "Reverse Fragility Index",
                 traj = traj, e1 = e1, n1 = n1, e2 = e2, n2 = n2)
        },

        .populateCounts = function(counts) {
            tab <- self$results$countsTable
            tab$addRow(rowKey = 1, values = list(
                grp = counts$g1, events = counts$e1,
                nonevents = counts$n1 - counts$e1, n = counts$n1,
                prop = counts$e1 / counts$n1))
            tab$addRow(rowKey = 2, values = list(
                grp = counts$g2, events = counts$e2,
                nonevents = counts$n2 - counts$e2, n = counts$n2,
                prop = counts$e2 / counts$n2))
        },

        .populateMain = function(fr) {
            tab <- self$results$mainTable
            tab$addRow(rowKey = "p0", values = list(
                statistic = "Baseline p-value", value = fr$p0))
            tab$addRow(rowKey = "fi", values = list(
                statistic = fr$type, value = fr$fi))
            tab$addRow(rowKey = "fq", values = list(
                statistic = "Fragility Quotient (FI / N)", value = fr$fq))
            note <- if (fr$sig0)
                sprintf("Result significant at alpha = %.3g; FI = reversals toward non-significance.", fr$alpha)
            else
                sprintf("Result NOT significant at alpha = %.3g; reverse FI = reversals toward significance.", fr$alpha)
            tab$setNote("fi", note)
        },

        .populateTrajectory = function(fr) {
            tab <- self$results$trajectoryTable
            for (row in fr$traj)
                tab$addRow(rowKey = row["step"], values = list(
                    step = as.integer(row["step"]),
                    e1 = as.integer(row["e1"]), e2 = as.integer(row["e2"]),
                    pval = unname(row["p"])))
        },

        .populateSummary = function(fr) {
            if (is.na(fr$fi)) {
                self$results$summary$setContent(
                    "<p>The significance of this result could not be reversed within the
                    available sample &mdash; the outcome is not fragile by this measure.</p>")
                return()
            }
            interp <- if (fr$sig0) {
                strength <- if (fr$fi <= 3) "very fragile"
                            else if (fr$fi <= 10) "moderately fragile"
                            else "relatively robust"
                glue::glue(
                    "<p>The result is statistically significant (p = {sprintf('%.4f', fr$p0)}).
                    Changing the outcome of just <b>{fr$fi}</b> patient(s)
                    (Fragility Quotient = {sprintf('%.3f', fr$fq)}) would make it
                    non-significant, so the finding is <b>{strength}</b>.</p>")
            } else {
                glue::glue(
                    "<p>The result is not statistically significant (p = {sprintf('%.4f', fr$p0)}).
                    A change in the outcome of <b>{fr$fi}</b> patient(s) would be enough to
                    reach significance (reverse Fragility Quotient = {sprintf('%.3f', fr$fq)}).</p>")
            }
            self$results$summary$setContent(interp)
        },

        .populateExplanation = function() {
            self$results$explanation$setContent(
                "<h4>Fragility index methodology</h4>
                <p>The fragility index (Walsh et al., 2014) quantifies how robust a
                dichotomous trial result is. Starting from the observed 2x2 table, the
                outcome of one patient at a time is switched in the direction that most
                rapidly moves the test p-value across the significance threshold. The
                number of switches required is the fragility index; small values indicate
                a result driven by a few events.</p>
                <p>The <b>fragility quotient</b> divides the index by the total sample size
                to enable comparison between studies of different sizes. When the observed
                result is already non-significant, the <b>reverse fragility index</b> is
                reported instead: the number of outcome switches needed to attain
                significance.</p>
                <p>P-values at each step use the selected test (Fisher's exact or
                chi-square without continuity correction).</p>")
        },

        .plot = function(image, ggtheme, theme, ...) {
            fr <- image$state
            if (is.null(fr)) return(FALSE)
            df <- data.frame(
                step = vapply(fr$traj, function(r) as.integer(r["step"]), integer(1)),
                p    = vapply(fr$traj, function(r) unname(r["p"]), numeric(1)))
            p <- ggplot2::ggplot(df, ggplot2::aes(x = step, y = p)) +
                ggplot2::geom_hline(yintercept = fr$alpha, linetype = "dashed",
                                    colour = "#de2d26") +
                ggplot2::geom_line(colour = "#2c7fb8") +
                ggplot2::geom_point(colour = "#2c7fb8") +
                ggplot2::annotate("text", x = max(df$step), y = fr$alpha,
                                  label = sprintf("alpha = %.3g", fr$alpha),
                                  hjust = 1, vjust = -0.5, size = 3, colour = "#de2d26") +
                ggplot2::labs(x = "Number of outcome reversals", y = "p-value",
                              title = sprintf("%s = %s", fr$type,
                                              ifelse(is.na(fr$fi), "not reached", fr$fi))) +
                ggtheme
            print(p)
            TRUE
        }
    )
)
