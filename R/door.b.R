#' @title Desirability of Outcome Ranking (DOOR)
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats qnorm var
#' @export

doorClass <- R6::R6Class(
    "doorClass",
    inherit = doorBase,
    private = list(

        .init = function() {
            todo <- glue::glue(
                "<h3>Desirability of Outcome Ranking (DOOR)</h3>
                <p>DOAR compares two groups on an ordinal outcome that ranks each patient
                by overall desirability (combining efficacy and safety). The <b>DOOR
                probability</b> is the chance that a randomly chosen index-group patient
                has a more desirable outcome than a randomly chosen reference-group
                patient, with ties split evenly. A value above <b>0.5</b> favours the
                index group.</p>
                <p><b>Provide:</b> a two-level <b>group variable</b> (and its reference
                level) and an <b>ordinal DOOR outcome</b>, plus the direction indicating
                which values are more desirable.</p>"
            )
            self$results$todo$setContent(todo)
        },

        .run = function() {
            opt <- self$options
            if (is.null(opt$group) || is.null(opt$doorRank)) return()
            if (is.null(opt$refLevel) || opt$refLevel == "") return()

            d <- private$.prepareData()
            if (is.null(d)) return()

            res <- private$.doorProb(d)
            private$.populateMain(res, d)
            if (opt$showDistribution)
                private$.populateDistribution(d)
            if (opt$showPlot)
                self$results$plot$setState(d)
            if (opt$showSummary)
                private$.populateSummary(res, d)
            if (opt$showExplanation)
                private$.populateExplanation()
        },

        .prepareData = function() {
            opt <- self$options
            g <- as.factor(self$data[[opt$group]])
            raw <- self$data[[opt$doorRank]]

            # numeric rank from the door outcome (preserve ordinal ordering)
            if (is.factor(raw) || is.character(raw)) {
                rf <- as.factor(raw)
                rankNum <- as.integer(rf)
                catLevels <- levels(rf)
            } else {
                rankNum <- jmvcore::toNumeric(raw)
                catLevels <- sort(unique(rankNum))
            }

            ok <- !is.na(g) & !is.na(rankNum)
            g <- droplevels(g[ok]); rankNum <- rankNum[ok]
            if (nlevels(g) != 2) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>The group variable must have exactly two levels.</p>")
                return(NULL)
            }
            refLbl <- opt$refLevel
            if (!(refLbl %in% levels(g))) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>The chosen reference level is not present.</p>")
                return(NULL)
            }
            idxLbl <- setdiff(levels(g), refLbl)

            # orient so that SMALLER oriented-rank = MORE desirable
            orient <- if (opt$rankDirection == "higher") -rankNum else rankNum

            list(g = g, rankNum = rankNum, orient = orient,
                 idxLbl = idxLbl, refLbl = refLbl,
                 catLevels = catLevels, conf = opt$conf_level,
                 higherBetter = (opt$rankDirection == "higher"))
        },

        .doorProb = function(d) {
            tx <- d$orient[d$g == d$idxLbl]
            ct <- d$orient[d$g == d$refLbl]
            n1 <- length(tx); n0 <- length(ct)
            # index "wins" when its oriented-rank is smaller (more desirable)
            wins <- 0; ties <- 0
            for (a in tx) { wins <- wins + sum(a < ct); ties <- ties + sum(a == ct) }
            door <- (wins + 0.5 * ties) / (n1 * n0)
            # placement-based variance (Mann-Whitney / DeLong style)
            p_tx <- vapply(tx, function(a) (sum(a < ct) + 0.5 * sum(a == ct)) / n0, numeric(1))
            p_ct <- vapply(ct, function(b) (sum(b > tx) + 0.5 * sum(b == tx)) / n1, numeric(1))
            se <- sqrt(stats::var(p_tx) / n1 + stats::var(p_ct) / n0)
            z <- stats::qnorm(1 - (1 - d$conf) / 2)
            ci <- door + c(-1, 1) * z * se
            ci <- pmin(pmax(ci, 0), 1)
            list(door = door, low = ci[1], high = ci[2], se = se,
                 n1 = n1, n0 = n0)
        },

        .populateMain = function(res, d) {
            self$results$mainTable$addRow(rowKey = "door", values = list(
                statistic = sprintf("DOOR probability (%s more desirable)", d$idxLbl),
                estimate = res$door, ci_lower = res$low, ci_upper = res$high))
            self$results$mainTable$setNote("door",
                "DOOR probability > 0.5 favours the index group; 0.5 = no difference.")
        },

        .populateDistribution = function(d) {
            tab <- self$results$distributionTable
            # report by original rank value, ordered by desirability
            ur <- sort(unique(d$rankNum))
            if (d$higherBetter) ur <- rev(ur)  # most desirable first
            idx <- d$rankNum[d$g == d$idxLbl]
            ref <- d$rankNum[d$g == d$refLbl]
            n1 <- length(idx); n0 <- length(ref)
            lab <- function(v) if (length(d$catLevels) >= max(ur, na.rm = TRUE) &&
                                   is.character(d$catLevels)) d$catLevels[v] else as.character(v)
            for (v in ur) {
                ni <- sum(idx == v); nr <- sum(ref == v)
                tab$addRow(rowKey = v, values = list(
                    category = lab(v),
                    n_index = ni, pct_index = if (n1 > 0) ni / n1 else 0,
                    n_ref = nr, pct_ref = if (n0 > 0) nr / n0 else 0))
            }
        },

        .populateSummary = function(res, d) {
            fav <- if (res$door > 0.5) d$idxLbl else d$refLbl
            # escape data-derived group level labels before HTML interpolation
            idxLbl <- htmltools::htmlEscape(d$idxLbl)
            refLbl <- htmltools::htmlEscape(d$refLbl)
            fav <- htmltools::htmlEscape(fav)
            strength <- if (res$low > 0.5 || res$high < 0.5)
                "statistically favours" else "does not significantly favour"
            html <- glue::glue(
                "<p>The DOOR probability is <b>{sprintf('%.3f', res$door)}</b>
                ({sprintf('%.0f', 100*d$conf)}% CI {sprintf('%.3f', res$low)}\u{2013}{sprintf('%.3f', res$high)}):
                a randomly selected <b>{idxLbl}</b> patient has a more desirable outcome
                than a randomly selected <b>{refLbl}</b> patient
                {sprintf('%.1f', 100*res$door)}% of the time (ties split evenly). The
                result {strength} <b>{fav}</b>.</p>")
            self$results$summary$setContent(html)
        },

        .populateExplanation = function() {
            self$results$explanation$setContent(
                "<h4>Desirability of Outcome Ranking</h4>
                <p>Each patient is assigned to an ordinal DOOR category reflecting overall
                clinical desirability, combining efficacy and safety into a single ranked
                outcome (Evans et al., 2015). Rather than dichotomizing, every index-group
                patient is compared with every reference-group patient.</p>
                <p>The <b>DOOR probability</b> is the proportion of such comparisons in
                which the index patient is more desirable, with ties contributing one half.
                This equals the Mann-Whitney/AUC statistic; 0.5 indicates no difference and
                values above 0.5 favour the index group. The confidence interval uses the
                placement-based (DeLong-type) variance for the two-sample statistic.</p>")
        },

        .plot = function(image, ggtheme, theme, ...) {
            d <- image$state
            if (is.null(d)) return(FALSE)
            ur <- sort(unique(d$rankNum))
            labFor <- function(v) if (is.character(d$catLevels) &&
                                      length(d$catLevels) >= max(ur, na.rm = TRUE))
                                      d$catLevels[v] else as.character(v)
            rows <- list()
            for (grp in c(d$idxLbl, d$refLbl)) {
                sub <- d$rankNum[d$g == grp]; ng <- length(sub)
                for (v in ur)
                    rows[[length(rows) + 1]] <- data.frame(
                        group = grp, category = labFor(v),
                        catval = v, prop = sum(sub == v) / ng)
            }
            df <- do.call(rbind, rows)
            # order categories by desirability (most desirable first in legend)
            ord <- if (d$higherBetter) rev(ur) else ur
            df$category <- factor(df$category, levels = vapply(ord, labFor, character(1)))
            df$group <- factor(df$group, levels = c(d$refLbl, d$idxLbl))
            p <- ggplot2::ggplot(df, ggplot2::aes(x = group, y = prop, fill = category)) +
                ggplot2::geom_col(width = 0.6) +
                ggplot2::scale_y_continuous(labels = scales::percent) +
                ggplot2::scale_fill_brewer(palette = "RdYlGn", direction = -1) +
                ggplot2::labs(x = NULL, y = "Proportion", fill = "DOOR category",
                              title = "DOOR outcome distribution by group") +
                ggtheme
            print(p)
            TRUE
        }
    )
)
