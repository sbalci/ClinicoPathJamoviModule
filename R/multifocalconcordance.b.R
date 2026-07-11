#' @title Multifocal / Primary-Metastasis Concordance
#' @importFrom R6 R6Class
#' @import jmvcore
#' @export

multifocalconcordanceClass <- R6::R6Class(
    "multifocalconcordanceClass",
    inherit = multifocalconcordanceBase,
    private = list(

        .init = function() {
            self$results$todo$setContent(glue::glue(
                "<h3>Multifocal / Primary-Metastasis Concordance</h3>
                <p>Assesses whether biomarkers or mutations agree across multiple foci of a
                tumour, or between paired primary and metastasis samples.</p>
                <p><b>Provide</b> a <b>Case ID</b> (grouping the foci of one patient), optionally a
                <b>Focus / Sample ID</b> (to order paired comparisons), and one or more
                categorical <b>marker</b> variables. Reports per-marker concordance, Cohen's kappa
                for two-sample designs, and a case-level clonality summary.</p>"))
        },

        .run = function() {
            opt <- self$options
            if (is.null(opt$caseId) || is.null(opt$markers) || length(opt$markers) == 0)
                return()
            prep <- private$.prepare()
            if (is.null(prep)) return()

            if (opt$showPerMarker)  private$.perMarker(prep)
            if (opt$showCaseLevel)  private$.caseLevel(prep)
            if (opt$showDiscordance && prep$paired) private$.discordance(prep)
            if (opt$showPlot)       self$results$plot$setState(prep)
            if (opt$showSummary)    private$.summary(prep)
            if (opt$showExplanation) private$.explanation()
        },

        .prepare = function() {
            opt <- self$options; data <- self$data
            cid <- as.character(data[[opt$caseId]])
            fid <- if (!is.null(opt$focusId)) as.character(data[[opt$focusId]]) else NULL
            markers <- opt$markers
            keep <- !is.na(cid)
            if (sum(keep) < 2) {
                self$results$todo$setContent(
                    "<p style='color:#a33'>Need at least two rows (foci) with a case ID.</p>")
                return(NULL)
            }
            # foci per case
            foci_per_case <- table(cid[keep])
            # paired design when every multi-focus case has exactly 2 foci
            multi <- foci_per_case[foci_per_case >= 2]
            paired <- length(multi) > 0 && all(multi == 2)
            list(cid = cid, fid = fid, markers = markers, data = data,
                 keep = keep, paired = paired, foci_per_case = foci_per_case)
        },

        .concordanceForMarker = function(p, marker) {
            # for each case with >=2 non-missing values, concordant = all identical
            vals <- as.character(p$data[[marker]])
            df <- data.frame(cid = p$cid, v = vals, stringsAsFactors = FALSE)
            df <- df[!is.na(df$v) & !is.na(df$cid), ]
            sp <- split(df$v, df$cid)
            sp <- sp[sapply(sp, length) >= 2]
            if (length(sp) == 0) return(NULL)
            conc <- sapply(sp, function(v) length(unique(v)) == 1)
            list(nCases = length(sp), concordant = sum(conc),
                 rate = mean(conc), sp = sp)
        },

        .kappaForMarker = function(p, marker) {
            # Cohen's kappa for a paired (exactly 2 foci) design, using focus order if given
            vals <- as.character(p$data[[marker]])
            df <- data.frame(cid = p$cid, fid = if (is.null(p$fid)) NA else p$fid,
                             v = vals, stringsAsFactors = FALSE)
            df <- df[!is.na(df$v) & !is.na(df$cid), ]
            sp <- split(df, df$cid)
            sp <- sp[sapply(sp, nrow) == 2]
            if (length(sp) < 3) return(NA_real_)
            pairs <- do.call(rbind, lapply(sp, function(s) {
                if (!all(is.na(s$fid))) s <- s[order(s$fid), ]
                data.frame(a = s$v[1], b = s$v[2], stringsAsFactors = FALSE)
            }))
            lv <- union(unique(pairs$a), unique(pairs$b))
            tt <- table(factor(pairs$a, levels = lv), factor(pairs$b, levels = lv))
            if (sum(tt) == 0) return(NA_real_)
            po <- sum(diag(tt)) / sum(tt)
            pe <- sum(rowSums(tt) * colSums(tt)) / sum(tt)^2
            if (pe == 1) return(NA_real_)
            (po - pe) / (1 - pe)
        },

        .perMarker = function(p) {
            tab <- self$results$perMarkerTable
            for (m in p$markers) {
                cm <- private$.concordanceForMarker(p, m)
                if (is.null(cm)) {
                    tab$addRow(rowKey = m, values = list(marker = m, nCases = 0,
                        concordant = 0, rate = NA, kappa = "\u2014"))
                    next
                }
                kappaTxt <- "\u2014"
                if (self$options$showKappa && p$paired) {
                    k <- private$.kappaForMarker(p, m)
                    if (!is.na(k)) kappaTxt <- sprintf("%.3f", k)
                }
                tab$addRow(rowKey = m, values = list(
                    marker = m, nCases = cm$nCases, concordant = cm$concordant,
                    rate = cm$rate, kappa = kappaTxt))
            }
            note <- "Concordance = proportion of cases whose foci all share the same value."
            if (self$options$showKappa && !p$paired)
                note <- paste(note, "Cohen's kappa is shown only for paired (two-focus) designs.")
            tab$setNote("def", note)
        },

        .caseLevel = function(p) {
            # per case: fully concordant across ALL markers, partially, or discordant
            cases <- names(p$foci_per_case)[p$foci_per_case >= 2]
            if (length(cases) == 0) {
                self$results$caseLevelTable$setNote("na", "No cases with \u22652 foci.")
                return()
            }
            status <- sapply(cases, function(cc) {
                idx <- which(p$cid == cc)
                agree <- sapply(p$markers, function(m) {
                    v <- as.character(p$data[[m]])[idx]; v <- v[!is.na(v)]
                    if (length(v) < 2) NA else length(unique(v)) == 1
                })
                agree <- agree[!is.na(agree)]
                if (length(agree) == 0) "Uninformative"
                else if (all(agree)) "Fully concordant"
                else if (any(agree)) "Partially concordant"
                else "Fully discordant"
            })
            n <- length(status)
            tab <- self$results$caseLevelTable
            for (lev in c("Fully concordant", "Partially concordant",
                          "Fully discordant", "Uninformative")) {
                cnt <- sum(status == lev)
                if (cnt > 0 || lev != "Uninformative")
                    tab$addRow(rowKey = lev, values = list(
                        pattern = lev, n = cnt, pct = cnt / n))
            }
        },

        .discordance = function(p) {
            tab <- self$results$discordanceTable
            for (m in p$markers) {
                vals <- as.character(p$data[[m]])
                df <- data.frame(cid = p$cid, fid = if (is.null(p$fid)) NA else p$fid,
                                 v = vals, stringsAsFactors = FALSE)
                df <- df[!is.na(df$v) & !is.na(df$cid), ]
                sp <- split(df, df$cid); sp <- sp[sapply(sp, nrow) == 2]
                if (length(sp) == 0) next
                # detect a 2-level (pos/neg-like) marker
                lv <- sort(unique(df$v))
                if (length(lv) != 2) {
                    tab$addRow(rowKey = m, values = list(marker = m,
                        gainDir = NA, lossDir = NA))
                    next
                }
                pos <- lv[2]; neg <- lv[1]   # second level treated as "positive"
                gain <- 0L; loss <- 0L
                for (s in sp) {
                    if (!all(is.na(s$fid))) s <- s[order(s$fid), ]
                    if (s$v[1] == neg && s$v[2] == pos) gain <- gain + 1L
                    if (s$v[1] == pos && s$v[2] == neg) loss <- loss + 1L
                }
                tab$addRow(rowKey = m, values = list(marker = m,
                    gainDir = gain, lossDir = loss))
            }
            tab$setNote("dir", "Direction uses focus order (or the second level as 'positive'); primary = first focus.")
        },

        .summary = function(p) {
            rates <- sapply(p$markers, function(m) {
                cm <- private$.concordanceForMarker(p, m)
                if (is.null(cm)) NA else cm$rate
            })
            rates <- rates[!is.na(rates)]
            if (length(rates) == 0) { self$results$summary$setContent(
                "<p>No cases with \u22652 foci were available for concordance.</p>"); return() }
            worst <- p$markers[which.min(rates)]
            html <- glue::glue(
                "<p>Across the assessed markers, concordance between foci ranged from
                <b>{sprintf('%.0f%%', 100*min(rates))}</b> ({worst}) to
                <b>{sprintf('%.0f%%', 100*max(rates))}</b>, with a mean of
                <b>{sprintf('%.0f%%', 100*mean(rates))}</b>.
                {ifelse(p$paired, 'This is a paired (two-sample) design; Cohen&#39;s kappa is reported per marker.', 'Cases have varying numbers of foci; concordance is defined as all foci sharing the same value.')}</p>")
            self$results$summary$setContent(html)
        },

        .explanation = function() {
            self$results$explanation$setContent(
                "<h4>Multifocal / primary-metastasis concordance</h4>
                <p>When a biomarker or mutation is assessed in more than one focus of a tumour,
                or in paired primary and metastatic samples, concordance quantifies how often the
                results agree. For each case with two or more foci, a marker is <b>concordant</b>
                when all foci share the same value.</p>
                <p>Per-marker concordance is the proportion of concordant cases; for paired
                (two-sample) designs, <b>Cohen's kappa</b> additionally corrects for chance
                agreement. The case-level summary classifies each case as fully concordant,
                partially concordant (agreement on some but not all markers), or fully discordant.
                For a two-level marker, the discordance table splits disagreements by direction
                (gain vs loss from the primary / first focus).</p>")
        },

        .plot = function(image, ggtheme, theme, ...) {
            p <- image$state
            if (is.null(p)) return(FALSE)
            rates <- sapply(p$markers, function(m) {
                cm <- private$.concordanceForMarker(p, m)
                if (is.null(cm)) NA else cm$rate
            })
            df <- data.frame(marker = p$markers, rate = 100 * rates)
            df <- df[!is.na(df$rate), ]
            if (nrow(df) == 0) return(FALSE)
            df <- df[order(df$rate), ]
            df$marker <- factor(df$marker, levels = df$marker)
            gg <- ggplot2::ggplot(df, ggplot2::aes(x = marker, y = rate)) +
                ggplot2::geom_col(fill = "#2c7fb8", width = 0.7) +
                ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", rate)),
                                   hjust = -0.15, size = 3.4) +
                ggplot2::geom_hline(yintercept = 100, linetype = "dashed", colour = "grey60") +
                ggplot2::coord_flip(ylim = c(0, 105)) +
                ggplot2::labs(x = NULL, y = "Concordance (%)",
                    title = "Between-focus concordance by marker") + ggtheme
            print(gg)
            TRUE
        }
    )
)
