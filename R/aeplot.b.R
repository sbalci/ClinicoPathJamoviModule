#' Adverse Events Butterfly Plot — backend
#'
#' Inspired by the Jamovi-TrialPlots module by highwind
#' (https://github.com/highwindmx/Jamovi-TrialPlots), released under LGPL.
#' This is an independent re-implementation for ClinicoPath (GPL-2): it adds a
#' patient-level input mode that computes AE incidence internally and uses an
#' English UI and ClinicoPath output patterns.
#'
#' @importFrom R6 R6Class
#' @import ggplot2
#' @importFrom magrittr %>%

aeplotClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "aeplotClass",
    inherit = aeplotBase,
    private = list(

        .run = function() {
            self$results$instructions$setContent(private$.instructionsHtml())

            model <- private$.buildButterflyData()
            if (is.null(model) || is.null(model$table) || nrow(model$table) == 0)
                return()

            # populate frequency table (one row per ae x arm)
            tbl <- self$results$freqTable
            tab <- model$table
            for (i in seq_len(nrow(tab))) {
                tbl$addRow(rowKey = i, values = list(
                    ae        = as.character(tab$ae[i]),
                    arm       = as.character(tab$arm[i]),
                    allGrade  = tab$allGrade[i],
                    highGrade = tab$highGrade[i]
                ))
            }

            self$results$plot$setState(model)
            self$results$interpretation$setContent(private$.interpretationHtml())
        },

        # ---- data assembly -------------------------------------------------
        .buildButterflyData = function() {
            if (self$options$inputMode == "summary")
                private$.buildFromSummary()
            else
                private$.buildFromPatient()
        },

        .buildFromPatient = function() {
            opt <- self$options
            if (is.null(opt$aeTerm))
                return(NULL)

            df <- self$data
            term <- as.character(df[[opt$aeTerm]])
            armcol <- if (!is.null(opt$armVar)) df[[opt$armVar]] else NULL
            arm  <- if (!is.null(armcol)) as.character(armcol) else rep("Test", nrow(df))
            subj <- if (!is.null(opt$subjectID)) as.character(df[[opt$subjectID]]) else NULL
            grade <- if (!is.null(opt$gradeVar)) jmvcore::toNumeric(df[[opt$gradeVar]]) else NULL

            keep <- !is.na(term) & !is.na(arm)
            term <- term[keep]; arm <- arm[keep]
            if (!is.null(subj)) subj <- subj[keep]
            if (!is.null(grade)) grade <- grade[keep]

            # deterministic arm order: factor levels if available, else sorted unique
            if (!is.null(armcol) && is.factor(armcol)) {
                arms <- levels(droplevels(factor(arm, levels = levels(armcol))))
            } else {
                arms <- sort(unique(arm))
            }

            # denominators: distinct subjects per arm (or event counts if no subjectID)
            denom <- sapply(arms, function(a) {
                if (!is.null(subj)) length(unique(subj[arm == a])) else sum(arm == a)
            })
            names(denom) <- arms

            terms <- unique(term)
            rows <- list()
            for (a in arms) {
                for (t in terms) {
                    sel <- arm == a & term == t
                    if (!is.null(subj)) {
                        all_n  <- length(unique(subj[sel]))
                        high_n <- if (!is.null(grade)) length(unique(subj[sel & grade >= opt$gradeThreshold])) else NA_real_
                    } else {
                        all_n  <- sum(sel)
                        high_n <- if (!is.null(grade)) sum(sel & grade >= opt$gradeThreshold) else NA_real_
                    }
                    d <- denom[[a]]
                    rows[[length(rows) + 1]] <- data.frame(
                        ae = t, arm = a,
                        allGrade  = if (d > 0) 100 * all_n / d else 0,
                        highGrade = if (is.na(high_n) || d == 0) NA_real_ else 100 * high_n / d,
                        stringsAsFactors = FALSE
                    )
                }
            }
            tab <- do.call(rbind, rows)
            private$.assembleModel(tab, arms)
        },

        .buildFromSummary = function() {
            opt <- self$options
            if (is.null(opt$aeTermS) || is.null(opt$testAll))
                return(NULL)
            df <- self$data
            has_ctrl <- !is.null(opt$controlAll)
            tab <- data.frame(
                ae = as.character(df[[opt$aeTermS]]),
                arm = "Test",
                allGrade = jmvcore::toNumeric(df[[opt$testAll]]),
                highGrade = if (!is.null(opt$testHigh)) jmvcore::toNumeric(df[[opt$testHigh]]) else NA_real_,
                stringsAsFactors = FALSE
            )
            arms <- "Test"
            if (has_ctrl) {
                ctrl <- data.frame(
                    ae = as.character(df[[opt$aeTermS]]),
                    arm = "Control",
                    allGrade = jmvcore::toNumeric(df[[opt$controlAll]]),
                    highGrade = if (!is.null(opt$controlHigh)) jmvcore::toNumeric(df[[opt$controlHigh]]) else NA_real_,
                    stringsAsFactors = FALSE
                )
                tab <- rbind(tab, ctrl)
                arms <- c("Test", "Control")
            }
            tab <- tab[!is.na(tab$ae), , drop = FALSE]
            private$.assembleModel(tab, arms)
        },

        # order terms by first-arm all-grade desc, apply topN, return model list
        .assembleModel = function(tab, arms) {
            if (is.null(tab) || nrow(tab) == 0) return(NULL)
            first_rows <- tab[tab$arm == arms[1], c("ae", "allGrade")]
            ord <- first_rows$ae[order(-first_rows$allGrade)]
            ord <- unique(ord)
            if (self$options$topN > 0 && length(ord) > self$options$topN)
                ord <- ord[seq_len(self$options$topN)]
            tab <- tab[tab$ae %in% ord, , drop = FALSE]
            # rev so the highest-frequency term ends up at the TOP after coord_flip
            tab$ae <- factor(tab$ae, levels = rev(ord))
            list(table = tab, arms = arms, hasControl = length(arms) > 1)
        },

        # ---- palette -------------------------------------------------------
        .armColor = function() {
            scheme <- self$options$colorScheme
            pick <- function(pal) tryCatch(pal(3)[1], error = function(e) "#0072B5")
            switch(scheme,
                nejm    = pick(ggsci::pal_nejm()),
                lancet  = pick(ggsci::pal_lancet()),
                jama    = pick(ggsci::pal_jama()),
                jco     = pick(ggsci::pal_jco()),
                npg     = pick(ggsci::pal_npg()),
                aaas    = pick(ggsci::pal_aaas()),
                colorblind = "#0072B2",
                "#0072B5"
            )
        },

        # ---- plot ----------------------------------------------------------
        .plot = function(image, ...) {
            model <- image$state
            if (is.null(model)) return(FALSE)
            tab <- model$table
            test_col <- private$.armColor()
            ctrl_col <- "grey60"

            # test arm on the negative side, control on the positive side
            tab$sign <- ifelse(tab$arm == model$arms[1], -1, 1)
            tab$allSigned  <- tab$sign * tab$allGrade
            tab$highSigned <- tab$sign * tab$highGrade

            p <- ggplot2::ggplot(tab, ggplot2::aes(x = ae))
            if (self$options$barShape == "inside") {
                # nested: all-grade (light) with high-grade (dark) overlaid
                p <- p +
                    ggplot2::geom_col(ggplot2::aes(y = allSigned, fill = arm), alpha = 0.5, width = 0.7) +
                    ggplot2::geom_col(ggplot2::aes(y = highSigned, fill = arm), alpha = 1.0, width = 0.7)
            } else {
                # stacked: (all - high) light stacked on high dark
                tab$lowSigned <- tab$allSigned - ifelse(is.na(tab$highSigned), 0, tab$highSigned)
                p <- ggplot2::ggplot(tab, ggplot2::aes(x = ae)) +
                    ggplot2::geom_col(ggplot2::aes(y = highSigned, fill = arm), alpha = 1.0, width = 0.7) +
                    ggplot2::geom_col(ggplot2::aes(y = lowSigned, fill = arm), alpha = 0.5, width = 0.7)
            }
            if (isTRUE(self$options$showValues)) {
                p <- p + ggplot2::geom_text(
                    ggplot2::aes(y = allSigned,
                                 label = sprintf("%.0f", abs(allGrade)),
                                 hjust = ifelse(sign < 0, 1.1, -0.1)),
                    size = 3)
            }
            fills <- stats::setNames(c(test_col, ctrl_col), c(model$arms[1], if (model$hasControl) model$arms[2] else NULL))

            p <- p +
                ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
                ggplot2::scale_fill_manual(name = .("Arm"), values = fills) +
                ggplot2::scale_y_continuous(labels = function(y) abs(y)) +
                ggplot2::coord_flip() +
                ggplot2::labs(x = .("Adverse Event Term"), y = .("Incidence (%)")) +
                ggplot2::theme_classic() +
                ggplot2::theme(legend.position = "bottom")
            print(p)
            TRUE
        },

        # ---- HTML ----------------------------------------------------------
        .instructionsHtml = function() {
            paste0(
                "<div style='padding:8px;'>",
                "<b>", .("Adverse Events Butterfly Plot"), "</b><br>",
                .("Patient mode: select Subject ID, AE Term, and (optionally) Arm and Grade \u{2014} incidence is computed internally."),
                "<br>",
                .("Summary mode: provide pre-computed all-grade and high-grade percentages per AE term."),
                "</div>"
            )
        },

        .interpretationHtml = function() {
            paste0(
                "<div style='padding:8px;'>",
                .("Bars extend left for the test arm and right for the control arm; darker shading marks high-grade events."),
                "<br><i>",
                .("Inspired by the Jamovi-TrialPlots module by highwind (github.com/highwindmx/Jamovi-TrialPlots)."),
                "</i></div>"
            )
        }
    )
)
