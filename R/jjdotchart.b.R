#' @title Dot Chart (Summary vs Reference Value)
#' @importFrom R6 R6Class
#' @importFrom rlang sym
#'
#' @return An \code{R6} class generator object for the \code{jjdotchartClass}
#'   backend; used internally by the jamovi analysis wrapper and not called
#'   directly.


jjdotchartClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjdotchartClass",
    inherit = jjdotchartBase,
    private = list(

        # Every resampling path in this analysis runs under this seed. The
        # error bars come from a datawizard bootstrap that ggstatsplot does not
        # seed, so three identical calls returned three different intervals
        # (group A conf.low 9.6999 / 9.7501 / 9.6782). In jamovi the analysis
        # re-runs on every option change, so unseeded bars visibly twitch when
        # the user toggles something unrelated - which reads as instability in
        # the data rather than in the renderer.
        .SEED = 20250101L,

        .noticeList = NULL,
        .inputsValid = FALSE,
        # Reason the statistics engine produced nothing, if it did (see
        # .engineFails). Set in .run(), where a notice can still be rendered.
        .engineFailure = NULL,
        .prepared = NULL,
        .tab = NULL,

        # ---- notices (HTML, not jmvcore::Notice - those cannot serialise) ----

        .addNotice = function(message, type = "INFO") {
            style <- switch(type,
                "ERROR"   = list(fg = "#721c24", bg = "#f8d7da", br = "#f5c6cb"),
                "WARNING" = list(fg = "#856404", bg = "#fff3cd", br = "#ffeaa7"),
                                  list(fg = "#004085", bg = "#cce5ff", br = "#b8daff"))
            private$.noticeList <- c(private$.noticeList, paste0(
                "<div style='background-color:", style$bg,
                "; border-left:4px solid ", style$br,
                "; padding:12px; margin:8px 0; color:", style$fg, ";'>",
                "<strong>", type, ":</strong> ", message, "</div>"))
            self$results$notices$setContent(paste(private$.noticeList, collapse = "\n"))
        },

        .clearNotices = function() {
            private$.noticeList <- NULL
            self$results$notices$setContent("")
        },

        # ---- vocabulary -------------------------------------------------------

        # ggdotplotstats plots a DIFFERENT summary per test type - verified on
        # skewed data: parametric 33.77 (mean), nonparametric 9.75 (median),
        # robust 9.81 (20% trimmed mean) for the same group. A results column
        # hard-labelled "Mean" would therefore be false for three of the four
        # settings, so the label is derived instead.
        .summaryLabel = function() {
            switch(as.character(self$options$typestatistics),
                   parametric    = "Mean",
                   nonparametric = "Median",
                   robust        = "Trimmed mean (20%)",
                   bayes         = "MAP estimate",
                   "Summary")
        },

        # Mid-sentence form. Not tolower() at the call sites: that rendered the
        # Bayesian label as "the map estimate" in every notice and table note.
        .summaryLabelLower = function() {
            if (identical(as.character(self$options$typestatistics), "bayes"))
                "MAP estimate"
            else
                tolower(private$.summaryLabel())
        },

        .testLabel = function() {
            switch(as.character(self$options$typestatistics),
                   parametric    = "one-sample t-test",
                   nonparametric = "Wilcoxon signed-rank test",
                   robust        = "bootstrapped trimmed-mean test",
                   bayes         = "Bayesian one-sample test",
                   "one-sample test")
        },

        # ---- data -------------------------------------------------------------

        .prepareData = function() {
            if (!is.null(private$.prepared)) return(private$.prepared)

            dep <- self$options$dep; grp <- self$options$group
            if (is.null(dep) || is.null(grp)) return(NULL)

            mydata <- self$data
            cols <- c(dep, grp)
            if (!is.null(self$options$grvar)) cols <- c(cols, self$options$grvar)
            if (!all(cols %in% names(mydata))) return(NULL)

            # toNumeric() is a no-op on a plain character/factor column - it only
            # unwraps a jamovi `values` attribute - so coerce explicitly rather
            # than letting text reach the summarisers.
            num <- jmvcore::toNumeric(mydata[[dep]])
            if (!is.numeric(num)) num <- suppressWarnings(as.numeric(as.character(num)))
            mydata[[dep]] <- num

            n0 <- nrow(mydata)
            mydata <- mydata[stats::complete.cases(mydata[cols]), , drop = FALSE]

            # complete.cases() follows is.na(), which is TRUE for NaN but FALSE
            # for Inf. An infinite observation therefore survives into the group
            # summary and plots that whole group at Inf (measured: means
            # 10.01, 10.08, Inf), which silently destroys the axis.
            fin <- is.finite(mydata[[dep]])
            n_inf <- sum(!fin)
            if (n_inf > 0) mydata <- mydata[fin, , drop = FALSE]

            mydata[[grp]] <- droplevels(as.factor(mydata[[grp]]))
            if (!is.null(self$options$grvar))
                mydata[[self$options$grvar]] <- droplevels(as.factor(mydata[[self$options$grvar]]))

            private$.prepared <- list(data = mydata, n_dropped = n0 - nrow(mydata),
                                      n_inf = n_inf)
            private$.prepared
        },

        # Per-group summaries taken from the SAME call ggdotplotstats makes to
        # build the figure - statsExpressions::centrality_description(data,
        # <group>, <measurement>) - so the table cannot disagree with the plot.
        #
        # Re-deriving them by hand was wrong twice. (1) The Bayesian summary is
        # the MAP, not the mean: hand-rolled, the table read 33.77 while the
        # figure plotted 9.19 for the same group, under a column headed "MAP
        # estimate". (2) The interval is type-specific and bootstrapped, not a
        # normal-theory t interval on the mean: for a skewed group the hand
        # interval around the MEDIAN was (-18.0, 37.5) while the figure's error
        # bar was (9.30, 10.17) - sixty times too wide, and negative for a
        # strictly positive measurement.
        #
        # The bootstrap must run under the same seed as the plot, or the table's
        # interval and the drawn error bar disagree by a resampling wobble.
        # Use the same user-selected seed for the table and the plot.
        .groupTable = function(mydata) {
            if (!is.null(private$.tab)) return(private$.tab)
            dep <- self$options$dep; grp <- self$options$group
            if (nrow(mydata) == 0) return(NULL)

            cd <- tryCatch(withr::with_seed(self$options$seed, suppressWarnings(
                    rlang::inject(statsExpressions::centrality_description(
                        data       = mydata,
                        x          = !!rlang::sym(grp),
                        y          = !!rlang::sym(dep),
                        type       = self$options$typestatistics,
                        conf.level = self$options$conflevel,
                        digits     = self$options$k,
                        tr         = 0.2)))),
                error = function(e) e)
            if (inherits(cd, "condition") || is.null(cd) || nrow(cd) == 0) return(NULL)
            cd <- as.data.frame(cd)

            # SD stays a plain stats::sd of the raw observations: it describes
            # the data, is defined the same way whatever test is selected, and
            # nothing on the figure contradicts it.
            sds <- tapply(mydata[[dep]], mydata[[grp]],
                          function(x) if (length(x) > 1) stats::sd(x) else NA_real_)

            private$.tab <- data.frame(
                grp     = as.character(cd[[grp]]),
                n       = as.integer(cd$n.obs),
                value   = as.numeric(cd[[dep]]),
                sd      = as.numeric(sds[as.character(cd[[grp]])]),
                ci_low  = as.numeric(cd$conf.low),
                ci_high = as.numeric(cd$conf.high),
                stringsAsFactors = FALSE, row.names = NULL)
            private$.tab
        },

        # ---- the silent-failure probe -----------------------------------------

        # ggdotplotstats swallows an engine failure and returns a plot whose
        # subtitle is NULL: a figure with no statistics and nothing saying why.
        # It is data dependent, so probe the same engine rather than guessing.
        #
        # The usual trigger is the reference value sitting far outside the group
        # summaries, which makes the standardised effect enormous and the
        # effect-size confidence interval fail to converge. Measured on means of
        # 9.89-10.06: reference 0 and 5 both die with "function cannot be
        # evaluated at initial parameters", while 9, 9.9 and 10 all compute.
        # That indicts the package default of 0 for any measurement not centred
        # near zero, which is most clinical data.
        .engineFails = function(values) {
            if (!isTRUE(self$options$resultssubtitle)) return(NULL)
            if (length(values) < 2) return(NULL)
            res <- tryCatch(
                withr::with_seed(self$options$seed,
                    statsExpressions::one_sample_test(
                        data       = data.frame(.v = values),
                        x          = !!rlang::sym(".v"),
                        type       = self$options$typestatistics,
                        test.value = self$options$testvalue,
                        conf.level = self$options$conflevel,
                        digits     = self$options$k)),
                error = function(e) e)
            if (inherits(res, "condition")) return(conditionMessage(res))
            if (is.null(res$expression) || length(res$expression) == 0)
                return("the statistics engine returned no expression")
            NULL
        },

        # ---- validation --------------------------------------------------------

        .validate = function(prep) {
            mydata <- prep$data
            grp <- self$options$group

            if (nrow(mydata) == 0) {
                private$.addNotice("No rows remain after removing missing values. Check the selected variables.", "ERROR")
                return(FALSE)
            }
            if (prep$n_inf > 0)
                private$.addNotice(sprintf(
                    "%d row(s) had an infinite value for '%s' and were excluded. Infinite values usually mean a division by zero or an out-of-range entry - check the source data.",
                    prep$n_inf, htmltools::htmlEscape(self$options$dep)), "WARNING")

            tab <- private$.groupTable(mydata)
            k <- if (is.null(tab)) 0L else nrow(tab)

            if (k < 2) {
                private$.addNotice(sprintf(
                    "Only %d group has data. A dot chart needs at least two groups, and the statistical test needs at least three to be meaningful.", k), "ERROR")
                return(FALSE)
            }

            # The test's n IS k. Say so numerically rather than leaving the
            # reader to infer it from a degrees-of-freedom term in the subtitle.
            private$.addNotice(sprintf(
                "Each of the %d groups is reduced to one point (its %s), so the %s runs on %d group summaries, NOT on the %d observations. This tests the group summaries against the Reference Value; it does not compare the groups with each other.",
                k, private$.summaryLabelLower(), private$.testLabel(), k, nrow(mydata)), "INFO")

            if (k < 4)
                private$.addNotice(sprintf(
                    "With only %d groups, inference from their summaries is imprecise; treat the p-value and effect estimate with caution.",
                    k), "WARNING")

            # Warn BEFORE the engine is asked, because this is the fixable cause.
            rng <- range(tab$value)
            tv <- self$options$testvalue
            if (tv < rng[1] || tv > rng[2]) {
                span <- diff(rng)
                far <- span == 0 || min(abs(tv - rng)) > 3 * span
                private$.addNotice(sprintf(
                    "The Reference Value (%s) lies outside the range of the group %ss (%s to %s).%s",
                    base::format(tv), private$.summaryLabelLower(),
                    base::format(signif(rng[1], 4)), base::format(signif(rng[2], 4)),
                    if (far) " That is far outside, which can make the effect size impossible to bound and leave the plot with no statistics at all - retain a scientifically prespecified reference and consider a different inferential method." else ""),
                    if (far) "WARNING" else "INFO")
            }
            TRUE
        },

        # ---- lifecycle ---------------------------------------------------------

        .init = function() {
            w <- self$options$plotwidth %||% 650
            h <- self$options$plotheight %||% 450
            self$results$plot$setSize(w, h)
            if (!is.null(self$options$grvar)) {
                n <- nlevels(droplevels(as.factor(self$data[[self$options$grvar]])))
                self$results$plot2$setSize(min(max(n, 1L) * w, 3000L), h)
            }
        },

        .run = function() {
            private$.clearNotices()
            private$.inputsValid <- FALSE
            private$.engineFailure <- NULL
            private$.prepared <- NULL
            private$.tab <- NULL

            if (is.null(self$options$dep) || is.null(self$options$group)) {
                self$results$todo$setContent(paste0(
                    "<br>Welcome to ClinicoPath<br><br>",
                    "This tool draws a <b>Cleveland dot chart</b>: each group becomes ONE point, ",
                    "and those points are tested against a Reference Value you choose.<br><br>",
                    "<b>The test's sample size is the number of groups, not the number of patients.</b> ",
                    "To compare groups with each other using every observation, use ",
                    "<b>Box-Violin Plots to Compare Between Groups</b> instead.<br>",
                    "<br>Select a Measurement and a Groups variable to begin.<br><hr>"))
                return()
            }
            self$results$todo$setContent("")

            if (nrow(self$data) == 0) {
                private$.addNotice("Data contains no rows.", "ERROR")
                return()
            }

            prep <- private$.prepareData()
            if (is.null(prep)) return()
            if (prep$n_dropped > 0)
                private$.addNotice(sprintf("%d row(s) with missing values were excluded.",
                                           prep$n_dropped), "INFO")

            if (!private$.validate(prep)) return()
            private$.inputsValid <- TRUE

            tab <- private$.groupTable(prep$data)
            private$.fillTable(tab)

            # Probe HERE, in .run(): a notice raised from a render callback is
            # discarded, because jamovi has already composed the results panel
            # by the time a figure is drawn.
            private$.engineFailure <- private$.engineFails(tab$value)
            if (!is.null(private$.engineFailure))
                private$.addNotice(sprintf(
                    "The statistics could not be computed for this data (%s), so the chart carries NO test result - only the points. This usually means the Reference Value is far from the group %ss; retain the scientifically prespecified reference value and consider a different inferential method.",
                    htmltools::htmlEscape(private$.engineFailure),
                    private$.summaryLabelLower()), "ERROR")
        },

        .fillTable = function(tab) {
            t <- self$results$summary
            if (is.null(tab)) return()
            t$setNote("agg", sprintf(
                "Each row is one plotted point. 'Summary' is the %s of that group's observations, which is the statistic the selected test uses, and Lower/Upper are the same %g%% interval drawn as that point's error bar on the chart. 'SD' is the standard deviation of the raw observations. 'vs Reference' is Summary minus the Reference Value (%s).",
                private$.summaryLabelLower(), 100 * self$options$conflevel,
                base::format(self$options$testvalue)))
            # jmvcore Tables have no setRows(); clear and re-add.
            t$deleteRows()
            for (i in seq_len(nrow(tab))) {
                t$addRow(rowKey = i, values = list(
                    grp = tab$grp[i], n = tab$n[i], value = tab$value[i],
                    sd = tab$sd[i],
                    ci_low = tab$ci_low[i], ci_high = tab$ci_high[i],
                    vsref = tab$value[i] - self$options$testvalue))
            }
        },

        # ---- plotting ----------------------------------------------------------

        .plotArgs = function(mydata, ggtheme) {
            o <- self$options
            xlab <- if (nzchar(o$xtitle)) o$xtitle else o$dep
            ylab <- if (nzchar(o$ytitle)) o$ytitle else o$group

            # The reference line is OURS. ggdotplotstats never draws test.value:
            # its only vertical line is the centre of the plotted points, and it
            # sits in the same place for reference values of 0, 12 and 999
            # (measured). Leaving that as the sole line invites the reader to
            # take it for the threshold they typed in.
            comp <- list(ggplot2::geom_vline(
                xintercept = o$testvalue, linetype = "dashed",
                colour = "#b22222", linewidth = 0.7))

            cap <- paste0("Dashed red line: Reference Value = ", base::format(o$testvalue))
            if (isTRUE(o$centralityplotting))
                cap <- paste0(cap, ". Solid blue line: centre of the plotted points (not the reference).")

            list(
                data = mydata,
                xlab = xlab, ylab = ylab,
                title = if (nzchar(o$mytitle)) o$mytitle else NULL,
                caption = cap,
                type = o$typestatistics,
                test.value = o$testvalue,
                conf.level = o$conflevel,
                digits = o$k,
                bf.message = isTRUE(o$bfmessage),
                results.subtitle = isTRUE(o$resultssubtitle),
                centrality.plotting = isTRUE(o$centralityplotting),
                centrality.type = o$centralitytype,
                ggplot.component = comp,
                ggtheme = if (isTRUE(o$originaltheme)) ggstatsplot::theme_ggstatsplot() else ggtheme)
        },

        .plotFailure = function(msg) {
            print(ggplot2::ggplot() +
                  ggplot2::annotate("text", x = 0, y = 0, size = 4, colour = "#721c24",
                                    label = paste(strwrap(msg, width = 60), collapse = "\n")) +
                  ggplot2::theme_void())
            TRUE
        },

        .plot = function(image, ggtheme, theme, ...) {
            if (!isTRUE(private$.inputsValid)) return()
            prep <- private$.prepareData()
            if (is.null(prep)) return()

            private$.checkpoint()
            args <- private$.plotArgs(prep$data, ggtheme)

            # Splice with `!!!`, NOT do.call(). rlang::inject() puts the SYMBOL
            # into the argument list, and do.call() then evaluates it, so
            # do.call(f, c(list(x = !!sym("v")), args)) dies with
            # "object 'v' not found" (measured). Calling directly with !!! keeps
            # the symbol unevaluated, which is what ggstatsplot's tidy-eval wants.
            p <- tryCatch(
                withr::with_seed(self$options$seed, rlang::inject(
                    ggstatsplot::ggdotplotstats(
                        x = !!rlang::sym(self$options$dep),
                        y = !!rlang::sym(self$options$group),
                        !!!args))),
                error = function(e) e)

            if (inherits(p, "condition"))
                return(private$.plotFailure(sprintf(
                    "The chart could not be drawn: %s. Check that each group has at least one usable observation.",
                    conditionMessage(p))))
            print(p)
            TRUE
        },

        .plot2 = function(image, ggtheme, theme, ...) {
            if (!isTRUE(private$.inputsValid) || is.null(self$options$grvar)) return()
            prep <- private$.prepareData()
            if (is.null(prep)) return()

            private$.checkpoint()
            args <- private$.plotArgs(prep$data, ggtheme)
            # grouped_ggdotplotstats titles each panel with its own level, so a
            # `title` passed through ... collides with its own argument and throws
            # 'formal argument "title" matched by multiple actual arguments'
            # (verified - the same defect that left the Split By figure of
            # jjdotplotstats blank). The overall title belongs to the annotation.
            ttl <- args$title; args$title <- NULL

            args$annotation.args <- list(title = ttl)
            p <- tryCatch(
                withr::with_seed(self$options$seed, rlang::inject(
                    ggstatsplot::grouped_ggdotplotstats(
                        x = !!rlang::sym(self$options$dep),
                        y = !!rlang::sym(self$options$group),
                        grouping.var = !!rlang::sym(self$options$grvar),
                        !!!args))),
                error = function(e) e)

            if (inherits(p, "condition"))
                return(private$.plotFailure(sprintf(
                    "The split chart could not be drawn: %s. Check that every level of the Split By variable has at least two groups with data.",
                    conditionMessage(p))))
            print(p)
            TRUE
        }
    )
)
