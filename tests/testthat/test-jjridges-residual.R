# Residual-defect regression tests for `jjridges`.
#
# One test per defect fixed in the residual-review pass. Each asserts the
# USER-VISIBLE outcome (rendered PNG, table cell text, notice text), not the
# internal mechanism that produced it.

rr_run <- function(dat, ...) {
    o <- do.call(ClinicoPath:::jjridgesOptions$new,
                 utils::modifyList(list(x_var = "v", y_var = "g"), list(...)))
    a <- ClinicoPath:::jjridgesClass$new(options = o, data = dat)
    a$init()
    a$.__enclos_env__$private$.run()
    a
}

rr_data <- function(seed = 42) {
    set.seed(seed)
    data.frame(v = c(rnorm(40, 5), rnorm(40, 7), rnorm(40, 9)),
               g = factor(rep(c("A", "B", "C"), each = 40)))
}

rr_png <- function(analysis) {
    f <- tempfile(fileext = ".png")
    grDevices::png(f, width = 700, height = 550)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(analysis$results$plot$state)
    grDevices::dev.off()
    on.exit()
    unname(tools::md5sum(f))
}


test_that("quantile lines do not paint an unstyled grey ridge over the figure", {
    # The option used to append a bare ggridges::stat_density_ridges() as a SECOND
    # layer: default grey70 fill, default scale, no alpha. It covered the palette
    # and hid the boxplots completely. The quantile arguments now go to the layer
    # that already exists, so the figure keeps exactly the layers the other options
    # asked for.
    a <- rr_run(rr_data(), add_quantiles = TRUE, add_boxplot = TRUE, show_stats = FALSE)
    p <- a$results$plot$state

    geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
    expect_equal(sum(geoms %in% c("GeomDensityRidges", "GeomDensityRidges2")), 1L)
    expect_true("GeomBoxplot" %in% geoms)

    built <- ggplot2::ggplot_build(p)
    ridge_i <- which(geoms %in% c("GeomDensityRidges", "GeomDensityRidges2"))[1]
    fills <- unique(as.character(built$data[[ridge_i]]$fill))
    # the clinical colourblind-safe palette, not ggridges' default grey
    expect_false(any(grepl("^grey", fills)))
    expect_gt(length(fills), 1L)

    # the quantile lines are actually drawn (vline rows in the ridge layer)
    expect_true("datatype" %in% names(built$data[[ridge_i]]))
    expect_true("vline" %in% built$data[[ridge_i]]$datatype)

    # and turning the option on changes the picture (it is not silently inert)
    b <- rr_run(rr_data(), add_quantiles = FALSE, add_boxplot = TRUE, show_stats = FALSE)
    expect_false(identical(rr_png(a), rr_png(b)))

    # same for the two other density-based types
    for (pt in c("violin_ridges", "density_ridges_gradient")) {
        on  <- rr_png(rr_run(rr_data(), plot_type = pt, add_quantiles = TRUE,  show_stats = FALSE))
        off <- rr_png(rr_run(rr_data(), plot_type = pt, add_quantiles = FALSE, show_stats = FALSE))
        expect_false(identical(on, off), label = pt)
    }

    # histogram ridges cannot draw them: say so rather than ignore the option
    h <- rr_run(rr_data(), plot_type = "histogram_ridges", add_quantiles = TRUE, show_stats = FALSE)
    expect_match(h$results$notices$content, "Quantile lines not available")
})


test_that("a non-finite X value is dropped with a named warning instead of poisoning the run", {
    # jmvcore::naOmit keeps Inf. One Inf used to abort the whole analysis with
    # "missing value where TRUE/FALSE needed" (skewness guard), or -- on small n --
    # reach the copy-ready report as "Mean=Inf (SD=NaN)".
    d <- rr_data(); d$v[3] <- Inf
    a <- rr_run(d, show_stats = TRUE)

    notices <- a$results$notices$content
    expect_match(notices, "Non-finite values removed")
    expect_false(grepl("Data Validation Error", notices, fixed = TRUE))

    st <- a$results$statistics$asDF
    expect_true(all(is.finite(st$mean)))
    expect_true(all(is.finite(st$sd)))
    expect_equal(sum(st$n), 119)          # 120 rows minus the one Inf
    expect_gt(a$results$tests$rowCount, 0)
    expect_false(is.null(a$results$plot$state))

    # small-n path: the skewness branch is skipped, so this used to sail through silently
    d10 <- data.frame(v = c(1, 2, 3, 4, Inf, 6, 7, 8, 9, 10),
                      g = factor(rep(c("A", "B"), each = 5)))
    a10 <- rr_run(d10, show_stats = TRUE)
    expect_match(a10$results$notices$content, "Non-finite values removed")
    expect_true(all(is.finite(a10$results$statistics$asDF$mean)))
    expect_false(grepl("Inf", a10$results$reportSummary$content, fixed = TRUE))
})


test_that("Basic Ridgeline, Density Ridges and Violin Ridges are three different figures", {
    d <- rr_data()
    md5 <- vapply(c("ridgeline", "density_ridges", "violin_ridges"),
                  function(pt) rr_png(rr_run(d, plot_type = pt, show_stats = FALSE)),
                  character(1))
    expect_equal(length(unique(md5)), 3L)
})


test_that("a comparison that cannot be tested says so in the Method column and in a notice", {
    # Method used to print the raw option token ("parametric") on a row where no test
    # ran, and the explanation built by .performSingleTest was returned in a `warning`
    # element that nothing read.
    d <- data.frame(v = c(rnorm(30, 5), 4, rep(9, 20)),
                    g = factor(c(rep("A", 30), "B", rep("C", 20))))
    a <- rr_run(d, show_stats = TRUE, test_type = "parametric")

    tbl <- a$results$tests$asDF
    expect_true("not testable (n < 2)" %in% tbl$method)
    expect_false("parametric" %in% tbl$method)
    expect_match(a$results$notices$content, "Insufficient observations")
})


test_that("reusing the Y variable as Fill or Facet still produces comparisons, with a notice", {
    # Stratifying BY the Y variable puts one Y group in every stratum, so no pairwise
    # comparison was ever run -- a visible, empty, unexplained table next to an
    # "Analysis Complete" notice claiming tests had been run.
    d <- rr_data()
    for (opt in c("fill_var", "facet_var")) {
        a <- do.call(rr_run, c(list(d), stats::setNames(list("g"), opt),
                               list(show_stats = TRUE)))
        expect_gt(a$results$tests$rowCount, 0)
        expect_match(a$results$notices$content, "Grouping variable reused")
    }
})


test_that("re-running the same analysis does not duplicate table rows", {
    # jamovi reuses the R6 instance when options change, and jmvcore's addRow()
    # happily accepts a rowKey the table already holds. Neither table called
    # deleteRows(), so a 3-group analysis grew to 6 then 9 rows on the second and
    # third run, and asDF() then threw "duplicate 'row.names' are not allowed" --
    # which .generateReportSummary swallows, degrading the copy-ready Method line
    # from the test actually used back to the option label. clearWith only lists
    # the variable options, so any visual change (scale, alpha, theme) hit this.
    o <- ClinicoPath:::jjridgesOptions$new(x_var = "v", y_var = "g", show_stats = TRUE)
    a <- ClinicoPath:::jjridgesClass$new(options = o, data = rr_data())
    a$init()

    for (i in 1:3) {
        a$.__enclos_env__$private$.run()
        expect_equal(a$results$statistics$rowCount, 3L)   # one row per group
        expect_equal(a$results$tests$rowCount, 3L)        # 3 choose 2 comparisons
    }

    expect_equal(nrow(a$results$statistics$asDF), 3L)
    expect_setequal(as.character(a$results$statistics$asDF$group), c("A", "B", "C"))

    # the copy-ready paragraph names the test that ran, not the option label
    expect_match(a$results$reportSummary$content, "Method: t-test", fixed = TRUE)
})


test_that("the copy-ready report body does not hardcode a white background", {
    # `background:white` under `color: inherit` text rendered near-white-on-white
    # in jamovi's dark theme -- the panel is always visible, so the whole
    # copy-ready summary disappeared.
    a <- rr_run(rr_data())
    html <- a$results$reportSummary$content
    expect_false(grepl("background\\s*:\\s*white", html))
    expect_false(grepl("color\\s*:\\s*#(666|999)\\b", html))
})


rr_capture <- function(expr) {
    msgs <- character(0); warns <- character(0)
    withCallingHandlers(
        expr,
        message = function(m) { msgs  <<- c(msgs,  conditionMessage(m)); invokeRestart("muffleMessage") },
        warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") })
    list(messages = trimws(unique(msgs)), warnings = trimws(unique(warns)))
}

rr_fill_data <- function(seed = 11) {
    set.seed(seed)
    n <- 40
    data.frame(v = c(rnorm(n, 5), rnorm(n, 7), rnorm(n, 9)),
               g = factor(rep(c("A", "B", "C"), each = n)),
               s = factor(sample(c("F", "M"), 3 * n, TRUE)))
}


test_that("an explicit legend choice is honoured; only the default is auto-rescued", {
    # .applyAutoLegendOverride() used to fire on ANY legend_position == "none", so a user
    # who deliberately switched the legend off while a fill variable was in play had it
    # silently rewritten to "right" and could never turn it off.
    d <- rr_fill_data()

    # default: rescued, so the fill colours are explained
    a <- rr_run(d, x_var = "v", y_var = "g", fill_var = "s")
    expect_equal(as.character(a$results$plot$state$theme$legend.position), "right")

    # deliberate suppression via the fill-legend switch: respected
    b <- rr_run(d, x_var = "v", y_var = "g", fill_var = "s", show_fill_legend = FALSE)
    expect_equal(as.character(b$results$plot$state$theme$legend.position), "none")

    # every explicit position is passed through untouched
    for (lp in c("right", "bottom", "top")) {
        p <- rr_run(d, x_var = "v", y_var = "g", fill_var = "s", legend_position = lp)
        expect_equal(as.character(p$results$plot$state$theme$legend.position), lp)
    }
})


test_that("a factor level with no rows is not reported as an undersized group", {
    # table() counts dropped levels at 0, so .validateData warned about a group that
    # appears nowhere in the data, the table or the plot -- routine after row filtering.
    d <- data.frame(v = rnorm(40),
                    g = factor(rep(c("A", "B"), 20), levels = c("A", "B", "Z")))
    a <- rr_run(d, x_var = "v", y_var = "g")
    expect_false(grepl("Z", a$results$notices$content))
    expect_false(grepl("fewer than", a$results$notices$content))
    expect_equal(a$results$statistics$rowCount, 2L)

    # a genuinely small group is still reported
    d2 <- data.frame(v = c(rnorm(20), 1, 2), g = factor(c(rep("A", 20), "B", "B")))
    expect_true(grepl("fewer than 3", rr_run(d2, x_var = "v", y_var = "g")$results$notices$content))
})


test_that("library chatter does not leak into the Analysis Notes panel", {
    # ggridges emits "Picking joint bandwidth of <x>" from print(); jamovi shows it to the
    # user on EVERY render. A bad quantile string additionally leaked R's raw
    # "NAs introduced by coercion" next to the friendly notice that already explains it.
    a <- rr_run(rr_data(), x_var = "v", y_var = "g")
    render <- rr_capture({
        f <- tempfile(fileext = ".png"); grDevices::png(f)
        on.exit(grDevices::dev.off(), add = TRUE)
        expect_true(a$.__enclos_env__$private$.plot(a$results$plot, NULL, NULL))
    })
    expect_false(any(grepl("Picking joint bandwidth", render$messages)))

    quant <- rr_capture({
        b <- rr_run(rr_data(), x_var = "v", y_var = "g",
                    add_quantiles = TRUE, quantiles = "abc, 5")
        expect_true(grepl("Invalid quantiles", b$results$notices$content))
    })
    expect_false(any(grepl("NAs introduced by coercion", quant$warnings)))
})


test_that("the removed dpi option is gone from every layer", {
    # options(device.dpi = n) was assigned inside the renderer, after jamovi had already
    # opened the device: the PNG was byte-identical and the same size at 72 and at 600,
    # while the About panel told users to set 300 for journal submission.
    expect_null(ClinicoPath:::jjridgesOptions$new(x_var = "v", y_var = "g")$option("dpi"))
    src <- paste(readLines(testthat::test_path("..", "..", "R", "jjridges.b.R")), collapse = "\n")
    expect_false(grepl("self$options$dpi", src, fixed = TRUE))
    expect_false(grepl("device.dpi = dpi", src, fixed = TRUE))
})


test_that("the clinical preset tables in R and JavaScript do not drift apart", {
    # The presets exist twice on purpose: private$.PRESETS in R/jjridges.b.R drives the
    # analysis (and R callers, who get no JavaScript), while PRESETS in
    # jamovi/js/jjridges.events.js writes the same values into the jamovi controls so the
    # options panel shows what actually runs. Two copies drift; this parses both and fails
    # the moment they disagree.
    root  <- testthat::test_path("..", "..")
    b_src <- readLines(file.path(root, "R", "jjridges.b.R"), warn = FALSE)
    j_src <- readLines(file.path(root, "jamovi", "js", "jjridges.events.js"), warn = FALSE)

    # ---- R side: list("<option>", <value>, "<label>") rows inside .PRESETS ----
    b_start <- grep("^\\s*\\.PRESETS = list\\(", b_src)
    b_end   <- grep("^\\s*\\.applyClinicalPreset = function\\(\\)", b_src)
    expect_length(b_start, 1L); expect_length(b_end, 1L)
    b_block <- b_src[b_start:(b_end - 1L)]

    r_tab <- list(); cur <- NULL
    for (ln in b_block) {
        h <- regmatches(ln, regexec("^\\s{12}([a-z_]+) = list\\(", ln))[[1]]
        if (length(h) == 2L) { cur <- h[2]; r_tab[[cur]] <- list(); next }
        e <- regmatches(ln, regexec('list\\("([a-z_]+)",\\s*(TRUE|FALSE|"[^"]*")', ln))[[1]]
        if (length(e) == 3L && !is.null(cur)) r_tab[[cur]][[e[2]]] <- gsub('"', "", e[3])
    }

    # ---- JS side: "<option>: <value>," rows inside PRESETS ----
    j_start <- grep("^const PRESETS = \\{", j_src)
    j_end   <- grep("^\\};", j_src)[1]
    expect_length(j_start, 1L)
    j_block <- j_src[j_start:j_end]

    js_tab <- list(); cur <- NULL
    for (ln in j_block) {
        h <- regmatches(ln, regexec("^\\s{4}([a-z_]+): \\{", ln))[[1]]
        if (length(h) == 2L) { cur <- h[2]; js_tab[[cur]] <- list(); next }
        e <- regmatches(ln, regexec('^\\s{8}([a-z_]+): (true|false|"[^"]*")', ln))[[1]]
        if (length(e) == 3L && !is.null(cur))
            js_tab[[cur]][[e[2]]] <- gsub('"', "", sub("^true$", "TRUE", sub("^false$", "FALSE", e[3])))
    }

    # both parsers found something (a silent 0-vs-0 match would prove nothing)
    expect_equal(length(r_tab), 6L)
    expect_gt(sum(lengths(r_tab)), 50L)

    expect_setequal(names(r_tab), names(js_tab))
    for (preset in names(r_tab)) {
        expect_setequal(names(r_tab[[preset]]), names(js_tab[[preset]]))
        for (opt in names(r_tab[[preset]]))
            expect_equal(js_tab[[preset]][[opt]], r_tab[[preset]][[opt]],
                         info = paste0(preset, "$", opt))
    }

    # every option a preset writes must exist, and every value must be a legal choice
    a <- yaml::read_yaml(file.path(root, "jamovi", "jjridges.a.yaml"))
    declared <- setNames(lapply(a$options, function(o) o), vapply(a$options, `[[`, "", "name"))
    for (preset in names(r_tab)) for (opt in names(r_tab[[preset]])) {
        expect_true(opt %in% names(declared), info = paste(preset, opt))
        o <- declared[[opt]]
        if (identical(o$type, "List"))
            expect_true(r_tab[[preset]][[opt]] %in% vapply(o$options, `[[`, "", "name"),
                        info = paste(preset, opt))
    }
})


test_that("the preset banner survives the options panel already carrying the values", {
    # With the events JS in place the GUI writes the preset's values into the options, so
    # nothing lands in private$overrides. The banner used to be built FROM the overrides
    # and would simply disappear for every GUI user; it is now built from the preset table.
    d <- rr_data()
    a <- rr_run(d, x_var = "v", y_var = "g", clinicalPreset = "biomarker_distribution")
    expect_true(a$results$warnings$visible)
    expect_match(a$results$warnings$content, "Biomarker Distribution")
    expect_match(a$results$warnings$content, "Cliff's delta effect size", fixed = TRUE)

    # simulate the GUI: the user's options already hold every preset value
    o <- ClinicoPath:::jjridgesOptions$new(
        x_var = "v", y_var = "g", clinicalPreset = "biomarker_distribution",
        plot_type = "density_ridges", add_boxplot = TRUE, add_quantiles = TRUE,
        quantiles = "0.25, 0.5, 0.75", theme_style = "theme_pubr",
        color_palette = "clinical_colorblind", show_stats = TRUE,
        test_type = "nonparametric", effsize_type = "cliff_delta",
        p_adjust_method = "fdr")
    b <- ClinicoPath:::jjridgesClass$new(options = o, data = d)
    b$init(); b$.__enclos_env__$private$.run()

    expect_length(b$.__enclos_env__$private$overrides, 0L)   # nothing left to override
    expect_true(b$results$warnings$visible)                  # banner still shown
    expect_match(b$results$warnings$content, "Biomarker Distribution")

    # and both routes produce the same analysis
    expect_equal(b$results$tests$rowCount, a$results$tests$rowCount)
    expect_equal(as.character(b$results$tests$asDF$method),
                 as.character(a$results$tests$asDF$method))
})


test_that("the clinical preset events handler behaves (node)", {
    # The handler cannot be exercised from R, and a single unresolved control name throws
    # a TypeError that silently skips every control after it -- the exact defect the
    # sibling jjhistostats module shipped with. development-scripts/verify_jjridges_events.js
    # drives the real handler against a stand-in ui object and exits non-zero on failure.
    node <- Sys.which("node")
    skip_if(!nzchar(node), "node is not available")

    script <- testthat::test_path("..", "..", "development-scripts", "verify_jjridges_events.js")
    skip_if_not(file.exists(script), "events verification script is missing")

    out <- suppressWarnings(system2(node, shQuote(normalizePath(script)),
                                    stdout = TRUE, stderr = TRUE))
    status <- attr(out, "status")
    expect_true(is.null(status) || status == 0L,
                info = paste(out, collapse = "\n"))
    expect_true(any(grepl("ALL EVENTS CHECKS PASSED", out)))
})
