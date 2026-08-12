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
