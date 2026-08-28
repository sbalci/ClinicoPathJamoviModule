# Residual-defect regression tests for `jjscatterstats`.
#
# Every expectation asserts what the USER SEES -- the text of the subtitle/label actually
# carried by the rendered plot object, or the text left in the Warnings panel -- not the
# internal mechanism that produces it.

rs_run <- function(dat, ...) {
    o <- do.call(ClinicoPath:::jjscatterstatsOptions$new,
                 utils::modifyList(list(dep = "x", group = "y"), list(...)))
    a <- ClinicoPath:::jjscatterstatsClass$new(options = o, data = dat)
    a$init()
    tryCatch(a$.__enclos_env__$private$.run(), error = function(e) NULL)
    a
}
rs_warn <- function(a) {
    v <- tryCatch(a$results$warnings$content, error = function(e) "")
    if (is.null(v)) "" else v
}
# Render on a real device AND keep the object that was printed: R6 methods resolve
# `print` lexically through the object's enclosing environment, so a binding placed
# there shadows base::print for this one analysis object. Building the returned plot
# is what proves a panel is not silently empty.
rs_capture <- function(a, method, item) {
    captured <- NULL
    assign("print", function(x, ...) { captured <<- x; base::print(x, ...) },
           envir = a$.__enclos_env__)
    f <- tempfile(fileext = ".png")
    grDevices::png(f, 800, 600)
    err <- tryCatch({ a$.__enclos_env__$private[[method]](item); NULL },
                    error = function(e) conditionMessage(e))
    grDevices::dev.off()
    if (exists("print", envir = a$.__enclos_env__, inherits = FALSE))
        rm("print", envir = a$.__enclos_env__)
    list(err = err, plot = captured, md5 = unname(tools::md5sum(f)))
}
rs_subtitle <- function(p) paste(deparse(p$labels$subtitle), collapse = "")
rs_labels <- function(p) {
    b <- ggplot2::ggplot_build(p)
    paste(unlist(lapply(b$data, function(x)
        if ("label" %in% names(x)) as.character(x$label))), collapse = " | ")
}
rs_xy <- function(seed = 42, n = 80) {
    set.seed(seed)
    d <- data.frame(x = rnorm(n))
    d$y <- 0.6 * d$x + rnorm(n, 0, 0.9)
    d$g <- factor(rep(c("A", "B"), length.out = n))
    d
}


test_that("an infinite value is removed and reported instead of silently voiding the statistics", {
    # Inf passes BOTH is.na() and stats::complete.cases(), so one Inf used to print
    # r = NA, p = NA beside "n = 79" on the main panel, the literal "r = NaN" on the
    # enhanced panel, and made type = "bayes" abort with "No valid values found".
    d <- rs_xy(); d$x[1] <- Inf
    fin <- is.finite(d$x) & is.finite(d$y)
    ref <- stats::cor.test(d$x[fin], d$y[fin])

    a <- rs_run(d, resultssubtitle = TRUE)
    expect_match(rs_warn(a), "Non-finite values removed")
    expect_true(a$results$warnings$visible)

    cp <- rs_capture(a, ".plot", a$results$plot)
    expect_null(cp$err)
    sub <- rs_subtitle(cp$plot)
    expect_match(sub, "widehat")                       # a real subtitle was produced
    expect_false(grepl('== "NA"', sub, fixed = TRUE))  # with numbers, not NA
    expect_match(sub, as.character(sum(fin)))          # on the finite pairs only
    expect_match(sub, base::format(round(unname(ref$estimate), 2)), fixed = TRUE)

    # the enhanced panel quotes the same coefficient instead of "r = NaN"
    a3 <- rs_run(d, colorvar = "g")
    expect_match(rs_subtitle(rs_capture(a3, ".plot3", a3$results$plot3)$plot),
                 paste0("n = ", sum(fin)), fixed = TRUE)

    # the ggpubr panel plots the finite rows and reports a coefficient for them
    ap <- rs_run(d, addGGPubrPlot = TRUE, ggpubrAddCorr = TRUE)
    pp <- rs_capture(ap, ".plotGGPubr", ap$results$ggpubrPlot)$plot
    expect_equal(nrow(ggplot2::ggplot_build(pp)$data[[1]]), sum(fin))

    # the Bayesian branch no longer errors out
    ab <- rs_run(d, typestatistics = "bayes", resultssubtitle = TRUE)
    expect_null(rs_capture(ab, ".plot", ab$results$plot)$err)

    # clean data stays quiet
    expect_false(grepl("Non-finite", rs_warn(rs_run(rs_xy())), fixed = TRUE))
})


test_that(".plot3 no longer wipes the Warnings panel written by .run()", {
    d <- rs_xy(); d$const <- 5
    a <- rs_run(d, dep = "const", grvar = "g", colorvar = "g",
                typestatistics = "robust", resultssubtitle = TRUE)
    expect_match(rs_warn(a), "One test per group")
    expect_match(rs_warn(a), "'const'", fixed = TRUE)

    rs_capture(a, ".plot3", a$results$plot3)
    after <- rs_warn(a)
    expect_match(after, "One test per group")     # multiplicity note survived the render
    expect_match(after, "'const'", fixed = TRUE)  # and the note NAMING the constant column
})


test_that("the ggpubr panel labels its coefficient with the symbol of the method it ran", {
    d <- rs_xy()
    label_of <- function(...) {
        a <- rs_run(d, addGGPubrPlot = TRUE, ggpubrAddCorr = TRUE, ...)
        rs_labels(rs_capture(a, ".plotGGPubr", a$results$ggpubrPlot)$plot)
    }
    expect_match(label_of(ggpubrCorrMethod = "spearman", typestatistics = "nonparametric"),
                 "italic(rho)", fixed = TRUE)
    expect_match(label_of(ggpubrCorrMethod = "pearson", typestatistics = "parametric"),
                 "italic(R)", fixed = TRUE)
})


test_that("robust/Bayesian analyses disclose that the ggpubr panel shows a different statistic", {
    d <- rs_xy()
    warn_after_render <- function(ty) {
        a <- rs_run(d, addGGPubrPlot = TRUE, ggpubrAddCorr = TRUE, typestatistics = ty)
        rs_capture(a, ".plotGGPubr", a$results$ggpubrPlot)
        rs_warn(a)
    }
    expect_match(warn_after_render("robust"), "not showing your analysis type", fixed = TRUE)
    expect_match(warn_after_render("bayes"),  "not showing your analysis type", fixed = TRUE)
    expect_false(grepl("not showing your analysis type",
                       warn_after_render("parametric"), fixed = TRUE))
})


test_that("an ordinal x variable still yields a coefficient in the ggpubr panel", {
    # jamovi's `permitted: [numeric]` accepts ordinal columns; they arrive as factors and
    # the ggpubr paths were the only render paths skipping jmvcore::toNumeric, so the
    # coefficient label evaluated to NA and the axis came out discrete.
    set.seed(7)
    d <- data.frame(x = factor(sample(1:4, 100, TRUE), ordered = TRUE))
    attr(d$x, "values") <- 1:4
    d$y <- 0.5 * as.numeric(as.character(d$x)) + rnorm(100)
    a <- rs_run(d, addGGPubrPlot = TRUE, ggpubrAddCorr = TRUE)
    p <- rs_capture(a, ".plotGGPubr", a$results$ggpubrPlot)$plot

    lab <- rs_labels(p)
    expect_match(lab, "italic(R)", fixed = TRUE)
    expect_false(grepl("NA", lab, fixed = TRUE))
    ref <- round(unname(stats::cor.test(as.numeric(as.character(d$x)), d$y)$estimate), 2)
    expect_match(lab, base::format(ref), fixed = TRUE)

    b <- ggplot2::ggplot_build(p)
    expect_s3_class(b$layout$panel_scales_x[[1]], "ScaleContinuousPosition")
    expect_equal(nrow(b$data[[1]]), nrow(d))
})


test_that("the Title option reaches the grouped plot", {
    # `title.prefix` is not a formal of grouped_ggscatterstats; it was swallowed by `...`,
    # so the Title option was inert and the grouped figure carried no title at all.
    d <- rs_xy()
    a_def <- rs_run(d, grvar = "g")
    a_cus <- rs_run(d, grvar = "g", mytitle = "ZZZ_MY_CUSTOM_TITLE_ZZZ")
    r_def <- rs_capture(a_def, ".plot2", a_def$results$plot2)
    r_cus <- rs_capture(a_cus, ".plot2", a_cus$results$plot2)
    expect_null(r_def$err); expect_null(r_cus$err)
    expect_identical(r_def$plot$patches$annotation$title, "x vs y by g")
    expect_identical(r_cus$plot$patches$annotation$title, "ZZZ_MY_CUSTOM_TITLE_ZZZ")
    expect_false(identical(r_def$md5, r_cus$md5))   # and the rendered figure differs
})
