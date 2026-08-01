# Regression tests for the split of singlearm's "sc" plot into two output items.
#
# The cumulative-incidence curve used to be a BRANCH inside .plot(), so under
# competing risks jamovi drew a CIF under a panel headed "Survival Plot" -- and
# 1 - S(t) is the one quantity a reader of a competing-risks analysis must not
# confuse the cumulative incidence with. It now has its own item, `plot_cif`,
# rendered by .plotCIF(), and exactly one of the two is ever visible.
#
# The legend was also unreadable: survminer maps cmprsk's raw failure codes onto
# the colour scale, so it read "event  1  2" with nothing on the panel to say
# which of the user's own outcome levels each curve was.

.cif_ns <- NULL
for (.p in c(intersect(c("ClinicoPath", "jsurvival"), loadedNamespaces()),
             "jsurvival", "ClinicoPath")) {
    if (.p %in% loadedNamespaces() || requireNamespace(.p, quietly = TRUE)) {
        .cand <- asNamespace(.p)
        if (exists("singlearmClass", envir = .cand, inherits = FALSE)) {
            .cif_ns <- .cand
            break
        }
    }
}
skip_if(is.null(.cif_ns), "singlearm not available in this distribution")

# The wrapper does NSE and `type: Level` options cannot carry defaults, so the
# analysis is built directly -- that also gives access to the render methods.
run_sa <- function(d, opts) {
    base <- list(elapsedtime = "time", tint = FALSE, sc = TRUE,
                 timetypeoutput = "months", endplot = 60, byplot = 12,
                 dod = NULL, dooc = NULL, awd = NULL, awod = NULL)
    o <- do.call(get("singlearmOptions", envir = .cif_ns)$new,
                 utils::modifyList(base, opts))
    a <- get("singlearmClass", envir = .cif_ns)$new(options = o, data = d)
    sink(tempfile()); on.exit(sink(), add = TRUE)
    suppressWarnings(a$run())
    a
}

cr_data <- function(n = 120, seed = 7) {
    set.seed(seed)
    tt <- rexp(n, 0.05); cc <- rexp(n, 0.03)
    data.frame(
        time = pmin(tt, cc),
        oc = factor(ifelse(tt <= cc,
                           sample(c("Dead of disease", "Dead of other cause"),
                                  n, TRUE, c(0.7, 0.3)),
                           "Alive with disease"),
                    levels = c("Alive with disease", "Alive without disease",
                               "Dead of disease", "Dead of other cause")))
}

cr_opts <- list(outcome = "oc", outcomeLevel = NULL,
                multievent = TRUE, analysistype = "compete",
                dod = "Dead of disease", dooc = "Dead of other cause",
                awd = "Alive with disease", awod = "Alive without disease")


test_that("competing-risk mode shows the CIF item and hides the survival plot", {
    skip_if_not_installed("cmprsk")
    a <- run_sa(cr_data(), cr_opts)

    expect_false(a$results$plot$visible)
    expect_true(a$results$plot_cif$visible)
    expect_false(is.null(a$results$plot_cif$state))
})


test_that("ordinary survival shows the survival plot and hides the CIF item", {
    set.seed(11); n <- 150
    tt <- rexp(n, 0.08); cc <- rexp(n, 0.02)
    d <- data.frame(time = pmin(tt, cc),
                    oc = factor(ifelse(tt <= cc, "Dead", "Alive")))
    a <- run_sa(d, list(outcome = "oc", outcomeLevel = "Dead"))

    expect_true(a$results$plot$visible)
    expect_false(a$results$plot_cif$visible)

    # And .plotCIF() refuses to draw even if a stale .omv reaches the renderer.
    priv <- a$.__enclos_env__$private
    expect_null(priv$.plotCIF(a$results$plot_cif, ggplot2::theme_bw(), NULL))
})


test_that("a 0/1/2 hand-off still gets the CIF although multievent is FALSE", {
    # visible: expressions can only see OPTIONS. An outcomeorganizer hand-off
    # delivers a pre-recoded Censored/Event/Competing column with
    # multievent = FALSE, so .run() has to re-assert both visibilities from the
    # recode -- otherwise these users get a refusal panel instead of their plot.
    skip_if_not_installed("cmprsk")
    d <- data.frame(
        time = c(10, 20, 30, 40, 50, 60),
        out = factor(c("Event", "Event", "Competing", "Competing",
                       "Censored", "Censored"),
                     levels = c("Censored", "Event", "Competing")))
    a <- run_sa(d, list(outcome = "out", outcomeLevel = "Event",
                        multievent = FALSE, analysistype = "overall"))

    expect_false(a$results$plot$visible)
    expect_true(a$results$plot_cif$visible)
    expect_identical(a$results$plot_cif$state$event_label, "Event")
})


test_that("the CIF legend names the events and the stray facet strip is gone", {
    skip_if_not_installed("cmprsk")
    a <- run_sa(cr_data(), cr_opts)
    priv <- a$.__enclos_env__$private

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    expect_true(priv$.plotCIF(a$results$plot_cif, ggplot2::theme_bw(), NULL))

    p <- ggplot2::last_plot()
    keys <- as.character(p$data$event)

    # Was: "event   1   2".
    expect_false(any(keys %in% c("1", "2")))
    expect_setequal(unique(keys), c("Dead of disease", "Dead of other cause"))
    expect_identical(p$labels$colour, "Event type")

    # Was: a strip labelled "1" -- ggcompetingrisks facets by group and a
    # single-arm cohort is one group.
    expect_true(inherits(p$facet, "FacetNull"))

    # Was: "Probability of an event".
    expect_identical(p$labels$y, "Cumulative incidence")
})

test_that("a zero target-event CIF remains visible as a flat zero curve", {
    skip_if_not_installed("cmprsk")
    d <- data.frame(
        time = seq(2, 40, by = 2),
        oc = factor(
            rep(c("Dead of other cause", "Alive with disease"), each = 10),
            levels = c("Alive with disease", "Alive without disease",
                       "Dead of disease", "Dead of other cause")))
    a <- run_sa(d, cr_opts)
    priv <- a$.__enclos_env__$private

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    expect_true(priv$.plotCIF(a$results$plot_cif, ggplot2::theme_bw(), NULL))

    p <- ggplot2::last_plot()
    expect_setequal(unique(as.character(p$data$event)),
                    c("Dead of disease", "Dead of other cause"))
    target <- p$data[p$data$event == "Dead of disease", , drop = FALSE]
    expect_gt(nrow(target), 0)
    expect_true(all(target$est == 0))
    uncertainty <- intersect(
        c("var", "std", "lower", "upper", "conf.low", "conf.high"),
        names(target))
    for (column in uncertainty)
        expect_true(all(is.na(target[[column]]) | target[[column]] == 0),
                    info = paste("flat-zero CIF must not inherit", column,
                                 "from another event curve"))
})
