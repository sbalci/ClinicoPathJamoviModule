# Release review 2026-09-25: agreement heatmap, specific-agreement guide,
# agreement by subgroup, rater and case clustering. One test_that per finding.
# Oracles are base R (table(), mean(), stats::cor, stats::kmeans on complete
# cases, hand formulas) or, for the plots, the pixels actually drawn.

skip_if_not_installed("png")

.plots_private <- function() {
    ns <- asNamespace("ClinicoPath")
    d <- data.frame(R1 = factor(c("A", "B")), R2 = factor(c("A", "B")))
    a <- ns$agreementClass$new(options = ns$agreementOptions$new(vars = c("R1", "R2")), data = d)
    a$.__enclos_env__$private
}

# Render `fun` from `state` on a png device; return the RGB (0-255) at the centre
# of every cell of an n x n grid, indexed [screen row from the TOP, column].
# The plot region is measured on a fresh device with the renderer's margins.
.cell_colours <- function(fun, state, n, mar, w = 480, h = 480) {
    f <- tempfile(fileext = ".png")
    grDevices::png(f, width = w, height = h)
    ok <- fun(image = list(state = state), ggtheme = NULL, theme = NULL)
    grDevices::dev.off()
    g <- tempfile(fileext = ".png")
    grDevices::png(g, width = w, height = h)
    graphics::par(mfrow = c(1, 1), mar = mar)
    plt <- graphics::par("plt")
    grDevices::dev.off()
    img <- png::readPNG(f)
    out <- array(NA_real_, c(n, n, 3))
    for (r in seq_len(n)) for (i in seq_len(n)) {
        x <- plt[1] + (i - 0.5) / n * (plt[2] - plt[1])
        y <- plt[4] - (r - 0.5) / n * (plt[4] - plt[3])
        out[r, i, ] <- round(255 * img[ceiling((1 - y) * h), ceiling(x * w), 1:3])
    }
    list(ok = ok, rgb = out)
}

.heat_state <- function(counts) {
    list(tables = list(counts), pair_names = list(c("R1", "R2")), total_pairs = 1L,
         color_scheme = "bluered", show_pct = FALSE, show_count = FALSE, annot_size = 3.5)
}

same_partition <- function(a, b) {
    ok <- !is.na(a) & !is.na(b)
    identical(is.na(a), is.na(b)) &&
        length(unique(paste(a[ok], b[ok]))) == length(unique(a[ok])) &&
        length(unique(a[ok])) == length(unique(b[ok]))
}

test_that("L326 agreement heatmap draws Rater 1's first category at the top, as in the contingency table", {
    priv <- .plots_private()
    # Every case in cell (A, A): row 1, column 1 of the table.
    tab <- matrix(c(10L, 0L, 0L, 0L), 2, dimnames = list(c("A", "B"), c("A", "B")))
    px <- .cell_colours(priv$.agreementHeatmap, .heat_state(tab), n = 2, mar = c(5, 5, 3, 2))
    expect_true(px$ok)
    top_left <- px$rgb[1, 1, ]
    bottom_left <- px$rgb[2, 1, ]
    # The full cell (100%) is the red end of the blue-red ramp, the empty one blue.
    expect_gt(top_left[1], top_left[3])
    expect_gt(bottom_left[3], bottom_left[1])

    d <- data.frame(R1 = factor(rep(c("A", "B"), 10)), R2 = factor(rep(c("A", "B"), 10)))
    a <- agreement_run(d, vars = c("R1", "R2"), showAgreementHeatmapGuide = TRUE)
    html <- a$results$agreementHeatmapExplanation$content
    expect_true(grepl("first category is at the top left", html, fixed = TRUE))
})

test_that("L395 agreement heatmap colours cells on a fixed 0-100% scale and the guide describes it truthfully", {
    priv <- .plots_private()
    # Largest cell holds 40% of the cases. On a fixed 0-100 scale whose white
    # midpoint is 50%, every cell must be on the blue side (blue >= red); over the
    # observed range the 40% cell was painted the saturated red end.
    tab <- matrix(c(4L, 1L, 1L, 4L), 2, dimnames = list(c("A", "B"), c("A", "B")))
    expect_equal(max(prop.table(tab)) * 100, 40)
    px <- .cell_colours(priv$.agreementHeatmap, .heat_state(tab), n = 2, mar = c(5, 5, 3, 2))
    for (r in 1:2) for (i in 1:2) expect_gte(px$rgb[r, i, 3], px$rgb[r, i, 1])

    d <- data.frame(R1 = factor(rep(c("A", "B"), 10)), R2 = factor(rep(c("A", "B"), 10)))
    a <- agreement_run(d, vars = c("R1", "R2"), showAgreementHeatmapGuide = TRUE)
    html <- a$results$agreementHeatmapExplanation$content
    expect_false(grepl("Darker colors indicate higher frequency", html, fixed = TRUE))
    expect_false(grepl("Green=agree", html, fixed = TRUE))
    expect_true(grepl("fixed 0-100% scale", html, fixed = TRUE))
})

test_that("L325 specific-agreement guide attributes a PSA/NSA gap to prevalence, not diagnostic bias", {
    d <- data.frame(R1 = factor(rep(c("A", "B"), 10)), R2 = factor(rep(c("A", "B"), 10)))
    a <- agreement_run(d, vars = c("R1", "R2"), showSpecificAgreementGuide = TRUE)
    html <- a$results$specificAgreementExplanation$content
    expect_true(nzchar(html))
    expect_false(grepl("over-diagnosis", html, fixed = TRUE))
    expect_false(grepl("under-diagnosis", html, fixed = TRUE))
    expect_false(grepl("Detect diagnostic bias", html, fixed = TRUE))
    expect_true(grepl("mainly reflects prevalence", html, fixed = TRUE))
    expect_true(grepl("McNemar", html, fixed = TRUE))
})

test_that("L329 rater clustering runs when one rater never used a category", {
    set.seed(2); lv <- c("G1", "G2", "G3"); b <- sample(lv, 40, TRUE)
    d <- data.frame(R1 = factor(b, lv),
                    R2 = factor(ifelse(runif(40) < .8, b, sample(lv, 40, TRUE)), lv),
                    R3 = factor(ifelse(b == "G3", "G2", b)))
    a <- agreement_run(d, vars = c("R1", "R2", "R3"), raterClustering = TRUE, nClusters = 2)
    tab <- a$results$raterClusterTable$asDF
    expect_equal(nrow(tab), 3L)
    expect_false("error" %in% names(agreement_notes(a$results$raterClusterTable)))
    # Oracle: pairwise proportion of identical labels.
    agree <- function(x, y) mean(as.character(x) == as.character(y))
    for (i in seq_len(3)) {
        mates <- setdiff(which(tab$cluster == tab$cluster[i]), i)
        if (length(mates) == 0) next
        expect_equal(tab$avg_similarity[i],
                     mean(vapply(mates, function(j) agree(d[[i]], d[[j]]), numeric(1))),
                     tolerance = 1e-12)
    }
})

test_that("L332 k-means rater and case clustering survive a missing rating", {
    set.seed(4); x <- rnorm(30, 50, 10)
    d <- data.frame(R1 = x, R2 = x + rnorm(30, 0, 3), R3 = x + rnorm(30, 0, 3), R4 = x + 5 + rnorm(30, 0, 3))
    d$R2[5] <- NA
    cc <- stats::complete.cases(d)

    a <- agreement_run(d, vars = names(d), raterClustering = TRUE, clusterMethod = "kmeans", nClusters = 2)
    tab <- a$results$raterClusterTable$asDF
    expect_equal(nrow(tab), 4L)
    set.seed(42)
    oracle <- stats::kmeans(t(as.matrix(d[cc, ])), centers = 2, nstart = 25)$cluster
    expect_true(same_partition(as.integer(tab$cluster), unname(oracle)))
    notes <- agreement_notes(a$results$raterClusterTable)
    expect_true(grepl("1 case(s) with a missing rating", notes[["kmeans_missing"]], fixed = TRUE))

    a <- agreement_run(d, vars = names(d), caseClustering = TRUE, caseClusterMethod = "kmeans",
                       nCaseClusters = 2, showCaseClusterHeatmap = TRUE)
    tab <- a$results$caseClusterTable$asDF
    expect_equal(nrow(tab), 30L)
    set.seed(42)
    oracle <- rep(NA_integer_, 30)
    oracle[cc] <- stats::kmeans(as.matrix(d[cc, ]), centers = 2, nstart = 25)$cluster
    expect_true(is.na(tab$cluster[5]))
    expect_true(same_partition(suppressWarnings(as.integer(tab$cluster)), oracle))
    notes <- agreement_notes(a$results$caseClusterTable)
    expect_true("kmeans_missing" %in% names(notes))
    # The heatmap renderer takes the NA label (order() puts it last).
    priv <- a$.__enclos_env__$private
    f <- tempfile(fileext = ".png"); grDevices::png(f)
    ok <- priv$.caseClusterHeatmap(image = list(state = a$results$caseClusterHeatmap$state),
                                   ggtheme = NULL, theme = NULL)
    grDevices::dev.off()
    expect_true(ok)
})

test_that("L331 correlation distance with a constant rater does not stop hclust", {
    set.seed(4); x <- rnorm(30, 50, 10)
    d <- data.frame(R1 = x, R2 = x + rnorm(30, 0, 3), R3 = x + rnorm(30, 0, 3), R4 = rep(50, 30))
    a <- agreement_run(d, vars = names(d), raterClustering = TRUE,
                       clusterDistance = "correlation", nClusters = 2)
    tab <- a$results$raterClusterTable$asDF
    expect_equal(nrow(tab), 4L)
    notes <- agreement_notes(a$results$raterClusterTable)
    expect_false("error" %in% names(notes))
    expect_true(grepl("^3 rater pair", notes[["undefined_cor"]]))
    # Oracle: R1-R3 share a cluster and their similarity is the mean correlation.
    r <- stats::cor(d[, 1:3])
    expect_equal(length(unique(tab$cluster[1:3])), 1L)
    expect_equal(tab$avg_similarity[1], mean(r[1, 2:3]), tolerance = 1e-12)
})

test_that("L403 clustering refuses a mix of continuous and categorical raters and names them", {
    set.seed(4); x <- rnorm(30, 50, 10)
    lv <- c("L", "M", "H")
    d <- data.frame(R1 = x,
                    R2 = factor(sample(lv, 30, TRUE), levels = lv, ordered = TRUE),
                    R3 = factor(sample(lv, 30, TRUE), levels = lv, ordered = TRUE))
    expect_equal(sum(stats::complete.cases(d)), 30L)
    a <- agreement_run(d, vars = names(d), raterClustering = TRUE, nClusters = 2)
    expect_equal(nrow(a$results$raterClusterTable$asDF), 0L)
    notes <- agreement_notes(a$results$raterClusterTable)
    expect_false(any(grepl("fewer than 3 complete cases", notes, fixed = TRUE)))
    expect_true(grepl("R1", notes[["error"]], fixed = TRUE))

    a <- agreement_run(d, vars = names(d), caseClustering = TRUE, nCaseClusters = 2)
    expect_equal(nrow(a$results$caseClusterTable$asDF), 0L)
    notes <- agreement_notes(a$results$caseClusterTable)
    expect_true(grepl("continuous measurements", notes[["error"]], fixed = TRUE))
})

test_that("L402 rater and case cluster heatmaps colour similarity on a fixed -1..1 scale", {
    priv <- .plots_private()
    # Every similarity is >= 0.5, so on a -1..1 scale with white at 0 every cell
    # is on the red side (red >= blue); over the observed range 0.5 was the
    # saturated blue end.
    m <- matrix(c(1, 0.9, 0.5, 0.9, 1, 0.5, 0.5, 0.5, 1), 3)
    px <- .cell_colours(priv$.raterClusterHeatmap,
                        list(similarity_matrix = m, cluster_assignments = c(1L, 1L, 2L),
                             rater_names = c("A", "B", "C")),
                        n = 3, mar = c(8, 8, 4, 2))
    expect_true(px$ok)
    for (r in 1:3) for (i in 1:3) expect_gte(px$rgb[r, i, 1], px$rgb[r, i, 3])
    px <- .cell_colours(priv$.caseClusterHeatmap,
                        list(similarity_matrix = m, cluster_assignments = c(1L, 1L, 2L),
                             case_ids = c("1", "2", "3")),
                        n = 3, mar = c(8, 8, 4, 2))
    expect_true(px$ok)
    for (r in 1:3) for (i in 1:3) expect_gte(px$rgb[r, i, 1], px$rgb[r, i, 3])

    d <- data.frame(R1 = rnorm(10), R2 = rnorm(10), R3 = rnorm(10))
    a <- agreement_run(d, vars = names(d), showRaterClusterGuide = TRUE, showCaseClusterGuide = TRUE)
    expect_false(grepl("Dark diagonal blocks", a$results$raterClusterExplanation$content, fixed = TRUE))
    expect_true(grepl("fixed scale", a$results$caseClusterExplanation$content, fixed = TRUE))
})

test_that("L404 k-means notes say the distance metric does not drive the clusters", {
    set.seed(4); x <- rnorm(30, 50, 10)
    d <- data.frame(R1 = x, R2 = x + rnorm(30, 0, 3), R3 = x + rnorm(30, 0, 3), R4 = x + 5 + rnorm(30, 0, 3))
    a <- agreement_run(d, vars = names(d), raterClustering = TRUE, clusterMethod = "kmeans",
                       clusterDistance = "agreement", nClusters = 2)
    notes <- agreement_notes(a$results$raterClusterTable)
    expect_true("kmeans_metric" %in% names(notes))
    expect_false(grepl("Use the Euclidean", notes[["exact_match"]], fixed = TRUE))
    expect_false(grepl("\\s$", notes[["exact_match"]]))

    a <- agreement_run(d, vars = names(d), caseClustering = TRUE, caseClusterMethod = "kmeans",
                       caseClusterDistance = "correlation", nCaseClusters = 2)
    notes <- agreement_notes(a$results$caseClusterTable)
    expect_true("kmeans_metric" %in% names(notes))
    expect_false("offset_blind" %in% names(notes))
})

test_that("L406 a one-member cluster's blank similarity is explained", {
    set.seed(4); x <- rnorm(30, 50, 10)
    d <- data.frame(R1 = x, R2 = x + rnorm(30, 0, 3), R3 = x + rnorm(30, 0, 3), R4 = x + 40 + rnorm(30, 0, 3))
    a <- agreement_run(d, vars = names(d), raterClustering = TRUE, nClusters = 2)
    tab <- a$results$raterClusterTable$asDF
    sizes <- table(tab$cluster)
    expect_true(any(sizes == 1))
    lone <- tab$cluster %in% names(sizes)[sizes == 1]
    expect_true(all(is.na(tab$avg_similarity[lone])))
    expect_true("singleton" %in% names(agreement_notes(a$results$raterClusterTable)))

    d2 <- d[, 1:3]
    d2[30, ] <- d2[30, ] + 500
    a <- agreement_run(d2, vars = names(d2), caseClustering = TRUE, nCaseClusters = 2)
    tab <- a$results$caseClusterTable$asDF
    expect_equal(sum(tab$cluster == tab$cluster[30]), 1L)
    expect_true(is.na(tab$avg_similarity[30]))
    expect_true("singleton" %in% names(agreement_notes(a$results$caseClusterTable)))
})

test_that("L400 a single-category subgroup shows NA and 'Not estimable' with a note (2 and 3 raters)", {
    set.seed(1)
    r1 <- c(sample(c("Benign", "Malignant"), 20, TRUE), rep("Benign", 20))
    r2 <- c(ifelse(runif(20) < .8, r1[1:20], sample(c("Benign", "Malignant"), 20, TRUE)), rep("Benign", 20))
    d <- data.frame(R1 = factor(r1), R2 = factor(r2), site = factor(rep(c("S1", "S2"), each = 20)))
    # Oracle: S2 uses one category, so chance agreement is 1 and kappa is 0/0.
    expect_equal(length(unique(c(r1[21:40], r2[21:40]))), 1L)
    t1 <- table(factor(r1[1:20], c("Benign", "Malignant")), factor(r2[1:20], c("Benign", "Malignant")))
    po <- sum(diag(t1)) / 20
    pe <- sum(rowSums(t1) * colSums(t1)) / 20^2
    for (vars in list(c("R1", "R2"), c("R1", "R2", "R3"))) {
        d$R3 <- d$R2
        a <- agreement_run(d, vars = vars, agreementBySubgroup = TRUE,
                           subgroupVariable = "site", subgroupMinCases = 5)
        tab <- a$results$subgroupAgreementTable$asDF
        s2 <- tab[tab$subgroup == "S2", ]
        expect_true(is.na(s2$agreement_stat))
        expect_false(is.nan(s2$agreement_stat))
        expect_equal(s2$interpretation, "Not estimable")
        notes <- agreement_notes(a$results$subgroupAgreementTable)
        expect_true(grepl("S2", notes[["one_category"]], fixed = TRUE))
        if (length(vars) == 2)
            expect_equal(tab$agreement_stat[tab$subgroup == "S1"], (po - pe) / (1 - pe), tolerance = 1e-10)
    }
})

test_that("L399 ticking agreement by subgroup without a subgroup variable explains the empty table", {
    d <- data.frame(R1 = factor(rep(c("A", "B"), 10)), R2 = factor(rep(c("A", "B"), 10)))
    a <- agreement_run(d, vars = c("R1", "R2"), agreementBySubgroup = TRUE)
    expect_equal(nrow(a$results$subgroupAgreementTable$asDF), 0L)
    notes <- agreement_notes(a$results$subgroupAgreementTable)
    expect_true(grepl("Select a subgroup variable", notes[["select"]], fixed = TRUE))
})
