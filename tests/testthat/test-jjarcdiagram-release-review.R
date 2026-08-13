# Release review of jjarcdiagram.
#
# The plot is decoration; the risk lives in the network statistics panel, which
# names a "most connected entity" and a "most central" node. Those come from
# igraph shortest-path centrality, and igraph reads edge weights as DISTANCES -
# so how a user's "connection strength" is converted into a distance decides
# which node is called the hub.

arc_txt <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x %||% "")))
arc_notices <- function(res) arc_txt(paste(as.character(res$notices$content), collapse = " "))
arc_stats <- function(res) arc_txt(res$networkStats$content)

# A 7-node network with a deliberate spread of weights.
arc_net <- function(w = c(10, 10, 7, 1, 4, 6, 2, 9, 3, 5)) {
    data.frame(from = factor(c("A","B","C","D","E","F","A","C","B","E")),
               to   = factor(c("B","C","D","E","F","G","D","F","E","G")),
               w    = w)
}


# ---- weights that used to crash the analysis --------------------------------

test_that("negative edge weights are rejected with an actionable message", {
    # igraph aborts at the C level - "Weight vector must be positive. Invalid
    # value / centrality/betweenness.c:437" - taking the whole analysis with it.
    # Negative weights are not exotic: this option is documented for correlations
    # and fold-changes, both routinely negative.
    d <- arc_net(c(0.9, 0.8, -0.7, 0.6, 0.5, -0.4, 0.3, 0.2, 0.1, 0.35))
    for (mode in c("strength", "distance")) {
        expect_no_error(jjarcdiagram(data = d, source = "from", target = "to",
                                     weight = "w", showStats = TRUE, weightMode = mode))
        n <- arc_notices(jjarcdiagram(data = d, source = "from", target = "to",
                                      weight = "w", showStats = TRUE, weightMode = mode))
        expect_match(n, "Negative Edge Weights", info = mode)
        expect_match(n, "2 of 10 edge weights", info = mode)
        expect_match(n, "absolute value", info = mode)   # tells the user what to do
    }
})

test_that("a zero minimum weight no longer breaks Strength mode", {
    # The old transform mapped the STRONGEST edge to distance min(w), so any
    # weight column containing a 0 produced a zero-length edge and crashed.
    d <- arc_net(c(0, 5, 10, 3, 2, 4, 6, 8, 7, 1))
    expect_no_error(jjarcdiagram(data = d, source = "from", target = "to",
                                 weight = "w", showStats = TRUE, weightMode = "strength"))
    res <- jjarcdiagram(data = d, source = "from", target = "to",
                        weight = "w", showStats = TRUE, weightMode = "strength")
    expect_false(grepl("Negative Edge Weights", arc_notices(res), fixed = TRUE))
    expect_match(arc_stats(res), "Number of Nodes")
})

test_that("a zero weight in Distance mode is rejected, not silently run", {
    # A zero-length edge means two nodes occupy the same point, which is undefined.
    d <- arc_net(c(0, 5, 10, 3, 2, 4, 6, 8, 7, 1))
    n <- arc_notices(jjarcdiagram(data = d, source = "from", target = "to",
                                  weight = "w", showStats = TRUE, weightMode = "distance"))
    expect_match(n, "Zero Edge Distances")
    expect_match(n, "1 of 10")
    expect_match(n, "switch Edge Weight Interpretation to Strength")
})

test_that("ordinary positive weights run in both modes", {
    d <- arc_net()
    for (mode in c("strength", "distance")) {
        n <- arc_notices(jjarcdiagram(data = d, source = "from", target = "to",
                                      weight = "w", showStats = TRUE, weightMode = mode))
        expect_false(grepl("Negative Edge Weights", n, fixed = TRUE), info = mode)
        expect_false(grepl("Zero Edge Distances", n, fixed = TRUE), info = mode)
    }
})


# ---- the strength-to-distance conversion ------------------------------------

test_that("Strength mode uses the reciprocal, matching igraph directly", {
    d <- arc_net()
    g <- igraph::graph_from_data_frame(d[, c("from", "to")], directed = FALSE)
    igraph::E(g)$weight <- d$w

    btw <- igraph::betweenness(g, weights = 1 / igraph::E(g)$weight)
    str_ <- igraph::strength(g, weights = igraph::E(g)$weight)

    t <- arc_stats(jjarcdiagram(data = d, source = "from", target = "to",
                                weight = "w", showStats = TRUE, weightMode = "strength"))
    expect_match(t, sprintf("Highest Betweenness: %s", names(which.max(btw))))
    expect_match(t, sprintf("Highest Degree: %s", names(which.max(str_))))
    expect_match(t, as.character(round(max(btw), 0)))
})

test_that("the reciprocal and the old reflection genuinely disagree", {
    # This is why the conversion was changed rather than left alone. The old
    # transform, max(w) - w + min(w), charges a fixed toll per edge, so it favours
    # few hops over strong ties. On these weights it routes S->T through M and
    # calls M the most central node; the reciprocal routes S->T directly and calls
    # T the most central. If this test ever fails, the two agree on this fixture
    # and a new discriminating case is needed - do not simply delete it.
    el <- rbind(c("S","M"), c("M","T"), c("S","T"), c("T","Z"))
    w  <- c(10, 10, 7, 1)
    g  <- igraph::graph_from_edgelist(el, directed = FALSE)
    igraph::E(g)$weight <- w

    reflection <- igraph::betweenness(g, weights = max(w) - w + min(w))
    reciprocal <- igraph::betweenness(g, weights = 1 / w)

    expect_false(identical(unname(reflection), unname(reciprocal)))
    expect_equal(names(which.max(reflection)), "M")
    expect_equal(names(which.max(reciprocal)), "T")
})

test_that("density matches igraph::edge_density", {
    d <- arc_net()
    g <- igraph::graph_from_data_frame(d[, c("from", "to")], directed = FALSE)
    t <- arc_stats(jjarcdiagram(data = d, source = "from", target = "to",
                                weight = "w", showStats = TRUE))
    expect_match(t, sprintf("Network Density: %s", round(igraph::edge_density(g), 4)))
    expect_match(t, sprintf("Number of Nodes: %d", igraph::vcount(g)))
    expect_match(t, sprintf("Number of Edges: %d", igraph::ecount(g)))
})


# ---- exclusions must be visible ---------------------------------------------

test_that("rows dropped for missing values are disclosed", {
    # Dropped rows are dropped EDGES, and density, degree, betweenness and the
    # "most central" claim are all computed on what survives. Only the
    # all-rows-dropped case used to say anything.
    d <- arc_net(); d$w[c(2, 5)] <- NA
    n <- arc_notices(jjarcdiagram(data = d, source = "from", target = "to",
                                  weight = "w", showStats = TRUE))
    expect_match(n, "Rows Excluded")
    expect_match(n, "2 of 10 rows")
    expect_match(n, "20.0%", fixed = TRUE)
    expect_match(n, "remaining 8 edges")
})

test_that("complete data raises no exclusion notice", {
    expect_false(grepl("Rows Excluded",
                       arc_notices(jjarcdiagram(data = arc_net(), source = "from",
                                                target = "to", weight = "w")),
                       fixed = TRUE))
})


# ---- the narrative must not contradict itself -------------------------------

test_that("the density adjective and the Insight paragraph agree", {
    # The two used different cut-points - the adjective switched at 0.2, the
    # Insight paragraph at 0.1 - so any density in [0.1, 0.2] printed "is sparsely
    # connected" and then "Moderate connectivity suggests a balanced network
    # structure" in the very next sentence.
    #
    # 7 nodes and 4 edges give density 4/21 = 0.190, which lands squarely in that
    # band. Asserted unconditionally: an `if` here would silently pass as an
    # "empty test" the moment the fixture drifted out of the band.
    band <- data.frame(from = factor(c("A", "C", "E", "F")),
                       to   = factor(c("B", "D", "F", "G")),
                       w    = c(1, 2, 3, 4))
    g <- igraph::graph_from_data_frame(band[, c("from", "to")], directed = FALSE)
    expect_equal(round(igraph::edge_density(g), 3), 0.190)   # pins the fixture

    t <- arc_stats(jjarcdiagram(data = band, source = "from", target = "to",
                                weight = "w", showStats = TRUE))
    expect_match(t, "sparsely connected")
    expect_match(t, "Sparse networks")
    expect_false(grepl("Moderate connectivity", t, fixed = TRUE))
})

test_that("the network overview reads as grammatical English", {
    t <- arc_stats(jjarcdiagram(data = arc_net(), source = "from", target = "to",
                                weight = "w", showStats = TRUE))
    expect_false(grepl("moderately connectivity", t, fixed = TRUE))
    expect_false(grepl("sparsely connectivity", t, fixed = TRUE))
    expect_false(grepl("highly connectivity", t, fixed = TRUE))
    expect_match(t, "connected \\(density =")
})
