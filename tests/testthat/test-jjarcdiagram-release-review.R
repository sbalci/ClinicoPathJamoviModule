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
        expect_match(n, "2 of 10", info = mode)     # counts the user's rows, pre-aggregation
        expect_match(n, "non-negative on every row", info = mode)
        # tells the user what to do, rather than only that the input was refused
        expect_match(n, "(1 + r) / 2", fixed = TRUE, info = mode)
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
    expect_match(t, "does not establish shared biological pathways")
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


# ---- preset overrides must not leak between preset selections ---------------

test_that("changing the preset rebuilds the option overrides", {
    # jamovi reuses one analysis instance across option changes, so an override
    # recorded for a previously selected preset used to survive forever: after
    # picking "Patient Similarity" the layout stayed horizontal back in "Custom
    # Network", and once "Gene Interaction" had written a plot title, the title
    # the user typed was ignored on every later run.
    Class <- getFromNamespace("jjarcdiagramClass", "ClinicoPath")
    Options <- getFromNamespace("jjarcdiagramOptions", "ClinicoPath")

    opts <- Options$new(source = "from", target = "to", analysisPreset = "custom")
    priv <- Class$new(options = opts, data = arc_net())$.__enclos_env__$private

    # stand in for overrides recorded while another preset was selected
    priv$overrides[["horizontal"]] <- TRUE
    priv$overrides[["plotTitle"]] <- "Gene Interaction Network"

    priv$.configurePresets()

    expect_length(priv$overrides, 0)
    expect_false(isTRUE(priv$.option("horizontal")))   # falls back to the real option
    expect_identical(priv$.option("plotTitle"), "")
})


# ---- reporting defects found in the 2026-09-08 release review -----------------

# S, M, T with a strong two-hop route (10, 10) against a weak direct edge (1).
# Under STRENGTH the two strong ties are the short path, so M bridges S and T.
# Under DISTANCE the direct edge is the short path, so M bridges nothing.
arc_bridge <- function() data.frame(
    from = factor(c("S", "M", "S")),
    to   = factor(c("M", "T", "T")),
    w    = c(10, 10, 1))

test_that("reciprocal weighting makes strong ties the short path", {
    # Hand-derived: reciprocal distances are 0.1, 0.1, 1. S->T via M costs 0.2
    # against 1 direct, so M lies on the only shortest path: betweenness(M) = 1.
    n <- arc_stats(jjarcdiagram(data = arc_bridge(), source = "from", target = "to",
                                weight = "w", showStats = TRUE, weightMode = "strength"))
    expect_match(n, "Highest Betweenness:\\s*M")
})

test_that("distance mode routes over the short direct edge instead", {
    # Distances 10, 10, 1. S->T direct costs 1 against 20 via M, so M lies on no
    # shortest path: betweenness(M) = 0 and no node bridges anything.
    n <- arc_stats(jjarcdiagram(data = arc_bridge(), source = "from", target = "to",
                                weight = "w", showStats = TRUE, weightMode = "distance"))
    # Betweenness is 0 for every node here. which.max() still returns one, and the
    # panel used to name it and call it "an important bridge between different
    # network regions" -- about a node with betweenness exactly 0.
    expect_match(n, "Highest Betweenness:\\s*none")
    expect_match(n, "no entity acts as a bridge")
    expect_false(grepl("important bridge between different network regions", n, fixed = TRUE))
})

test_that("the copy-ready summary does not call a distant node a hub", {
    # A has three edges of distance 1; E has one edge of distance 100. Summed
    # weight makes E the maximum, but under a distance reading that means E is
    # the most PERIPHERAL node. The summary is offered for pasting into
    # manuscripts, so it must not assert the opposite of the data.
    d <- data.frame(from = factor(c("A", "A", "A", "E")),
                    to   = factor(c("B", "C", "D", "F")),
                    w    = c(1, 1, 1, 100))
    s <- arc_txt(jjarcdiagram(data = d, source = "from", target = "to", weight = "w",
                              weightMode = "distance", showSummary = TRUE)$reportSentence$content)
    expect_false(grepl("'E' emerged as the most highly connected hub", s, fixed = TRUE))
    expect_match(s, "'A' has the most connections")
    expect_match(s, "interpreted as distances")

    # Strength mode is unaffected: there a large summed weight IS a strong hub.
    s2 <- arc_txt(jjarcdiagram(data = d, source = "from", target = "to", weight = "w",
                               weightMode = "strength", showSummary = TRUE)$reportSentence$content)
    expect_match(s2, "'E' emerged as the most highly connected hub")
})

test_that("the distance caveat appears only in distance mode", {
    d <- arc_net()
    cav <- "marks the most peripheral entity"
    expect_match(arc_stats(jjarcdiagram(data = d, source = "from", target = "to", weight = "w",
                                        showStats = TRUE, weightMode = "distance")), cav)
    expect_false(grepl(cav, arc_stats(jjarcdiagram(data = d, source = "from", target = "to",
                                                   weight = "w", showStats = TRUE,
                                                   weightMode = "strength"))))
    # and never on an unweighted network, which has no weight mode at all
    expect_false(grepl(cav, arc_stats(jjarcdiagram(data = d, source = "from", target = "to",
                                                   showStats = TRUE))))
})

test_that("edge count and density state which edge set each uses", {
    # 3 parallel A-B edges + 1 B-C. Density is computed on distinct node pairs
    # (2 of 3 possible = 0.667); the edge count is the 4 rows. Printing both
    # without saying so left the density irreproducible by the reader.
    dup <- data.frame(from = factor(c("A", "A", "A", "B")),
                      to   = factor(c("B", "B", "B", "C")))
    n <- arc_stats(jjarcdiagram(data = dup, source = "from", target = "to",
                                aggregateEdges = FALSE, showStats = TRUE))
    expect_match(n, "Number of Edges:\\s*4 \\(2 distinct node pairs")
    expect_match(n, "Network Density:\\s*0.6667")
    # aggregated, the two coincide and the parenthetical is dropped
    expect_match(arc_stats(jjarcdiagram(data = dup, source = "from", target = "to",
                                        aggregateEdges = TRUE, showStats = TRUE)),
                 "Number of Edges:\\s*2 ")
})

test_that("undirected reciprocal duplicates are reported without aggregation", {
    # A->B and B->A are one undirected edge. The warn-only branch used an
    # un-normalised key, so this network drew no duplicate warning while igraph
    # still counted two parallel edges into degree and strength.
    sym <- data.frame(from = factor(c("A", "B", "B")), to = factor(c("B", "A", "C")))
    expect_match(arc_notices(jjarcdiagram(data = sym, source = "from", target = "to",
                                          aggregateEdges = FALSE, directed = FALSE)),
                 "duplicate edge")
})
