# Vendored from the 'arcdiagram' package (Gaston Sanchez), GPL-3, v0.1.12.
# arcdiagram is GitHub-only (gastonstat/arcdiagram) and was never published to CRAN,
# so jamovi's library installer (which does not process Remotes) could not install it
# and jjarcdiagram crashed for end users. These 7 functions - arcdiagram's entire
# public surface - are vendored here so jjarcdiagram works with no external dependency.
# (The code path jjarcdiagram uses takes a 2-column matrix edgelist and needs only base
# graphics; igraph is not invoked.) Upstream: https://github.com/gastonstat/arcdiagram

xynodes <- function (num_nodes, aux_ord, labels) 
{
    nf = rep(1/num_nodes, num_nodes)
    fin = cumsum(nf)
    ini = c(0, cumsum(nf)[-num_nodes])
    centers = (ini + fin)/2
    names(centers) = labels[aux_ord]
    centers
}

node_coords <- function (edgelist, sorted = FALSE, decreasing = FALSE, ordering = NULL, 
    labels = NULL) 
{
    nodes_edges = graph_info(edgelist, sorted = sorted, decreasing = decreasing, 
        ordering = ordering, labels = labels)
    nodes = nodes_edges$nodes
    num_nodes = nodes_edges$num_nodes
    num_edges = nodes_edges$num_edges
    aux_ord = nodes_edges$aux_ord
    labels = nodes_edges$labels
    centers = xynodes(num_nodes, aux_ord, labels)
}

arc_radius_locs <- function (edgelist, nodes, centers) 
{
    edges_from_to = matrix(0, nrow(edgelist), 2)
    for (i in 1L:nrow(edgelist)) {
        edges_from_to[i, 1] = centers[which(nodes == edgelist[i, 
            1])]
        edges_from_to[i, 2] = centers[which(nodes == edgelist[i, 
            2])]
    }
    radios = abs(edges_from_to[, 1] - edges_from_to[, 2])/2
    max_radios = which(radios == max(radios))
    max_rad = unique(radios[max_radios]/2)
    locs = rowSums(edges_from_to)/2
    list(locs = locs, radios = radios)
}

min_max_margin <- function (radios, above) 
{
    max_radios = which(radios == max(radios))
    lim_min = 0
    lim_max = 0
    above_radios = radios[above]
    if (length(above_radios > 0)) {
        max_above_radios = which(above_radios == max(above_radios))[1]
        lim_max = above_radios[max_above_radios]
    }
    below_radios = radios[!above]
    if (length(below_radios > 0)) {
        max_below_radios = which(below_radios == max(below_radios))[1]
        lim_min = -1 * below_radios[max_below_radios]
    }
    list(min = lim_min, max = lim_max)
}

above_below <- function (edgelist, above) 
{
    if (is.null(above)) {
        above = rep(TRUE, nrow(edgelist))
    }
    else {
        if (length(above) > nrow(edgelist)) 
            stop("\nlength of 'above' exceeds number of rows in 'edgelist'")
        if (is.numeric(above)) {
            above_positive <- any(above > 0)
            above_negative <- any(above < 0)
            if (above_positive & above_negative) 
                stop("\n'above' cannot contain both negative and positive indices")
            if (all(above > 0)) {
                above = 1:nrow(edgelist) %in% above
            }
            if (all(above < 0)) {
                above <- !(-(1:nrow(edgelist)) %in% above)
            }
            if (all(above == 0)) {
                above = rep(FALSE, nrow(edgelist))
            }
        }
        if (is.logical(above)) {
            if (length(above) != nrow(edgelist)) 
                stop("\nlength of 'above' must equal number of rows in 'edgelist'")
        }
    }
    above
}

graph_info <- function (edgelist, vertices, sorted = FALSE, decreasing = FALSE, 
    ordering = NULL, labels = NULL) 
{
    if (!is.matrix(edgelist) || ncol(edgelist) != 2) 
        stop("\nSorry, 'edgelist' must be a two column matrix")
    num_edges = nrow(edgelist)
    if (methods::hasArg(vertices)) {
        nodes = vertices
    }
    else {
        nodes = unique(as.vector(t(edgelist)))
    }
    num_nodes = length(nodes)
    if (!is.null(labels)) {
        if (length(labels) != num_nodes) 
            stop("\nLength of 'labels' differs from number of nodes")
    }
    else {
        labels = nodes
    }
    aux_ord = 1:num_nodes
    if (sorted) {
        ordered_nodes = order(nodes, decreasing = decreasing)
        nodes = nodes[ordered_nodes]
        labels = labels[ordered_nodes]
        aux_ord = ordered_nodes
    }
    if (!is.null(ordering)) {
        if (length(ordering) != num_nodes) {
            stop("\nLength of 'ordering' differs from number of nodes")
        }
        if (is.character(ordering)) {
            unmatched_ordering <- !(ordering %in% labels)
            if (any(unmatched_ordering)) {
                undetected = ordering[unmatched_ordering]
                stop(sprintf("\nUnrecognized values in ordering: '%s'", 
                  undetected))
            }
            ordering = match(ordering, labels)
        }
        nodes = nodes[ordering]
        labels = labels[ordering]
        aux_ord = ordering
    }
    list(nodes = nodes, labels = labels, num_nodes = num_nodes, 
        num_edges = num_edges, aux_ord = aux_ord)
}

arcplot <- function (edgelist, vertices, sorted = FALSE, decreasing = FALSE, 
    ordering = NULL, labels = NULL, horizontal = TRUE, above = NULL, 
    col.arcs = "#5998ff77", lwd.arcs = 1.8, lty.arcs = 1, lend = 1, 
    ljoin = 2, lmitre = 1, show.nodes = TRUE, pch.nodes = 19, 
    cex.nodes = 1, col.nodes = "gray80", bg.nodes = "gray80", 
    lwd.nodes = 1, show.labels = TRUE, col.labels = "gray55", 
    cex.labels = 0.9, las = 2, font = 1, line = 0, outer = FALSE, 
    adj = NA, padj = NA, axes = FALSE, xlim = NULL, ylim = NULL, 
    ...) 
{
    if (methods::hasArg(vertices)) {
        nodes_edges = graph_info(edgelist, vertices = vertices, 
            sorted = sorted, decreasing = decreasing, ordering = ordering, 
            labels = labels)
    }
    else {
        nodes_edges = graph_info(edgelist, sorted = sorted, decreasing = decreasing, 
            ordering = ordering, labels = labels)
    }
    nodes = nodes_edges$nodes
    num_nodes = nodes_edges$num_nodes
    num_edges = nodes_edges$num_edges
    aux_ord = nodes_edges$aux_ord
    labels = nodes_edges$labels
    centers = xynodes(num_nodes, aux_ord, labels)
    above = above_below(edgelist, above)
    radios_locs = arc_radius_locs(edgelist, nodes, centers)
    radios = radios_locs$radios
    locs = radios_locs$locs
    if (length(col.arcs) != num_edges) 
        col.arcs = rep(col.arcs, length = num_edges)
    if (length(lwd.arcs) != num_edges) 
        lwd.arcs = rep(lwd.arcs, length = num_edges)
    if (length(lty.arcs) != num_edges) 
        lty.arcs = rep(lty.arcs, length = num_edges)
    if (length(pch.nodes) != num_nodes) {
        pch.nodes = rep(pch.nodes, length = num_nodes)
    }
    pch.nodes = pch.nodes[aux_ord]
    if (length(cex.nodes) != num_nodes) {
        cex.nodes = rep(cex.nodes, length = num_nodes)
    }
    cex.nodes = cex.nodes[aux_ord]
    if (length(col.nodes) != num_nodes) {
        col.nodes = rep(col.nodes, length = num_nodes)
    }
    col.nodes = col.nodes[aux_ord]
    if (length(bg.nodes) != num_nodes) {
        bg.nodes = rep(bg.nodes, length = num_nodes)
    }
    bg.nodes = bg.nodes[aux_ord]
    if (length(lwd.nodes) != num_nodes) {
        lwd.nodes = rep(lwd.nodes, length = num_nodes)
    }
    lwd.nodes = lwd.nodes[aux_ord]
    if (length(col.labels) != num_nodes) {
        col.labels = rep(col.labels, length = num_nodes)
    }
    col.labels = col.labels[aux_ord]
    if (length(cex.labels) != num_nodes) {
        cex.labels = rep(cex.labels, length = num_nodes)
    }
    cex.labels = cex.labels[aux_ord]
    z = seq(0, pi, length.out = 100)
    if (horizontal) {
        side = 1
    }
    else {
        side = 2
    }
    if (is.null(xlim)) {
        if (horizontal) {
            xlim = c(-0.015, 1.015)
            x_nodes = centers
        }
        else {
            xlims = min_max_margin(radios, above)
            xlim = c(xlims$min, xlims$max)
            x_nodes = rep(0, num_nodes)
        }
    }
    else {
        if (horizontal) {
            x_nodes = centers
        }
        else {
            x_nodes = rep(0, num_nodes)
        }
    }
    if (is.null(ylim)) {
        if (horizontal) {
            ylims = min_max_margin(radios, above)
            ylim = c(ylims$min, ylims$max)
            y_nodes = rep(0, num_nodes)
        }
        else {
            ylim = c(-0.015, 1.015)
            y_nodes = centers
        }
    }
    else {
        if (horizontal) {
            y_nodes = rep(0, num_nodes)
        }
        else {
            y_nodes = centers
        }
    }
    plot(0.5, 0.5, xlim = xlim, ylim = ylim, type = "n", xlab = "", 
        ylab = "", axes = axes, ...)
    for (i in 1L:num_edges) {
        radio = radios[i]
        if (horizontal) {
            x_arc = locs[i] + radio * cos(z)
            if (above[i]) {
                y_arc = radio * sin(z)
            }
            else {
                y_arc = radio * sin(-z)
            }
        }
        else {
            y_arc = locs[i] + radio * cos(z)
            if (above[i]) {
                x_arc = radio * sin(z)
            }
            else {
                x_arc = radio * sin(-z)
            }
        }
        graphics::lines(x_arc, y_arc, col = col.arcs[i], lwd = lwd.arcs[i], 
            lty = lty.arcs[i], lend = lend, ljoin = ljoin, lmitre = lmitre)
        if (show.nodes) {
            graphics::points(x = x_nodes, y = y_nodes, pch = pch.nodes, 
                col = col.nodes, bg = bg.nodes, cex = cex.nodes, 
                lwd = lwd.nodes)
        }
        if (show.labels) {
            graphics::mtext(labels, side = side, line = line, at = centers, 
                cex = cex.labels, outer = outer, col = col.labels, 
                las = las, font = font, adj = adj, padj = padj, 
                ...)
        }
    }
}

