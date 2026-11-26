#' @title Arc Diagram
#' @importFrom R6 R6Class
#' @importFrom jmvcore .
#' @importFrom igraph betweenness is_connected
#' @importFrom grDevices adjustcolor col2rgb rgb
#' @importFrom graphics par title legend
#' @return An \code{R6} class generator object for the \code{jjarcdiagramClass} backend; used internally by the jamovi analysis wrapper and not called directly.


jjarcdiagramClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjarcdiagramClass",
    inherit = jjarcdiagramBase,
    private = list(

        # Safe syntax-pane codegen: drop undeclared option names and list values
        # (from crafted .omv/.omt files) and escape non-default character scalars
        # via encodeString() so quotes/backslashes/newlines cannot inject arguments.
        .sourcifyOption = function(option) {
            if (!option$name %in% names(formals(jjarcdiagram)))
                return("")
            value <- option$value
            if (is.list(value))
                return("")
            if (is.character(value)) {
                if (length(value) != 1L || is.na(value) || identical(value, option$default))
                    return("")
                return(paste0(option$name, " = ", encodeString(value, quote = '"')))
            }
            super$.sourcifyOption(option)
        },

        # Defense-in-depth option backstop for R callers using run(noThrow = TRUE)
        # or direct R6 option mutation.
        .validateOptions = function() {
            in_range <- function(x, lo, hi) {
                is.numeric(x) && length(x) == 1L && is.finite(x) && x >= lo && x <= hi
            }
            if (!in_range(self$options$nodeSizeValue, 0.1, 10) ||
                !in_range(self$options$arcWidthValue, 0.1, 5) ||
                !in_range(self$options$arcTransparency, 0, 1) ||
                !in_range(self$options$labelSize, 0.1, 2) ||
                !isTRUE(self$options$analysisPreset %in% c("custom", "gene_interaction", "patient_network", "pathway_network", "comorbidity_network")) ||
                !isTRUE(self$options$weightMode %in% c("strength", "distance")) ||
                !isTRUE(self$options$nodeSize %in% c("fixed", "degree")) ||
                !isTRUE(self$options$arcWidth %in% c("fixed", "weight")) ||
                !isTRUE(self$options$sortNodes %in% c("none", "name", "group", "degree")) ||
                !isTRUE(self$options$arcColorMode %in% c("source", "target", "gradient"))) {
                private$.addNotice("ERROR", .("Invalid Option Value"),
                    .("One or more numeric or list options are outside their permitted range."))
                return(FALSE)
            }
            TRUE
        },

        # Option overrides for clinical presets (jamovi options are read-only at runtime;
        # presets record overrides here and reads go through private$.option()).
        overrides = list(),
        .option = function(option) {
            if (option %in% names(private$overrides)) return(private$overrides[[option]])
            opt_obj <- self$options$option(option)
            if (!is.null(opt_obj)) return(opt_obj$value)
            return(NULL)
        },

        # Notice collection rendered into the Preformatted notices result item
        # (sorted by severity: ERROR -> STRONG_WARNING -> WARNING -> INFO).
        .noticeList = list(),

        .addNotice = function(type, title, content) {
            entry <- list(type = type, title = title, content = content)
            # Idempotent by identity. Both .run() and .plot() call
            # .prepareNetworkData() on the SAME R6 instance, so the render pass
            # re-emits every network notice. Dropping an exact duplicate here is
            # what lets .prepareNetworkData() stop wiping the list -- and the wipe
            # was destroying notices raised OUTSIDE it (the summary-failure notice
            # in .run() is added after prepare, so the render pass deleted it and
            # the user never saw why the summary was missing).
            for (existing in private$.noticeList) {
                if (identical(existing, entry))
                    return(invisible(NULL))
            }
            private$.noticeList[[length(private$.noticeList) + 1]] <- entry
            private$.renderNotices()
        },

        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                self$results$notices$setContent("")
                return()
            }
            # Order notices by severity (ERROR -> STRONG_WARNING -> WARNING -> INFO)
            # while preserving emission order within each severity tier.
            prios <- vapply(private$.noticeList, function(notice) {
                switch(notice$type,
                    ERROR = 1L, STRONG_WARNING = 2L, WARNING = 3L, INFO = 4L, 5L)
            }, integer(1))
            ordered_notices <- private$.noticeList[order(prios, seq_along(prios))]
            blocks <- vapply(ordered_notices, function(notice) {
                prefix <- switch(notice$type,
                    ERROR = "ERROR: ", STRONG_WARNING = "WARNING: ",
                    WARNING = "WARNING: ", "")
                paste0(prefix, notice$title, "\n", notice$content)
            }, character(1))
            self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
        },

        # Init function
        .init = function() {
            # The Getting Started panel belongs to the UNCONFIGURED state only.
            configured <- !is.null(self$options$source) && !is.null(self$options$target)
            self$results$instructions$setVisible(!configured)
            if (!configured) {
                self$results$instructions$setContent(private$.createInstructions())
                return()
            }
            self$results$instructions$setContent("")

            # Apply preset overrides BEFORE sizing: some presets (e.g. patient_network)
            # force a horizontal layout, and the canvas dimensions must reflect that
            # override. Reading .option("horizontal") before .configurePresets() sized a
            # forced-horizontal plot inside a portrait (600x800) canvas.
            private$.configurePresets()

            # Set plot size based on layout
            plot_width <- if (private$.option("horizontal")) 800 else 600
            plot_height <- if (private$.option("horizontal")) 600 else 800
            self$results$plot$setSize(plot_width, plot_height)

            # Set up assumptions panel conditionally
            private$.updateAssumptionsPanel()
        },

        # Run function
        .run = function() {

            private$.noticeList <- list()
            # This Html result persists on the R6 object across option changes.
            # Render the empty list immediately so a notice from the previous
            # run cannot survive when the new run has nothing to report.
            private$.renderNotices()

            # Check for required variables
            if (is.null(self$options$source) || is.null(self$options$target) || 
                length(self$options$source) == 0 || length(self$options$target) == 0) {
                todo <- paste(
                    .("<br>Welcome to ClinicoPath Arc Diagram"),
                    "<br><br>",
                    .("Create interactive network visualizations to explore relationships between entities."),
                    "<br><br>",
                    .("<b>Required:</b>"),
                    .("<li>Source Node: Starting point of connections</li>"),
                    .("<li>Target Node: Endpoint of connections</li>"),
                    "<br>",
                    .("<b>Optional:</b>"),
                    .("<li>Edge Weight: Strength of connections</li>"),
                    .("<li>Node Groups: Categories for color coding</li>"),
                    "<br><hr>",
                    sep = "\n"
                )
                self$results$todo$setContent(todo)
                # Clear report sentence when no variables selected
                self$results$reportSentence$setContent("")
                return()
            }


            if (is.null(self$data))
                return()

            if (!private$.validateOptions()) {
                self$results$todo$setContent("")
                self$results$networkStats$setContent("")
                self$results$reportSentence$setContent("")
                return()
            }

            private$.checkpoint()
            private$.configurePresets()
            if (length(private$overrides) > 0) {
                ov_desc <- paste(
                    vapply(names(private$overrides), function(k) {
                        sprintf("%s = %s", k, as.character(private$overrides[[k]]))
                    }, character(1)),
                    collapse = ", "
                )
                private$.addNotice(
                    'INFO',
                    .('Analysis Preset Active'),
                    sprintf(.("Preset '%s' applied default override(s): %s."),
                            self$options$analysisPreset, ov_desc)
                )
            }
            # Update assumptions panel when variables change
            private$.updateAssumptionsPanel()

            # Prepare and validate the network data
            private$.checkpoint()  # Before expensive network data processing
            network_data <- private$.prepareNetworkData()
            
            if (is.null(network_data)) {
                # Clear prior run outputs so stale results never survive beside an
                # ERROR notice when jamovi reuses this R6 instance across option edits.
                self$results$todo$setContent("")
                self$results$networkStats$setContent("")
                self$results$reportSentence$setContent("")
                return()
            }
            
            # Generate network statistics
            if (self$options$showStats) {
                private$.checkpoint()  # Before statistical calculations
                private$.generateNetworkStats(network_data)
            }
            
            # Generate copy-ready report sentence with error handling (visible only when showSummary=TRUE)
            if (self$options$showSummary) {
                tryCatch({
                    private$.generateReportSentence(network_data)
                }, error = function(e) {
                    # jamovi does not surface base warning() output, so the reason the
                    # summary is missing goes to the notices item the user can see.
                    private$.addNotice(
                        "WARNING",
                        .("Summary could not be generated"),
                        sprintf(.("%s. The network plot and statistics above are unaffected."),
                                conditionMessage(e)))
                    # Set fallback content
                    fallback_html <- paste(
                        .("<h3> Analysis Summary</h3>"),
                        "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 10px; border-left: 4px solid #ffc107; color: inherit;'>",
                        .("<p><strong>Status:</strong> Network analysis completed successfully.</p>"),
                        .("<p><strong>Note:</strong> Detailed summary generation encountered an issue. Basic network visualization is available above.</p>"),
                        "</div>",
                        sep = "\n"
                    )
                    self$results$reportSentence$setContent(fallback_html)
                })
            }

            # Generate glossary if requested (visible only when showGlossary=TRUE)
            if (self$options$showGlossary) {
                private$.generateGlossary()
            }

            # Update todo with processing status
            todo <- paste(
                .("<br>Network Analysis Complete"),
                sprintf(.("<br>Nodes: %d"), network_data$n_nodes),
                sprintf(.("<br>Edges: %d"), network_data$n_edges),
                sprintf(.("<br>Density: %.3f"), network_data$density),
                "<br><hr>",
                sep = "\n"
            )
            self$results$todo$setContent(todo)
        },

        # Plot function (pure data route with requiresData: true so edge lists are
        # never duplicated into .omv plot state; safe on resize and .omv reopen).
        .plot = function(image, ggtheme, theme, ...) {
            # Check for required variables - don't attempt plot without them
            if (is.null(self$options$source) || is.null(self$options$target) ||
                length(self$options$source) == 0 || length(self$options$target) == 0)
                return(FALSE)

            if (is.null(self$data) || nrow(self$data) == 0)
                return(FALSE)

            if (!private$.validateOptions())
                return(FALSE)

            private$.checkpoint()  # Before network data preparation in plot
            network_data <- private$.prepareNetworkData()
            if (is.null(network_data))
                return(FALSE)

            # Create the plot with proper error handling
            private$.checkpoint()  # Before expensive plot creation
            ok <- tryCatch({
                private$.createArcPlot(network_data, theme)
                TRUE
            }, error = function(e) {
                private$.addNotice("ERROR", .("Plot Creation Error"),
                    sprintf(.("Error creating plot: %s"), conditionMessage(e)))
                FALSE
            })

            ok
        },

        # Helper method to escape variable names for safe igraph handling
        .escapeVar = function(x) {
            x_char <- as.character(x)
            safe <- make.names(x_char)
            safe <- gsub("\\.", "_", safe)
            # Enforce uniqueness AFTER the dot substitution. Running make.names(unique=TRUE)
            # first and gsub second let distinct originals ('HLA.A' and 'HLA_A') both collapse
            # to 'HLA_A', silently merging two entities into one node. Uniqueness must be the
            # final operation so any post-substitution collisions get re-suffixed.
            safe <- make.names(safe, unique = TRUE)
            return(safe)
        },

        # Map make.names-escaped node key(s) back to the original display label(s).
        # Falls back to the key itself when unmapped so nothing is dropped.
        .toDisplayLabel = function(escaped, network_data) {
            if (is.null(escaped) || length(escaped) == 0) return(escaped)
            lbl <- network_data$escaped_to_label[escaped]
            lbl[is.na(lbl)] <- escaped[is.na(lbl)]
            unname(lbl)
        },

        # Identify all tied maxima deterministically (alphabetical order) so
        # reported hub/bridge labels never depend on spreadsheet row order.
        .getTopNodes = function(vec, network_data, tol = 1e-9) {
            max_val <- max(vec)
            top_idx <- which(abs(vec - max_val) <= tol)
            top_labels <- sort(private$.toDisplayLabel(names(vec)[top_idx], network_data))
            display <- if (length(top_labels) == 1L) {
                top_labels
            } else if (length(top_labels) <= 3L) {
                paste0(paste(top_labels, collapse = ", "),
                       " ", sprintf(.("(%d tied)"), length(top_labels)))
            } else {
                sprintf(.("%s, %s, %s (+%d more tied)"),
                        top_labels[1], top_labels[2], top_labels[3],
                        length(top_labels) - 3L)
            }
            list(
                labels = top_labels,
                display = display,
                n_tied = length(top_labels),
                max_val = max_val
            )
        },

        # Exact Brandes (2001) weighted betweenness using relative-tolerance Dijkstra.
        # Used as a numerical-stability fallback when finite positive edge lengths span
        # > 1e8 orders of magnitude (where igraph's C absolute epsilon silently corrupts
        # shortest-path counts even after median scaling).
        .exactWeightedBetweenness = function(g, edge_lengths, rel_eps = 1e-12) {
            n <- igraph::vcount(g)
            vnames <- igraph::V(g)$name
            cb <- stats::setNames(numeric(n), vnames)
            if (n < 3L || igraph::ecount(g) == 0L) return(cb)

            directed <- igraph::is_directed(g)
            el <- igraph::as_edgelist(g, names = FALSE)
            adj <- vector("list", n)
            for (i in seq_len(nrow(el))) {
                len <- edge_lengths[i]
                if (!is.finite(len) || len <= 0) next
                u <- el[i, 1L]
                v <- el[i, 2L]
                adj[[u]][[length(adj[[u]]) + 1L]] <- c(v, len)
                if (!directed) {
                    adj[[v]][[length(adj[[v]]) + 1L]] <- c(u, len)
                }
            }

            for (s in seq_len(n)) {
                S <- integer(0)
                P <- vector("list", n)
                sigma <- numeric(n)
                sigma[s] <- 1
                dist <- rep(Inf, n)
                dist[s] <- 0
                settled <- logical(n)

                while (TRUE) {
                    cand <- which(!settled & is.finite(dist))
                    if (length(cand) == 0L) break
                    v <- cand[which.min(dist[cand])]
                    settled[v] <- TRUE
                    S <- c(S, v)
                    for (nb in adj[[v]]) {
                        w <- as.integer(nb[1L])
                        d_alt <- dist[v] + nb[2L]
                        tol_w <- rel_eps * max(1, abs(d_alt), if (is.finite(dist[w])) abs(dist[w]) else 0)
                        if (d_alt < dist[w] - tol_w) {
                            dist[w] <- d_alt
                            sigma[w] <- sigma[v]
                            P[[w]] <- v
                        } else if (is.finite(dist[w]) && abs(d_alt - dist[w]) <= tol_w) {
                            sigma[w] <- sigma[w] + sigma[v]
                            P[[w]] <- c(P[[w]], v)
                        }
                    }
                }

                delta <- numeric(n)
                for (k in rev(seq_along(S))) {
                    w <- S[k]
                    for (v in P[[w]]) {
                        delta[v] <- delta[v] + (sigma[v] / sigma[w]) * (1 + delta[w])
                    }
                    if (w != s) cb[w] <- cb[w] + delta[w]
                }
            }

            if (!directed) cb <- cb / 2
            cb
        },

        # Helper method to create user instructions
        .createInstructions = function() {
            # Get preset-specific guidance
            preset_guidance <- private$.getPresetGuidance()
            
            instructions <- paste(
                .("<h3> Arc Diagram Network Visualization</h3>"),
                preset_guidance,
                
                .("<h4> Getting Started:</h4>"),
                "<ul>",
                .("<li><strong>Source Node:</strong> Select the variable representing connection origins</li>"),
                .("<li><strong>Target Node:</strong> Select the variable representing connection destinations</li>"),
                .("<li><strong>Edge Weight:</strong> Optional numeric variable for connection strength</li>"),
                .("<li><strong>Node Groups:</strong> Optional categorical variable for color coding</li>"),
                "</ul>",
                
                .("<h4> Customization Options:</h4>"),
                "<ul>",
                .("<li><strong>Layout:</strong> Choose horizontal or vertical arrangement</li>"),
                .("<li><strong>Node Sizing:</strong> Fixed size or proportional to degree centrality</li>"),
                .("<li><strong>Node Sorting:</strong> Arrange by name, degree, or group</li>"),
                .("<li><strong>Arc Styling:</strong> Fixed width or scaled by edge weights</li>"),
                .("<li><strong>Color Coding:</strong> Automatic group-based color palettes for nodes and arcs</li>"),
                "</ul>",
                
                .("<h4> Network Metrics:</h4>"),
                "<ul>",
                .("<li><strong>Density:</strong> Measure of network connectivity</li>"),
                .("<li><strong>Centrality:</strong> Node importance in the network (degree/strength and betweenness)</li>"),
                .("<li><strong>Node Groups:</strong> Categorical group distribution when a group variable is supplied</li>"),
                "</ul>",
                
                .("<p><em> Tip: Arc diagrams are ideal for visualizing hierarchical or sequential relationships with minimal visual clutter.</em></p>"),
                sep = "\n"
            )
            return(instructions)
        },
        
        # Helper method to get preset-specific guidance
        .getPresetGuidance = function() {
            preset <- self$options$analysisPreset
            
            guidance <- switch(preset,
                "gene_interaction" = paste(
                    "<div style='background-color: rgba(33, 159, 33, 0.1); padding: 10px; margin: 10px 0; border-left: 4px solid #4CAF50; color: inherit;'>",
                    .("<h4> Gene Interaction Network Analysis</h4>"),
                    .("<p><strong>Optimized for:</strong> Gene regulatory relationships, protein interactions, pathway analysis</p>"),
                    .("<p><strong>Recommendations:</strong> Use gene symbols as nodes, interaction scores as weights, pathways/functions as groups</p>"),
                    .("<p><strong>Suggested Settings:</strong> Sort by group, enable statistics, use degree-based node sizing</p>"),
                    "</div>",
                    sep = "\n"
                ),
                "patient_network" = paste(
                    "<div style='background-color: rgba(33, 152, 239, 0.13); padding: 10px; margin: 10px 0; border-left: 4px solid #2196F3; color: inherit;'>",
                    .("<h4> Patient Similarity Network Analysis</h4>"),
                    .("<p><strong>Optimized for:</strong> Patient similarity, treatment response, clinical outcomes</p>"),
                    .("<p><strong>Recommendations:</strong> Use patient IDs as nodes, similarity scores as weights, clinical subtypes as groups</p>"),
                    .("<p><strong>Suggested Settings:</strong> Sort by group, enable statistics, use horizontal layout</p>"),
                    "</div>",
                    sep = "\n"
                ),
                "pathway_network" = paste(
                    "<div style='background-color: rgba(255, 169, 33, 0.14); padding: 10px; margin: 10px 0; border-left: 4px solid #ff9800; color: inherit;'>",
                    .("<h4> Biological Pathway Network Analysis</h4>"),
                    .("<p><strong>Optimized for:</strong> Pathway interactions, biological processes, functional modules</p>"),
                    .("<p><strong>Recommendations:</strong> Use pathway names as nodes, interaction strength as weights, functional categories as groups</p>"),
                    .("<p><strong>Suggested Settings:</strong> Sort by degree, enable statistics, use weight-based arc widths</p>"),
                    "</div>",
                    sep = "\n"
                ),
                "comorbidity_network" = paste(
                    "<div style='background-color: rgba(230, 33, 99, 0.12); padding: 10px; margin: 10px 0; border-left: 4px solid #e91e63; color: inherit;'>",
                    .("<h4> Disease Co-occurrence Network Analysis</h4>"),
                    .("<p><strong>Optimized for:</strong> Disease associations, comorbidity patterns, epidemiological analysis</p>"),
                    .("<p><strong>Recommendations:</strong> Use disease codes/names as nodes, co-occurrence frequency as weights, disease categories as groups</p>"),
                    .("<p><strong>Suggested Settings:</strong> Sort by degree, enable statistics, use weight-based arc widths</p>"),
                    "</div>",
                    sep = "\n"
                ),
                "" # Default: no specific guidance
            )
            
            return(guidance)
        },
        
        # Helper method to update assumptions panel conditionally
        .updateAssumptionsPanel = function() {
            # The panel is hidden by .r.yaml `visible: (showAssumptions)`, so building
            # its HTML when the option is off is work nobody ever sees. glossary and
            # reportSentence are already gated this way in .run(); this matches them.
            if (!self$options$showAssumptions ||
                is.null(self$options$source) || is.null(self$options$target) ||
                length(self$options$source) == 0 || length(self$options$target) == 0) {
                self$results$assumptions$setContent("")
                return()
            }
            self$results$assumptions$setContent(private$.createAssumptions())
        },
        
        # Helper method to configure analysis presets
        .configurePresets = function() {
            preset <- self$options$analysisPreset

            # Rebuild from scratch every call. jamovi reuses this R6 instance across
            # option changes, so an override left from a previously selected preset
            # otherwise persists forever: switching back to "Custom Network" kept the
            # forced horizontal layout, and once gene_interaction had set plotTitle
            # the user's own title was ignored on every subsequent run.
            private$overrides <- list()

            # Apply preset-specific optimizations without clobbering explicit
            # non-default user selections for sortNodes or plotTitle.
            switch(preset,
                "gene_interaction" = {
                    if (is.null(self$options$plotTitle) || self$options$plotTitle == "") {
                        private$overrides[["plotTitle"]] <- .("Gene Interaction Network")
                    }
                },
                "patient_network" = {
                    private$overrides[["horizontal"]] <- TRUE
                },
                "pathway_network" = {
                    if (identical(self$options$sortNodes, "none")) {
                        private$overrides[["sortNodes"]] <- "degree"
                    }
                },
                "comorbidity_network" = {
                    if (identical(self$options$sortNodes, "none")) {
                        private$overrides[["sortNodes"]] <- "degree"
                    }
                }
            )
        },
        
        # Helper method to prepare network data
        .prepareNetworkData = function() {
            # NOTE: deliberately does NOT reset the notice list. It used to, because
            # both .run() and .plot() call this method on the same R6 instance and the
            # render pass would otherwise duplicate every warning. That reset also
            # destroyed notices raised outside this method -- the summary-failure
            # notice added later in .run() vanished as soon as the plot drew.
            # Duplication is now prevented in .addNotice() instead, which ignores an
            # exact repeat. A fresh .run() still clears the list at its top.

            mydata <- self$data

            if (nrow(mydata) == 0) {
                private$.addNotice("ERROR", .("No Data Available"),
                    .("No data available. Please load a dataset and re-run the analysis."))
                return(NULL)
            }

            # Get variable names
            source_var <- self$options$source
            target_var <- self$options$target
            weight_var <- self$options$weight
            group_var <- self$options$group

            # Validate required columns exist
            if (!source_var %in% names(mydata) || !target_var %in% names(mydata)) {
                private$.addNotice("ERROR", .("Variables Not Found"),
                    .("Source or target variables not found in the dataset."))
                return(NULL)
            }

            # Build list of required edge columns to check for NAs.
            # Optional group_var is a node-coloring/sorting attribute, NOT an edge-existence
            # attribute: rows with missing group labels stay in the network and render as
            # ungrouped (gray) rather than silently altering network topology and centrality.
            required_cols <- c(source_var, target_var)
            has_weight_col <- !is.null(weight_var) && weight_var %in% names(mydata)
            has_group_col <- !is.null(group_var) && group_var %in% names(mydata)
            if (has_weight_col) {
                required_cols <- c(required_cols, weight_var)
            }

            # Unwrap jamovi Nominal/Ordinal numeric weights BEFORE row subsetting:
            # base R [.factor strips custom attributes including attr(x, "values"),
            # which would otherwise make jmvcore::toNumeric() fail after subsetting.
            raw_weights_all <- if (has_weight_col) {
                jmvcore::toNumeric(mydata[[weight_var]])
            } else {
                NULL
            }

            # SELECTIVE NA OMISSION: Only drop rows with missing/blank source, target, or weight.
            # Check as.character() AND nzchar(trimws()) so explicit NA levels (addNA()) AND
            # empty/whitespace strings ("", "   ") are excluded cleanly rather than crashing
            # R's named-vector lookup node_map[""] -> NA_character_.
            complete_rows <- rep(TRUE, nrow(mydata))
            for (col in c(source_var, target_var)) {
                vals <- mydata[[col]]
                vals_chr <- as.character(vals)
                complete_rows <- complete_rows & !is.na(vals) & !is.na(vals_chr) & nzchar(trimws(vals_chr))
            }
            if (has_weight_col) {
                wcheck <- if (is.numeric(raw_weights_all)) raw_weights_all else mydata[[weight_var]]
                complete_rows <- complete_rows & !is.na(wcheck)
            }
            mydata_clean <- mydata[complete_rows, , drop = FALSE]

            if (nrow(mydata_clean) == 0) {
                private$.addNotice("ERROR", .("No Complete Rows"), .("No complete rows after removing missing or blank values. Check your data for NA or empty values in source, target, or weight variables."))
                return(NULL)
            }

            # Dropped rows are dropped EDGES, and every number this analysis reports
            # - density, degree, betweenness, the "most central" node - is computed
            # on what survives.
            n_dropped <- nrow(mydata) - nrow(mydata_clean)
            if (n_dropped > 0) {
                private$.addNotice("WARNING", .("Rows Excluded"),
                    sprintf(.("%d of %d rows (%.1f%%) were excluded because of missing or blank values in %s. The network below, and every statistic computed from it, describes the remaining %d edges only."),
                            n_dropped, nrow(mydata), 100 * n_dropped / nrow(mydata),
                            paste(required_cols, collapse = ", "), nrow(mydata_clean)))
            }

            # Build a SINGLE escape mapping across BOTH source and target columns so
            # that the same node value always maps to the same safe name and distinct
            # values never collide.
            node_labels <- unique(c(as.character(mydata_clean[[source_var]]),
                                    as.character(mydata_clean[[target_var]])))
            node_map <- stats::setNames(private$.escapeVar(node_labels), node_labels)

            # Create edge data frame with escaped names + original labels
            edge_df <- data.frame(
                source = unname(node_map[as.character(mydata_clean[[source_var]])]),
                target = unname(node_map[as.character(mydata_clean[[target_var]])]),
                stringsAsFactors = FALSE
            )

            # Add weights if specified
            if (has_weight_col) {
                raw_weights <- raw_weights_all[complete_rows]
                if (!is.numeric(raw_weights) || any(!is.finite(raw_weights))) {
                    private$.addNotice("ERROR", .("Invalid edge weights"),
                        .("Weights must be finite numeric values on every row."))
                    return(NULL)
                }
                # Shortest-path centrality is undefined on negative edge lengths, and
                # igraph enforces that at the C level: it aborts the whole analysis with
                # "Weight vector must be positive. Invalid value / centrality/
                # betweenness.c:437" and no usable explanation. Negative weights are not
                # exotic here - this option is documented for correlations and
                # fold-changes, both routinely negative - so catch them with a message
                # that says what to do instead.
                #
                # This must run BEFORE edge aggregation. The same check used to sit after
                # the igraph object was built, where it was unreachable (the combined
                # guard above had already rejected negatives with a generic message that
                # dropped all of this guidance), and where the counts it reported would
                # have been of summed weights rather than of the user's own rows.
                if (any(raw_weights < 0)) {
                    private$.addNotice("ERROR", .("Negative Edge Weights"),
                        sprintf(.("%d of %d edge weights in '%s' are negative, and network centrality is undefined on negative edge lengths. Weights must be non-negative on every row. If these are correlations, use their absolute value, or rescale to a non-negative strength such as (1 + r) / 2. If they are log fold-changes, use the absolute value."),
                                sum(raw_weights < 0), length(raw_weights), weight_var))
                    return(NULL)
                }
                edge_df$weight <- raw_weights
            } else {
                edge_df$weight <- 1
            }

            # Robust escaped -> original-label map built from the full node set
            # (node_map maps original -> escaped; invert it). Order-independent, so it stays
            # valid through self-loop removal and edge aggregation below. Display labels are
            # (re)derived from the FINAL edgelist after aggregation, never from this stale
            # pre-aggregation edge_df (which caused length/order mismatches with arcplot).
            escaped_to_label <- stats::setNames(names(node_map), unname(node_map))
            has_weight_var <- has_weight_col

            # Remove self-loops before aggregation
            self_loops <- sum(edge_df$source == edge_df$target)
            if (self_loops > 0) {
                edge_df <- edge_df[edge_df$source != edge_df$target, , drop = FALSE]
            }

            # Refuse cleanly if every row was a self-loop (prevents aggregate() crash
            # and NaN density -> missing value where TRUE/FALSE needed).
            if (nrow(edge_df) == 0) {
                private$.addNotice(
                    "ERROR",
                    .("No Non-Self-Loop Edges"),
                    sprintf(.("All %d edge(s) were self-loops (source == target) and were removed. No edges remain to construct a network."), self_loops)
                )
                return(NULL)
            }

            # ONE canonical edge key, computed once and used by both the aggregate
            # and the warn-only branch below.
            edge_key <- if (self$options$directed) {
                paste(edge_df$source, edge_df$target, sep = "~~~")
            } else {
                paste(pmin(edge_df$source, edge_df$target),
                      pmax(edge_df$source, edge_df$target), sep = "~~~")
            }
            duplicates <- sum(duplicated(edge_key))

            aggregation_warnings <- c()
            if (self$options$aggregateEdges) {
                if (duplicates > 0) {
                    agg_msg <- if (has_weight_var && identical(self$options$weightMode, "distance")) {
                        sprintf(.("Note: %d duplicate edge(s) aggregated by taking minimum distance"), duplicates)
                    } else if (has_weight_var) {
                        sprintf(.("Note: %d duplicate edge(s) aggregated by summing weights"), duplicates)
                    } else {
                        sprintf(.("Note: %d duplicate edge(s) aggregated (unweighted network)"), duplicates)
                    }
                    aggregation_warnings <- c(aggregation_warnings, agg_msg)
                }

                # Preserve first-appearance edge order AND first-seen (source, target)
                # orientation so sortNodes = "none" retains input sequence and
                # arcColorMode = "source"/"target" respects original endpoints.
                uniq_keys <- unique(edge_key)
                first_idx <- match(uniq_keys, edge_key)
                if (has_weight_var && identical(self$options$weightMode, "distance")) {
                    agg_w <- tapply(edge_df$weight, factor(edge_key, levels = uniq_keys), min)
                } else if (has_weight_var) {
                    agg_w <- tapply(edge_df$weight, factor(edge_key, levels = uniq_keys), sum)
                } else {
                    # Keep unweighted edges at unit weight (do not store multiplicity as weight)
                    agg_w <- rep(1, length(uniq_keys))
                }
                edge_df <- data.frame(
                    source = edge_df$source[first_idx],
                    target = edge_df$target[first_idx],
                    weight = as.numeric(agg_w),
                    stringsAsFactors = FALSE
                )
            } else {
                # Warn if there are duplicates but aggregation is disabled
                if (duplicates > 0) {
                    aggregation_warnings <- c(aggregation_warnings,
                        sprintf(.("Warning: %d duplicate edge(s) detected but not aggregated. Network metrics may be inflated. Enable 'Duplicate edge aggregation' for accurate statistics."), duplicates))
                }
            }

            # Create edge list matrix from aggregated data
            edgelist <- as.matrix(edge_df[, c("source", "target")])
            weights <- edge_df$weight

            # Calculate network metrics after aggregation.
            # Canonical node order MUST match arcplot AND igraph, both of which derive nodes
            # as unique(as.vector(t(edgelist))) (row-major, first appearance).
            vlabels <- unique(as.vector(t(edgelist)))
            vlabels_display <- unname(escaped_to_label[vlabels])
            n_nodes <- length(vlabels)
            n_edges <- nrow(edgelist)

            # Display self-loops warning as Notice
            if (self_loops > 0) {
                private$.addNotice("WARNING", .("Self-Loops Removed"), sprintf(.("%d self-loop(s) detected and removed from the network."), self_loops))
            }

            # Display aggregation warnings as Notices
            if (length(aggregation_warnings) > 0) {
                for (i in seq_along(aggregation_warnings)) {
                    # Remove the "Warning: " prefix from the message since the notice type already indicates it's a warning
                    msg <- sub("^Warning: ", "", aggregation_warnings[i])
                    private$.addNotice("WARNING", .("Edge Aggregation"), msg)
                }
            }

            # Check for small/large networks with tiered warnings
            if (n_nodes < 3) {
                private$.addNotice("STRONG_WARNING", .("Trivial Network"), .("Network has fewer than 3 nodes. Centrality measures are not meaningful for networks this small. Consider adding more entities or combining data."))
            } else if (n_nodes < 5) {
                private$.addNotice("STRONG_WARNING", .("Very Small Network"), .("Network has fewer than 5 nodes. Centrality measures may be unreliable. Interpret with caution and consider qualitative assessment instead of quantitative metrics."))
            } else if (n_nodes < 10) {
                private$.addNotice("WARNING", .("Small Network"), .("Small network (< 10 nodes). Centrality measures may show limited variation. Results should be interpreted cautiously."))
            }

            if (n_edges > 10000) {
                private$.addNotice("INFO", .("Large Network"), .("Large network detected. Analysis may take longer."))
            }

            if (n_edges > 50000) {
                private$.addNotice("ERROR", .("Network Too Large"), .("Network too large (> 50,000 edges after aggregation). Consider filtering or sampling data first."))
                return(NULL)
            }

            # NODE-LEVEL GROUP HANDLING: O(E + V) deterministic, row-order-invariant
            # node-to-group mapping with conflict detection.
            node_groups <- NULL
            group_conflicts <- c()
            if (has_group_col) {
                node_groups <- stats::setNames(rep(NA_character_, length(vlabels)), vlabels)
                src_chr <- as.character(mydata_clean[[source_var]])
                tgt_chr <- as.character(mydata_clean[[target_var]])
                grp_chr <- as.character(mydata_clean[[group_var]])
                grp_chr[!is.na(grp_chr) & !nzchar(trimws(grp_chr))] <- NA_character_

                valid_grp <- !is.na(grp_chr)
                src_grp_map <- if (any(valid_grp)) split(grp_chr[valid_grp], src_chr[valid_grp]) else list()
                tgt_grp_map <- if (any(valid_grp)) split(grp_chr[valid_grp], tgt_chr[valid_grp]) else list()

                for (i in seq_along(vlabels)) {
                    node_escaped <- vlabels[i]
                    node_original <- escaped_to_label[node_escaped]
                    src_grps <- src_grp_map[[node_original]]
                    tgt_grps <- tgt_grp_map[[node_original]]
                    all_grps <- c(src_grps, tgt_grps)

                    if (length(all_grps) > 0) {
                        uniq_grps <- sort(unique(all_grps))
                        if (length(uniq_grps) == 1L) {
                            node_groups[node_escaped] <- uniq_grps[1L]
                        } else {
                            # Deterministic resolution independent of spreadsheet row order:
                            # 1) Unambiguous source-side group (standard edge-table convention)
                            # 2) Otherwise majority vote across all incident edges, breaking
                            #    exact frequency ties alphabetically.
                            uniq_src <- unique(src_grps)
                            if (length(uniq_src) == 1L) {
                                chosen <- uniq_src[1L]
                                rule_desc <- .("source-side group")
                            } else {
                                tab <- table(all_grps)
                                max_cnt <- max(tab)
                                chosen <- sort(names(tab)[tab == max_cnt])[1L]
                                rule_desc <- .("deterministic majority/alphabetical resolution")
                            }
                            group_conflicts <- c(group_conflicts,
                                sprintf(.("Warning: Node '%s' has conflicting groups (%s). Using %s: '%s'"),
                                    node_original, paste(uniq_grps, collapse = ", "), rule_desc, chosen))
                            node_groups[node_escaped] <- chosen
                        }
                    }
                }

                # Disclose ungrouped nodes when optional group variable has missing annotations
                n_ungrouped <- sum(is.na(node_groups))
                if (n_ungrouped > 0) {
                    private$.addNotice("INFO", .("Missing Group Labels"),
                        sprintf(.("%d of %d node(s) have no group label in '%s'. All edges remain included in the network; ungrouped nodes are shown in gray."),
                                n_ungrouped, length(node_groups), group_var))
                }

                # Display group conflict warnings as Notices (sorted by node for invariance)
                if (length(group_conflicts) > 0) {
                    group_conflicts <- sort(group_conflicts)
                    for (i in seq_along(group_conflicts)) {
                        msg <- sub("^Warning: ", "", group_conflicts[i])
                        private$.addNotice("WARNING", .("Group Conflict"), msg)
                    }
                }
            }

            # Create igraph object for analysis
            private$.checkpoint(flush = FALSE)  # Before igraph creation
            g <- tryCatch({
                igraph::graph_from_edgelist(edgelist, directed = self$options$directed)
            }, error = function(e) {
                private$.addNotice("ERROR", .("Invalid Edge Structure"),
                    .("Invalid edge structure detected. Check for duplicate or self-referencing edges."))
                return(NULL)
            })

            if (is.null(g)) return(NULL)

            # INJECT EDGE WEIGHTS into igraph object ONLY when a real weight variable
            # is supplied, preventing unweighted multiplicity counts from leaking into
            # igraph functions whose default `weights = NULL` reads E(g)$weight.
            if (has_weight_var) {
                igraph::E(g)$weight <- weights
                w_vals <- igraph::E(g)$weight
                # A zero DISTANCE means two nodes occupy the same point, which
                # igraph rejects. A zero STRENGTH means no connection and is
                # excluded from shortest-path betweenness below (never passed as 1/0 = Inf).
                if (identical(self$options$weightMode, "distance") &&
                    any(w_vals == 0, na.rm = TRUE)) {
                    private$.addNotice("ERROR", .("Zero Edge Distances"),
                        sprintf(.("%d of %d edge weights in '%s' are zero. In Distance mode a weight is a path length, and a zero-length edge is undefined. Remove those edges, or switch Edge Weight Interpretation to Strength."),
                                sum(w_vals == 0, na.rm = TRUE), length(w_vals), weight_var))
                    return(NULL)
                }
                if (identical(self$options$weightMode, "strength") &&
                    any(w_vals == 0, na.rm = TRUE)) {
                    private$.addNotice("INFO", .("Zero Strength Edges"),
                        sprintf(.("%d of %d edge(s) in '%s' have weight 0. They are excluded from shortest-path betweenness calculations (treated as unreachable) while remaining in edge counts and the arc diagram."),
                                sum(w_vals == 0, na.rm = TRUE), length(w_vals), weight_var))
                }
            }

            # INJECT NODE GROUPS into igraph object as vertex attribute
            if (!is.null(node_groups)) {
                igraph::V(g)$group <- node_groups[igraph::V(g)$name]
            }

            # Get vertex attributes with WEIGHTED degree if weights provided
            private$.checkpoint(flush = FALSE)  # Before degree calculation
            if (has_weight_var) {
                # Use weighted degree (strength)
                degrees <- igraph::strength(g, weights = igraph::E(g)$weight)
            } else {
                # Use unweighted degree
                degrees <- igraph::degree(g)
            }

            # Calculate network density
            private$.checkpoint(flush = FALSE)  # Before density calculation
            g_simple <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
            density <- igraph::edge_density(g_simple)
            n_edges_distinct <- igraph::gsize(g_simple)

            # Domain-specific network density warnings
            if (density > 0.7) {
                private$.addNotice("WARNING", .("High Network Density"), .("Network density is very high (>70%). Consider filtering weak connections or using different visualization methods for clearer interpretation."))
            }

            if (n_nodes > 1 && !igraph::is_connected(g, mode = "weak")) {
                private$.addNotice("WARNING", .("Disconnected Network"),
                    .("This network contains isolated components. Betweenness is computed only within a component, so nodes unreachable from others are assigned zero centrality and the reported hub describes its own component rather than the whole network."))
            }

            # Info for sparse networks (helpful guidance, not a problem)
            if (density < 0.05 && n_nodes > 20) {
                private$.addNotice("INFO", .("Sparse Network"), .("Network is sparse (density <5%). Arc diagram is well-suited for visualizing sparse network structures."))
            }

            return(list(
                edgelist = edgelist,
                weights = weights,
                node_groups = node_groups,
                g = g,
                vlabels = vlabels,              # Escaped names for igraph
                vlabels_display = vlabels_display,  # Original for plotting
                escaped_to_label = escaped_to_label,  # Escaped -> original for stats/report
                has_weight_var = has_weight_var,      # TRUE only if a real weight var supplied
                degrees = degrees,
                n_nodes = n_nodes,
                n_edges = n_edges,
                n_edges_distinct = n_edges_distinct,
                degrees_unweighted = igraph::degree(g),
                density = density
            ))
        },
        
        # Helper method to create the arc plot
        .createArcPlot = function(network_data, theme = NULL) {
            # jamovi renders results in a light or a dark theme and hands the plot
            # the active palette. Everything here used to be hardcoded light --
            # black node borders, near-black arcs, arcplot's own "gray55" labels --
            # so on the dark theme the diagram was drawn in near-invisible ink.
            # theme$color[[1]] is the foreground the rest of the results use.
            fg <- tryCatch(theme$color[[1]], error = function(e) NULL)
            if (is.null(fg) || length(fg) != 1 || is.na(fg) || !nzchar(fg))
                fg <- par("fg")

            # Set up margins and plotting parameters (reserve room for directed subtitle)
            bottom_mar <- if (isTRUE(self$options$directed)) 3 else 2
            oldpar <- par(mar = c(bottom_mar, 2, 3, 2), fg = fg, col = fg,
                          col.axis = fg, col.lab = fg, col.main = fg)
            on.exit(par(oldpar))

            # Calculate node ordering
            private$.checkpoint(flush = FALSE)  # Before node calculations
            ord <- private$.calculateNodeOrdering(network_data)
            
            # Calculate node sizes
            node_sizes <- private$.calculateNodeSizes(network_data)
            
            # Calculate arc widths
            arc_widths <- private$.calculateArcWidths(network_data)
            
            # Get colors
            colors <- private$.getColors(network_data, fg)

            # Create the plot using the vendored arcplot (see R/jjarcdiagram-arcplot.R);
            # arcdiagram is GitHub-only and cannot be installed by jamovi, so it is bundled.
            private$.checkpoint(flush = FALSE)  # Before arcplot rendering
            arcplot(
                network_data$edgelist,
                ordering = ord,
                horizontal = private$.option("horizontal"),
                labels = network_data$vlabels_display,
                show.nodes = self$options$showNodes,
                col.nodes = colors$node_border,
                bg.nodes = colors$node_fill,
                cex.nodes = node_sizes,
                pch.nodes = 21,
                lwd.nodes = 1.5,
                col.arcs = colors$arc_color,
                lwd.arcs = arc_widths,
                cex.labels = self$options$labelSize,
                col.labels = fg
            )
            
            # Add title if specified, and note directionality when directed = TRUE
            if (private$.option("plotTitle") != "") {
                title(main = private$.option("plotTitle"), cex.main = 1.2)
            }
            if (isTRUE(self$options$directed)) {
                title(sub = .("Directed network (statistics respect direction; arcs omit arrowheads)"),
                      line = 1.2, cex.sub = 0.75, col.sub = fg)
            }
            
            # Add legend if groups are specified and color by group is enabled
            if (!is.null(network_data$node_groups) && self$options$colorByGroup && self$options$showLegend) {
                private$.addLegend(colors, fg)
            }
        },
        
        # Helper method to calculate node ordering
        .calculateNodeOrdering = function(network_data) {
            vlabels <- network_data$vlabels
            vlabels_display <- network_data$vlabels_display
            # In Distance mode, summed edge distances mark peripheral nodes;
            # sort by unweighted connection count instead so hubs rank by connectivity.
            deg_metric <- if (isTRUE(network_data$has_weight_var) &&
                              identical(self$options$weightMode, "distance")) {
                network_data$degrees_unweighted
            } else {
                network_data$degrees
            }
            node_groups <- network_data$node_groups

            ord <- switch(private$.option("sortNodes"),
                "none" = seq_along(vlabels),
                "name" = order(vlabels_display, decreasing = self$options$sortDecreasing),
                "degree" = order(deg_metric, decreasing = self$options$sortDecreasing),
                "group" = {
                    if (!is.null(node_groups)) {
                        # node_groups is already a named vector mapping node labels to groups
                        if (all(is.na(node_groups))) {
                            seq_along(vlabels)
                        } else {
                            order(node_groups, decreasing = self$options$sortDecreasing, na.last = TRUE)
                        }
                    } else {
                        seq_along(vlabels)
                    }
                }
            )

            return(ord)
        },
        
        # Helper method to calculate node sizes
        .calculateNodeSizes = function(network_data) {
            if (self$options$nodeSize == "fixed") {
                node_sizes <- rep(self$options$nodeSizeValue, length(network_data$vlabels))
            } else {
                # Size logarithmically by connectivity (weighted strength in Strength mode;
                # unweighted connection count in Distance mode so peripheral nodes do not grow largest).
                deg_metric <- if (isTRUE(network_data$has_weight_var) &&
                                  identical(self$options$weightMode, "distance")) {
                    network_data$degrees_unweighted
                } else {
                    network_data$degrees
                }
                node_sizes <- log(deg_metric + 1) * self$options$nodeSizeValue / 2 + 0.5
                node_sizes <- pmax(node_sizes, 0.5)  # Minimum size
            }
            return(node_sizes)
        },
        
        # Helper method to calculate arc widths
        .calculateArcWidths = function(network_data) {
            if (self$options$arcWidth == "fixed" || !isTRUE(network_data$has_weight_var)) {
                arc_widths <- rep(self$options$arcWidthValue, length(network_data$weights))
            } else {
                # In Distance mode, shorter distances represent stronger/closer links,
                # so invert weights before mapping to arc thickness.
                w_visual <- if (identical(self$options$weightMode, "distance")) {
                    1 / network_data$weights
                } else {
                    network_data$weights
                }
                if (all(w_visual == w_visual[1])) {
                    # All weights are the same
                    arc_widths <- rep(self$options$arcWidthValue, length(w_visual))
                } else {
                    # Ensure strictly increasing range across the entire [0.1, 5] schema domain
                    # (at default arcWidthValue = 1.0 this remains c(0.3, 2.0)).
                    min_w <- min(0.3, self$options$arcWidthValue * 0.5)
                    max_w <- self$options$arcWidthValue * 2
                    arc_widths <- scales::rescale(w_visual, to = c(min_w, max_w))
                }
            }
            return(arc_widths)
        },
        
        # Helper method to get color schemes
        .getColors = function(network_data, fg = par("fg")) {
            node_groups <- network_data$node_groups  # Now using node-level groups

            if (!is.null(node_groups) && self$options$colorByGroup) {
                # Get unique groups (node-level)
                unique_groups <- unique(node_groups)
                unique_groups <- unique_groups[!is.na(unique_groups)]
                n_groups <- length(unique_groups)

                # Get color palette (color-blind safe)
                # Use viridis-based palettes for better accessibility
                if (n_groups <= 8) {
                    # viridis "turbo" palette - excellent for categorical data, CB-safe
                    group_colors <- viridisLite::turbo(n_groups, begin = 0.1, end = 0.9)
                } else if (n_groups <= 12) {
                    # Extend to 12 colors with viridis
                    group_colors <- viridisLite::viridis(n_groups, option = "H")
                } else if (n_groups <= 24) {
                    # Use HCL-based palette for many groups
                    group_colors <- grDevices::hcl.colors(n_groups, palette = "Dark 3")
                } else {
                    # For very large number of groups, use polychrome approach
                    group_colors <- grDevices::hcl.colors(n_groups, palette = "Zissou 1")
                }
                names(group_colors) <- unique_groups

                # Assign colors to vertices using node_groups directly
                # node_groups is already a named vector mapping node labels to groups
                node_fill <- group_colors[node_groups]
                node_fill[is.na(node_fill)] <- "lightgray"
                node_border <- fg

                # Arc colors: Support multiple coloring modes
                edgelist <- network_data$edgelist
                source_groups <- node_groups[edgelist[, 1]]
                target_groups <- node_groups[edgelist[, 2]]

                arc_color_mode <- self$options$arcColorMode

                if (arc_color_mode == "source") {
                    # Color arcs by SOURCE node's group
                    arc_colors <- group_colors[source_groups]
                    arc_colors[is.na(arc_colors)] <- "gray50"
                } else if (arc_color_mode == "target") {
                    # Color arcs by TARGET node's group
                    arc_colors <- group_colors[target_groups]
                    arc_colors[is.na(arc_colors)] <- "gray50"
                } else if (arc_color_mode == "gradient") {
                    # Gradient: Blend source and target colors
                    arc_colors <- character(nrow(edgelist))
                    for (i in seq_len(nrow(edgelist))) {
                        src_color <- group_colors[source_groups[i]]
                        tgt_color <- group_colors[target_groups[i]]

                        if (!is.na(src_color) && !is.na(tgt_color)) {
                            # Blend the two colors (simple average in RGB space)
                            src_rgb <- col2rgb(src_color) / 255
                            tgt_rgb <- col2rgb(tgt_color) / 255
                            blended_rgb <- (src_rgb + tgt_rgb) / 2
                            arc_colors[i] <- rgb(blended_rgb[1], blended_rgb[2], blended_rgb[3])
                        } else if (!is.na(src_color)) {
                            arc_colors[i] <- src_color
                        } else if (!is.na(tgt_color)) {
                            arc_colors[i] <- tgt_color
                        } else {
                            arc_colors[i] <- "gray50"
                        }
                    }
                } else {
                    # Default to source mode
                    arc_colors <- group_colors[source_groups]
                    arc_colors[is.na(arc_colors)] <- "gray50"
                }

                # Apply transparency to arc colors
                arc_color <- adjustcolor(arc_colors, alpha.f = self$options$arcTransparency)

            } else {
                # Default colors
                node_fill <- "lightblue"
                node_border <- fg
                arc_color <- adjustcolor(fg, alpha.f = self$options$arcTransparency)
                group_colors <- NULL
            }

            return(list(
                node_fill = node_fill,
                node_border = node_border,
                arc_color = arc_color,
                group_colors = group_colors
            ))
        },
        
        # Helper method to add legend
        .addLegend = function(colors, fg = par("fg")) {
            if (!is.null(colors$group_colors)) {
                legend("topright", 
                      legend = names(colors$group_colors),
                      fill = colors$group_colors,
                      cex = 0.8,
                      text.col = fg,
                      border = fg,
                      title.col = fg,
                      title = .("Groups"))
            }
        },
        
        # Helper method to generate network statistics
        .generateNetworkStats = function(network_data) {
            g <- network_data$g
            
            # Basic network metrics with clinical interpretations
            density_interp <- ifelse(network_data$density > 0.5, .("highly"), ifelse(network_data$density > 0.2, .("moderately"), .("sparsely")))
            is_weak_conn <- igraph::is_connected(g, mode = "weak")
            if (isTRUE(self$options$directed)) {
                is_strong_conn <- igraph::is_connected(g, mode = "strong")
                conn_yn <- ifelse(is_weak_conn, .("Yes"), .("No"))
                connectivity_interp <- if (is_strong_conn) {
                    .("strongly connected directed network")
                } else if (is_weak_conn) {
                    .("weakly connected only; not strongly connected along edge directions")
                } else {
                    .("disconnected network with isolated components")
                }
            } else {
                conn_yn <- ifelse(is_weak_conn, .("Yes"), .("No"))
                connectivity_interp <- ifelse(is_weak_conn, .("connected network"), .("network with isolated components"))
            }
            
            stats_text <- paste(
                .("<h3> Network Statistics</h3>"),
                .("<h4>Basic Metrics:</h4>"),
                "<ul>",
                paste(.("<li><strong>Number of Nodes:</strong>"), network_data$n_nodes, "</li>"),
                if (!is.null(network_data$n_edges_distinct) &&
                    network_data$n_edges_distinct != network_data$n_edges) {
                    sprintf(.("<li><strong>Number of Edges:</strong> %d (%d distinct node pairs; density below is computed on the distinct pairs)</li>"),
                            network_data$n_edges, network_data$n_edges_distinct)
                } else {
                    sprintf(.("<li><strong>Number of Edges:</strong> %d</li>"), network_data$n_edges)
                },
                paste(.("<li><strong>Network Density:</strong>"), round(network_data$density, 4), sprintf(.("(%s connected)"), density_interp), "</li>"),
                paste(.("<li><strong>Is Connected:</strong>"), conn_yn, sprintf(.("(%s)"), connectivity_interp), "</li>"),
                "</ul>",
                sep = "\n"
            )
            
            # Add clinical interpretation summary
            clinical_summary <- paste(
                .("<h4> Clinical Interpretation:</h4>"),
                "<div style='background-color: rgba(33, 152, 255, 0.07); padding: 10px; margin: 10px 0; border-left: 4px solid #4CAF50; color: inherit;'>",
                sprintf(.("<p><strong>Network Overview:</strong> This network contains %d entities with %d relationships, and is %s connected (density = %.3f).</p>"),
                        network_data$n_nodes, network_data$n_edges, density_interp, network_data$density),
                .("<p>Density is the proportion of distinct possible edges observed. It does not establish shared biological pathways, causation, or clinical importance.</p>"),
                "</div>",
                sep = "\n"
            )
            
            stats_text <- paste(stats_text, clinical_summary, sep = "\n")
            
            # Centrality measures with clinical context
            if (network_data$n_nodes > 1) {
                private$.checkpoint(flush = FALSE)  # Before expensive centrality calculations

                has_weights <- isTRUE(network_data$has_weight_var)
                is_dir <- isTRUE(self$options$directed)

                if (has_weights) {
                    if (self$options$weightMode == "strength") {
                        # STRENGTH MODE: higher weight = stronger tie.
                        # Convert to reciprocal edge length 1/w (Opsahl, Agneessens &
                        # Skvoretz 2010, alpha = 1).
                        # 1) Zero-strength edges mean no connection: DELETE them before
                        #    shortest-path computation rather than passing 1/0 = Inf.
                        # 2) Scale positive inverse weights by median(pos_w) to O(1).
                        # 3) If within-graph dynamic range still exceeds 1e8 (where igraph's
                        #    C absolute epsilon silently corrupts shortest paths), compute
                        #    exact relative-tolerance Brandes/Dijkstra for n <= 500, and
                        #    capture any igraph warning into a visible notice.
                        raw_w <- igraph::E(g)$weight
                        zero_edges <- which(raw_w == 0)
                        if (length(zero_edges) == length(raw_w)) {
                            betweenness <- stats::setNames(rep(0, igraph::vcount(g)), igraph::V(g)$name)
                        } else {
                            g_btw <- if (length(zero_edges) > 0) igraph::delete_edges(g, zero_edges) else g
                            pos_w <- igraph::E(g_btw)$weight
                            inv_weights <- stats::median(pos_w) / pos_w
                            dyn_range <- max(pos_w) / min(pos_w)
                            if (dyn_range > 1e8 && igraph::vcount(g_btw) <= 500L) {
                                btw_sub <- private$.exactWeightedBetweenness(g_btw, 1 / pos_w)
                                if (dyn_range > 1e14) {
                                    private$.addNotice('STRONG_WARNING', .('Extreme Edge Weight Range'),
                                        .('Edge weights span more than 14 orders of magnitude, exceeding standard double-precision arithmetic for path-length sums. Consider log-transforming or rescaling weights.'))
                                }
                            } else {
                                ig_warn <- NULL
                                btw_sub <- withCallingHandlers(
                                    igraph::betweenness(g_btw, weights = inv_weights),
                                    warning = function(w) {
                                        ig_warn <<- conditionMessage(w)
                                        invokeRestart("muffleWarning")
                                    }
                                )
                                if (!is.null(ig_warn)) {
                                    private$.addNotice('STRONG_WARNING', .('Betweenness Precision Warning'),
                                        sprintf(.('Edge weights span a wide numeric range and triggered a numerical precision warning in igraph (%s). Consider rescaling edge weights.'), ig_warn))
                                }
                            }
                            betweenness <- stats::setNames(rep(0, igraph::vcount(g)), igraph::V(g)$name)
                            betweenness[names(btw_sub)] <- btw_sub
                        }
                        degree_label <- if (is_dir) .("total weighted strength, in + out") else .("weighted strength")
                    } else {
                        # DISTANCE MODE: Higher weights = longer paths.
                        # Scale positive distances by median to O(1); fall back to exact
                        # relative-tolerance Brandes/Dijkstra when dynamic range > 1e8.
                        raw_d <- igraph::E(g)$weight
                        scaled_d <- raw_d / stats::median(raw_d)
                        dyn_range <- max(raw_d) / min(raw_d)
                        if (dyn_range > 1e8 && igraph::vcount(g) <= 500L) {
                            betweenness <- private$.exactWeightedBetweenness(g, raw_d)
                            if (dyn_range > 1e14) {
                                private$.addNotice('STRONG_WARNING', .('Extreme Edge Weight Range'),
                                    .('Edge distances span more than 14 orders of magnitude, exceeding standard double-precision arithmetic for path-length sums. Consider rescaling distances.'))
                            }
                        } else {
                            ig_warn <- NULL
                            betweenness <- withCallingHandlers(
                                igraph::betweenness(g, weights = scaled_d),
                                warning = function(w) {
                                    ig_warn <<- conditionMessage(w)
                                    invokeRestart("muffleWarning")
                                }
                            )
                            if (!is.null(ig_warn)) {
                                private$.addNotice('STRONG_WARNING', .('Betweenness Precision Warning'),
                                    sprintf(.('Edge distances span a wide numeric range and triggered a numerical precision warning in igraph (%s). Consider rescaling edge distances.'), ig_warn))
                            }
                        }
                        degree_label <- if (is_dir) .("total weighted distance, in + out") else .("weighted distance")
                    }
                } else {
                    # Unweighted network: pass weights = NA explicitly so E(g)$weight
                    # can never be read implicitly by igraph::betweenness().
                    betweenness <- igraph::betweenness(g, weights = NA)
                    degree_label <- if (is_dir) .("total connections, in + out") else .("connections")
                }

                # HANDLE DISCONNECTED GRAPHS: Filter out Inf/NA values
                betweenness[is.infinite(betweenness) | is.na(betweenness)] <- 0

                # Get key nodes for clinical interpretation (tie-aware & row-order-invariant)
                valid_degrees <- network_data$degrees[is.finite(network_data$degrees) & !is.na(network_data$degrees)]
                valid_betweenness <- betweenness[is.finite(betweenness) & !is.na(betweenness)]

                if (length(valid_degrees) > 0 && length(valid_betweenness) > 0) {
                    top_deg <- private$.getTopNodes(network_data$degrees, network_data)
                    top_btw <- private$.getTopNodes(betweenness, network_data)
                    highest_degree_node <- top_deg$display
                    highest_betweenness_node <- top_btw$display
                    max_degree <- top_deg$max_val
                    max_betweenness <- top_btw$max_val
                } else {
                    # Fallback if no valid centrality values
                    stats_text <- paste(stats_text,
                        "<div style='background-color: rgba(216, 33, 50, 0.18); padding: 8px; margin: 8px 0; border-left: 4px solid #dc3545; color: inherit;'>",
                        .("<p><strong> Error:</strong> Could not calculate valid centrality measures. Network may be too disconnected or contain invalid data.</p>"),
                        "</div>",
                        sep = "\n"
                    )
                    self$results$networkStats$setContent(stats_text)
                    return()
                }

                stats_text <- paste(stats_text,
                    .("<h4>Centrality Measures:</h4>"),
                    "<ul>",
                    paste(.("<li><strong>Highest Degree:</strong>"), htmltools::htmlEscape(highest_degree_node), " (", round(max_degree, 2), " ", degree_label, ")</li>"),
                    if (max_betweenness > 0) {
                        paste(.("<li><strong>Highest Betweenness:</strong>"), htmltools::htmlEscape(highest_betweenness_node), " (", round(max_betweenness, 2), ")</li>")
                    } else {
                        .("<li><strong>Highest Betweenness:</strong> none (no node lies on a shortest path between two others)</li>")
                    },
                    "</ul>",
                    sep = "\n"
                )

                # Add centrality interpretation (distance-aware & tie-aware)
                deg_val_fmt <- if (has_weights) sprintf("%.2f", max_degree) else sprintf("%.0f", max_degree)
                is_distance_mode <- has_weights && identical(self$options$weightMode, "distance")
                same_top_set <- identical(top_btw$labels, top_deg$labels)

                centrality_interp <- paste(
                    "<div style='background-color: rgba(255, 88, 88, 0.06); padding: 8px; margin: 8px 0; border-left: 4px solid #FF6B6B; color: inherit;'>",
                    if (is_distance_mode) {
                        if (top_deg$n_tied > 1L) {
                            sprintf(.("<p><strong>Key Players:</strong> '%s' have the largest summed edge distance (%s). Because weights are read as distances here, that marks the most peripheral entities rather than stronger hubs.</p>"),
                                    htmltools::htmlEscape(highest_degree_node), deg_val_fmt)
                        } else {
                            sprintf(.("<p><strong>Key Players:</strong> '%s' has the largest summed edge distance (%s). Because weights are read as distances here, that marks the most peripheral entity rather than a stronger hub.</p>"),
                                    htmltools::htmlEscape(highest_degree_node), deg_val_fmt)
                        }
                    } else {
                        if (top_deg$n_tied > 1L) {
                            sprintf(.("<p><strong>Key Players:</strong> '%s' have the largest summed edge metric (%s %s).</p>"),
                                    htmltools::htmlEscape(highest_degree_node), deg_val_fmt, degree_label)
                        } else {
                            sprintf(.("<p><strong>Key Players:</strong> '%s' has the largest summed edge metric (%s %s).</p>"),
                                    htmltools::htmlEscape(highest_degree_node), deg_val_fmt, degree_label)
                        }
                    },
                    if (max_betweenness == 0) {
                        .("<p><strong>Bridging:</strong> Betweenness is zero for every node, so no entity acts as a bridge here: each pair of connected entities is already joined by its own shortest path.</p>")
                    } else if (is_distance_mode) {
                        sprintf(.("<p><strong> Bridge Entity:</strong> '%s' has the highest betweenness centrality (%.2f) along shortest-distance paths, indicating an important bridge role between different network regions.</p>"),
                                htmltools::htmlEscape(highest_betweenness_node), max_betweenness)
                    } else if (!same_top_set) {
                        sprintf(.("<p><strong> Bridge Entity:</strong> '%s' has the highest betweenness centrality, indicating it serves as an important bridge between different network regions.</p>"),
                                htmltools::htmlEscape(highest_betweenness_node))
                    } else if (top_deg$n_tied > 1L) {
                        .("<p><strong> Central Hub:</strong> The same tied entities serve as both the most connected nodes and the main bridges in the network.</p>")
                    } else {
                        .("<p><strong> Central Hub:</strong> The same entity serves as both the most connected node and the main bridge in the network.</p>")
                    },
                    "</div>",
                    sep = "\n"
                )

                stats_text <- paste(stats_text, centrality_interp, sep = "\n")
            }

            # NODE-LEVEL Group statistics if available
            if (!is.null(network_data$node_groups)) {
                # Count nodes per group (not edges)
                group_counts <- table(network_data$node_groups, useNA = "no")
                n_ungrouped <- sum(is.na(network_data$node_groups))
                group_items <- sprintf("<li><strong>%s:</strong> %d %s</li>",
                                       htmltools::htmlEscape(names(group_counts)),
                                       as.integer(group_counts),
                                       .("nodes"))
                if (n_ungrouped > 0L) {
                    group_items <- c(group_items, sprintf("<li><strong>%s:</strong> %d %s</li>",
                                                          .("Ungrouped (missing)"),
                                                          n_ungrouped,
                                                          .("nodes")))
                }
                stats_text <- paste(stats_text,
                    .("<h4>Group Distribution (Node Counts):</h4>"),
                    "<ul>",
                    paste(group_items, collapse = ""),
                    "</ul>",
                    sep = "\n"
                )
            }
            
            self$results$networkStats$setContent(stats_text)
        },
        
        # Helper method to create assumptions and guidelines
        .createAssumptions = function() {
            assumptions <- paste(
                .("<h3> Network Analysis Assumptions & Guidelines</h3>"),
                "<div style='background-color: rgba(155, 155, 155, 0.06); padding: 15px; margin: 10px 0; border-radius: 5px; color: inherit;'>",

                .("<h4> Data Requirements:</h4>"),
                "<ul style='margin-left: 20px;'>",
                .("<li><strong>Data Format:</strong> Each row represents one relationship/connection</li>"),
                .("<li><strong>Node Variables:</strong> Source and Target must be categorical (factors)</li>"),
                .("<li><strong>Edge Weights:</strong> Optional non-negative numeric values for connection strength or path distance</li>"),
                .("<li><strong>Missing Data:</strong> Rows with missing or blank values in source, target, or weight are excluded and reported; missing group labels leave edges intact and show ungrouped nodes in gray</li>"),
                "</ul>",

                .("<h4> Network Analysis Guidelines:</h4>"),
                "<ul style='margin-left: 20px;'>",
                .("<li><strong>Network Size:</strong> Optimal range is 10-1000 nodes for meaningful analysis</li>"),
                .("<li><strong>Self-loops:</strong> Connections from a node to itself are automatically detected and removed</li>"),
                .("<li><strong>Duplicate Edges:</strong> When 'Duplicate edge aggregation' is enabled (recommended), parallel edges are combined (summing weights in Strength mode, taking minimum distance in Distance mode, or deduplicating in unweighted networks) for accurate centrality calculations. Density is always computed on distinct node pairs</li>"),
                .("<li><strong>Edge Weight Mode:</strong> 'Strength' mode (default) treats higher weights as stronger connections (using reciprocal lengths 1/w for betweenness), suitable for correlations/scores. 'Distance' mode treats higher weights as longer paths, suitable for costs/distances (note: dissociation constants Kd are inverse\u2014use Distance mode or 1/Kd)</li>"),
                .("<li><strong>Isolated Nodes:</strong> Because input is an edge list, entities only appear if they have at least one surviving non-self-loop edge; degree-0 isolates and unused factor levels are not plotted</li>"),
                "</ul>",

                .("<h4> Interpretation Notes:</h4>"),
                "<ul style='margin-left: 20px;'>",
                .("<li><strong>Network Density:</strong> As an arbitrary rule of thumb, \u22640.2 (sparse), >0.2\u20130.5 (moderate), >0.5 (dense); note that biological and clinical networks with >30 nodes are typically much sparser (<0.05). Density is computed on distinct node pairs, so it is always between 0 and 1</li>"),
                .("<li><strong>Degree Centrality:</strong> Number of direct connections (total in + out for directed graphs). For weighted networks in Strength mode, uses strength (sum of weights)</li>"),
                .("<li><strong>Betweenness Centrality:</strong> Importance as a bridge along shortest paths between network regions (Freeman, 1977; Opsahl et al., 2010 for reciprocal Strength weighting)</li>"),
                .("<li><strong>Connected Components:</strong> Isolated subnetworks may indicate distinct functional modules. For directed graphs, weak and strong connectivity are distinguished</li>"),
                "</ul>",

                .("<h4> Common Pitfalls & Important Notes:</h4>"),
                "<ul style='margin-left: 20px;'>",
                .("<li><strong>Small Networks:</strong> Centrality measures less reliable with < 10 nodes (warning issued)</li>"),
                .("<li><strong>Duplicate Edges:</strong> If not aggregated, parallel edges inflate degree and strength. Density is unaffected because it is computed on distinct node pairs. Enable 'Duplicate edge aggregation' to fix the centrality measures</li>"),
                .("<li><strong>Weight Interpretation:</strong> Ensure 'Edge weight interpretation' matches your data. Most biological/clinical networks use 'Strength' mode</li>"),
                .("<li><strong>Group Conflicts:</strong> If a node appears with multiple group labels, warnings are issued and a deterministic rule (unambiguous source-side group, or majority/alphabetical resolution) is used</li>"),
                .("<li><strong>Arc Coloring:</strong> Choose 'Source', 'Target', or 'Blended' mode to control how arcs are colored when grouping is enabled</li>"),
                .("<li><strong>Directed Networks:</strong> When 'Directed network' is enabled, statistics respect edge direction but the arc diagram visualization does NOT display arrows. This is a limitation of the arc plot style</li>"),
                .("<li><strong>Disconnected Graphs:</strong> Betweenness is computed only within each connected component, so cross-component paths are absent and warnings are issued</li>"),
                .("<li><strong>Clinical Relevance:</strong> High connectivity doesn't always imply biological significance. Consider domain knowledge when interpreting results</li>"),
                "</ul>",

                "</div>",
                sep = "\n"
            )
            return(assumptions)
        },
        
        # Helper method to generate copy-ready report sentence
        .generateReportSentence = function(network_data) {
            # Get analysis type for context (avoid "a network network" under custom preset)
            analysis_phrase <- switch(self$options$analysisPreset,
                "gene_interaction" = .("a gene interaction network"),
                "patient_network" = .("a patient similarity network"),
                "pathway_network" = .("a biological pathway network"),
                "comorbidity_network" = .("a disease co-occurrence network"),
                .("the network")
            )
            
            # Generate density interpretation
            density_desc <- ifelse(network_data$density > 0.5, .("highly connected"), 
                                 ifelse(network_data$density > 0.2, .("moderately connected"), .("sparsely connected")))

            # Disclose distinct pairs when parallel edges remain unaggregated
            edge_phrase <- if (!is.null(network_data$n_edges_distinct) &&
                               network_data$n_edges_distinct != network_data$n_edges) {
                sprintf(.("%d relationships (%d distinct node pairs)"),
                        network_data$n_edges, network_data$n_edges_distinct)
            } else {
                sprintf(.("%d relationships"), network_data$n_edges)
            }

            is_disconnected <- network_data$n_nodes > 1 && !igraph::is_connected(network_data$g, mode = "weak")
            disconn_clause <- if (is_disconnected) {
                paste0(" ", .("Note that the network is disconnected (contains multiple isolated components), so hub rankings reflect a single component rather than the full network."))
            } else {
                ""
            }
            
            # Create the copy-ready sentence
            report_html <- paste(
                .("<h3> Copy-Ready Analysis Summary</h3>"),
                "<div style='background-color: rgba(33, 159, 33, 0.1); padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #4CAF50; color: inherit;'>",
                
                sprintf(.("<p><strong>Network Summary:</strong> Arc diagram analysis of %s revealed %d entities connected by %s, with a network density of %.3f indicating %s structure.</p>"),
                        analysis_phrase, network_data$n_nodes, edge_phrase, network_data$density, density_desc),
                
                if (network_data$n_nodes > 1) {
                    top_deg <- private$.getTopNodes(network_data$degrees, network_data)
                    if (isTRUE(network_data$has_weight_var) &&
                        identical(self$options$weightMode, "distance")) {
                        top_unw <- private$.getTopNodes(network_data$degrees_unweighted, network_data)
                        if (top_unw$n_tied > 1L) {
                            sprintf(.("<p><strong>Key Findings:</strong> The entities '%s' have the most connections (%.0f). Edge weights were interpreted as distances, so they describe path length rather than tie strength and do not rank hubs; see the network statistics panel for betweenness-based centrality.%s</p>"),
                                    htmltools::htmlEscape(top_unw$display), top_unw$max_val, disconn_clause)
                        } else {
                            sprintf(.("<p><strong>Key Findings:</strong> The entity '%s' has the most connections (%.0f). Edge weights were interpreted as distances, so they describe path length rather than tie strength and do not rank hubs; see the network statistics panel for betweenness-based centrality.%s</p>"),
                                    htmltools::htmlEscape(top_unw$display), top_unw$max_val, disconn_clause)
                        }
                    } else if (isTRUE(network_data$has_weight_var)) {
                        if (top_deg$n_tied > 1L) {
                            sprintf(.("<p><strong>Key Findings:</strong> The entities '%s' emerged as the most highly connected hubs with %.2f weighted connections, suggesting a central role in the network.%s</p>"),
                                    htmltools::htmlEscape(top_deg$display), top_deg$max_val, disconn_clause)
                        } else {
                            sprintf(.("<p><strong>Key Findings:</strong> The entity '%s' emerged as the most highly connected hub with %.2f weighted connections, suggesting a central role in the network.%s</p>"),
                                    htmltools::htmlEscape(top_deg$display), top_deg$max_val, disconn_clause)
                        }
                    } else {
                        if (top_deg$n_tied > 1L) {
                            sprintf(.("<p><strong>Key Findings:</strong> The entities '%s' emerged as the most highly connected hubs with %.0f direct connections, suggesting a central role in the network.%s</p>"),
                                    htmltools::htmlEscape(top_deg$display), top_deg$max_val, disconn_clause)
                        } else {
                            sprintf(.("<p><strong>Key Findings:</strong> The entity '%s' emerged as the most highly connected hub with %.0f direct connections, suggesting a central role in the network.%s</p>"),
                                    htmltools::htmlEscape(top_deg$display), top_deg$max_val, disconn_clause)
                        }
                    }
                } else {
                    ""
                },

                if (!is.null(network_data$node_groups)) {
                    # Count unique NODE groups (not edge groups)
                    group_count <- length(unique(network_data$node_groups[!is.na(network_data$node_groups)]))
                    if (isTRUE(self$options$colorByGroup)) {
                        sprintf(.("<p><strong>Grouping:</strong> %d nodes were categorized into %d distinct groups, with color-coding revealing potential functional clusters or classifications.</p>"),
                                sum(!is.na(network_data$node_groups)), group_count)
                    } else {
                        sprintf(.("<p><strong>Grouping:</strong> %d nodes were categorized into %d distinct groups.</p>"),
                                sum(!is.na(network_data$node_groups)), group_count)
                    }
                } else {
                    ""
                },
                
                .("<p><em> Tip: Copy the text above for use in reports, presentations, or publications.</em></p>"),
                "</div>",
                sep = "\n"
            )
            
            self$results$reportSentence$setContent(report_html)
        },

        # Helper method to generate glossary panel
        .generateGlossary = function() {
            glossary_html <- paste(
                .("<h3> Network Analysis Glossary</h3>"),
                "<div style='background-color: rgba(155, 155, 155, 0.06); padding: 15px; margin: 10px 0; border-radius: 5px; border-left: 4px solid #2196F3; color: inherit;'>",

                "<dl style='margin: 0;'>",

                "<dt style='font-weight: bold; margin-top: 10px;'>", .("Node (Vertex)"), "</dt>",
                "<dd style='margin-left: 20px; margin-bottom: 8px;'>",
                .("An entity or element in the network (e.g., gene, patient, disease, protein). Nodes are the fundamental building blocks of networks, representing the items being studied."),
                "</dd>",

                "<dt style='font-weight: bold; margin-top: 10px;'>", .("Edge (Arc)"), "</dt>",
                "<dd style='margin-left: 20px; margin-bottom: 8px;'>",
                .("A connection or relationship between two nodes (e.g., regulatory link, similarity score, co-occurrence). In arc diagrams, edges are visualized as curved arcs connecting nodes."),
                "</dd>",

                "<dt style='font-weight: bold; margin-top: 10px;'>", .("Degree Centrality"), "</dt>",
                "<dd style='margin-left: 20px; margin-bottom: 8px;'>",
                .("Number of direct connections a node has (total in + out for directed networks; weighted sum of strengths in Strength mode). High-degree nodes are 'hubs' that interact with many other entities. <strong>Clinical interpretation:</strong> A gene with high degree might be a master regulator; a patient with high degree might share characteristics with many others."),
                "</dd>",

                "<dt style='font-weight: bold; margin-top: 10px;'>", .("Betweenness Centrality"), "</dt>",
                "<dd style='margin-left: 20px; margin-bottom: 8px;'>",
                .("Measures how often a node lies on shortest paths between other nodes (Freeman, 1977; reciprocal tie strength 1/w per Opsahl et al., 2010 in Strength mode). High betweenness indicates the node serves as a 'bridge' connecting different network regions. <strong>Clinical interpretation:</strong> Entities with high betweenness may be critical bottlenecks or gateways in biological pathways."),
                "</dd>",

                "<dt style='font-weight: bold; margin-top: 10px;'>", .("Closeness Centrality"), "</dt>",
                "<dd style='margin-left: 20px; margin-bottom: 8px;'>",
                .("Reciprocal of the sum (or average) shortest-path distance from a node to all other reachable nodes. Higher closeness means the node can reach others more quickly. <strong>Note:</strong> Not reported in this module; in disconnected graphs, infinite inter-component distances yield zero or undefined closeness rather than Inf."),
                "</dd>",

                "<dt style='font-weight: bold; margin-top: 10px;'>", .("Network Density"), "</dt>",
                "<dd style='margin-left: 20px; margin-bottom: 8px;'>",
                .("Proportion of actual distinct connections vs. all possible connections (0-1 scale). <strong>Rule-of-thumb bands:</strong> \u22640.2 = sparse, >0.2\u20130.5 = moderate, >0.5 = dense (note: biological and clinical networks with >30 nodes are typically <0.05; density alone does not establish shared biological mechanisms)."),
                "</dd>",

                "<dt style='font-weight: bold; margin-top: 10px;'>", .("Weighted Network"), "</dt>",
                "<dd style='margin-left: 20px; margin-bottom: 8px;'>",
                .("A network where edges have numerical values representing connection strength, correlation, distance, or other quantitative measures. Weights influence how centrality measures are calculated."),
                "</dd>",

                "<dt style='font-weight: bold; margin-top: 10px;'>", .("Directed vs. Undirected"), "</dt>",
                "<dd style='margin-left: 20px; margin-bottom: 8px;'>",
                .("<strong>Directed:</strong> Edges have direction (A\u2192B is different from B\u2192A), suitable for regulatory relationships or causal pathways. <strong>Undirected:</strong> Edges are bidirectional (A-B same as B-A), suitable for similarity or co-occurrence networks. <strong>Note:</strong> Arc diagrams do not display arrows even for directed networks."),
                "</dd>",

                "<dt style='font-weight: bold; margin-top: 10px;'>", .("Isolated Node"), "</dt>",
                "<dd style='margin-left: 20px; margin-bottom: 8px;'>",
                .("A node with degree = 0 (no connections). Because this module reads an edge list and removes self-loops, degree-0 isolated nodes and unused factor levels do not appear in the diagram."),
                "</dd>",

                "<dt style='font-weight: bold; margin-top: 10px;'>", .("Connected Component"), "</dt>",
                "<dd style='margin-left: 20px; margin-bottom: 8px;'>",
                .("In an undirected network (or weakly connected directed network), a subgroup of nodes joined by undirected paths regardless of edge direction. In a strongly connected directed component, every node can reach every other node following edge directions. Disconnected networks have multiple separate components, which may represent distinct functional modules or pathways."),
                "</dd>",

                "</dl>",

                "<p style='margin-top: 15px; font-style: italic; opacity: 0.75;'>",
                .("Tip: Use this glossary as a quick reference when interpreting network statistics and visualizations."),
                "</p>",

                "</div>",
                sep = "\n"
            )

            self$results$glossary$setContent(glossary_html)
        }
    )
)
