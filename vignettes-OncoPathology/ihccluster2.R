cluster_ihc <- function(
        data,
        markers,
        id_col       = NULL,                # optional case id column
        method       = c("pam", "kmodes", "mca"),
        k            = NULL,                # if NULL and auto_k=TRUE → choose automatically
        auto_k       = TRUE,
        weights      = NULL,                # named numeric vector (PAM only). e.g., c(GATA6=1.2, CK5_6=1.2)
        mca_var_pct  = 75,                  # target cumulative % inertia for MCA
        stability_B  = 0,                   # bootstrap iters for PAM stability (0 = skip)
        seed         = 42,
        return_heatmap = TRUE               # build ComplexHeatmap object for PAM workflow
) {
    # ---- deps ----
    requireNamespace("dplyr"); requireNamespace("purrr")
    requireNamespace("cluster"); requireNamespace("factoextra")
    if (return_heatmap) { requireNamespace("ComplexHeatmap"); requireNamespace("circlize") }
    method <- match.arg(method)

    set.seed(seed)

    # ---- pick id ----
    id <- if (!is.null(id_col) && id_col %in% names(data)) data[[id_col]] else seq_len(nrow(data))

    # ---- slice to markers ----
    X <- data[, markers, drop = FALSE]

    # ---- helpers ----
    .is_num <- vapply(X, is.numeric, logical(1))
    .numeric_cols <- names(X)[.is_num]
    .cat_cols     <- names(X)[!.is_num]

    # For summaries later (avoid ordered vs factor issues when pivoting)
    X_for_pivot <- dplyr::mutate(X, dplyr::across(dplyr::all_of(.cat_cols), as.character))

    out <- list(method = method)

    if (method == "pam") {
        # -------- PAM on Gower (mixed types) --------
        # weights
        w <- rep(1, length(markers)); names(w) <- markers
        if (!is.null(weights)) {
            stopifnot(all(names(weights) %in% markers))
            w[names(weights)] <- weights
        }
        d <- cluster::daisy(X, metric = "gower", weights = w)
        out$distance <- d

        # choose k
        if (auto_k || is.null(k)) {
            ks <- 2:8
            sil_df <- purrr::map_df(ks, function(kk) {
                fit <- cluster::pam(d, k = kk, diss = TRUE)
                sil <- cluster::silhouette(fit)
                tibble::tibble(k = kk, avg_sil = mean(sil[, "sil_width"]))
            })
            k <- sil_df$k[which.max(sil_df$avg_sil)]
            out$sil_df <- sil_df
        }

        fit <- cluster::pam(d, k = k, diss = TRUE)
        clusters <- factor(fit$clustering, labels = paste0("C", seq_len(k)))
        out$fit <- fit
        out$k   <- k
        out$clusters <- clusters
        out$medoids  <- if (!is.null(id_col)) id[fit$id.med] else fit$id.med

        # silhouette plot (ggplot)
        sil <- cluster::silhouette(fit)
        out$silhouette <- sil
        out$sil_plot <- factoextra::fviz_silhouette(sil, label = FALSE)

        # summaries
        df_aug <- dplyr::mutate(data, cluster = clusters)
        # numeric summary
        summ_num <- if (length(.numeric_cols)) {
            df_aug %>%
                dplyr::group_by(cluster) %>%
                dplyr::summarise(dplyr::across(dplyr::all_of(.numeric_cols),
                                               list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)),
                                               .names = "{.col}_{.fn}"))
        } else NULL

        # categorical summary
        summ_cat <- NULL
        if (length(.cat_cols)) {
            summ_cat <- df_aug %>%
                dplyr::select(cluster, dplyr::all_of(.cat_cols)) %>%
                dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
                tidyr::pivot_longer(-cluster, names_to = "marker", values_to = "level") %>%
                dplyr::group_by(cluster, marker, level) %>%
                dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
                dplyr::group_by(cluster, marker) %>%
                dplyr::mutate(pct = 100 * n / sum(n)) %>%
                dplyr::arrange(marker, cluster, dplyr::desc(pct))
        }
        out$summ_num <- summ_num
        out$summ_cat <- summ_cat

        # optional heatmap (numeric-only for visualization)
        if (isTRUE(return_heatmap)) {
            # best-effort numeric transform: numeric stay numeric; factors→integer codes
            X_num <- as.data.frame(X, stringsAsFactors = FALSE)
            if (length(.cat_cols)) {
                X_num[.cat_cols] <- lapply(X[.cat_cols], function(z) as.numeric(as.factor(z)) - 1L)
            }
            X_num <- as.matrix(X_num)

            # z-score only continuous-looking columns (heuristic: original numeric)
            if (length(.numeric_cols)) {
                X_num[, .numeric_cols] <- scale(X_num[, .numeric_cols, drop = FALSE])
            }

            ord <- order(clusters)
            mat <- t(X_num[ord, , drop = FALSE])
            cl_anno <- data.frame(Cluster = clusters[ord])
            rownames(cl_anno) <- as.character(id)[ord]
            col_fun <- circlize::colorRamp2(c(-2, 0, 2), c("#2c7bb6", "#ffffbf", "#d7191c"))
            ht <- ComplexHeatmap::Heatmap(
                mat, name = "z/score", col = col_fun,
                column_title = "Cases", row_title = "Markers",
                top_annotation = ComplexHeatmap::HeatmapAnnotation(df = cl_anno),
                cluster_rows = TRUE, cluster_columns = FALSE, show_column_names = FALSE
            )
            out$heatmap <- ht
        }

        # optional stability
        if (stability_B > 0) {
            assignments <- matrix(NA_character_, nrow = nrow(X), ncol = stability_B, dimnames = list(as.character(id), NULL))
            for (b in seq_len(stability_B)) {
                idx <- sample.int(nrow(X), replace = TRUE)
                d_b <- cluster::daisy(X[idx, , drop = FALSE], metric = "gower")
                fit_b <- cluster::pam(d_b, k = k, diss = TRUE)
                assignments[as.character(id)[idx], b] <- paste0("C", fit_b$clustering)
            }
            co <- matrix(0, nrow = nrow(X), ncol = nrow(X), dimnames = list(as.character(id), as.character(id)))
            for (b in seq_len(stability_B)) {
                lab <- assignments[, b]
                inbag <- !is.na(lab)
                same <- outer(lab[inbag], lab[inbag], `==`)
                co[inbag, inbag] <- co[inbag, inbag] + same
            }
            co <- co / stability_B
            out$stability_coclust <- co
        }

    } else if (method == "kmodes") {
        # -------- k-modes (nominal) --------
        requireNamespace("klaR")
        # sanitize input: all categorical, drop constants, character safest
        X_km <- X
        X_km[] <- lapply(X_km, as.factor)
        keep <- vapply(X_km, function(x) dplyr::n_distinct(x, na.rm = TRUE) >= 2, logical(1))
        X_km <- X_km[, keep, drop = FALSE]
        X_km2 <- data.frame(lapply(X_km, as.character), check.names = TRUE, stringsAsFactors = FALSE)

        if (auto_k || is.null(k)) {
            ks <- 2:8
            grid <- purrr::map_df(ks, function(kk) {
                fit <- klaR::kmodes(X_km2, modes = kk, iter.max = 50, weighted = FALSE)
                tibble::tibble(k = kk, cost = fit$withindiff)
            })
            k <- grid$k[which.min(grid$cost)]
            out$kmodes_grid <- grid
        }
        fit <- klaR::kmodes(X_km2, modes = k, iter.max = 100, weighted = FALSE)
        clusters <- factor(fit$cluster, labels = paste0("C", seq_len(k)))
        out$fit     <- fit
        out$k       <- k
        out$clusters <- clusters
        out$modes    <- fit$modes

    } else if (method == "mca") {
        # -------- MCA + k-means --------
        requireNamespace("FactoMineR")
        X_cat <- X
        X_cat[] <- lapply(X_cat, as.factor)
        mca <- FactoMineR::MCA(X_cat, graph = FALSE)
        eig <- mca$eig
        cumpct <- cumsum(eig[, "cumulative percentage of variance"])
        nc <- which(cumpct >= mca_var_pct)[1]
        if (is.na(nc) || nc < 2) nc <- max(2, min(5, ncol(X_cat)))  # reasonable fallback
        scores <- mca$ind$coord[, 1:nc, drop = FALSE]

        if (auto_k || is.null(k)) {
            ks <- 2:8
            sil_df <- purrr::map_df(ks, function(kk) {
                cl  <- stats::kmeans(scale(scores), centers = kk, nstart = 50)
                sil <- cluster::silhouette(cl$cluster, stats::dist(scale(scores)))
                tibble::tibble(k = kk, avg_sil = mean(sil[, "sil_width"]))
            })
            k <- sil_df$k[which.max(sil_df$avg_sil)]
            out$sil_df <- sil_df
        }

        km <- stats::kmeans(scale(scores), centers = k, nstart = 100)
        clusters <- factor(km$cluster, labels = paste0("C", seq_len(k)))
        out$fit       <- km
        out$k         <- k
        out$clusters  <- clusters
        out$mca       <- mca
        out$mca_dims  <- nc
        out$silhouette <- cluster::silhouette(km$cluster, stats::dist(scale(scores)))
        out$sil_plot   <- factoextra::fviz_silhouette(out$silhouette, label = FALSE)
    }

    # Attach id for convenience
    out$cluster_df <- tibble::tibble(
        id = id,
        cluster = out$clusters
    )

    class(out) <- c("ihccluster_result", class(out))
    return(out)
}




# 1) Mixed data, PAM on Gower, auto-k:
res_pam <- cluster_ihc(ihc_pre, markers = c("GATA6","CK17","p63","CK5_6","CD44","FOXI1"),
                       id_col = "case_id", method = "pam", auto_k = TRUE,
                       weights = c(GATA6 = 1.2, CK5_6 = 1.2), stability_B = 50)

res_pam$k
res_pam$cluster_df %>% count(cluster)
res_pam$sil_plot
if (!is.null(res_pam$heatmap)) ComplexHeatmap::draw(res_pam$heatmap)

# 2) All-categorical, k-modes:
res_km <- cluster_ihc(ihc, markers = c("GATA6_cat","CK17_cat","p63_cat","CK5_6_cat","CD44_cat","FOXI1_cat"),
                      id_col = "case_id", method = "kmodes", auto_k = TRUE)

res_km$k
res_km$modes
res_km$cluster_df %>% count(cluster)

# 3) All-categorical, MCA + k-means:
res_mca <- cluster_ihc(ihc, markers = c("GATA6_cat","CK17_cat","p63_cat","CK5_6_cat","CD44_cat","FOXI1_cat"),
                       id_col = "case_id", method = "mca", auto_k = TRUE, mca_var_pct = 75)
res_mca$k
res_mca$sil_plot
