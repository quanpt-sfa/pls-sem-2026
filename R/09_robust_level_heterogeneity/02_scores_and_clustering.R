# ==============================================================================
# 02_scores_and_clustering.R — Step 1: Extract exogenous scores & cluster
# ==============================================================================
# Input:  module config, backend env (pls_model or cSEM scores)
# Output: clustering results (groups, k, diagnostics)
# ==============================================================================

# ---- Main entry point --------------------------------------------------------

#' Run Step 1: Scores extraction + clustering
#' @param env list from lh_detect_backend()
#' @param cfg module config
#' @param log_info log object
#' @return list(groups, k_opt, diagnostics, exo_scores)
lh_step1_clustering <- function(env, cfg, log_info) {
  lh_step(log_info, "Step 1 — Exogenous Construct Scores & Clustering")

  # ------------------------------------------------------------------
  # 1a. Extract construct scores
  # ------------------------------------------------------------------
  scores <- env$scores
  if (is.null(scores)) {
    lh_log(log_info, "No pre-computed scores. Re-estimating from raw data...",
           level = "WARN")
    if (is.null(env$data_raw) || is.null(env$base_cfg))
      stop("Cannot re-estimate: need data_path + config_path in config.")
    tmp_model <- lh_estimate_pls(env$data_raw, env$base_cfg, env$active_ind)
    scores <- tmp_model$construct_scores
  }

  all_constructs <- colnames(scores)
  lh_log(log_info, sprintf("Available constructs: %s",
                            paste(all_constructs, collapse = ", ")))

  # ------------------------------------------------------------------
  # 1b. Keep ONLY exogenous constructs (circularity guard)
  # ------------------------------------------------------------------
  exo_names <- cfg$exogenous_constructs
  endo_names <- cfg$endogenous_constructs

  # Double-check: nothing from endo in exo list
  endo_in_exo <- intersect(exo_names, endo_names)
  if (length(endo_in_exo) > 0)
    stop("CIRCULARITY: ", paste(endo_in_exo, collapse = ", "),
         " listed as both exogenous and endogenous.")

  # Check that requested exo are actually in scores
  missing_exo <- setdiff(exo_names, all_constructs)
  if (length(missing_exo) > 0) {
    lh_log(log_info, sprintf("Exogenous constructs not in scores (skipping): %s",
                              paste(missing_exo, collapse = ", ")), level = "WARN")
    exo_names <- intersect(exo_names, all_constructs)
  }
  if (length(exo_names) < 2)
    stop("Need at least 2 exogenous constructs for clustering. Found: ",
         length(exo_names))

  exo_scores <- scores[, exo_names, drop = FALSE]
  lh_log(log_info, sprintf("Exogenous scores: %d obs × %d constructs (%s)",
                            nrow(exo_scores), ncol(exo_scores),
                            paste(exo_names, collapse = ", ")))

  # ------------------------------------------------------------------
  # 1c. Centre / scale
  # ------------------------------------------------------------------
  if (isTRUE(cfg$clustering$center_scores)) {
    exo_scores <- scale(exo_scores, center = TRUE, scale = FALSE)
    lh_log(log_info, "  Scores mean-centred")
  }
  if (isTRUE(cfg$clustering$scale_scores)) {
    exo_scores <- scale(exo_scores, center = FALSE, scale = TRUE)
    lh_log(log_info, "  Scores z-standardised")
  }

  # ------------------------------------------------------------------
  # 1d. Clustering
  # ------------------------------------------------------------------
  method    <- tolower(cfg$clustering$method)
  k_cands   <- cfg$clustering$k_candidates
  seed      <- cfg$seed
  lh_log(log_info, sprintf("Clustering method: %s, k candidates: %s",
                            method, paste(k_cands, collapse = ", ")))

  if (method == "mclust") {
    result <- .lh_cluster_mclust(exo_scores, k_cands, seed, cfg, log_info)
  } else if (method == "hclust") {
    result <- .lh_cluster_hclust(exo_scores, k_cands, seed, cfg, log_info)
  } else {
    result <- .lh_cluster_kmeans(exo_scores, k_cands, seed, cfg, log_info)
  }

  result$exo_scores <- exo_scores
  result
}

# ---- K-means -----------------------------------------------------------------

.lh_cluster_kmeans <- function(scores, k_cands, seed, cfg, log_info) {
  set.seed(seed)
  nstart <- cfg$clustering$kmeans_nstart

  diagnostics <- data.frame(
    k = integer(), WSS = numeric(), Silhouette_Mean = numeric(),
    stringsAsFactors = FALSE
  )
  km_list <- list()

  for (k in k_cands) {
    km <- kmeans(scores, centers = k, nstart = nstart, iter.max = 100)
    km_list[[as.character(k)]] <- km

    sil_val <- tryCatch({
      sil <- cluster::silhouette(km$cluster, dist(scores))
      mean(sil[, "sil_width"])
    }, error = function(e) NA_real_)

    diagnostics <- rbind(diagnostics, data.frame(
      k = k, WSS = km$tot.withinss, Silhouette_Mean = sil_val,
      stringsAsFactors = FALSE
    ))
    lh_log(log_info, sprintf("  k=%d  WSS=%.1f  Silhouette=%.3f",
                              k, km$tot.withinss,
                              ifelse(is.na(sil_val), 0, sil_val)))
  }

  # Select optimal k
  k_opt <- .lh_select_k_silhouette(diagnostics, log_info)
  groups <- km_list[[as.character(k_opt)]]$cluster

  lh_log(log_info, sprintf("Optimal k = %d (silhouette criterion)", k_opt))
  lh_log(log_info, sprintf("  Group sizes: %s",
                            paste(table(groups), collapse = " / ")))

  list(groups = groups, k_opt = k_opt, diagnostics = diagnostics, method = "kmeans")
}

# ---- Hierarchical clustering -------------------------------------------------

.lh_cluster_hclust <- function(scores, k_cands, seed, cfg, log_info) {
  set.seed(seed)
  linkage <- cfg$clustering$hclust_linkage

  d <- dist(scores)
  hc <- hclust(d, method = linkage)

  diagnostics <- data.frame(
    k = integer(), WSS = numeric(), Silhouette_Mean = numeric(),
    stringsAsFactors = FALSE
  )

  for (k in k_cands) {
    grp <- cutree(hc, k = k)

    wss <- sum(sapply(unique(grp), function(g) {
      idx <- which(grp == g)
      if (length(idx) < 2) return(0)
      sum(scale(scores[idx, , drop = FALSE], scale = FALSE)^2)
    }))

    sil_val <- tryCatch({
      sil <- cluster::silhouette(grp, d)
      mean(sil[, "sil_width"])
    }, error = function(e) NA_real_)

    diagnostics <- rbind(diagnostics, data.frame(
      k = k, WSS = wss, Silhouette_Mean = sil_val,
      stringsAsFactors = FALSE
    ))
    lh_log(log_info, sprintf("  k=%d  WSS=%.1f  Silhouette=%.3f",
                              k, wss, ifelse(is.na(sil_val), 0, sil_val)))
  }

  k_opt <- .lh_select_k_silhouette(diagnostics, log_info)
  groups <- cutree(hc, k = k_opt)

  lh_log(log_info, sprintf("Optimal k = %d (silhouette criterion)", k_opt))
  lh_log(log_info, sprintf("  Group sizes: %s",
                            paste(table(groups), collapse = " / ")))

  list(groups = groups, k_opt = k_opt, diagnostics = diagnostics, method = "hclust")
}

# ---- Model-based clustering (mclust) ----------------------------------------

.lh_cluster_mclust <- function(scores, k_cands, seed, cfg, log_info) {
  if (!requireNamespace("mclust", quietly = TRUE))
    stop("mclust package required for method='mclust'. Install: install.packages('mclust')")

  set.seed(seed)
  mc <- mclust::Mclust(scores, G = k_cands)

  if (is.null(mc))
    stop("mclust failed to find any valid model.")

  k_opt <- mc$G
  groups <- mc$classification

  diagnostics <- data.frame(
    k = k_cands,
    BIC = sapply(k_cands, function(k) {
      tryCatch({
        tmp <- mclust::Mclust(scores, G = k)
        if (is.null(tmp)) NA_real_ else tmp$bic
      }, error = function(e) NA_real_)
    }),
    stringsAsFactors = FALSE
  )

  # Add silhouette for consistency
  diagnostics$Silhouette_Mean <- sapply(k_cands, function(k) {
    tryCatch({
      tmp <- mclust::Mclust(scores, G = k)
      if (is.null(tmp)) return(NA_real_)
      sil <- cluster::silhouette(tmp$classification, dist(scores))
      mean(sil[, "sil_width"])
    }, error = function(e) NA_real_)
  })

  lh_log(log_info, sprintf("mclust selected k = %d (BIC criterion)", k_opt))
  lh_log(log_info, sprintf("  Group sizes: %s",
                            paste(table(groups), collapse = " / ")))

  list(groups = groups, k_opt = k_opt, diagnostics = diagnostics, method = "mclust")
}

# ---- k selection helper ------------------------------------------------------

.lh_select_k_silhouette <- function(diagnostics, log_info) {
  valid <- diagnostics[!is.na(diagnostics$Silhouette_Mean), ]
  if (nrow(valid) == 0) {
    lh_log(log_info, "No valid silhouette values; defaulting to k=2", level = "WARN")
    return(min(diagnostics$k))
  }
  k_opt <- valid$k[which.max(valid$Silhouette_Mean)]
  k_opt
}

# ---- Clustering plots --------------------------------------------------------

#' Generate elbow and silhouette plots
#' @param diagnostics data.frame from clustering
#' @param output_dir output directory
#' @param cfg module config
#' @param log_info log object
lh_plot_clustering <- function(diagnostics, k_opt, output_dir, cfg, log_info) {
  if (!isTRUE(cfg$reporting$export_plots)) return(invisible(NULL))

  w   <- cfg$reporting$plot_width
  h   <- cfg$reporting$plot_height
  dpi <- cfg$reporting$plot_dpi
  plot_dir <- file.path(output_dir, "plots")

  # Elbow plot (WSS)
  if ("WSS" %in% names(diagnostics)) {
    p_elbow <- ggplot2::ggplot(diagnostics, ggplot2::aes(x = k, y = WSS)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_vline(xintercept = k_opt, linetype = "dashed", colour = "red") +
      ggplot2::labs(title = "Elbow Plot (Within-cluster SS)",
                    x = "Number of clusters (k)", y = "Total WSS") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::annotate("text", x = k_opt + 0.2, y = max(diagnostics$WSS, na.rm = TRUE),
                         label = paste0("k* = ", k_opt), colour = "red", hjust = 0)
    ggplot2::ggsave(file.path(plot_dir, "elbow.png"), p_elbow,
                     width = w, height = h, dpi = dpi)
    lh_log(log_info, "  Elbow plot saved")
  }

  # Silhouette plot
  if ("Silhouette_Mean" %in% names(diagnostics)) {
    diag_sil <- diagnostics[!is.na(diagnostics$Silhouette_Mean), ]
    if (nrow(diag_sil) > 0) {
      p_sil <- ggplot2::ggplot(diag_sil, ggplot2::aes(x = k, y = Silhouette_Mean)) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_vline(xintercept = k_opt, linetype = "dashed", colour = "blue") +
        ggplot2::labs(title = "Mean Silhouette Width by k",
                      x = "Number of clusters (k)", y = "Mean Silhouette") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::annotate("text", x = k_opt + 0.2,
                           y = max(diag_sil$Silhouette_Mean, na.rm = TRUE),
                           label = paste0("k* = ", k_opt), colour = "blue", hjust = 0)
      ggplot2::ggsave(file.path(plot_dir, "silhouette.png"), p_sil,
                       width = w, height = h, dpi = dpi)
      lh_log(log_info, "  Silhouette plot saved")
    }
  }

  # BIC plot (mclust)
  if ("BIC" %in% names(diagnostics)) {
    diag_bic <- diagnostics[!is.na(diagnostics$BIC), ]
    if (nrow(diag_bic) > 0) {
      p_bic <- ggplot2::ggplot(diag_bic, ggplot2::aes(x = k, y = BIC)) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_vline(xintercept = k_opt, linetype = "dashed", colour = "darkgreen") +
        ggplot2::labs(title = "BIC by Number of Clusters",
                      x = "Number of clusters (k)", y = "BIC") +
        ggplot2::theme_minimal(base_size = 12)
      ggplot2::ggsave(file.path(plot_dir, "bic.png"), p_bic,
                       width = w, height = h, dpi = dpi)
      lh_log(log_info, "  BIC plot saved")
    }
  }

  invisible(NULL)
}

# ---- Export clustering results -----------------------------------------------

#' Save clustering diagnostics and group assignments
lh_export_clustering <- function(clust_result, output_dir, cfg, log_info) {
  tbl_dir <- file.path(output_dir, "tables")

  # k selection table
  write.csv(clust_result$diagnostics,
            file.path(tbl_dir, "clustering_k_selection.csv"),
            row.names = FALSE)
  lh_log(log_info, "  Saved: tables/clustering_k_selection.csv")

  # Group assignments
  groups_df <- data.frame(
    observation = seq_along(clust_result$groups),
    group = clust_result$groups,
    stringsAsFactors = FALSE
  )
  write.csv(groups_df, file.path(tbl_dir, "groups.csv"), row.names = FALSE)
  lh_log(log_info, sprintf("  Saved: tables/groups.csv (%d observations)",
                            nrow(groups_df)))

  invisible(NULL)
}
