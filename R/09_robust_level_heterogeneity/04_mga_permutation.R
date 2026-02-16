# ==============================================================================
# 04_mga_permutation.R — Step 3: Permutation-based Multi-Group Analysis (MGA)
# ==============================================================================
# For each requested path (from config), compare path coefficients across
# groups using permutation test.
#
# If k > 2, runs all pairwise comparisons with p-value adjustment.
# ==============================================================================

# ---- Main entry point --------------------------------------------------------

#' Run Permutation MGA
#' @param env backend environment
#' @param cfg module config
#' @param groups integer vector of group assignments
#' @param k_opt optimal k
#' @param micom_result result from Step 2
#' @param log_info log object
#' @return list(mga_df, pair_results, micom_ok)
lh_step3_mga <- function(env, cfg, groups, k_opt, micom_result, log_info) {
  lh_step(log_info, "Step 3 — Permutation Multi-Group Analysis (MGA)")

  micom_ok <- micom_result$passed_step2
  if (!micom_ok) {
    lh_log(log_info, paste(
      "  WARNING: MICOM Step 2 FAILED — compositional invariance NOT established.",
      "MGA results are EXPLORATORY ONLY."), level = "WARN")
  }

  n_perm       <- cfg$mga$permutations
  alpha        <- cfg$mga$alpha
  seed         <- cfg$seed
  p_adj_method <- cfg$mga$p_adjust_method
  paths_str    <- cfg$mga$paths_to_test

  lh_log(log_info, sprintf("  Permutations: %d | Alpha: %.2f | Adjust: %s",
                            n_perm, alpha, p_adj_method))
  lh_log(log_info, sprintf("  Paths to test: %d", length(paths_str)))

  # Parse paths
  paths_parsed <- lapply(paths_str, lh_parse_path)
  path_labels  <- sapply(paths_str, function(s) s)

  # Generate group pairs
  group_labels <- sort(unique(groups))
  pairs <- combn(group_labels, 2, simplify = FALSE)

  # ------------------------------------------------------------------
  # Estimate PLS per group
  # ------------------------------------------------------------------
  lh_log(log_info, "  Estimating PLS model per group...")

  group_models <- list()
  group_boots  <- list()
  group_data   <- list()

  for (g in group_labels) {
    g_idx <- which(groups == g)
    g_data <- env$data_raw[g_idx, , drop = FALSE]
    group_data[[as.character(g)]] <- g_data

    lh_log(log_info, sprintf("    Group %d: n = %d", g, nrow(g_data)))

    g_model <- tryCatch(
      lh_estimate_pls(g_data, env$base_cfg, env$active_ind),
      error = function(e) {
        lh_log(log_info, sprintf("    ERROR estimating Group %d: %s", g, e$message),
               level = "ERROR")
        NULL
      }
    )
    group_models[[as.character(g)]] <- g_model

    if (!is.null(g_model)) {
      g_boot <- tryCatch(
        lh_bootstrap_pls(g_model, nboot = cfg$bootstrap_samples, seed = seed),
        error = function(e) {
          lh_log(log_info, sprintf("    WARN bootstrap Group %d: %s", g, e$message),
                 level = "WARN")
          NULL
        }
      )
      group_boots[[as.character(g)]] <- g_boot
    }
  }

  # ------------------------------------------------------------------
  # Extract path coefficients per group
  # ------------------------------------------------------------------
  group_paths <- list()
  for (g in as.character(group_labels)) {
    m <- group_models[[g]]
    if (is.null(m)) next
    pc <- .lh_extract_path_coefs(m, paths_parsed, path_labels)
    group_paths[[g]] <- pc
  }

  # ------------------------------------------------------------------
  # Permutation test for each pair
  # ------------------------------------------------------------------
  all_pair_results <- list()

  for (pr in pairs) {
    g1 <- as.character(pr[1]); g2 <- as.character(pr[2])
    pair_label <- sprintf("G%s_vs_G%s", g1, g2)
    lh_log(log_info, sprintf("\n  --- %s ---", pair_label))

    beta_g1 <- group_paths[[g1]]
    beta_g2 <- group_paths[[g2]]
    if (is.null(beta_g1) || is.null(beta_g2)) {
      lh_log(log_info, "    Skipped: model estimation failed for one group.",
             level = "WARN")
      next
    }

    diff_obs <- beta_g1 - beta_g2

    # Permutation
    data_g1 <- group_data[[g1]]
    data_g2 <- group_data[[g2]]
    pooled <- rbind(data_g1, data_g2)
    n1 <- nrow(data_g1)
    n_total <- nrow(pooled)

    diff_perm <- matrix(NA, nrow = n_perm, ncol = length(path_labels))
    colnames(diff_perm) <- path_labels

    set.seed(seed)
    lh_log(log_info, sprintf("    Running %d permutations...", n_perm))

    for (b in seq_len(n_perm)) {
      perm_idx <- sample(n_total)
      perm_d1 <- pooled[perm_idx[1:n1], , drop = FALSE]
      perm_d2 <- pooled[perm_idx[(n1 + 1):n_total], , drop = FALSE]

      perm_m1 <- tryCatch(lh_estimate_pls(perm_d1, env$base_cfg, env$active_ind),
                           error = function(e) NULL)
      perm_m2 <- tryCatch(lh_estimate_pls(perm_d2, env$base_cfg, env$active_ind),
                           error = function(e) NULL)

      if (!is.null(perm_m1) && !is.null(perm_m2)) {
        pc1 <- .lh_extract_path_coefs(perm_m1, paths_parsed, path_labels)
        pc2 <- .lh_extract_path_coefs(perm_m2, paths_parsed, path_labels)
        diff_perm[b, ] <- pc1 - pc2
      }

      if (b %% 500 == 0)
        lh_log(log_info, sprintf("      Permutation %d / %d", b, n_perm))
    }

    # Compute p-values (two-tailed)
    p_perm <- sapply(seq_along(path_labels), function(j) {
      perm_vals <- diff_perm[!is.na(diff_perm[, j]), j]
      if (length(perm_vals) == 0) return(NA_real_)
      2 * min(mean(perm_vals <= diff_obs[j]),
              mean(perm_vals >= diff_obs[j]))
    })
    names(p_perm) <- path_labels

    # Build result data.frame
    pair_df <- data.frame(
      Pair = pair_label,
      Path = path_labels,
      Beta_G1 = beta_g1,
      Beta_G2 = beta_g2,
      Diff = diff_obs,
      p_perm = p_perm,
      stringsAsFactors = FALSE
    )
    rownames(pair_df) <- NULL

    # CI from permutation distribution (2.5th / 97.5th percentile of diff)
    ci_level <- cfg$confidence_level
    ci_lo_q <- (1 - ci_level) / 2
    ci_hi_q <- 1 - ci_lo_q
    pair_df$Diff_CI_lo <- apply(diff_perm, 2, quantile, probs = ci_lo_q, na.rm = TRUE)
    pair_df$Diff_CI_hi <- apply(diff_perm, 2, quantile, probs = ci_hi_q, na.rm = TRUE)

    all_pair_results[[pair_label]] <- pair_df

    for (j in seq_along(path_labels)) {
      sig_flag <- ifelse(!is.na(p_perm[j]) && p_perm[j] < alpha, " *SIG*", "")
      lh_log(log_info, sprintf("    %s: diff=%.3f  p=%.4f%s",
                                path_labels[j], diff_obs[j], p_perm[j], sig_flag))
    }
  }

  # ------------------------------------------------------------------
  # Combine all pairs
  # ------------------------------------------------------------------
  if (length(all_pair_results) == 0) {
    lh_log(log_info, "  No valid pair results.", level = "WARN")
    mga_df <- data.frame()
  } else {
    mga_df <- do.call(rbind, all_pair_results)
    rownames(mga_df) <- NULL
  }

  # ------------------------------------------------------------------
  # p-value adjustment (across all tests within a pair)
  # ------------------------------------------------------------------
  if (nrow(mga_df) > 0 && p_adj_method != "none") {
    mga_df$p_adjusted <- p.adjust(mga_df$p_perm, method = p_adj_method)
    mga_df$Sig <- ifelse(!is.na(mga_df$p_adjusted) & mga_df$p_adjusted < alpha,
                         "*", "")
    lh_log(log_info, sprintf("  p-values adjusted (%s). Total tests: %d",
                              p_adj_method, nrow(mga_df)))
  } else {
    mga_df$p_adjusted <- mga_df$p_perm
    mga_df$Sig <- ifelse(!is.na(mga_df$p_perm) & mga_df$p_perm < alpha, "*", "")
  }

  n_sig <- sum(mga_df$Sig == "*", na.rm = TRUE)
  lh_gate(log_info, "MGA Permutation Test",
          TRUE,  # always PASS as gate (it's a sensitivity)
          sprintf("— %d / %d path-pair comparisons significant", n_sig, nrow(mga_df)))

  list(
    mga_df       = mga_df,
    pair_results = all_pair_results,
    micom_ok     = micom_ok,
    n_sig        = n_sig
  )
}

# ---- Extract path coefficients -----------------------------------------------

#' Extract path coefficient estimates from a seminr model
#' @param model seminr PLS model
#' @param paths_parsed list of list(from, to)
#' @param path_labels character vector of path labels
#' @return named numeric vector of path coefficients
.lh_extract_path_coefs <- function(model, paths_parsed, path_labels) {
  betas <- numeric(length(path_labels))
  names(betas) <- path_labels

  # seminr stores path coefficients in model$path_coef (matrix)
  pm <- model$path_coef

  for (j in seq_along(paths_parsed)) {
    fr <- paths_parsed[[j]]$from
    to <- paths_parsed[[j]]$to
    if (fr %in% rownames(pm) && to %in% colnames(pm)) {
      betas[j] <- pm[fr, to]
    } else {
      betas[j] <- NA_real_
    }
  }
  betas
}

# ---- MGA plots ---------------------------------------------------------------

#' Plot permutation distribution for key paths
lh_plot_mga <- function(mga_result, output_dir, cfg, log_info) {
  if (!isTRUE(cfg$reporting$export_plots)) return(invisible(NULL))

  w   <- cfg$reporting$plot_width
  h   <- cfg$reporting$plot_height
  dpi <- cfg$reporting$plot_dpi
  plot_dir <- file.path(output_dir, "plots")

  mga_df <- mga_result$mga_df
  if (nrow(mga_df) == 0) return(invisible(NULL))

  # Only plot significant or near-significant paths
  interesting <- mga_df[!is.na(mga_df$p_perm) & mga_df$p_perm < 0.10, ]
  if (nrow(interesting) == 0) {
    lh_log(log_info, "  No significant MGA paths to plot.")
    return(invisible(NULL))
  }

  for (i in seq_len(nrow(interesting))) {
    row <- interesting[i, ]
    p_label <- gsub(" -> ", "_", row$Path)
    fname <- sprintf("permutation_diff_%s_%s.png", row$Pair, p_label)

    # We don't have the raw perm distribution stored, so generate a placeholder
    # showing the point estimate + CI
    plot_df <- data.frame(
      x = c(row$Diff_CI_lo, row$Diff, row$Diff_CI_hi),
      type = c("CI Lower", "Observed", "CI Upper")
    )

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = x, y = 0)) +
      ggplot2::geom_point(size = 4, ggplot2::aes(colour = type)) +
      ggplot2::geom_segment(ggplot2::aes(x = row$Diff_CI_lo, xend = row$Diff_CI_hi,
                                          y = 0, yend = 0),
                             linewidth = 1, colour = "grey40") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
      ggplot2::labs(title = sprintf("MGA: %s (%s)", row$Path, row$Pair),
                    subtitle = sprintf("Diff = %.3f, p = %s",
                                       row$Diff, lh_fmt_p(row$p_perm)),
                    x = "Difference in path coefficient",
                    y = "", colour = "") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())

    ggplot2::ggsave(file.path(plot_dir, fname), p,
                     width = w, height = h * 0.6, dpi = dpi)
  }

  lh_log(log_info, sprintf("  MGA plots saved: %d paths", nrow(interesting)))
  invisible(NULL)
}

# ---- Export MGA results ------------------------------------------------------

#' Save MGA results to CSV
lh_export_mga <- function(mga_result, output_dir, cfg, log_info) {
  tbl_dir <- file.path(output_dir, "tables")

  if (nrow(mga_result$mga_df) > 0) {
    write.csv(mga_result$mga_df,
              file.path(tbl_dir, "mga_results.csv"), row.names = FALSE)
    lh_log(log_info, "  Saved: tables/mga_results.csv")
  }

  invisible(NULL)
}
