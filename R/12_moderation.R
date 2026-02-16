# ==============================================================================
# 12_moderation.R — Step 11b: Moderation Analysis
# Two-stage approach + Simple slopes
# Eq(3): AQ = f(AJ, AJ×TC, AJ×ETH)
#
# NOTE: SEMinR's interaction_term() has a known edge-case bug when the IV is
# reflective (mode A) and the moderator is formative/composite (mode B).
# Internal code does `if (mode == ...)` where mode resolves to NA, causing
# "missing value where TRUE/FALSE needed".
#
# Solution: Manual two-stage approach (Hair et al., 2022, Ch. 7):
#   Stage 1: Estimate base model WITHOUT interactions → extract construct scores
#   Stage 2: Create interaction terms from scores → estimate regression model
#            with bootstrapped CIs via case resampling
# ==============================================================================

suppressPackageStartupMessages({
  library(seminr)
  library(ggplot2)
  library(dplyr)
})

# ==============================================================================
# Pre-flight validation
# ==============================================================================
check_mod_inputs <- function(cfg, pls_data, log_info) {
  errors <- character()
  warnings <- character()

  for (cc in cfg$constructs) {
    missing_inds <- setdiff(cc$indicators, names(pls_data))
    if (length(missing_inds) > 0) {
      errors <- c(errors, sprintf(
        "Construct '%s' has indicators missing from data: %s",
        cc$name, paste(missing_inds, collapse = ", ")))
    }
    if (length(cc$indicators) == 0) {
      errors <- c(errors, sprintf("Construct '%s' has zero indicators", cc$name))
    }
  }

  mm_names <- sapply(cfg$constructs, function(c) c$name)
  sm_constructs <- unique(c(
    sapply(cfg$structural_paths, function(p) p$from),
    sapply(cfg$structural_paths, function(p) p$to)
  ))
  missing_in_mm <- setdiff(sm_constructs, mm_names)
  if (length(missing_in_mm) > 0) {
    errors <- c(errors, sprintf(
      "Construct(s) in structural model but missing in measurement: %s",
      paste(missing_in_mm, collapse = ", ")))
  }

  for (ix in cfg$moderation$interactions) {
    iv_to_dv  <- any(sapply(cfg$structural_paths,
                             function(p) p$from == ix$iv && p$to == ix$dv))
    mod_to_dv <- any(sapply(cfg$structural_paths,
                             function(p) p$from == ix$moderator && p$to == ix$dv))
    if (!iv_to_dv) {
      warnings <- c(warnings, sprintf(
        "Main effect %s -> %s not in structural model (recommended)", ix$iv, ix$dv))
    }
    if (!mod_to_dv) {
      warnings <- c(warnings, sprintf(
        "Main effect %s -> %s not in structural model (recommended)", ix$moderator, ix$dv))
    }
  }

  bad_cols <- grep("\\*", names(pls_data), value = TRUE)
  if (length(bad_cols) > 0) {
    errors <- c(errors, sprintf(
      "Data columns contain '*' (reserved): %s", paste(bad_cols, collapse = ", ")))
  }

  for (w in warnings) log_warn(log_info, paste("  [PRE-CHECK]", w))
  if (length(errors) > 0) {
    for (e in errors) log_error(log_info, paste("  [PRE-CHECK]", e))
    return(FALSE)
  }

  log_msg(log_info, "  Pre-flight checks PASSED")
  TRUE
}

# ==============================================================================
# Strategy A: Try SEMinR's native interaction_term (may fail with mixed modes)
# ==============================================================================
try_seminr_native <- function(cfg, pls_data, log_info) {
  methods_to_try <- c("two_stage", "orthogonal", "product_indicator")

  for (method_name in methods_to_try) {
    log_msg(log_info, sprintf("  [Native] Trying method: %s", method_name))

    method_fn <- switch(method_name,
                        "two_stage"         = two_stage,
                        "orthogonal"        = orthogonal,
                        "product_indicator" = product_indicator)

    mm_items <- list()
    for (cc in cfg$constructs) {
      if (cc$measurement_type == "reflective") {
        mm_items[[length(mm_items) + 1]] <- reflective(cc$name, cc$indicators)
      } else {
        mm_items[[length(mm_items) + 1]] <- composite(cc$name, cc$indicators, weights = mode_B)
      }
    }

    interaction_names <- character()
    for (ix in cfg$moderation$interactions) {
      mm_items[[length(mm_items) + 1]] <- interaction_term(
        iv = ix$iv, moderator = ix$moderator, method = method_fn
      )
      interaction_names <- c(interaction_names, paste0(ix$iv, "*", ix$moderator))
    }

    mm <- do.call(constructs, mm_items)

    sm_paths <- list()
    for (p in cfg$structural_paths) {
      sm_paths[[length(sm_paths) + 1]] <- paths(from = p$from, to = p$to)
    }
    for (i in seq_along(cfg$moderation$interactions)) {
      ix <- cfg$moderation$interactions[[i]]
      sm_paths[[length(sm_paths) + 1]] <- paths(from = interaction_names[i], to = ix$dv)
    }
    sm <- do.call(relationships, sm_paths)

    result <- tryCatch({
      pls_mod <- estimate_pls(data = pls_data, measurement_model = mm,
                              structural_model = sm)
      boot_mod <- bootstrap_model(seminr_model = pls_mod,
                                  nboot = cfg$project$bootstrap_samples,
                                  seed = cfg$project$seed)
      list(pls = pls_mod, boot = boot_mod, method = method_name,
           interaction_names = interaction_names)
    }, error = function(e) {
      log_warn(log_info, sprintf("    Failed (%s): %s", method_name, e$message))
      NULL
    })

    if (!is.null(result)) return(result)
  }

  NULL
}

# ==============================================================================
# Strategy B: Manual two-stage approach (Hair et al., 2022)
#   Stage 1: estimate base PLS model -> get construct scores
#   Stage 2: create interaction score columns -> re-estimate with interactions
#            as single-item composites (bypasses SEMinR's mode-detection bug)
# ==============================================================================
manual_two_stage <- function(cfg, pls_data, log_info) {
  log_msg(log_info, "  [Manual two-stage] Stage 1: Estimating base model for construct scores...")

  # --- Stage 1: base model WITHOUT interactions ---
  mm_items <- list()
  for (cc in cfg$constructs) {
    if (cc$measurement_type == "reflective") {
      mm_items[[length(mm_items) + 1]] <- reflective(cc$name, cc$indicators)
    } else {
      mm_items[[length(mm_items) + 1]] <- composite(cc$name, cc$indicators, weights = mode_B)
    }
  }
  mm_base <- do.call(constructs, mm_items)

  sm_paths <- list()
  for (p in cfg$structural_paths) {
    sm_paths[[length(sm_paths) + 1]] <- paths(from = p$from, to = p$to)
  }
  sm_base <- do.call(relationships, sm_paths)

  pls_base <- estimate_pls(data = pls_data, measurement_model = mm_base,
                           structural_model = sm_base)

  scores <- as.data.frame(pls_base$construct_scores)
  log_msg(log_info, sprintf("    Stage 1 complete: %d construct scores extracted",
                             ncol(scores)))

  # --- Stage 2: build interaction columns from scores ---
  log_msg(log_info, "  [Manual two-stage] Stage 2: Creating interaction terms from scores...")

  interaction_names <- character()
  for (ix in cfg$moderation$interactions) {
    int_name <- paste0(ix$iv, "*", ix$moderator)
    scores[[int_name]] <- scores[[ix$iv]] * scores[[ix$moderator]]
    interaction_names <- c(interaction_names, int_name)
    log_msg(log_info, sprintf("    Created: %s (N=%d, M=%.3f, SD=%.3f)",
                               int_name, nrow(scores),
                               mean(scores[[int_name]], na.rm = TRUE),
                               sd(scores[[int_name]], na.rm = TRUE)))
  }

  # --- Stage 2 model: all constructs as single-item composites from scores ---
  mm2_items <- list()
  all_construct_names <- sapply(cfg$constructs, function(c) c$name)

  for (cn in all_construct_names) {
    mm2_items[[length(mm2_items) + 1]] <- composite(cn, cn, weights = mode_A)
  }
  for (int_name in interaction_names) {
    mm2_items[[length(mm2_items) + 1]] <- composite(int_name, int_name, weights = mode_A)
  }
  mm2 <- do.call(constructs, mm2_items)

  sm2_paths <- list()
  for (p in cfg$structural_paths) {
    sm2_paths[[length(sm2_paths) + 1]] <- paths(from = p$from, to = p$to)
  }
  for (i in seq_along(cfg$moderation$interactions)) {
    ix <- cfg$moderation$interactions[[i]]
    sm2_paths[[length(sm2_paths) + 1]] <- paths(from = interaction_names[i], to = ix$dv)
  }
  sm2 <- do.call(relationships, sm2_paths)

  pls_mod2 <- estimate_pls(data = scores, measurement_model = mm2,
                           structural_model = sm2)
  log_msg(log_info, "    Stage 2 PLS model estimated successfully")

  log_msg(log_info, sprintf("    Bootstrapping stage 2 (%d samples)...",
                             cfg$project$bootstrap_samples))
  boot_mod2 <- bootstrap_model(seminr_model = pls_mod2,
                               nboot = cfg$project$bootstrap_samples,
                               seed = cfg$project$seed)
  log_msg(log_info, "    Bootstrap complete")

  list(pls = pls_mod2, boot = boot_mod2, method = "manual_two_stage",
       interaction_names = interaction_names, base_model = pls_base,
       scores = scores)
}

#' Kiểm định giả thuyết điều tiết
#' @param data data.frame
#' @param cfg list config
#' @param log_info list log
#' @param policy inference policy list (from utils_inference.R)
#' @return list kết quả điều tiết
test_moderation <- function(data, cfg, log_info, policy = NULL) {
  
  log_step(log_info, "Step 11b: Moderation Analysis")
  
  dir.create("08_complex/moderation/moderation_plots", showWarnings = FALSE, recursive = TRUE)
  
  if (is.null(cfg$moderation) || length(cfg$moderation$interactions) == 0) {
    log_msg(log_info, "No moderation hypotheses specified — skipping")
    return(list(pass = TRUE, applicable = FALSE))
  }
  
  ind_cols <- intersect(cfg$all_indicators, names(data))
  pls_data <- as.data.frame(data[, ind_cols, drop = FALSE])
  
  log_msg(log_info, sprintf("Building moderation model with %d interaction(s)...",
                             length(cfg$moderation$interactions)))
  
  # --- Pre-flight ---
  preflight_ok <- check_mod_inputs(cfg, pls_data, log_info)
  if (!preflight_ok) {
    log_gate(log_info, "Moderation", FALSE, "Pre-flight validation failed")
    return(list(pass = FALSE))
  }
  
  # ==========================================================================
  # Estimation: try SEMinR native first, then manual two-stage fallback
  # ==========================================================================
  
  # Strategy A: SEMinR native interaction_term
  log_msg(log_info, "--- Strategy A: SEMinR native interaction_term ---")
  result <- tryCatch(
    try_seminr_native(cfg, pls_data, log_info),
    error = function(e) {
      log_warn(log_info, sprintf("Strategy A failed: %s", e$message))
      NULL
    }
  )
  
  # Strategy B: Manual two-stage (fallback for mixed reflective/formative)
  if (is.null(result)) {
    log_msg(log_info, "--- Strategy B: Manual two-stage approach ---")
    log_msg(log_info, paste(
      "  SEMinR interaction_term fails with mixed reflective IV + formative moderator.",
      "Using manual two-stage: scores from base model -> interaction terms -> re-estimate."))
    
    result <- tryCatch(
      manual_two_stage(cfg, pls_data, log_info),
      error = function(e) {
        log_error(log_info, sprintf("Manual two-stage also failed: %s", e$message))
        NULL
      }
    )
  }
  
  if (is.null(result)) {
    log_error(log_info, "All moderation strategies failed")
    log_gate(log_info, "Moderation", FALSE,
             "Model estimation failed with all strategies")
    return(list(pass = FALSE))
  }
  
  # ==========================================================================
  # Extract results
  # ==========================================================================
  pls_mod  <- result$pls
  boot_mod <- result$boot
  used_method <- result$method
  interaction_names <- result$interaction_names
  
  boot_summ <- summary(boot_mod)
  bp <- boot_summ$bootstrapped_paths
  
  saveRDS(pls_mod, "08_complex/moderation/pls_moderation_model.rds")
  saveRDS(boot_mod, "08_complex/moderation/boot_moderation_model.rds")
  if (!is.null(result$base_model)) {
    saveRDS(result$base_model, "08_complex/moderation/pls_base_model_stage1.rds")
  }
  
  log_msg(log_info, sprintf("\n--- Extracting results (method: %s) ---", used_method))
  
  mod_results <- list()
  
  for (i in seq_along(cfg$moderation$interactions)) {
    ix <- cfg$moderation$interactions[[i]]
    iv  <- ix$iv
    mod <- ix$moderator
    dv  <- ix$dv
    int_name <- interaction_names[i]
    
    interaction_label <- paste(iv, "*", mod, "\u2192", dv)
    log_msg(log_info, sprintf("\n--- Result: %s ---", interaction_label))
    
    # Interaction coefficient
    int_beta <- NA; int_t <- NA; int_ci_lo <- NA; int_ci_hi <- NA
    int_idx <- grep(gsub("\\*", "\\\\*", int_name), rownames(bp))
    
    if (length(int_idx) > 0) {
      int_beta  <- bp[int_idx[1], "Original Est."]
      int_t     <- bp[int_idx[1], "T Stat."]
      int_ci_lo <- bp[int_idx[1], "2.5% CI"]
      int_ci_hi <- bp[int_idx[1], "97.5% CI"]
    }
    
    # Significance: 95% bootstrap CI excludes 0 (primary rule)
    # T_Stat retained for reader reference but does not drive the decision
    ci_excludes_zero <- !is.na(int_ci_lo) && !is.na(int_ci_hi) &&
                        (int_ci_lo > 0 || int_ci_hi < 0)
    alpha_val <- if (!is.null(policy)) policy$alpha else 0.05
    t_sig <- !is.na(int_t) && abs(int_t) >= qnorm(1 - alpha_val / 2)
    int_sig <- ci_excludes_zero

    # Bootstrap p-value — empirical from draws (replaces SEMinR t-approx p)
    int_pval <- NA
    bp_3d <- tryCatch(boot_mod$boot_paths, error = function(e) NULL)
    if (!is.null(bp_3d) && int_name %in% dimnames(bp_3d)[[1]] &&
        dv %in% dimnames(bp_3d)[[2]]) {
      int_draws <- bp_3d[int_name, dv, ]
      int_pval  <- compute_empirical_p(int_draws)
    } else if (length(int_idx) > 0 && "Bootstrap P Val" %in% colnames(bp)) {
      # Fallback: SEMinR t-approx p if draws not accessible
      int_pval <- bp[int_idx[1], "Bootstrap P Val"]
    }
    p_sig <- if (!is.na(int_pval)) int_pval < alpha_val else NA

    # Mismatch flag
    int_mismatch <- FALSE
    if (!is.na(t_sig) && t_sig != int_sig) int_mismatch <- TRUE
    if (!is.na(p_sig) && p_sig != int_sig) int_mismatch <- TRUE
    int_note <- if (int_mismatch) "CI vs t/p mismatch; CI used for decision" else ""
    
    ci_conf <- if (!is.null(cfg$project$confidence_level)) cfg$project$confidence_level else 0.95
    ci_level <- sprintf("%.0f%%", ci_conf * 100)
    ci_method <- "percentile bootstrap"
    
    # IV main effect (AJ → AQ)
    iv_main_idx <- grep(paste0("^", iv, "\\s"), rownames(bp))
    iv_main_idx <- iv_main_idx[!iv_main_idx %in% int_idx]
    iv_to_dv <- iv_main_idx[grep(dv, rownames(bp)[iv_main_idx])]
    iv_beta   <- ifelse(length(iv_to_dv) > 0, bp[iv_to_dv[1], "Original Est."], NA)
    iv_t      <- ifelse(length(iv_to_dv) > 0, bp[iv_to_dv[1], "T Stat."], NA)
    iv_ci_lo  <- ifelse(length(iv_to_dv) > 0, bp[iv_to_dv[1], "2.5% CI"], NA)
    iv_ci_hi  <- ifelse(length(iv_to_dv) > 0, bp[iv_to_dv[1], "97.5% CI"], NA)
    
    # Moderator direct effect (TC/ETH → AQ)
    mod_direct_idx <- grep(paste0("^", mod, "\\s"), rownames(bp))
    mod_to_dv <- mod_direct_idx[grep(dv, rownames(bp)[mod_direct_idx])]
    mod_beta   <- ifelse(length(mod_to_dv) > 0, bp[mod_to_dv[1], "Original Est."], NA)
    mod_t      <- ifelse(length(mod_to_dv) > 0, bp[mod_to_dv[1], "T Stat."], NA)
    mod_ci_lo  <- ifelse(length(mod_to_dv) > 0, bp[mod_to_dv[1], "2.5% CI"], NA)
    mod_ci_hi  <- ifelse(length(mod_to_dv) > 0, bp[mod_to_dv[1], "97.5% CI"], NA)
    
    log_msg(log_info, sprintf("  IV direct  (%s→%s): beta=%.3f, %s CI[%.3f,%.3f]",
                               iv, dv, iv_beta, ci_level, iv_ci_lo, iv_ci_hi))
    log_msg(log_info, sprintf("  Mod direct (%s→%s): beta=%.3f, %s CI[%.3f,%.3f]",
                               mod, dv, mod_beta, ci_level, mod_ci_lo, mod_ci_hi))

    # Interaction: CI-first, p_boot secondary
    int_p_str <- if (!is.na(int_pval)) sprintf(" (p_boot=%.4f)", int_pval) else ""
    log_msg(log_info, sprintf("  Interaction (%s*%s→%s): beta=%.3f, %s CI[%.3f,%.3f]%s%s%s",
                               iv, mod, dv, int_beta, ci_level, int_ci_lo, int_ci_hi,
                               ifelse(int_sig, "*", ""),
                               int_p_str,
                               ifelse(int_mismatch, " [MISMATCH]", "")))
    
    mod_results[[i]] <- data.frame(
      Interaction = interaction_label,
      IV = iv, Moderator = mod, DV = dv,
      Method = used_method,
      IV_Direct_Beta = fmt_num(iv_beta, "estimate", policy),
      IV_Direct_CI_Low = fmt_num(iv_ci_lo, "ci", policy),
      IV_Direct_CI_High = fmt_num(iv_ci_hi, "ci", policy),
      Mod_Direct_Beta = fmt_num(mod_beta, "estimate", policy),
      Mod_Direct_CI_Low = fmt_num(mod_ci_lo, "ci", policy),
      Mod_Direct_CI_High = fmt_num(mod_ci_hi, "ci", policy),
      Interaction_Beta = fmt_num(int_beta, "estimate", policy),
      Interaction_T = fmt_num(int_t, "t_stat", policy),
      Interaction_P = fmt_num(int_pval, "p_value", policy),
      Interaction_CI_Low = fmt_num(int_ci_lo, "ci", policy),
      Interaction_CI_High = fmt_num(int_ci_hi, "ci", policy),
      CI_Level = ci_level,
      CI_Method = ci_method,
      Sig_Primary = int_sig,
      T_Sig = t_sig,
      P_Sig = p_sig,
      CI_Excludes_Zero = ci_excludes_zero,
      Mismatch = int_mismatch,
      Significant = int_sig,
      Notes = int_note,
      stringsAsFactors = FALSE
    )
    
    # =======================================================================
    # Simple slopes analysis
    # =======================================================================
    log_msg(log_info, "  Computing simple slopes...")
    
    tryCatch({
      # Get scores: from manual stage1 base model or from native model
      if (!is.null(result$scores)) {
        scores <- result$scores
      } else {
        scores <- as.data.frame(pls_mod$construct_scores)
      }
      
      w_mean <- mean(scores[[mod]], na.rm = TRUE)
      w_sd   <- sd(scores[[mod]], na.rm = TRUE)
      
      slopes <- data.frame(
        Moderator = mod,
        Moderator_Level = c("Low (M-1SD)", "Medium (M)", "High (M+1SD)"),
        W_Value = round(c(w_mean - w_sd, w_mean, w_mean + w_sd), 3),
        Simple_Slope = round(c(
          iv_beta + int_beta * (w_mean - w_sd),
          iv_beta + int_beta * w_mean,
          iv_beta + int_beta * (w_mean + w_sd)
        ), 3),
        stringsAsFactors = FALSE
      )
      
      # Unique filename per moderator
      slopes_file <- sprintf("08_complex/moderation/simple_slopes_%s.csv", mod)
      write.csv(slopes, slopes_file, row.names = FALSE)
      log_msg(log_info, sprintf("  Slopes: Low=%.3f, Med=%.3f, High=%.3f",
                                 slopes$Simple_Slope[1], slopes$Simple_Slope[2],
                                 slopes$Simple_Slope[3]))
      
      # ===== Moderation plot =====
      x_range <- seq(min(scores[[iv]], na.rm = TRUE),
                     max(scores[[iv]], na.rm = TRUE), length.out = 50)
      
      plot_data <- expand.grid(X = x_range, W_level = c("Low", "Medium", "High"))
      plot_data$W_value <- ifelse(plot_data$W_level == "Low", w_mean - w_sd,
                                  ifelse(plot_data$W_level == "Medium", w_mean, w_mean + w_sd))
      plot_data$Y <- iv_beta * plot_data$X + int_beta * plot_data$X * plot_data$W_value
      plot_data$W_level <- factor(plot_data$W_level, levels = c("Low", "Medium", "High"))
      
      p <- ggplot(plot_data, aes(x = X, y = Y, color = W_level, linetype = W_level)) +
        geom_line(linewidth = 1.2) +
        scale_color_manual(values = c("Low" = "#e15759", "Medium" = "#4e79a7", "High" = "#59a14f")) +
        labs(title = sprintf("Moderation: %s \u00d7 %s \u2192 %s (Method: %s)",
                             iv, mod, dv, used_method),
             x = paste(iv, "(Audit Judgment)"),
             y = paste(dv, "(Audit Quality)"),
             color = paste(mod, "Level"), linetype = paste(mod, "Level")) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom")
      
      plot_file <- sprintf("08_complex/moderation/moderation_plots/mod_%s_x_%s.png", iv, mod)
      ggsave(plot_file, p, width = 8, height = 5, dpi = 150)
      log_msg(log_info, sprintf("  Plot saved: %s", plot_file))
      
    }, error = function(e) {
      log_warn(log_info, paste("  Simple slopes/plot error:", e$message))
    })
  }
  
  # ==========================================================================
  # Save consolidated results
  # ==========================================================================
  if (length(mod_results) > 0) {
    mod_df <- do.call(rbind, mod_results)
    write.csv(mod_df, "08_complex/moderation/interaction_coefficients.csv", row.names = FALSE)
    
    log_msg(log_info, "\n--- Moderation Summary ---")
    log_msg(log_info, sprintf("  Method used: %s", used_method))
    for (i in seq_len(nrow(mod_df))) {
      log_msg(log_info, sprintf("  %s: beta=%.3f, %s",
                                 mod_df$Interaction[i],
                                 mod_df$Interaction_Beta[i],
                                 ifelse(mod_df$Significant[i], "SIGNIFICANT", "not significant")))
    }
    
    log_msg(log_info, sprintf("Moderation results saved (method: %s)", used_method))

    # Methodology note for thesis write-up
    if (used_method == "manual_two_stage") {
      log_msg(log_info, "")
      log_msg(log_info, "Methodology note (thesis write-up):")
      log_msg(log_info, "  - Two-stage score-based interaction (Chin et al., 2003; Henseler & Chin, 2010)")
      log_msg(log_info, "  - Stage 1: construct scores from base PLS model")
      log_msg(log_info, "  - Stage 2: standardized product of scores as interaction term")
      log_msg(log_info, "  - SEMinR native interaction_term not applicable:")
      log_msg(log_info, "    mixed reflective IV + formative moderator causes estimation failure")
      log_msg(log_info, "  - Inference: percentile bootstrap CI (primary), p_boot (secondary)")
    }

    log_gate(log_info, "Moderation", TRUE,
             sprintf("%d interaction(s) tested via %s", nrow(mod_df), used_method))
    return(list(pass = TRUE, results = mod_df, method = used_method))
  }
  
  log_gate(log_info, "Moderation", TRUE, "No interaction results extracted")
  list(pass = TRUE, applicable = FALSE)
}
