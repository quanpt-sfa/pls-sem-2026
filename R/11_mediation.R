# ==============================================================================
# 11_mediation.R — Step 11a: Mediation Analysis
# Nitzl et al. (2016) / Zhao et al. (2010) classification
# Saturated model approach: adds direct X→Y paths for proper classification
#
# KEY METHODOLOGICAL CHOICES:
#   1. Bootstrap indirect = a_boot[i] * b_boot[i] (aligned draws, NOT product of t's)
#   2. Significance decision = CI-based (95% percentile CI excludes 0)
#   3. t and p reported for completeness; CI takes precedence if conflict
#   4. VAF = indirect/total (descriptive only, NOT used for classification)
# ==============================================================================

suppressPackageStartupMessages({
  library(seminr)
  library(dplyr)
})

#' Kiểm định giả thuyết trung gian bằng mô hình bão hòa (saturated model)
#'
#' Xây dựng model thứ hai có đầy đủ direct paths X→AQ để phân loại
#' mediation theo Nitzl et al. (2016) / Zhao et al. (2010).
#' Measurement model giữ nguyên; chỉ bổ sung direct paths vào structural.
#' Bootstrap inference sử dụng aligned draws: indirect_boot[i] = a_boot[i] * b_boot[i].
#'
#' @param pls_model seminr PLS model (main model — reference only)
#' @param boot_model seminr bootstrap model (main model — reference only)
#' @param data_final data.frame dữ liệu phân tích
#' @param active_indicators list chỉ báo sau optimizer
#' @param cfg list config
#' @param log_info list log
#' @param policy inference policy list (from utils_inference.R)
#' @return list kết quả trung gian
test_mediation <- function(pls_model, boot_model, data_final,
                           active_indicators, cfg, log_info, policy = NULL) {

  log_step(log_info, "Step 11a: Mediation Analysis (Nitzl et al. 2016)")

  dir.create("08_complex/mediation", showWarnings = FALSE, recursive = TRUE)

  if (is.null(cfg$mediation) || length(cfg$mediation$indirect_paths) == 0) {
    log_msg(log_info, "No mediation hypotheses specified — skipping")
    return(list(pass = TRUE, applicable = FALSE))
  }

  mediator    <- cfg$mediation$mediator
  outcome     <- cfg$mediation$indirect_paths[[1]]$to
  antecedents <- unique(sapply(cfg$mediation$indirect_paths, function(p) p$from))

  log_msg(log_info, sprintf("Mediator: %s, Outcome: %s", mediator, outcome))
  log_msg(log_info, sprintf("Antecedents: %s", paste(antecedents, collapse = ", ")))
  log_msg(log_info, sprintf("Decision rule: CI-based (95%% percentile CI excludes 0)"))
  log_msg(log_info, sprintf("Bootstrap: %d resamples, seed=%d",
                             cfg$project$bootstrap_samples, cfg$project$seed))

  # ========================================================================
  # 11a.1 Build SATURATED structural model
  # ========================================================================
  log_msg(log_info, "--- 11a.1. Building saturated structural model ---")

  existing_paths <- lapply(cfg$structural_paths, function(p) {
    list(from = p$from, to = p$to)
  })

  # Identify which antecedent→outcome direct paths already exist
  already_have <- sapply(
    Filter(function(p) p$from %in% antecedents && p$to == outcome, existing_paths),
    function(p) p$from
  )

  need_direct <- setdiff(antecedents, already_have)

  if (length(need_direct) == 0) {
    log_msg(log_info, "All direct paths already exist — using main model directly")
  } else {
    log_msg(log_info, sprintf("Adding direct paths: %s",
                               paste(paste0(need_direct, " -> ", outcome), collapse = ", ")))
  }

  # Saturated path list = existing + new direct paths
  sat_path_list <- existing_paths
  for (x in need_direct) {
    sat_path_list[[length(sat_path_list) + 1]] <- list(from = x, to = outcome)
  }

  sm_sat_items <- lapply(sat_path_list, function(p) paths(from = p$from, to = p$to))
  sm_sat <- do.call(relationships, sm_sat_items)

  # Measurement model (unchanged)
  build_mm_local <- function(cfg, active_inds) {
    mm_items <- list()
    for (cc in cfg$constructs) {
      inds <- active_inds[[cc$name]]
      if (length(inds) == 0) next
      if (cc$measurement_type == "reflective") {
        mm_items[[length(mm_items) + 1]] <- reflective(cc$name, inds)
      } else {
        mm_items[[length(mm_items) + 1]] <- composite(cc$name, inds, weights = mode_B)
      }
    }
    do.call(constructs, mm_items)
  }

  mm_sat <- build_mm_local(cfg, active_indicators)

  # Data matrix
  current_inds <- unique(unlist(active_indicators))
  current_cols <- intersect(current_inds, names(data_final))
  pls_data_sat <- as.data.frame(data_final[, current_cols, drop = FALSE])

  # ========================================================================
  # 11a.2 Estimate saturated model + Bootstrap
  # ========================================================================
  log_msg(log_info, "--- 11a.2. Estimating saturated PLS + bootstrap ---")

  sat_pls <- tryCatch({
    estimate_pls(data = pls_data_sat,
                 measurement_model = mm_sat,
                 structural_model = sm_sat)
  }, error = function(e) {
    log_error(log_info, paste("Saturated PLS estimation failed:", e$message))
    NULL
  })

  if (is.null(sat_pls)) {
    log_warn(log_info, "Falling back to main model (no direct paths)")
    return(test_mediation_fallback(pls_model, boot_model, cfg, log_info))
  }

  log_msg(log_info, "Saturated PLS model estimated successfully")
  log_msg(log_info, sprintf("Bootstrapping (%d samples, seed=%d)...",
                             cfg$project$bootstrap_samples, cfg$project$seed))

  sat_boot <- tryCatch({
    bootstrap_model(seminr_model = sat_pls,
                    nboot = cfg$project$bootstrap_samples,
                    seed  = cfg$project$seed)
  }, error = function(e) {
    log_error(log_info, paste("Saturated bootstrap failed:", e$message))
    NULL
  })

  if (is.null(sat_boot)) {
    log_warn(log_info, "Bootstrap failed — falling back to main model")
    return(test_mediation_fallback(pls_model, boot_model, cfg, log_info))
  }

  log_msg(log_info, "Saturated bootstrap complete")

  # ========================================================================
  # 11a.3 Quality check: validate paths exist in saturated model
  # ========================================================================
  log_msg(log_info, "--- 11a.3. Validating saturated model paths ---")

  sat_pc   <- sat_pls$path_coef          # original path_coef matrix
  sat_bp   <- sat_boot$boot_paths        # 3D array: constructs x constructs x nboot
  nboot    <- dim(sat_bp)[3]
  all_cnms <- rownames(sat_pc)

  # Verify a, b, c' paths exist for every antecedent
  validation_ok <- TRUE
  for (x in antecedents) {
    a_exists <- x %in% all_cnms && mediator %in% colnames(sat_pc) &&
                sat_pc[x, mediator] != 0
    b_exists <- mediator %in% all_cnms && outcome %in% colnames(sat_pc) &&
                sat_pc[mediator, outcome] != 0
    c_exists <- x %in% all_cnms && outcome %in% colnames(sat_pc)
    # c' can be 0 if truly zero effect — path must exist in model, not necessarily non-zero
    # We check colnames of smMatrix instead
    c_in_model <- any(sapply(sat_path_list, function(p) p$from == x && p$to == outcome))

    if (!a_exists) {
      log_warn(log_info, sprintf("  MISSING: %s -> %s (a-path)", x, mediator))
      validation_ok <- FALSE
    }
    if (!b_exists) {
      log_warn(log_info, sprintf("  MISSING: %s -> %s (b-path)", mediator, outcome))
      validation_ok <- FALSE
    }
    if (!c_in_model) {
      log_warn(log_info, sprintf("  MISSING: %s -> %s (c'-path not in saturated model)", x, outcome))
      validation_ok <- FALSE
    }
  }
  if (validation_ok) {
    log_msg(log_info, "  All a, b, c' paths validated in saturated model")
  }

  # ========================================================================
  # 11a.4 Compute effects & classify mediation
  # ========================================================================
  log_msg(log_info, "--- 11a.4. Mediation Classification ---")

  #' Compute bootstrap percentile CI and two-tailed p-value from draws
  #' Uses compute_empirical_p() with continuity correction and NA/Inf filtering.
  #' @param draws numeric vector of bootstrap draws
  #' @param alpha significance level (default 0.05)
  #' @return list(ci_lo, ci_hi, p_val, mean, sd)
  boot_inference <- function(draws, alpha = 0.05) {
    clean <- draws[is.finite(draws)]
    ci_lo <- unname(quantile(clean, alpha / 2, na.rm = TRUE))
    ci_hi <- unname(quantile(clean, 1 - alpha / 2, na.rm = TRUE))
    # Empirical p with continuity correction (Davison & Hinkley 1997)
    p_val <- compute_empirical_p(draws)
    list(
      ci_lo  = ci_lo,
      ci_hi  = ci_hi,
      p_val  = p_val,
      t_stat = mean(clean, na.rm = TRUE) / sd(clean, na.rm = TRUE),
      mean   = mean(clean, na.rm = TRUE),
      sd     = sd(clean, na.rm = TRUE)
    )
  }

  #' CI-based significance check (PRIMARY decision rule)
  #' "Significant" iff 95% CI does NOT include 0.
  ci_sig <- function(ci_lo, ci_hi) {
    if (is.na(ci_lo) || is.na(ci_hi)) return(FALSE)
    !(ci_lo <= 0 & ci_hi >= 0)
  }

  med_rows <- list()

  for (ip in cfg$mediation$indirect_paths) {
    from    <- ip$from
    through <- ip$through
    to      <- ip$to
    path_label <- paste(from, "->", through, "->", to)

    log_msg(log_info, sprintf("Testing: %s", path_label))

    # ---- Point estimates ----
    a_est <- sat_pc[from, through]         # X → M
    b_est <- sat_pc[through, to]           # M → Y
    c_est <- sat_pc[from, to]              # X → Y (direct, c')
    indirect_est <- a_est * b_est          # a*b
    total_est    <- c_est + indirect_est   # c' + a*b

    # ---- Bootstrap draws (aligned) ----
    a_draws <- sat_bp[from, through, ]     # vector of length nboot
    b_draws <- sat_bp[through, to, ]       # vector of length nboot
    c_draws <- sat_bp[from, to, ]          # vector of length nboot
    indirect_draws <- a_draws * b_draws    # element-wise product
    total_draws    <- c_draws + indirect_draws

    # ---- Bootstrap inference ----
    a_inf <- boot_inference(a_draws)
    b_inf <- boot_inference(b_draws)
    c_inf <- boot_inference(c_draws)       # direct effect
    ind_inf <- boot_inference(indirect_draws)
    tot_inf <- boot_inference(total_draws)

    # ---- Significance (CI-based, primary rule) ----
    direct_sig   <- ci_sig(c_inf$ci_lo, c_inf$ci_hi)
    indirect_sig <- ci_sig(ind_inf$ci_lo, ind_inf$ci_hi)

    # ---- Mismatch detection (inference policy) ----
    ind_inf_pack <- infer_pack(indirect_est, boot_se = ind_inf$sd,
                                t_stat = ind_inf$t_stat, p_value = ind_inf$p_val,
                                ci_low = ind_inf$ci_lo, ci_high = ind_inf$ci_hi,
                                policy = policy)
    dir_inf_pack <- infer_pack(c_est, boot_se = c_inf$sd,
                                t_stat = c_inf$t_stat, p_value = c_inf$p_val,
                                ci_low = c_inf$ci_lo, ci_high = c_inf$ci_hi,
                                policy = policy)

    # ---- Sanity check: CI vs t consistency ----
    ci_t_note <- ""
    if (ind_inf_pack$mismatch) {
      ci_t_note <- "CI vs t/p mismatch for indirect; CI used for decision. "
    }
    if (dir_inf_pack$mismatch) {
      ci_t_note <- paste0(ci_t_note, "CI vs t/p mismatch for direct; CI used for decision. ")
    }
    # BUG CHECK: If CI excludes 0 but sig is FALSE, that's a logic error
    if (ind_inf$ci_lo > 0 && !indirect_sig) {
      log_error(log_info, sprintf("  BUG: CI_low=%.4f > 0 but indirect marked non-sig! Fixing.", ind_inf$ci_lo))
      indirect_sig <- TRUE
    }
    if (c_inf$ci_lo > 0 && !direct_sig) {
      log_error(log_info, sprintf("  BUG: CI_low=%.4f > 0 but direct marked non-sig! Fixing.", c_inf$ci_lo))
      direct_sig <- TRUE
    }

    # ---- Mediation classification (Nitzl et al. 2016 / Zhao et al. 2010) ----
    if (!indirect_sig && !direct_sig) {
      med_type <- "No-effect"
    } else if (indirect_sig && !direct_sig) {
      med_type <- "Indirect-only (full mediation)"
    } else if (!indirect_sig && direct_sig) {
      med_type <- "Direct-only (no mediation)"
    } else {
      # Both significant
      same_sign <- (sign(indirect_est) == sign(c_est))
      if (same_sign) {
        med_type <- "Complementary partial mediation"
      } else {
        med_type <- "Competitive partial mediation"
      }
    }

    # ---- VAF (descriptive only) ----
    vaf <- NA
    if (!is.na(total_est) && abs(total_est) > 0.02) {
      vaf <- indirect_est / total_est
    }

    # ---- Notes ----
    notes <- ci_t_note
    if (nchar(trimws(notes)) == 0) notes <- ""

    med_rows[[length(med_rows) + 1]] <- data.frame(
      X = from, M = through, Y = to,
      a = fmt_num(a_est, "estimate", policy),
      a_CI_Low = fmt_num(a_inf$ci_lo, "ci", policy),
      a_CI_High = fmt_num(a_inf$ci_hi, "ci", policy),
      b = fmt_num(b_est, "estimate", policy),
      b_CI_Low = fmt_num(b_inf$ci_lo, "ci", policy),
      b_CI_High = fmt_num(b_inf$ci_hi, "ci", policy),
      Direct_c = fmt_num(c_est, "estimate", policy),
      Direct_T = fmt_num(c_inf$t_stat, "t_stat", policy),
      Direct_P = fmt_num(c_inf$p_val, "p_value", policy),
      Direct_CI_Low = fmt_num(c_inf$ci_lo, "ci", policy),
      Direct_CI_High = fmt_num(c_inf$ci_hi, "ci", policy),
      Direct_Sig = direct_sig,
      Direct_T_Sig = dir_inf_pack$t_sig,
      Direct_P_Sig = dir_inf_pack$p_sig,
      Direct_Mismatch = dir_inf_pack$mismatch,
      Indirect_ab = fmt_num(indirect_est, "estimate", policy),
      Indirect_T = fmt_num(ind_inf$t_stat, "t_stat", policy),
      Indirect_P = fmt_num(ind_inf$p_val, "p_value", policy),
      Indirect_CI_Low = fmt_num(ind_inf$ci_lo, "ci", policy),
      Indirect_CI_High = fmt_num(ind_inf$ci_hi, "ci", policy),
      Indirect_Sig = indirect_sig,
      Indirect_T_Sig = ind_inf_pack$t_sig,
      Indirect_P_Sig = ind_inf_pack$p_sig,
      Indirect_Mismatch = ind_inf_pack$mismatch,
      Total = fmt_num(total_est, "estimate", policy),
      Total_CI_Low = fmt_num(tot_inf$ci_lo, "ci", policy),
      Total_CI_High = fmt_num(tot_inf$ci_hi, "ci", policy),
      VAF = fmt_num(vaf, "vaf", policy),
      Classification = med_type,
      Notes = notes,
      stringsAsFactors = FALSE
    )

    # ---- Logging ----
    log_msg(log_info, sprintf("  a (%s->%s): %.4f, CI[%.4f, %.4f]",
                               from, through, a_est, a_inf$ci_lo, a_inf$ci_hi))
    log_msg(log_info, sprintf("  b (%s->%s): %.4f, CI[%.4f, %.4f]",
                               through, to, b_est, b_inf$ci_lo, b_inf$ci_hi))
    log_msg(log_info, sprintf("  c' (%s->%s): %.4f, CI[%.4f, %.4f]%s (p_boot=%s)",
                               from, to, c_est,
                               c_inf$ci_lo, c_inf$ci_hi,
                               ifelse(direct_sig, "*", ""),
                               fmt_p(c_inf$p_val)))
    log_msg(log_info, sprintf("  Indirect a*b: %.4f, CI[%.4f, %.4f]%s (p_boot=%s)",
                               indirect_est,
                               ind_inf$ci_lo, ind_inf$ci_hi,
                               ifelse(indirect_sig, "*", ""),
                               fmt_p(ind_inf$p_val)))
    log_msg(log_info, sprintf("  Total: %.4f, CI[%.4f, %.4f]",
                               total_est, tot_inf$ci_lo, tot_inf$ci_hi))
    if (!is.na(vaf)) {
      log_msg(log_info, sprintf("  VAF: %.1f%% (descriptive only)", vaf * 100))
    }
    log_msg(log_info, sprintf("  => %s", med_type))
    if (nchar(notes) > 0) log_msg(log_info, sprintf("  Note: %s", notes))
    log_msg(log_info, "")
  }

  # ========================================================================
  # 11a.5 Export results
  # ========================================================================
  if (length(med_rows) > 0) {
    med_df <- do.call(rbind, med_rows)

    # Full results table
    write.csv(med_df, "08_complex/mediation/specific_indirect_effects.csv",
              row.names = FALSE)

    # Classification summary
    class_summary <- med_df[, c("X", "M", "Y",
                                 "Direct_c", "Direct_Sig",
                                 "Indirect_ab", "Indirect_Sig",
                                 "Total", "VAF", "Classification")]
    write.csv(class_summary, "08_complex/mediation/mediation_classification.csv",
              row.names = FALSE)

    # Thesis-ready CSV for 10_report/
    dir.create("10_report", showWarnings = FALSE, recursive = TRUE)
    write.csv(med_df, "10_report/mediation_classification_saturated.csv",
              row.names = FALSE)

    log_msg(log_info, "Mediation results exported:")
    log_msg(log_info, "  08_complex/mediation/specific_indirect_effects.csv (full)")
    log_msg(log_info, "  08_complex/mediation/mediation_classification.csv (summary)")
    log_msg(log_info, "  10_report/mediation_classification_saturated.csv (thesis-ready)")
    log_msg(log_info, "")
    log_msg(log_info, "Methodology notes:")
    log_msg(log_info, sprintf("  - Saturated model: %d direct paths added to base model",
                               length(need_direct)))
    log_msg(log_info, "  - Indirect effect inference: bootstrap product a_boot[i]*b_boot[i]")
    log_msg(log_info, "  - CI method: percentile (2.5%, 97.5%)")
    log_msg(log_info, "  - Primary significance rule: CI excludes 0")
    log_msg(log_info, "  - Classification: Nitzl et al. (2016) / Zhao et al. (2010)")
    log_msg(log_info, sprintf("  - Reproducibility: seed=%d, B=%d",
                               cfg$project$seed, cfg$project$bootstrap_samples))

    return(list(pass = TRUE, results = med_df))
  }

  list(pass = TRUE, applicable = FALSE)
}


# ==============================================================================
# FALLBACK: mediation from main model when saturated estimation fails
# ==============================================================================

#' Fallback mediation (main model only — cannot classify type)
#' @keywords internal
test_mediation_fallback <- function(pls_model, boot_model, cfg, log_info) {
  log_warn(log_info, "Using FALLBACK mediation (main model — no direct paths)")
  log_warn(log_info, "Cannot classify mediation type without direct effects")
  log_warn(log_info, "Reporting specific indirect effects only")

  sat_bp <- boot_model$boot_paths
  sat_pc <- pls_model$path_coef
  mediator <- cfg$mediation$mediator
  outcome  <- cfg$mediation$indirect_paths[[1]]$to

  med_rows <- list()

  for (ip in cfg$mediation$indirect_paths) {
    from    <- ip$from
    through <- ip$through
    to      <- ip$to
    path_label <- paste(from, "->", through, "->", to)

    # Point estimates
    a_est <- sat_pc[from, through]
    b_est <- sat_pc[through, to]
    indirect_est <- a_est * b_est

    # Bootstrap draws
    a_draws <- sat_bp[from, through, ]
    b_draws <- sat_bp[through, to, ]
    indirect_draws <- a_draws * b_draws

    ci_lo <- unname(quantile(indirect_draws, 0.025, na.rm = TRUE))
    ci_hi <- unname(quantile(indirect_draws, 0.975, na.rm = TRUE))
    p_val <- 2 * min(mean(indirect_draws >= 0), mean(indirect_draws <= 0))
    t_val <- mean(indirect_draws) / sd(indirect_draws)
    indirect_sig <- !(ci_lo <= 0 & ci_hi >= 0)

    med_rows[[length(med_rows) + 1]] <- data.frame(
      X = from, M = through, Y = to,
      a = round(a_est, 4),
      a_CI_Low = NA, a_CI_High = NA,
      b = round(b_est, 4),
      b_CI_Low = NA, b_CI_High = NA,
      Direct_c = NA, Direct_T = NA, Direct_P = NA,
      Direct_CI_Low = NA, Direct_CI_High = NA, Direct_Sig = NA,
      Indirect_ab = round(indirect_est, 4),
      Indirect_T = round(t_val, 3),
      Indirect_P = round(p_val, 4),
      Indirect_CI_Low = round(ci_lo, 4),
      Indirect_CI_High = round(ci_hi, 4),
      Indirect_Sig = indirect_sig,
      Total = NA, Total_CI_Low = NA, Total_CI_High = NA,
      VAF = NA,
      Classification = ifelse(indirect_sig,
                               "Specific indirect effect (direct path not estimated)",
                               "No-effect"),
      Notes = "Fallback: direct path not in model, classification not possible",
      stringsAsFactors = FALSE
    )
  }

  if (length(med_rows) > 0) {
    med_df <- do.call(rbind, med_rows)
    write.csv(med_df, "08_complex/mediation/specific_indirect_effects.csv",
              row.names = FALSE)
    class_summary <- med_df[, c("X", "M", "Y",
                                 "Direct_c", "Direct_Sig",
                                 "Indirect_ab", "Indirect_Sig",
                                 "Total", "VAF", "Classification")]
    write.csv(class_summary, "08_complex/mediation/mediation_classification.csv",
              row.names = FALSE)
    return(list(pass = TRUE, results = med_df))
  }

  list(pass = TRUE, applicable = FALSE)
}
