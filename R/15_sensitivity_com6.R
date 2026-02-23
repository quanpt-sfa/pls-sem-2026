# ==============================================================================
# 15_sensitivity_com6.R — COM6 Indicator Sensitivity (Phase 5)
# ==============================================================================
# Tích hợp vào pipeline chính: re-estimate PLS sau khi loại COM6,
# so sánh path coefficients, R², f², significance với baseline.
# Không chạy lại Phase 1 (data prep) — tái sử dụng data_final.
#
# Tham khảo: Hair et al. (2022) — indicator sensitivity for formative constructs
# ==============================================================================

#' Run COM6 indicator sensitivity analysis
#'
#' @param pls_baseline   seminr PLS model (baseline, with COM6)
#' @param boot_baseline  seminr bootstrap model (baseline)
#' @param data_final     analysis-ready data frame
#' @param active_indicators  named list of construct -> indicator vectors (baseline)
#' @param cfg            config list
#' @param log_info       log object
#' @param policy         inference policy list
#' @param sensitivity_cfg list with: construct, indicator, bootstrap_samples, seed, output_subdir
#' @return list with comparison data frames
run_com6_sensitivity <- function(pls_baseline, boot_baseline, data_final,
                                  active_indicators, cfg, log_info, policy,
                                  sensitivity_cfg) {

  construct <- sensitivity_cfg$construct   # "COM"
  indicator <- sensitivity_cfg$indicator   # "COM6"
  boot_n    <- sensitivity_cfg$bootstrap_samples  # e.g. 5000
  seed      <- if (!is.null(sensitivity_cfg$seed)) sensitivity_cfg$seed else cfg$project$seed
  out_dir   <- if (!is.null(sensitivity_cfg$output_subdir))
    sensitivity_cfg$output_subdir else "09_robust/com6_sensitivity"

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  log_msg(log_info, "\n===== PHASE 5: INDICATOR SENSITIVITY (COM6) =====")
  log_msg(log_info, sprintf("  Target: %s.%s", construct, indicator))
  log_msg(log_info, sprintf("  Bootstrap samples: %d", boot_n))
  log_msg(log_info, sprintf("  Output: %s", out_dir))

  # --------------------------------------------------------------------------
  # 1. Validate: indicator exists in baseline
  # --------------------------------------------------------------------------
  if (!construct %in% names(active_indicators)) {
    log_warn(log_info, sprintf("  Construct '%s' not found in active_indicators — skipping", construct))
    return(invisible(NULL))
  }
  if (!indicator %in% active_indicators[[construct]]) {
    log_warn(log_info, sprintf("  Indicator '%s' not in %s — skipping", indicator, construct))
    return(invisible(NULL))
  }
  if (length(active_indicators[[construct]]) <= 2) {
    log_warn(log_info, sprintf("  %s has only %d indicators — removing %s would leave < 2; skipping",
                                construct, length(active_indicators[[construct]]), indicator))
    return(invisible(NULL))
  }

  # --------------------------------------------------------------------------
  # 2. Build variant indicators (drop COM6)
  # --------------------------------------------------------------------------
  variant_indicators <- active_indicators
  variant_indicators[[construct]] <- setdiff(variant_indicators[[construct]], indicator)

  log_msg(log_info, sprintf("  Baseline %s: %s", construct,
                             paste(active_indicators[[construct]], collapse = ", ")))
  log_msg(log_info, sprintf("  Variant  %s: %s (dropped %s)", construct,
                             paste(variant_indicators[[construct]], collapse = ", "), indicator))

  # --------------------------------------------------------------------------
  # 3. Build measurement + structural model (variant)
  # --------------------------------------------------------------------------
  build_mm_local <- function(cfg_obj, act_inds) {
    mm_items <- list()
    for (cc in cfg_obj$constructs) {
      inds <- act_inds[[cc$name]]
      if (length(inds) == 0) next
      if (cc$measurement_type == "reflective") {
        mm_items[[length(mm_items) + 1]] <- seminr::reflective(cc$name, inds)
      } else {
        mm_items[[length(mm_items) + 1]] <- seminr::composite(cc$name, inds, weights = seminr::mode_B)
      }
    }
    do.call(seminr::constructs, mm_items)
  }

  sm_list <- lapply(cfg$structural_paths, function(p) seminr::paths(from = p$from, to = p$to))
  sm <- do.call(seminr::relationships, sm_list)

  variant_cols <- intersect(unique(unlist(variant_indicators)), names(data_final))
  pls_data <- as.data.frame(data_final[, variant_cols, drop = FALSE])

  # --------------------------------------------------------------------------
  # 4. Estimate PLS (variant)
  # --------------------------------------------------------------------------
  log_msg(log_info, "\n--- 5.1. PLS Estimation (variant — no COM6) ---")

  pls_variant <- tryCatch({
    seminr::estimate_pls(data = pls_data,
                          measurement_model = build_mm_local(cfg, variant_indicators),
                          structural_model = sm)
  }, error = function(e) {
    log_error(log_info, sprintf("  PLS variant estimation failed: %s", e$message))
    NULL
  })

  if (is.null(pls_variant)) {
    log_warn(log_info, "  Cannot proceed — PLS variant failed")
    return(invisible(NULL))
  }
  log_msg(log_info, "  PLS variant estimated successfully")

  # --------------------------------------------------------------------------
  # 5. Bootstrap (variant)
  # --------------------------------------------------------------------------
  log_msg(log_info, sprintf("--- 5.2. Bootstrap (variant, %d samples) ---", boot_n))

  boot_variant <- tryCatch({
    seminr::bootstrap_model(seminr_model = pls_variant, nboot = boot_n, seed = seed)
  }, error = function(e) {
    log_warn(log_info, sprintf("  Bootstrap variant failed: %s — comparing point estimates only", e$message))
    NULL
  })

  if (!is.null(boot_variant)) {
    log_msg(log_info, "  Bootstrap variant complete")
  }

  # --------------------------------------------------------------------------
  # 6. Extract & compare path coefficients
  # --------------------------------------------------------------------------
  log_msg(log_info, "\n--- 5.3. Path Coefficient Comparison ---")

  extract_paths <- function(pls_mod, boot_mod, label, pol) {
    if (is.null(boot_mod)) {
      # Point estimates only
      summ <- summary(pls_mod)
      pc <- summ$paths
      if (is.null(pc)) return(NULL)
      df <- data.frame(
        Path = rownames(pc),
        Beta = round(as.numeric(pc[, 1]), 4),
        stringsAsFactors = FALSE
      )
      return(df)
    }

    boot_summ <- summary(boot_mod)
    bp <- boot_summ$bootstrapped_paths
    if (is.null(bp)) return(NULL)

    df <- as.data.frame(bp)
    df$Path <- rownames(df)

    col_map <- c("Original Est." = "Beta", "Bootstrap Mean" = "Boot_Mean",
                 "Bootstrap SD" = "Boot_SE", "T Stat." = "T_Stat",
                 "2.5% CI" = "CI_Low", "97.5% CI" = "CI_High")
    for (old_name in names(col_map)) {
      if (old_name %in% names(df))
        names(df)[names(df) == old_name] <- col_map[old_name]
    }

    # Empirical p-value
    bp_3d <- tryCatch(boot_mod$boot_paths, error = function(e) NULL)
    df$P_Value <- NA_real_
    if (!is.null(bp_3d)) {
      for (ri in seq_len(nrow(df))) {
        parts <- trimws(strsplit(df$Path[ri], "->")[[1]])
        if (length(parts) == 2) {
          from_c <- parts[1]; to_c <- parts[2]
          if (from_c %in% dimnames(bp_3d)[[1]] && to_c %in% dimnames(bp_3d)[[2]]) {
            draws <- bp_3d[from_c, to_c, ]
            df$P_Value[ri] <- compute_empirical_p(draws)
          }
        }
      }
    }

    # Inference flags
    inf_flags <- infer_pack_df(df, policy = pol)
    df$Sig_Primary <- inf_flags$Sig_Primary

    df
  }

  paths_base <- extract_paths(pls_baseline, boot_baseline, "Baseline", policy)
  paths_var  <- extract_paths(pls_variant, boot_variant, "NoCOM6", policy)

  path_compare <- NULL
  if (!is.null(paths_base) && !is.null(paths_var)) {
    # Select key columns for merge
    base_cols <- intersect(c("Path", "Beta", "P_Value", "CI_Low", "CI_High", "Sig_Primary"), names(paths_base))
    var_cols  <- intersect(c("Path", "Beta", "P_Value", "CI_Low", "CI_High", "Sig_Primary"), names(paths_var))

    b <- paths_base[, base_cols, drop = FALSE]
    v <- paths_var[, var_cols, drop = FALSE]

    path_compare <- merge(b, v, by = "Path", all = TRUE, suffixes = c("_Baseline", "_NoCOM6"))

    # Delta beta
    beta_b_col <- if ("Beta_Baseline" %in% names(path_compare)) "Beta_Baseline" else NULL
    beta_v_col <- if ("Beta_NoCOM6" %in% names(path_compare)) "Beta_NoCOM6" else NULL
    if (!is.null(beta_b_col) && !is.null(beta_v_col)) {
      path_compare$Delta_Beta <- round(
        as.numeric(path_compare[[beta_v_col]]) - as.numeric(path_compare[[beta_b_col]]), 4)
    }

    # Significance flip
    sig_b <- if ("Sig_Primary_Baseline" %in% names(path_compare)) path_compare$Sig_Primary_Baseline else NULL
    sig_v <- if ("Sig_Primary_NoCOM6" %in% names(path_compare)) path_compare$Sig_Primary_NoCOM6 else NULL
    if (!is.null(sig_b) && !is.null(sig_v)) {
      path_compare$Sig_Flip <- ifelse(is.na(sig_b) | is.na(sig_v), NA, sig_b != sig_v)
    }

    write.csv(path_compare, file.path(out_dir, "path_coefficients_compare.csv"), row.names = FALSE)

    # Log
    for (i in seq_len(nrow(path_compare))) {
      flip_str <- if ("Sig_Flip" %in% names(path_compare) && isTRUE(path_compare$Sig_Flip[i]))
        " ** SIG FLIP **" else ""
      log_msg(log_info, sprintf("  %s: Beta %.3f -> %.3f (Delta=%.4f)%s",
                                 path_compare$Path[i],
                                 as.numeric(path_compare[[beta_b_col]][i]),
                                 as.numeric(path_compare[[beta_v_col]][i]),
                                 path_compare$Delta_Beta[i],
                                 flip_str))
    }

    n_flips <- sum(path_compare$Sig_Flip %in% TRUE, na.rm = TRUE)
    log_msg(log_info, sprintf("\n  Significance flips: %d / %d paths", n_flips, nrow(path_compare)))
  }

  # --------------------------------------------------------------------------
  # 7. R² comparison
  # --------------------------------------------------------------------------
  log_msg(log_info, "\n--- 5.4. R-squared Comparison ---")

  extract_r2 <- function(pls_mod) {
    summ <- summary(pls_mod)
    r2_mat <- summ$paths
    # seminr summary stores R² in the paths matrix diagonal or in $r_squared
    r2_vec <- tryCatch({
      r2 <- pls_mod$rSquared
      if (is.null(r2)) r2 <- summ$paths[, "R^2", drop = TRUE]
      r2
    }, error = function(e) NULL)

    if (is.null(r2_vec)) return(NULL)

    # Extract endogenous constructs' R²
    endogenous <- unique(sapply(cfg$structural_paths, function(p) p$to))
    r2_df <- data.frame(
      Construct = endogenous,
      R_squared = round(sapply(endogenous, function(dv) {
        if (dv %in% names(r2_vec)) r2_vec[dv]
        else if (dv %in% rownames(r2_vec)) r2_vec[dv, 1]
        else NA_real_
      }), 4),
      stringsAsFactors = FALSE, row.names = NULL
    )
    r2_df
  }

  r2_base <- extract_r2(pls_baseline)
  r2_var  <- extract_r2(pls_variant)

  r2_compare <- NULL
  if (!is.null(r2_base) && !is.null(r2_var)) {
    names(r2_base)[2] <- "R2_Baseline"
    names(r2_var)[2]  <- "R2_NoCOM6"
    r2_compare <- merge(r2_base, r2_var, by = "Construct", all = TRUE)
    r2_compare$Delta_R2 <- round(r2_compare$R2_NoCOM6 - r2_compare$R2_Baseline, 4)

    write.csv(r2_compare, file.path(out_dir, "r_squared_compare.csv"), row.names = FALSE)

    for (i in seq_len(nrow(r2_compare))) {
      log_msg(log_info, sprintf("  %s: R²=%.3f -> %.3f (Delta=%.4f)",
                                 r2_compare$Construct[i],
                                 r2_compare$R2_Baseline[i],
                                 r2_compare$R2_NoCOM6[i],
                                 r2_compare$Delta_R2[i]))
    }
  }

  # --------------------------------------------------------------------------
  # 8. f² comparison (from seminr's built-in f²)
  # --------------------------------------------------------------------------
  log_msg(log_info, "\n--- 5.5. f-squared Comparison ---")

  extract_f2 <- function(pls_mod) {
    summ <- summary(pls_mod)
    f2 <- tryCatch(summ$fSquare, error = function(e) NULL)
    if (is.null(f2)) return(NULL)

    f2_rows <- list()
    endogenous <- unique(sapply(cfg$structural_paths, function(p) p$to))
    for (dv in endogenous) {
      if (!dv %in% colnames(f2)) next
      for (iv in rownames(f2)) {
        val <- f2[iv, dv]
        if (!is.na(val) && abs(val) > 1e-10) {
          f2_label <- ifelse(abs(val) >= 0.35, "Large",
                             ifelse(abs(val) >= 0.15, "Medium",
                                    ifelse(abs(val) >= 0.02, "Small", "None")))
          f2_rows[[length(f2_rows) + 1]] <- data.frame(
            IV = iv, DV = dv, f_squared = round(val, 4), Effect_Size = f2_label,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    if (length(f2_rows) > 0) do.call(rbind, f2_rows) else NULL
  }

  f2_base <- extract_f2(pls_baseline)
  f2_var  <- extract_f2(pls_variant)

  f2_compare <- NULL
  if (!is.null(f2_base) && !is.null(f2_var)) {
    names(f2_base)[3:4] <- c("f2_Baseline", "Effect_Baseline")
    names(f2_var)[3:4]  <- c("f2_NoCOM6", "Effect_NoCOM6")
    f2_compare <- merge(f2_base, f2_var, by = c("IV", "DV"), all = TRUE)
    f2_compare$Delta_f2 <- round(
      ifelse(is.na(f2_compare$f2_NoCOM6), 0, f2_compare$f2_NoCOM6) -
      ifelse(is.na(f2_compare$f2_Baseline), 0, f2_compare$f2_Baseline), 4)

    write.csv(f2_compare, file.path(out_dir, "f_squared_compare.csv"), row.names = FALSE)

    for (i in seq_len(nrow(f2_compare))) {
      log_msg(log_info, sprintf("  %s -> %s: f²=%.3f -> %.3f (Delta=%.4f) [%s -> %s]",
                                 f2_compare$IV[i], f2_compare$DV[i],
                                 ifelse(is.na(f2_compare$f2_Baseline[i]), 0, f2_compare$f2_Baseline[i]),
                                 ifelse(is.na(f2_compare$f2_NoCOM6[i]), 0, f2_compare$f2_NoCOM6[i]),
                                 f2_compare$Delta_f2[i],
                                 ifelse(is.na(f2_compare$Effect_Baseline[i]), "-", f2_compare$Effect_Baseline[i]),
                                 ifelse(is.na(f2_compare$Effect_NoCOM6[i]), "-", f2_compare$Effect_NoCOM6[i])))
    }
  }

  # --------------------------------------------------------------------------
  # 9. Instrument snapshot (variant)
  # --------------------------------------------------------------------------
  variant_snapshot <- list(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    type      = "com6_sensitivity_variant",
    dropped   = list(construct = construct, indicator = indicator),
    constructs = lapply(cfg$constructs, function(cc) {
      list(name = cc$name, measurement_type = cc$measurement_type,
           active_indicators = variant_indicators[[cc$name]])
    })
  )
  jsonlite::write_json(variant_snapshot, file.path(out_dir, "instrument_variant.json"),
                        pretty = TRUE, auto_unbox = TRUE)

  # --------------------------------------------------------------------------
  # 10. Conclusion
  # --------------------------------------------------------------------------
  log_msg(log_info, "\n--- 5.6. COM6 Sensitivity Conclusion ---")

  n_flips <- if (!is.null(path_compare) && "Sig_Flip" %in% names(path_compare))
    sum(path_compare$Sig_Flip %in% TRUE, na.rm = TRUE) else NA
  max_delta_beta <- if (!is.null(path_compare) && "Delta_Beta" %in% names(path_compare))
    max(abs(path_compare$Delta_Beta), na.rm = TRUE) else NA
  max_delta_r2 <- if (!is.null(r2_compare) && "Delta_R2" %in% names(r2_compare))
    max(abs(r2_compare$Delta_R2), na.rm = TRUE) else NA

  robust <- isTRUE(!is.na(n_flips) && n_flips == 0 &&
                    !is.na(max_delta_beta) && max_delta_beta < 0.10)

  if (robust) {
    conclusion <- "ROBUST — Removing COM6 does NOT change any hypothesis conclusions."
    log_msg(log_info, sprintf("  Conclusion: %s", conclusion))
    log_msg(log_info, sprintf("  Max |Delta_Beta| = %.4f (< 0.10 threshold)", max_delta_beta))
  } else {
    conclusion <- "SENSITIVE — Removing COM6 changes some results. Investigate further."
    log_msg(log_info, sprintf("  Conclusion: %s", conclusion))
    if (!is.na(n_flips) && n_flips > 0) {
      log_msg(log_info, sprintf("  Significance flips: %d", n_flips))
      flipped <- path_compare[path_compare$Sig_Flip %in% TRUE, "Path"]
      log_msg(log_info, sprintf("  Flipped paths: %s", paste(flipped, collapse = ", ")))
    }
    if (!is.na(max_delta_beta)) {
      log_msg(log_info, sprintf("  Max |Delta_Beta| = %.4f", max_delta_beta))
    }
  }

  # Write conclusion markdown
  conclusion_lines <- c(
    "# COM6 Indicator Sensitivity — Conclusion",
    sprintf("Generated: %s", Sys.time()),
    "",
    sprintf("**Indicator dropped**: %s.%s", construct, indicator),
    sprintf("**Baseline %s indicators**: %s", construct,
            paste(active_indicators[[construct]], collapse = ", ")),
    sprintf("**Variant  %s indicators**: %s", construct,
            paste(variant_indicators[[construct]], collapse = ", ")),
    "",
    sprintf("**Bootstrap**: %d samples (seed=%d)", boot_n, seed),
    "",
    "## Results",
    sprintf("- Significance flips: %s", ifelse(is.na(n_flips), "N/A", as.character(n_flips))),
    sprintf("- Max |Delta_Beta|: %s", ifelse(is.na(max_delta_beta), "N/A", sprintf("%.4f", max_delta_beta))),
    sprintf("- Max |Delta_R²|: %s", ifelse(is.na(max_delta_r2), "N/A", sprintf("%.4f", max_delta_r2))),
    "",
    sprintf("## Conclusion: **%s**", ifelse(robust, "ROBUST", "SENSITIVE")),
    "",
    conclusion
  )
  writeLines(conclusion_lines, file.path(out_dir, "sensitivity_conclusion.md"))

  log_msg(log_info, sprintf("\n  All outputs saved to: %s", out_dir))
  log_msg(log_info, "===== PHASE 5 COMPLETE =====")

  invisible(list(
    robust = robust,
    n_flips = n_flips,
    max_delta_beta = max_delta_beta,
    max_delta_r2 = max_delta_r2,
    path_compare = path_compare,
    r2_compare = r2_compare,
    f2_compare = f2_compare,
    pls_variant = pls_variant,
    boot_variant = boot_variant
  ))
}
