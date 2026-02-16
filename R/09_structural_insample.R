# ==============================================================================
# 09_structural_insample.R — Step 9: Structural Model (In-sample Explanatory)
# Gate: "Cấu trúc không bị méo bởi cộng tuyến và có ý nghĩa"
# ==============================================================================

suppressPackageStartupMessages({
  library(seminr)
  library(dplyr)
})

#' Đánh giá mô hình cấu trúc — trục giải thích trong mẫu
#' @param pls_model seminr PLS model
#' @param boot_model seminr bootstrap model
#' @param cfg list config
#' @param log_info list log
#' @param policy inference policy list (from utils_inference.R)
#' @return list kết quả cấu trúc
assess_structural <- function(pls_model, boot_model, cfg, log_info, policy = NULL) {
  
  log_step(log_info, "Step 9: Structural Model — In-sample Explanatory Power")
  
  dir.create("06_structural", showWarnings = FALSE)
  
  model_summary <- summary(pls_model)
  boot_summary  <- summary(boot_model)
  thresh <- cfg$thresholds_structural
  
  results <- list()
  
  # ==========================================================================
  # 9.1. Inner VIF — Multicollinearity among predictors
  # ==========================================================================
  log_msg(log_info, "--- 9.1. Inner VIF ---")
  
  # Robust approach: iterate over endogenous constructs to avoid cbind crash
  endogenous <- unique(sapply(cfg$structural_paths, function(p) p$to))
  
  vif_rows <- list()
  vif_ok <- TRUE
  
  tryCatch({
    # seminr stores VIF in different formats; handle both
    vif_raw <- model_summary$vif_antecedents
    
    if (!is.null(vif_raw)) {
      # If it's a matrix/data.frame, iterate over columns (endogenous constructs)
      if (is.matrix(vif_raw) || is.data.frame(vif_raw)) {
        for (dv in colnames(vif_raw)) {
          col_vals <- vif_raw[, dv]
          valid <- col_vals[!is.na(col_vals) & col_vals != 0]
          for (iv in names(valid)) {
            vif_rows[[length(vif_rows) + 1]] <- data.frame(
              DV = dv, IV = iv, VIF = round(valid[iv], 3),
              stringsAsFactors = FALSE
            )
          }
        }
      } else if (is.list(vif_raw)) {
        # Some seminr versions return a named list
        for (dv in names(vif_raw)) {
          vals <- vif_raw[[dv]]
          for (iv in names(vals)) {
            vif_rows[[length(vif_rows) + 1]] <- data.frame(
              DV = dv, IV = iv, VIF = round(vals[iv], 3),
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
    
    # Fallback: compute manually from construct scores if seminr didn't provide
    if (length(vif_rows) == 0) {
      log_msg(log_info, "  Computing Inner VIF manually from construct scores...")
      scores <- as.data.frame(pls_model$construct_scores)
      
      for (dv in endogenous) {
        # Find antecedents of this DV
        antecedents <- sapply(
          Filter(function(p) p$to == dv, cfg$structural_paths),
          function(p) p$from
        )
        if (length(antecedents) < 2) next
        
        # Regress each antecedent on all others → VIF = 1/(1-R²)
        for (focal in antecedents) {
          others <- setdiff(antecedents, focal)
          if (length(others) == 0) next
          
          available <- intersect(c(focal, others), names(scores))
          if (length(available) < 2 || !focal %in% available) next
          
          fmla <- as.formula(paste(focal, "~", paste(setdiff(available, focal), collapse = " + ")))
          fit <- tryCatch(lm(fmla, data = scores), error = function(e) NULL)
          if (!is.null(fit)) {
            r2 <- summary(fit)$r.squared
            vif_val <- 1 / (1 - r2)
            vif_rows[[length(vif_rows) + 1]] <- data.frame(
              DV = dv, IV = focal, VIF = round(vif_val, 3),
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }, error = function(e) {
    log_warn(log_info, paste("Inner VIF extraction error:", e$message))
  })
  
  if (length(vif_rows) > 0) {
    vif_df <- do.call(rbind, vif_rows)
    write.csv(vif_df, "06_structural/inner_vif.csv", row.names = FALSE)
    
    max_vif <- max(vif_df$VIF, na.rm = TRUE)
    vif_ok <- max_vif < thresh$inner_vif_max
    
    log_msg(log_info, sprintf("  Inner VIF range: [%.3f, %.3f]",
                               min(vif_df$VIF, na.rm = TRUE), max_vif))
    
    # Log per-DV details
    for (dv in unique(vif_df$DV)) {
      sub <- vif_df[vif_df$DV == dv, ]
      items <- paste(sprintf("%s=%.2f", sub$IV, sub$VIF), collapse = ", ")
      log_msg(log_info, sprintf("    %s: %s", dv, items))
    }
    
    if (!vif_ok) {
      log_warn(log_info, sprintf("  Inner VIF > %.1f detected — collinearity issue", thresh$inner_vif_max))
    }
  } else {
    vif_ok <- TRUE
    log_warn(log_info, "  Inner VIF not available — no antecedent pairs found")
  }
  
  # ==========================================================================
  # 9.2. Path Coefficients (bootstrap)
  # ==========================================================================
  log_msg(log_info, "--- 9.2. Path Coefficients ---")
  
  paths_boot <- boot_summary$bootstrapped_paths
  
  if (!is.null(paths_boot)) {
    paths_df <- as.data.frame(paths_boot)
    paths_df$Path <- rownames(paths_df)
    
    # Rename columns for clarity
    col_map <- c("Original Est." = "Beta", "Bootstrap Mean" = "Boot_Mean",
                 "Bootstrap SD" = "Boot_SE", "T Stat." = "T_Stat",
                 "2.5% CI" = "CI_Low", "97.5% CI" = "CI_High")
    for (old_name in names(col_map)) {
      if (old_name %in% names(paths_df)) {
        names(paths_df)[names(paths_df) == old_name] <- col_map[old_name]
      }
    }

    # --- Empirical p-value from bootstrap draws (replaces SEMinR t-approx p) ---
    # boot_model$boot_paths is a 3D array: constructs x constructs x nboot
    bp_3d <- tryCatch(boot_model$boot_paths, error = function(e) NULL)
    paths_df$P_Value <- NA_real_
    if (!is.null(bp_3d)) {
      for (ri in seq_len(nrow(paths_df))) {
        # Parse "FROM  ->  TO" path label
        parts <- trimws(strsplit(paths_df$Path[ri], "->")[[1]])
        if (length(parts) == 2) {
          from_c <- parts[1]; to_c <- parts[2]
          if (from_c %in% dimnames(bp_3d)[[1]] && to_c %in% dimnames(bp_3d)[[2]]) {
            draws <- bp_3d[from_c, to_c, ]
            paths_df$P_Value[ri] <- compute_empirical_p(draws)
          }
        }
      }
    }
    # Drop the old SEMinR column if it was renamed
    if ("Bootstrap P Val" %in% names(paths_df)) {
      paths_df[["Bootstrap P Val"]] <- NULL
    }
    
    # --- Apply inference policy (CI-based primary rule) ---
    inf_flags <- infer_pack_df(paths_df, policy = policy)
    paths_df$Sig_Primary <- inf_flags$Sig_Primary
    paths_df$T_Sig       <- inf_flags$T_Sig
    paths_df$P_Sig       <- inf_flags$P_Sig
    paths_df$Mismatch    <- inf_flags$Mismatch
    paths_df$Notes       <- inf_flags$Notes
    # Legacy column for backward compat
    paths_df$Significant <- paths_df$Sig_Primary
    
    # Format numbers per policy
    paths_df$Beta    <- fmt_num(paths_df$Beta,    "estimate", policy)
    paths_df$Boot_SE <- fmt_num(paths_df$Boot_SE, "estimate", policy)
    paths_df$T_Stat  <- fmt_num(paths_df$T_Stat,  "t_stat",  policy)
    paths_df$CI_Low  <- fmt_num(paths_df$CI_Low,  "ci",      policy)
    paths_df$CI_High <- fmt_num(paths_df$CI_High, "ci",      policy)
    if ("P_Value" %in% names(paths_df)) {
      paths_df$P_Value <- fmt_num(paths_df$P_Value, "p_value", policy)
    }
    
    write.csv(paths_df, "06_structural/path_coefficients_bootstrap.csv", row.names = FALSE)
    
    for (i in seq_len(nrow(paths_df))) {
      sig_star <- ifelse(paths_df$Sig_Primary[i], "*", "")
      mismatch_flag <- ifelse(paths_df$Mismatch[i], " [MISMATCH]", "")
      p_boot_str <- if ("P_Value" %in% names(paths_df) && !is.na(paths_df$P_Value[i]))
        sprintf(" (p_boot=%s)", fmt_p(paths_df$P_Value[i])) else ""
      log_msg(log_info, sprintf("  %s: beta=%.3f, 95%% CI[%.3f, %.3f]%s%s%s",
                                 paths_df$Path[i],
                                 paths_df$Beta[i],
                                 paths_df$CI_Low[i],
                                 paths_df$CI_High[i],
                                 sig_star, p_boot_str, mismatch_flag))
    }
    
    results$paths <- paths_df
  }
  
  # ==========================================================================
  # 9.3. R² — Coefficient of Determination
  # ==========================================================================
  log_msg(log_info, "--- 9.3. R-squared ---")
  
  # Extract R² for endogenous constructs
  # seminr stores rSquared as a matrix: rows = c("Rsq", "AdjRsq"), cols = endogenous constructs
  r2_rows <- list()
  endogenous <- unique(sapply(cfg$structural_paths, function(p) p$to))
  
  for (en in endogenous) {
    r2_val <- NA
    adjr2_val <- NA
    
    # Primary: access from model's rSquared matrix
    tryCatch({
      rs_mat <- pls_model$rSquared
      if (is.matrix(rs_mat) && en %in% colnames(rs_mat)) {
        r2_val <- rs_mat["Rsq", en]
        adjr2_val <- rs_mat["AdjRsq", en]
      }
    }, error = function(e) NULL)
    
    r2_rows[[length(r2_rows) + 1]] <- data.frame(
      Construct = en,
      R_squared = round(ifelse(is.null(r2_val) || is.na(r2_val), NA, r2_val), 3),
      Adj_R_squared = round(ifelse(is.null(adjr2_val) || is.na(adjr2_val), NA, adjr2_val), 3),
      stringsAsFactors = FALSE
    )
    
    log_msg(log_info, sprintf("  R²(%s) = %.3f, Adj.R² = %.3f", en,
                               ifelse(is.na(r2_val), 0, r2_val),
                               ifelse(is.na(adjr2_val), 0, adjr2_val)))
  }
  
  r2_df <- do.call(rbind, r2_rows)
  write.csv(r2_df, "06_structural/r_squared.csv", row.names = FALSE)
  results$r_squared <- r2_df
  
  # ==========================================================================
  # 9.4. f² — Effect Size (from seminr summary)
  # ==========================================================================
  log_msg(log_info, "--- 9.4. Effect Size (f²) ---")
  
  f2_rows <- list()
  
  # seminr's summary() already computes f² as an 11x11 matrix
  # s$fSquare[iv, dv] = f² for path iv → dv
  tryCatch({
    f2_mat <- model_summary$fSquare
    
    if (!is.null(f2_mat) && is.matrix(f2_mat)) {
      for (path in cfg$structural_paths) {
        iv <- path$from
        dv <- path$to
        
        if (iv %in% rownames(f2_mat) && dv %in% colnames(f2_mat)) {
          f2_val <- f2_mat[iv, dv]
          
          if (!is.na(f2_val) && f2_val != 0) {
            f2_label <- ifelse(abs(f2_val) >= thresh$f_squared_large, "Large",
                               ifelse(abs(f2_val) >= thresh$f_squared_medium, "Medium",
                                      ifelse(abs(f2_val) >= thresh$f_squared_small, "Small", "None")))
            
            f2_rows[[length(f2_rows) + 1]] <- data.frame(
              IV = iv, DV = dv,
              f_squared = round(f2_val, 3),
              Effect_Size = f2_label,
              stringsAsFactors = FALSE
            )
            
            log_msg(log_info, sprintf("  f²(%s→%s) = %.3f [%s]", iv, dv, f2_val, f2_label))
          }
        }
      }
    } else {
      log_warn(log_info, "  f² matrix not available from seminr summary")
    }
  }, error = function(e) {
    log_warn(log_info, paste("f² extraction error:", e$message))
  })
  
  if (length(f2_rows) > 0) {
    f2_df <- do.call(rbind, f2_rows)
    write.csv(f2_df, "06_structural/f_squared.csv", row.names = FALSE)
    results$f_squared <- f2_df
  }
  
  # ==========================================================================
  # 9.5. SRMR (Reference only — NOT a fit criterion in PLS-SEM)
  # ==========================================================================
  log_msg(log_info, "--- 9.5. SRMR (Reference) ---")
  
  tryCatch({
    srmr_val <- model_summary$fit$SRMR
    if (!is.null(srmr_val)) {
      srmr_df <- data.frame(
        Metric = "SRMR",
        Value = round(srmr_val, 3),
        Note = "Reference only — NOT a global fit criterion in PLS-SEM"
      )
      write.csv(srmr_df, "06_structural/srmr_reference.csv", row.names = FALSE)
      log_msg(log_info, sprintf("SRMR = %.3f (reference, not fit criterion)", srmr_val))
    }
  }, error = function(e) {
    log_msg(log_info, "SRMR not available")
  })
  
  # ==========================================================================
  # Gate Check
  # ==========================================================================
  gate_pass <- vif_ok
  log_info <- log_gate(log_info, "Structural In-sample", gate_pass,
                       sprintf("Inner VIF OK: %s", vif_ok))
  
  results$pass <- gate_pass
  results
}
