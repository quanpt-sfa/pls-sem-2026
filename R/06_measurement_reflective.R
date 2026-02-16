# ==============================================================================
# 06_measurement_reflective.R — Step 6: Reflective Measurement Model Assessment
# Gate: "Đo lường phản xạ đạt chuẩn"
# CCA framework — Mode A composite-based evaluation
# ==============================================================================

suppressPackageStartupMessages({
  library(seminr)
  library(dplyr)
})

#' Đánh giá mô hình đo lường cho cấu trúc phản xạ
#' @param pls_model seminr PLS model object
#' @param boot_model seminr bootstrap model object (NULL if bootstrap skipped)
#' @param cfg list config
#' @param log_info list log
#' @param policy inference policy list (from utils_inference.R)
#' @return list chứa kết quả đánh giá
assess_reflective <- function(pls_model, boot_model, cfg, log_info, policy = NULL) {
  
  log_step(log_info, "Step 6: Reflective Measurement Model Assessment (CCA Mode A)")
  
  dir.create("05_measurement/reflective", showWarnings = FALSE, recursive = TRUE)
  
  model_summary <- summary(pls_model)
  boot_summary  <- if (!is.null(boot_model)) summary(boot_model) else NULL
  
  if (is.null(boot_model)) {
    log_msg(log_info, "NOTE: No bootstrap — HTMT CI and significance tests unavailable (point estimates only)")
  }
  
  reflective_names <- sapply(cfg$reflective_constructs, function(c) c$name)
  
  if (length(reflective_names) == 0) {
    log_msg(log_info, "No reflective constructs — skipping Step 6")
    return(list(pass = TRUE))
  }
  
  results <- list()
  
  # ==========================================================================
  # 6.1. Outer Loadings — Indicator Reliability
  # ==========================================================================
  log_msg(log_info, "--- 6.1. Outer Loadings (Indicator Reliability) ---")
  
  loadings_raw <- model_summary$loadings
  
  # Tạo bảng loadings cho reflective constructs
  loading_rows <- list()
  indicator_decisions <- list()
  
  for (cc in cfg$reflective_constructs) {
    for (ind in cc$indicators) {
      if (ind %in% rownames(loadings_raw)) {
        loading_val <- loadings_raw[ind, cc$name]
        
        # Quyết định giữ/loại
        thresh <- cfg$thresholds_reflective
        if (loading_val >= thresh$outer_loading_min) {
          decision <- "KEEP"
          reason   <- sprintf("Loading %.3f >= %.3f", loading_val, thresh$outer_loading_min)
        } else if (loading_val >= thresh$outer_loading_drop_range[1] &&
                   loading_val < thresh$outer_loading_min) {
          decision <- "REVIEW"
          reason   <- sprintf("Loading %.3f in [%.2f, %.3f] — review if removing improves CR/AVE",
                              loading_val, thresh$outer_loading_drop_range[1], thresh$outer_loading_min)
        } else {
          decision <- "DROP_CANDIDATE"
          reason   <- sprintf("Loading %.3f < %.2f", loading_val, thresh$outer_loading_drop_range[1])
        }
        
        loading_rows[[length(loading_rows) + 1]] <- data.frame(
          Construct = cc$name, Indicator = ind,
          Loading = fmt_num(loading_val, "estimate", policy),
          stringsAsFactors = FALSE
        )
        
        indicator_decisions[[length(indicator_decisions) + 1]] <- data.frame(
          Construct = cc$name, Indicator = ind,
          Loading = round(loading_val, 3),
          Decision = decision, Reason = reason,
          stringsAsFactors = FALSE
        )
        
        log_indicator_decision(log_info, ind, decision, reason)
      }
    }
  }
  
  loadings_df <- do.call(rbind, loading_rows)
  decisions_df <- do.call(rbind, indicator_decisions)
  
  write.csv(loadings_df, "05_measurement/reflective/outer_loadings.csv", row.names = FALSE)
  write.csv(decisions_df, "05_measurement/reflective/indicator_decisions.csv", row.names = FALSE)
  
  # ==========================================================================
  # 6.2. Internal Consistency Reliability: Alpha, rho_A, CR
  # ==========================================================================
  log_msg(log_info, "--- 6.2. Internal Consistency Reliability ---")
  
  reliability_raw <- model_summary$reliability
  
  rel_rows <- list()
  for (cn in reflective_names) {
    if (cn %in% rownames(reliability_raw)) {
      row_data <- reliability_raw[cn, ]
      
      alpha <- ifelse("alpha" %in% names(row_data), round(row_data["alpha"], 3), NA)
      rhoA  <- ifelse("rhoA" %in% names(row_data), round(row_data["rhoA"], 3), NA)
      
      # seminr có thể dùng tên khác cho CR
      cr_names <- c("rhoC", "CR")
      cr_val <- NA
      for (crn in cr_names) {
        if (crn %in% names(row_data)) { cr_val <- round(row_data[crn], 3); break }
      }
      
      ave_val <- ifelse("AVE" %in% names(row_data), round(row_data["AVE"], 3), NA)
      
      rel_rows[[length(rel_rows) + 1]] <- data.frame(
        Construct = cn,
        Cronbachs_Alpha = alpha,
        rho_A = rhoA,
        CR = cr_val,
        AVE = ave_val,
        stringsAsFactors = FALSE
      )
      
      log_msg(log_info, sprintf("  %s: Alpha=%.3f, rho_A=%.3f, CR=%.3f, AVE=%.3f",
                                 cn, alpha, rhoA, cr_val, ave_val))
    }
  }
  
  reliability_df <- do.call(rbind, rel_rows)
  write.csv(reliability_df, "05_measurement/reflective/reliability_table.csv", row.names = FALSE)
  results$reliability <- reliability_df
  
  # Kiểm tra ngưỡng
  rel_ok <- TRUE
  thresh <- cfg$thresholds_reflective
  
  for (i in seq_len(nrow(reliability_df))) {
    cn <- reliability_df$Construct[i]
    
    # rho_A (ưu tiên)
    if (!is.na(reliability_df$rho_A[i])) {
      if (reliability_df$rho_A[i] < thresh$reliability_min) {
        log_warn(log_info, sprintf("%s: rho_A=%.3f < %.2f", cn, reliability_df$rho_A[i], thresh$reliability_min))
        rel_ok <- FALSE
      }
      if (reliability_df$rho_A[i] > thresh$reliability_max) {
        log_warn(log_info, sprintf("%s: rho_A=%.3f > %.2f (possible redundancy)", cn, reliability_df$rho_A[i], thresh$reliability_max))
      }
    }
    
    # AVE
    if (!is.na(reliability_df$AVE[i]) && reliability_df$AVE[i] < thresh$ave_min) {
      log_warn(log_info, sprintf("%s: AVE=%.3f < %.2f", cn, reliability_df$AVE[i], thresh$ave_min))
      rel_ok <- FALSE
    }
  }
  
  # ==========================================================================
  # 6.3. Discriminant Validity — HTMT
  # ==========================================================================
  log_msg(log_info, "--- 6.3. Discriminant Validity (HTMT) ---")
  
  htmt_raw <- model_summary$validity$htmt
  
  if (!is.null(htmt_raw)) {
    # --- Full HTMT matrix (all constructs) with annotation ---
    htmt_full <- round(htmt_raw, 3)
    write.csv(htmt_full, "05_measurement/reflective/htmt_matrix_full.csv")
    # Write annotation file alongside
    writeLines(
      c("NOTE: Cells involving formative constructs (COM, MO, CG, ETH, TC, AQ)",
        "are reported for information only and are NOT used for discriminant",
        "validity decisions. HTMT thresholds apply only to reflective-reflective pairs."),
      "05_measurement/reflective/htmt_matrix_full_NOTE.txt"
    )

    # --- Reflective-only HTMT matrix (primary output for thesis) ---
    refl_names <- sapply(cfg$reflective_constructs, function(c) c$name)
    refl_in_htmt <- intersect(refl_names, rownames(htmt_raw))
    if (length(refl_in_htmt) >= 2) {
      htmt_refl <- round(htmt_raw[refl_in_htmt, refl_in_htmt, drop = FALSE], 3)
      write.csv(htmt_refl, "05_measurement/reflective/htmt_matrix.csv")
      log_msg(log_info, sprintf("HTMT reflective-only matrix saved (%d constructs: %s)",
                                 length(refl_in_htmt), paste(refl_in_htmt, collapse = ", ")))
      log_msg(log_info, "HTMT full matrix (11x11) saved as htmt_matrix_full.csv (info only for formative pairs)")
    } else {
      write.csv(htmt_full, "05_measurement/reflective/htmt_matrix.csv")
      log_msg(log_info, "HTMT matrix saved (fewer than 2 reflective constructs in HTMT — using full matrix)")
    }

    # --- HTMT evaluation: only reflective-reflective pairs for gate ---
    warn_th <- 0.85   # warning level
    fail_th <- 0.90   # fail level
    htmt_ok <- TRUE
    
    # Extract lower-triangle values
    htmt_vals <- htmt_raw
    htmt_vals[upper.tri(htmt_vals, diag = TRUE)] <- NA
    
    for (i in seq_len(nrow(htmt_vals))) {
      for (j in seq_len(ncol(htmt_vals))) {
        if (!is.na(htmt_vals[i, j]) && htmt_vals[i, j] > 0) {
          val <- htmt_vals[i, j]
          rn <- rownames(htmt_vals)[i]
          cn <- colnames(htmt_vals)[j]
          is_refl_pair <- (rn %in% refl_names) && (cn %in% refl_names)
          
          if (is_refl_pair) {
            # Reflective-reflective: affect gate
            if (val >= fail_th) {
              log_warn(log_info, sprintf("HTMT(%s, %s) = %.3f >= %.2f [FAIL — reflective pair]",
                                          rn, cn, val, fail_th))
              htmt_ok <- FALSE
            } else if (val >= warn_th) {
              log_warn(log_info, sprintf("HTMT(%s, %s) = %.3f >= %.2f [WARN — reflective pair]",
                                          rn, cn, val, warn_th))
            }
          } else {
            # Formative involved: log for reference, no gate impact
            if (val >= warn_th) {
              log_msg(log_info, sprintf("  HTMT(%s, %s) = %.3f (formative pair — info only)",
                                         rn, cn, val))
            }
          }
        }
      }
    }
    
    # Log overall max
    all_vals <- as.numeric(htmt_vals)
    all_vals <- all_vals[!is.na(all_vals) & all_vals > 0]
    refl_mat <- htmt_vals[refl_names, refl_names, drop = FALSE]
    refl_vals <- as.numeric(refl_mat)
    refl_vals <- refl_vals[!is.na(refl_vals) & refl_vals > 0]
    
    if (length(refl_vals) > 0) {
      log_msg(log_info, sprintf("HTMT max (reflective pairs): %.3f", max(refl_vals)))
    }
    if (length(all_vals) > 0) {
      log_msg(log_info, sprintf("HTMT max (all pairs): %.3f", max(all_vals)))
    }
  } else {
    htmt_ok <- TRUE
    log_warn(log_info, "HTMT matrix not available from model summary")
  }
  
  # HTMT Bootstrap CI
  tryCatch({
    # Trích HTMT CI từ boot_summary
    if (!is.null(boot_summary) && !is.null(boot_summary$bootstrapped_HTMT)) {
      write.csv(round(boot_summary$bootstrapped_HTMT, 3),
                "05_measurement/reflective/htmt_ci_bootstrap.csv")
      log_msg(log_info, "HTMT bootstrap CI saved")
    } else if (is.null(boot_summary)) {
      log_msg(log_info, "HTMT bootstrap CI not available — bootstrap was skipped")
    }
  }, error = function(e) {
    log_msg(log_info, "HTMT bootstrap CI not available — using point estimates")
  })
  
  # ==========================================================================
  # Gate Check
  # ==========================================================================
  gate_pass <- rel_ok && htmt_ok
  log_info <- log_gate(log_info, "Reflective Measurement", gate_pass,
                       sprintf("Reliability OK: %s, HTMT OK: %s", rel_ok, htmt_ok))
  
  results$pass <- gate_pass
  results$loadings <- loadings_df
  results$decisions <- decisions_df
  
  results
}
