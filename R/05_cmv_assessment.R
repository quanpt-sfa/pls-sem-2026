# ==============================================================================
# 05_cmv_assessment.R — Step 5: Common Method Variance / Bias Assessment
# Gate: "Không có tín hiệu CMV quá mạnh"
# ==============================================================================

suppressPackageStartupMessages({
  library(seminr)
})

#' Đánh giá sai lệch phương pháp chung
#' @param data data.frame từ Step 3
#' @param cfg list config
#' @param log_info list log
#' @param stage "pilot" or "main" — affects gate severity
#' @return logical — TRUE nếu pass gate
assess_cmv <- function(data, cfg, log_info, stage = "main") {
  
  log_step(log_info, "Step 5: CMV/CMB Assessment")
  
  dir.create("04_cmv", showWarnings = FALSE)
  ind_cols <- intersect(cfg$all_indicators, names(data))
  
  # ==========================================================================
  # A. Biện pháp thủ tục (ghi nhận)
  # ==========================================================================
  procedural_text <- paste(
    "Procedural remedies applied during survey design:",
    "  - Anonymity emphasized in instructions",
    "  - Academic purpose clearly stated",
    "  - Standardized, clear question wording",
    "  - Varied item ordering to reduce cognitive load",
    "  - Pilot-tested for clarity and completion time",
    "  - Consistent Likert scale format",
    sep = "\n"
  )
  log_msg(log_info, procedural_text)
  
  # ==========================================================================
  # B. Full Collinearity Assessment (Kock, 2015) — PRIMARY test
  # ==========================================================================
  log_msg(log_info, "Running Full Collinearity VIF Assessment...")
  
  # Tạo mô hình PLS với tất cả constructs → để tính VIF toàn diện
  # Strategy: mỗi construct lần lượt làm DV, các construct còn lại làm IV
  construct_names <- cfg$construct_names
  
  # Tạo measurement model (all constructs)
  mm_items <- list()
  for (cc in cfg$constructs) {
    if (cc$measurement_type == "reflective") {
      mm_items[[length(mm_items) + 1]] <- reflective(cc$name, cc$indicators)
    } else {
      mm_items[[length(mm_items) + 1]] <- composite(cc$name, cc$indicators, weights = mode_B)
    }
  }
  mm <- do.call(constructs, mm_items)
  
  # Tính composite scores
  pls_data <- as.data.frame(data[, ind_cols, drop = FALSE])
  
  # Full collinearity VIF: regress mỗi construct lên TẤT CẢ constructs còn lại
  # Cần estimate PLS model trước để lấy construct scores
  # Dùng một structural model "saturated" đơn giản
  full_col_vif <- tryCatch({
    # Tạo structural model giả: tất cả → biến cuối (chỉ để ước lượng)
    dv_name <- construct_names[length(construct_names)]
    iv_names <- construct_names[-length(construct_names)]
    
    sm <- relationships(
      paths(from = iv_names, to = dv_name)
    )
    
    pls_temp <- estimate_pls(
      data = pls_data,
      measurement_model = mm,
      structural_model = sm
    )
    
    # Lấy construct scores
    scores <- as.data.frame(pls_temp$construct_scores)
    
    # Tính full collinearity VIF cho mỗi construct
    vif_results <- data.frame(
      Construct = character(), Full_Collinearity_VIF = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (cn in construct_names) {
      others <- setdiff(construct_names, cn)
      if (length(others) >= 1) {
        formula_str <- paste(cn, "~", paste(others, collapse = " + "))
        fit <- lm(as.formula(formula_str), data = scores)
        r_sq <- summary(fit)$r.squared
        vif_val <- 1 / (1 - r_sq)
        vif_results <- rbind(vif_results, data.frame(
          Construct = cn, Full_Collinearity_VIF = round(vif_val, 3),
          stringsAsFactors = FALSE
        ))
      }
    }
    
    vif_results
  }, error = function(e) {
    log_warn(log_info, paste("Full collinearity VIF failed:", e$message))
    NULL
  })
  
  if (!is.null(full_col_vif)) {
    write.csv(full_col_vif, "04_cmv/full_collinearity_vif.csv", row.names = FALSE)
    
    max_vif <- max(full_col_vif$Full_Collinearity_VIF)
    threshold_warn <- if (!is.null(cfg$cmv$full_collinearity_vif_threshold))
                        cfg$cmv$full_collinearity_vif_threshold else 3.3
    threshold_fail <- if (!is.null(cfg$cmv$full_collinearity_vif_fail))
                        cfg$cmv$full_collinearity_vif_fail else 5.0
    
    log_msg(log_info, sprintf("Full Collinearity VIF range: [%.3f, %.3f]",
                               min(full_col_vif$Full_Collinearity_VIF), max_vif))
    log_msg(log_info, sprintf("  Thresholds: WARN >= %.1f, FAIL >= %.1f", threshold_warn, threshold_fail))
    
    for (i in seq_len(nrow(full_col_vif))) {
      v <- full_col_vif$Full_Collinearity_VIF[i]
      status <- if (v >= threshold_fail) "FAIL" else if (v >= threshold_warn) "WARN" else "OK"
      log_msg(log_info, sprintf("  %s: VIF = %.3f [%s]",
                                 full_col_vif$Construct[i], v, status))
    }
    
    if (max_vif >= threshold_fail) {
      cmv_pass <- FALSE  # hard fail
    } else if (max_vif >= threshold_warn) {
      cmv_pass <- TRUE   # warn but pass
    } else {
      cmv_pass <- TRUE
    }
    cmv_serious <- max_vif >= threshold_fail
  } else {
    cmv_pass <- NA
    cmv_serious <- FALSE
  }
  
  # ==========================================================================
  # C. Harman's Single-Factor Test (tham khảo phụ trợ)
  # ==========================================================================
  log_msg(log_info, "Running Harman's single-factor test (reference only)...")
  
  tryCatch({
    pca_result <- prcomp(data[, ind_cols, drop = FALSE], center = TRUE, scale. = TRUE)
    var_explained <- summary(pca_result)$importance[2, 1] * 100  # % variance 1st component
    
    harman_text <- sprintf(
      "Harman's single-factor test (REFERENCE ONLY):\n  First component explains: %.1f%% of variance\n  Conclusion: %s\n  NOTE: This is a supplementary reference, NOT a primary CMV test.",
      var_explained,
      ifelse(var_explained > 50, "WARNING — >50% by single factor", "OK — <50%")
    )
    log_msg(log_info, harman_text)
    
    writeLines(harman_text, "04_cmv/harman_reference.txt")
  }, error = function(e) {
    log_warn(log_info, paste("Harman test failed:", e$message))
  })
  
  # ==========================================================================
  # D. Marker variable
  # ==========================================================================
  log_msg(log_info, "Marker variable: NOT available in survey design (limitation noted)")
  
  # ==========================================================================
  # Gate check
  # ==========================================================================
  if (!is.na(cmv_pass)) {
    if (cmv_serious) {
      log_info <- log_gate(log_info, "CMV/CMB", FALSE,
                           sprintf("Max full collinearity VIF: %.3f >= %.1f (serious CMV concern)",
                                   max_vif, threshold_fail))
    } else if (max_vif >= threshold_warn) {
      log_info <- log_gate(log_info, "CMV/CMB", TRUE,
                           sprintf("WARN — Max full collinearity VIF: %.3f (>= %.1f warn, < %.1f fail). Potential CMV; interpret with caution",
                                   max_vif, threshold_warn, threshold_fail))
    } else {
      log_info <- log_gate(log_info, "CMV/CMB", TRUE,
                           sprintf("Max full collinearity VIF: %.3f < %.1f (no CMV concern)",
                                   max_vif, threshold_warn))
    }
  } else {
    # CMV could not be computed (singular data / PLS failed)
    if (stage == "pilot") {
      log_info <- log_gate(log_info, "CMV/CMB", TRUE,
                           "SKIPPED — Full collinearity VIF could not be computed (pilot feasibility, singular data). CMV inference not performed in pilot; evaluated in MAIN.")
      cmv_pass <- TRUE  # Not a failure in pilot context
    } else {
      log_info <- log_gate(log_info, "CMV/CMB", FALSE,
                           "Full collinearity test could not be computed")
    }
  }
  
  cmv_pass
}
