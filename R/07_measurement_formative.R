# ==============================================================================
# 07_measurement_formative.R — Step 7: Formative/Composite Measurement Assessment
# Gate: "Composite ổn định & hợp lý nội dung"
# CCA framework — Mode B composite evaluation
# ==============================================================================

suppressPackageStartupMessages({
  library(seminr)
  library(dplyr)
})

#' Đánh giá mô hình đo lường cho cấu trúc tạo thành/composite
#' @param pls_model seminr PLS model object
#' @param boot_model seminr bootstrap model object (NULL if bootstrap skipped)
#' @param cfg list config
#' @param log_info list log
#' @param policy inference policy list (from utils_inference.R)
#' @return list chứa kết quả đánh giá
assess_formative <- function(pls_model, boot_model, cfg, log_info, policy = NULL) {
  
  log_step(log_info, "Step 7: Formative/Composite Measurement Assessment (CCA Mode B)")
  
  dir.create("05_measurement/formative", showWarnings = FALSE, recursive = TRUE)
  
  formative_names <- sapply(cfg$formative_constructs, function(c) c$name)
  
  if (length(formative_names) == 0) {
    log_msg(log_info, "No formative constructs — skipping Step 7")
    return(list(pass = TRUE))
  }
  
  model_summary <- summary(pls_model)
  boot_summary  <- if (!is.null(boot_model)) summary(boot_model) else NULL
  
  if (is.null(boot_model)) {
    log_msg(log_info, "NOTE: No bootstrap — weight significance tests unavailable (point estimates only)")
  }
  thresh <- cfg$thresholds_formative
  
  results <- list()
  
  # ==========================================================================
  # 7.1. Outer VIF — Multicollinearity among indicators
  # ==========================================================================
  log_msg(log_info, "--- 7.1. Outer VIF (Indicator Collinearity) ---")
  
  vif_rows <- list()
  vif_ok <- TRUE
  
  for (cc in cfg$formative_constructs) {
    # Tính VIF cho indicators của construct này
    ind_data <- as.data.frame(pls_model$data[, cc$indicators, drop = FALSE])
    
    for (ind in cc$indicators) {
      others <- setdiff(cc$indicators, ind)
      if (length(others) >= 1) {
        formula_str <- paste(ind, "~", paste(others, collapse = " + "))
        fit <- lm(as.formula(formula_str), data = ind_data)
        r_sq <- summary(fit)$r.squared
        vif_val <- 1 / (1 - r_sq)
      } else {
        vif_val <- 1.0
      }
      
      vif_status <- ifelse(vif_val < thresh$outer_vif_preferred, "OK",
                           ifelse(vif_val < thresh$outer_vif_max, "CAUTION", "FAIL"))
      
      if (vif_val >= thresh$outer_vif_max) vif_ok <- FALSE
      
      vif_rows[[length(vif_rows) + 1]] <- data.frame(
        Construct = cc$name, Indicator = ind,
        Outer_VIF = round(vif_val, 3), Status = vif_status,
        stringsAsFactors = FALSE
      )
      
      log_msg(log_info, sprintf("  %s → %s: VIF = %.3f [%s]", cc$name, ind, vif_val, vif_status))
    }
  }
  
  vif_df <- do.call(rbind, vif_rows)
  write.csv(vif_df, "05_measurement/formative/outer_vif.csv", row.names = FALSE)
  
  # ==========================================================================
  # 7.2. Outer Weights (bootstrap) — Relative contribution
  # ==========================================================================
  log_msg(log_info, "--- 7.2. Outer Weights (Bootstrap) ---")
  
  weights_raw <- model_summary$weights
  has_bootstrap <- !is.null(boot_summary)
  
  if (!has_bootstrap) {
    log_msg(log_info, "  No bootstrap available — reporting point estimates only (weight/loading)")
    log_msg(log_info, "  Significance columns (T_Stat, P_Value, CI) will be NA")
  }
  
  weight_rows <- list()
  loading_rows <- list()
  decision_rows <- list()
  
  for (cc in cfg$formative_constructs) {
    for (ind in cc$indicators) {
      # Outer weight
      weight_val <- NA
      if (ind %in% rownames(weights_raw) && cc$name %in% colnames(weights_raw)) {
        weight_val <- weights_raw[ind, cc$name]
      }
      
      # Outer loading (absolute contribution)
      loading_val <- NA
      if (!is.null(model_summary$loadings) &&
          ind %in% rownames(model_summary$loadings) &&
          cc$name %in% colnames(model_summary$loadings)) {
        loading_val <- model_summary$loadings[ind, cc$name]
      }
      
      # Bootstrap significance for weight (only if bootstrap available)
      weight_sig <- NA
      weight_ci_lo <- NA
      weight_ci_hi <- NA
      weight_pval <- NA
      if (has_bootstrap) {
        tryCatch({
          bw <- boot_summary[["bootstrapped_weights"]]
          if (!is.null(bw) && is.matrix(bw)) {
            # seminr rownames use "IND  ->  CONSTRUCT" format
            row_key <- paste0(ind, "  ->  ", cc$name)
            if (row_key %in% rownames(bw)) {
              weight_sig   <- bw[row_key, "T Stat."]
              weight_ci_lo <- bw[row_key, "2.5% CI"]
              weight_ci_hi <- bw[row_key, "97.5% CI"]
              weight_pval  <- bw[row_key, "Bootstrap P Val"]
            }
          }
        }, error = function(e) NULL)
      }
      
      weight_significant <- !is.na(weight_sig) && abs(weight_sig) >= 1.96
      # CI-based significance (primary inference rule)
      weight_sig_ci <- infer_sig_ci(weight_ci_lo, weight_ci_hi)
      if (!is.na(weight_sig_ci)) weight_significant <- weight_sig_ci
      
      # Mismatch detection
      inf <- infer_pack(est = weight_val, t_stat = weight_sig,
                         p_value = weight_pval,
                         ci_low = weight_ci_lo, ci_high = weight_ci_hi,
                         policy = policy)
      weight_mismatch <- inf$mismatch
      
      # ===== Cây quyết định giữ/loại =====
      if (!has_bootstrap) {
        # No bootstrap → cannot decide on significance → KEEP all, flag for review
        decision <- "KEEP (no bootstrap)"
        reason <- sprintf("Weight=%.3f, Loading=%.3f — no bootstrap, cannot test significance",
                          ifelse(is.na(weight_val), 0, weight_val),
                          ifelse(is.na(loading_val), 0, loading_val))
      } else if (weight_significant) {
        decision <- "KEEP"
        reason <- sprintf("Weight significant (CI[%.3f,%.3f] excl. 0, t=%.3f, p=%s)",
                          weight_ci_lo, weight_ci_hi,
                          ifelse(is.na(weight_sig), 0, weight_sig),
                          fmt_p(weight_pval))
      } else if (!is.na(loading_val) && abs(loading_val) >= thresh$loading_keep_threshold) {
        decision <- "KEEP (content)"
        reason <- sprintf("Weight ns (CI includes 0, t=%.3f, p=%s) but loading=%.3f >= %.2f; content important",
                          ifelse(is.na(weight_sig), 0, weight_sig),
                          fmt_p(weight_pval),
                          loading_val, thresh$loading_keep_threshold)
      } else {
        decision <- "REVIEW"
        reason <- sprintf("Weight ns (t=%.3f), loading=%.3f < %.2f — check content validity",
                          ifelse(is.na(weight_sig), 0, weight_sig),
                          ifelse(is.na(loading_val), 0, loading_val),
                          thresh$loading_keep_threshold)
      }
      
      weight_rows[[length(weight_rows) + 1]] <- data.frame(
        Construct = cc$name, Indicator = ind,
        Weight = fmt_num(weight_val, "estimate", policy),
        T_Stat = fmt_num(weight_sig, "t_stat", policy),
        P_Value = fmt_num(weight_pval, "p_value", policy),
        CI_Low = fmt_num(weight_ci_lo, "ci", policy),
        CI_High = fmt_num(weight_ci_hi, "ci", policy),
        Sig_Primary = weight_significant,
        T_Sig = if (!is.na(weight_sig)) abs(weight_sig) >= 1.96 else NA,
        Mismatch = weight_mismatch,
        Significant = weight_significant,
        stringsAsFactors = FALSE
      )
      
      loading_rows[[length(loading_rows) + 1]] <- data.frame(
        Construct = cc$name, Indicator = ind,
        Loading = round(loading_val, 3),
        stringsAsFactors = FALSE
      )
      
      decision_rows[[length(decision_rows) + 1]] <- data.frame(
        Construct = cc$name, Indicator = ind,
        Weight = round(weight_val, 3),
        Loading = round(loading_val, 3),
        Weight_Significant = weight_significant,
        Decision = decision, Reason = reason,
        stringsAsFactors = FALSE
      )
      
      log_indicator_decision(log_info, ind, decision, reason)
    }
  }
  
  weights_df <- do.call(rbind, weight_rows)
  loadings_form_df <- do.call(rbind, loading_rows)
  decisions_df <- do.call(rbind, decision_rows)
  
  write.csv(weights_df, "05_measurement/formative/outer_weights_bootstrap.csv", row.names = FALSE)
  write.csv(loadings_form_df, "05_measurement/formative/outer_loadings_formative.csv", row.names = FALSE)
  write.csv(decisions_df, "05_measurement/formative/indicator_decisions.csv", row.names = FALSE)

  # --- Formative REVIEW items: thesis discussion guide ---
  review_items <- decisions_df[decisions_df$Decision == "REVIEW", ]
  if (nrow(review_items) > 0) {
    log_msg(log_info, sprintf("\n--- %d Formative Indicator(s) Flagged for Thesis Discussion ---", nrow(review_items)))
    for (ri in seq_len(nrow(review_items))) {
      log_msg(log_info, sprintf("  %s.%s: weight ns, loading=%.3f < 0.50",
                                 review_items$Construct[ri], review_items$Indicator[ri],
                                 review_items$Loading[ri]))
      log_msg(log_info, "    Retain ONLY with strong content/face validity argument")
      log_msg(log_info, "    Check: item wording, reverse-coded?, low variance?, content overlap?")
    }
    log_msg(log_info, "  Hair et al. (2022): weight ns + loading >= 0.50 -> keep (content);")
    log_msg(log_info, "  loading < 0.50 -> requires explicit justification in dissertation.")
  }

  # ==========================================================================
  # 7.3. Nomological Validity
  # ==========================================================================
  log_msg(log_info, "--- 7.3. Nomological Validity ---")
  log_msg(log_info, "No global single items available — redundancy analysis not feasible")
  log_msg(log_info, "Assessing nomological validity via path relationships...")
  
  # Kiểm tra formative constructs có path coefficients đúng chiều với lý thuyết
  nomo_rows <- list()
  for (cn in formative_names) {
    # Tìm paths liên quan
    paths_from <- Filter(function(p) p$from == cn, cfg$structural_paths)
    paths_to   <- Filter(function(p) p$to == cn, cfg$structural_paths)
    
    all_related <- c(
      sapply(paths_from, function(p) p$to),
      sapply(paths_to, function(p) p$from)
    )
    
    nomo_rows[[length(nomo_rows) + 1]] <- data.frame(
      Formative_Construct = cn,
      Related_Constructs = paste(all_related, collapse = ", "),
      Note = "Check path directions align with theory",
      stringsAsFactors = FALSE
    )
    
    log_msg(log_info, sprintf("  %s relates to: %s", cn, paste(all_related, collapse = ", ")))
  }
  
  nomo_df <- do.call(rbind, nomo_rows)
  write.csv(nomo_df, "05_measurement/formative/nomological_validity.csv", row.names = FALSE)
  
  # ==========================================================================
  # Gate Check
  # ==========================================================================
  gate_pass <- vif_ok  # VIF is the hard requirement
  log_info <- log_gate(log_info, "Formative Measurement", gate_pass,
                       sprintf("VIF OK: %s, max VIF: %.3f", vif_ok,
                               max(vif_df$Outer_VIF)))
  
  results$pass <- gate_pass
  results$vif <- vif_df
  results$weights <- weights_df
  results$decisions <- decisions_df
  
  results
}
