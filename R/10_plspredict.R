# ==============================================================================
# 10_plspredict.R — Step 10: PLSpredict — Out-of-sample Predictive Power
# Gate: "Có bằng chứng dự báo thực tiễn"
# ==============================================================================

suppressPackageStartupMessages({
  library(seminr)
  library(dplyr)
})

#' Chạy PLSpredict — dự báo ngoài mẫu
#' @param pls_model seminr PLS model
#' @param cfg list config
#' @param log_info list log
#' @return list kết quả PLSpredict
run_plspredict <- function(pls_model, cfg, log_info) {
  
  log_step(log_info, "Step 10: PLSpredict — Out-of-sample Predictive Power")
  
  dir.create("07_predict", showWarnings = FALSE)
  
  # ==========================================================================
  # Chạy PLSpredict
  # ==========================================================================
  log_msg(log_info, sprintf("PLSpredict: k=%d folds, %d repetitions",
                             cfg$plspredict$k_folds, cfg$plspredict$repetitions))
  
  predict_result <- tryCatch({
    predict_pls(
      model      = pls_model,
      technique  = predict_DA,
      noFolds    = cfg$plspredict$k_folds,
      reps       = cfg$plspredict$repetitions
    )
  }, error = function(e) {
    log_error(log_info, paste("PLSpredict failed:", e$message))
    NULL
  })
  
  if (is.null(predict_result)) {
    log_info <- log_gate(log_info, "PLSpredict", FALSE, "PLSpredict could not be computed")
    return(list(pass = FALSE))
  }
  
  pred_summary <- summary(predict_result)
  
  # ==========================================================================
  # Trích kết quả
  # ==========================================================================
  
  # Target: indicators of endogenous constructs
  endogenous <- unique(sapply(cfg$structural_paths, function(p) p$to))
  target_indicators <- unlist(lapply(endogenous, function(en) {
    get_indicators(cfg, en)
  }))
  
  results_rows <- list()
  comparison_rows <- list()
  
  # PLSpredict summary: rows = c("RMSE", "MAE"), cols = indicator names
  tryCatch({
    pls_oos <- pred_summary$PLS_out_of_sample    # matrix: RMSE/MAE x indicators
    lm_oos  <- pred_summary$LM_out_of_sample     # matrix: RMSE/MAE x indicators
    pls_is  <- pred_summary$PLS_in_sample         # matrix: RMSE/MAE x indicators
    
    if (is.null(pls_oos) || is.null(lm_oos)) stop("OOS matrices not available")
    
    for (ind in colnames(pls_oos)) {
      # PLS predictions
      pls_rmse <- pls_oos["RMSE", ind]
      pls_mae  <- pls_oos["MAE", ind]
      
      # LM benchmark
      lm_rmse <- lm_oos["RMSE", ind]
      lm_mae  <- lm_oos["MAE", ind]
      
      # Q²_predict = 1 - (PLS_OOS_RMSE² / LM_OOS_RMSE²)  — simplified
      q2_predict <- NA
      if (!is.na(lm_rmse) && lm_rmse != 0) {
        q2_predict <- 1 - (pls_rmse^2 / lm_rmse^2)
      }
      
      results_rows[[length(results_rows) + 1]] <- data.frame(
        Indicator = ind,
        Q2_predict = round(ifelse(is.null(q2_predict), NA, q2_predict), 3),
        PLS_RMSE = round(pls_rmse, 3),
        PLS_MAE = round(pls_mae, 3),
        LM_RMSE = round(lm_rmse, 3),
        LM_MAE = round(lm_mae, 3),
        stringsAsFactors = FALSE
      )
      
      # So sánh PLS vs LM
      rmse_better <- pls_rmse < lm_rmse
      mae_better  <- pls_mae < lm_mae
      
      comparison_rows[[length(comparison_rows) + 1]] <- data.frame(
        Indicator = ind,
        RMSE_PLS_better = rmse_better,
        MAE_PLS_better = mae_better,
        stringsAsFactors = FALSE
      )
      
      log_msg(log_info, sprintf("  %s: Q²=%.3f, RMSE(PLS=%.3f vs LM=%.3f) %s, MAE(PLS=%.3f vs LM=%.3f) %s",
                                 ind, ifelse(is.na(q2_predict), 0, q2_predict),
                                 pls_rmse, lm_rmse, ifelse(rmse_better, "✓", "✗"),
                                 pls_mae, lm_mae, ifelse(mae_better, "✓", "✗")))
    }
  }, error = function(e) {
    log_warn(log_info, paste("Error extracting PLSpredict details:", e$message))
    log_msg(log_info, "Attempting alternative extraction...")
    
    # Fallback: print raw summary
    tryCatch({
      capture.output(print(pred_summary), file = "07_predict/plspredict_raw_summary.txt")
      log_msg(log_info, "Raw summary saved to 07_predict/plspredict_raw_summary.txt")
    }, error = function(e2) NULL)
  })
  
  # ==========================================================================
  # Phân loại mức dự báo
  # ==========================================================================
  if (length(comparison_rows) > 0) {
    results_df <- do.call(rbind, results_rows)
    comparison_df <- do.call(rbind, comparison_rows)
    
    write.csv(results_df, "07_predict/plspredict_results.csv", row.names = FALSE)
    write.csv(comparison_df, "07_predict/plspredict_vs_lm.csv", row.names = FALSE)
    
    n_total <- nrow(comparison_df)
    n_rmse_better <- sum(comparison_df$RMSE_PLS_better)
    n_mae_better  <- sum(comparison_df$MAE_PLS_better)
    
    # Phân loại theo quy tắc chốt từ Bước 0
    pct_better <- max(n_rmse_better, n_mae_better) / n_total
    
    if (pct_better >= 0.90) {
      predict_level <- "HIGH"
    } else if (pct_better >= 0.50) {
      predict_level <- "MEDIUM"
    } else {
      predict_level <- "LOW"
    }
    
    classification_df <- data.frame(
      N_indicators = n_total,
      N_RMSE_PLS_better = n_rmse_better,
      N_MAE_PLS_better = n_mae_better,
      Pct_better = round(100 * pct_better, 1),
      Prediction_Level = predict_level,
      stringsAsFactors = FALSE
    )
    
    write.csv(classification_df, "07_predict/prediction_classification.csv", row.names = FALSE)
    
    log_msg(log_info, sprintf("\nPredictive power classification: %s", predict_level))
    log_msg(log_info, sprintf("  PLS better RMSE: %d/%d, MAE: %d/%d",
                               n_rmse_better, n_total, n_mae_better, n_total))
    
    # Gate check
    q2_pos <- sum(results_df$Q2_predict > 0, na.rm = TRUE)
    gate_pass <- q2_pos > 0
    log_info <- log_gate(log_info, "PLSpredict", gate_pass,
                         sprintf("Q²_predict > 0 for %d/%d indicators, level: %s",
                                 q2_pos, n_total, predict_level))
    
    return(list(pass = gate_pass, results = results_df, classification = classification_df))
  }
  
  log_info <- log_gate(log_info, "PLSpredict", FALSE, "Could not extract prediction results")
  list(pass = FALSE)
}
