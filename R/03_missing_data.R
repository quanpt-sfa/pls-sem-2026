# ==============================================================================
# 03_missing_data.R — Step 3: Missing Data Handling
# Gate: "Đủ dữ liệu để ước lượng"
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

#' Xử lý dữ liệu thiếu theo luật định trước
#' @param data data.frame từ Step 2
#' @param cfg list config
#' @param log_info list log
#' @return data.frame sau xử lý thiếu
handle_missing <- function(data, cfg, log_info) {
  
  log_step(log_info, "Step 3: Missing Data Handling")
  
  ind_cols <- intersect(cfg$all_indicators, names(data))
  n_original <- nrow(data)
  
  # --- 3.1. Tính tỷ lệ thiếu ---
  # Theo từng quan sát
  data$missing_pct_row <- apply(data[, ind_cols, drop = FALSE], 1,
                                function(r) 100 * mean(is.na(r)))
  
  # Theo từng biến
  col_missing <- data.frame(
    variable    = ind_cols,
    n_missing   = sapply(ind_cols, function(v) sum(is.na(data[[v]]))),
    pct_missing = round(sapply(ind_cols, function(v) 100 * mean(is.na(data[[v]]))), 2)
  )
  
  log_msg(log_info, sprintf("Variable-level missing: min=%.1f%%, max=%.1f%%, median=%.1f%%",
                             min(col_missing$pct_missing),
                             max(col_missing$pct_missing),
                             median(col_missing$pct_missing)))
  
  # --- 3.2. Loại quan sát vượt ngưỡng ---
  threshold <- cfg$screening$missing_threshold_pct
  high_missing <- data$missing_pct_row > threshold
  n_removed <- sum(high_missing)
  
  if (n_removed > 0) {
    data <- data[!high_missing, ]
    log_msg(log_info, sprintf("Removed %d observations with >%d%% missing", n_removed, threshold))
  } else {
    log_msg(log_info, "No observations exceed missing threshold")
  }
  
  # --- 3.3. Imputation cho thiếu rải rác ---
  method <- cfg$missing_data$imputation_method
  n_imputed <- 0
  
  for (v in ind_cols) {
    na_idx <- is.na(data[[v]])
    if (any(na_idx)) {
      if (method == "mean") {
        data[[v]][na_idx] <- mean(data[[v]], na.rm = TRUE)
      } else if (method == "median") {
        data[[v]][na_idx] <- median(data[[v]], na.rm = TRUE)
      }
      # "none" — giữ nguyên NA
      n_imputed <- n_imputed + sum(na_idx)
    }
  }
  
  if (method != "none") {
    log_msg(log_info, sprintf("Imputed %d values using '%s' method", n_imputed, method))
    log_msg(log_info, sprintf("Assumption: %s", cfg$missing_data$mcar_mar_assumption))
  }
  
  # --- 3.4. Optional: Little's MCAR test ---
  tryCatch({
    if (requireNamespace("naniar", quietly = TRUE)) {
      mcar_result <- naniar::mcar_test(data[, ind_cols, drop = FALSE])
      log_msg(log_info, sprintf("Little's MCAR test: chi2=%.2f, df=%d, p=%.4f",
                                 mcar_result$statistic, mcar_result$df, mcar_result$p.value))
      if (mcar_result$p.value < 0.05) {
        log_warn(log_info, "MCAR rejected (p < .05) — missing may not be completely random")
      }
    }
  }, error = function(e) {
    log_msg(log_info, "Little's MCAR test skipped (naniar not available or insufficient data)")
  })
  
  # --- 3.5. Xuất kết quả ---
  dir.create("02_clean", showWarnings = FALSE)
  
  # Loại cột phụ trợ trước khi lưu dataset cuối
  helper_cols <- c("missing_pct_row", "flag_straightline", "flag_longstring",
                   "flag_speeding", "flag_inconsistent", "flag_duplicate",
                   "n_flags", "qc_remove", "longstring_max", "irv")
  
  data_final <- data[, !names(data) %in% helper_cols, drop = FALSE]
  saveRDS(data_final, "02_clean/clean_final.rds")
  
  # Báo cáo
  missing_report <- rbind(
    col_missing,
    data.frame(variable = "SUMMARY",
               n_missing = n_imputed,
               pct_missing = NA)
  )
  write.csv(missing_report, "02_clean/missing_data_report.csv", row.names = FALSE)
  
  log_msg(log_info, sprintf("Final dataset: %d rows x %d indicator columns",
                             nrow(data_final), length(ind_cols)))
  
  # Gate check
  remaining_missing <- sum(sapply(ind_cols, function(v) sum(is.na(data_final[[v]]))))
  gate_pass <- remaining_missing == 0 || method == "none"
  log_info <- log_gate(log_info, "Missing Data", gate_pass,
                       sprintf("Remaining NA: %d, method: %s", remaining_missing, method))
  
  data_final
}
