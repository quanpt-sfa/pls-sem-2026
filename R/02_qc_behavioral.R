# ==============================================================================
# 02_qc_behavioral.R — Step 2: Behavioral Quality Control
# Gate: "Phản hồi có tham gia nhận thức"
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

#' Tính long-string index (chuỗi lặp tối đa) cho một vector
#' @param x numeric vector (một dòng trả lời)
#' @return integer — chuỗi lặp dài nhất
calc_longstring <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(length(x))
  
  rle_result <- rle(x)
  max(rle_result$lengths)
}

#' Kiểm tra straight-lining cho một block
#' @param row_data numeric vector cho 1 block
#' @return numeric — tỷ lệ giá trị trùng (0-1)
calc_straightline_ratio <- function(row_data) {
  row_data <- row_data[!is.na(row_data)]
  if (length(row_data) <= 1) return(0)
  
  # Tỷ lệ giá trị phổ biến nhất
  tab <- table(row_data)
  max(tab) / length(row_data)
}

#' Chạy sàng lọc chất lượng phản hồi hành vi
#' @param data data.frame từ Step 1
#' @param cfg list config
#' @param log_info list log
#' @param pilot_min_n optional integer — override min_sample for pilot (NULL = use 10*max formula)
#' @return data.frame đã sàng lọc
run_behavioral_qc <- function(data, cfg, log_info, pilot_min_n = NULL) {
  
  log_step(log_info, "Step 2: Behavioral Quality Control")
  
  ind_cols <- intersect(cfg$all_indicators, names(data))
  n_original <- nrow(data)
  
  # --- 2.1. Straight-lining ---
  # --- 2.1. Straight-lining ---
  # Global Straight-lining Check (variance across ALL indicators)
  # Flag if SD is effectively 0 (respondent gave SAME answer for everything)
  row_sds <- apply(data[, ind_cols, drop = FALSE], 1, sd, na.rm = TRUE)
  data$flag_straightline <- !is.na(row_sds) & row_sds < 1e-6
  
  n_sl <- sum(data$flag_straightline)
  log_msg(log_info, sprintf("Straight-lining flagged (Global SD=0): %d (%.1f%%)", n_sl, 100 * n_sl / n_original))
  
  # --- 2.2. Long-string ---
  data$longstring_max <- apply(data[, ind_cols, drop = FALSE], 1, calc_longstring)
  
  # Xác định ngưỡng
  if (is.null(cfg$screening$longstring_max) || is.na(cfg$screening$longstring_max)) {
    # Tự tính: trung bình + 2*SD hoặc phân vị 95%
    ls_threshold <- ceiling(quantile(data$longstring_max, 0.95, na.rm = TRUE))
    log_msg(log_info, sprintf("Longstring threshold (auto, P95): %d", ls_threshold))
  } else {
    ls_threshold <- cfg$screening$longstring_max
  }
  
  data$flag_longstring <- data$longstring_max >= ls_threshold
  n_ls <- sum(data$flag_longstring)
  log_msg(log_info, sprintf("Long-string flagged: %d (%.1f%%)", n_ls, 100 * n_ls / n_original))
  
  # --- 2.3. Speeding ---
  data$flag_speeding <- FALSE
  max_plausible_sec <- if (!is.null(cfg$screening$speeding_max_plausible_sec))
                         cfg$screening$speeding_max_plausible_sec else 86400  # 24h default
  if ("Time" %in% names(data)) {
    completion_time <- suppressWarnings(as.numeric(data$Time))
    
    if (!all(is.na(completion_time))) {
      # Sanity check: if median duration looks like epoch or ms, try to convert
      med_val <- median(completion_time, na.rm = TRUE)
      if (med_val > 1e9) {
        # Likely epoch seconds — compute duration as diff from min (proxy)
        log_warn(log_info, sprintf(
          "Time values appear to be epoch timestamps (median=%.0f). Converting to durations from earliest response.", med_val))
        completion_time <- completion_time - min(completion_time, na.rm = TRUE)
        # After this, the "duration" is relative start time — not a true survey
        # duration. Flag as unreliable and skip speeding.
        log_warn(log_info, "Epoch-derived durations are not true completion times — speeding check SKIPPED")
        n_sp <- 0
        log_msg(log_info, sprintf("Speeding flagged: %d (skipped — no valid duration column)", n_sp))
      } else if (med_val > 1e6) {
        # Likely milliseconds
        log_warn(log_info, sprintf(
          "Time values appear to be milliseconds (median=%.0f). Converting to seconds.", med_val))
        completion_time <- completion_time / 1000
        med_val <- median(completion_time, na.rm = TRUE)
      }
      
      # Only proceed if values are plausible durations (not epoch-derived skip)
      if (med_val <= max_plausible_sec && med_val > 0) {
        if (!is.null(cfg$screening$speeding_absolute_min_sec) &&
            !is.na(cfg$screening$speeding_absolute_min_sec)) {
          speed_threshold <- cfg$screening$speeding_absolute_min_sec
        } else {
          speed_threshold <- quantile(completion_time, cfg$screening$speeding_percentile / 100,
                                      na.rm = TRUE)
        }
        # Final sanity: threshold itself must be plausible
        if (speed_threshold > max_plausible_sec) {
          log_warn(log_info, sprintf(
            "Computed speeding threshold (%.1f sec) exceeds max plausible (%d sec) — speeding check SKIPPED",
            speed_threshold, max_plausible_sec))
          n_sp <- 0
          log_msg(log_info, sprintf("Speeding flagged: %d (skipped — implausible threshold)", n_sp))
        } else {
          data$flag_speeding <- !is.na(completion_time) & completion_time < speed_threshold
          n_sp <- sum(data$flag_speeding)
          log_msg(log_info, sprintf("Speeding flagged: %d (threshold: %.1f sec)", n_sp, speed_threshold))
        }
      }
    } else {
      log_warn(log_info, "Time column not numeric — speeding check skipped")
    }
  } else {
    log_warn(log_info, "No 'Time' column — speeding check skipped")
  }
  
  # --- 2.4. Intra-individual response variability (IRV) ---
  # Phương sai cực thấp trên toàn bộ indicators
  data$irv <- apply(data[, ind_cols, drop = FALSE], 1, function(r) sd(r, na.rm = TRUE))
  irv_threshold <- quantile(data$irv, 0.01, na.rm = TRUE)  # Phân vị 1%
  data$flag_inconsistent <- !is.na(data$irv) & data$irv <= irv_threshold
  n_inc <- sum(data$flag_inconsistent)
  log_msg(log_info, sprintf("Low IRV flagged: %d (threshold: %.3f)", n_inc, irv_threshold))
  
  # --- 2.5. Tổng hợp cờ ---
  flag_cols <- c("flag_straightline", "flag_longstring", "flag_speeding", "flag_inconsistent")
  if ("flag_duplicate" %in% names(data)) flag_cols <- c(flag_cols, "flag_duplicate")
  
  data$n_flags <- rowSums(data[, flag_cols, drop = FALSE], na.rm = TRUE)
  
  # Quyết định loại: ≥ flag_threshold_to_remove cờ HOẶC 100% straight-line
  threshold <- cfg$screening$flag_threshold_to_remove
  data$qc_remove <- data$n_flags >= threshold
  
  n_remove <- sum(data$qc_remove)
  log_msg(log_info, sprintf("Total flagged for removal: %d (%.1f%%)",
                             n_remove, 100 * n_remove / n_original))

  # --- 2.5b. Duplicate Triage ---
  if ("flag_duplicate" %in% flag_cols) {
    n_dup_total    <- sum(data$flag_duplicate, na.rm = TRUE)
    n_dup_removed  <- sum(data$flag_duplicate & data$qc_remove, na.rm = TRUE)
    n_dup_retained <- n_dup_total - n_dup_removed
    log_msg(log_info, sprintf("Duplicate triage: %d flagged, %d removed by QC, %d remain in dataset",
                               n_dup_total, n_dup_removed, n_dup_retained))
    if (n_dup_retained > 0) {
      log_msg(log_info, sprintf("  Note: %d duplicates retained — Likert coincidence plausible at N=%d with limited scale points",
                                 n_dup_retained, n_original))
      log_msg(log_info, "  These are flagged (flag_duplicate=TRUE) for sensitivity analysis if needed")
    }
  }

  # --- 2.6. Xuất kết quả ---
  dir.create("03_qc", showWarnings = FALSE)
  dir.create("02_clean", showWarnings = FALSE)
  
  # Chi tiết cờ
  flag_detail <- data[, c("Responder ID", flag_cols, "n_flags", "qc_remove",
                           "longstring_max", "irv"), drop = FALSE]
  write.csv(flag_detail, "03_qc/qc_flags_detail.csv", row.names = FALSE)
  
  # Summary
  summary_df <- data.frame(
    Criterion = c("Straight-lining", "Long-string", "Speeding", "Low IRV", "Total removed"),
    N_flagged = c(n_sl, n_ls, sum(data$flag_speeding), n_inc, n_remove),
    Pct = round(100 * c(n_sl, n_ls, sum(data$flag_speeding), n_inc, n_remove) / n_original, 1)
  )
  write.csv(summary_df, "03_qc/qc_summary.csv", row.names = FALSE)
  
  # Phễu sàng lọc
  tryCatch({
    library(ggplot2)
    funnel_data <- data.frame(
      Stage = factor(c("Raw", "After Tech QC", "After Behavioral QC"),
                     levels = c("Raw", "After Tech QC", "After Behavioral QC")),
      N = c(nrow(readRDS("01_raw/raw_data.rds")), n_original, n_original - n_remove)
    )
    p <- ggplot(funnel_data, aes(x = Stage, y = N)) +
      geom_col(fill = c("#4e79a7", "#59a14f", "#e15759"), width = 0.6) +
      geom_text(aes(label = N), vjust = -0.5, size = 5) +
      labs(title = "Screening Funnel", y = "N observations", x = "") +
      theme_minimal(base_size = 14)
    ggsave("03_qc/screening_funnel.png", p, width = 8, height = 5, dpi = 150)
    log_msg(log_info, "Screening funnel plot saved")
  }, error = function(e) {
    log_warn(log_info, paste("Could not create funnel plot:", e$message))
  })
  
  # Lưu 2 phiên bản
  saveRDS(data, "02_clean/clean_qc_flagged.rds")  # Có cờ, chưa loại
  
  data_pass <- data[!data$qc_remove, ]
  saveRDS(data_pass, "02_clean/clean_qc.rds")      # Đã loại
  
  log_msg(log_info, sprintf("After QC screening: %d rows (removed %d)", nrow(data_pass), n_remove))
  
  # Gate check
  default_min <- 10 * max(sapply(cfg$constructs, function(c) length(c$indicators)))
  min_sample  <- if (!is.null(pilot_min_n)) pilot_min_n else default_min
  if (!is.null(pilot_min_n)) {
    log_msg(log_info, sprintf("QC gate uses pilot_min_n=%d (default would be %d)", min_sample, default_min))
  }
  gate_pass <- nrow(data_pass) >= min_sample
  log_info <- log_gate(log_info, "Behavioral QC", gate_pass,
                       sprintf("N=%d, minimum required=%d", nrow(data_pass), min_sample))
  
  data_pass
}
