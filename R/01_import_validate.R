# ==============================================================================
# 01_import_validate.R — Step 1: Import & Technical Validation
# Gate: "Đúng định dạng"
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

#' Nhập dữ liệu và kiểm tra tính hợp lệ kỹ thuật
#' @param cfg list config
#' @param log_info list log
#' @return data.frame đã qua kiểm tra kỹ thuật
import_and_validate <- function(cfg, log_info) {
  
  log_step(log_info, "Step 1: Import & Technical Validation")
  
  # --- 1.1. Đọc dữ liệu ---
  raw <- read_excel(cfg$paths$data_file, sheet = cfg$paths$data_sheet)
  log_msg(log_info, sprintf("Raw data: %d rows x %d cols", nrow(raw), ncol(raw)))
  
  # Lưu snapshot gốc
  dir.create("01_raw", showWarnings = FALSE)
  saveRDS(raw, "01_raw/raw_data.rds")
  log_msg(log_info, "Raw snapshot saved: 01_raw/raw_data.rds")
  
  # --- 1.2. Đối chiếu cột với codebook ---
  expected_indicators <- cfg$all_indicators
  actual_cols <- names(raw)
  
  missing_cols <- setdiff(expected_indicators, actual_cols)
  if (length(missing_cols) > 0) {
    log_warn(log_info, paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }
  
  extra_cols <- setdiff(actual_cols, c(expected_indicators, cfg$demographics, cfg$meta_columns))
  if (length(extra_cols) > 0) {
    log_msg(log_info, paste("Extra columns (not in config):", paste(extra_cols, collapse = ", ")))
  }
  
  # --- 1.3. Chuẩn hóa kiểu dữ liệu ---
  validation_issues <- data.frame(
    column = character(), issue = character(), count = integer(),
    stringsAsFactors = FALSE
  )
  
  for (ind in expected_indicators) {
    if (ind %in% actual_cols) {
      # Ép numeric
      raw[[ind]] <- suppressWarnings(as.numeric(raw[[ind]]))
      
      # Kiểm tra phạm vi thang đo
      vals <- raw[[ind]][!is.na(raw[[ind]])]
      out_of_range <- sum(vals < cfg$scale$min | vals > cfg$scale$max)
      if (out_of_range > 0) {
        validation_issues <- rbind(validation_issues, data.frame(
          column = ind,
          issue  = sprintf("Out of range [%d-%d]", cfg$scale$min, cfg$scale$max),
          count  = out_of_range,
          stringsAsFactors = FALSE
        ))
        log_warn(log_info, sprintf("%s: %d values out of range [%d-%d]",
                                   ind, out_of_range, cfg$scale$min, cfg$scale$max))
      }
    }
  }
  
  # --- 1.4. Đảo chiều (reverse code) ---
  for (cc in cfg$constructs) {
    if (!is.null(cc$reverse_coded) && length(cc$reverse_coded) > 0) {
      for (rev_item in cc$reverse_coded) {
        if (rev_item %in% names(raw)) {
          raw[[rev_item]] <- (cfg$scale$max + cfg$scale$min) - raw[[rev_item]]
          log_msg(log_info, sprintf("Reverse coded: %s", rev_item))
        }
      }
    }
  }
  
  # --- 1.5. Loại dòng trống hoàn toàn ---
  ind_cols <- intersect(expected_indicators, names(raw))
  all_na_rows <- apply(raw[, ind_cols], 1, function(r) all(is.na(r)))
  n_empty <- sum(all_na_rows)
  if (n_empty > 0) {
    raw <- raw[!all_na_rows, ]
    log_msg(log_info, sprintf("Removed %d completely empty rows", n_empty))
  }
  
  # Loại dòng thiếu > 80% indicators
  missing_pct <- apply(raw[, ind_cols], 1, function(r) mean(is.na(r)))
  high_missing <- missing_pct > 0.80
  n_high <- sum(high_missing)
  if (n_high > 0) {
    raw <- raw[!high_missing, ]
    log_msg(log_info, sprintf("Removed %d rows with >80%% missing indicators", n_high))
  }
  
  # --- 1.6. Gắn cờ trùng lặp kỹ thuật ---
  if ("Time" %in% names(raw)) {
    # Kiểm tra timestamp trùng (cùng phút)
    raw$flag_duplicate <- duplicated(raw[, ind_cols]) | duplicated(raw[, ind_cols], fromLast = TRUE)
    n_dup <- sum(raw$flag_duplicate)
    if (n_dup > 0) {
      log_warn(log_info, sprintf("Flagged %d potential duplicates (same response pattern)", n_dup))
    }
  } else {
    raw$flag_duplicate <- FALSE
  }
  
  # --- 1.7. Xuất kết quả ---
  dir.create("02_clean", showWarnings = FALSE)
  saveRDS(raw, "02_clean/clean_tech.rds")
  
  write.csv(validation_issues, "02_clean/tech_validation_report.csv", row.names = FALSE)
  
  log_msg(log_info, sprintf("After tech validation: %d rows x %d cols", nrow(raw), ncol(raw)))
  
  # Gate check
  gate_pass <- length(missing_cols) == 0
  log_info <- log_gate(log_info, "Tech Validation", gate_pass,
                       ifelse(gate_pass, "All expected columns present",
                              paste("Missing:", paste(missing_cols, collapse = ", "))))
  
  raw
}
