# ==============================================================================
# 04_descriptives.R — Step 4: Descriptive Statistics, Demographics & Control Vars
# Gate: "Dữ liệu không bất thường thô"
# Gộp: thống kê mô tả + demographic profile + dummy/ordinal coding
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

#' Chạy thống kê mô tả, nhân khẩu học, và mã hoá biến kiểm soát
#' @param data data.frame từ Step 3
#' @param cfg list config
#' @param log_info list log
#' @param stage "pilot" or "main" — affects gate severity
#' @return data.frame với các cột dummy/numeric mới được thêm vào
run_descriptives <- function(data, cfg, log_info, stage = "main") {

  log_step(log_info, "Step 4: Descriptive Statistics & Plausibility Checks")

  dir.create("04_descriptives", showWarnings = FALSE, recursive = TRUE)
  ind_cols <- intersect(cfg$all_indicators, names(data))
  n_total  <- nrow(data)

  # ============================================================================
  # 4.1. Descriptive Statistics — Indicators
  # ============================================================================
  desc_stats <- data.frame(
    Variable  = ind_cols,
    N         = sapply(ind_cols, function(v) sum(!is.na(data[[v]]))),
    Mean      = round(sapply(ind_cols, function(v) mean(data[[v]], na.rm = TRUE)), 3),
    SD        = round(sapply(ind_cols, function(v) sd(data[[v]], na.rm = TRUE)), 3),
    Min       = sapply(ind_cols, function(v) min(data[[v]], na.rm = TRUE)),
    Max       = sapply(ind_cols, function(v) max(data[[v]], na.rm = TRUE)),
    Skewness  = round(sapply(ind_cols, function(v) {
      x <- data[[v]][!is.na(data[[v]])]
      n <- length(x)
      (n / ((n-1)*(n-2))) * sum(((x - mean(x)) / sd(x))^3)
    }), 3),
    Kurtosis  = round(sapply(ind_cols, function(v) {
      x <- data[[v]][!is.na(data[[v]])]
      m4 <- mean((x - mean(x))^4)
      s4 <- sd(x)^4
      m4/s4 - 3
    }), 3),
    stringsAsFactors = FALSE, row.names = NULL
  )

  write.csv(desc_stats, "04_descriptives/descriptive_stats.csv", row.names = FALSE)
  log_msg(log_info, sprintf("Descriptive stats: %d variables computed", nrow(desc_stats)))

  # Cảnh báo skewness/kurtosis cực
  extreme_skew <- desc_stats$Variable[abs(desc_stats$Skewness) > 2]
  extreme_kurt <- desc_stats$Variable[abs(desc_stats$Kurtosis) > 7]
  if (length(extreme_skew) > 0)
    log_warn(log_info, paste("Extreme skewness (|>2|):", paste(extreme_skew, collapse = ", ")))
  if (length(extreme_kurt) > 0)
    log_warn(log_info, paste("Extreme kurtosis (|>7|):", paste(extreme_kurt, collapse = ", ")))

  # ============================================================================
  # 4.2. Correlation Matrix
  # ============================================================================
  cor_matrix <- round(cor(data[, ind_cols, drop = FALSE], use = "pairwise.complete.obs"), 3)
  write.csv(cor_matrix, "04_descriptives/correlation_matrix.csv")

  diag(cor_matrix) <- NA
  high_cor <- which(abs(cor_matrix) > 0.95, arr.ind = TRUE)
  if (nrow(high_cor) > 0) {
    log_warn(log_info, sprintf("Suspiciously high correlations (|r|>0.95): %d pairs", nrow(high_cor) / 2))
    for (i in seq_len(nrow(high_cor))) {
      if (high_cor[i, 1] < high_cor[i, 2]) {
        log_warn(log_info, sprintf("  %s ~ %s: r=%.3f",
                                    ind_cols[high_cor[i, 1]], ind_cols[high_cor[i, 2]],
                                    cor_matrix[high_cor[i, 1], high_cor[i, 2]]))
      }
    }
  }

  # ============================================================================
  # 4.3. Demographic Profile (Table 1 for thesis)
  # ============================================================================
  log_msg(log_info, "--- 4b.1. Demographic Profile (Table 1) ---")

  demo_rows <- list()

  # 1) Giới tính (Gen)
  if ("Gen" %in% names(data)) {
    tab <- table(data[["Gen"]], useNA = "ifany")
    demo_rows[["gen"]] <- data.frame(
      Variable = "Giới tính",
      Category = names(tab),
      N = as.integer(tab),
      Pct = round(100 * as.numeric(tab) / n_total, 1),
      stringsAsFactors = FALSE
    )
    log_msg(log_info, sprintf("  Giới tính: %d categories, N=%d",
                               nrow(demo_rows[["gen"]]), sum(demo_rows[["gen"]]$N)))
  }

  # 2) Bằng cấp — derived from Cert column
  if ("Cert" %in% names(data)) {
    edu <- ifelse(grepl("Sau đại học", data[["Cert"]], ignore.case = TRUE),
                  "Trên đại học", "Đại học")
    tab_edu <- table(edu, useNA = "ifany")
    demo_rows[["edu"]] <- data.frame(
      Variable = "Bằng cấp",
      Category = names(tab_edu),
      N = as.integer(tab_edu),
      Pct = round(100 * as.numeric(tab_edu) / n_total, 1),
      stringsAsFactors = FALSE
    )
    log_msg(log_info, sprintf("  Bằng cấp: %d categories, N=%d",
                               nrow(demo_rows[["edu"]]), sum(demo_rows[["edu"]]$N)))
  }

  # 3) Chứng chỉ hành nghề — derived from Cert column
  if ("Cert" %in% names(data)) {
    cert_yn <- ifelse(grepl("Chứng chỉ", data[["Cert"]], ignore.case = TRUE),
                      "Có", "Không")
    tab_cert <- table(cert_yn, useNA = "ifany")
    demo_rows[["cert"]] <- data.frame(
      Variable = "Chứng chỉ hành nghề",
      Category = names(tab_cert),
      N = as.integer(tab_cert),
      Pct = round(100 * as.numeric(tab_cert) / n_total, 1),
      stringsAsFactors = FALSE
    )
    log_msg(log_info, sprintf("  Chứng chỉ hành nghề: %d categories, N=%d",
                               nrow(demo_rows[["cert"]]), sum(demo_rows[["cert"]]$N)))
  }

  # 4) Kinh nghiệm (Exp)
  if ("Exp" %in% names(data)) {
    tab_exp <- table(data[["Exp"]], useNA = "ifany")
    demo_rows[["exp"]] <- data.frame(
      Variable = "Kinh nghiệm",
      Category = names(tab_exp),
      N = as.integer(tab_exp),
      Pct = round(100 * as.numeric(tab_exp) / n_total, 1),
      stringsAsFactors = FALSE
    )
    log_msg(log_info, sprintf("  Kinh nghiệm: %d categories, N=%d",
                               nrow(demo_rows[["exp"]]), sum(demo_rows[["exp"]]$N)))
  }

  # 5) Vị trí (Pos)
  if ("Pos" %in% names(data)) {
    tab_pos <- table(data[["Pos"]], useNA = "ifany")
    demo_rows[["pos"]] <- data.frame(
      Variable = "Vị trí",
      Category = names(tab_pos),
      N = as.integer(tab_pos),
      Pct = round(100 * as.numeric(tab_pos) / n_total, 1),
      stringsAsFactors = FALSE
    )
    log_msg(log_info, sprintf("  Vị trí: %d categories, N=%d",
                               nrow(demo_rows[["pos"]]), sum(demo_rows[["pos"]]$N)))
  }

  if (length(demo_rows) > 0) {
    demo_profile <- do.call(rbind, demo_rows)
    row.names(demo_profile) <- NULL
    write.csv(demo_profile, "04_descriptives/demographic_profile.csv", row.names = FALSE)
    log_msg(log_info, sprintf("  Saved: 04_descriptives/demographic_profile.csv (%d rows)",
                               nrow(demo_profile)))
    # Also save as sample_demographics for backward compatibility
    write.csv(demo_profile, "04_descriptives/sample_demographics.csv", row.names = FALSE)
    log_msg(log_info, sprintf("Sample demographics: %d variables summarized", length(demo_rows)))
  } else {
    log_warn(log_info, "  No demographic variables available for Table 1")
  }

  # ============================================================================
  # 4.4. Distribution Plots
  # ============================================================================
  tryCatch({
    pdf("04_descriptives/distribution_plots.pdf", width = 12, height = 8)
    for (cc in cfg$constructs) {
      block_cols <- intersect(cc$indicators, names(data))
      if (length(block_cols) > 0) {
        plot_data <- tidyr::pivot_longer(
          data[, block_cols, drop = FALSE],
          cols = everything(),
          names_to = "Indicator",
          values_to = "Value"
        )
        p <- ggplot(plot_data, aes(x = Value)) +
          geom_histogram(bins = 5, fill = "#4e79a7", color = "white", alpha = 0.8) +
          facet_wrap(~ Indicator, scales = "free_y") +
          labs(title = paste(cc$name, "-", cc$full_name),
               x = "Response", y = "Frequency") +
          theme_minimal(base_size = 12)
        print(p)
      }
    }
    dev.off()
    log_msg(log_info, "Distribution plots saved: 04_descriptives/distribution_plots.pdf")
  }, error = function(e) {
    log_warn(log_info, paste("Could not create distribution plots:", e$message))
  })

  # ============================================================================
  # 4.5. Gate Check — Descriptive Statistics
  # ============================================================================
  # Tier 1 (FAIL): data validity issues
  out_of_range <- character()
  for (v in ind_cols) {
    vals <- data[[v]][!is.na(data[[v]])]
    if (any(vals < cfg$scale$min | vals > cfg$scale$max))
      out_of_range <- c(out_of_range, v)
  }
  if (length(out_of_range) > 0)
    log_warn(log_info, paste("Out-of-range values:", paste(out_of_range, collapse = ", ")))

  constant_items <- desc_stats$Variable[desc_stats$SD == 0]
  if (length(constant_items) > 0)
    log_warn(log_info, paste("Constant (zero-variance) items:", paste(constant_items, collapse = ", ")))

  has_out_of_range   <- length(out_of_range) > 0
  has_constant_items <- length(constant_items) > 0
  has_high_cor       <- nrow(high_cor) > 0
  has_dist_warnings  <- length(extreme_skew) > 0 || length(extreme_kurt) > 0

  # In pilot, high correlations / zero-variance are deferred to the
  # Pre-PLS Feasibility gate — Descriptives only FAILs for out-of-range values.
  if (stage == "pilot") {
    data_validity_fail <- has_out_of_range  # hard fail only for out-of-range
    has_warnings <- has_constant_items || has_high_cor || has_dist_warnings
  } else {
    data_validity_fail <- has_out_of_range || has_constant_items || has_high_cor
    has_warnings <- has_dist_warnings
  }

  if (data_validity_fail) {
    log_info <- log_gate(log_info, "Descriptives", FALSE,
                         "Data validity issues detected — review before proceeding")
    log_msg(log_info, "Descriptives complete — data NOT cleared: resolve validity issues first")
  } else if (has_warnings) {
    warn_parts <- character()
    if (has_dist_warnings) warn_parts <- c(warn_parts, "skew/kurtosis anomalies")
    if (stage == "pilot" && has_high_cor)
      warn_parts <- c(warn_parts, sprintf("high correlations |r|>0.95 (%d pairs, deferred to feasibility gate)", nrow(high_cor) / 2))
    if (stage == "pilot" && has_constant_items)
      warn_parts <- c(warn_parts, sprintf("zero-variance items (%s, deferred to feasibility gate)", paste(constant_items, collapse = ", ")))
    log_info <- log_gate(log_info, "Descriptives", TRUE,
                         sprintf("PASS with warnings — %s", paste(warn_parts, collapse = "; ")))
    log_msg(log_info, "Descriptives complete — data cleared for modeling (warnings noted)")
  } else {
    log_info <- log_gate(log_info, "Descriptives", TRUE,
                         "No extreme anomalies detected")
    log_msg(log_info, "Descriptives complete — data cleared for modeling")
  }

  # ============================================================================
  # 4.6. Control Variable Preparation — Dummy / Ordinal Coding
  # ============================================================================
  log_msg(log_info, "--- 4b.2. Dummy / Ordinal Coding ---")

  ctrl_vars <- cfg$control_vars

  if (is.null(ctrl_vars) || length(ctrl_vars) == 0) {
    log_msg(log_info, "  No control_vars defined in config — skipping coding")
    return(data)
  }

  coded_count <- 0

  for (cv in ctrl_vars) {
    src_col    <- cv$source_column
    target_col <- cv$target_column
    coding     <- cv$coding_type

    if (!src_col %in% names(data)) {
      log_warn(log_info, sprintf("  [SKIP] Source column '%s' not found in data", src_col))
      next
    }

    # ---- DUMMY coding (binary: value_map) ----
    if (coding == "dummy") {
      value_map <- cv$value_map
      if (is.null(value_map)) {
        log_warn(log_info, sprintf("  [SKIP] %s: dummy coding requires 'value_map'", src_col))
        next
      }

      data[[target_col]] <- NA_real_
      for (label in names(value_map)) {
        data[[target_col]][data[[src_col]] == label] <- as.numeric(value_map[[label]])
      }

      n_mapped   <- sum(!is.na(data[[target_col]]))
      n_unmapped <- sum(is.na(data[[target_col]]))

      log_msg(log_info, sprintf("  [DUMMY] %s -> %s: %d mapped, %d unmapped",
                                 src_col, target_col, n_mapped, n_unmapped))
      if (n_unmapped > 0) {
        unmapped_vals <- unique(data[[src_col]][is.na(data[[target_col]])])
        log_warn(log_info, sprintf("    Unmapped values: %s",
                                    paste(head(unmapped_vals, 10), collapse = ", ")))
      }
      coded_count <- coded_count + 1
    }

    # ---- ORDINAL coding (ordered categories -> numeric ranks) ----
    else if (coding == "ordinal") {
      level_order <- cv$levels
      if (is.null(level_order)) {
        log_warn(log_info, sprintf("  [SKIP] %s: ordinal coding requires 'levels'", src_col))
        next
      }

      rank_map <- setNames(seq_along(level_order), level_order)
      data[[target_col]] <- rank_map[data[[src_col]]]

      n_mapped   <- sum(!is.na(data[[target_col]]))
      n_unmapped <- sum(is.na(data[[target_col]]) & !is.na(data[[src_col]]))

      log_msg(log_info, sprintf("  [ORDINAL] %s -> %s: %d mapped (1..%d), %d unmapped",
                                 src_col, target_col, n_mapped, length(level_order), n_unmapped))
      if (n_unmapped > 0) {
        unmapped_vals <- unique(data[[src_col]][is.na(data[[target_col]]) & !is.na(data[[src_col]])])
        log_warn(log_info, sprintf("    Unmapped values: %s",
                                    paste(head(unmapped_vals, 10), collapse = ", ")))
      }
      coded_count <- coded_count + 1
    }

    # ---- NUMERIC_PASSTHROUGH (already numeric, just rename/coerce) ----
    else if (coding == "numeric_passthrough") {
      data[[target_col]] <- suppressWarnings(as.numeric(data[[src_col]]))
      n_valid <- sum(!is.na(data[[target_col]]))
      log_msg(log_info, sprintf("  [NUMERIC] %s -> %s: %d valid numeric values",
                                 src_col, target_col, n_valid))
      coded_count <- coded_count + 1
    }

    # ---- MULTI_DUMMY (k categories -> k-1 dummy columns) ----
    else if (coding == "multi_dummy") {
      ref_category <- cv$reference_category
      categories <- sort(unique(data[[src_col]][!is.na(data[[src_col]])]))

      if (is.null(ref_category)) {
        ref_category <- categories[1]
        log_msg(log_info, sprintf("    Reference category (auto): '%s'", ref_category))
      }

      dummy_categories <- setdiff(categories, ref_category)
      for (cat in dummy_categories) {
        dummy_name <- paste0(target_col, "_", gsub("[^a-zA-Z0-9]", "", cat))
        data[[dummy_name]] <- as.integer(data[[src_col]] == cat)
        log_msg(log_info, sprintf("    Created: %s (N=1: %d)",
                                   dummy_name, sum(data[[dummy_name]] == 1, na.rm = TRUE)))
      }

      log_msg(log_info, sprintf("  [MULTI_DUMMY] %s -> %d dummies (ref='%s')",
                                 src_col, length(dummy_categories), ref_category))
      coded_count <- coded_count + 1
    }

    # ---- REGEX_DUMMY (pattern match in text -> 1/0) ----
    else if (coding == "regex_dummy") {
      pattern <- cv$pattern
      if (is.null(pattern)) {
        log_warn(log_info, sprintf("  [SKIP] %s: regex_dummy coding requires 'pattern'", src_col))
        next
      }

      data[[target_col]] <- as.integer(grepl(pattern, data[[src_col]], ignore.case = TRUE))
      n_match    <- sum(data[[target_col]] == 1, na.rm = TRUE)
      n_no_match <- sum(data[[target_col]] == 0, na.rm = TRUE)

      log_msg(log_info, sprintf("  [REGEX_DUMMY] %s -> %s: pattern='%s', match=%d, no_match=%d",
                                 src_col, target_col, pattern, n_match, n_no_match))
      coded_count <- coded_count + 1
    }

    else {
      log_warn(log_info, sprintf("  [SKIP] %s: unknown coding_type '%s'", src_col, coding))
    }
  }

  log_msg(log_info, sprintf("\nControl variable coding complete: %d variable(s) coded", coded_count))

  # ============================================================================
  # 4.7. Summary of New Coded Columns
  # ============================================================================
  new_cols <- sapply(ctrl_vars, function(cv) cv$target_column)
  new_cols_present <- intersect(new_cols, names(data))

  if (length(new_cols_present) > 0) {
    log_msg(log_info, "\n--- New Coded Columns Summary ---")
    for (col in new_cols_present) {
      vals <- data[[col]]
      n_valid <- sum(!is.na(vals))
      if (n_valid > 0) {
        log_msg(log_info, sprintf("  %s: N=%d, range=[%.0f, %.0f], M=%.2f, SD=%.2f",
                                   col, n_valid,
                                   min(vals, na.rm = TRUE), max(vals, na.rm = TRUE),
                                   mean(vals, na.rm = TRUE), sd(vals, na.rm = TRUE)))
      }
    }

    coding_summary <- data.frame(
      Source = sapply(ctrl_vars, function(cv) cv$source_column),
      Target = sapply(ctrl_vars, function(cv) cv$target_column),
      Type = sapply(ctrl_vars, function(cv) cv$coding_type),
      stringsAsFactors = FALSE
    )
    write.csv(coding_summary, "04_descriptives/control_var_coding.csv", row.names = FALSE)
  }

  data
}
