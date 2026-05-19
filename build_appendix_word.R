suppressPackageStartupMessages({
  library(officer)
  library(flextable)
})

root_dir <- "."
docx_output <- file.path(root_dir, "Phu_luc_ket_qua_phan_tich_du_lieu.docx")
manifest_output <- file.path(root_dir, "appendix_csv_manifest.csv")
build_log_output <- file.path(root_dir, "appendix_build_log.md")

# Formatting constants (A4 thesis appendix)
margin_cfg <- page_mar(
  top = 3.5 / 2.54,
  bottom = 3.0 / 2.54,
  left = 3.5 / 2.54,
  right = 2.5 / 2.54
)

portrait_section <- prop_section(
  page_size = page_size(orient = "portrait", width = 8.27, height = 11.69),
  page_margins = margin_cfg,
  type = "continuous"
)

landscape_section <- prop_section(
  page_size = page_size(orient = "landscape", width = 11.69, height = 8.27),
  page_margins = margin_cfg,
  type = "continuous"
)

read_csv_robust <- function(path) {
  encodings <- c("UTF-8", "UTF-8-BOM", "windows-1258", "latin1")
  last_err <- NULL

  for (enc in encodings) {
    out <- tryCatch({
      df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = enc)
      list(ok = TRUE, df = df, encoding = enc, err = NULL)
    }, error = function(e) {
      last_err <<- conditionMessage(e)
      list(ok = FALSE, df = NULL, encoding = enc, err = conditionMessage(e))
    })

    if (out$ok) {
      return(out)
    }
  }

  list(ok = FALSE, df = NULL, encoding = NA_character_, err = last_err)
}

collapse_names <- function(x) {
  if (length(x) == 0) return("")
  paste(x, collapse = " | ")
}

normalize_rel <- function(path_str) {
  p <- gsub("\\\\", "/", path_str)
  p <- sub("^\\./", "", p)
  p
}

infer_step <- function(rel_path) {
  p <- gsub("\\\\", "/", rel_path)
  if (grepl("^02_clean/", p)) return("Tiền xử lý và dữ liệu thiếu")
  if (grepl("^03_qc/", p)) return("Kiểm soát chất lượng hành vi")
  if (grepl("^04_cmv/", p)) return("Đánh giá CMV")
  if (grepl("^04_descriptives/", p)) return("Thống kê mô tả và hồ sơ mẫu")
  if (grepl("^05_measurement/reflective/", p)) return("Đo lường phản xạ")
  if (grepl("^05_measurement/formative/", p)) return("Đo lường tạo thành")
  if (grepl("^06_structural/", p)) return("Mô hình cấu trúc")
  if (grepl("^07_predict/", p)) return("Đánh giá dự báo")
  if (grepl("^08_complex/mediation/", p)) return("Kiểm định trung gian")
  if (grepl("^08_complex/moderation/", p)) return("Kiểm định điều tiết")
  if (grepl("^09_robust/", p)) return("Kiểm định độ bền và nhạy cảm")
  if (grepl("^10_report/com6_sensitivity/", p)) return("Tổng hợp nhạy cảm COM6")
  if (grepl("^10_report/", p)) return("Báo cáo tổng hợp")
  return("Khác")
}

should_include_full_table <- function(rel_path, n_rows, n_cols) {
  p <- gsub("\\\\", "/", rel_path)

  reporting_patterns <- c(
    "descriptive_stats", "construct_descriptive_stats", "correlation_matrix",
    "demographic_profile", "sample_demographics", "full_collinearity_vif",
    "outer_loadings", "outer_vif", "outer_weights", "reliability", "htmt",
    "path_coefficients", "r_squared", "f_squared", "plspredict", "prediction_classification",
    "mediation", "indirect", "interaction", "simple_slopes", "copula", "micom", "mga",
    "nonlinear", "comparison", "diagnostics"
  )

  is_reporting <- any(vapply(reporting_patterns, function(k) grepl(k, p, ignore.case = TRUE), logical(1)))

  if (grepl("package_versions|pilot_ids", p, ignore.case = TRUE)) {
    return(list(include = FALSE, reason = "Tệp metadata phiên bản/ID, không phải bảng kết quả phân tích"))
  }

  if (n_rows == 0) {
    return(list(include = FALSE, reason = "Tệp rỗng"))
  }

  if (grepl("qc_flags_detail", p, ignore.case = TRUE) && n_rows > 120) {
    return(list(include = FALSE, reason = "Bảng mức dòng quá dài; chỉ tóm tắt trong phụ lục"))
  }

  if (!is_reporting && n_rows > 120) {
    return(list(include = FALSE, reason = "Bảng trung gian quá dài, ưu tiên ghi trong manifest"))
  }

  if (n_cols > 25 && n_rows > 80) {
    return(list(include = FALSE, reason = "Bảng rất rộng và dài; đưa vào manifest để đối chiếu"))
  }

  list(include = TRUE, reason = "")
}

make_ft <- function(df, wide = FALSE) {
  if (ncol(df) > 0) {
    nm <- names(df)
    nm[nm == "" | is.na(nm)] <- paste0("col_", which(nm == "" | is.na(nm)))
    names(df) <- make.unique(nm, sep = "_")
  }

  ft <- flextable(df)
  ft <- font(ft, fontname = "Times New Roman", part = "all")
  ft <- fontsize(ft, size = if (wide) 10 else 11, part = "all")
  ft <- bold(ft, part = "header")
  ft <- align(ft, align = "left", part = "header")
  ft <- border_inner(ft, border = fp_border(color = "#666666", width = 0.5))
  ft <- border_outer(ft, border = fp_border(color = "#000000", width = 1))

  if (ncol(df) > 0) {
    for (j in seq_len(ncol(df))) {
      col_data <- df[[j]]
      if (is.numeric(col_data) || is.integer(col_data)) {
        ft <- align(ft, j = j, align = "right", part = "body")
      } else {
        ft <- align(ft, j = j, align = "left", part = "body")
      }
    }
  }

  ft <- set_table_properties(ft, layout = "autofit", opts_word = list(repeat_headers = TRUE))
  ft <- autofit(ft)
  ft
}

csv_files <- list.files(root_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
csv_files <- csv_files[!grepl("^\\.git/", gsub("\\\\", "/", csv_files))]
csv_files <- sort(csv_files)

manifest <- data.frame(
  csv_path = character(),
  inferred_pipeline_step = character(),
  included_in_word = character(),
  reason_if_excluded = character(),
  n_rows = integer(),
  n_columns = integer(),
  column_names = character(),
  notes = character(),
  stringsAsFactors = FALSE
)

csv_data <- list()

for (f in csv_files) {
  rel <- normalize_rel(f)
  info <- file.info(f)

  if (is.na(info$size) || info$size == 0) {
    manifest <- rbind(
      manifest,
      data.frame(
        csv_path = rel,
        inferred_pipeline_step = infer_step(rel),
        included_in_word = "no",
        reason_if_excluded = "Tệp rỗng hoặc không đọc được",
        n_rows = 0,
        n_columns = 0,
        column_names = "",
        notes = "Kích thước tệp = 0",
        stringsAsFactors = FALSE
      )
    )
    next
  }

  rd <- read_csv_robust(f)

  if (!rd$ok) {
    manifest <- rbind(
      manifest,
      data.frame(
        csv_path = rel,
        inferred_pipeline_step = infer_step(rel),
        included_in_word = "no",
        reason_if_excluded = "Không đọc được CSV",
        n_rows = 0,
        n_columns = 0,
        column_names = "",
        notes = paste("Read error:", rd$err),
        stringsAsFactors = FALSE
      )
    )
    next
  }

  df <- rd$df
  n_rows <- nrow(df)
  n_cols <- ncol(df)
  dup_cols <- names(df)[duplicated(names(df))]
  inclusion <- should_include_full_table(rel, n_rows, n_cols)

  notes <- c(paste("encoding=", rd$encoding))
  if (length(dup_cols) > 0) {
    notes <- c(notes, paste("duplicated_columns=", paste(unique(dup_cols), collapse = ";")))
  }
  if (any(names(df) == "")) {
    notes <- c(notes, "empty_column_name_detected")
  }

  manifest <- rbind(
    manifest,
    data.frame(
      csv_path = rel,
      inferred_pipeline_step = infer_step(rel),
      included_in_word = if (inclusion$include) "yes" else "no",
      reason_if_excluded = inclusion$reason,
      n_rows = n_rows,
      n_columns = n_cols,
      column_names = collapse_names(names(df)),
      notes = collapse_names(notes),
      stringsAsFactors = FALSE
    )
  )

  csv_data[[rel]] <- df
}

write.csv(manifest, manifest_output, row.names = FALSE, fileEncoding = "UTF-8")

# Build Word document
doc <- read_docx()

add_title_page <- function(doc_obj) {
  doc_obj <- body_add_par(doc_obj, "PHỤ LỤC: KẾT QUẢ CÁC BƯỚC PHÂN TÍCH DỮ LIỆU", style = "heading 1")
  doc_obj <- body_add_par(doc_obj, "Luận án tiến sĩ", style = "Normal")
  doc_obj <- body_add_par(
    doc_obj,
    "Phụ lục này tổng hợp các kết quả đã được xuất ra từ các tệp CSV của quy trình phân tích dữ liệu. Các bảng được trình bày nhằm hỗ trợ việc kiểm tra, đối chiếu và tái lập kết quả trong chương phương pháp và chương kết quả.",
    style = "Normal"
  )
  doc_obj <- body_end_block_section(doc_obj, block_section(portrait_section))
  doc_obj
}

doc <- add_title_page(doc)

section_titles <- c(
  "A" = "Phụ lục A. Tổng quan quy trình phân tích dữ liệu",
  "B" = "Phụ lục B. Danh mục các tệp kết quả CSV được sử dụng",
  "C" = "Phụ lục C. Kết quả tiền xử lý và kiểm soát chất lượng dữ liệu",
  "D" = "Phụ lục D. Kết quả mô tả mẫu và thống kê mô tả",
  "E" = "Phụ lục E. Kết quả đánh giá thang đo và cấu trúc đo lường",
  "F" = "Phụ lục F. Kết quả mô hình chính",
  "G" = "Phụ lục G. Kiểm định bổ sung và độ bền",
  "H" = "Phụ lục H. Nhật ký đối chiếu và kiểm tra nhất quán",
  "I" = "Phụ lục I. Ghi chú về các tệp chưa đưa vào phụ lục"
)

section_intro <- c(
  "A" = "Mục này mô tả thứ tự các bước trong pipeline phân tích, được đối chiếu từ README, run_pipeline.R, run_core.R và các script module trong thư mục R.",
  "B" = "Danh mục này liệt kê các tệp CSV kết quả đã được quét từ repository và thông tin cấu trúc cơ bản của từng tệp.",
  "C" = "Mục này trình bày các bảng kết quả liên quan đến kiểm tra kỹ thuật, xử lý dữ liệu thiếu và kiểm soát chất lượng hành vi.",
  "D" = "Mục này tổng hợp các bảng mô tả mẫu, thống kê mô tả và ma trận tương quan được xuất từ pipeline.",
  "E" = "Mục này trình bày kết quả đánh giá đo lường cho các cấu trúc phản xạ và tạo thành.",
  "F" = "Mục này tổng hợp kết quả mô hình cấu trúc và đánh giá khả năng dự báo của mô hình chính.",
  "G" = "Mục này trình bày các kiểm định trung gian, điều tiết, endogeneity, heterogeneity, nonlinear và các phân tích nhạy cảm.",
  "H" = "Mục này đối chiếu tính nhất quán giữa các tệp kết quả theo trình tự pipeline và ghi nhận các điểm cần lưu ý.",
  "I" = "Mục này ghi nhận các tệp không đưa bảng đầy đủ vào Word để đảm bảo tính đọc được và tập trung vào bảng báo cáo chính."
)

for (sec in names(section_titles)) {
  doc <- body_add_break(doc, pos = "after")
  doc <- body_add_par(doc, section_titles[[sec]], style = "heading 1")
  doc <- body_add_par(doc, section_intro[[sec]], style = "Normal")
}

pipeline_summary <- data.frame(
  Buoc = c(
    "1-4", "5", "6-8", "9", "10", "11a-11b", "12-14"
  ),
  Mo_ta = c(
    "Import, kiểm tra kỹ thuật, QC hành vi, xử lý dữ liệu thiếu, thống kê mô tả",
    "Đánh giá CMV bằng full collinearity VIF",
    "Đánh giá mô hình đo lường reflective/formative và HOC",
    "Ước lượng mô hình cấu trúc trong mẫu",
    "Đánh giá dự báo bằng PLSpredict",
    "Kiểm định trung gian và điều tiết",
    "Kiểm định độ bền: controlled model, endogeneity, heterogeneity, nonlinear, sensitivity"
  ),
  Tep_tham_chieu = c(
    "R/01_import_validate.R ... R/04_descriptives.R",
    "R/05_cmv_assessment.R",
    "R/06_measurement_reflective.R ... R/08_hoc_assessment.R",
    "R/09_structural_insample.R",
    "R/10_plspredict.R",
    "R/11_mediation.R; R/12_moderation.R",
    "R/13_robustness.R; R/13b_gaussian_copula.R; R/14_robust_nonlinear.R; R/15_sensitivity_com6.R"
  ),
  stringsAsFactors = FALSE
)

doc <- cursor_reach(doc, keyword = section_titles[["A"]])
doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_par(doc, "Bảng PL.1.1. Tổng hợp các bước pipeline", style = "Normal")
doc <- body_add_flextable(doc, make_ft(pipeline_summary))
doc <- body_add_par(doc, "Nguồn: Tổng hợp từ README.md và các script pipeline trong thư mục R.", style = "Normal")

catalog_df <- manifest[, c("csv_path", "inferred_pipeline_step", "n_rows", "n_columns", "included_in_word")]
names(catalog_df) <- c("Tep_CSV", "Buoc_phan_tich", "So_dong", "So_cot", "Dua_vao_Word")

doc <- cursor_reach(doc, keyword = section_titles[["B"]])
doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_par(doc, "Bảng PL.2.1. Danh mục tệp kết quả CSV", style = "Normal")
doc <- body_add_flextable(doc, make_ft(catalog_df, wide = TRUE))
doc <- body_add_par(doc, "Nguồn: Tổng hợp từ toàn bộ tệp CSV trong repository.", style = "Normal")

section_map <- list(
  "C" = c("02_clean", "03_qc", "04_cmv"),
  "D" = c("04_descriptives"),
  "E" = c("05_measurement"),
  "F" = c("06_structural", "07_predict"),
  "G" = c("08_complex", "09_robust", "10_report/com6_sensitivity", "10_report/mediation_classification_saturated.csv")
)

section_counter <- list(C = 1, D = 1, E = 1, F = 1, G = 1)

for (sec in names(section_map)) {
  prefixes <- section_map[[sec]]
  rows_idx <- which(vapply(manifest$csv_path, function(p) {
    pp <- normalize_rel(p)
    any(vapply(prefixes, function(pr) startsWith(pp, pr), logical(1)))
  }, logical(1)))

  include_idx <- rows_idx[manifest$included_in_word[rows_idx] == "yes"]

  if (length(include_idx) == 0) next

  doc <- cursor_reach(doc, keyword = section_titles[[sec]])

  for (i in include_idx) {
    rel <- manifest$csv_path[i]
    df <- csv_data[[rel]]
    if (is.null(df)) next

    # Limit very long tables while preserving traceability
    table_note <- "Ghi chú: Các giá trị được giữ nguyên theo tệp kết quả gốc."
    view_df <- df
    if (nrow(df) > 120) {
      view_df <- head(df, 120)
      table_note <- paste(
        "Ghi chú: Bảng hiển thị 120 dòng đầu để đảm bảo tính đọc được;",
        "toàn bộ dữ liệu gốc được lưu trong tệp CSV tương ứng."
      )
    }

    wide <- ncol(view_df) > 8
    if (wide) {
      doc <- body_end_block_section(doc, block_section(landscape_section))
    }

    cap <- sprintf("Bảng PL.%s.%d. %s", sec, section_counter[[sec]], basename(rel))
    doc <- body_add_par(doc, "", style = "Normal")
    doc <- body_add_par(doc, cap, style = "Normal")
    doc <- body_add_flextable(doc, make_ft(view_df, wide = wide))
    doc <- body_add_par(doc, sprintf("Nguồn: Tổng hợp từ tệp %s.", rel), style = "Normal")
    doc <- body_add_par(doc, table_note, style = "Normal")

    if (wide) {
      doc <- body_end_block_section(doc, block_section(portrait_section))
    }

    section_counter[[sec]] <- section_counter[[sec]] + 1
  }
}

doc <- cursor_reach(doc, keyword = section_titles[["H"]])

qc_summary <- data.frame(
  Kiem_tra = c(
    "Số tệp CSV quét được",
    "Số tệp đưa vào Word",
    "Số tệp chỉ ghi manifest",
    "Nguyên tắc trích xuất",
    "Cảnh báo dữ liệu"
  ),
  Ket_qua = c(
    as.character(nrow(manifest)),
    as.character(sum(manifest$included_in_word == "yes")),
    as.character(sum(manifest$included_in_word == "no")),
        "Chỉ đọc và trình bày lại kết quả đã xuất; không tính toán lại mô hình",
    ifelse(any(grepl("duplicated_columns|Khong doc duoc|Không đọc được|Tep rong|Tệp rỗng", manifest$notes)),
          "Có tệp cần lưu ý, đã ghi trong manifest",
          "Không ghi nhận lỗi cấu trúc nghiêm trọng")
  ),
  stringsAsFactors = FALSE
)

doc <- body_add_par(doc, "Bảng PL.8.1. Nhật ký đối chiếu và kiểm tra nhất quán", style = "Normal")
doc <- body_add_flextable(doc, make_ft(qc_summary))
doc <- body_add_par(doc, "Nguồn: Tổng hợp từ quá trình rà soát tệp CSV và script pipeline.", style = "Normal")

doc <- cursor_reach(doc, keyword = section_titles[["I"]])
excluded <- manifest[manifest$included_in_word == "no", c("csv_path", "reason_if_excluded")]
if (nrow(excluded) == 0) {
  excluded <- data.frame(
    csv_path = "(khong co)",
    reason_if_excluded = "Tất cả tệp CSV hợp lệ đều đã được đưa vào phụ lục",
    stringsAsFactors = FALSE
  )
}
names(excluded) <- c("Tep_CSV", "Ly_do_khong_dua_day_du")
doc <- body_add_par(doc, "Bảng PL.9.1. Các tệp không đưa đầy đủ vào Word", style = "Normal")
doc <- body_add_flextable(doc, make_ft(excluded, wide = TRUE))
doc <- body_add_par(doc, "Nguồn: Tổng hợp từ manifest appendix_csv_manifest.csv.", style = "Normal")

print(doc, target = docx_output)

# Build log
included_paths <- manifest$csv_path[manifest$included_in_word == "yes"]
excluded_paths <- manifest$csv_path[manifest$included_in_word == "no"]

log_lines <- c(
  "# Appendix Build Log",
  "",
  paste0("Build time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## 1) Repository folders inspected",
  "- 00_meta, 01_raw, 02_clean, 03_qc, 04_cmv, 04_descriptives, 05_measurement, 06_structural, 07_predict, 08_complex, 09_robust, 10_report, config, R",
  "",
  "## 2) Pipeline files read",
  "- README.md",
  "- run_pipeline.R",
  "- R/run_core.R",
  "- run_main.R",
  "- run_pilot.R",
  "- config/main.yml",
  "- config/pilot.yml",
  "- R/14_report_export.R",
  "- R/utils_tables.R",
  "- R/04_descriptives.R",
  "- R/05_cmv_assessment.R",
  "",
  "## 3) CSV files included in Word",
  if (length(included_paths) > 0) paste0("- ", included_paths) else "- (none)",
  "",
  "## 4) CSV files excluded and reasons",
  if (length(excluded_paths) > 0) {
    apply(manifest[manifest$included_in_word == "no", c("csv_path", "reason_if_excluded")], 1,
          function(x) paste0("- ", x[[1]], " :: ", x[[2]]))
  } else {
    "- (none)"
  },
  "",
  "## 5) Unresolved issues or assumptions",
  "- Encoding được thử theo nhiều bộ mã; kết quả cuối cùng ghi trong cột notes của manifest.",
  "- Các bảng quá dài/quá rộng có thể được giới hạn dòng để đảm bảo tính đọc được trong Word; tệp gốc vẫn được đối chiếu trong manifest.",
  "",
  "## 6) Non-modification confirmation",
  "- Không chỉnh sửa bất kỳ file code phân tích, file cấu hình, hoặc file CSV kết quả gốc.",
  "- Chỉ tạo mới các file phụ lục: .docx, manifest, build log và script lắp ráp Word."
)

writeLines(log_lines, con = build_log_output, useBytes = TRUE)

cat("Done:\n")
cat("- ", normalizePath(docx_output), "\n", sep = "")
cat("- ", normalizePath(manifest_output), "\n", sep = "")
cat("- ", normalizePath(build_log_output), "\n", sep = "")
