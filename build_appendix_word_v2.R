suppressPackageStartupMessages({
  library(officer)
  library(flextable)
})

root_dir <- "."
input_docx <- file.path(root_dir, "Phu_luc_ket_qua_phan_tich_du_lieu.docx")
output_docx <- file.path(root_dir, "Phu_luc_ket_qua_phan_tich_du_lieu_v2.docx")
tmp_docx <- file.path(root_dir, "Phu_luc_ket_qua_phan_tich_du_lieu_v2.__tmp__.docx")
output_manifest <- file.path(root_dir, "appendix_v2_csv_manifest.csv")
output_log <- file.path(root_dir, "appendix_v2_build_log.md")

normalize_rel <- function(path_str) {
  p <- gsub("\\\\", "/", path_str)
  p <- sub("^\\./", "", p)
  p
}

collapse_names <- function(x) {
  if (length(x) == 0) return("")
  paste(x, collapse = " | ")
}

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

    if (out$ok) return(out)
  }

  list(ok = FALSE, df = NULL, encoding = NA_character_, err = last_err)
}

infer_step <- function(rel_path) {
  p <- normalize_rel(rel_path)
  if (grepl("^02_clean/", p)) return("Tiền xử lý dữ liệu")
  if (grepl("^03_qc/", p)) return("Kiểm soát chất lượng phản hồi")
  if (grepl("^04_cmv/", p)) return("Đánh giá sai lệch phương pháp chung")
  if (grepl("^04_descriptives/", p)) return("Mô tả mẫu và thống kê mô tả")
  if (grepl("^05_measurement/", p)) return("Đánh giá mô hình đo lường")
  if (grepl("^06_structural/", p)) return("Mô hình cấu trúc")
  if (grepl("^07_predict/", p)) return("Khả năng dự báo")
  if (grepl("^08_complex/mediation/", p)) return("Kiểm định trung gian")
  if (grepl("^08_complex/moderation/", p)) return("Kiểm định điều tiết")
  if (grepl("^09_robust/", p)) return("Kiểm định độ bền")
  if (grepl("^10_report/com6_sensitivity/", p)) return("Phân tích nhạy cảm COM6")
  if (grepl("^10_report/", p)) return("Báo cáo tổng hợp")
  "Khác"
}

infer_section <- function(rel_path) {
  p <- normalize_rel(rel_path)
  if (grepl("^(02_clean|03_qc|04_cmv)/", p)) return("C")
  if (grepl("^04_descriptives/", p)) return("D")
  if (grepl("^05_measurement/", p)) return("E")
  if (grepl("^(06_structural|07_predict)/", p)) return("F")
  if (grepl("^(08_complex|09_robust|10_report/com6_sensitivity)/", p) || p == "10_report/mediation_classification_saturated.csv") return("G")
  if (grepl("^10_report/", p)) return("G")
  "I"
}

caption_map <- list(
  "missing_data_report.csv" = "Tình trạng dữ liệu thiếu theo biến quan sát",
  "qc_summary.csv" = "Tóm tắt kết quả sàng lọc chất lượng phản hồi",
  "sample_flow.csv" = "Quy trình hình thành mẫu nghiên cứu",
  "full_collinearity_vif.csv" = "Kết quả đánh giá full collinearity VIF",
  "construct_descriptive_stats.csv" = "Thống kê mô tả các cấu trúc nghiên cứu",
  "demographic_profile.csv" = "Đặc điểm nhân khẩu học của mẫu khảo sát",
  "sample_demographics.csv" = "Mô tả mẫu khảo sát theo nhóm nhân khẩu học",
  "descriptive_stats.csv" = "Thống kê mô tả các biến quan sát",
  "reliability_table.csv" = "Độ tin cậy và giá trị hội tụ của các cấu trúc phản xạ",
  "outer_loadings.csv" = "Hệ số tải ngoài của các chỉ báo phản xạ",
  "outer_vif.csv" = "Outer VIF của các chỉ báo tạo thành",
  "outer_weights_bootstrap.csv" = "Trọng số ngoài bootstrap của các chỉ báo tạo thành",
  "path_coefficients_bootstrap.csv" = "Hệ số đường dẫn và kết quả bootstrap",
  "r_squared.csv" = "Hệ số xác định R² của biến nội sinh",
  "f_squared.csv" = "Quy mô tác động f²",
  "plspredict_results.csv" = "Kết quả PLSpredict",
  "plspredict_vs_lm.csv" = "So sánh sai số dự báo giữa PLS và mô hình tuyến tính",
  "prediction_classification.csv" = "Phân loại mức độ năng lực dự báo",
  "mediation_classification.csv" = "Phân loại hiệu ứng trung gian",
  "specific_indirect_effects.csv" = "Kết quả hiệu ứng gián tiếp cụ thể",
  "interaction_coefficients.csv" = "Kết quả kiểm định hệ số tương tác",
  "simple_slopes_TC.csv" = "Kết quả hệ số dốc đơn cho biến điều tiết TC",
  "simple_slopes_ETH.csv" = "Kết quả hệ số dốc đơn cho biến điều tiết ETH",
  "mga_results.csv" = "Kết quả phân tích đa nhóm",
  "micom_results.csv" = "Kết quả kiểm định bất biến đo lường MICOM",
  "nonlinear_quadratic_results.csv" = "Kết quả kiểm định quan hệ phi tuyến",
  "com6_sensitivity_paths.csv" = "Kết quả phân tích nhạy cảm sau xử lý COM6"
)

caption_for <- function(rel_path) {
  p <- normalize_rel(rel_path)
  b <- basename(p)

  if (p == "10_report/com6_sensitivity/baseline/f_squared.csv") return("Quy mô tác động f² của mô hình baseline trong phân tích COM6")
  if (p == "10_report/com6_sensitivity/no_com6/f_squared.csv") return("Quy mô tác động f² của mô hình loại chỉ báo COM6")
  if (p == "10_report/com6_sensitivity/compare/f_squared_compare.csv") return("So sánh quy mô tác động f² giữa các kịch bản COM6")
  if (p == "10_report/com6_sensitivity/compare/path_coefficients_compare.csv") return("So sánh hệ số đường dẫn giữa các kịch bản COM6")
  if (p == "10_report/com6_sensitivity/compare/r_squared_compare.csv") return("So sánh hệ số xác định R² giữa các kịch bản COM6")

  if (!is.null(caption_map[[b]])) return(caption_map[[b]])

  paste("Kết quả chi tiết thuộc bước", infer_step(p))
}

is_important_result <- function(rel_path) {
  p <- normalize_rel(rel_path)
  keys <- c(
    "descriptive", "demographic", "vif", "loading", "weight", "reliability", "htmt",
    "path", "r_squared", "f_squared", "plspredict", "prediction", "mediation",
    "interaction", "slopes", "micom", "mga", "nonlinear", "copula", "comparison", "diagnostics"
  )
  any(vapply(keys, function(k) grepl(k, p, ignore.case = TRUE), logical(1)))
}

classify_table <- function(rel_path, n_rows, n_cols, read_ok, file_empty) {
  p <- normalize_rel(rel_path)
  b <- basename(p)

  if (!read_ok || file_empty) {
    return(list(category = "4_excluded", included = "no", reason = "Tệp rỗng hoặc không đọc được"))
  }

  if (n_rows == 0) {
    return(list(category = "4_excluded", included = "no", reason = "Tệp không có bản ghi dữ liệu"))
  }

  if (grepl("package_versions\\.csv$|pilot_ids\\.csv$", p, ignore.case = TRUE)) {
    return(list(category = "4_excluded", included = "no", reason = "Tệp metadata phiên bản/ID, không phải bảng kết quả phân tích"))
  }

  if (grepl("(^|/)appendix.*manifest\\.csv$", p, ignore.case = TRUE)) {
    return(list(category = "4_excluded", included = "no", reason = "Tệp manifest phục vụ kiểm kê, không phải kết quả phân tích gốc"))
  }

  if (grepl("qc_flags_detail\\.csv$|correlation_matrix\\.csv$|/groups\\.csv$", p, ignore.case = TRUE)) {
    return(list(category = "3_summary", included = "yes", reason = "Tệp quá dài/rộng ở mức dòng quan sát; chỉ trình bày tóm tắt trong Word"))
  }

  if (n_rows > 300) {
    return(list(category = "3_summary", included = "yes", reason = "Số dòng lớn, ưu tiên tóm tắt để đảm bảo tính đọc được"))
  }

  if (n_cols > 20) {
    return(list(category = "3_summary", included = "yes", reason = "Số cột quá rộng, không phù hợp trình bày đầy đủ trong khổ in"))
  }

  if (n_cols > 12 && is_important_result(p) && n_rows <= 200) {
    return(list(category = "2_split", included = "yes", reason = "Bảng quan trọng nhưng rộng; tách thành các khối cột logic"))
  }

  if (n_rows <= 60 && n_cols <= 12) {
    return(list(category = "1_full", included = "yes", reason = ""))
  }

  if (is_important_result(p) && n_rows <= 140 && n_cols <= 12) {
    return(list(category = "1_full", included = "yes", reason = ""))
  }

  if (n_rows > 140 || n_cols > 12) {
    return(list(category = "3_summary", included = "yes", reason = "Bảng dài/rộng; trình bày tóm tắt và truy vết qua manifest"))
  }

  list(category = "1_full", included = "yes", reason = "")
}

sanitize_df <- function(df) {
  out <- df
  nm <- names(out)
  if (length(nm) > 0) {
    nm[nm == "" | is.na(nm)] <- paste0("col_", which(nm == "" | is.na(nm)))
    names(out) <- make.unique(nm, sep = "_")
  }
  out
}

split_wide_table <- function(df) {
  x <- sanitize_df(df)
  cols <- names(x)
  lc <- tolower(cols)

  id_idx <- which(grepl("construct|indicator|path|effect|interaction|term|group|model|variable|predictor|outcome|hypothesis|from|to|criterion|decision|class", lc))
  est_idx <- which(grepl("estimate|beta|coef|weight|loading|vif|r2|f2|q2|rmse|mae|mean|sd|min|max|t_stat|p_value|p\\.|sig|signif", lc))
  ci_idx <- which(grepl("ci|lower|upper|ll|ul|2\\.5|97\\.5|percentile", lc))

  id_idx <- unique(id_idx)
  est_idx <- setdiff(unique(est_idx), id_idx)
  ci_idx <- setdiff(unique(ci_idx), c(id_idx, est_idx))
  other_idx <- setdiff(seq_along(cols), c(id_idx, est_idx, ci_idx))

  blocks <- list()

  if (length(est_idx) > 0) {
    idx <- unique(c(id_idx, est_idx))
    blocks[["Phần 1: Chỉ tiêu ước lượng và ý nghĩa thống kê"]] <- x[, idx, drop = FALSE]
  }

  if (length(ci_idx) > 0) {
    idx <- unique(c(id_idx, ci_idx))
    blocks[["Phần 2: Khoảng tin cậy bootstrap"]] <- x[, idx, drop = FALSE]
  }

  if (length(other_idx) > 0) {
    idx <- unique(c(id_idx, other_idx))
    blocks[["Phần 3: Chỉ tiêu bổ sung"]] <- x[, idx, drop = FALSE]
  }

  if (length(blocks) == 0) {
    half <- ceiling(ncol(x) / 2)
    blocks[["Phần 1: Nhóm cột 1"]] <- x[, seq_len(half), drop = FALSE]
    if (half < ncol(x)) {
      blocks[["Phần 2: Nhóm cột 2"]] <- x[, (half + 1):ncol(x), drop = FALSE]
    }
  }

  blocks
}

quick_numeric_note <- function(df) {
  x <- sanitize_df(df)
  num_cols <- names(x)[vapply(x, is.numeric, logical(1))]
  if (length(num_cols) == 0) return("")

  mins <- vapply(num_cols, function(cn) suppressWarnings(min(x[[cn]], na.rm = TRUE)), numeric(1))
  maxs <- vapply(num_cols, function(cn) suppressWarnings(max(x[[cn]], na.rm = TRUE)), numeric(1))
  ok <- is.finite(mins) & is.finite(maxs)
  if (!any(ok)) return("")

  sel <- num_cols[ok][1:min(3, sum(ok))]
  pieces <- vapply(sel, function(cn) {
    mn <- suppressWarnings(min(x[[cn]], na.rm = TRUE))
    mx <- suppressWarnings(max(x[[cn]], na.rm = TRUE))
    sprintf("%s: [%.3f; %.3f]", cn, mn, mx)
  }, character(1))

  paste("Khoảng giá trị tham chiếu:", paste(pieces, collapse = "; "))
}

make_ft <- function(df, printable_width = 5.8, force_compact = FALSE) {
  x <- sanitize_df(df)
  font_size <- if (force_compact || ncol(x) > 10) 9 else 10

  ft <- flextable(x)
  ft <- font(ft, fontname = "Times New Roman", part = "all")
  ft <- fontsize(ft, size = font_size, part = "all")
  ft <- bold(ft, part = "header")
  ft <- align(ft, align = "left", part = "header")

  if (ncol(x) > 0) {
    for (j in seq_len(ncol(x))) {
      if (is.numeric(x[[j]]) || is.integer(x[[j]])) {
        ft <- align(ft, j = j, align = "right", part = "body")
      } else {
        ft <- align(ft, j = j, align = "left", part = "body")
      }
    }
  }

  ft <- border_inner(ft, border = fp_border(color = "#666666", width = 0.5))
  ft <- border_outer(ft, border = fp_border(color = "#000000", width = 1))
  ft <- set_table_properties(ft, layout = "autofit", opts_word = list(repeat_headers = TRUE))
  ft <- autofit(ft)
  ft <- fit_to_width(ft, max_width = printable_width)
  ft
}

# Page layout
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

fp_body <- fp_text(font.family = "Times New Roman", font.size = 13)
fp_body_bold <- fp_text(font.family = "Times New Roman", font.size = 13, bold = TRUE)
fp_title <- fp_text(font.family = "Times New Roman", font.size = 16, bold = TRUE)
pp_left <- fp_par(text.align = "left", line_spacing = 1.3)
pp_center <- fp_par(text.align = "center", line_spacing = 1.3)

add_text <- function(doc, text, bold = FALSE, center = FALSE, is_title = FALSE) {
  fmt <- if (is_title) fp_title else if (bold) fp_body_bold else fp_body
  pp <- if (center) pp_center else pp_left
  body_add_fpar(doc, fpar(ftext(text, prop = fmt), fp_p = pp))
}

csv_files <- sort(list.files(root_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE))

manifest <- data.frame(
  csv_path = character(),
  inferred_pipeline_step = character(),
  table_category = character(),
  included_in_word = character(),
  word_table_number = character(),
  reason_if_summarized_or_excluded = character(),
  n_rows = integer(),
  n_columns = integer(),
  column_names = character(),
  notes = character(),
  stringsAsFactors = FALSE
)

csv_store <- list()
split_list <- character()
summary_list <- character()
exclude_list <- character()
full_list <- character()

for (abs_path in csv_files) {
  rel <- normalize_rel(abs_path)
  fi <- file.info(abs_path)
  file_empty <- is.na(fi$size) || fi$size == 0

  if (file_empty) {
    cl <- classify_table(rel, 0, 0, FALSE, TRUE)
    manifest <- rbind(manifest, data.frame(
      csv_path = rel,
      inferred_pipeline_step = infer_step(rel),
      table_category = cl$category,
      included_in_word = cl$included,
      word_table_number = "",
      reason_if_summarized_or_excluded = cl$reason,
      n_rows = 0,
      n_columns = 0,
      column_names = "",
      notes = "Kích thước tệp = 0",
      stringsAsFactors = FALSE
    ))
    exclude_list <- c(exclude_list, rel)
    next
  }

  rd <- read_csv_robust(abs_path)

  if (!rd$ok) {
    cl <- classify_table(rel, 0, 0, FALSE, FALSE)
    manifest <- rbind(manifest, data.frame(
      csv_path = rel,
      inferred_pipeline_step = infer_step(rel),
      table_category = cl$category,
      included_in_word = cl$included,
      word_table_number = "",
      reason_if_summarized_or_excluded = cl$reason,
      n_rows = 0,
      n_columns = 0,
      column_names = "",
      notes = paste("Read error:", rd$err),
      stringsAsFactors = FALSE
    ))
    exclude_list <- c(exclude_list, rel)
    next
  }

  df <- sanitize_df(rd$df)
  n_rows <- nrow(df)
  n_cols <- ncol(df)

  cl <- classify_table(rel, n_rows, n_cols, TRUE, FALSE)

  dup_cols <- names(df)[duplicated(names(df))]
  notes <- c(paste("encoding=", rd$encoding))
  if (length(dup_cols) > 0) notes <- c(notes, paste("duplicated_columns=", paste(unique(dup_cols), collapse = ";")))

  manifest <- rbind(manifest, data.frame(
    csv_path = rel,
    inferred_pipeline_step = infer_step(rel),
    table_category = cl$category,
    included_in_word = cl$included,
    word_table_number = "",
    reason_if_summarized_or_excluded = cl$reason,
    n_rows = n_rows,
    n_columns = n_cols,
    column_names = collapse_names(names(df)),
    notes = collapse_names(notes),
    stringsAsFactors = FALSE
  ))

  csv_store[[rel]] <- df

  if (cl$category == "1_full") full_list <- c(full_list, rel)
  if (cl$category == "2_split") split_list <- c(split_list, rel)
  if (cl$category == "3_summary") summary_list <- c(summary_list, rel)
  if (cl$category == "4_excluded") exclude_list <- c(exclude_list, rel)
}

manifest <- manifest[order(manifest$csv_path), ]
row.names(manifest) <- NULL

# Build DOCX V2
doc <- read_docx()
current_orient <- "portrait"

switch_orientation <- function(doc_obj, target_orient) {
  if (target_orient == current_orient) return(doc_obj)
  if (target_orient == "landscape") {
    doc_obj <- body_end_block_section(doc_obj, block_section(landscape_section))
  } else {
    doc_obj <- body_end_block_section(doc_obj, block_section(portrait_section))
  }
  current_orient <<- target_orient
  doc_obj
}

# Title page
doc <- add_text(doc, "PHỤ LỤC: KẾT QUẢ CÁC BƯỚC PHÂN TÍCH DỮ LIỆU", bold = TRUE, center = TRUE, is_title = TRUE)
doc <- add_text(doc, "", center = TRUE)
doc <- add_text(doc, "Luận án tiến sĩ", center = TRUE)
doc <- add_text(doc, "", center = TRUE)

doc <- add_text(doc, "Giới thiệu phụ lục", bold = TRUE)
doc <- add_text(
  doc,
  "Phụ lục này tổng hợp các kết quả được xuất ra từ các tệp CSV của quy trình phân tích dữ liệu. Việc trình bày trong phụ lục nhằm hỗ trợ kiểm tra, đối chiếu và tái lập kết quả, không nhằm thay thế phần diễn giải chính trong Chương 4. Trong quá trình xây dựng phụ lục, các tệp mã phân tích và tệp kết quả gốc chỉ được đọc để xác định trình tự phân tích, ý nghĩa bảng và nguồn dữ liệu; không có mô hình thống kê nào được ước lượng lại và không có tệp kết quả gốc nào được chỉnh sửa. Các tệp quá rộng hoặc ở mức dòng quan sát được trình bày tóm tắt trong Word và được liệt kê đầy đủ trong manifest đi kèm.",
  bold = FALSE
)

section_titles <- c(
  "A" = "Phụ lục A. Tổng quan quy trình phân tích dữ liệu",
  "B" = "Phụ lục B. Danh mục các tệp kết quả CSV được sử dụng",
  "C" = "Phụ lục C. Kết quả tiền xử lý và kiểm soát chất lượng dữ liệu",
  "D" = "Phụ lục D. Kết quả mô tả mẫu và thống kê mô tả",
  "E" = "Phụ lục E. Kết quả đánh giá mô hình đo lường",
  "F" = "Phụ lục F. Kết quả mô hình cấu trúc và khả năng dự báo",
  "G" = "Phụ lục G. Kết quả kiểm định trung gian, điều tiết và độ bền",
  "H" = "Phụ lục H. Nhật ký đối chiếu và kiểm tra nhất quán",
  "I" = "Phụ lục I. Danh mục tệp không trình bày đầy đủ trong Word"
)

section_intro <- c(
  "A" = "Mục này mô tả trình tự các bước xử lý và phân tích dữ liệu theo pipeline đã được cấu hình trong repository.",
  "B" = "Mục này liệt kê các tệp CSV kết quả được dùng để lập phụ lục, kèm thông tin phân loại bảng theo mức độ trình bày.",
  "C" = "Mục này tổng hợp kết quả tiền xử lý, kiểm soát dữ liệu thiếu, chất lượng phản hồi và đánh giá CMV.",
  "D" = "Mục này trình bày các thống kê mô tả và đặc điểm mẫu phục vụ bối cảnh phân tích.",
  "E" = "Mục này trình bày các kết quả đánh giá thang đo phản xạ và tạo thành.",
  "F" = "Mục này trình bày kết quả mô hình cấu trúc và đánh giá năng lực dự báo.",
  "G" = "Mục này trình bày các kết quả trung gian, điều tiết và các kiểm định độ bền.",
  "H" = "Mục này ghi nhận kiểm tra nhất quán giữa các tệp CSV và các bảng trong tài liệu.",
  "I" = "Mục này liệt kê các tệp không trình bày đầy đủ trong Word để đảm bảo tính đọc được của phụ lục in."
)

tbl_counter <- list(A = 1, B = 1, C = 1, D = 1, E = 1, F = 1, G = 1, H = 1, I = 1)

add_caption <- function(doc_obj, sec, title_text) {
  cap <- sprintf("Bảng PL.%s.%d. %s", sec, tbl_counter[[sec]], title_text)
  doc_obj <- add_text(doc_obj, cap, bold = TRUE)
  tbl_no <- sprintf("PL.%s.%d", sec, tbl_counter[[sec]])
  tbl_counter[[sec]] <<- tbl_counter[[sec]] + 1
  list(doc = doc_obj, table_no = tbl_no)
}

add_source <- function(doc_obj, rel_path) {
  add_text(doc_obj, sprintf("Nguồn: Tổng hợp từ tệp %s.", normalize_rel(rel_path)))
}

add_optional_note <- function(doc_obj, note_text) {
  if (nzchar(note_text)) {
    add_text(doc_obj, paste("Ghi chú:", note_text))
  } else {
    doc_obj
  }
}

# Section A
doc <- body_add_break(doc, pos = "after")
doc <- add_text(doc, section_titles[["A"]], bold = TRUE)
doc <- add_text(doc, section_intro[["A"]])

pipeline_tbl <- data.frame(
  Giai_doan = c("Chuẩn bị dữ liệu", "CMV và đo lường", "Mô hình cấu trúc", "Dự báo", "Trung gian/điều tiết", "Độ bền và nhạy cảm", "Xuất báo cáo"),
  Noi_dung = c(
    "Import, kiểm tra kỹ thuật, QC hành vi, xử lý dữ liệu thiếu, thống kê mô tả",
    "Đánh giá CMV, mô hình đo lường phản xạ/tạo thành, HOC",
    "Ước lượng hệ số đường dẫn, R², f²",
    "PLSpredict và so sánh benchmark",
    "Kiểm định hiệu ứng trung gian và điều tiết",
    "Controlled model, endogeneity, heterogeneity, nonlinear, sensitivity",
    "Tổng hợp bảng/hình và đối chiếu kết quả"
  ),
  Tep_tham_chieu = c(
    "R/01_import_validate.R ... R/04_descriptives.R",
    "R/05_cmv_assessment.R ... R/08_hoc_assessment.R",
    "R/09_structural_insample.R",
    "R/10_plspredict.R",
    "R/11_mediation.R; R/12_moderation.R",
    "R/13_robustness.R; R/13b_gaussian_copula.R; R/14_robust_nonlinear.R; R/15_sensitivity_com6.R",
    "R/14_report_export.R"
  ),
  stringsAsFactors = FALSE
)

cap_a <- add_caption(doc, "A", "Tổng quan trình tự các bước phân tích dữ liệu")
doc <- cap_a$doc
doc <- switch_orientation(doc, "landscape")
doc <- body_add_flextable(doc, make_ft(pipeline_tbl, printable_width = 9.2, force_compact = TRUE))
doc <- switch_orientation(doc, "portrait")
doc <- add_text(doc, "Nguồn: Tổng hợp từ README.md và các script pipeline trong thư mục R.")

# Section B
doc <- body_add_break(doc, pos = "after")
doc <- add_text(doc, section_titles[["B"]], bold = TRUE)
doc <- add_text(doc, section_intro[["B"]])

catalog <- manifest[, c("csv_path", "inferred_pipeline_step", "table_category", "included_in_word", "n_rows", "n_columns")]
names(catalog) <- c("Tệp CSV", "Bước phân tích", "Nhóm trình bày", "Có trong Word", "Số dòng", "Số cột")

cap_b <- add_caption(doc, "B", "Danh mục tệp CSV và phân loại trình bày trong phụ lục")
doc <- cap_b$doc
doc <- switch_orientation(doc, "landscape")
doc <- body_add_flextable(doc, make_ft(catalog, printable_width = 9.2, force_compact = TRUE))
doc <- switch_orientation(doc, "portrait")
doc <- add_text(doc, "Nguồn: Tổng hợp từ toàn bộ tệp CSV trong repository.")

# Sections C-G
for (sec in c("C", "D", "E", "F", "G")) {
  doc <- body_add_break(doc, pos = "after")
  doc <- add_text(doc, section_titles[[sec]], bold = TRUE)
  doc <- add_text(doc, section_intro[[sec]])

  section_vec <- vapply(manifest$csv_path, infer_section, character(1))
  sub_manifest <- manifest[section_vec == sec, ]
  sub_manifest <- sub_manifest[order(sub_manifest$csv_path), ]

  for (i in seq_len(nrow(sub_manifest))) {
    rel <- sub_manifest$csv_path[i]
    cat_type <- sub_manifest$table_category[i]

    if (cat_type == "4_excluded") {
      next
    }

    df <- csv_store[[rel]]
    if (is.null(df)) {
      next
    }

    if (cat_type == "1_full") {
      need_land <- ncol(df) > 8
      doc <- switch_orientation(doc, if (need_land) "landscape" else "portrait")
      cap <- add_caption(doc, sec, caption_for(rel))
      doc <- cap$doc
      manifest$word_table_number[manifest$csv_path == rel] <- cap$table_no
      doc <- body_add_flextable(doc, make_ft(df, printable_width = if (need_land) 9.2 else 5.8, force_compact = need_land))
      doc <- add_source(doc, rel)
      doc <- add_text(doc, "")
    }

    if (cat_type == "2_split") {
      blocks <- split_wide_table(df)
      tbl_nums <- character()
      part_id <- 1
      for (nm in names(blocks)) {
        bdf <- blocks[[nm]]
        need_land <- ncol(bdf) > 8
        doc <- switch_orientation(doc, if (need_land) "landscape" else "portrait")
        cap <- add_caption(doc, sec, paste0(caption_for(rel), " (", nm, ")"))
        doc <- cap$doc
        tbl_nums <- c(tbl_nums, cap$table_no)
        doc <- body_add_flextable(doc, make_ft(bdf, printable_width = if (need_land) 9.2 else 5.8, force_compact = TRUE))
        doc <- add_source(doc, rel)
        if (part_id == 1) {
          doc <- add_optional_note(doc, "Bảng gốc được tách thành các phần để đảm bảo khả năng đọc trong khổ in luận án.")
        }
        doc <- add_text(doc, "")
        part_id <- part_id + 1
      }
      manifest$word_table_number[manifest$csv_path == rel] <- paste(tbl_nums, collapse = "; ")
    }

    if (cat_type == "3_summary") {
      doc <- switch_orientation(doc, "portrait")

      summary_tbl <- data.frame(
        Tep_csv = rel,
        So_dong = nrow(df),
        So_cot = ncol(df),
        Muc_dich = infer_step(rel),
        stringsAsFactors = FALSE
      )

      cap <- add_caption(doc, sec, paste0("Tóm tắt tệp ", caption_for(rel)))
      doc <- cap$doc
      manifest$word_table_number[manifest$csv_path == rel] <- cap$table_no

      doc <- body_add_flextable(doc, make_ft(summary_tbl, printable_width = 5.8))
      doc <- add_source(doc, rel)

      n_note <- quick_numeric_note(df)
      if (nzchar(n_note)) {
        doc <- add_optional_note(doc, n_note)
      }
      doc <- add_optional_note(doc, "Bảng chi tiết đầy đủ được lưu trong tệp CSV gốc và được liệt kê trong manifest.")
      doc <- add_text(doc, "")
    }
  }
}

doc <- switch_orientation(doc, "portrait")

# Section H
doc <- body_add_break(doc, pos = "after")
doc <- add_text(doc, section_titles[["H"]], bold = TRUE)
doc <- add_text(doc, section_intro[["H"]])

qc_tbl <- data.frame(
  Noi_dung_kiem_tra = c(
    "Tổng số tệp CSV được rà soát",
    "Số tệp trình bày đầy đủ (Category 1)",
    "Số tệp tách bảng (Category 2)",
    "Số tệp chỉ tóm tắt (Category 3)",
    "Số tệp không trình bày trong Word (Category 4)",
    "Nguyên tắc đối chiếu"
  ),
  Ket_qua = c(
    as.character(nrow(manifest)),
    as.character(sum(manifest$table_category == "1_full")),
    as.character(sum(manifest$table_category == "2_split")),
    as.character(sum(manifest$table_category == "3_summary")),
    as.character(sum(manifest$table_category == "4_excluded")),
    "Đối chiếu một-một giữa bảng Word, đường dẫn CSV nguồn và manifest"
  ),
  stringsAsFactors = FALSE
)

cap_h <- add_caption(doc, "H", "Nhật ký kiểm tra nhất quán giữa bảng Word và tệp CSV nguồn")
doc <- cap_h$doc
doc <- body_add_flextable(doc, make_ft(qc_tbl, printable_width = 5.8))
doc <- add_text(doc, "Nguồn: Tổng hợp từ quá trình lập phụ lục và manifest đính kèm.")

# Section I
doc <- body_add_break(doc, pos = "after")
doc <- add_text(doc, section_titles[["I"]], bold = TRUE)
doc <- add_text(doc, section_intro[["I"]])

excluded_df <- manifest[manifest$table_category == "4_excluded" | manifest$table_category == "3_summary",
                        c("csv_path", "table_category", "reason_if_summarized_or_excluded", "n_rows", "n_columns")]
if (nrow(excluded_df) == 0) {
  excluded_df <- data.frame(
    csv_path = "(không có)",
    table_category = "-",
    reason_if_summarized_or_excluded = "Tất cả tệp đều đã trình bày đầy đủ",
    n_rows = "-",
    n_columns = "-",
    stringsAsFactors = FALSE
  )
}
names(excluded_df) <- c("Tệp CSV", "Nhóm", "Lý do", "Số dòng", "Số cột")

cap_i <- add_caption(doc, "I", "Danh mục tệp không trình bày đầy đủ trong Word")
doc <- cap_i$doc
doc <- body_add_flextable(doc, make_ft(excluded_df, printable_width = 5.8, force_compact = TRUE))
doc <- add_text(doc, "Nguồn: Tổng hợp từ appendix_v2_csv_manifest.csv.")

# Save artifacts
if (file.exists(tmp_docx)) {
  unlink(tmp_docx, force = TRUE)
}

print(doc, target = tmp_docx)

if (file.exists(output_docx)) {
  unlink(output_docx, force = TRUE)
}

ok_move <- file.rename(tmp_docx, output_docx)
if (!ok_move) {
  stop("Không thể ghi đè tệp DOCX đích; vui lòng đóng Word nếu đang mở tài liệu.")
}

# Ensure all CSV files in repository are listed, including the V2 manifest file itself
if (!(normalize_rel(output_manifest) %in% manifest$csv_path)) {
  self_row <- data.frame(
    csv_path = normalize_rel(output_manifest),
    inferred_pipeline_step = "Khác",
    table_category = "4_excluded",
    included_in_word = "no",
    word_table_number = "",
    reason_if_summarized_or_excluded = "Tệp manifest tự sinh của phụ lục V2",
    n_rows = nrow(manifest),
    n_columns = ncol(manifest),
    column_names = collapse_names(names(manifest)),
    notes = "Tệp kiểm kê do script tạo ra",
    stringsAsFactors = FALSE
  )
  manifest <- rbind(manifest, self_row)
  manifest <- manifest[order(manifest$csv_path), ]
  row.names(manifest) <- NULL
}

write.csv(manifest, output_manifest, row.names = FALSE, fileEncoding = "UTF-8")

build_lines <- c(
  "# Appendix V2 Build Log",
  "",
  paste0("Build time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## 1) Files inspected",
  "- Phu_luc_ket_qua_phan_tich_du_lieu.docx",
  "- README.md",
  "- run_pipeline.R",
  "- R/run_core.R",
  "- run_main.R",
  "- run_pilot.R",
  "- config/main.yml",
  "- config/pilot.yml",
  "- R/14_report_export.R",
  "- R/utils_tables.R",
  "- Toàn bộ tệp CSV trong repository",
  "",
  "## 2) CSV files included fully (Category 1)",
  if (length(full_list) > 0) paste0("- ", sort(unique(full_list))) else "- (none)",
  "",
  "## 3) CSV files split into multiple Word tables (Category 2)",
  if (length(split_list) > 0) paste0("- ", sort(unique(split_list))) else "- (none)",
  "",
  "## 4) CSV files summarized only (Category 3)",
  if (length(summary_list) > 0) paste0("- ", sort(unique(summary_list))) else "- (none)",
  "",
  "## 5) CSV files excluded from Word (Category 4)",
  if (length(exclude_list) > 0) paste0("- ", sort(unique(exclude_list))) else "- (none)",
  "",
  "## 6) Formatting checks performed",
  "- Áp dụng khổ A4, lề trên 3.5 cm, dưới 3.0 cm, trái 3.5 cm, phải 2.5 cm.",
  "- Chuẩn hóa font Times New Roman; cỡ chữ nội dung 13pt; bảng 10pt/9pt.",
  "- Chuẩn hóa đánh số bảng theo hệ PL.A.1 ... PL.I.n.",
  "- Bảng rộng được tách khối cột logic hoặc chuyển sang tóm tắt.",
  "- Chỉ dùng landscape khi cần thiết cho độ rộng bảng.",
  "- Dòng nguồn dưới mỗi bảng luôn ghi đường dẫn CSV tương đối.",
  "",
  "## 7) Non-modification confirmation",
  "- Không chỉnh sửa bất kỳ script phân tích hoặc tệp CSV kết quả gốc.",
  "- Không chạy lại pipeline phân tích thống kê.",
  "- Chỉ tạo mới file DOCX V2, manifest V2, build log V2 và script lắp ráp.",
  "",
  "## 8) Unresolved assumptions",
  "- Việc kiểm tra tràn lề được kiểm soát bằng quy tắc phân loại full/split/summary và fit-to-width; các bảng quá rộng không được đưa nguyên khối vào Word.",
  "- Các tệp ở mức dòng quan sát được ưu tiên tóm tắt để phù hợp tài liệu in."
)

writeLines(build_lines, con = output_log, useBytes = TRUE)

cat("Done:\n")
cat("- ", normalizePath(output_docx), "\n", sep = "")
cat("- ", normalizePath(output_manifest), "\n", sep = "")
cat("- ", normalizePath(output_log), "\n", sep = "")
