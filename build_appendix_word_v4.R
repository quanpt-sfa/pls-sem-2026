suppressPackageStartupMessages({
  library(officer)
  library(flextable)
})

root_dir <- "."
output_docx <- file.path(root_dir, "Phu_luc_ket_qua_phan_tich_du_lieu_v4.docx")
tmp_docx <- file.path(root_dir, "Phu_luc_ket_qua_phan_tich_du_lieu_v4.__tmp__.docx")
output_manifest <- file.path(root_dir, "appendix_v4_csv_manifest.csv")
output_log <- file.path(root_dir, "appendix_v4_build_log.md")

normalize_rel <- function(path_str) {
  p <- gsub("\\\\", "/", path_str)
  sub("^\\./", "", p)
}

read_csv_robust <- function(path) {
  encodings <- c("UTF-8", "UTF-8-BOM", "windows-1258", "latin1")
  for (enc in encodings) {
    out <- tryCatch({
      df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = enc)
      list(ok = TRUE, df = df, encoding = enc, err = "")
    }, error = function(e) {
      list(ok = FALSE, df = NULL, encoding = enc, err = conditionMessage(e))
    })
    if (out$ok) return(out)
  }
  list(ok = FALSE, df = NULL, encoding = NA_character_, err = "Không đọc được CSV")
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

caption_map <- list(
  "02_clean/missing_data_report.csv" = "Tình trạng dữ liệu thiếu theo biến quan sát",
  "03_qc/qc_summary.csv" = "Tóm tắt kết quả sàng lọc chất lượng phản hồi",
  "03_qc/sample_flow.csv" = "Quy trình hình thành mẫu nghiên cứu",
  "04_cmv/full_collinearity_vif.csv" = "Kết quả đánh giá full collinearity VIF",
  "04_descriptives/sample_demographics.csv" = "Đặc điểm nhân khẩu học của mẫu khảo sát",
  "04_descriptives/descriptive_stats.csv" = "Thống kê mô tả các biến quan sát",
  "04_descriptives/construct_descriptive_stats.csv" = "Thống kê mô tả các cấu trúc nghiên cứu",
  "05_measurement/reflective/outer_loadings.csv" = "Hệ số tải ngoài của các chỉ báo phản xạ",
  "05_measurement/reflective/reliability_table.csv" = "Độ tin cậy và giá trị hội tụ của các cấu trúc phản xạ",
  "05_measurement/reflective/htmt_matrix_full.csv" = "Ma trận HTMT cho giá trị phân biệt",
  "05_measurement/formative/outer_vif.csv" = "Outer VIF của các chỉ báo tạo thành",
  "05_measurement/formative/outer_weights_bootstrap.csv" = "Trọng số ngoài bootstrap của các chỉ báo tạo thành",
  "06_structural/path_coefficients_bootstrap.csv" = "Hệ số đường dẫn và kết quả bootstrap",
  "06_structural/r_squared.csv" = "Hệ số xác định R² của biến nội sinh",
  "06_structural/f_squared.csv" = "Quy mô tác động f²",
  "06_structural/inner_vif.csv" = "Inner VIF của mô hình cấu trúc",
  "07_predict/plspredict_results.csv" = "Kết quả PLSpredict",
  "08_complex/mediation/mediation_classification.csv" = "Phân loại hiệu ứng trung gian",
  "08_complex/moderation/interaction_coefficients.csv" = "Kết quả kiểm định hệ số tương tác",
  "08_complex/moderation/simple_slopes_TC.csv" = "Kết quả hệ số dốc đơn theo mức điều tiết TC",
  "08_complex/moderation/simple_slopes_ETH.csv" = "Kết quả hệ số dốc đơn theo mức điều tiết ETH",
  "09_robust/controlled_model/path_comparison_A_vs_B.csv" = "So sánh hệ số đường dẫn giữa mô hình A và mô hình B",
  "09_robust/endogeneity/copula_decisions.csv" = "Kết luận kiểm định nội sinh bằng Gaussian copula",
  "09_robust/level_heterogeneity/tables/micom_results.csv" = "Kết quả kiểm định bất biến đo lường MICOM",
  "09_robust/level_heterogeneity/tables/mga_results.csv" = "Kết quả phân tích đa nhóm",
  "09_robust/nonlinear/nonlinear_quadratic_results.csv" = "Kết quả kiểm định quan hệ phi tuyến",
  "09_robust/sensitivity_data/com6_sensitivity_paths.csv" = "Kết quả phân tích nhạy cảm sau xử lý COM6",
  "04_descriptives/correlation_matrix.csv" = "Tóm tắt ma trận tương quan (toàn bộ ma trận lưu trong CSV)"
)

include_files <- names(caption_map)

infer_step <- function(p) {
  if (grepl("^02_clean/|^03_qc/|^04_cmv/", p)) return("Tiền xử lý và kiểm soát chất lượng dữ liệu")
  if (grepl("^04_descriptives/", p)) return("Mô tả mẫu và thống kê mô tả")
  if (grepl("^05_measurement/", p)) return("Đánh giá mô hình đo lường")
  if (grepl("^06_structural/|^07_predict/", p)) return("Mô hình cấu trúc và khả năng dự báo")
  if (grepl("^08_complex/|^09_robust/", p)) return("Trung gian, điều tiết và độ bền")
  "Khác"
}

infer_section <- function(p) {
  if (grepl("^02_clean/|^03_qc/|^04_cmv/", p)) return("C")
  if (grepl("^04_descriptives/", p)) return("D")
  if (grepl("^05_measurement/", p)) return("E")
  if (grepl("^06_structural/|^07_predict/", p)) return("F")
  if (grepl("^08_complex/|^09_robust/", p)) return("G")
  "I"
}

detect_id_col <- function(df) {
  x <- sanitize_df(df)
  if (ncol(x) == 0) return(1)
  lc <- tolower(names(x))
  key <- c("variable", "construct", "indicator", "path", "group", "relation", "equation", "term", "model", "file")
  hit <- which(vapply(lc, function(nm) any(vapply(key, function(k) grepl(k, nm), logical(1))), logical(1)))
  if (length(hit) > 0) return(hit[1])
  char_cols <- which(vapply(x, function(c) is.character(c) || is.factor(c), logical(1)))
  if (length(char_cols) > 0) return(char_cols[1])
  1
}

split_by_max_cols <- function(df, max_cols = 8) {
  x <- sanitize_df(df)
  if (ncol(x) <= max_cols) return(list("Phần 1" = x))

  id_col <- detect_id_col(x)
  others <- setdiff(seq_len(ncol(x)), id_col)
  chunk <- max_cols - 1
  if (chunk < 1) chunk <- 1
  groups <- split(others, ceiling(seq_along(others) / chunk))

  out <- list()
  k <- 1
  for (g in groups) {
    take <- c(id_col, g)
    out[[paste0("Phần ", k)]] <- x[, take, drop = FALSE]
    k <- k + 1
  }
  out
}

split_logical_then_narrow <- function(df) {
  x <- sanitize_df(df)
  cols <- names(x)
  lc <- tolower(cols)

  id_col <- detect_id_col(x)
  id_set <- id_col

  est_idx <- which(grepl("beta|estimate|coef|weight|loading|vif|r2|f2|q2|rmse|mae|mean|sd|min|max|t|p|sig", lc))
  ci_idx <- which(grepl("ci|lower|upper|lo|hi|2\\.5|97\\.5", lc))

  est_idx <- setdiff(est_idx, id_set)
  ci_idx <- setdiff(ci_idx, c(id_set, est_idx))
  rem_idx <- setdiff(seq_along(cols), c(id_set, est_idx, ci_idx))

  blocks <- list()
  if (length(est_idx) > 0) blocks[["Khối A: Hệ số và ý nghĩa thống kê"]] <- x[, c(id_set, est_idx), drop = FALSE]
  if (length(ci_idx) > 0) blocks[["Khối B: Khoảng tin cậy"]] <- x[, c(id_set, ci_idx), drop = FALSE]
  if (length(rem_idx) > 0) blocks[["Khối C: Chỉ tiêu bổ sung"]] <- x[, c(id_set, rem_idx), drop = FALSE]
  if (length(blocks) == 0) blocks[["Khối A: Dữ liệu"]] <- x

  narrow <- list()
  for (bn in names(blocks)) {
    sb <- split_by_max_cols(blocks[[bn]], max_cols = 8)
    if (length(sb) == 1) {
      narrow[[bn]] <- sb[[1]]
    } else {
      for (sn in names(sb)) {
        narrow[[paste0(bn, " - ", sn)]] <- sb[[sn]]
      }
    }
  }
  narrow
}

make_correlation_summary <- function(df, rel_path) {
  x <- sanitize_df(df)
  data.frame(
    file_path = rel_path,
    rows = nrow(x),
    columns = ncol(x),
    purpose = "Ma trận tương quan giữa các biến quan sát",
    note = "Toàn bộ ma trận được lưu trong CSV và liệt kê trong manifest",
    stringsAsFactors = FALSE
  )
}

make_ft <- function(df, printable_width = 5.8, compact = FALSE) {
  x <- sanitize_df(df)
  ft <- flextable(x)
  ft <- font(ft, fontname = "Times New Roman", part = "all")
  ft <- fontsize(ft, size = if (compact) 9 else 10, part = "all")
  ft <- bold(ft, part = "header")
  ft <- align(ft, align = "left", part = "header")

  if (ncol(x) > 0) {
    for (j in seq_len(ncol(x))) {
      if (j == detect_id_col(x)) {
        ft <- align(ft, j = j, align = "left", part = "body")
      } else if (is.numeric(x[[j]]) || is.integer(x[[j]])) {
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

all_csv <- sort(list.files(root_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE))
all_rel <- normalize_rel(all_csv)

manifest <- data.frame(
  csv_path = character(),
  inferred_pipeline_step = character(),
  included_in_word_v4 = character(),
  word_table_number = character(),
  handling_method = character(),
  reason_if_excluded_or_summarized = character(),
  n_rows = integer(),
  n_columns = integer(),
  column_names = character(),
  notes = character(),
  stringsAsFactors = FALSE
)

store <- list()

for (i in seq_along(all_csv)) {
  abs_path <- all_csv[i]
  rel <- all_rel[i]
  rd <- read_csv_robust(abs_path)

  if (!rd$ok) {
    manifest <- rbind(manifest, data.frame(
      csv_path = rel,
      inferred_pipeline_step = infer_step(rel),
      included_in_word_v4 = "no",
      word_table_number = "",
      handling_method = "exclude",
      reason_if_excluded_or_summarized = "Không đọc được CSV",
      n_rows = 0,
      n_columns = 0,
      column_names = "",
      notes = rd$err,
      stringsAsFactors = FALSE
    ))
    next
  }

  df <- sanitize_df(rd$df)
  store[[rel]] <- df

  include_v4 <- rel %in% include_files
  handling <- "exclude"
  reason <- "Không thuộc tập bảng báo cáo V4"

  if (include_v4) {
    handling <- "full"
    reason <- ""
  }

  if (rel == "04_descriptives/correlation_matrix.csv") {
    include_v4 <- TRUE
    handling <- "summary"
    reason <- "Không đưa full matrix vào Word theo quy tắc readability"
  }

  if (include_v4 && ncol(df) > 8 && handling != "summary") {
    handling <- "split"
  }

  if (nrow(df) == 0 && include_v4) {
    include_v4 <- FALSE
    handling <- "exclude"
    reason <- "Tệp không có dòng dữ liệu"
  }

  manifest <- rbind(manifest, data.frame(
    csv_path = rel,
    inferred_pipeline_step = infer_step(rel),
    included_in_word_v4 = if (include_v4) "yes" else "no",
    word_table_number = "",
    handling_method = handling,
    reason_if_excluded_or_summarized = reason,
    n_rows = nrow(df),
    n_columns = ncol(df),
    column_names = paste(names(df), collapse = " | "),
    notes = paste("encoding=", rd$encoding),
    stringsAsFactors = FALSE
  ))
}

manifest <- manifest[order(manifest$csv_path), ]
row.names(manifest) <- NULL

margin_cfg <- page_mar(top = 3.5 / 2.54, bottom = 3.0 / 2.54, left = 3.5 / 2.54, right = 2.5 / 2.54)
portrait_section <- prop_section(page_size = page_size(orient = "portrait", width = 8.27, height = 11.69), page_margins = margin_cfg, type = "continuous")

fp_body <- fp_text(font.family = "Times New Roman", font.size = 13)
fp_body_bold <- fp_text(font.family = "Times New Roman", font.size = 13, bold = TRUE)
fp_title <- fp_text(font.family = "Times New Roman", font.size = 16, bold = TRUE)
pp_left <- fp_par(text.align = "left", line_spacing = 1.3)
pp_center <- fp_par(text.align = "center", line_spacing = 1.3)

add_text <- function(doc, text, bold = FALSE, center = FALSE, title = FALSE) {
  fp <- if (title) fp_title else if (bold) fp_body_bold else fp_body
  pp <- if (center) pp_center else pp_left
  body_add_fpar(doc, fpar(ftext(text, prop = fp), fp_p = pp))
}

doc <- read_docx()
doc <- body_end_block_section(doc, block_section(portrait_section))

doc <- add_text(doc, "PHỤ LỤC: KẾT QUẢ CÁC BƯỚC PHÂN TÍCH DỮ LIỆU", bold = TRUE, center = TRUE, title = TRUE)
doc <- add_text(doc, "", center = TRUE)
doc <- add_text(doc, "Luận án tiến sĩ", center = TRUE)
doc <- add_text(doc, "", center = TRUE)
doc <- add_text(doc, "Giới thiệu phụ lục", bold = TRUE)
doc <- add_text(doc, "Bản V4 được dựng lại theo nguyên tắc readability: mọi bảng phải đọc được sau khi xuất PDF, không mất cột định danh, không mất nhãn dòng, không mất dòng nguồn và không tràn lề trang. Các bảng vượt 8 cột được tách hẹp theo khối logic có giữ cột định danh ở đầu. Riêng correlation_matrix chỉ trình bày bảng tóm tắt, toàn bộ ma trận giữ ở CSV/manifest.")

section_titles <- c(
  A = "Phụ lục A. Tổng quan quy trình phân tích dữ liệu",
  B = "Phụ lục B. Danh mục các tệp kết quả CSV được sử dụng",
  C = "Phụ lục C. Kết quả tiền xử lý và kiểm soát chất lượng dữ liệu",
  D = "Phụ lục D. Kết quả mô tả mẫu và thống kê mô tả",
  E = "Phụ lục E. Kết quả đánh giá mô hình đo lường",
  F = "Phụ lục F. Kết quả mô hình cấu trúc và khả năng dự báo",
  G = "Phụ lục G. Kết quả kiểm định trung gian, điều tiết và độ bền",
  H = "Phụ lục H. Nhật ký đối chiếu và kiểm tra nhất quán",
  I = "Phụ lục I. Danh mục tệp không trình bày đầy đủ trong Word"
)

table_counter <- list(A = 1, B = 1, C = 1, D = 1, E = 1, F = 1, G = 1, H = 1, I = 1)
add_caption <- function(doc_obj, sec, ttl) {
  cap <- sprintf("Bảng PL.%s.%d. %s", sec, table_counter[[sec]], ttl)
  doc_obj <- add_text(doc_obj, cap, bold = TRUE)
  num <- sprintf("PL.%s.%d", sec, table_counter[[sec]])
  table_counter[[sec]] <<- table_counter[[sec]] + 1
  list(doc = doc_obj, no = num)
}

doc <- body_add_break(doc, pos = "after")
doc <- add_text(doc, section_titles[["A"]], bold = TRUE)
pipeline_tbl <- data.frame(
  Buoc = c("Tiền xử lý", "Mô tả mẫu", "Đo lường", "Cấu trúc", "Dự báo", "Trung gian/điều tiết", "Độ bền"),
  Noi_dung = c(
    "Kiểm tra dữ liệu, QC, dữ liệu thiếu, CMV",
    "Mô tả mẫu, thống kê mô tả, tương quan (dạng tóm tắt)",
    "Đánh giá thang đo phản xạ và tạo thành",
    "Ước lượng hệ số đường dẫn, R², f², inner VIF",
    "Đánh giá năng lực dự báo PLSpredict",
    "Kiểm định trung gian và điều tiết",
    "Endogeneity, MICOM, MGA, nonlinear, sensitivity"
  ),
  stringsAsFactors = FALSE
)
ca <- add_caption(doc, "A", "Tóm tắt các bước phân tích trong pipeline")
doc <- ca$doc
doc <- body_add_flextable(doc, make_ft(pipeline_tbl, printable_width = 5.8, compact = TRUE))
doc <- add_text(doc, "Nguồn: Tổng hợp từ README và script pipeline hiện hành.")

doc <- body_add_break(doc, pos = "after")
doc <- add_text(doc, section_titles[["B"]], bold = TRUE)
catalog <- manifest[manifest$included_in_word_v4 == "yes", c("csv_path", "inferred_pipeline_step", "handling_method", "n_rows", "n_columns")]
names(catalog) <- c("Tệp CSV", "Bước phân tích", "Cách xử lý", "Số dòng", "Số cột")
cb <- add_caption(doc, "B", "Danh mục bảng được trình bày trong bản V4")
doc <- cb$doc
doc <- body_add_flextable(doc, make_ft(catalog, printable_width = 5.8, compact = TRUE))
doc <- add_text(doc, "Nguồn: Tổng hợp từ manifest V4.")

for (sec in c("C", "D", "E", "F", "G")) {
  doc <- body_add_break(doc, pos = "after")
  doc <- add_text(doc, section_titles[[sec]], bold = TRUE)

  rows_idx <- which(manifest$included_in_word_v4 == "yes" & vapply(manifest$csv_path, infer_section, character(1)) == sec)
  if (length(rows_idx) == 0) next

  for (idx in rows_idx) {
    rel <- manifest$csv_path[idx]
    df <- store[[rel]]
    if (is.null(df)) next

    if (rel == "04_descriptives/correlation_matrix.csv") {
      sum_tbl <- make_correlation_summary(df, rel)
      cap <- add_caption(doc, sec, caption_map[[rel]])
      doc <- cap$doc
      manifest$word_table_number[manifest$csv_path == rel] <- cap$no
      doc <- body_add_flextable(doc, make_ft(sum_tbl, printable_width = 5.8, compact = TRUE))
      doc <- add_text(doc, sprintf("Nguồn: Tổng hợp từ tệp %s.", rel))
      doc <- add_text(doc, "Ghi chú: Ma trận tương quan đầy đủ không đưa trực tiếp vào Word; tệp gốc được giữ nguyên trong CSV và manifest.")
      doc <- add_text(doc, "")
      next
    }

    if (ncol(df) > 8) {
      blocks <- split_logical_then_narrow(df)
      nums <- character()
      first <- TRUE
      for (bn in names(blocks)) {
        bdf <- blocks[[bn]]
        cap <- add_caption(doc, sec, paste0(caption_map[[rel]], " (", bn, ")"))
        doc <- cap$doc
        nums <- c(nums, cap$no)
        doc <- body_add_flextable(doc, make_ft(bdf, printable_width = 5.8, compact = TRUE))
        doc <- add_text(doc, sprintf("Nguồn: Tổng hợp từ tệp %s.", rel))
        if (first) {
          doc <- add_text(doc, "Ghi chú: Bảng gốc vượt 8 cột nên được tách thành các bảng hẹp có giữ cột định danh ở đầu.")
          first <- FALSE
        }
        doc <- add_text(doc, "")
      }
      manifest$word_table_number[manifest$csv_path == rel] <- paste(nums, collapse = "; ")
    } else {
      cap <- add_caption(doc, sec, caption_map[[rel]])
      doc <- cap$doc
      manifest$word_table_number[manifest$csv_path == rel] <- cap$no
      doc <- body_add_flextable(doc, make_ft(df, printable_width = 5.8, compact = FALSE))
      doc <- add_text(doc, sprintf("Nguồn: Tổng hợp từ tệp %s.", rel))
      doc <- add_text(doc, "")
    }
  }
}

doc <- body_add_break(doc, pos = "after")
doc <- add_text(doc, section_titles[["H"]], bold = TRUE)
qc <- data.frame(
  Noi_dung = c("Tổng số CSV trong repo", "Số CSV đưa vào Word V4", "Số CSV không đưa vào Word V4", "Bảng split theo rule >8 cột", "Bảng summary-only"),
  Ket_qua = c(
    as.character(nrow(manifest)),
    as.character(sum(manifest$included_in_word_v4 == "yes")),
    as.character(sum(manifest$included_in_word_v4 == "no")),
    as.character(sum(manifest$handling_method == "split" & manifest$included_in_word_v4 == "yes")),
    as.character(sum(manifest$handling_method == "summary" & manifest$included_in_word_v4 == "yes"))
  ),
  stringsAsFactors = FALSE
)
ch <- add_caption(doc, "H", "Nhật ký kiểm tra nhất quán giữa phụ lục V4 và dữ liệu nguồn")
doc <- ch$doc
doc <- body_add_flextable(doc, make_ft(qc, printable_width = 5.8, compact = TRUE))
doc <- add_text(doc, "Nguồn: Tổng hợp từ appendix_v4_csv_manifest.csv.")

doc <- body_add_break(doc, pos = "after")
doc <- add_text(doc, section_titles[["I"]], bold = TRUE)
excluded <- manifest[manifest$included_in_word_v4 == "no", c("csv_path", "reason_if_excluded_or_summarized", "n_rows", "n_columns")]
if (nrow(excluded) == 0) {
  excluded <- data.frame(csv_path = "(không có)", reason_if_excluded_or_summarized = "-", n_rows = "-", n_columns = "-", stringsAsFactors = FALSE)
}
names(excluded) <- c("Tệp CSV", "Lý do không đưa vào Word V4", "Số dòng", "Số cột")
ci <- add_caption(doc, "I", "Danh mục tệp không trình bày trong bản V4")
doc <- ci$doc
doc <- body_add_flextable(doc, make_ft(excluded, printable_width = 5.8, compact = TRUE))
doc <- add_text(doc, "Nguồn: Tổng hợp từ appendix_v4_csv_manifest.csv.")

if (file.exists(tmp_docx)) unlink(tmp_docx, force = TRUE)
print(doc, target = tmp_docx)
if (file.exists(output_docx)) unlink(output_docx, force = TRUE)
ok <- file.rename(tmp_docx, output_docx)
if (!ok) stop("Không thể ghi đè tệp DOCX V4. Vui lòng đóng Word nếu đang mở tệp đích.")

write.csv(manifest, output_manifest, row.names = FALSE, fileEncoding = "UTF-8")

log_lines <- c(
  "# Appendix V4 Build Log",
  "",
  paste0("Build time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## Quy tắc readability áp dụng",
  "- Mọi bảng trình bày trên khổ đứng (portrait).",
  "- Bảng > 8 cột không đưa trực tiếp; tách theo khối hẹp có giữ cột định danh ở đầu.",
  "- Correlation matrix chỉ trình bày bảng tóm tắt, không đưa full matrix vào Word.",
  "",
  "## Số lượng bảng",
  paste0("- CSV đưa vào Word V4: ", sum(manifest$included_in_word_v4 == "yes")),
  paste0("- CSV split: ", sum(manifest$included_in_word_v4 == "yes" & manifest$handling_method == "split")),
  paste0("- CSV summary-only: ", sum(manifest$included_in_word_v4 == "yes" & manifest$handling_method == "summary")),
  "",
  "## Xác nhận an toàn",
  "- Không chỉnh sửa code phân tích.",
  "- Không chỉnh sửa CSV kết quả gốc.",
  "- Không chạy lại pipeline phân tích thống kê."
)
writeLines(log_lines, con = output_log, useBytes = TRUE)

cat("Done:\n")
cat("- ", normalizePath(output_docx), "\n", sep = "")
cat("- ", normalizePath(output_manifest), "\n", sep = "")
cat("- ", normalizePath(output_log), "\n", sep = "")
