suppressPackageStartupMessages({
  library(officer)
  library(flextable)
})

root_dir <- "."
output_docx <- file.path(root_dir, "Phu_luc_ket_qua_phan_tich_du_lieu_v3.docx")
tmp_docx <- file.path(root_dir, "Phu_luc_ket_qua_phan_tich_du_lieu_v3.__tmp__.docx")
output_manifest <- file.path(root_dir, "appendix_v3_csv_manifest.csv")
output_log <- file.path(root_dir, "appendix_v3_build_log.md")

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
  "04_descriptives/correlation_matrix.csv" = "Ma trận tương quan giữa các biến quan sát",
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
  "09_robust/sensitivity_data/com6_sensitivity_paths.csv" = "Kết quả phân tích nhạy cảm sau xử lý COM6"
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

split_wide_table <- function(df) {
  x <- sanitize_df(df)
  cols <- names(x)
  lc <- tolower(cols)

  id_idx <- which(grepl("construct|indicator|path|effect|interaction|term|group|model|variable|equation|relation|from|to", lc))
  est_idx <- which(grepl("beta|estimate|coef|weight|loading|vif|r2|f2|q2|rmse|mae|mean|sd|min|max|t|p|sig", lc))
  ci_idx <- which(grepl("ci|lower|upper|lo|hi|2\\.5|97\\.5", lc))

  id_idx <- unique(id_idx)
  est_idx <- setdiff(unique(est_idx), id_idx)
  ci_idx <- setdiff(unique(ci_idx), c(id_idx, est_idx))
  other_idx <- setdiff(seq_along(cols), c(id_idx, est_idx, ci_idx))

  blocks <- list()
  if (length(est_idx) > 0) blocks[["Phần 1: Hệ số và ý nghĩa thống kê"]] <- x[, unique(c(id_idx, est_idx)), drop = FALSE]
  if (length(ci_idx) > 0) blocks[["Phần 2: Khoảng tin cậy"]] <- x[, unique(c(id_idx, ci_idx)), drop = FALSE]
  if (length(other_idx) > 0) blocks[["Phần 3: Chỉ tiêu bổ sung"]] <- x[, unique(c(id_idx, other_idx)), drop = FALSE]

  if (length(blocks) == 0) {
    half <- ceiling(ncol(x) / 2)
    blocks[["Phần 1: Nhóm cột 1"]] <- x[, seq_len(half), drop = FALSE]
    if (half < ncol(x)) blocks[["Phần 2: Nhóm cột 2"]] <- x[, (half + 1):ncol(x), drop = FALSE]
  }

  # Force portrait-friendly blocks: at most 8 columns per table.
  split_portrait_blocks <- function(d, max_cols = 8) {
    d <- sanitize_df(d)
    if (ncol(d) <= max_cols) {
      return(list("Phần 1" = d))
    }

    anchor <- 1
    others <- if (ncol(d) > 1) 2:ncol(d) else integer(0)
    per_block <- max_cols - 1
    res <- list()
    part <- 1

    if (length(others) == 0) {
      res[[paste0("Phần ", part)]] <- d
      return(res)
    }

    idx <- split(others, ceiling(seq_along(others) / per_block))
    for (g in idx) {
      take <- unique(c(anchor, g))
      res[[paste0("Phần ", part)]] <- d[, take, drop = FALSE]
      part <- part + 1
    }
    res
  }

  final_blocks <- list()
  for (nm in names(blocks)) {
    small <- split_portrait_blocks(blocks[[nm]], max_cols = 8)
    if (length(small) == 1) {
      final_blocks[[nm]] <- small[[1]]
    } else {
      k <- 1
      for (sn in names(small)) {
        final_blocks[[paste0(nm, " - ", sn)]] <- small[[sn]]
        k <- k + 1
      }
    }
  }

  final_blocks
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

# Read all CSV for manifest, but only curated set will be included in Word V3
all_csv <- sort(list.files(root_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE))
all_rel <- normalize_rel(all_csv)

manifest <- data.frame(
  csv_path = character(),
  inferred_pipeline_step = character(),
  included_in_word_v3 = character(),
  word_table_number = character(),
  reason_if_excluded = character(),
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
      included_in_word_v3 = "no",
      word_table_number = "",
      reason_if_excluded = "Không đọc được CSV",
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

  include_v3 <- rel %in% include_files ||
    grepl("^04_descriptives/(sample_demographics|descriptive_stats|construct_descriptive_stats|correlation_matrix)\\.csv$", rel)
  reason <- ""
  if (!include_v3) {
    reason <- "Không thuộc tập bảng tổng hợp cốt lõi của bản V3"
  }

  if (nrow(df) == 0 && include_v3) {
    include_v3 <- FALSE
    reason <- "Tệp không có dòng dữ liệu"
  }

  manifest <- rbind(manifest, data.frame(
    csv_path = rel,
    inferred_pipeline_step = infer_step(rel),
    included_in_word_v3 = if (include_v3) "yes" else "no",
    word_table_number = "",
    reason_if_excluded = reason,
    n_rows = nrow(df),
    n_columns = ncol(df),
    column_names = paste(names(df), collapse = " | "),
    notes = paste("encoding=", rd$encoding),
    stringsAsFactors = FALSE
  ))
}

manifest <- manifest[order(manifest$csv_path), ]
row.names(manifest) <- NULL

# Document layout
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
doc <- add_text(doc, "Bản V3 chỉ giữ các bảng kết quả tổng hợp theo từng bước phân tích. Các tệp kỹ thuật, metadata, bảng mức dòng quan sát và bảng trung gian không cần thiết cho báo cáo luận án được loại khỏi phần trình bày Word, nhưng vẫn được liệt kê trong manifest để bảo đảm khả năng truy vết.")

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

# Section A
doc <- body_add_break(doc, pos = "after")
doc <- add_text(doc, section_titles[["A"]], bold = TRUE)
pipeline_tbl <- data.frame(
  Bước = c("Tiền xử lý", "Mô tả mẫu", "Đo lường", "Cấu trúc", "Dự báo", "Trung gian/điều tiết", "Độ bền"),
  Nội_dung = c(
    "Kiểm tra dữ liệu, QC, dữ liệu thiếu, CMV",
    "Mô tả mẫu và thống kê mô tả",
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
doc <- body_add_flextable(doc, make_ft(pipeline_tbl, printable_width = 5.8))
doc <- add_text(doc, "Nguồn: Tổng hợp từ README và script pipeline hiện hành.")

# Section B
doc <- body_add_break(doc, pos = "after")
doc <- add_text(doc, section_titles[["B"]], bold = TRUE)
catalog <- manifest[manifest$included_in_word_v3 == "yes", c("csv_path", "inferred_pipeline_step", "n_rows", "n_columns")]
names(catalog) <- c("Tệp CSV", "Bước phân tích", "Số dòng", "Số cột")
cb <- add_caption(doc, "B", "Danh mục bảng kết quả cốt lõi được đưa vào bản V3")
doc <- cb$doc
doc <- body_add_flextable(doc, make_ft(catalog, printable_width = 5.8, compact = TRUE))
doc <- add_text(doc, "Nguồn: Tổng hợp từ manifest V3.")

# Sections C-G only core tables
for (sec in c("C", "D", "E", "F", "G")) {
  doc <- body_add_break(doc, pos = "after")
  doc <- add_text(doc, section_titles[[sec]], bold = TRUE)

  rows_idx <- which(manifest$included_in_word_v3 == "yes" & vapply(manifest$csv_path, infer_section, character(1)) == sec)
  if (length(rows_idx) == 0) next

  for (idx in rows_idx) {
    rel <- manifest$csv_path[idx]
    df <- store[[rel]]
    if (is.null(df)) next

    if (ncol(df) > 12) {
      blocks <- split_wide_table(df)
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
          doc <- add_text(doc, "Ghi chú: Bảng được tách thành nhiều phần để bảo đảm tính đọc được trong khổ in.")
          first <- FALSE
        }
        doc <- add_text(doc, "")
      }
      manifest$word_table_number[manifest$csv_path == rel] <- paste(nums, collapse = "; ")
    } else {
      cap <- add_caption(doc, sec, caption_map[[rel]])
      doc <- cap$doc
      manifest$word_table_number[manifest$csv_path == rel] <- cap$no
      doc <- body_add_flextable(doc, make_ft(df, printable_width = 5.8, compact = (ncol(df) > 8)))
      doc <- add_text(doc, sprintf("Nguồn: Tổng hợp từ tệp %s.", rel))
      doc <- add_text(doc, "")
    }
  }
}

# Section H
doc <- body_add_break(doc, pos = "after")
doc <- add_text(doc, section_titles[["H"]], bold = TRUE)
qc <- data.frame(
  Nội_dung = c("Tổng số CSV trong repo", "Số CSV đưa vào Word V3", "Số CSV loại khỏi Word V3", "Nguyên tắc bản V3"),
  Kết_quả = c(
    as.character(nrow(manifest)),
    as.character(sum(manifest$included_in_word_v3 == "yes")),
    as.character(sum(manifest$included_in_word_v3 == "no")),
    "Chỉ giữ bảng kết quả tổng hợp theo bước; loại bảng kỹ thuật và bảng dòng chi tiết"
  ),
  stringsAsFactors = FALSE
)
ch <- add_caption(doc, "H", "Nhật ký kiểm tra nhất quán giữa phụ lục V3 và dữ liệu nguồn")
doc <- ch$doc
doc <- body_add_flextable(doc, make_ft(qc, printable_width = 5.8))
doc <- add_text(doc, "Nguồn: Tổng hợp từ appendix_v3_csv_manifest.csv.")

# Section I
doc <- body_add_break(doc, pos = "after")
doc <- add_text(doc, section_titles[["I"]], bold = TRUE)
excluded <- manifest[manifest$included_in_word_v3 == "no", c("csv_path", "reason_if_excluded", "n_rows", "n_columns")]
if (nrow(excluded) == 0) {
  excluded <- data.frame(csv_path = "(không có)", reason_if_excluded = "-", n_rows = "-", n_columns = "-", stringsAsFactors = FALSE)
}
names(excluded) <- c("Tệp CSV", "Lý do không đưa vào Word V3", "Số dòng", "Số cột")
ci <- add_caption(doc, "I", "Danh mục tệp không trình bày trong bản V3")
doc <- ci$doc
doc <- body_add_flextable(doc, make_ft(excluded, printable_width = 5.8, compact = TRUE))
doc <- add_text(doc, "Nguồn: Tổng hợp từ appendix_v3_csv_manifest.csv.")

# Save
if (file.exists(tmp_docx)) unlink(tmp_docx, force = TRUE)
print(doc, target = tmp_docx)
if (file.exists(output_docx)) unlink(output_docx, force = TRUE)
ok <- file.rename(tmp_docx, output_docx)
if (!ok) stop("Không thể ghi đè tệp DOCX V3. Vui lòng đóng Word nếu đang mở tệp đích.")

write.csv(manifest, output_manifest, row.names = FALSE, fileEncoding = "UTF-8")

log_lines <- c(
  "# Appendix V3 Build Log",
  "",
  paste0("Build time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## Triết lý bản V3",
  "- Chỉ giữ các bảng kết quả tổng hợp theo từng bước phân tích.",
  "- Loại bảng xử lý kỹ thuật, metadata và bảng mức dòng quan sát.",
  "",
  "## Số lượng bảng",
  paste0("- CSV đưa vào Word V3: ", sum(manifest$included_in_word_v3 == "yes")),
  paste0("- CSV không đưa vào Word V3: ", sum(manifest$included_in_word_v3 == "no")),
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
