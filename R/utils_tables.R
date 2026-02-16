# ==============================================================================
# utils_tables.R — Thesis-ready Table Utilities (flextable + officer)
# ==============================================================================

suppressPackageStartupMessages({
  library(flextable)
  library(officer)
})

#' Tạo flextable thesis-ready
#' @param df data.frame
#' @param title Tiêu đề bảng
#' @param note Ghi chú dưới bảng
#' @param font_size Cỡ chữ (default 10)
#' @return flextable object
make_thesis_table <- function(df, title = NULL, note = NULL, font_size = 10) {
  ft <- flextable(df) |>
    font(fontname = "Times New Roman", part = "all") |>
    fontsize(size = font_size, part = "all") |>
    fontsize(size = font_size + 1, part = "header") |>
    bold(part = "header") |>
    align(align = "center", part = "header") |>
    align(align = "center", part = "body") |>
    autofit() |>
    border_inner(border = fp_border(color = "gray60", width = 0.5)) |>
    border_outer(border = fp_border(color = "black", width = 1))
  
  # Align first column left if it's text
  if (ncol(df) > 0 && is.character(df[[1]])) {
    ft <- align(ft, j = 1, align = "left", part = "body")
  }
  
  if (!is.null(title)) {
    ft <- set_caption(ft, caption = title)
  }
  
  if (!is.null(note)) {
    ft <- add_footer_lines(ft, values = note) |>
      fontsize(size = font_size - 1, part = "footer") |>
      italic(part = "footer")
  }
  
  ft
}

#' Xuất nhiều flextable vào một file Word
#' @param tables list of flextable objects
#' @param file_path Đường dẫn file .docx output
#' @param page_size "A4" hoặc "letter"
export_tables_to_word <- function(tables, file_path, page_size = "A4") {
  if (page_size == "A4") {
    doc <- read_docx() |>
      body_add_par("", style = "Normal")  # Paragraph mở đầu
  } else {
    doc <- read_docx()
  }
  
  for (i in seq_along(tables)) {
    if (i > 1) {
      doc <- body_add_break(doc, pos = "after")  # Page break giữa các bảng
    }
    
    ft <- tables[[i]]
    doc <- body_add_flextable(doc, value = ft)
    doc <- body_add_par(doc, value = "", style = "Normal")  # Spacing
  }
  
  print(doc, target = file_path)
  cat("Exported to:", file_path, "\n")
}

#' Highlight giá trị không đạt ngưỡng (đỏ)
#' @param ft flextable
#' @param col Tên cột
#' @param threshold Ngưỡng
#' @param direction "below" hoặc "above"
highlight_threshold <- function(ft, col, threshold, direction = "below") {
  # Safe approach: get data from flextable body, compute row indices, apply color
  tryCatch({
    col_data <- ft$body$dataset[[col]]
    if (!is.null(col_data) && is.numeric(col_data)) {
      if (direction == "below") {
        flag_rows <- which(col_data < threshold)
      } else {
        flag_rows <- which(col_data > threshold)
      }
      if (length(flag_rows) > 0) {
        ft <- color(ft, i = flag_rows, j = col, color = "red")
      }
    }
  }, error = function(e) NULL)
  ft
}
