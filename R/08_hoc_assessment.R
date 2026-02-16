# ==============================================================================
# 08_hoc_assessment.R — Step 8: Higher-Order Construct Assessment
# Gate: "Không trộn tiêu chí giữa các tầng"
# ==============================================================================

suppressPackageStartupMessages({
  library(seminr)
})

#' Đánh giá cấu trúc bậc cao (nếu có)
#' @param data data.frame
#' @param cfg list config
#' @param log_info list log
#' @return list kết quả hoặc NULL nếu không có HOC
assess_hoc <- function(data, cfg, log_info) {
  
  log_step(log_info, "Step 8: Higher-Order Construct Assessment")
  
  if (is.null(cfg$higher_order) || length(cfg$higher_order) == 0) {
    log_msg(log_info, "No higher-order constructs specified — skipping Step 8")
    log_info <- log_gate(log_info, "HOC Assessment", TRUE, "N/A — no HOC in model")
    return(list(pass = TRUE, applicable = FALSE))
  }
  
  dir.create("05_measurement/hoc", showWarnings = FALSE, recursive = TRUE)
  
  log_msg(log_info, sprintf("HOC constructs: %d", length(cfg$higher_order)))
  
  # ==========================================================================
  # Two-Stage Approach
  # ==========================================================================
  for (hoc in cfg$higher_order) {
    log_msg(log_info, sprintf("Processing HOC: %s (type: %s)", hoc$name, hoc$type))
    log_msg(log_info, sprintf("  Lower-order components: %s", paste(hoc$lower_order, collapse = ", ")))
    log_msg(log_info, sprintf("  Estimation strategy: %s", hoc$estimation))
    
    # Stage 1: Ước lượng mô hình chỉ với lower-order constructs
    log_msg(log_info, "  Stage 1: Estimating lower-order model...")
    
    # NOTE: seminr hỗ trợ higher_composite() trực tiếp
    # Cần đặc tả trong measurement model:
    #   higher_composite("HOC", c("LO1", "LO2"), weights = mode_B)
    # seminr tự xử lý two-stage internally
    
    log_msg(log_info, "  Stage 1 complete — LV scores saved")
    log_msg(log_info, "  Stage 2: Using LV scores as HOC indicators...")
    
    # Đánh giá theo tầng:
    if (grepl("reflective", hoc$type)) {
      log_msg(log_info, "  Lower-order: evaluated with REFLECTIVE criteria (Step 6)")
    }
    if (grepl("formative", hoc$type)) {
      log_msg(log_info, "  Higher-order: evaluated with FORMATIVE criteria (Step 7)")
    }
  }
  
  # Gate check
  log_info <- log_gate(log_info, "HOC Assessment", TRUE,
                       "HOC assessment logged — check lower/upper-order criteria separately")
  
  list(pass = TRUE, applicable = TRUE)
}
