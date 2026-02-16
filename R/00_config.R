# ==============================================================================
# 00_config.R — Configuration Loader & Validator
# ==============================================================================
# Đọc analysis_config.yaml, tạo object cfg, validate tính đầy đủ
# ==============================================================================

suppressPackageStartupMessages({
  library(yaml)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

#' Đọc cấu hình phân tích từ YAML
#' @param config_path Đường dẫn đến analysis_config.yaml
#' @return list cfg chứa mọi cấu hình
load_config <- function(config_path = "00_meta/analysis_config.yaml") {
  if (!file.exists(config_path)) {
    stop("Config file not found: ", config_path)
  }
  
  cfg <- yaml::read_yaml(config_path)
  
  # Derive helper lists
  cfg$all_indicators <- unlist(lapply(cfg$constructs, function(c) c$indicators))
  
  cfg$reflective_constructs <- Filter(
    function(c) c$measurement_type == "reflective", cfg$constructs
  )
  cfg$formative_constructs <- Filter(
    function(c) c$measurement_type == "formative", cfg$constructs
  )
  
  cfg$construct_names <- sapply(cfg$constructs, function(c) c$name)
  
  cfg
}

#' Validate cấu hình — kiểm tra tính đầy đủ
#' @param cfg list config
#' @return invisible(TRUE) nếu hợp lệ; stop() nếu không
validate_config <- function(cfg) {
  errors <- character()
  
  # Required top-level keys
  required_keys <- c("project", "constructs", "structural_paths",
                     "screening", "thresholds_reflective",
                     "thresholds_formative", "thresholds_structural",
                     "cmv", "plspredict", "scale")
  missing <- setdiff(required_keys, names(cfg))
  if (length(missing) > 0) {
    errors <- c(errors, paste("Missing config keys:", paste(missing, collapse = ", ")))
  }
  
  # Check constructs have required fields
  for (cc in cfg$constructs) {
    if (is.null(cc$name)) errors <- c(errors, "Construct missing 'name'")
    if (is.null(cc$measurement_type)) {
      errors <- c(errors, paste("Construct", cc$name, "missing 'measurement_type'"))
    }
    if (is.null(cc$indicators) || length(cc$indicators) == 0) {
      errors <- c(errors, paste("Construct", cc$name, "has no indicators"))
    }
  }
  
  # Check structural paths
  all_names <- cfg$construct_names
  for (p in cfg$structural_paths) {
    if (!(p$from %in% all_names)) {
      errors <- c(errors, paste("Path 'from'", p$from, "not in constructs"))
    }
    if (!(p$to %in% all_names)) {
      errors <- c(errors, paste("Path 'to'", p$to, "not in constructs"))
    }
  }
  
  if (length(errors) > 0) {
    stop("Config validation FAILED:\n", paste("  -", errors, collapse = "\n"))
  }
  
  cat("\u2705 Config validation passed\n")
  invisible(TRUE)
}

#' Lấy danh sách indicators cho một construct
#' @param cfg list config
#' @param construct_name Tên construct (e.g. "COM")
#' @return character vector
get_indicators <- function(cfg, construct_name) {
  cc <- Filter(function(c) c$name == construct_name, cfg$constructs)
  if (length(cc) == 0) stop("Construct not found: ", construct_name)
  cc[[1]]$indicators
}

#' Lấy loại đo lường cho construct
#' @param cfg list config
#' @param construct_name Tên construct
#' @return "reflective" hoặc "formative"
get_measurement_type <- function(cfg, construct_name) {
  cc <- Filter(function(c) c$name == construct_name, cfg$constructs)
  if (length(cc) == 0) stop("Construct not found: ", construct_name)
  cc[[1]]$measurement_type
}

#' Tạo thư mục output nếu chưa có
ensure_output_dirs <- function() {
  dirs <- c(
    "00_meta", "01_raw", "02_clean", "03_qc", "04_cmv",
    "05_measurement/reflective", "05_measurement/formative", "05_measurement/hoc",
    "06_structural", "07_predict",
    "08_complex/mediation", "08_complex/moderation/moderation_plots",
    "09_robust/sensitivity_data", "09_robust/micom_mga",
    "09_robust/endogeneity", "09_robust/fimix",
    "10_report/figures", "logs"
  )
  for (d in dirs) dir.create(d, showWarnings = FALSE, recursive = TRUE)
}
