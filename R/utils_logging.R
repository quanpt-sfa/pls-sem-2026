# ==============================================================================
# utils_logging.R — Logging & Audit Trail Utilities
# ==============================================================================
# Cung cấp hệ thống ghi nhật ký cho toàn bộ pipeline:
#   - Console output + file log đồng thời
#   - Timestamp mỗi dòng
#   - Gate pass/fail tracking
# ==============================================================================

# --- Module-level environment to track the active log connection ---
# This prevents "closing unused connection" warnings when a previous run
# crashed without closing its log file. Only the log connection is stored
# here — no other connections (DB, graphics, sinks) are touched.
.log_env <- new.env(parent = emptyenv())
.log_env$con <- NULL

#' Khởi tạo hệ thống log
#' @param log_dir Thư mục chứa file log
#' @param prefix Tiền tố cho tên file log ("pipeline", "pilot", "main")
#' @return list chứa thông tin log (file connection, path, start time)
init_pipeline_log <- function(log_dir = "logs", prefix = "pipeline") {
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)

  # Close stale log connection from a previous crashed run (if any)
  if (!is.null(.log_env$con)) {
    tryCatch({
      if (isOpen(.log_env$con)) close(.log_env$con)
    }, error = function(e) NULL)
    .log_env$con <- NULL
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_path  <- file.path(log_dir, paste0(prefix, "_", timestamp, ".log"))
  log_con   <- file(log_path, open = "wt")

  # Store handle so we can clean up on next init if this run crashes
  .log_env$con <- log_con
  
  # Use an environment for gates so modifications persist by reference
  # across all function calls (R lists are copy-on-modify)
  gate_env <- new.env(parent = emptyenv())
  gate_env$gates <- list()

  log_info <- list(
    con        = log_con,
    path       = log_path,
    start_time = Sys.time(),
    gate_env   = gate_env    # Pass-by-reference gate tracker
  )
  
  log_msg(log_info, "========================================")
  log_msg(log_info, "CCA-SEM Pipeline — Log started")
  log_msg(log_info, paste("Time:", Sys.time()))
  log_msg(log_info, paste("R version:", R.version.string))
  log_msg(log_info, "========================================")
  
  log_info
}

#' Ghi một dòng log
#' @param log_info list từ init_pipeline_log()
#' @param msg Nội dung message
#' @param level Mức: "INFO", "WARN", "ERROR", "GATE"
log_msg <- function(log_info, msg, level = "INFO") {
  ts   <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  line <- paste(ts, paste0("[", level, "]"), msg)
  
  # Console
  cat(line, "\n")
  
  # File
  if (!is.null(log_info$con) && isOpen(log_info$con)) {
    writeLines(line, log_info$con)
    flush(log_info$con)
  }
}

#' Log bắt đầu một bước (step)
#' @param log_info list log
#' @param step_name Tên bước (e.g. "Step 1: Import & Validate")
log_step <- function(log_info, step_name) {
  log_msg(log_info, "")
  log_msg(log_info, paste(">>>", step_name))
  log_msg(log_info, paste(rep("-", 50), collapse = ""))
}

#' Ghi kết quả gate (pass/fail)
#' @param log_info list log
#' @param gate_name Tên gate
#' @param passed TRUE/FALSE
#' @param details Chi tiết (optional)
log_gate <- function(log_info, gate_name, passed, details = NULL) {
  status <- ifelse(isTRUE(passed), "PASS", "FAIL")
  icon   <- ifelse(isTRUE(passed), "\u2705", "\u274c")
  log_msg(log_info, paste(icon, "GATE [", gate_name, "]:", status), level = "GATE")
  
  if (!is.null(details)) {
    log_msg(log_info, paste("  Details:", details), level = "GATE")
  }
  
  # Track in gate environment (pass-by-reference — persists in caller)
  log_info$gate_env$gates[[gate_name]] <- list(
    passed  = isTRUE(passed),
    details = details,
    time    = Sys.time()
  )
  
  invisible(log_info)
}

#' Log quyết định giữ/loại indicator
#' @param log_info list log
#' @param indicator Tên indicator (e.g. "COM3")
#' @param decision "KEEP" hoặc "DROP"
#' @param reason Lý do
log_indicator_decision <- function(log_info, indicator, decision, reason) {
  log_msg(log_info,
          paste("INDICATOR [", indicator, "]:", decision, "—", reason),
          level = "INFO")
}

#' Log cảnh báo
log_warn <- function(log_info, msg) {
  log_msg(log_info, msg, level = "WARN")
}

#' Log lỗi
log_error <- function(log_info, msg) {
  log_msg(log_info, msg, level = "ERROR")
}

#' Đóng hệ thống log
#' @param log_info list log
close_pipeline_log <- function(log_info) {
  elapsed <- difftime(Sys.time(), log_info$start_time, units = "mins")
  
  log_msg(log_info, "")
  log_msg(log_info, "========================================")
  log_msg(log_info, "Pipeline log closed")
  log_msg(log_info, sprintf("Total time: %.1f minutes", as.numeric(elapsed)))
  
  # Summary of gates (from environment — always up-to-date)
  gate_list <- log_info$gate_env$gates
  if (length(gate_list) > 0) {
    n_pass <- sum(vapply(gate_list, function(g) isTRUE(g$passed), logical(1)))
    n_fail <- length(gate_list) - n_pass
  } else {
    n_pass <- 0L
    n_fail <- 0L
  }
  log_msg(log_info, sprintf("Gates: %d passed, %d failed (total: %d)",
                             n_pass, n_fail, length(gate_list)))
  # Print individual gate results
  for (gn in names(gate_list)) {
    g <- gate_list[[gn]]
    log_msg(log_info, sprintf("  %s %s%s",
                               ifelse(isTRUE(g$passed), "\u2705", "\u274c"),
                               gn,
                               ifelse(is.null(g$details), "",
                                      paste0(" — ", g$details))))
  }
  log_msg(log_info, "========================================")
  
  if (!is.null(log_info$con) && isOpen(log_info$con)) {
    close(log_info$con)
  }
  # Clear module-level handle so next init doesn't re-close

  .log_env$con <- NULL
  
  cat("\nLog saved to:", log_info$path, "\n")
}
