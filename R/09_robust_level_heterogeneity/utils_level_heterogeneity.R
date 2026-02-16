# ==============================================================================
# utils_level_heterogeneity.R — Shared helpers for Level-based Heterogeneity
# ==============================================================================
# Self-contained logging + config loading + validation.
# Does NOT depend on pipeline utils_logging.R (but mirrors its style).
# ==============================================================================

# ---- Silence helper ----------------------------------------------------------

#' Evaluate an expression while suppressing ALL console output (cat, message, print).
#' Uses capture.output for stdout and suppressMessages for stderr.
#' Error-safe: no manual sink manipulation needed.
#' @param expr expression to evaluate quietly
#' @return result of expr
.lh_quietly <- function(expr) {
  env <- environment()
  invisible(utils::capture.output(
    env$.result <- suppressWarnings(suppressMessages(force(expr)))
  ))
  env$.result
}

# ---- Logging ----------------------------------------------------------------

#' Initialise a standalone log for this module
#' @param output_dir character path for output directory
#' @return log_info list with `con`, `path`, `start_time`
lh_init_log <- function(output_dir) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  log_path <- file.path(output_dir, "log.txt")
  con <- file(log_path, open = "wt")
  log_info <- list(con = con, path = log_path, start_time = Sys.time())
  lh_log(log_info, "========================================================")
  lh_log(log_info, "Sensitivity Analysis for Level-based Heterogeneity")
  lh_log(log_info, sprintf("Started: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  lh_log(log_info, sprintf("R version: %s", R.version.string))
  lh_log(log_info, "========================================================")
  log_info
}

#' Write a line to log file + console
#' @param log_info list from lh_init_log (may be NULL for console-only)
#' @param msg character message
#' @param level character "INFO" | "WARN" | "ERROR" | "STEP" | "GATE"
lh_log <- function(log_info, msg, level = "INFO") {
  ts   <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  line <- paste(ts, sprintf("[%-5s]", level), msg)
  cat(line, "\n")
  con_ok <- tryCatch(
    !is.null(log_info) && is.list(log_info) &&
      !is.null(log_info$con) && isOpen(log_info$con),
    error = function(e) FALSE
  )
  if (con_ok) {
    writeLines(line, log_info$con)
    flush(log_info$con)
  }
}

#' Log a step header
lh_step <- function(log_info, name) {
  lh_log(log_info, "")
  lh_log(log_info, paste(">>>", name), level = "STEP")
  lh_log(log_info, paste(rep("-", 60), collapse = ""))
}

#' Log a gate result (PASS/FAIL)
lh_gate <- function(log_info, gate_name, passed, details = "") {
  status <- ifelse(passed, "PASS", "FAIL")
  lh_log(log_info, sprintf("[GATE] %s: %s %s", gate_name, status, details),
         level = "GATE")
}

#' Close log
lh_close_log <- function(log_info) {
  elapsed <- difftime(Sys.time(), log_info$start_time, units = "mins")
  lh_log(log_info, "")
  lh_log(log_info, sprintf("Elapsed: %.1f minutes", as.numeric(elapsed)))
  lh_log(log_info, "========================================================")
  lh_log(log_info, "Module finished.")
  lh_log(log_info, "========================================================")
  tryCatch(close(log_info$con), error = function(e) NULL)
}

# ---- Config helpers ----------------------------------------------------------

#' Load and validate module config
#' @param config_path character path to YAML
#' @return validated config list
lh_load_config <- function(config_path) {
  if (!file.exists(config_path))
    stop("Config file not found: ", config_path)
  cfg <- yaml::read_yaml(config_path)

  # Required top-level keys
  required <- c("enabled", "seed", "paths", "exogenous_constructs",
                 "endogenous_constructs", "clustering", "micom", "mga")
  missing <- setdiff(required, names(cfg))
  if (length(missing) > 0)
    stop("Config missing required keys: ", paste(missing, collapse = ", "))

  # Circularity check: exo ∩ endo must be empty
  overlap <- intersect(cfg$exogenous_constructs, cfg$endogenous_constructs)
  if (length(overlap) > 0)
    stop("CIRCULARITY VIOLATION: constructs in BOTH exogenous and endogenous: ",
         paste(overlap, collapse = ", "),
         "\n  Clustering must use ONLY exogenous construct scores.")

  # Defaults
  if (is.null(cfg$backend)) cfg$backend <- "auto"
  if (is.null(cfg$confidence_level)) cfg$confidence_level <- 0.95
  if (is.null(cfg$bootstrap_samples)) cfg$bootstrap_samples <- 5000
  if (is.null(cfg$clustering$kmeans_nstart)) cfg$clustering$kmeans_nstart <- 25
  if (is.null(cfg$clustering$hclust_linkage)) cfg$clustering$hclust_linkage <- "ward.D2"
  if (is.null(cfg$mga$p_adjust_method)) cfg$mga$p_adjust_method <- "holm"
  if (is.null(cfg$reporting)) cfg$reporting <- list(
    export_csv = TRUE, export_md = TRUE, export_plots = TRUE,
    plot_width = 8, plot_height = 6, plot_dpi = 300
  )

  cfg
}

# ---- Backend detection -------------------------------------------------------

#' Detect available PLS backend and load model / scores
#' @param cfg module config
#' @param log_info log object
#' @return list(backend, pls_model, boot_model, scores_matrix, data_raw,
#'              base_cfg, active_indicators)
lh_detect_backend <- function(cfg, log_info) {

  backend_choice <- tolower(cfg$backend)
  pls_model  <- NULL
  boot_model <- NULL
  scores_mat <- NULL
  data_raw   <- NULL
  base_cfg   <- NULL
  active_ind <- NULL

  # -- Try loading saved seminr objects first ----------------------------------
  pls_path  <- cfg$paths$pls_model_path
  boot_path <- cfg$paths$boot_model_path
  data_path <- cfg$paths$data_path
  ai_path   <- cfg$paths$active_indicators_path
  base_path <- cfg$paths$config_path

  seminr_ok <- FALSE
  if (backend_choice %in% c("auto", "seminr")) {
    if (!is.null(pls_path) && file.exists(pls_path)) {
      lh_log(log_info, sprintf("Loading PLS model from %s", pls_path))
      pls_model <- readRDS(pls_path)
      if ("construct_scores" %in% names(pls_model)) {
        scores_mat <- pls_model$construct_scores
        seminr_ok  <- TRUE
        lh_log(log_info, sprintf("  seminr model loaded: %d obs, %d constructs",
                                  nrow(scores_mat), ncol(scores_mat)))
      }
    }
    if (!is.null(boot_path) && file.exists(boot_path)) {
      boot_model <- readRDS(boot_path)
      lh_log(log_info, sprintf("  Boot model loaded from %s", boot_path))
    }
    if (!is.null(ai_path) && file.exists(ai_path)) {
      active_ind <- readRDS(ai_path)
      lh_log(log_info, sprintf("  Active indicators loaded (%d constructs)",
                                length(active_ind)))
    }
  }

  # -- Fallback: cSEM (not implemented in detail, placeholder) ----------------
  csem_ok <- FALSE
  if (!seminr_ok && backend_choice %in% c("auto", "csem")) {
    if (requireNamespace("cSEM", quietly = TRUE)) {
      lh_log(log_info, "seminr model not available; cSEM backend selected.",
             level = "WARN")
      lh_log(log_info, "cSEM backend: will attempt to re-estimate.",
             level = "WARN")
      csem_ok <- TRUE
      # cSEM re-estimation would go here if needed
      # For now, we require raw data + base config to rebuild
    } else {
      lh_log(log_info, "Neither seminr model nor cSEM package available.",
             level = "ERROR")
      stop("No usable PLS backend. Provide pls_model.rds or install cSEM.")
    }
  }

  # -- Load raw data (needed for re-estimation in sub-groups) -----------------
  if (!is.null(data_path) && file.exists(data_path)) {
    lh_log(log_info, sprintf("Loading raw data from %s", data_path))
    data_raw <- readRDS(data_path)
    lh_log(log_info, sprintf("  Data: %d rows × %d cols", nrow(data_raw), ncol(data_raw)))
  }

  # -- Load base analysis config (for model spec) ----------------------------
  if (!is.null(base_path) && file.exists(base_path)) {
    base_cfg <- yaml::read_yaml(base_path)
    lh_log(log_info, sprintf("  Base analysis config loaded: %s", base_path))
  }

  backend_used <- if (seminr_ok) "seminr" else if (csem_ok) "cSEM" else "none"
  lh_log(log_info, sprintf("Backend selected: %s", backend_used))

  list(
    backend      = backend_used,
    pls_model    = pls_model,
    boot_model   = boot_model,
    scores       = scores_mat,
    data_raw     = data_raw,
    base_cfg     = base_cfg,
    active_ind   = active_ind
  )
}

# ---- Model specification helpers (seminr) ------------------------------------

#' Build seminr measurement_model from base config + active indicators
#' @param base_cfg list from analysis_config.yaml
#' @param active_ind named list of construct -> indicator vectors (optional override)
#' @return seminr measurement_model object
lh_build_mm <- function(base_cfg, active_ind = NULL) {
  mm_items <- list()
  for (cc in base_cfg$constructs) {
    inds <- if (!is.null(active_ind) && cc$name %in% names(active_ind))
      active_ind[[cc$name]] else cc$indicators
    if (cc$measurement_type == "reflective") {
      mm_items[[length(mm_items) + 1]] <- seminr::reflective(cc$name, inds)
    } else {
      mm_items[[length(mm_items) + 1]] <- seminr::composite(cc$name, inds,
                                                              weights = seminr::mode_B)
    }
  }
  do.call(seminr::constructs, mm_items)
}

#' Build seminr structural_model from base config
#' @param base_cfg list from analysis_config.yaml
#' @return seminr structural_model object
lh_build_sm <- function(base_cfg) {
  sm_list <- lapply(base_cfg$structural_paths, function(p) {
    seminr::paths(from = p$from, to = p$to)
  })
  do.call(seminr::relationships, sm_list)
}

#' Estimate PLS model for a data subset
#' @param data data.frame of raw indicators
#' @param base_cfg base analysis config
#' @param active_ind optional indicator override
#' @return seminr estimated PLS model
lh_estimate_pls <- function(data, base_cfg, active_ind = NULL) {
  mm <- lh_build_mm(base_cfg, active_ind)
  sm <- lh_build_sm(base_cfg)
  all_ind <- unique(unlist(lapply(base_cfg$constructs, function(cc) {
    if (!is.null(active_ind) && cc$name %in% names(active_ind))
      active_ind[[cc$name]] else cc$indicators
  })))
  pls_data <- data[, intersect(all_ind, colnames(data)), drop = FALSE]
  .lh_quietly(
    seminr::estimate_pls(data = pls_data, measurement_model = mm,
                          structural_model = sm)
  )
}

#' Bootstrap a PLS model
lh_bootstrap_pls <- function(pls_model, nboot, seed) {
  .lh_quietly(
    seminr::bootstrap_model(seminr_model = pls_model, nboot = nboot, seed = seed)
  )
}

# ---- Formatting helpers ------------------------------------------------------

#' Format a numeric value
lh_fmt <- function(x, digits = 3) {
  ifelse(is.na(x), "NA", formatC(round(x, digits), format = "f", digits = digits))
}

#' Format p-value
lh_fmt_p <- function(p) {
  ifelse(is.na(p), "NA",
         ifelse(p < 0.001, "<0.001", formatC(round(p, 4), format = "f", digits = 4)))
}

#' Significance star
lh_star <- function(p, alpha = 0.05) {
  ifelse(!is.na(p) & p < alpha, "*", "")
}

# ---- Parse path strings ------------------------------------------------------

#' Parse "COM -> AJ" into list(from, to)
lh_parse_path <- function(path_str) {
  parts <- trimws(strsplit(path_str, "->")[[1]])
  if (length(parts) != 2) stop("Invalid path format: ", path_str, " (expected 'X -> Y')")
  list(from = parts[1], to = parts[2])
}

# ---- Safe directory creation -------------------------------------------------

lh_ensure_dirs <- function(output_dir) {
  sub_dirs <- c("tables", "plots", "models")
  for (d in sub_dirs) {
    dir.create(file.path(output_dir, d), showWarnings = FALSE, recursive = TRUE)
  }
}
