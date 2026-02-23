# ==============================================================================
# run_core.R — Core Pipeline Orchestrator (Stage-aware)
# ==============================================================================
# Cung cấp hàm run_stage(stage_config_path) dùng chung cho cả pilot và main.
# Tất cả logic pipeline nằm ở đây; entrypoints chỉ gọi hàm này.
# ==============================================================================

# --- Package check ---
check_required_packages <- function() {
  required_packages <- c(
    "yaml", "readxl", "dplyr", "tidyr", "stringr",
    "seminr", "ggplot2", "flextable", "officer", "jsonlite"
  )
  missing_pkgs <- setdiff(required_packages, rownames(installed.packages()))
  if (length(missing_pkgs) > 0) {
    cat("\nMissing required packages:", paste(missing_pkgs, collapse = ", "), "\n")
    cat("Install with: install.packages(c(",
        paste0('"', missing_pkgs, '"', collapse = ", "), "))\n")
    stop("Please install required packages first.")
  }
}

# --- Source all analysis modules ---
source_modules <- function() {
  source("R/00_config.R")
  source("R/utils_logging.R")
  source("R/utils_tables.R")
  source("R/utils_inference.R")
  source("R/01_import_validate.R")
  source("R/02_qc_behavioral.R")
  source("R/03_missing_data.R")
  source("R/04_descriptives.R")
  source("R/05_cmv_assessment.R")
  source("R/06_measurement_reflective.R")
  source("R/07_measurement_formative.R")
  source("R/08_hoc_assessment.R")
  source("R/09_structural_insample.R")
  source("R/10_plspredict.R")
  source("R/11_mediation.R")
  source("R/12_moderation.R")
  source("R/13_robustness.R")
  source("R/13b_gaussian_copula.R")
  source("R/14_robust_nonlinear.R")
  source("R/14_report_export.R")
  source("R/15_sensitivity_com6.R")
}

# ==============================================================================
# INSTRUMENT LOCK — Write (pilot) / Read+Enforce (main)
# ==============================================================================

#' Write instrument lock file after pilot completes
#' @param cfg config object
#' @param active_indicators list of construct -> indicator vectors (post-optimizer)
#' @param removed_log data.frame of auto-removed indicators
#' @param output_dir output directory for lock file
#' @param log_info log object
write_instrument_lock <- function(cfg, active_indicators, removed_log, output_dir, log_info) {
  lock <- list(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    git_commit = tryCatch(
      trimws(system("git rev-parse --short HEAD", intern = TRUE)),
      error   = function(e) NA_character_,
      warning = function(w) NA_character_
    ),
    r_version   = R.version.string,
    seed        = cfg$project$seed,
    n_constructs = length(cfg$constructs),
    constructs  = lapply(cfg$constructs, function(cc) {
      list(
        name             = cc$name,
        full_name        = cc$full_name,
        measurement_type = cc$measurement_type,
        original_indicators = cc$indicators,     # From config YAML (pre-optimizer)
        active_indicators   = active_indicators[[cc$name]]
      )
    })
  )

  # Items removed during pilot
  if (nrow(removed_log) > 0) {
    lock$removed_items <- lapply(seq_len(nrow(removed_log)), function(i) {
      list(
        construct = removed_log$Construct[i],
        indicator = removed_log$Indicator[i],
        loading   = removed_log$Loading[i],
        reason    = sprintf("Auto-removed: loading %.3f < 0.40 (pilot iteration %d)",
                            removed_log$Loading[i], removed_log$Iteration[i])
      )
    })
  } else {
    lock$removed_items <- list()
  }

  lock_path <- file.path(output_dir, "instrument_locked.json")
  jsonlite::write_json(lock, lock_path, pretty = TRUE, auto_unbox = TRUE)
  log_msg(log_info, sprintf("Instrument lock written: %s", lock_path))

  invisible(lock_path)
}

#' Read instrument lock file and enforce frozen indicators
#' @param lock_path path to instrument_locked.json
#' @param cfg config object (will be modified in-place via returned value)
#' @param log_info log object
#' @return list(cfg, active_indicators) with frozen instrument applied
read_instrument_lock <- function(lock_path, cfg, log_info) {
  if (!file.exists(lock_path)) {
    stop(
      "\n============================================================\n",
      "  INSTRUMENT LOCK NOT FOUND\n",
      "  Expected: ", lock_path, "\n\n",
      "  You must run the PILOT stage first to create the instrument lock:\n",
      "    Rscript run_pilot.R\n\n",
      "  The pilot will evaluate and freeze the measurement instrument.\n",
      "  Only then can the main study proceed.\n",
      "============================================================\n"
    )
  }

  lock <- jsonlite::fromJSON(lock_path, simplifyVector = FALSE)
  log_msg(log_info, sprintf("Instrument lock loaded: %s", lock_path))
  log_msg(log_info, sprintf("  Lock timestamp: %s", lock$timestamp))
  if (!is.na(lock$git_commit) && !is.null(lock$git_commit)) {
    log_msg(log_info, sprintf("  Lock git commit: %s", lock$git_commit))
  }

  # Build active_indicators from lock
  active_indicators <- list()
  for (cc_lock in lock$constructs) {
    active_indicators[[cc_lock$name]] <- unlist(cc_lock$active_indicators)
    log_msg(log_info, sprintf("  %s [%s]: %d indicators (locked)",
                               cc_lock$name, cc_lock$measurement_type,
                               length(active_indicators[[cc_lock$name]])))
  }

  # Report removed items
  if (length(lock$removed_items) > 0) {
    log_msg(log_info, sprintf("  Items removed in pilot: %d", length(lock$removed_items)))
    for (ri in lock$removed_items) {
      log_msg(log_info, sprintf("    %s.%s — %s", ri$construct, ri$indicator, ri$reason))
    }
  }

  # Sync cfg$constructs with locked indicators
  for (i in seq_along(cfg$constructs)) {
    cn <- cfg$constructs[[i]]$name
    if (cn %in% names(active_indicators)) {
      cfg$constructs[[i]]$indicators <- active_indicators[[cn]]
    }
  }
  cfg$all_indicators <- unique(unlist(active_indicators))
  cfg$reflective_constructs <- Filter(function(c) c$measurement_type == "reflective", cfg$constructs)
  cfg$formative_constructs  <- Filter(function(c) c$measurement_type == "formative",  cfg$constructs)

  list(cfg = cfg, active_indicators = active_indicators)
}

# ==============================================================================
# APPLY FREEZE RULES (main only) — explicit overrides from main.yml
# ==============================================================================
apply_freeze_rules <- function(active_indicators, freeze_rules, log_info) {
  if (is.null(freeze_rules) || length(freeze_rules) == 0) return(active_indicators)

  log_msg(log_info, sprintf("Applying %d freeze rule(s)...", length(freeze_rules)))
  for (rule in freeze_rules) {
    cn  <- rule$construct
    ind <- rule$indicator
    act <- rule$action    # "drop" or "keep"
    rsn <- rule$reason

    if (act == "drop") {
      if (ind %in% active_indicators[[cn]]) {
        active_indicators[[cn]] <- setdiff(active_indicators[[cn]], ind)
        log_msg(log_info, sprintf("  [FREEZE-RULE] DROP %s.%s — %s", cn, ind, rsn))
      }
    } else if (act == "keep") {
      if (!(ind %in% active_indicators[[cn]])) {
        active_indicators[[cn]] <- c(active_indicators[[cn]], ind)
        log_msg(log_info, sprintf("  [FREEZE-RULE] KEEP %s.%s — %s", cn, ind, rsn))
      }
    }
  }
  active_indicators
}

# ==============================================================================
# AUDIT TRAIL — snapshot config + sessionInfo
# ==============================================================================
write_audit_trail <- function(stage_cfg_path, output_dir, log_info) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Copy stage config into output
  cfg_copy <- file.path(output_dir, paste0("config_snapshot_", basename(stage_cfg_path)))
  file.copy(stage_cfg_path, cfg_copy, overwrite = TRUE)
  log_msg(log_info, sprintf("Config snapshot saved: %s", cfg_copy))

  # Copy base config
  base_cfg <- "00_meta/analysis_config.yaml"
  if (file.exists(base_cfg)) {
    file.copy(base_cfg, file.path(output_dir, "config_snapshot_analysis_config.yaml"),
              overwrite = TRUE)
  }

  # Write sessionInfo
  si_path <- file.path(output_dir, "session_info.txt")
  writeLines(capture.output(sessionInfo()), si_path)
  log_msg(log_info, sprintf("Session info saved: %s", si_path))

  # Package versions
  pkgs <- c("seminr", "dplyr", "ggplot2", "flextable", "yaml", "jsonlite")
  pkg_versions <- sapply(pkgs, function(p) {
    tryCatch(as.character(packageVersion(p)), error = function(e) "not installed")
  })
  pkg_df <- data.frame(Package = names(pkg_versions), Version = unname(pkg_versions),
                        stringsAsFactors = FALSE)
  write.csv(pkg_df, file.path(output_dir, "package_versions.csv"), row.names = FALSE)
}

# ==============================================================================
# PILOT SUMMARY REPORT
# ==============================================================================
write_pilot_summary <- function(data_tech, data_qc, data_final,
                                 removed_log, cfg, log_info, output_dir,
                                 pls_feasibility = NULL) {
  lines <- character()
  lines <- c(lines, "# Pilot Summary Report (Type A — Feasibility)")
  lines <- c(lines, sprintf("Date: %s", Sys.time()))
  lines <- c(lines, sprintf("Seed: %d", cfg$project$seed))
  lines <- c(lines, "")

  # Sample flow
  lines <- c(lines, "## Sample")
  lines <- c(lines, sprintf("- Raw observations:           %d", nrow(data_tech)))
  lines <- c(lines, sprintf("- After QC screening:         %d", nrow(data_qc)))
  lines <- c(lines, sprintf("- After missing data:         %d", nrow(data_final)))
  n_removed <- nrow(data_tech) - nrow(data_final)
  pct_removed <- round(100 * n_removed / nrow(data_tech), 1)
  lines <- c(lines, sprintf("- Total removed:              %d (%.1f%%)", n_removed, pct_removed))
  lines <- c(lines, "")

  # Pre-PLS feasibility
  if (!is.null(pls_feasibility)) {
    lines <- c(lines, "## Pre-PLS Feasibility Check")
    if (pls_feasibility$feasible) {
      lines <- c(lines, "- Status: PASS — no data singularity issues")
    } else {
      lines <- c(lines, sprintf("- Status: RED FLAG (%d issue(s))", length(pls_feasibility$red_flags)))
      for (rf in pls_feasibility$red_flags) {
        lines <- c(lines, sprintf("  - %s", rf))
      }
      lines <- c(lines, "- Phase 2 (CMV + Measurement): SKIPPED due to red flags")
      lines <- c(lines, "- Note: Data singularity is expected with small N. The main study (N>>30) will not have this issue.")
    }
    lines <- c(lines, "")
  }

  # Indicator decisions
  lines <- c(lines, "## Indicator Review")
  if (nrow(removed_log) > 0) {
    lines <- c(lines, sprintf("Items auto-removed: %d", nrow(removed_log)))
    for (i in seq_len(nrow(removed_log))) {
      lines <- c(lines, sprintf("  - %s.%s (loading=%.3f, iter=%d)",
                                  removed_log$Construct[i], removed_log$Indicator[i],
                                  removed_log$Loading[i], removed_log$Iteration[i]))
    }
  } else {
    lines <- c(lines, "No items auto-removed (auto_drop = DISABLED for Type A pilot).")
  }
  lines <- c(lines, "")

  # Active constructs
  lines <- c(lines, "## Final Instrument")
  for (cc in cfg$constructs) {
    lines <- c(lines, sprintf("- %s [%s]: %d indicators — %s",
                                cc$name, cc$measurement_type,
                                length(cc$indicators),
                                paste(cc$indicators, collapse = ", ")))
  }
  lines <- c(lines, "")
  lines <- c(lines, "## Bootstrap")
  lines <- c(lines, sprintf("- bootstrap_samples: %d", cfg$project$bootstrap_samples))
  if (cfg$project$bootstrap_samples == 0) {
    lines <- c(lines, "- HTMT CI and formative weight significance: NOT AVAILABLE")
    lines <- c(lines, "- Point estimates (loadings, weights, VIF, reliability) are reported.")
  }
  lines <- c(lines, "")
  lines <- c(lines, "## Instrument Lock")
  lines <- c(lines, "- instrument_locked.json = frozen snapshot (no statistical trimming)")
  lines <- c(lines, "- Pilot did NOT remove or add any indicators.")
  lines <- c(lines, "- Statistical refinement (if needed) is performed in MAIN stage.")
  lines <- c(lines, "")
  lines <- c(lines, "## CMV")
  lines <- c(lines, "- CMV inference not performed in pilot (insufficient conditions for full collinearity VIF).")
  lines <- c(lines, "- CMV/CMB is evaluated in the MAIN study with full sample.")
  lines <- c(lines, "")
  lines <- c(lines, "## Interpretation Guide (Type A Pilot)")
  lines <- c(lines, "This is a Type A feasibility pilot (~30 respondents).")
  lines <- c(lines, "Purpose: spot catastrophic issues (process, data quality, instrument).")
  lines <- c(lines, "")
  lines <- c(lines, "Red flags to investigate:")
  lines <- c(lines, "- Reflective loadings < 0.30 (catastrophic)")
  lines <- c(lines, "- Outer VIF > 10 (severe multicollinearity)")
  lines <- c(lines, "- Reliability (CR, rho_A) < 0.60")
  lines <- c(lines, "- HTMT > 0.95 (near-total lack of discriminant validity)")
  lines <- c(lines, "")
  lines <- c(lines, "Do NOT use p-values or bootstrap to remove indicators.")
  lines <- c(lines, "Do NOT interpret results as final research conclusions.")

  md_path <- file.path(output_dir, "pilot_summary.md")
  writeLines(lines, md_path)
  log_msg(log_info, sprintf("Pilot summary: %s", md_path))
}

# ==============================================================================
# PRE-PLS FEASIBILITY CHECK — early abort for pilot if data is singular
# ==============================================================================
#' Check for data issues that would make PLS estimation impossible.
#' Reads analysis-ready data and checks for zero-variance, perfect correlations,
#' and degenerate construct blocks.
#' @param data_final analysis-ready data
#' @param cfg config list
#' @param log_info log object
#' @return list(feasible = TRUE/FALSE, red_flags = character vector)
check_pls_feasibility <- function(data_final, cfg, log_info) {
  log_msg(log_info, "\n--- Pre-PLS Feasibility Check ---")

  ind_cols   <- intersect(cfg$all_indicators, names(data_final))
  red_flags  <- character()

  # 1. Zero-variance (SD = 0) items
  sds <- sapply(ind_cols, function(v) sd(data_final[[v]], na.rm = TRUE))
  zero_sd <- names(sds)[!is.na(sds) & sds == 0]
  if (length(zero_sd) > 0) {
    msg <- sprintf("Zero-variance items (SD=0): %s", paste(zero_sd, collapse = ", "))
    log_warn(log_info, msg)
    red_flags <- c(red_flags, msg)
  }

  # 2. Perfect correlations (|r| >= 1.000)
  cor_mat <- cor(data_final[, ind_cols, drop = FALSE], use = "pairwise.complete.obs")
  diag(cor_mat) <- NA
  perfect_pairs <- which(abs(cor_mat) >= 1.000 - 1e-10, arr.ind = TRUE)
  if (nrow(perfect_pairs) > 0) {
    seen <- character()
    for (i in seq_len(nrow(perfect_pairs))) {
      r <- perfect_pairs[i, 1]; c_ <- perfect_pairs[i, 2]
      if (r < c_) {
        pair_str <- sprintf("%s ~ %s (r=%.4f)", ind_cols[r], ind_cols[c_], cor_mat[r, c_])
        seen <- c(seen, pair_str)
      }
    }
    if (length(seen) > 0) {
      msg <- sprintf("Perfect correlations (|r|>=1.000): %s", paste(seen, collapse = "; "))
      log_warn(log_info, msg)
      red_flags <- c(red_flags, msg)
    }
  }

  # 3. Construct blocks with identical response patterns across all items
  for (cc in cfg$constructs) {
    block_cols <- intersect(cc$indicators, ind_cols)
    if (length(block_cols) < 2) next
    block_data <- data_final[, block_cols, drop = FALSE]
    all_same <- all(sapply(block_cols[-1], function(col)
      identical(block_data[[col]], block_data[[block_cols[1]]])))
    if (all_same) {
      msg <- sprintf("Block %s: all %d items have identical values (degenerate)",
                     cc$name, length(block_cols))
      log_warn(log_info, msg)
      red_flags <- c(red_flags, msg)
    }
  }

  feasible <- length(red_flags) == 0

  if (feasible) {
    log_msg(log_info, "Pre-PLS feasibility: PASS — no data singularity issues detected")
  } else {
    log_warn(log_info, sprintf("Pre-PLS feasibility: %d red flag(s) — data matrix likely singular",
                                length(red_flags)))
  }

  list(feasible = feasible, red_flags = red_flags)
}

# ==============================================================================
# MAIN ENTRY — run_stage()
# ==============================================================================

#' Run PLS-SEM pipeline for a given stage (pilot or main)
#' @param stage_config_path Path to stage config YAML (e.g. "config/pilot.yml")
run_stage <- function(stage_config_path) {

  check_required_packages()
  source_modules()

  suppressPackageStartupMessages(library(jsonlite))

  # --------------------------------------------------------------------------
  # 1. Load configs: base + stage overlay
  # --------------------------------------------------------------------------
  if (!file.exists(stage_config_path)) {
    stop("Stage config not found: ", stage_config_path)
  }
  stage_cfg <- yaml::read_yaml(stage_config_path)
  stage     <- stage_cfg$stage  # "pilot" or "main"

  cat("========================================\n")
  cat(sprintf("CCA-SEM Pipeline — %s\n", toupper(stage)))
  cat("========================================\n")

  # Load base config (always from 00_meta)
  cfg <- load_config("00_meta/analysis_config.yaml")

  # Override base config with stage-specific values
  if (!is.null(stage_cfg$data_input)) {
    cfg$paths$data_file      <- stage_cfg$data_input$data_file
    cfg$paths$data_sheet     <- stage_cfg$data_input$data_sheet
    cfg$paths$codebook_sheet <- stage_cfg$data_input$codebook_sheet
  }
  if (!is.null(stage_cfg$seed))              cfg$project$seed              <- stage_cfg$seed
  if (!is.null(stage_cfg$bootstrap_samples)) cfg$project$bootstrap_samples <- stage_cfg$bootstrap_samples
  if (!is.null(stage_cfg$confidence_level))  cfg$project$confidence_level  <- stage_cfg$confidence_level
  if (!is.null(stage_cfg$plspredict))        cfg$plspredict                <- stage_cfg$plspredict

  validate_config(cfg)

  # Stage-specific directories
  output_dir <- stage_cfg$output_dir  # "10_report/pilot" or "10_report/main"
  log_prefix <- stage_cfg$log_prefix  # "pilot" or "main"
  modules    <- stage_cfg$modules     # enable flags

  ensure_output_dirs()
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  set.seed(cfg$project$seed)

  # --------------------------------------------------------------------------
  # 2. Initialize log
  # --------------------------------------------------------------------------
  log_info <- init_pipeline_log("logs", prefix = log_prefix)

  log_msg(log_info, sprintf("Stage: %s", toupper(stage)))
  df <- stage_cfg$date_filter
  if (!is.null(df)) {
    log_msg(log_info, sprintf("Date filter: %s %s..%s",
                             df$column,
                             if (!is.null(df$start)) df$start else "*",
                             if (!is.null(df$end))   df$end   else "*"))
  }
  if (!is.null(stage_cfg$exclude_pilot_ids)) {
    log_msg(log_info, sprintf("Exclude pilot IDs from: %s", stage_cfg$exclude_pilot_ids))
  }
  if (!is.null(stage_cfg$pilot_min_n)) {
    log_msg(log_info, sprintf("Pilot min_n override: %d", stage_cfg$pilot_min_n))
  }
  if (!is.null(stage_cfg$stop_after_phase)) {
    log_msg(log_info, sprintf("Stop after phase: %d", stage_cfg$stop_after_phase))
  }
  log_msg(log_info, paste("Project:", cfg$project$name))
  log_msg(log_info, paste("Config:", stage_config_path))
  log_msg(log_info, paste("Seed:", cfg$project$seed))
  log_msg(log_info, paste("Bootstrap:", cfg$project$bootstrap_samples))
  log_msg(log_info, paste("Constructs:", length(cfg$constructs)))
  log_msg(log_info, paste("  Reflective:", length(cfg$reflective_constructs)))
  log_msg(log_info, paste("  Formative:", length(cfg$formative_constructs)))
  log_msg(log_info, paste("Structural paths:", length(cfg$structural_paths)))
  log_msg(log_info, paste("Output dir:", output_dir))

  # Audit trail
  write_audit_trail(stage_config_path, output_dir, log_info)

  # --------------------------------------------------------------------------
  # 2b. Load Inference Policy
  # --------------------------------------------------------------------------
  policy_path <- "config/inference_policy.yml"
  if (file.exists(policy_path)) {
    inference_policy <- load_inference_policy(policy_path)
    log_msg(log_info, sprintf("Inference policy loaded: primary=%s, alpha=%.2f, CI=%s %d%% %s",
                               inference_policy$primary_rule,
                               inference_policy$alpha,
                               inference_policy$ci_method,
                               as.integer(inference_policy$ci_level * 100),
                               ifelse(inference_policy$two_tailed, "two-tailed", "one-tailed")))
    snapshot_policy(policy_path, output_dir)
  } else {
    log_warn(log_info, "Inference policy not found — using built-in defaults")
    inference_policy <- list(
      primary_rule = "ci_excludes_zero", alpha = 0.05,
      ci_method = "percentile", ci_level = 0.95, two_tailed = TRUE,
      decimals = list(estimate = 3, t_stat = 3, p_value = 4, ci = 3,
                      vif = 3, r_squared = 3, vaf = 3)
    )
  }

  # --------------------------------------------------------------------------
  # 3. INSTRUMENT LOCK — read if main stage
  # --------------------------------------------------------------------------
  instrument_locked  <- FALSE
  locked_indicators  <- NULL
  removed_log_frozen <- data.frame(Iteration = integer(), Construct = character(),
                                    Indicator = character(), Loading = numeric(),
                                    stringsAsFactors = FALSE)

  if (stage == "main") {
    lock_path <- stage_cfg$instrument_lock$pilot_lock_path
    if (is.null(lock_path)) lock_path <- "10_report/pilot/instrument_locked.json"

    lock_result <- read_instrument_lock(lock_path, cfg, log_info)
    cfg <- lock_result$cfg
    locked_indicators <- lock_result$active_indicators
    instrument_locked <- TRUE

    # Apply freeze rules from main.yml
    freeze_rules <- stage_cfg$freeze_rules
    if (!is.null(freeze_rules) && length(freeze_rules) > 0) {
      locked_indicators <- apply_freeze_rules(locked_indicators, freeze_rules, log_info)
      # Re-sync cfg
      for (i in seq_along(cfg$constructs)) {
        cn <- cfg$constructs[[i]]$name
        if (cn %in% names(locked_indicators))
          cfg$constructs[[i]]$indicators <- locked_indicators[[cn]]
      }
      cfg$all_indicators <- unique(unlist(locked_indicators))
      cfg$reflective_constructs <- Filter(function(c) c$measurement_type == "reflective", cfg$constructs)
      cfg$formative_constructs  <- Filter(function(c) c$measurement_type == "formative",  cfg$constructs)
    }

    log_msg(log_info, "Instrument FROZEN — using locked indicators from pilot")
    if (!is.null(freeze_rules) && length(freeze_rules) > 0) {
      log_msg(log_info, sprintf("  Freeze rules applied: %d change(s) — see audit trail above",
                                 length(freeze_rules)))
    }
  }

  # ==========================================================================
  # PHASE 1: DATA PREPARATION
  # ==========================================================================
  log_msg(log_info, "\n===== PHASE 1: DATA PREPARATION =====")

  # Step 1: Import & Technical Validation
  data_tech <- import_and_validate(cfg, log_info)

  # --------------------------------------------------------------------------
  # SUBSET — date filter (pilot) + ID-based exclusion (main)
  # --------------------------------------------------------------------------
  date_filter <- stage_cfg$date_filter
  if (!is.null(date_filter)) {
    col_name  <- date_filter$column  # e.g. "Time"
    orig_n    <- nrow(data_tech)
    if (!col_name %in% names(data_tech)) {
      close_pipeline_log(log_info)
      stop(sprintf("date_filter.column '%s' not found in data", col_name))
    }
    ts <- as.Date(data_tech[[col_name]])
    keep <- rep(TRUE, orig_n)
    if (!is.null(date_filter$start)) keep <- keep & (ts >= as.Date(date_filter$start))
    if (!is.null(date_filter$end))   keep <- keep & (ts <= as.Date(date_filter$end))
    data_tech <- data_tech[keep, , drop = FALSE]
    log_msg(log_info, sprintf("Date filter applied (%s): %d -> %d rows  [%s .. %s]",
                               col_name, orig_n, nrow(data_tech),
                               if (!is.null(date_filter$start)) date_filter$start else "*",
                               if (!is.null(date_filter$end))   date_filter$end   else "*"))
  }

  # --- Save pilot IDs (pilot stage) ---
  id_col <- stage_cfg$id_column  # e.g. "Responder ID"
  if (stage == "pilot" && !is.null(id_col)) {
    if (!id_col %in% names(data_tech)) {
      log_warn(log_info, sprintf("id_column '%s' not found — cannot save pilot_ids.csv", id_col))
    } else {
      pilot_ids_path <- file.path(output_dir, "pilot_ids.csv")
      # Enrich: ID + Time + flag_duplicate + duplicate_group_id (audit trail)
      pilot_ids_df   <- data.frame(id = data_tech[[id_col]], stringsAsFactors = FALSE)
      names(pilot_ids_df) <- id_col
      # Add Time column if available
      time_col <- if (!is.null(stage_cfg$date_filter$column)) stage_cfg$date_filter$column else "Time"
      if (time_col %in% names(data_tech)) {
        pilot_ids_df[[time_col]] <- as.character(data_tech[[time_col]])
      }
      # Add flag_duplicate if available
      if ("flag_duplicate" %in% names(data_tech)) {
        pilot_ids_df$flag_duplicate <- data_tech$flag_duplicate
      }
      # Add duplicate_group_id: group identical response patterns
      ind_cols_avail <- intersect(cfg$all_indicators, names(data_tech))
      if (length(ind_cols_avail) > 0) {
        pattern_str <- apply(data_tech[, ind_cols_avail, drop = FALSE], 1, function(row) {
          paste(row, collapse = "|")
        })
        # Assign group IDs to unique patterns
        hash_ids <- match(pattern_str, unique(pattern_str))
        # Only label groups with >1 member (actual duplicates)
        group_counts <- table(hash_ids)
        dup_groups <- as.integer(names(group_counts[group_counts > 1]))
        pilot_ids_df$duplicate_group_id <- ifelse(hash_ids %in% dup_groups, hash_ids, NA_integer_)
      }
      write.csv(pilot_ids_df, pilot_ids_path, row.names = FALSE)
      log_msg(log_info, sprintf("Pilot IDs saved: %s (%d respondents, cols: %s)",
                                 pilot_ids_path, nrow(pilot_ids_df),
                                 paste(names(pilot_ids_df), collapse = ", ")))
    }
  }

  # --- Exclude pilot IDs (main stage) ---
  exclude_path <- stage_cfg$exclude_pilot_ids
  if (!is.null(exclude_path) && file.exists(exclude_path)) {
    excl_col <- if (!is.null(id_col)) id_col else "Responder ID"
    if (!excl_col %in% names(data_tech)) {
      log_warn(log_info, sprintf("id_column '%s' not in data — skipping pilot ID exclusion", excl_col))
    } else {
      pilot_ids <- read.csv(exclude_path, stringsAsFactors = FALSE)[[1]]
      orig_n <- nrow(data_tech)
      data_tech <- data_tech[!data_tech[[excl_col]] %in% pilot_ids, , drop = FALSE]
      n_excluded <- orig_n - nrow(data_tech)
      log_msg(log_info, sprintf("Pilot ID exclusion: %d -> %d rows (excluded %d pilot respondents from %s)",
                                 orig_n, nrow(data_tech), n_excluded, exclude_path))
    }
  } else if (!is.null(exclude_path) && !file.exists(exclude_path)) {
    close_pipeline_log(log_info)
    stop(sprintf(
      "\n============================================================\n",
      "  PILOT IDS FILE NOT FOUND: %s\n\n",
      "  You must run the PILOT stage first:\n",
      "    source('run_pilot.R')\n",
      "============================================================\n", exclude_path))
  }

  # SAFETY GATE — sample size sanity check
  if (toupper(stage) == "MAIN" && nrow(data_tech) <= 150) {
    close_pipeline_log(log_info)
    stop(
      "\n============================================================\n",
      "  SAFETY STOP: MAIN stage has only ", nrow(data_tech), " rows (<= 150).\n",
      "  This looks like pilot data. Use run_pilot.R for pilot analysis.\n",
      "  MAIN requires the full dataset (typically > 200 observations).\n",
      "============================================================\n"
    )
  }

  # Step 2: Behavioral QC
  data_qc <- run_behavioral_qc(data_tech, cfg, log_info,
                                pilot_min_n = stage_cfg$pilot_min_n,
                                stage = stage)

  # Step 3: Missing Data
  data_final <- handle_missing(data_qc, cfg, log_info)
  saveRDS(data_final, "02_clean/analysis_ready.rds")

  # Step 4: Descriptive Statistics + Demographics + Control Variable Coding
  data_final <- run_descriptives(data_final, cfg, log_info, stage = stage)
  saveRDS(data_final, "02_clean/analysis_ready.rds")

  log_msg(log_info, sprintf("\nPhase 1 complete — Analysis-ready dataset: %d rows, %d cols",
                             nrow(data_final), ncol(data_final)))

  # ==========================================================================
  # CONTROL VARIABLES — availability check (Model B built later in robustness)
  # ==========================================================================
  # Control variables (Gen_Dummy, Exp_Ord, …) were coded in Step 4b.
  # They are NOT part of Model A (core/theoretical model).
  # Model B (with controls) is estimated in 13_robustness.R Section E
  # to confirm substantive paths are robust after adding controls.
  # ==========================================================================
  ctrl_vars_cfg <- cfg$control_vars
  ctrl_col_names <- character()
  if (!is.null(ctrl_vars_cfg) && length(ctrl_vars_cfg) > 0) {
    ctrl_col_names <- sapply(ctrl_vars_cfg, function(cv) cv$target_column)
    ctrl_col_names <- intersect(ctrl_col_names, names(data_final))
  }
  cfg$control_construct_names <- ctrl_col_names   # stored for Model B (robustness)

  if (length(ctrl_col_names) > 0) {
    log_msg(log_info, sprintf("\nControl variables available for Model B (robustness): %s",
                               paste(ctrl_col_names, collapse = ", ")))
  }

  # ==========================================================================
  # PRE-PLS FEASIBILITY CHECK (pilot: early exit if singular; main: warn only)
  # ==========================================================================
  pls_feasibility <- check_pls_feasibility(data_final, cfg, log_info)

  if (stage == "pilot") {
    # Log as a dedicated gate: PILOT_FEASIBILITY
    if (pls_feasibility$feasible) {
      log_info <- log_gate(log_info, "Pilot Feasibility", TRUE,
                           "Pre-PLS check PASS — data matrix is non-singular")
    } else {
      log_info <- log_gate(log_info, "Pilot Feasibility", TRUE,
                           sprintf("WARN — %d red flag(s) detected; Phase 2 will be skipped (planned early exit)",
                                   length(pls_feasibility$red_flags)))
    }
  }

  if (!pls_feasibility$feasible && stage == "pilot") {
    log_msg(log_info, "")
    log_msg(log_info, "=== PILOT PLANNED EARLY EXIT — Data singularity detected ===")
    log_msg(log_info, "Phase 1 completed successfully. Phase 2 SKIPPED (pre-PLS red flags).")
    log_msg(log_info, "Red flags found:")
    for (rf in pls_feasibility$red_flags) {
      log_msg(log_info, sprintf("  - %s", rf))
    }
    log_msg(log_info, "")
    log_msg(log_info, "This is expected for a Type A feasibility pilot with small N.")
    log_msg(log_info, "Data singularity does NOT indicate research design problems.")
    log_msg(log_info, "The main study (N>>30) will not have this issue.")

    # Build active_indicators from cfg (no optimizer ran)
    active_indicators <- list()
    for (cc in cfg$constructs) {
      active_indicators[[cc$name]] <- cc$indicators
    }
    removed_log <- data.frame(Iteration = integer(), Construct = character(),
                               Indicator = character(), Loading = numeric(),
                               stringsAsFactors = FALSE)

    # Save instrument lock (frozen snapshot)
    if (isTRUE(stage_cfg$instrument_lock$create_on_completion)) {
      write_instrument_lock(cfg, active_indicators, removed_log, output_dir, log_info)
    }
    write_pilot_summary(data_tech, data_qc, data_final, removed_log, cfg, log_info, output_dir,
                         pls_feasibility = pls_feasibility)

    log_msg(log_info, "\n========================================")
    log_msg(log_info, sprintf("PIPELINE SUMMARY (%s — Type A Feasibility)", toupper(stage)))
    log_msg(log_info, "========================================")
    log_msg(log_info, sprintf("Final sample: %d observations", nrow(data_final)))
    log_msg(log_info, "Phase 1: COMPLETE")
    log_msg(log_info, "Phase 2: SKIPPED (data singularity — planned early exit)")
    log_msg(log_info, sprintf("Red flags: %d", length(pls_feasibility$red_flags)))
    log_msg(log_info, "========================================")

    close_pipeline_log(log_info)

    cat(sprintf("\n%s pipeline: Phase 1 OK; Phase 2 skipped (data singularity, see log).\n", toupper(stage)))
    cat(sprintf("Results in: %s\n", output_dir))
    cat("Log file:", log_info$path, "\n")

    return(invisible(list(
      stage = stage, cfg = cfg, pls_model = NULL, boot_model = NULL,
      active_indicators = active_indicators,
      pls_feasibility = pls_feasibility,
      log_path = log_info$path, output_dir = output_dir
    )))
  }

  if (!pls_feasibility$feasible && stage == "main") {
    log_warn(log_info, "Pre-PLS red flags detected in MAIN — PLS may fail. Proceeding anyway.")
  }

  # ==========================================================================
  # PHASE 2: CMV & MEASUREMENT MODEL
  # ==========================================================================
  log_msg(log_info, "\n===== PHASE 2: CMV & MEASUREMENT MODEL =====")

  suppressPackageStartupMessages(library(seminr))

  # Step 5: CMV/CMB
  if (isTRUE(modules$enable_cmv)) {
    cmv_pass <- assess_cmv(data_final, cfg, log_info, stage = stage)
  }

  # --- Build active_indicators ---
  if (instrument_locked && !is.null(locked_indicators)) {
    # Main: start from locked indicators (pilot snapshot)
    active_indicators <- locked_indicators
    log_msg(log_info, "Using instrument-locked indicators from pilot as starting point")
  } else {
    # Pilot or legacy: deep-copy from cfg
    active_indicators <- list()
    for (cc in cfg$constructs) {
      active_indicators[[cc$name]] <- cc$indicators
    }
  }

  # Helper: build measurement model from active_indicators
  build_mm <- function(cfg, active_inds) {
    mm_items <- list()
    for (cc in cfg$constructs) {
      inds <- active_inds[[cc$name]]
      if (length(inds) == 0) next
      if (cc$measurement_type == "reflective") {
        mm_items[[length(mm_items) + 1]] <- reflective(cc$name, inds)
      } else {
        mm_items[[length(mm_items) + 1]] <- composite(cc$name, inds, weights = mode_B)
      }
    }
    do.call(constructs, mm_items)
  }

  # Structural model
  sm_list <- lapply(cfg$structural_paths, function(p) paths(from = p$from, to = p$to))
  sm <- do.call(relationships, sm_list)

  # =========================================================================
  # ITERATIVE OPTIMIZER (pilot) or SKIP (main with frozen instrument)
  # =========================================================================
  optimizer_cfg <- stage_cfg$optimizer
  auto_drop_enabled     <- isTRUE(optimizer_cfg$auto_drop_enabled)
  LOADING_DROP_THRESHOLD <- if (!is.null(optimizer_cfg$loading_drop_threshold)) optimizer_cfg$loading_drop_threshold else 0.40
  MAX_ITER <- if (!is.null(optimizer_cfg$max_iterations)) optimizer_cfg$max_iterations else 5

  removed_log <- data.frame(Iteration = integer(), Construct = character(),
                             Indicator = character(), Loading = numeric(),
                             stringsAsFactors = FALSE)

  if (auto_drop_enabled) {
    log_msg(log_info, "\n--- Measurement Model Optimizer ---")
    log_msg(log_info, sprintf("Strategy: auto-remove reflective indicators with loading < %.2f",
                               LOADING_DROP_THRESHOLD))
    log_msg(log_info, sprintf("Max iterations: %d", MAX_ITER))
  } else {
    n_freeze <- length(stage_cfg$freeze_rules)
    if (n_freeze > 0) {
      log_msg(log_info, sprintf("\n--- Measurement Model Optimizer: DISABLED — %d freeze rule(s) applied above ---", n_freeze))
    } else {
      log_msg(log_info, "\n--- Measurement Model Optimizer: DISABLED (instrument frozen, no changes) ---")
    }
    MAX_ITER <- 1  # single pass, no removal
  }

  pls_model <- NULL

  for (iter in seq_len(MAX_ITER)) {
    if (auto_drop_enabled) {
      log_msg(log_info, sprintf("\n--- Optimizer Iteration %d/%d ---", iter, MAX_ITER))
    }

    current_inds   <- unique(unlist(active_indicators))
    current_cols   <- intersect(current_inds, names(data_final))
    pls_data_iter  <- as.data.frame(data_final[, current_cols, drop = FALSE])

    mm <- build_mm(cfg, active_indicators)

    pls_model <- tryCatch({
      estimate_pls(data = pls_data_iter, measurement_model = mm, structural_model = sm)
    }, error = function(e) {
      log_error(log_info, paste("PLS estimation failed at iteration", iter, ":", e$message))
      NULL
    })

    if (is.null(pls_model)) {
      log_error(log_info, "Cannot continue — PLS estimation failed")
      break
    }
    log_msg(log_info, "PLS model estimated successfully")

    if (!auto_drop_enabled) break  # frozen — no removal loop

    # Check reflective outer loadings
    model_summ <- summary(pls_model)
    loadings_mat <- model_summ$loadings
    refl_names <- sapply(cfg$reflective_constructs, function(c) c$name)

    bad_items <- data.frame(Construct = character(), Indicator = character(),
                            Loading = numeric(), stringsAsFactors = FALSE)

    for (rn in refl_names) {
      inds <- active_indicators[[rn]]
      for (ind in inds) {
        if (ind %in% rownames(loadings_mat) && rn %in% colnames(loadings_mat)) {
          ld <- loadings_mat[ind, rn]
          if (!is.na(ld) && abs(ld) < LOADING_DROP_THRESHOLD) {
            bad_items <- rbind(bad_items, data.frame(
              Construct = rn, Indicator = ind, Loading = round(ld, 4),
              stringsAsFactors = FALSE))
          }
        }
      }
    }

    if (nrow(bad_items) == 0) {
      log_msg(log_info, "All reflective loadings >= 0.40 — optimizer converged")
      break
    }

    for (i in seq_len(nrow(bad_items))) {
      cc_name  <- bad_items$Construct[i]
      ind_name <- bad_items$Indicator[i]
      ld_val   <- bad_items$Loading[i]

      if (length(active_indicators[[cc_name]]) <= 2) {
        log_warn(log_info, sprintf(
          "  [SKIP] %s (Loading=%.3f) — cannot remove, %s would have < 2 indicators",
          ind_name, ld_val, cc_name))
        next
      }

      log_msg(log_info, sprintf(
        "  [AUTO-FIX] Removing indicator %s (Loading = %.3f < %.2f). Re-estimating...",
        ind_name, ld_val, LOADING_DROP_THRESHOLD))

      active_indicators[[cc_name]] <- setdiff(active_indicators[[cc_name]], ind_name)
      removed_log <- rbind(removed_log, data.frame(
        Iteration = iter, Construct = cc_name, Indicator = ind_name,
        Loading = ld_val, stringsAsFactors = FALSE))
    }

    if (iter == MAX_ITER) {
      log_warn(log_info, "Max iterations reached — proceeding with current model")
    }
  }

  # Log removal summary
  if (nrow(removed_log) > 0) {
    log_msg(log_info, sprintf("\n--- Auto-removed %d indicator(s) across %d iteration(s) ---",
                               nrow(removed_log), max(removed_log$Iteration)))
    for (i in seq_len(nrow(removed_log))) {
      log_msg(log_info, sprintf("  Iter %d: %s.%s (Loading=%.3f)",
                                 removed_log$Iteration[i], removed_log$Construct[i],
                                 removed_log$Indicator[i], removed_log$Loading[i]))
    }
    dir.create("05_measurement/reflective", showWarnings = FALSE, recursive = TRUE)
    write.csv(removed_log, "05_measurement/reflective/auto_removed_indicators.csv",
              row.names = FALSE)
  } else {
    log_msg(log_info, "No indicators auto-removed — model is clean")
  }

  # --- PLS estimation failure: graceful exit ---
  if (is.null(pls_model)) {
    log_warn(log_info, "PLS estimation failed — cannot proceed with measurement assessment")
    log_msg(log_info, "Possible causes: singular data matrix, perfect collinearity among indicators")
    log_msg(log_info, "  Check for r=1.000 correlations in descriptive stats")

    if (stage == "pilot") {
      log_msg(log_info, "")
      log_msg(log_info, "=== PILOT EARLY EXIT — PLS estimation failed (singular data matrix) ===")
      log_msg(log_info, "Phase 1 completed successfully. PLS could not be estimated.")
      log_msg(log_info, "Possible causes: perfect collinearity among indicators, zero-variance items,")
      log_msg(log_info, "  or identical response patterns creating a singular data matrix.")
      log_msg(log_info, "  Small N makes these issues more likely but is not the root cause.")

      # Still save pilot IDs and instrument lock (no removals)
      if (isTRUE(stage_cfg$instrument_lock$create_on_completion)) {
        write_instrument_lock(cfg, active_indicators, removed_log, output_dir, log_info)
      }
      write_pilot_summary(data_tech, data_qc, data_final, removed_log, cfg, log_info, output_dir)

      log_msg(log_info, "\n========================================")
      log_msg(log_info, sprintf("PIPELINE SUMMARY (%s — Type A Feasibility)", toupper(stage)))
      log_msg(log_info, "========================================")
      log_msg(log_info, sprintf("Final sample: %d observations", nrow(data_final)))
      log_msg(log_info, "PLS estimation: FAILED (singular data matrix / perfect collinearity)")
      log_msg(log_info, "Measurement assessment: SKIPPED")
      log_msg(log_info, "========================================")

      close_pipeline_log(log_info)

      cat(sprintf("\n\u26a0\ufe0f %s pipeline: Phase 1 OK, PLS failed (see log).\n", toupper(stage)))
      cat(sprintf("Results in: %s\n", output_dir))
      cat("Log file:", log_info$path, "\n")

      return(invisible(list(
        stage = stage, cfg = cfg, pls_model = NULL, boot_model = NULL,
        active_indicators = active_indicators,
        log_path = log_info$path, output_dir = output_dir
      )))
    } else {
      close_pipeline_log(log_info)
      stop("PLS estimation failed in MAIN stage — cannot continue. Check data quality.")
    }
  }

  # Log final indicator counts
  log_msg(log_info, "\n--- Final Indicator Counts ---")
  for (cc in cfg$constructs) {
    n_orig <- length(cc$indicators)
    n_now  <- length(active_indicators[[cc$name]])
    # Original from config might be shorter if instrument locked
    suffix <- ifelse(n_orig == n_now, "", sprintf(" (removed %d)", n_orig - n_now))
    log_msg(log_info, sprintf("  %s [%s]: %d/%d%s",
                               cc$name, cc$measurement_type, n_now, n_orig, suffix))
  }

  # Sync cfg with post-optimizer active_indicators
  for (i in seq_along(cfg$constructs)) {
    cn <- cfg$constructs[[i]]$name
    cfg$constructs[[i]]$indicators <- active_indicators[[cn]]
  }
  cfg$all_indicators <- unique(unlist(active_indicators))
  cfg$reflective_constructs <- Filter(function(c) c$measurement_type == "reflective", cfg$constructs)
  cfg$formative_constructs  <- Filter(function(c) c$measurement_type == "formative",  cfg$constructs)

  # ==========================================================================
  # RE-EXPORT DESCRIPTIVE STATS — only indicators retained in the final model
  # ==========================================================================
  if (nrow(removed_log) > 0) {
    log_msg(log_info, "\n--- Re-exporting descriptive stats (final-model indicators only) ---")
    final_ind_cols <- intersect(cfg$all_indicators, names(data_final))
    desc_stats_final <- data.frame(
      Variable  = final_ind_cols,
      N         = sapply(final_ind_cols, function(v) sum(!is.na(data_final[[v]]))),
      Mean      = round(sapply(final_ind_cols, function(v) mean(data_final[[v]], na.rm = TRUE)), 3),
      SD        = round(sapply(final_ind_cols, function(v) sd(data_final[[v]], na.rm = TRUE)), 3),
      Min       = sapply(final_ind_cols, function(v) min(data_final[[v]], na.rm = TRUE)),
      Max       = sapply(final_ind_cols, function(v) max(data_final[[v]], na.rm = TRUE)),
      Skewness  = round(sapply(final_ind_cols, function(v) {
        x <- data_final[[v]][!is.na(data_final[[v]])]
        n <- length(x)
        (n / ((n-1)*(n-2))) * sum(((x - mean(x)) / sd(x))^3)
      }), 3),
      Kurtosis  = round(sapply(final_ind_cols, function(v) {
        x <- data_final[[v]][!is.na(data_final[[v]])]
        m4 <- mean((x - mean(x))^4)
        s4 <- sd(x)^4
        m4/s4 - 3
      }), 3),
      stringsAsFactors = FALSE, row.names = NULL
    )
    write.csv(desc_stats_final, "04_descriptives/descriptive_stats.csv", row.names = FALSE)
    dropped_names <- paste(removed_log$Indicator, collapse = ", ")
    log_msg(log_info, sprintf("  Descriptive stats updated: %d indicators (removed: %s)",
                               nrow(desc_stats_final), dropped_names))

    # Also update correlation matrix to match final indicators
    cor_matrix_final <- round(cor(data_final[, final_ind_cols, drop = FALSE],
                                  use = "pairwise.complete.obs"), 3)
    write.csv(cor_matrix_final, "04_descriptives/correlation_matrix.csv")
    log_msg(log_info, sprintf("  Correlation matrix updated: %dx%d",
                               nrow(cor_matrix_final), ncol(cor_matrix_final)))
  }

  # ==========================================================================
  # BOOTSTRAP (conditional — skip when bootstrap_samples = 0)
  # ==========================================================================
  boot_model <- NULL
  if (cfg$project$bootstrap_samples > 0) {
    log_msg(log_info, sprintf("\nBootstrapping (%d samples)...", cfg$project$bootstrap_samples))
    boot_model <- tryCatch({
      bm <- bootstrap_model(
        seminr_model = pls_model,
        nboot = cfg$project$bootstrap_samples,
        seed = cfg$project$seed
      )
      log_msg(log_info, "Bootstrap complete")
      bm
    }, error = function(e) {
      log_warn(log_info, sprintf("Bootstrap failed: %s — proceeding without bootstrap", e$message))
      NULL
    })
  } else {
    log_msg(log_info, "\nBootstrap SKIPPED (bootstrap_samples = 0 — pilot feasibility mode)")
  }

  saveRDS(pls_model, "06_structural/pls_model.rds")
  if (!is.null(boot_model)) saveRDS(boot_model, "06_structural/boot_model.rds")
  saveRDS(active_indicators, "06_structural/active_indicators.rds")

  # Step 6: Reflective Measurement
  refl_results <- list(pass = TRUE)
  if (isTRUE(modules$enable_reflective)) {
    refl_results <- assess_reflective(pls_model, boot_model, cfg, log_info,
                                       policy = inference_policy)
  }

  # Step 7: Formative Measurement
  form_results <- list(pass = TRUE)
  if (isTRUE(modules$enable_formative)) {
    form_results <- assess_formative(pls_model, boot_model, cfg, log_info,
                                       policy = inference_policy)
  }

  # Step 8: HOC
  if (isTRUE(modules$enable_hoc)) {
    hoc_results <- assess_hoc(data_final, cfg, log_info)
  }

  # Measurement gate
  measurement_pass <- refl_results$pass && form_results$pass
  log_msg(log_info, "")
  log_msg(log_info, sprintf("=== MEASUREMENT MODEL GATE: %s ===",
                             ifelse(measurement_pass, "PASS \u2705", "FAIL \u274c")))

  if (!measurement_pass) {
    log_warn(log_info, "Measurement model NOT passed — structural results should be interpreted with caution")
  }

  # ==========================================================================
  # PILOT EARLY STOP — stop_after_phase = 2
  # ==========================================================================
  stop_after <- stage_cfg$stop_after_phase
  if (!is.null(stop_after) && stop_after <= 2) {
    log_msg(log_info, "")
    log_msg(log_info, "=== PILOT stop_after_phase=2 reached — structural/hypotheses SKIPPED ===")

    # Write instrument lock
    if (isTRUE(stage_cfg$instrument_lock$create_on_completion)) {
      write_instrument_lock(cfg, active_indicators, removed_log, output_dir, log_info)
    }

    # Write pilot summary
    write_pilot_summary(data_tech, data_qc, data_final, removed_log, cfg, log_info, output_dir)

    # Pipeline summary (pilot)
    log_msg(log_info, "\n========================================")
    log_msg(log_info, sprintf("PIPELINE SUMMARY (%s — Type A Feasibility)", toupper(stage)))
    log_msg(log_info, "========================================")
    log_msg(log_info, sprintf("Date filter: %s",
                               if (!is.null(date_filter)) paste0(date_filter$column, " ",
                                 if (!is.null(date_filter$start)) paste0(">=", date_filter$start) else "",
                                 if (!is.null(date_filter$start) && !is.null(date_filter$end)) " & " else "",
                                 if (!is.null(date_filter$end)) paste0("<=", date_filter$end) else ""
                               ) else "none"))
    log_msg(log_info, sprintf("Pilot IDs saved: %s", file.path(output_dir, "pilot_ids.csv")))
    log_msg(log_info, sprintf("Final sample: %d observations", nrow(data_final)))
    log_msg(log_info, sprintf("Constructs: %d total (%d reflective, %d formative)",
                               length(cfg$constructs),
                               length(cfg$reflective_constructs),
                               length(cfg$formative_constructs)))
    log_msg(log_info, sprintf("Bootstrap: %s",
                               ifelse(cfg$project$bootstrap_samples > 0,
                                      sprintf("%d samples", cfg$project$bootstrap_samples),
                                      "DISABLED (feasibility mode)")))
    log_msg(log_info, sprintf("Auto-drop: %s",
                               ifelse(isTRUE(stage_cfg$optimizer$auto_drop_enabled),
                                      "ENABLED", "DISABLED")))
    log_msg(log_info, sprintf("Measurement gate: %s", ifelse(measurement_pass, "PASS", "FAIL")))
    log_msg(log_info, "Structural: SKIPPED (pilot stop_after_phase=2)")
    log_msg(log_info, sprintf("Instrument locked: %s",
                               ifelse(isTRUE(stage_cfg$instrument_lock$create_on_completion),
                                      "CREATED", "NOT requested")))
    log_msg(log_info, "========================================")

    close_pipeline_log(log_info)

    cat(sprintf("\n\u2705 %s pipeline complete (Phase 1-2 only)!\n", toupper(stage)))
    cat(sprintf("Results in: %s\n", output_dir))
    cat("Log file:", log_info$path, "\n")

    return(invisible(list(
      stage             = stage,
      cfg               = cfg,
      pls_model         = pls_model,
      boot_model        = boot_model,
      active_indicators = active_indicators,
      log_path          = log_info$path,
      output_dir        = output_dir
    )))
  }

  # ==========================================================================
  # PHASE 3: STRUCTURAL MODEL & HYPOTHESES
  # ==========================================================================
  log_msg(log_info, "\n===== PHASE 3: STRUCTURAL MODEL & HYPOTHESES =====")

  struct_results <- list(pass = TRUE)
  if (isTRUE(modules$enable_structural)) {
    struct_results <- assess_structural(pls_model, boot_model, cfg, log_info,
                                          policy = inference_policy)
  }

  predict_results <- list(pass = TRUE)
  if (isTRUE(modules$enable_plspredict)) {
    predict_results <- run_plspredict(pls_model, cfg, log_info)
  }

  if (isTRUE(modules$enable_mediation)) {
    med_results <- test_mediation(pls_model, boot_model, data_final, active_indicators,
                                   cfg, log_info, policy = inference_policy)
  }

  if (isTRUE(modules$enable_moderation)) {
    mod_results <- test_moderation(data_final, cfg, log_info, policy = inference_policy)
  }

  # ==========================================================================
  # PHASE 4: ROBUSTNESS & REPORTING
  # ==========================================================================
  log_msg(log_info, "\n===== PHASE 4: ROBUSTNESS & REPORTING =====")

  if (isTRUE(modules$enable_robustness)) {
    robust_results <- run_robustness(pls_model, data_final, cfg, log_info)

    # ---- Gaussian Copula Endogeneity Test ----
    tryCatch({
      # Build structural_spec from cfg (DV -> predictors + controls)
      gc_structural_spec <- list()
      endogenous_dvs <- unique(sapply(cfg$structural_paths, function(p) p$to))
      ctrl_names <- if (!is.null(cfg$control_vars))
        sapply(cfg$control_vars, function(cv) cv$target_column) else character()

      for (dv in endogenous_dvs) {
        preds <- sapply(
          Filter(function(p) p$to == dv, cfg$structural_paths),
          function(p) p$from
        )
        rhs <- preds
        if (length(ctrl_names) > 0)
          rhs <- c(rhs, paste0("controls:", paste(ctrl_names, collapse = ",")))
        gc_structural_spec[[dv]] <- rhs
      }

      # Suspected regressors: read from config (only genuinely suspected)
      gc_suspected <- if (!is.null(stage_cfg$endogeneity$suspected))
        stage_cfg$endogeneity$suspected
      else if (!is.null(cfg$endogeneity$suspected))
        cfg$endogeneity$suspected
      else character()  # empty = skip

      if (length(gc_suspected) == 0) {
        log_msg(log_info, "  Gaussian Copula: skipped (no suspected regressors in config)")
        stop("skip")
      }
      log_msg(log_info, paste("  Suspected regressors (from config):",
                              paste(gc_suspected, collapse = ", ")))

      gc_cor_threshold <- if (!is.null(stage_cfg$endogeneity$cor_threshold))
        stage_cfg$endogeneity$cor_threshold else 0.95
      gc_force_normal <- if (!is.null(stage_cfg$endogeneity$force_normal))
        stage_cfg$endogeneity$force_normal else FALSE

      gc_scores <- get_copula_scores(
        pls_model  = pls_model,
        data_final = data_final,
        controls   = ctrl_names
      )

      gc_results <- gaussian_copula_test(
        scores          = gc_scores,
        structural_spec = gc_structural_spec,
        suspected       = gc_suspected,
        B               = cfg$project$bootstrap_samples,
        seed            = cfg$project$seed,
        output_dir      = "09_robust/endogeneity",
        policy          = inference_policy,
        log_info        = log_info,
        force_normal    = gc_force_normal,
        cor_threshold   = gc_cor_threshold
      )
    }, error = function(e) {
      log_warn(log_info, paste("Gaussian Copula test error:", e$message))
    })
  }

  # ---- Nonlinear / Quadratic Effects Robustness ----
  if (isTRUE(modules$enable_nonlinear)) {
    tryCatch({
      run_robust_nonlinear(
        config_path = "config/robustness_nonlinear.yml",
        log_info    = log_info,
        cfg         = cfg
      )
    }, error = function(e) {
      log_warn(log_info, paste("Nonlinear robustness test error:", e$message))
    })
  }

  # ---- Level-based Heterogeneity (Score-based Clustering + MICOM + MGA) ----
  if (isTRUE(modules$enable_level_heterogeneity)) {
    tryCatch({
      lh_config_path <- if (!is.null(stage_cfg$level_heterogeneity$config_path))
        stage_cfg$level_heterogeneity$config_path
      else "R/09_robust_level_heterogeneity/00_config_template.yml"
      source("R/09_robust_level_heterogeneity/01_run_level_heterogeneity.R")
      run_level_heterogeneity(
        config_path = lh_config_path,
        log_info    = log_info,
        cfg         = cfg
      )
    }, error = function(e) {
      log_warn(log_info, paste("Level-based heterogeneity error:", e$message))
    })
  }

  if (isTRUE(modules$enable_report)) {
    export_report(cfg, log_info, output_dir = output_dir, policy = inference_policy)
  }

  # ==========================================================================
  # PHASE 5: COM6 INDICATOR SENSITIVITY (main only, if enabled)
  # ==========================================================================
  # Re-estimate PLS sans COM6, compare path coefficients / R² / f².
  # Reuses data_final from Phase 1 — no re-import, no re-clean.
  # Only costs one additional bootstrap run.
  # ==========================================================================
  com6_sens_result <- NULL
  if (stage == "main" && isTRUE(modules$enable_com6_sensitivity)) {
    tryCatch({
      sens_cfg <- stage_cfg$com6_sensitivity
      if (is.null(sens_cfg)) {
        sens_cfg <- list(construct = "COM", indicator = "COM6",
                         bootstrap_samples = cfg$project$bootstrap_samples,
                         output_subdir = "09_robust/com6_sensitivity")
      }
      if (is.null(sens_cfg$bootstrap_samples))
        sens_cfg$bootstrap_samples <- cfg$project$bootstrap_samples
      if (is.null(sens_cfg$seed))
        sens_cfg$seed <- cfg$project$seed

      com6_sens_result <- run_com6_sensitivity(
        pls_baseline      = pls_model,
        boot_baseline     = boot_model,
        data_final        = data_final,
        active_indicators = active_indicators,
        cfg               = cfg,
        log_info          = log_info,
        policy            = inference_policy,
        sensitivity_cfg   = sens_cfg
      )
    }, error = function(e) {
      log_warn(log_info, paste("COM6 sensitivity error:", e$message))
    })
  }

  # ==========================================================================
  # INSTRUMENT LOCK — Write (pilot only)
  # ==========================================================================
  if (stage == "pilot" && isTRUE(stage_cfg$instrument_lock$create_on_completion)) {
    # Save the ORIGINAL indicators (pre-optimizer) alongside post-optimizer
    # by re-reading from base config
    orig_cfg <- yaml::read_yaml("00_meta/analysis_config.yaml")
    orig_active <- list()
    for (cc in orig_cfg$constructs) {
      orig_active[[cc$name]] <- cc$indicators
    }
    # But for instrument_lock, we use the post-optimizer cfg
    write_instrument_lock(cfg, active_indicators, removed_log, output_dir, log_info)

    # Write pilot summary
    write_pilot_summary(data_tech, data_qc, data_final, removed_log, cfg, log_info, output_dir)
  }

  # ==========================================================================
  # INSTRUMENT FINAL — snapshot (main only) for reproducibility audit trail
  # ==========================================================================
  if (stage == "main") {
    final_lock <- list(
      timestamp   = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      stage       = "main",
      r_version   = R.version.string,
      seed        = cfg$project$seed,
      n_obs       = nrow(data_final),
      freeze_rules_applied = if (!is.null(stage_cfg$freeze_rules)) stage_cfg$freeze_rules else list(),
      constructs  = lapply(cfg$constructs, function(cc) {
        list(name = cc$name, measurement_type = cc$measurement_type,
             active_indicators = active_indicators[[cc$name]])
      })
    )
    final_path <- file.path(output_dir, "instrument_final.json")
    jsonlite::write_json(final_lock, final_path, pretty = TRUE, auto_unbox = TRUE)
    log_msg(log_info, sprintf("Instrument final snapshot: %s", final_path))
  }

  # ==========================================================================
  # PIPELINE SUMMARY
  # ==========================================================================
  log_msg(log_info, "\n========================================")
  log_msg(log_info, sprintf("PIPELINE SUMMARY (%s)", toupper(stage)))
  log_msg(log_info, "========================================")
  log_msg(log_info, sprintf("Final sample: %d observations", nrow(data_final)))
  log_msg(log_info, sprintf("Constructs: %d total (%d reflective, %d formative)",
                             length(cfg$constructs),
                             length(cfg$reflective_constructs),
                             length(cfg$formative_constructs)))
  log_msg(log_info, sprintf("Measurement gate: %s", ifelse(measurement_pass, "PASS", "FAIL")))
  if (isTRUE(modules$enable_structural)) {
    log_msg(log_info, sprintf("Structural VIF gate: %s", ifelse(struct_results$pass, "PASS", "FAIL")))
  }
  if (isTRUE(modules$enable_plspredict)) {
    log_msg(log_info, sprintf("PLSpredict gate: %s", ifelse(predict_results$pass, "PASS", "FAIL")))
  }
  log_msg(log_info, sprintf("Instrument locked: %s",
                             ifelse(instrument_locked, "YES (from pilot)",
                                    ifelse(stage == "pilot", "CREATED", "NO"))))
  if (!is.null(com6_sens_result)) {
    log_msg(log_info, sprintf("COM6 Sensitivity: %s (flips=%s, max|Delta_Beta|=%s)",
                               ifelse(isTRUE(com6_sens_result$robust), "ROBUST", "SENSITIVE"),
                               ifelse(is.na(com6_sens_result$n_flips), "N/A",
                                      as.character(com6_sens_result$n_flips)),
                               ifelse(is.na(com6_sens_result$max_delta_beta), "N/A",
                                      sprintf("%.4f", com6_sens_result$max_delta_beta))))
  }
  log_msg(log_info, "========================================")

  close_pipeline_log(log_info)

  cat(sprintf("\n\u2705 %s pipeline complete!\n", toupper(stage)))
  cat(sprintf("Results in: %s\n", output_dir))
  cat("Log file:", log_info$path, "\n")

  invisible(list(
    stage             = stage,
    cfg               = cfg,
    pls_model         = pls_model,
    boot_model        = boot_model,
    active_indicators = active_indicators,
    log_path          = log_info$path,
    output_dir        = output_dir
  ))
}
