# ==============================================================================
# 01_run_level_heterogeneity.R — Master runner
# ==============================================================================
# Sensitivity Analysis for Level-based Heterogeneity
# (Score-based clustering on exogenous constructs)
#
# Usage (standalone — from project root):
#   Rscript R/09_robust_level_heterogeneity/01_run_level_heterogeneity.R
#
# With custom config:
#   Rscript R/09_robust_level_heterogeneity/01_run_level_heterogeneity.R \
#           config=path/to/my_config.yml
#
# From R console:
#   source("R/09_robust_level_heterogeneity/01_run_level_heterogeneity.R")
#   run_level_heterogeneity()   # default config
#   run_level_heterogeneity("path/to/config.yml")
#
# From pipeline (run_core.R):
#   run_level_heterogeneity(config_path, log_info = log_info, cfg = cfg)
# ==============================================================================

# ---- Source module files (relative to project root) --------------------------
.lh_source_modules <- function() {
  mod_dir <- "R/09_robust_level_heterogeneity"
  source(file.path(mod_dir, "utils_level_heterogeneity.R"))
  source(file.path(mod_dir, "02_scores_and_clustering.R"))
  source(file.path(mod_dir, "03_micom.R"))
  source(file.path(mod_dir, "04_mga_permutation.R"))
  source(file.path(mod_dir, "05_reporting.R"))
}

# ---- Package check ----------------------------------------------------------
.lh_check_packages <- function() {
  required <- c("yaml", "seminr", "cSEM", "cluster", "ggplot2")
  missing  <- setdiff(required, rownames(installed.packages()))
  if (length(missing) > 0) {
    cat("Missing required packages:", paste(missing, collapse = ", "), "\n")
    cat("Install with: install.packages(c(",
        paste0('"', missing, '"', collapse = ", "), "))\n")
    stop("Please install required packages first.")
  }
}

# ==============================================================================
# MAIN FUNCTION
# ==============================================================================

#' Run Sensitivity Analysis for Level-based Heterogeneity
#'
#' @param config_path Path to YAML config (default: module template)
#' @param log_info    Optional pipeline log_info object (for integration)
#' @param cfg         Optional pipeline cfg object (unused here; reserved)
#' @return invisible list of all results
run_level_heterogeneity <- function(
    config_path = "R/09_robust_level_heterogeneity/00_config_template.yml",
    log_info    = NULL,
    cfg         = NULL
) {

  t_start <- Sys.time()

  # ---- 0. Setup --------------------------------------------------------------
  .lh_check_packages()
  .lh_source_modules()

  # Load module config
  mod_cfg <- lh_load_config(config_path)

  if (!isTRUE(mod_cfg$enabled)) {
    message("Level-based heterogeneity module disabled in config.")
    return(invisible(NULL))
  }

  output_dir <- mod_cfg$paths$output_dir
  lh_ensure_dirs(output_dir)

  # Init logging (standalone if no pipeline log)
  own_log <- is.null(log_info)
  if (own_log) {
    log_info <- lh_init_log(output_dir)
  } else {
    lh_log(log_info, "")
    lh_log(log_info, "========================================================")
    lh_log(log_info, "Module: Sensitivity Analysis for Level-based Heterogeneity")
    lh_log(log_info, "========================================================")
  }

  lh_log(log_info, sprintf("Config: %s", config_path))
  lh_log(log_info, sprintf("Output: %s", output_dir))
  lh_log(log_info, sprintf("Seed: %d", mod_cfg$seed))
  lh_log(log_info, sprintf("Backend: %s", mod_cfg$backend))

  set.seed(mod_cfg$seed)

  # ---- 0b. Detect backend & load data ----------------------------------------
  lh_step(log_info, "Step 0 — Backend Detection & Data Loading")

  env <- lh_detect_backend(mod_cfg, log_info)
  if (env$backend == "none")
    stop("No usable PLS backend. Provide pls_model.rds or install seminr/cSEM.")

  # Validate that data is available for sub-group estimation
  if (is.null(env$data_raw)) {
    stop("Raw data required for sub-group PLS estimation. Set paths.data_path in config.")
  }
  if (is.null(env$base_cfg)) {
    stop("Base analysis config required for model spec. Set paths.config_path in config.")
  }

  # ---- STEP 1 — Clustering ---------------------------------------------------
  tryCatch({
    clust_result <- lh_step1_clustering(env, mod_cfg, log_info)
    lh_export_clustering(clust_result, output_dir, mod_cfg, log_info)
    lh_plot_clustering(clust_result$diagnostics, clust_result$k_opt,
                       output_dir, mod_cfg, log_info)
    lh_gate(log_info, "Clustering", TRUE,
            sprintf("k=%d, n_groups=%s", clust_result$k_opt,
                    paste(table(clust_result$groups), collapse = "/")))
  }, error = function(e) {
    lh_log(log_info, paste("FATAL Step 1:", e$message), level = "ERROR")
    if (own_log) lh_close_log(log_info)
    stop(e)
  })

  # ---- STEP 2 — MICOM --------------------------------------------------------
  micom_result <- tryCatch({
    lh_step2_micom(env, mod_cfg, clust_result$groups, clust_result$k_opt, log_info)
  }, error = function(e) {
    lh_log(log_info, paste("ERROR Step 2 (MICOM):", e$message), level = "ERROR")
    lh_log(log_info, "  Proceeding to MGA with MICOM=NOT ASSESSED.", level = "WARN")
    list(passed_step2 = FALSE,
         results_df = data.frame(),
         all_results = list(),
         n_pairs = 0)
  })

  tryCatch({
    lh_export_micom(micom_result, output_dir, mod_cfg, log_info)
  }, error = function(e) {
    lh_log(log_info, paste("WARN export MICOM:", e$message), level = "WARN")
  })

  # ---- STEP 3 — MGA ----------------------------------------------------------
  mga_result <- tryCatch({
    lh_step3_mga(env, mod_cfg, clust_result$groups, clust_result$k_opt,
                 micom_result, log_info)
  }, error = function(e) {
    lh_log(log_info, paste("ERROR Step 3 (MGA):", e$message), level = "ERROR")
    list(mga_df = data.frame(), pair_results = list(),
         micom_ok = FALSE, n_sig = 0)
  })

  tryCatch({
    lh_export_mga(mga_result, output_dir, mod_cfg, log_info)
    lh_plot_mga(mga_result, output_dir, mod_cfg, log_info)
  }, error = function(e) {
    lh_log(log_info, paste("WARN export MGA:", e$message), level = "WARN")
  })

  # ---- STEP 4 — Reporting ----------------------------------------------------
  tryCatch({
    lh_step4_reporting(clust_result, micom_result, mga_result,
                       mod_cfg, output_dir, log_info)
  }, error = function(e) {
    lh_log(log_info, paste("WARN reporting:", e$message), level = "WARN")
  })

  # ---- Wrap up ----------------------------------------------------------------
  elapsed <- difftime(Sys.time(), t_start, units = "mins")
  lh_log(log_info, "")
  lh_log(log_info, "========================================================")
  lh_log(log_info, "LEVEL-BASED HETEROGENEITY — PIPELINE SUMMARY")
  lh_log(log_info, "========================================================")
  lh_log(log_info, sprintf("Backend:          %s", env$backend))
  lh_log(log_info, sprintf("Clustering:       %s, k = %d",
                            mod_cfg$clustering$method, clust_result$k_opt))
  lh_log(log_info, sprintf("Group sizes:      %s",
                            paste(table(clust_result$groups), collapse = " / ")))
  lh_log(log_info, sprintf("MICOM Step 2:     %s",
                            ifelse(micom_result$passed_step2, "PASS", "FAIL/NOT ASSESSED")))
  lh_log(log_info, sprintf("MGA sig paths:    %d / %d",
                            mga_result$n_sig, nrow(mga_result$mga_df)))
  lh_log(log_info, sprintf("Elapsed:          %.1f minutes", as.numeric(elapsed)))
  lh_log(log_info, sprintf("Output:           %s", output_dir))
  lh_log(log_info, "========================================================")

  if (own_log) lh_close_log(log_info)

  cat(sprintf("\n\u2705 Level-based heterogeneity analysis complete (%.1f min)\n",
              as.numeric(elapsed)))
  cat(sprintf("Results in: %s/\n", output_dir))

  invisible(list(
    clustering = clust_result,
    micom      = micom_result,
    mga        = mga_result,
    config     = mod_cfg,
    output_dir = output_dir
  ))
}

# ==============================================================================
# CLI entry point — allows running via Rscript
# ==============================================================================
if (!interactive() || identical(Sys.getenv("LH_RUN"), "1")) {
  args <- commandArgs(trailingOnly = TRUE)
  config_arg <- grep("^config=", args, value = TRUE)

  config_path <- if (length(config_arg) > 0) {
    sub("^config=", "", config_arg[1])
  } else {
    "R/09_robust_level_heterogeneity/00_config_template.yml"
  }

  cat("=== Sensitivity Analysis for Level-based Heterogeneity ===\n")
  cat("Config:", config_path, "\n\n")

  run_level_heterogeneity(config_path = config_path)
}
