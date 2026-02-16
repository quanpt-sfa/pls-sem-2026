# ==============================================================================
# run_pipeline.R — Master Pipeline Orchestrator (Backward-Compatible Wrapper)
# CCA-SEM Analysis Pipeline (Hair et al., 2022)
# ==============================================================================
# Cách chạy mới (khuyến nghị):
#   Rscript run_pilot.R   # Giai đoạn pilot — tinh chỉnh instrument
#   Rscript run_main.R    # Giai đoạn chính — phân tích đầy đủ
#
# Backward compatibility:
#   File này vẫn hoạt động như trước. Nếu config/main.yml tồn tại,
#   nó sẽ gọi run_stage("config/main.yml"). Nếu không, chạy pipeline gốc.
# ==============================================================================
# Cách chạy (legacy):
#   setwd("d:/Works/Data analysis/C4")
#   source("R/run_pipeline.R")
#
# Hoặc từ terminal:
#   cd "d:\Works\Data analysis\C4"
#   Rscript R/run_pipeline.R
# ==============================================================================

# --- Check if stage-based config exists ---
if (file.exists("config/main.yml")) {
  # Stage-aware mode: delegate to run_core.R
  cat("Detected config/main.yml — running MAIN stage pipeline\n")
  source("R/run_core.R")
  run_stage("config/main.yml")
} else {
  # ============================================================================
  # LEGACY MODE — original pipeline (no stage separation)
  # ============================================================================

cat("========================================\n")
cat("CCA-SEM Analysis Pipeline\n")
cat("========================================\n")

# --- Package check ---
required_packages <- c(
  "yaml", "readxl", "dplyr", "tidyr", "stringr",
  "seminr", "ggplot2", "flextable", "officer"
)

optional_packages <- c("naniar")  # For MCAR test

missing_pkgs <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  cat("\nMissing required packages:", paste(missing_pkgs, collapse = ", "), "\n")
  cat("Install with: install.packages(c(", paste0('"', missing_pkgs, '"', collapse = ", "), "))\n")
  stop("Please install required packages first.")
}

# --- Source all modules ---
source("R/00_config.R")
source("R/utils_logging.R")
source("R/utils_tables.R")
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
source("R/14_report_export.R")

# ==============================================================================
# INITIALIZE
# ==============================================================================
cfg <- load_config("00_meta/analysis_config.yaml")
validate_config(cfg)
ensure_output_dirs()
set.seed(cfg$project$seed)

log_info <- init_pipeline_log("logs")

log_msg(log_info, paste("Project:", cfg$project$name))
log_msg(log_info, paste("Config locked:", cfg$project$date_locked))
log_msg(log_info, paste("Seed:", cfg$project$seed))
log_msg(log_info, paste("Bootstrap:", cfg$project$bootstrap_samples))
log_msg(log_info, paste("Constructs:", length(cfg$constructs)))
log_msg(log_info, paste("  Reflective:", length(cfg$reflective_constructs)))
log_msg(log_info, paste("  Formative:", length(cfg$formative_constructs)))
log_msg(log_info, paste("Structural paths:", length(cfg$structural_paths)))

# ==============================================================================
# PHASE 1: DATA PREPARATION
# ==============================================================================
log_msg(log_info, "\n===== PHASE 1: DATA PREPARATION =====")

# Step 1: Import & Technical Validation
data_tech <- import_and_validate(cfg, log_info)

# Step 2: Behavioral QC
data_qc <- run_behavioral_qc(data_tech, cfg, log_info)

# Step 3: Missing Data
data_final <- handle_missing(data_qc, cfg, log_info)
saveRDS(data_final, "02_clean/analysis_ready.rds")

# Step 4: Descriptive Statistics + Demographics + Control Variable Coding
data_final <- run_descriptives(data_final, cfg, log_info)
saveRDS(data_final, "02_clean/analysis_ready.rds")  # Re-save with coded columns

log_msg(log_info, sprintf("\nPhase 1 complete — Analysis-ready dataset: %d rows, %d cols", nrow(data_final), ncol(data_final)))

# ==============================================================================
# PHASE 2: CMV & MEASUREMENT MODEL (Quality Gate)
# ==============================================================================
log_msg(log_info, "\n===== PHASE 2: CMV & MEASUREMENT MODEL =====")

suppressPackageStartupMessages(library(seminr))

# Step 5: CMV/CMB Assessment
cmv_pass <- assess_cmv(data_final, cfg, log_info)

# --- Prepare mutable indicator list (for iterative removal) ---
# Deep-copy the construct indicator lists so we can modify without touching cfg
active_indicators <- list()
for (cc in cfg$constructs) {
  active_indicators[[cc$name]] <- cc$indicators
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

# Structural model (fixed throughout optimization)
sm_list <- lapply(cfg$structural_paths, function(p) paths(from = p$from, to = p$to))
sm <- do.call(relationships, sm_list)

# Data matrix
all_ind_cols <- unique(unlist(active_indicators))
ind_cols <- intersect(all_ind_cols, names(data_final))
pls_data <- as.data.frame(data_final[, ind_cols, drop = FALSE])

# ==============================================================================
# ITERATIVE OPTIMIZER: Auto-remove reflective indicators with loading < 0.40
# ==============================================================================
log_msg(log_info, "\n--- Measurement Model Optimizer ---")
log_msg(log_info, "Strategy: auto-remove reflective indicators with loading < 0.40")
log_msg(log_info, "Max iterations: 5")

LOADING_DROP_THRESHOLD <- 0.40
MAX_ITER <- 5
removed_log <- data.frame(Iteration = integer(), Construct = character(),
                          Indicator = character(), Loading = numeric(),
                          stringsAsFactors = FALSE)
pls_model <- NULL

for (iter in seq_len(MAX_ITER)) {
  log_msg(log_info, sprintf("\n--- Optimizer Iteration %d/%d ---", iter, MAX_ITER))
  
  # Update data matrix to match current active indicators
  current_inds <- unique(unlist(active_indicators))
  current_cols <- intersect(current_inds, names(data_final))
  pls_data_iter <- as.data.frame(data_final[, current_cols, drop = FALSE])
  
  # Build measurement model
  mm <- build_mm(cfg, active_indicators)
  
  # Estimate PLS
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
  
  # Check reflective outer loadings
  model_summ <- summary(pls_model)
  loadings_mat <- model_summ$loadings
  
  # Identify reflective constructs
  refl_names <- sapply(cfg$constructs[sapply(cfg$constructs, function(c) c$measurement_type == "reflective")],
                       function(c) c$name)
  
  # Find bad indicators (loading < 0.40 in reflective constructs only)
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
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  if (nrow(bad_items) == 0) {
    log_msg(log_info, "All reflective loadings >= 0.40 — optimizer converged")
    break
  }
  
  # Auto-remove bad indicators
  for (i in seq_len(nrow(bad_items))) {
    cc_name <- bad_items$Construct[i]
    ind_name <- bad_items$Indicator[i]
    ld_val <- bad_items$Loading[i]
    
    # Safety: don't remove if construct would have < 2 indicators
    if (length(active_indicators[[cc_name]]) <= 2) {
      log_warn(log_info, sprintf(
        "  [SKIP] %s (Loading=%.3f) — cannot remove, %s would have < 2 indicators",
        ind_name, ld_val, cc_name))
      next
    }
    
    log_msg(log_info, sprintf(
      "  [AUTO-FIX] Removing indicator %s (Loading = %.3f < 0.40). Re-estimating...",
      ind_name, ld_val))
    
    active_indicators[[cc_name]] <- setdiff(active_indicators[[cc_name]], ind_name)
    
    removed_log <- rbind(removed_log, data.frame(
      Iteration = iter, Construct = cc_name, Indicator = ind_name,
      Loading = ld_val, stringsAsFactors = FALSE
    ))
  }
  
  if (iter == MAX_ITER) {
    log_warn(log_info, "Max iterations reached — proceeding with current model")
  }
}

# --- Log removal summary ---
if (nrow(removed_log) > 0) {
  log_msg(log_info, sprintf("\n--- Auto-removed %d indicator(s) across %d iteration(s) ---",
                             nrow(removed_log), max(removed_log$Iteration)))
  for (i in seq_len(nrow(removed_log))) {
    log_msg(log_info, sprintf("  Iter %d: %s.%s (Loading=%.3f)",
                               removed_log$Iteration[i], removed_log$Construct[i],
                               removed_log$Indicator[i], removed_log$Loading[i]))
  }
  write.csv(removed_log, "05_measurement/reflective/auto_removed_indicators.csv",
            row.names = FALSE)
} else {
  log_msg(log_info, "No indicators auto-removed — original model is clean")
}

# --- Log final indicator counts per construct ---
log_msg(log_info, "\n--- Final Indicator Counts ---")
for (cc in cfg$constructs) {
  n_orig <- length(cc$indicators)
  n_now  <- length(active_indicators[[cc$name]])
  suffix <- ifelse(n_orig == n_now, "", sprintf(" (removed %d)", n_orig - n_now))
  log_msg(log_info, sprintf("  %s [%s]: %d/%d%s",
                             cc$name, cc$measurement_type, n_now, n_orig, suffix))
}

# --- Sync cfg$constructs with post-optimizer active_indicators ---
# This ensures all downstream scripts (moderation, robustness, etc.) use the
# correct indicator set after auto-removal
for (i in seq_along(cfg$constructs)) {
  cn <- cfg$constructs[[i]]$name
  cfg$constructs[[i]]$indicators <- active_indicators[[cn]]
}
cfg$all_indicators <- unique(unlist(active_indicators))
# Also sync reflective/formative sub-lists
cfg$reflective_constructs <- Filter(function(c) c$measurement_type == "reflective", cfg$constructs)
cfg$formative_constructs  <- Filter(function(c) c$measurement_type == "formative",  cfg$constructs)

# ==============================================================================
# BOOTSTRAP — only after optimizer converges
# ==============================================================================
log_msg(log_info, sprintf("\nBootstrapping (%d samples)...", cfg$project$bootstrap_samples))
boot_model <- bootstrap_model(
  seminr_model = pls_model,
  nboot = cfg$project$bootstrap_samples,
  seed = cfg$project$seed
)
log_msg(log_info, "Bootstrap complete")

# Save model objects
saveRDS(pls_model, "06_structural/pls_model.rds")
saveRDS(boot_model, "06_structural/boot_model.rds")
saveRDS(active_indicators, "06_structural/active_indicators.rds")

# Step 6: Reflective Measurement (assess the optimized model)
refl_results <- assess_reflective(pls_model, boot_model, cfg, log_info)

# Step 7: Formative Measurement
form_results <- assess_formative(pls_model, boot_model, cfg, log_info)

# Step 8: Higher-Order Constructs
hoc_results <- assess_hoc(data_final, cfg, log_info)

# === GATE CHECK: Measurement Model ===
measurement_pass <- refl_results$pass && form_results$pass
log_msg(log_info, "")
log_msg(log_info, sprintf("=== MEASUREMENT MODEL GATE: %s ===",
                           ifelse(measurement_pass, "PASS ✅", "FAIL ❌")))

if (!measurement_pass) {
  log_warn(log_info, "Measurement model NOT passed — structural results should be interpreted with caution")
  log_warn(log_info, "Review indicator decisions and consider modifications before proceeding")
}

# ==============================================================================
# PHASE 3: STRUCTURAL MODEL & HYPOTHESES
# ==============================================================================
log_msg(log_info, "\n===== PHASE 3: STRUCTURAL MODEL & HYPOTHESES =====")

# Step 9: Structural Model (in-sample) — uses the optimized pls_model & boot_model
struct_results <- assess_structural(pls_model, boot_model, cfg, log_info)

# Step 10: PLSpredict (out-of-sample)
predict_results <- run_plspredict(pls_model, cfg, log_info)

# Step 11a: Mediation (saturated model with direct paths for Nitzl classification)
med_results <- test_mediation(pls_model, boot_model, data_final, active_indicators, cfg, log_info)

# Step 11b: Moderation
mod_results <- test_moderation(data_final, cfg, log_info)

# ==============================================================================
# PHASE 4: ROBUSTNESS & REPORTING
# ==============================================================================
log_msg(log_info, "\n===== PHASE 4: ROBUSTNESS & REPORTING =====")

# Step 12: Robustness
robust_results <- run_robustness(pls_model, data_final, cfg, log_info)

# Step 13: Report Export
export_report(cfg, log_info)

# ==============================================================================
# PIPELINE SUMMARY
# ==============================================================================
log_msg(log_info, "\n========================================")
log_msg(log_info, "PIPELINE SUMMARY")
log_msg(log_info, "========================================")
log_msg(log_info, sprintf("Final sample: %d observations", nrow(data_final)))
log_msg(log_info, sprintf("Constructs: %d total (%d reflective, %d formative)",
                           length(cfg$constructs),
                           length(cfg$reflective_constructs),
                           length(cfg$formative_constructs)))
log_msg(log_info, sprintf("Measurement gate: %s", ifelse(measurement_pass, "PASS", "FAIL")))
log_msg(log_info, sprintf("Structural VIF gate: %s", ifelse(struct_results$pass, "PASS", "FAIL")))
log_msg(log_info, sprintf("PLSpredict gate: %s", ifelse(predict_results$pass, "PASS", "FAIL")))
log_msg(log_info, "========================================")

# Close log
close_pipeline_log(log_info)

cat("\n✅ Pipeline complete!\n")
cat("Results in: 02_clean/ through 10_report/\n")
cat("Log file:", log_info$path, "\n")
}  # end legacy else block
