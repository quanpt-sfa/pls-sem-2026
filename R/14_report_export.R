# ==============================================================================
# 14_report_export.R — Step 13: Export Thesis-Ready Tables & Figures
# ==============================================================================

suppressPackageStartupMessages({
  library(flextable)
  library(officer)
  library(dplyr)
})

#' Tổng hợp & xuất tất cả bảng/hình cho luận án
#' @param cfg list config
#' @param log_info list log
#' @param output_dir Thư mục output (default "10_report")
#' @param policy inference policy list (from utils_inference.R)
export_report <- function(cfg, log_info, output_dir = "10_report", policy = NULL) {
  
  log_step(log_info, "Step 13: Report Export — Thesis-Ready Tables & Figures")
  
  fig_dir <- file.path(output_dir, "figures")
  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
  
  source("R/utils_tables.R")
  
  tables <- list()
  
  # ==========================================================================
  # Table 1: Quy trình sàng lọc (Screening Funnel)
  # ==========================================================================
  if (file.exists("03_qc/qc_summary.csv")) {
    qc <- read.csv("03_qc/qc_summary.csv")
    tables[["qc"]] <- make_thesis_table(
      qc,
      title = "Table X: Behavioral Quality Control Summary",
      note = paste("Screening threshold:", cfg$screening$flag_threshold_to_remove, "flags")
    )
    log_msg(log_info, "Table: QC Summary created")
  }
  
  # ==========================================================================
  # Table 2: Descriptive Statistics
  # ==========================================================================
  if (file.exists("04_descriptives/descriptive_stats.csv")) {
    desc <- read.csv("04_descriptives/descriptive_stats.csv")
    tables[["desc"]] <- make_thesis_table(
      desc,
      title = "Table X: Descriptive Statistics of Measurement Indicators",
      note = sprintf("N = final sample; Scale: %d-%d Likert", cfg$scale$min, cfg$scale$max)
    )
    log_msg(log_info, "Table: Descriptive Statistics created")
  }
  
  # ==========================================================================
  # Table 3: Sample Demographics
  # ==========================================================================
  if (file.exists("04_descriptives/sample_demographics.csv")) {
    demo <- read.csv("04_descriptives/sample_demographics.csv")
    tables[["demo"]] <- make_thesis_table(
      demo,
      title = "Table X: Sample Demographics"
    )
    log_msg(log_info, "Table: Demographics created")
  }
  
  # ==========================================================================
  # Table 4: CMV Assessment
  # ==========================================================================
  if (file.exists("04_cmv/full_collinearity_vif.csv")) {
    cmv <- read.csv("04_cmv/full_collinearity_vif.csv")
    threshold_warn <- if (!is.null(cfg$cmv$full_collinearity_vif_threshold))
                        cfg$cmv$full_collinearity_vif_threshold else 3.3
    threshold_fail <- if (!is.null(cfg$cmv$full_collinearity_vif_fail))
                        cfg$cmv$full_collinearity_vif_fail else 5.0
    ft_cmv <- make_thesis_table(
      cmv,
      title = "Table X: Full Collinearity VIF Assessment (CMV/CMB)",
      note = sprintf("WARN: VIF >= %.1f; FAIL: VIF >= %.1f (Kock, 2015)",
                     threshold_warn, threshold_fail)
    )
    ft_cmv <- highlight_threshold(ft_cmv, "Full_Collinearity_VIF",
                                   threshold_warn, "above")
    tables[["cmv"]] <- ft_cmv
    log_msg(log_info, "Table: CMV Assessment created")
  }
  
  # ==========================================================================
  # Table 5: Outer Loadings (Reflective)
  # ==========================================================================
  if (file.exists("05_measurement/reflective/outer_loadings.csv")) {
    loadings <- read.csv("05_measurement/reflective/outer_loadings.csv")
    ft_load <- make_thesis_table(
      loadings,
      title = "Table X: Outer Loadings (Reflective Constructs)",
      note = sprintf("Threshold: >= %.3f (Hair et al., 2022)",
                     cfg$thresholds_reflective$outer_loading_min)
    )
    ft_load <- highlight_threshold(ft_load, "Loading",
                                    cfg$thresholds_reflective$outer_loading_min, "below")
    tables[["loadings"]] <- ft_load
    log_msg(log_info, "Table: Outer Loadings created")
  }
  
  # ==========================================================================
  # Table 6: Reliability & Validity
  # ==========================================================================
  if (file.exists("05_measurement/reflective/reliability_table.csv")) {
    rel <- read.csv("05_measurement/reflective/reliability_table.csv")
    ft_rel <- make_thesis_table(
      rel,
      title = "Table X: Construct Reliability & Convergent Validity",
      note = "Alpha/rho_A/CR: [0.70, 0.95]; AVE >= 0.50 (Hair et al., 2022)"
    )
    ft_rel <- highlight_threshold(ft_rel, "AVE", cfg$thresholds_reflective$ave_min, "below")
    tables[["reliability"]] <- ft_rel
    log_msg(log_info, "Table: Reliability & Validity created")
  }
  
  # ==========================================================================
  # Table 7: HTMT Matrix
  # ==========================================================================
  if (file.exists("05_measurement/reflective/htmt_matrix.csv")) {
    htmt <- read.csv("05_measurement/reflective/htmt_matrix.csv", row.names = 1, check.names = FALSE)
    # Convert row names to a column for flextable
    htmt <- cbind(Construct = rownames(htmt), htmt)
    rownames(htmt) <- NULL
    tables[["htmt"]] <- make_thesis_table(
      htmt,
      title = "Table X: Heterotrait-Monotrait Ratio (HTMT)",
      note = sprintf("Threshold: < %.2f strict / < %.2f lenient (Henseler et al., 2015)",
                     cfg$thresholds_reflective$htmt_strict,
                     cfg$thresholds_reflective$htmt_lenient)
    )
    log_msg(log_info, "Table: HTMT Matrix created")
  }
  
  # ==========================================================================
  # Table 8: Path Coefficients
  # ==========================================================================
  if (file.exists("06_structural/path_coefficients_bootstrap.csv")) {
    paths <- read.csv("06_structural/path_coefficients_bootstrap.csv")
    path_note <- if (!is.null(policy)) {
      build_table_note(policy, n = nrow(readRDS("02_clean/analysis_ready.rds")),
                       B = cfg$project$bootstrap_samples, seed = cfg$project$seed)
    } else {
      sprintf("Bootstrap samples: %d; * significant at alpha = %.2f (CI excludes 0).",
              cfg$project$bootstrap_samples, 0.05)
    }
    tables[["paths"]] <- make_thesis_table(
      paths,
      title = "Table X: Structural Model Path Coefficients",
      note = path_note
    )
    log_msg(log_info, "Table: Path Coefficients created")
  }
  
  # ==========================================================================
  # Table 9: R² and f²
  # ==========================================================================
  if (file.exists("06_structural/r_squared.csv")) {
    r2 <- read.csv("06_structural/r_squared.csv")
    tables[["r2"]] <- make_thesis_table(
      r2,
      title = "Table X: Coefficient of Determination (R²)",
      note = "Cohen (1988): 0.26 substantial, 0.13 moderate, 0.02 weak"
    )
  }
  
  if (file.exists("06_structural/f_squared.csv")) {
    f2 <- read.csv("06_structural/f_squared.csv")
    tables[["f2"]] <- make_thesis_table(
      f2,
      title = "Table X: Effect Sizes (f²)",
      note = "Cohen (1988): 0.35 large, 0.15 medium, 0.02 small"
    )
    log_msg(log_info, "Table: R² and f² created")
  }
  
  # ==========================================================================
  # Table 10: PLSpredict
  # ==========================================================================
  if (file.exists("07_predict/plspredict_results.csv")) {
    pred <- read.csv("07_predict/plspredict_results.csv")
    tables[["plspredict"]] <- make_thesis_table(
      pred,
      title = "Table X: PLSpredict Out-of-sample Predictive Power",
      note = sprintf("k = %d folds, %d repetitions (Shmueli et al., 2019)",
                     cfg$plspredict$k_folds, cfg$plspredict$repetitions)
    )
    log_msg(log_info, "Table: PLSpredict created")
  }
  
  # ==========================================================================
  # Table 11: Mediation
  # ==========================================================================
  if (file.exists("08_complex/mediation/mediation_classification.csv")) {
    med_class <- read.csv("08_complex/mediation/mediation_classification.csv")
    med_note <- if (!is.null(policy)) {
      build_table_note(policy, n = nrow(readRDS("02_clean/analysis_ready.rds")),
                       B = cfg$project$bootstrap_samples, seed = cfg$project$seed,
                       extra = c(
                         "Direct effects (c') estimated in a saturated structural model for mediation classification per Nitzl et al. (2016) / Zhao et al. (2010).",
                         "Indirect effect = a_boot[i] * b_boot[i] (aligned bootstrap draws).",
                         "VAF = indirect/total (descriptive only, not used for classification)."
                       ))
    } else {
      paste("Direct effects (c') estimated in a saturated structural model",
            "for mediation classification per Nitzl et al. (2016) / Zhao et al. (2010).",
            sprintf("Bootstrap: %d resamples, seed = %d.",
                    cfg$project$bootstrap_samples, cfg$project$seed),
            "Significance: 95% percentile CI excludes 0 (primary rule).",
            "VAF = indirect/total (descriptive only, not used for classification).")
    }
    tables[["mediation"]] <- make_thesis_table(
      med_class,
      title = "Table X: Mediation Classification (Saturated Model with Direct Paths)",
      note = med_note
    )
    log_msg(log_info, "Table: Mediation Classification created (saturated model)")
  }

  if (file.exists("08_complex/mediation/specific_indirect_effects.csv")) {
    med_full <- read.csv("08_complex/mediation/specific_indirect_effects.csv")
    med_full_note <- if (!is.null(policy)) {
      build_table_note(policy, n = nrow(readRDS("02_clean/analysis_ready.rds")),
                       B = cfg$project$bootstrap_samples, seed = cfg$project$seed,
                       extra = c(
                         "Saturated structural model with direct X->Y paths.",
                         "Nitzl et al. (2016); Zhao et al. (2010); Hair et al. (2022)."
                       ))
    } else {
      paste("Saturated structural model with direct X->Y paths.",
            "CI = 95% percentile bootstrap confidence interval.",
            "Nitzl et al. (2016); Zhao et al. (2010); Hair et al. (2022).")
    }
    tables[["mediation_full"]] <- make_thesis_table(
      med_full,
      title = "Table X: Mediation Full Results (a, b, c', indirect, total)",
      note = med_full_note
    )
    log_msg(log_info, "Table: Mediation Full Results created")
  }
  
  # ==========================================================================
  # Table 12: Moderation
  # ==========================================================================
  if (file.exists("08_complex/moderation/interaction_coefficients.csv")) {
    mod <- read.csv("08_complex/moderation/interaction_coefficients.csv")
    mod_note <- if (!is.null(policy)) {
      ci_pct <- as.integer(policy$ci_level * 100)
      build_table_note(policy, n = nrow(readRDS("02_clean/analysis_ready.rds")),
                       B = cfg$project$bootstrap_samples, seed = cfg$project$seed,
                       extra = c(
                         "Two-stage approach (Hair et al., 2022).",
                         sprintf("Simple slopes illustrate the conditional effect at Low (M-1SD), Medium (M), and High (M+1SD) levels of the moderator.")
                       ))
    } else {
      "Two-stage approach (Hair et al., 2022). Significant = 95% bootstrap CI excludes 0."
    }
    tables[["moderation"]] <- make_thesis_table(
      mod,
      title = "Table X: Moderation Interaction Effects",
      note = mod_note
    )
    log_msg(log_info, "Table: Moderation created")
  }
  
  # ==========================================================================
  # Table 13: Model B — Controlled Model Comparison
  # ==========================================================================
  if (file.exists("09_robust/controlled_model/path_comparison_A_vs_B.csv")) {
    ab_comp <- read.csv("09_robust/controlled_model/path_comparison_A_vs_B.csv")
    tables[["model_b_paths"]] <- make_thesis_table(
      ab_comp,
      title = "Table X: Model A vs Model B Path Comparison (Robustness Check with Control Variables)",
      note = paste("Model A = core theoretical model (no controls).",
                   "Model B = Model A + control variables (Gen_Dummy, Exp_Ord, Pos_Ord, Edu_PostGrad, CPA_Dummy).",
                   "Robust = same sign & same significance conclusion between A and B.",
                   sprintf("Bootstrap: %d samples, seed = %d.",
                           cfg$project$bootstrap_samples, cfg$project$seed))
    )
    log_msg(log_info, "Table: Model A vs B Path Comparison created")
  }

  if (file.exists("09_robust/controlled_model/control_path_coefficients.csv")) {
    ctrl_paths <- read.csv("09_robust/controlled_model/control_path_coefficients.csv")
    tables[["control_paths"]] <- make_thesis_table(
      ctrl_paths,
      title = "Table X: Control Variable Path Coefficients (Model B)",
      note = "Single-item composite constructs. * = 95% bootstrap CI excludes 0."
    )
    log_msg(log_info, "Table: Control Path Coefficients created")
  }

  if (file.exists("09_robust/controlled_model/indirect_effects_A_vs_B.csv")) {
    ie_comp <- read.csv("09_robust/controlled_model/indirect_effects_A_vs_B.csv")
    tables[["model_b_indirect"]] <- make_thesis_table(
      ie_comp,
      title = "Table X: Indirect Effects Comparison — Model A vs Model B",
      note = paste("Model A = no controls; Model B = with controls.",
                   "Robust = same sign & same significance conclusion.",
                   "Indirect = a × b (aligned bootstrap draws).")
    )
    log_msg(log_info, "Table: Indirect Effects A vs B Comparison created")
  }

  # ==========================================================================
  # Xuất tất cả vào Word
  # ==========================================================================
  if (length(tables) > 0) {
    tryCatch({
      word_path <- file.path(output_dir, "all_tables.docx")
      export_tables_to_word(tables, word_path)
      log_msg(log_info, sprintf("All %d tables exported to: %s", length(tables), word_path))
    }, error = function(e) {
      log_warn(log_info, paste("Word export error:", e$message))
      log_msg(log_info, "Attempting individual exports...")
      for (nm in names(tables)) {
        tryCatch({
          save_as_docx(tables[[nm]], path = file.path(output_dir, paste0("table_", nm, ".docx")))
        }, error = function(e2) NULL)
      }
    })
  }
  
  # ==========================================================================
  # Copy figures
  # ==========================================================================
  figure_sources <- c(
    "03_qc/screening_funnel.png",
    "04_descriptives/distribution_plots.pdf"
  )
  
  # Moderation plots
  mod_plots <- list.files("08_complex/moderation/moderation_plots", full.names = TRUE,
                          pattern = "\\.png$")
  figure_sources <- c(figure_sources, mod_plots)
  
  for (src in figure_sources) {
    if (file.exists(src)) {
      file.copy(src, fig_dir, overwrite = TRUE)
    }
  }
  
  log_msg(log_info, "Report export complete")
}
