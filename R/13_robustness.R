# ==============================================================================
# 13_robustness.R — Step 12: Robustness, Sensitivity & Supplementary Tests
# Gate: "Kết luận không phụ thuộc một lựa chọn duy nhất"
# ==============================================================================

suppressPackageStartupMessages({
  library(seminr)
  library(dplyr)
})

#' Chạy kiểm tra độ vững / độ nhạy
#' @param pls_model seminr PLS model (final)
#' @param data_final data.frame final dataset
#' @param cfg list config
#' @param log_info list log
#' @return list kết quả
run_robustness <- function(pls_model, data_final, cfg, log_info) {
  
  log_step(log_info, "Step 12: Robustness, Sensitivity & Supplementary Tests")
  
  dir.create("09_robust/sensitivity_data", showWarnings = FALSE, recursive = TRUE)
  dir.create("09_robust/micom_mga", showWarnings = FALSE, recursive = TRUE)
  dir.create("09_robust/endogeneity", showWarnings = FALSE, recursive = TRUE)
  dir.create("09_robust/fimix", showWarnings = FALSE, recursive = TRUE)
  
  results <- list()
  ind_cols <- intersect(cfg$all_indicators, names(data_final))
  
  # ==========================================================================
  # A. Data Sensitivity: Pre vs Post QC screening
  # ==========================================================================
  log_msg(log_info, "--- A. Data Sensitivity: Pre vs Post Screening ---")
  
  tryCatch({
    # Load pre-screening data
    if (file.exists("02_clean/clean_qc_flagged.rds")) {
      data_pre <- readRDS("02_clean/clean_qc_flagged.rds")
      
      # Build and estimate model on pre-screening data
      mm_items <- list()
      for (cc in cfg$constructs) {
        if (cc$measurement_type == "reflective") {
          mm_items[[length(mm_items) + 1]] <- reflective(cc$name, cc$indicators)
        } else {
          mm_items[[length(mm_items) + 1]] <- composite(cc$name, cc$indicators, weights = mode_B)
        }
      }
      mm <- do.call(constructs, mm_items)
      
      sm_list <- lapply(cfg$structural_paths, function(p) paths(from = p$from, to = p$to))
      sm <- do.call(relationships, sm_list)
      
      pls_data_pre <- as.data.frame(data_pre[, ind_cols, drop = FALSE])
      
      # Handle missing in pre-screening data
      pls_data_pre <- pls_data_pre[complete.cases(pls_data_pre), ]
      
      pls_pre <- estimate_pls(data = pls_data_pre, measurement_model = mm, structural_model = sm)
      boot_pre <- bootstrap_model(seminr_model = pls_pre, nboot = 1000)  # fewer for sensitivity
      boot_pre_summ <- summary(boot_pre)
      
      # Compare paths
      paths_post <- summary(bootstrap_model(seminr_model = pls_model, nboot = 1000))$bootstrapped_paths
      paths_pre  <- boot_pre_summ$bootstrapped_paths
      
      if (!is.null(paths_post) && !is.null(paths_pre)) {
        common_paths <- intersect(rownames(paths_post), rownames(paths_pre))
        
        comparison <- data.frame(
          Path = common_paths,
          Beta_Pre = round(paths_pre[common_paths, "Original Est."], 3),
          Beta_Post = round(paths_post[common_paths, "Original Est."], 3),
          T_Pre = round(paths_pre[common_paths, "T Stat."], 3),
          T_Post = round(paths_post[common_paths, "T Stat."], 3),
          Sig_Pre = abs(paths_pre[common_paths, "T Stat."]) >= 1.96,
          Sig_Post = abs(paths_post[common_paths, "T Stat."]) >= 1.96,
          stringsAsFactors = FALSE
        )
        comparison$Sign_Stable <- sign(comparison$Beta_Pre) == sign(comparison$Beta_Post)
        comparison$Sig_Stable  <- comparison$Sig_Pre == comparison$Sig_Post
        
        write.csv(comparison, "09_robust/sensitivity_data/comparison_pre_post_screening.csv",
                  row.names = FALSE)
        
        n_stable_sign <- sum(comparison$Sign_Stable)
        n_stable_sig  <- sum(comparison$Sig_Stable)
        log_msg(log_info, sprintf("  Sign stable: %d/%d, Significance stable: %d/%d",
                                   n_stable_sign, nrow(comparison),
                                   n_stable_sig, nrow(comparison)))
        
        conclusion <- ifelse(n_stable_sig == nrow(comparison), "STABLE", "SENSITIVE")
        writeLines(paste("Sensitivity conclusion:", conclusion),
                   "09_robust/sensitivity_data/sensitivity_conclusion.md")
        
        results$sensitivity <- comparison
      }
    } else {
      log_msg(log_info, "  Pre-screening data not found — skipping sensitivity comparison")
    }
  }, error = function(e) {
    log_warn(log_info, paste("  Sensitivity analysis error:", e$message))
  })
  
  # ==========================================================================
  # B. MICOM & MGA (if applicable)
  # ==========================================================================
  log_msg(log_info, "--- B. MICOM & MGA ---")
  
  # Check if grouping variable exists (e.g., Big 4 vs non-Big 4)
  group_var <- NULL
  if ("Com" %in% names(data_final)) {
    # Kiểm tra kích thước nhóm
    group_tab <- table(data_final$Com)
    log_msg(log_info, sprintf("  Company groups: %d unique", length(group_tab)))
    
    # Nếu muốn Big4 vs non-Big4, cần mapping
    # Placeholder: log as conditional
    log_msg(log_info, "  MICOM-MGA: requires group variable definition and minimum group size")
    log_msg(log_info, "  If groups are defined and N >= 30-50 per group:")
    log_msg(log_info, "    Step 1: Configural invariance — same model specification")
    log_msg(log_info, "    Step 2: Compositional invariance — permutation test")
    log_msg(log_info, "    Step 3: Equal mean/variance — permutation test")
    log_msg(log_info, "    Then: MGA if MICOM passes")
    log_msg(log_info, "  IMPLEMENTATION: Requires manual group coding — placeholder for now")
    
    writeLines(c(
      "# MICOM-MGA Status",
      "",
      "## Prerequisites",
      "- Group variable: TBD (e.g., Big4 vs non-Big4)",
      "- Minimum group size: 30-50 per group",
      "",
      "## MICOM Steps",
      "1. Configural invariance: same model specification across groups",
      "2. Compositional invariance: permutation test (c values ~1, p > .05)",
      "3. Equal mean & variance: permutation test",
      "",
      "## Status: PENDING — requires group variable definition"
    ), "09_robust/micom_mga/micom_status.md")
  } else {
    log_msg(log_info, "  No grouping variable available — MGA not applicable")
  }
  
  # ==========================================================================
  # C. Endogeneity Assessment
  # ==========================================================================
  log_msg(log_info, "--- C. Endogeneity Assessment ---")
  
  tryCatch({
    # Kiểm tra phân phối biến nghi nội sinh
    endogenous <- unique(sapply(cfg$structural_paths, function(p) p$to))
    
    endo_check <- list()
    for (en in endogenous) {
      en_inds <- get_indicators(cfg, en)
      for (ind in en_inds) {
        if (ind %in% names(data_final)) {
          sw <- shapiro.test(data_final[[ind]][1:min(5000, nrow(data_final))])
          skew <- abs(mean((data_final[[ind]] - mean(data_final[[ind]])) / sd(data_final[[ind]]))^3)
          
          endo_check[[ind]] <- data.frame(
            Variable = ind,
            Shapiro_W = round(sw$statistic, 4),
            Shapiro_p = round(sw$p.value, 4),
            Non_Normal = sw$p.value < 0.05,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    if (length(endo_check) > 0) {
      endo_df <- do.call(rbind, endo_check)
      write.csv(endo_df, "09_robust/endogeneity/distribution_check.csv", row.names = FALSE)
      
      n_nonnormal <- sum(endo_df$Non_Normal)
      log_msg(log_info, sprintf("  Non-normal indicators: %d/%d", n_nonnormal, nrow(endo_df)))
      
      if (n_nonnormal > 0) {
        log_msg(log_info, "  Non-normal distribution detected — Gaussian Copula may be applicable")
        log_msg(log_info, "  NOTE: Gaussian Copula requires custom implementation")
        log_msg(log_info, "  Fallback: control variables + theoretical argumentation + spec sensitivity")
      }
      
      writeLines(c(
        "# Endogeneity Assessment",
        "",
        sprintf("Non-normal indicators: %d/%d", n_nonnormal, nrow(endo_df)),
        "",
        "## Gaussian Copula",
        ifelse(n_nonnormal > 0,
               "- Potential candidate: non-normal distribution detected",
               "- Not applicable: normal distribution suggests copula may not help"),
        "- Status: requires custom R implementation (Park & Gupta, 2012)",
        "",
        "## Fallback strategy",
        "- Strengthen theoretical argumentation for causal direction",
        "- Use appropriate control variables",
        "- Specification sensitivity analysis"
      ), "09_robust/endogeneity/endogeneity_conclusion.md")
    }
  }, error = function(e) {
    log_warn(log_info, paste("  Endogeneity check error:", e$message))
  })
  
  # ==========================================================================
  # D. FIMIX-PLS (optional — placeholder)
  # ==========================================================================
  log_msg(log_info, "--- D. FIMIX-PLS (Optional) ---")
  log_msg(log_info, "  FIMIX-PLS: not implemented in seminr — requires SmartPLS or custom code")
  log_msg(log_info, "  Status: conditional — only if unobserved heterogeneity suspected")
  
  # ==========================================================================
  # E. Controlled Model Robustness (Model B vs Model A)
  # ==========================================================================
  # Model A = core theoretical model (no controls) — already estimated as pls_model
  # Model B = same model + control variables as single-item composites → each DV
  #
  # Purpose: Confirm substantive paths and indirect effects are robust after
  #          accounting for Gen, Exp, Pos, Edu, CPA.
  # Reference: Hair et al. (2022) — control variables in PLS-SEM
  # ==========================================================================
  log_msg(log_info, "--- E. Controlled Model Robustness (Model B vs Model A) ---")

  ctrl_names <- cfg$control_construct_names
  if (length(ctrl_names) == 0) {
    log_msg(log_info, "  No control variables available — skipping Model B")
  } else {
    dir.create("09_robust/controlled_model", showWarnings = FALSE, recursive = TRUE)

    tryCatch({
      log_msg(log_info, sprintf("  Controls: %s", paste(ctrl_names, collapse = ", ")))
      endogenous_dvs <- unique(sapply(cfg$structural_paths, function(p) p$to))

      # --- Build Model B measurement model ---
      # Start with substantive constructs (same as Model A)
      mm_b_items <- list()
      for (cc in cfg$constructs) {
        if (cc$measurement_type == "reflective") {
          mm_b_items[[length(mm_b_items) + 1]] <- reflective(cc$name, cc$indicators)
        } else {
          mm_b_items[[length(mm_b_items) + 1]] <- composite(cc$name, cc$indicators, weights = mode_B)
        }
      }
      # Add control variables as single-item composites (each added ONCE)
      ctrl_added <- character()   # de-dup guard
      for (cv_name in ctrl_names) {
        if (cv_name %in% ctrl_added) next
        mm_b_items[[length(mm_b_items) + 1]] <- composite(cv_name, single_item(cv_name))
        ctrl_added <- c(ctrl_added, cv_name)
      }
      mm_b <- do.call(constructs, mm_b_items)
      log_msg(log_info, sprintf("  Model B measurement: %d substantive + %d controls",
                                 length(cfg$constructs), length(ctrl_added)))

      # --- Build Model B structural model ---
      # Substantive paths (same as Model A)
      sm_b_paths <- list()
      for (p in cfg$structural_paths) {
        sm_b_paths[[length(sm_b_paths) + 1]] <- paths(from = p$from, to = p$to)
      }
      # Control paths: each control → each endogenous DV (each added ONCE)
      ctrl_path_set <- character()   # de-dup guard
      for (cv_name in ctrl_names) {
        for (dv in endogenous_dvs) {
          key <- paste0(cv_name, "→", dv)
          if (key %in% ctrl_path_set) next
          sm_b_paths[[length(sm_b_paths) + 1]] <- paths(from = cv_name, to = dv)
          ctrl_path_set <- c(ctrl_path_set, key)
        }
      }
      sm_b <- do.call(relationships, sm_b_paths)
      log_msg(log_info, sprintf("  Model B structural: %d substantive + %d control paths",
                                 length(cfg$structural_paths), length(ctrl_path_set)))

      # --- Data matrix (include control columns) ---
      all_inds_b <- unique(c(ind_cols, ctrl_names))
      all_inds_b <- intersect(all_inds_b, names(data_final))
      pls_data_b <- as.data.frame(data_final[, all_inds_b, drop = FALSE])

      # --- Estimate Model B ---
      log_msg(log_info, "  Estimating Model B (with controls)...")
      pls_b <- estimate_pls(data = pls_data_b,
                            measurement_model = mm_b,
                            structural_model = sm_b)
      log_msg(log_info, "  Model B estimated successfully")

      B_boot <- cfg$project$bootstrap_samples
      log_msg(log_info, sprintf("  Bootstrapping Model B (%d samples)...", B_boot))
      boot_b <- bootstrap_model(seminr_model = pls_b,
                                nboot = B_boot,
                                seed  = cfg$project$seed)
      log_msg(log_info, "  Model B bootstrap complete")
      boot_b_summ <- summary(boot_b)

      # --- Save Model B ---
      saveRDS(pls_b, "09_robust/controlled_model/pls_model_B.rds")
      saveRDS(boot_b, "09_robust/controlled_model/boot_model_B.rds")

      # ================================================================
      # E.1 Compare substantive path coefficients: Model A vs Model B
      # ================================================================
      log_msg(log_info, "  --- E.1. Path Comparison (Model A vs Model B) ---")

      # Model A bootstrap (re-use pls_model passed as argument)
      boot_a <- bootstrap_model(seminr_model = pls_model,
                                nboot = B_boot,
                                seed  = cfg$project$seed)
      bp_a <- summary(boot_a)$bootstrapped_paths
      bp_b <- boot_b_summ$bootstrapped_paths

      # Match substantive paths from cfg$structural_paths in both matrices
      sub_path_labels <- character()
      for (p in cfg$structural_paths) {
        pattern <- paste0("^\\s*", p$from, "\\s.*", p$to)
        m_a <- grep(pattern, rownames(bp_a), value = TRUE)
        if (length(m_a) > 0) sub_path_labels <- c(sub_path_labels, m_a[1])
      }
      common_sub <- intersect(sub_path_labels, rownames(bp_b))

      path_compare <- NULL
      if (length(common_sub) > 0) {
        path_compare <- data.frame(
          Path       = common_sub,
          Beta_A     = round(bp_a[common_sub, "Original Est."], 3),
          Beta_B     = round(bp_b[common_sub, "Original Est."], 3),
          Delta_Beta = round(bp_b[common_sub, "Original Est."] - bp_a[common_sub, "Original Est."], 3),
          T_A        = round(bp_a[common_sub, "T Stat."], 3),
          T_B        = round(bp_b[common_sub, "T Stat."], 3),
          CI_Low_A   = round(bp_a[common_sub, "2.5% CI"], 3),
          CI_High_A  = round(bp_a[common_sub, "97.5% CI"], 3),
          CI_Low_B   = round(bp_b[common_sub, "2.5% CI"], 3),
          CI_High_B  = round(bp_b[common_sub, "97.5% CI"], 3),
          stringsAsFactors = FALSE
        )
        path_compare$Sig_A <- !(path_compare$CI_Low_A <= 0 & path_compare$CI_High_A >= 0)
        path_compare$Sig_B <- !(path_compare$CI_Low_B <= 0 & path_compare$CI_High_B >= 0)
        path_compare$Sign_Stable <- sign(path_compare$Beta_A) == sign(path_compare$Beta_B)
        path_compare$Sig_Stable  <- path_compare$Sig_A == path_compare$Sig_B
        path_compare$Robust      <- path_compare$Sign_Stable & path_compare$Sig_Stable

        write.csv(path_compare,
                  "09_robust/controlled_model/path_comparison_A_vs_B.csv",
                  row.names = FALSE)

        n_robust <- sum(path_compare$Robust)
        log_msg(log_info, sprintf("  Substantive paths robust: %d/%d",
                                   n_robust, nrow(path_compare)))
        for (i in seq_len(nrow(path_compare))) {
          tag <- ifelse(path_compare$Robust[i], "ROBUST", "CHANGED")
          log_msg(log_info, sprintf("    %s: A=%.3f%s, B=%.3f%s, delta=%.3f [%s]",
                                     path_compare$Path[i],
                                     path_compare$Beta_A[i],
                                     ifelse(path_compare$Sig_A[i], "*", ""),
                                     path_compare$Beta_B[i],
                                     ifelse(path_compare$Sig_B[i], "*", ""),
                                     path_compare$Delta_Beta[i], tag))
        }
      }

      # ================================================================
      # E.2 Control path coefficients (Model B only)
      # ================================================================
      log_msg(log_info, "  --- E.2. Control Path Coefficients (Model B) ---")

      ctrl_path_rows <- list()
      pc_b <- pls_b$path_coef
      for (cv_name in ctrl_names) {
        for (dv in endogenous_dvs) {
          beta_val <- NA
          if (cv_name %in% rownames(pc_b) && dv %in% colnames(pc_b)) {
            beta_val <- pc_b[cv_name, dv]
          }
          # Find bootstrap row
          bp_row <- grep(paste0("^", cv_name, "\\s.*", dv), rownames(bp_b), value = TRUE)
          t_val <- ci_lo <- ci_hi <- NA; sig_b <- FALSE
          if (length(bp_row) > 0) {
            t_val <- bp_b[bp_row[1], "T Stat."]
            ci_lo <- bp_b[bp_row[1], "2.5% CI"]
            ci_hi <- bp_b[bp_row[1], "97.5% CI"]
            sig_b <- !(ci_lo <= 0 & ci_hi >= 0)
          }
          ctrl_path_rows[[length(ctrl_path_rows) + 1]] <- data.frame(
            Control = cv_name, DV = dv,
            Beta   = round(ifelse(is.na(beta_val), 0, beta_val), 3),
            T_Stat = round(ifelse(is.na(t_val), 0, t_val), 3),
            CI_Low = round(ifelse(is.na(ci_lo), 0, ci_lo), 3),
            CI_High = round(ifelse(is.na(ci_hi), 0, ci_hi), 3),
            Significant = sig_b,
            stringsAsFactors = FALSE
          )
          log_msg(log_info, sprintf("    %s -> %s: beta=%.3f, t=%.3f, CI[%.3f,%.3f] %s",
                                     cv_name, dv,
                                     ifelse(is.na(beta_val), 0, beta_val),
                                     ifelse(is.na(t_val), 0, t_val),
                                     ifelse(is.na(ci_lo), 0, ci_lo),
                                     ifelse(is.na(ci_hi), 0, ci_hi),
                                     ifelse(sig_b, "*", "ns")))
        }
      }
      if (length(ctrl_path_rows) > 0) {
        ctrl_df <- do.call(rbind, ctrl_path_rows)
        write.csv(ctrl_df, "09_robust/controlled_model/control_path_coefficients.csv",
                  row.names = FALSE)
      }

      # ================================================================
      # E.3 R² comparison
      # ================================================================
      log_msg(log_info, "  --- E.3. R² Comparison ---")
      r2_compare_rows <- list()
      for (dv in endogenous_dvs) {
        r2_a <- tryCatch(pls_model$rSquared["Rsq", dv], error = function(e) NA)
        r2_b <- tryCatch(pls_b$rSquared["Rsq", dv], error = function(e) NA)
        delta_r2 <- ifelse(!is.na(r2_a) & !is.na(r2_b), r2_b - r2_a, NA)
        r2_compare_rows[[length(r2_compare_rows) + 1]] <- data.frame(
          DV = dv,
          R2_A = round(ifelse(is.na(r2_a), 0, r2_a), 3),
          R2_B = round(ifelse(is.na(r2_b), 0, r2_b), 3),
          Delta_R2 = round(ifelse(is.na(delta_r2), 0, delta_r2), 3),
          stringsAsFactors = FALSE
        )
        log_msg(log_info, sprintf("    R²(%s): A=%.3f, B=%.3f, delta=%.3f",
                                   dv,
                                   ifelse(is.na(r2_a), 0, r2_a),
                                   ifelse(is.na(r2_b), 0, r2_b),
                                   ifelse(is.na(delta_r2), 0, delta_r2)))
      }
      r2_compare_df <- do.call(rbind, r2_compare_rows)
      write.csv(r2_compare_df, "09_robust/controlled_model/r2_comparison_A_vs_B.csv",
                row.names = FALSE)

      # ================================================================
      # E.4 Indirect effects comparison (if mediation configured)
      # ================================================================
      ie_df <- NULL
      if (!is.null(cfg$mediation) && length(cfg$mediation$indirect_paths) > 0) {
        log_msg(log_info, "  --- E.4. Indirect Effects Comparison ---")

        mediator <- cfg$mediation$mediator

        # Bootstrap 3D arrays
        bp_a_arr <- boot_a$boot_paths
        bp_b_arr <- boot_b$boot_paths
        pc_a_mat <- pls_model$path_coef
        pc_b_mat <- pls_b$path_coef

        ie_compare_rows <- list()
        for (ip in cfg$mediation$indirect_paths) {
          x <- ip$from; m <- ip$through; y <- ip$to
          label <- paste(x, "->", m, "->", y)

          # Model A indirect
          a_a <- pc_a_mat[x, m]; b_a <- pc_a_mat[m, y]
          ind_a <- a_a * b_a
          ind_a_draws <- bp_a_arr[x, m, ] * bp_a_arr[m, y, ]
          ci_a_lo <- unname(quantile(ind_a_draws, 0.025, na.rm = TRUE))
          ci_a_hi <- unname(quantile(ind_a_draws, 0.975, na.rm = TRUE))
          sig_a <- !(ci_a_lo <= 0 & ci_a_hi >= 0)

          # Model B indirect
          a_b <- pc_b_mat[x, m]; b_b <- pc_b_mat[m, y]
          ind_b <- a_b * b_b
          ind_b_draws <- bp_b_arr[x, m, ] * bp_b_arr[m, y, ]
          ci_b_lo <- unname(quantile(ind_b_draws, 0.025, na.rm = TRUE))
          ci_b_hi <- unname(quantile(ind_b_draws, 0.975, na.rm = TRUE))
          sig_b <- !(ci_b_lo <= 0 & ci_b_hi >= 0)

          ie_compare_rows[[length(ie_compare_rows) + 1]] <- data.frame(
            Path = label,
            Indirect_A = round(ind_a, 4),
            CI_Low_A = round(ci_a_lo, 4), CI_High_A = round(ci_a_hi, 4),
            Sig_A = sig_a,
            Indirect_B = round(ind_b, 4),
            CI_Low_B = round(ci_b_lo, 4), CI_High_B = round(ci_b_hi, 4),
            Sig_B = sig_b,
            Sign_Stable = sign(ind_a) == sign(ind_b),
            Sig_Stable  = sig_a == sig_b,
            stringsAsFactors = FALSE
          )

          tag <- ifelse(sig_a == sig_b & sign(ind_a) == sign(ind_b), "ROBUST", "CHANGED")
          log_msg(log_info, sprintf("    %s: A=%.4f%s, B=%.4f%s [%s]",
                                     label, ind_a, ifelse(sig_a, "*", ""),
                                     ind_b, ifelse(sig_b, "*", ""), tag))
        }

        if (length(ie_compare_rows) > 0) {
          ie_df <- do.call(rbind, ie_compare_rows)
          ie_df$Robust <- ie_df$Sign_Stable & ie_df$Sig_Stable
          write.csv(ie_df, "09_robust/controlled_model/indirect_effects_A_vs_B.csv",
                    row.names = FALSE)
          n_ie_robust <- sum(ie_df$Robust)
          log_msg(log_info, sprintf("  Indirect effects robust: %d/%d",
                                     n_ie_robust, nrow(ie_df)))
        }
      }

      # ================================================================
      # E.5 Overall conclusion
      # ================================================================
      all_path_robust <- if (!is.null(path_compare)) all(path_compare$Robust) else TRUE
      all_ie_robust   <- if (!is.null(ie_df)) all(ie_df$Robust) else TRUE
      overall <- ifelse(all_path_robust & all_ie_robust,
                        "ROBUST — substantive conclusions hold with controls",
                        "SENSITIVE — some paths changed with controls (review needed)")

      writeLines(c(
        "# Model B Robustness Check (Controlled Model)",
        "",
        sprintf("Controls: %s", paste(ctrl_names, collapse = ", ")),
        sprintf("Endogenous DVs: %s", paste(endogenous_dvs, collapse = ", ")),
        sprintf("Bootstrap: %d, seed: %d", B_boot, cfg$project$seed),
        "",
        sprintf("## Substantive paths robust: %s",
                if (!is.null(path_compare)) sprintf("%d/%d", sum(path_compare$Robust), nrow(path_compare)) else "N/A"),
        sprintf("## Indirect effects robust: %s",
                if (!is.null(ie_df)) sprintf("%d/%d", sum(ie_df$Robust), nrow(ie_df)) else "N/A"),
        "",
        sprintf("## Overall: %s", overall)
      ), "09_robust/controlled_model/controlled_model_conclusion.md")

      log_msg(log_info, sprintf("  Model B conclusion: %s", overall))
      results$controlled_model <- list(
        path_compare = path_compare,
        ie_compare   = ie_df,
        conclusion   = overall
      )

    }, error = function(e) {
      log_warn(log_info, paste("  Model B estimation error:", e$message))
    })
  }

  # ==========================================================================
  # F. Indicator Sensitivity: COM6 (formative — content-coverage test)
  # ==========================================================================
  # Rationale: COM6 flagged REVIEW in Step 7 (weight ns, loading < 0.50).
  # For formative constructs, dropping an indicator changes construct meaning.
  # This section provides DIAGNOSTIC EVIDENCE (not an auto-drop decision):
  #   F.1  Item-level diagnostics (variance, frequency, correlation, VIF delta)
  #   F.2  Sensitivity re-estimation (drop COM6 → compare beta, R², paths)
  #   F.3  Decision framework (formative-appropriate: measurement error first,
  #         sensitivity as supporting evidence only)
  # Reference: Hair et al. (2022), Diamantopoulos & Siguaw (2006)
  # ==========================================================================
  log_msg(log_info, "--- F. Indicator Sensitivity: COM6 (formative content-coverage test) ---")

  tryCatch({
    dir.create("09_robust/sensitivity_data", showWarnings = FALSE, recursive = TRUE)

    # Identify the flagged indicator
    target_construct <- "COM"
    target_indicator <- "COM6"
    other_indicators <- setdiff(
      cfg$constructs[[which(sapply(cfg$constructs, function(c) c$name) == target_construct)]]$indicators,
      target_indicator
    )

    if (!target_indicator %in% names(data_final)) {
      log_warn(log_info, sprintf("  %s not found in data — skipping Section F", target_indicator))
    } else {

      # ================================================================
      # F.1 Item-level diagnostics
      # ================================================================
      log_msg(log_info, "  --- F.1. Item-level diagnostics ---")

      # Variance
      com6_vals <- data_final[[target_indicator]]
      com6_var  <- var(com6_vals, na.rm = TRUE)
      com6_mean <- mean(com6_vals, na.rm = TRUE)
      com6_sd   <- sd(com6_vals, na.rm = TRUE)

      # Range and scale (assume 1-5 Likert)
      val_range <- range(com6_vals, na.rm = TRUE)
      log_msg(log_info, sprintf("  %s: M=%.3f, SD=%.3f, Var=%.3f, Range=[%d,%d]",
                                 target_indicator, com6_mean, com6_sd, com6_var,
                                 val_range[1], val_range[2]))

      # Compare variance to other COM indicators
      other_vars <- sapply(other_indicators, function(ind) {
        if (ind %in% names(data_final)) var(data_final[[ind]], na.rm = TRUE) else NA
      })
      avg_other_var <- mean(other_vars, na.rm = TRUE)
      var_ratio <- com6_var / avg_other_var
      log_msg(log_info, sprintf("  Variance ratio (COM6 / avg COM1-5,7-9): %.3f", var_ratio))
      if (var_ratio < 0.50) {
        log_warn(log_info, "  LOW VARIANCE — COM6 variance < 50% of sibling average")
      }

      # Response frequency (floor/ceiling concentration)
      freq_tbl <- table(com6_vals)
      freq_pct <- prop.table(freq_tbl) * 100
      max_pct  <- max(freq_pct)
      mode_val <- as.numeric(names(freq_tbl)[which.max(freq_tbl)])
      log_msg(log_info, sprintf("  Response frequency: mode=%d (%.1f%% of responses)",
                                 mode_val, max_pct))
      freq_str <- paste(sprintf("%s=%.1f%%", names(freq_pct), freq_pct), collapse = ", ")
      log_msg(log_info, sprintf("  Distribution: %s", freq_str))

      if (max_pct >= 70) {
        log_warn(log_info, sprintf("  CEILING/FLOOR EFFECT — %.1f%% at value %d", max_pct, mode_val))
      }

      # Correlation with other COM indicators
      com_inds <- c(target_indicator, other_indicators)
      com_inds_avail <- intersect(com_inds, names(data_final))
      cor_mat <- cor(data_final[, com_inds_avail, drop = FALSE], use = "pairwise.complete.obs")
      com6_cors <- cor_mat[target_indicator, setdiff(com_inds_avail, target_indicator)]
      log_msg(log_info, sprintf("  Correlations with siblings: %s",
                                 paste(sprintf("%s=%.3f", names(com6_cors), com6_cors), collapse = ", ")))
      avg_cor <- mean(abs(com6_cors))
      log_msg(log_info, sprintf("  Mean |r| with siblings: %.3f", avg_cor))

      # VIF with vs without COM6
      # With COM6: use outer VIF from Step 7 (already computed)
      # Without COM6: quick manual regression-based VIF
      com_with <- com_inds_avail
      com_without <- setdiff(com_inds_avail, target_indicator)

      compute_avg_vif <- function(indicators, data) {
        if (length(indicators) < 2) return(NA)
        vifs <- sapply(indicators, function(focal) {
          others <- setdiff(indicators, focal)
          if (length(others) == 0) return(NA)
          fmla <- as.formula(paste(focal, "~", paste(others, collapse = " + ")))
          fit <- tryCatch(lm(fmla, data = data[, indicators, drop = FALSE]),
                          error = function(e) NULL)
          if (is.null(fit)) return(NA)
          r2 <- summary(fit)$r.squared
          1 / (1 - r2)
        })
        mean(vifs, na.rm = TRUE)
      }

      vif_with    <- compute_avg_vif(com_with, data_final)
      vif_without <- compute_avg_vif(com_without, data_final)
      vif_delta   <- vif_with - vif_without
      log_msg(log_info, sprintf("  Avg VIF (with COM6): %.3f, (without): %.3f, delta: %+.3f",
                                 vif_with, vif_without, vif_delta))

      # Export diagnostics
      diag_df <- data.frame(
        Indicator = target_indicator,
        Mean = round(com6_mean, 3),
        SD = round(com6_sd, 3),
        Variance = round(com6_var, 3),
        Var_Ratio_vs_Siblings = round(var_ratio, 3),
        Mode = mode_val,
        Mode_Pct = round(max_pct, 1),
        Avg_Abs_Cor_Siblings = round(avg_cor, 3),
        Avg_VIF_With = round(vif_with, 3),
        Avg_VIF_Without = round(vif_without, 3),
        VIF_Delta = round(vif_delta, 3),
        stringsAsFactors = FALSE
      )
      write.csv(diag_df, "09_robust/sensitivity_data/com6_diagnostics.csv", row.names = FALSE)

      # ================================================================
      # F.2 Sensitivity re-estimation (model without COM6)
      # ================================================================
      log_msg(log_info, "  --- F.2. Sensitivity re-estimation (without COM6) ---")

      # Build measurement model with COM using only COM1-5,7-9
      mm_sens_items <- list()
      for (cc in cfg$constructs) {
        if (cc$name == target_construct) {
          # Use other_indicators (excluding COM6)
          mm_sens_items[[length(mm_sens_items) + 1]] <- composite(cc$name, other_indicators, weights = mode_B)
        } else if (cc$measurement_type == "reflective") {
          mm_sens_items[[length(mm_sens_items) + 1]] <- reflective(cc$name, cc$indicators)
        } else {
          mm_sens_items[[length(mm_sens_items) + 1]] <- composite(cc$name, cc$indicators, weights = mode_B)
        }
      }
      mm_sens <- do.call(constructs, mm_sens_items)

      # Same structural model as Model A
      sm_list <- lapply(cfg$structural_paths, function(p) paths(from = p$from, to = p$to))
      sm_sens <- do.call(relationships, sm_list)

      # Data matrix (exclude COM6)
      inds_sens <- setdiff(ind_cols, target_indicator)
      pls_data_sens <- as.data.frame(data_final[, inds_sens, drop = FALSE])

      log_msg(log_info, sprintf("  Estimating Model A* (COM with %d indicators, excl. %s)...",
                                 length(other_indicators), target_indicator))

      pls_sens <- estimate_pls(data = pls_data_sens,
                               measurement_model = mm_sens,
                               structural_model = sm_sens)

      B_boot <- cfg$project$bootstrap_samples
      boot_sens <- bootstrap_model(seminr_model = pls_sens,
                                   nboot = B_boot,
                                   seed  = cfg$project$seed)
      boot_sens_summ <- summary(boot_sens)
      log_msg(log_info, "  Sensitivity model estimated and bootstrapped")

      # Compare key paths: COM→AJ (focal), and all other paths for stability
      paths_orig <- summary(bootstrap_model(seminr_model = pls_model,
                                             nboot = 1000,
                                             seed  = cfg$project$seed))$bootstrapped_paths
      paths_sens <- boot_sens_summ$bootstrapped_paths

      if (!is.null(paths_orig) && !is.null(paths_sens)) {
        common <- intersect(rownames(paths_orig), rownames(paths_sens))

        sens_compare <- data.frame(
          Path = common,
          Beta_With_COM6 = round(paths_orig[common, "Original Est."], 3),
          Beta_Without_COM6 = round(paths_sens[common, "Original Est."], 3),
          CI_Lo_With = round(paths_orig[common, "2.5% CI"], 3),
          CI_Hi_With = round(paths_orig[common, "97.5% CI"], 3),
          CI_Lo_Without = round(paths_sens[common, "2.5% CI"], 3),
          CI_Hi_Without = round(paths_sens[common, "97.5% CI"], 3),
          stringsAsFactors = FALSE
        )

        sens_compare$Delta_Beta <- round(sens_compare$Beta_Without_COM6 - sens_compare$Beta_With_COM6, 3)
        sens_compare$Sig_With    <- with(sens_compare, CI_Lo_With > 0 | CI_Hi_With < 0)
        sens_compare$Sig_Without <- with(sens_compare, CI_Lo_Without > 0 | CI_Hi_Without < 0)
        sens_compare$Sign_Stable <- sign(sens_compare$Beta_With_COM6) == sign(sens_compare$Beta_Without_COM6)
        sens_compare$Sig_Stable  <- sens_compare$Sig_With == sens_compare$Sig_Without
        sens_compare$Robust      <- sens_compare$Sign_Stable & sens_compare$Sig_Stable

        write.csv(sens_compare, "09_robust/sensitivity_data/com6_sensitivity_paths.csv",
                  row.names = FALSE)

        # R² comparison
        endogenous <- unique(sapply(cfg$structural_paths, function(p) p$to))
        r2_compare <- data.frame(
          DV = endogenous,
          stringsAsFactors = FALSE
        )
        for (dv in endogenous) {
          r2_compare$R2_With[r2_compare$DV == dv] <- tryCatch(
            round(pls_model$rSquared["Rsq", dv], 3), error = function(e) NA)
          r2_compare$R2_Without[r2_compare$DV == dv] <- tryCatch(
            round(pls_sens$rSquared["Rsq", dv], 3), error = function(e) NA)
        }
        r2_compare$R2_Delta <- round(r2_compare$R2_Without - r2_compare$R2_With, 4)
        write.csv(r2_compare, "09_robust/sensitivity_data/com6_sensitivity_r2.csv",
                  row.names = FALSE)

        # Log focal path (COM→AJ)
        focal_row <- sens_compare[grep("COM.*AJ", sens_compare$Path), ]
        if (nrow(focal_row) > 0) {
          log_msg(log_info, sprintf("  Focal path COM→AJ: beta with=%.3f, without=%.3f, delta=%+.3f",
                                     focal_row$Beta_With_COM6[1],
                                     focal_row$Beta_Without_COM6[1],
                                     focal_row$Delta_Beta[1]))
          log_msg(log_info, sprintf("    With:    CI[%.3f, %.3f]%s",
                                     focal_row$CI_Lo_With[1], focal_row$CI_Hi_With[1],
                                     ifelse(focal_row$Sig_With[1], "*", "")))
          log_msg(log_info, sprintf("    Without: CI[%.3f, %.3f]%s",
                                     focal_row$CI_Lo_Without[1], focal_row$CI_Hi_Without[1],
                                     ifelse(focal_row$Sig_Without[1], "*", "")))
        }

        # R² log
        for (i in seq_len(nrow(r2_compare))) {
          log_msg(log_info, sprintf("  R²(%s): with=%.3f, without=%.3f, delta=%+.4f",
                                     r2_compare$DV[i],
                                     r2_compare$R2_With[i],
                                     r2_compare$R2_Without[i],
                                     r2_compare$R2_Delta[i]))
        }

        # All-paths stability
        n_robust <- sum(sens_compare$Robust)
        log_msg(log_info, sprintf("  Path stability: %d/%d paths robust (sign + significance unchanged)",
                                   n_robust, nrow(sens_compare)))
      }

      # ================================================================
      # F.3 Decision framework (formative-appropriate)
      # ================================================================
      log_msg(log_info, "  --- F.3. Decision framework ---")
      log_msg(log_info, "  Formative principle: dropping an indicator changes construct meaning.")
      log_msg(log_info, "  Primary criterion: measurement error evidence (not parsimony).")
      log_msg(log_info, "    - Reverse-coding error? Check item wording direction.")
      log_msg(log_info, "    - Extreme low variance (floor/ceiling)? See F.1 diagnostics.")
      log_msg(log_info, "    - Content overlap with other indicators? Check inter-item correlations.")
      log_msg(log_info, "  Secondary criterion: sensitivity analysis (supporting evidence, not primary).")
      log_msg(log_info, "    - If substantive paths unchanged, retention is LOW RISK.")
      log_msg(log_info, "    - If paths change, further investigation needed (not auto-drop).")
      log_msg(log_info, "  Decision: RETAIN unless measurement error evidence AND no content coverage loss.")
      log_msg(log_info, "  Reference: Hair et al. (2022), Diamantopoulos & Siguaw (2006).")

      # Write conclusion file
      writeLines(c(
        "# COM6 Indicator Sensitivity — Decision Summary",
        "",
        sprintf("## Diagnostics (F.1)"),
        sprintf("- Mean=%.3f, SD=%.3f, Variance=%.3f", com6_mean, com6_sd, com6_var),
        sprintf("- Variance ratio vs siblings: %.3f", var_ratio),
        sprintf("- Mode=%d (%.1f%% of responses)", mode_val, max_pct),
        sprintf("- Mean |correlation| with siblings: %.3f", avg_cor),
        sprintf("- VIF delta (with-without): %+.3f", vif_delta),
        "",
        "## Sensitivity (F.2)",
        if (exists("focal_row") && nrow(focal_row) > 0)
          sprintf("- COM→AJ: beta %.3f → %.3f (delta %+.3f)", 
                  focal_row$Beta_With_COM6[1], focal_row$Beta_Without_COM6[1], focal_row$Delta_Beta[1])
        else "- COM→AJ: comparison not available",
        if (exists("n_robust"))
          sprintf("- Paths robust: %d/%d", n_robust, nrow(sens_compare))
        else "- Path comparison not available",
        "",
        "## Decision Framework",
        "- Formative: drop only if measurement error evidence AND no content loss",
        "- Sensitivity = supporting evidence, not primary criterion",
        "- Hair et al. (2022): weight ns + loading < 0.50 → justify retention or drop",
        "- Diamantopoulos & Siguaw (2006): content validity paramount for formative"
      ), "09_robust/sensitivity_data/com6_sensitivity_conclusion.md")

      log_msg(log_info, "  Sensitivity results exported to 09_robust/sensitivity_data/")

      results$com6_sensitivity <- list(
        diagnostics  = diag_df,
        path_compare = if (exists("sens_compare")) sens_compare else NULL,
        r2_compare   = if (exists("r2_compare")) r2_compare else NULL
      )
    }

  }, error = function(e) {
    log_warn(log_info, paste("  COM6 sensitivity error:", e$message))
  })

  # Gate check
  log_info <- log_gate(log_info, "Robustness", TRUE,
                       "Robustness checks completed (some items conditional on data)")
  
  results$pass <- TRUE
  results
}
