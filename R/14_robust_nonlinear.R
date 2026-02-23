# ==============================================================================
# 14_robust_nonlinear.R — Quadratic / Nonlinear Effects Robustness Check
# ==============================================================================
# Standalone module.  Does NOT modify any existing pipeline file.
#
# Purpose
# -------
# Test whether adding a quadratic term X² materially changes the linear
# conclusions from the base PLS-SEM model (Hair et al., 2022).
#
# Method
# ------
# Two-stage score-based approach (Henseler & Chin, 2010):
#   Stage 1 — reuse construct scores from the base PLS model (SEMinR).
#   Stage 2 — for each (X, Y) relation:
#     1. Centre X scores (mean-centre or z-score).
#     2. Compute X² = Xc².
#     3. Fit Y ~ all_linear_predictors + X²  via OLS.
#     4. Bootstrap (case-resampling) for percentile CI on every coefficient.
#     5. Compare β_linear and R² with vs. without X².
#
# Outputs → 09_robust/nonlinear/
#   nonlinear_quadratic_results.csv   (thesis-ready table)
#   nonlinear_quadratic_conclusion.md (interpretive summary)
#   nonlinear_plot_<X>_<Y>.png        (per relation, if quadratic significant)
#
# Running standalone
# ------------------
#   setwd("d:/Works/Data analysis/C4")
#   source("R/utils_logging.R")
#   source("R/utils_inference.R")
#   source("R/14_robust_nonlinear.R")
#   run_robust_nonlinear()            # uses default config path
#
# Or from an existing pipeline log_info:
#   run_robust_nonlinear(log_info = log_info)
# ==============================================================================

suppressPackageStartupMessages({
  library(yaml)
  library(seminr)
  library(ggplot2)
})

# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================

#' Light-weight logger when running standalone (no pipeline log_info)
.nl_standalone_log <- function(msg, level = "INFO") {
  ts   <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  line <- paste(ts, paste0("[", level, "]"), msg)
  cat(line, "\n")
}

#' Unified logging — use pipeline logger if available, else standalone
.nl_log <- function(log_info, msg, level = "INFO") {
  con_ok <- tryCatch(
    !is.null(log_info) && is.list(log_info) &&
      !is.null(log_info$con) && isOpen(log_info$con),
    error = function(e) FALSE
  )
  if (con_ok) {
    log_msg(log_info, msg, level = level)
  } else {
    .nl_standalone_log(msg, level)
  }
}

.nl_warn <- function(log_info, msg) .nl_log(log_info, msg, "WARN")

#' Centre a numeric vector
.nl_centre <- function(x, method = "mean_center") {
  if (method == "z_score") {
    s <- sd(x, na.rm = TRUE)
    if (is.na(s) || s == 0) return(x - mean(x, na.rm = TRUE))
    return((x - mean(x, na.rm = TRUE)) / s)
  }
  x - mean(x, na.rm = TRUE)
}

#' Classify f² effect size per Cohen (1988)
#' @param f2 numeric f² value
#' @return character classification
.nl_classify_f2 <- function(f2) {
  if (is.na(f2) || !is.finite(f2) || f2 < 0) return("N/A")
  if (f2 >= 0.35) return("Large")
  if (f2 >= 0.15) return("Medium")
  if (f2 >= 0.02) return("Small")
  "Negligible"
}

# ==============================================================================
# CORE: BOOTSTRAP STAGE-2 REGRESSION
# ==============================================================================

#' Case-resampling bootstrap for OLS regression
#'
#' @param df       data.frame with columns used in formula
#' @param formula  formula object  (Y ~ X1 + X2 + ... + X2_sq)
#' @param B        number of bootstrap resamples
#' @param seed     RNG seed
#' @param ci_level confidence level (0.95)
#' @return list(ols_fit, coef_boot_matrix, ci_lo, ci_hi, p_boot)
.nl_boot_ols <- function(df, formula, B = 5000, seed = 18, ci_level = 0.95) {
  set.seed(seed)
  n <- nrow(df)

  # Point estimate (full sample)
  ols_fit <- lm(formula, data = df)
  coef_names <- names(coef(ols_fit))
  k <- length(coef_names)

  # Bootstrap matrix
  boot_mat <- matrix(NA_real_, nrow = B, ncol = k)
  colnames(boot_mat) <- coef_names

  for (b in seq_len(B)) {
    idx <- sample.int(n, replace = TRUE)
    fit_b <- tryCatch(lm(formula, data = df[idx, , drop = FALSE]),
                       error = function(e) NULL)
    if (!is.null(fit_b)) {
      cb <- coef(fit_b)
      boot_mat[b, names(cb)] <- cb
    }
  }

  # Percentile CI
  alpha <- 1 - ci_level
  ci_lo <- apply(boot_mat, 2, quantile, probs = alpha / 2,   na.rm = TRUE)
  ci_hi <- apply(boot_mat, 2, quantile, probs = 1 - alpha / 2, na.rm = TRUE)

  # Empirical p (two-tailed, continuity-corrected)
  p_boot <- sapply(seq_len(k), function(j) {
    draws <- boot_mat[, j]
    draws <- draws[is.finite(draws)]
    Bj <- length(draws)
    if (Bj < 2) return(NA_real_)
    kj <- sum(draws <= 0)
    min(2 * min((kj + 1) / (Bj + 1), (Bj - kj + 1) / (Bj + 1)), 1)
  })
  names(p_boot) <- coef_names

  list(
    ols_fit  = ols_fit,
    boot_mat = boot_mat,
    ci_lo    = ci_lo,
    ci_hi    = ci_hi,
    p_boot   = p_boot
  )
}

# ==============================================================================
# BUILD SCORE DATA FRAME + LINEAR PREDICTORS FOR ONE DV
# ==============================================================================

#' Assemble a score-level data.frame for a given DV
#'
#' @param scores     matrix of construct scores (N x C) from SEMinR
#' @param dv         name of the dependent variable construct
#' @param cfg        config object (for structural_paths)
#' @param ctrl_names character vector of control variables in data_final
#' @param data_final data.frame (only needed for controls columns)
#' @return data.frame with DV, all its predictors, and control columns
.nl_build_score_df <- function(scores, dv, cfg, ctrl_names = character(),
                                data_final = NULL) {
  # Identify all direct predictors of dv from structural paths
  preds <- unique(sapply(
    Filter(function(p) p$to == dv, cfg$structural_paths),
    function(p) p$from
  ))

  cols_needed <- c(dv, preds)
  missing_scores <- setdiff(cols_needed, colnames(scores))
  if (length(missing_scores) > 0) {
    stop(sprintf("Scores missing for constructs: %s",
                 paste(missing_scores, collapse = ", ")))
  }

  df <- as.data.frame(scores[, cols_needed, drop = FALSE])

  # Attach control columns if available
  if (length(ctrl_names) > 0 && !is.null(data_final)) {
    ctrl_available <- intersect(ctrl_names, names(data_final))
    if (length(ctrl_available) > 0) {
      # Ensure row alignment (scores is N rows, same order as data_final rows)
      df <- cbind(df, data_final[, ctrl_available, drop = FALSE])
    }
  }

  list(df = df, predictors = preds)
}

# ==============================================================================
# MAIN ENTRY POINT
# ==============================================================================

#' Run Quadratic / Nonlinear Effects Robustness Check
#'
#' @param config_path  path to robustness_nonlinear.yml
#' @param log_info     pipeline log_info list (optional; standalone if NULL)
#' @param cfg          base pipeline config (optional; loaded from YAML if NULL)
#' @return invisible list of results
run_robust_nonlinear <- function(config_path = "config/robustness_nonlinear.yml",
                                  log_info    = NULL,
                                  cfg         = NULL) {

  t_start <- Sys.time()

  # --------------------------------------------------------------------------
  # 0. Load module config
  # --------------------------------------------------------------------------
  if (!file.exists(config_path)) {
    stop("Nonlinear robustness config not found: ", config_path)
  }
  nl_cfg <- yaml::read_yaml(config_path)

  if (!isTRUE(nl_cfg$enabled)) {
    .nl_log(log_info, "Nonlinear robustness module: DISABLED (enabled=false in config)")
    return(invisible(NULL))
  }

  # Parameters (fall back to pipeline defaults)
  B_boot      <- if (!is.null(nl_cfg$bootstrap_samples)) nl_cfg$bootstrap_samples else 5000
  seed        <- if (!is.null(nl_cfg$seed))              nl_cfg$seed              else 18
  ci_level    <- if (!is.null(nl_cfg$confidence_level))  nl_cfg$confidence_level  else 0.95
  centre_meth <- if (!is.null(nl_cfg$center_method))     nl_cfg$center_method     else "mean_center"
  output_dir  <- if (!is.null(nl_cfg$output_dir))        nl_cfg$output_dir        else "09_robust/nonlinear"

  relations <- nl_cfg$relations
  if (is.null(relations) || length(relations) == 0) {
    .nl_log(log_info, "No relations specified — nonlinear check skipped")
    return(invisible(NULL))
  }

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # --------------------------------------------------------------------------
  # 1. Load base artifacts
  # --------------------------------------------------------------------------
  .nl_log(log_info, "")
  .nl_log(log_info, ">>> Robustness: Quadratic / Nonlinear Effects Check")
  .nl_log(log_info, paste(rep("-", 50), collapse = ""))
  .nl_log(log_info, sprintf("  Seed: %d, Bootstrap: %d, CI: %s %.0f%%",
                              seed, B_boot, "percentile", ci_level * 100))
  .nl_log(log_info, sprintf("  Center method: %s", centre_meth))
  .nl_log(log_info, sprintf("  Relations to test: %d", length(relations)))

  pls_path  <- if (!is.null(nl_cfg$inputs$pls_model_path))
    nl_cfg$inputs$pls_model_path else "06_structural/pls_model.rds"
  data_path <- if (!is.null(nl_cfg$inputs$data_path))
    nl_cfg$inputs$data_path else "02_clean/analysis_ready.rds"

  if (!file.exists(pls_path)) stop("Base PLS model not found: ", pls_path)
  if (!file.exists(data_path)) stop("Analysis-ready data not found: ", data_path)

  pls_model  <- readRDS(pls_path)
  data_final <- readRDS(data_path)

  # Load base pipeline cfg if not provided
  if (is.null(cfg)) {
    if (file.exists("00_meta/analysis_config.yaml")) {
      source("R/00_config.R")
      cfg <- load_config("00_meta/analysis_config.yaml")
      # Apply freeze rules from main.yml if present
      if (file.exists("config/main.yml")) {
        main_cfg <- yaml::read_yaml("config/main.yml")
        if (!is.null(main_cfg$freeze_rules)) {
          for (rule in main_cfg$freeze_rules) {
            cn  <- rule$construct
            ind <- rule$indicator
            if (rule$action == "drop") {
              cfg$constructs <- lapply(cfg$constructs, function(cc) {
                if (cc$name == cn)
                  cc$indicators <- setdiff(cc$indicators, ind)
                cc
              })
            }
          }
          cfg$all_indicators <- unlist(lapply(cfg$constructs, function(c) c$indicators))
        }
      }
    } else {
      stop("Cannot load base config — 00_meta/analysis_config.yaml not found")
    }
  }

  # --------------------------------------------------------------------------
  # 2. Extract construct scores from base PLS model (Stage 1)
  # --------------------------------------------------------------------------
  scores <- tryCatch({
    # SEMinR stores scores as a matrix: N rows x C constructs
    sc <- pls_model$construct_scores
    if (is.null(sc)) stop("construct_scores slot is NULL")
    sc
  }, error = function(e) {
    .nl_warn(log_info, sprintf("Cannot extract scores from saved model: %s", e$message))
    .nl_log(log_info, "  Re-estimating base PLS model to obtain construct scores...")

    # Rebuild measurement + structural model from cfg
    mm_items <- list()
    for (cc in cfg$constructs) {
      inds <- cc$indicators
      if (length(inds) == 0) next
      if (cc$measurement_type == "reflective") {
        mm_items[[length(mm_items) + 1]] <- reflective(cc$name, inds)
      } else {
        mm_items[[length(mm_items) + 1]] <- composite(cc$name, inds, weights = mode_B)
      }
    }
    mm <- do.call(constructs, mm_items)
    sm_list <- lapply(cfg$structural_paths, function(p) paths(from = p$from, to = p$to))
    sm <- do.call(relationships, sm_list)

    ind_cols <- intersect(cfg$all_indicators, names(data_final))
    pls_data <- as.data.frame(data_final[, ind_cols, drop = FALSE])
    pls_re <- estimate_pls(data = pls_data, measurement_model = mm, structural_model = sm)
    pls_re$construct_scores
  })

  .nl_log(log_info, sprintf("  Construct scores: %d observations x %d constructs",
                              nrow(scores), ncol(scores)))

  # Detect control variable columns
  ctrl_names <- character()
  if (!is.null(cfg$control_vars) && length(cfg$control_vars) > 0) {
    ctrl_names <- sapply(cfg$control_vars, function(cv) cv$target_column)
    ctrl_names <- intersect(ctrl_names, names(data_final))
    if (length(ctrl_names) > 0) {
      .nl_log(log_info, sprintf("  Controls included in Stage 2: %s",
                                  paste(ctrl_names, collapse = ", ")))
    }
  }

  # --------------------------------------------------------------------------
  # 3. Base linear R² (for delta comparison)
  # --------------------------------------------------------------------------
  base_r2 <- list()
  endogenous <- unique(sapply(cfg$structural_paths, function(p) p$to))
  for (dv in endogenous) {
    base_r2[[dv]] <- tryCatch(
      pls_model$rSquared["Rsq", dv], error = function(e) NA_real_)
  }

  # --------------------------------------------------------------------------
  # 4. Loop over each relation
  # --------------------------------------------------------------------------
  all_results <- list()

  for (ri in seq_along(relations)) {
    rel <- relations[[ri]]
    x_name <- rel$iv
    y_name <- rel$dv
    label  <- if (!is.null(rel$label)) rel$label else paste0(x_name, " -> ", y_name)

    .nl_log(log_info, "")
    .nl_log(log_info, sprintf("--- Testing: %s (quadratic) ---", label))

    # 4a. Build score-level data
    tryCatch({
      build <- .nl_build_score_df(scores, y_name, cfg, ctrl_names, data_final)
      df_stage2 <- build$df
      preds     <- build$predictors

      # 4b. Centre and compute quadratic term
      xc_col <- paste0(x_name, "_c")
      x2_col <- paste0(x_name, "_sq")
      df_stage2[[xc_col]] <- .nl_centre(df_stage2[[x_name]], centre_meth)
      df_stage2[[x2_col]] <- df_stage2[[xc_col]]^2

      # Remove original X column to avoid aliasing with Xc
      # (Xc is used as the linear effect; X² as the quadratic)
      df_stage2[[x_name]] <- NULL

      # Rename Xc back to X name for clean formula labelling
      names(df_stage2)[names(df_stage2) == xc_col] <- x_name

      .nl_log(log_info, sprintf("  %s: M=%.3f (centred), %s: M=%.3f",
                                  x_name,
                                  mean(df_stage2[[x_name]], na.rm = TRUE),
                                  x2_col,
                                  mean(df_stage2[[x2_col]], na.rm = TRUE)))

      # 4c. Build formulas
      # Linear-only model (baseline for ΔR²)
      rhs_lin <- c(preds, ctrl_names)
      # Replace x_name in preds with centred version (already renamed)
      fml_lin_str <- paste(y_name, "~", paste(rhs_lin, collapse = " + "))
      fml_lin <- as.formula(fml_lin_str)

      # Quadratic model: add X²
      rhs_quad <- c(rhs_lin, x2_col)
      fml_quad_str <- paste(y_name, "~", paste(rhs_quad, collapse = " + "))
      fml_quad <- as.formula(fml_quad_str)

      .nl_log(log_info, sprintf("  Linear:    %s", fml_lin_str))
      .nl_log(log_info, sprintf("  Quadratic: %s", fml_quad_str))

      # 4d. Bootstrap linear model
      .nl_log(log_info, sprintf("  Bootstrapping linear model (B=%d)...", B_boot))
      res_lin <- .nl_boot_ols(df_stage2, fml_lin, B = B_boot,
                               seed = seed, ci_level = ci_level)

      # 4e. Bootstrap quadratic model
      .nl_log(log_info, sprintf("  Bootstrapping quadratic model (B=%d)...", B_boot))
      res_quad <- .nl_boot_ols(df_stage2, fml_quad, B = B_boot,
                                seed = seed, ci_level = ci_level)

      # 4f. Extract results
      coef_lin  <- coef(res_lin$ols_fit)
      coef_quad <- coef(res_quad$ols_fit)
      r2_lin    <- summary(res_lin$ols_fit)$r.squared
      r2_quad   <- summary(res_quad$ols_fit)$r.squared

      beta_lin_base   <- coef_lin[x_name]
      beta_lin_quad   <- coef_quad[x_name]
      beta_quadratic  <- coef_quad[x2_col]

      ci_lin_lo  <- res_quad$ci_lo[x_name]
      ci_lin_hi  <- res_quad$ci_hi[x_name]
      ci_q_lo    <- res_quad$ci_lo[x2_col]
      ci_q_hi    <- res_quad$ci_hi[x2_col]

      sig_lin    <- infer_sig_ci(ci_lin_lo, ci_lin_hi)
      sig_quad   <- infer_sig_ci(ci_q_lo, ci_q_hi)

      p_lin  <- res_quad$p_boot[x_name]
      p_quad <- res_quad$p_boot[x2_col]

      delta_beta <- beta_lin_quad - beta_lin_base
      delta_r2   <- r2_quad - r2_lin

      # f² effect size for the quadratic term (Hair et al., 2021; Haans et al., 2016)
      # f² = (R²_full - R²_reduced) / (1 - R²_full)
      # where full = linear + X², reduced = linear-only
      f2_quad <- ifelse(r2_quad < 1,
                        delta_r2 / (1 - r2_quad),
                        NA_real_)
      f2_class <- .nl_classify_f2(f2_quad)

      # VIF of quadratic term
      vif_quad <- NA_real_
      tryCatch({
        # Simple VIF from R²_j
        fml_vif <- as.formula(paste(x2_col, "~", paste(rhs_lin, collapse = " + ")))
        r2j <- summary(lm(fml_vif, data = df_stage2))$r.squared
        vif_quad <- 1 / (1 - r2j)
      }, error = function(e) NULL)

      # Note construction
      note <- ""
      if (isTRUE(sig_quad)) {
        note <- "Evidence of nonlinearity — "
        if (isTRUE(sig_lin)) {
          if (sign(beta_lin_quad) == sign(beta_lin_base)) {
            note <- paste0(note,
              "linear effect stable (same sign/sig); quadratic adds curvature")
          } else {
            note <- paste0(note,
              "WARNING: linear effect reverses sign — investigate further")
          }
        } else {
          note <- paste0(note,
            "linear effect becomes non-significant in quadratic model")
        }
      } else {
        if (isTRUE(sig_lin) || isTRUE(infer_sig_ci(
              res_lin$ci_lo[x_name], res_lin$ci_hi[x_name]))) {
          note <- "Linear conclusion robust — no significant quadratic effect"
        } else {
          note <- "Neither linear nor quadratic significant"
        }
      }

      # Log
      .nl_log(log_info, sprintf("  beta_linear (quad model): %.3f, CI[%.3f, %.3f]%s (p=%s)",
                                  beta_lin_quad, ci_lin_lo, ci_lin_hi,
                                  ifelse(isTRUE(sig_lin), "*", ""),
                                  fmt_p(p_lin)))
      .nl_log(log_info, sprintf("  beta_quadratic:           %.3f, CI[%.3f, %.3f]%s (p=%s)",
                                  beta_quadratic, ci_q_lo, ci_q_hi,
                                  ifelse(isTRUE(sig_quad), "*", ""),
                                  fmt_p(p_quad)))
      .nl_log(log_info, sprintf("  VIF(%s): %.2f",
                                  x2_col, ifelse(is.na(vif_quad), 0, vif_quad)))
      .nl_log(log_info, sprintf("  R²(linear): %.3f, R²(quadratic): %.3f, deltaR²: %+.4f",
                                  r2_lin, r2_quad, delta_r2))
      .nl_log(log_info, sprintf("  f²(quadratic): %.4f [%s]  (Cohen 1988: >=.02 Small, >=.15 Medium, >=.35 Large)",
                                  ifelse(is.na(f2_quad), 0, f2_quad), f2_class))
      .nl_log(log_info, sprintf("  delta(beta_linear): %+.4f (quad model vs linear-only)",
                                  delta_beta))
      .nl_log(log_info, sprintf("  => %s", note))

      # 4g. Store result
      all_results[[ri]] <- data.frame(
        Relation             = label,
        X                    = x_name,
        Y                    = y_name,
        Beta_Linear_Base     = round(beta_lin_base, 3),
        Beta_Linear_QuadModel = round(beta_lin_quad, 3),
        CI_Linear_Lo         = round(ci_lin_lo, 3),
        CI_Linear_Hi         = round(ci_lin_hi, 3),
        Sig_Linear           = sig_lin,
        P_Linear             = round(p_lin, 4),
        Beta_Quadratic       = round(beta_quadratic, 3),
        CI_Quad_Lo           = round(ci_q_lo, 3),
        CI_Quad_Hi           = round(ci_q_hi, 3),
        Sig_Quadratic        = sig_quad,
        P_Quadratic          = round(p_quad, 4),
        VIF_Quadratic        = round(vif_quad, 2),
        Delta_Beta_Linear    = round(delta_beta, 4),
        R2_Linear            = round(r2_lin, 3),
        R2_Quadratic         = round(r2_quad, 3),
        Delta_R2             = round(delta_r2, 4),
        f2_Quadratic         = round(f2_quad, 4),
        f2_Classification    = f2_class,
        R2_Base_PLS          = round(ifelse(y_name %in% names(base_r2),
                                             base_r2[[y_name]], NA_real_), 3),
        Note                 = note,
        stringsAsFactors     = FALSE
      )

      # 4h. Plot if quadratic significant
      if (isTRUE(sig_quad)) {
        .nl_log(log_info, "  Generating predicted curve plot...")
        tryCatch({
          x_vals <- seq(min(df_stage2[[x_name]]), max(df_stage2[[x_name]]),
                        length.out = 100)

          # Predicted Y holding other predictors at mean
          newdata <- as.data.frame(matrix(0, nrow = 100, ncol = ncol(df_stage2)))
          names(newdata) <- names(df_stage2)
          # Set other predictors to their means
          for (col in names(df_stage2)) {
            if (is.numeric(df_stage2[[col]])) {
              newdata[[col]] <- mean(df_stage2[[col]], na.rm = TRUE)
            }
          }
          newdata[[x_name]] <- x_vals
          newdata[[x2_col]] <- x_vals^2

          y_pred <- predict(res_quad$ols_fit, newdata = newdata)

          plot_df <- data.frame(X = x_vals, Y_pred = y_pred)

          p <- ggplot(plot_df, aes(x = X, y = Y_pred)) +
            geom_line(linewidth = 1, colour = "#2c3e50") +
            geom_rug(data = data.frame(X = df_stage2[[x_name]]),
                     aes(x = X), inherit.aes = FALSE, alpha = 0.3) +
            labs(
              title = sprintf("Quadratic Effect: %s", label),
              subtitle = sprintf("beta_quad=%.3f, CI[%.3f,%.3f], p=%s",
                                  beta_quadratic, ci_q_lo, ci_q_hi, fmt_p(p_quad)),
              x = paste0(x_name, " (centred)"),
              y = paste0("Predicted ", y_name)
            ) +
            theme_minimal(base_size = 11) +
            theme(plot.title = element_text(face = "bold", size = 12))

          plot_path <- file.path(output_dir,
                                  sprintf("nonlinear_plot_%s_%s.png", x_name, y_name))
          ggsave(plot_path, p, width = 6, height = 4, dpi = 150)
          .nl_log(log_info, sprintf("  Plot saved: %s", plot_path))
        }, error = function(e) {
          .nl_warn(log_info, sprintf("  Plot failed: %s", e$message))
        })
      }

    }, error = function(e) {
      .nl_warn(log_info, sprintf("  FAILED: %s — %s", label, e$message))
      all_results[[ri]] <<- data.frame(
        Relation = label, X = x_name, Y = y_name,
        Beta_Linear_Base = NA, Beta_Linear_QuadModel = NA,
        CI_Linear_Lo = NA, CI_Linear_Hi = NA,
        Sig_Linear = NA, P_Linear = NA,
        Beta_Quadratic = NA, CI_Quad_Lo = NA, CI_Quad_Hi = NA,
        Sig_Quadratic = NA, P_Quadratic = NA, VIF_Quadratic = NA,
        Delta_Beta_Linear = NA, R2_Linear = NA, R2_Quadratic = NA,
        Delta_R2 = NA, f2_Quadratic = NA, f2_Classification = NA,
        R2_Base_PLS = NA,
        Note = paste("ERROR:", e$message),
        stringsAsFactors = FALSE
      )
    })
  }

  # --------------------------------------------------------------------------
  # 5. Export results
  # --------------------------------------------------------------------------
  results_df <- do.call(rbind, all_results)
  csv_path <- file.path(output_dir, "nonlinear_quadratic_results.csv")
  write.csv(results_df, csv_path, row.names = FALSE)
  .nl_log(log_info, "")
  .nl_log(log_info, sprintf("Results exported: %s (%d relation(s))",
                              csv_path, nrow(results_df)))

  # --------------------------------------------------------------------------
  # 6. Write interpretive conclusion
  # --------------------------------------------------------------------------
  n_sig_quad <- sum(isTRUE(results_df$Sig_Quadratic), na.rm = TRUE)
  # Handle vectorised isTRUE correctly
  sig_flags <- vapply(results_df$Sig_Quadratic, isTRUE, logical(1))
  n_sig_quad <- sum(sig_flags)

  conclusion_lines <- c(
    "# Nonlinear Robustness Check — Conclusion",
    sprintf("Date: %s", Sys.time()),
    sprintf("Seed: %d, Bootstrap: %d, CI: percentile %.0f%%", seed, B_boot, ci_level * 100),
    sprintf("Center method: %s", centre_meth),
    "",
    "## Summary",
    sprintf("Relations tested: %d", nrow(results_df)),
    sprintf("Significant quadratic effects: %d", n_sig_quad),
    ""
  )

  for (i in seq_len(nrow(results_df))) {
    r <- results_df[i, ]
    conclusion_lines <- c(conclusion_lines,
      sprintf("### %s", r$Relation),
      sprintf("- beta_linear (base):       %.3f", r$Beta_Linear_Base),
      sprintf("- beta_linear (quad model): %.3f, CI[%.3f, %.3f]%s",
               r$Beta_Linear_QuadModel, r$CI_Linear_Lo, r$CI_Linear_Hi,
               ifelse(isTRUE(r$Sig_Linear), "*", "")),
      sprintf("- beta_quadratic:           %.3f, CI[%.3f, %.3f]%s",
               r$Beta_Quadratic, r$CI_Quad_Lo, r$CI_Quad_Hi,
               ifelse(isTRUE(r$Sig_Quadratic), "*", "")),
      sprintf("- delta R²: %+.4f", r$Delta_R2),
      sprintf("- f²(quadratic): %.4f [%s]",
               ifelse(is.na(r$f2_Quadratic), 0, r$f2_Quadratic),
               ifelse(is.na(r$f2_Classification), "N/A", r$f2_Classification)),
      sprintf("- Conclusion: %s", r$Note),
      ""
    )
  }

  conclusion_lines <- c(conclusion_lines,
    "## Interpretation Guide",
    "This analysis is a ROBUSTNESS CHECK, not a hypothesis test.",
    "The base linear model remains the primary basis for inference.",
    "",
    "- If beta_quadratic CI excludes 0: evidence of nonlinearity;",
    "  check whether the linear coefficient changes sign or significance.",
    "- If beta_quadratic CI includes 0 and beta_linear is stable:",
    "  'linear specification is robust to quadratic augmentation'.",
    "",
    "Reference: Hair et al. (2022), Henseler & Chin (2010), Sarstedt et al. (2020)."
  )

  md_path <- file.path(output_dir, "nonlinear_quadratic_conclusion.md")
  writeLines(conclusion_lines, md_path)
  .nl_log(log_info, sprintf("Conclusion: %s", md_path))

  # --------------------------------------------------------------------------
  # 7. Final log summary
  # --------------------------------------------------------------------------
  elapsed <- round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1)

  .nl_log(log_info, "")
  .nl_log(log_info, "--- Nonlinear Robustness Summary ---")
  .nl_log(log_info, sprintf("  Relations tested: %d", nrow(results_df)))
  .nl_log(log_info, sprintf("  Significant quadratic: %d/%d", n_sig_quad, nrow(results_df)))
  for (i in seq_len(nrow(results_df))) {
    r <- results_df[i, ]
    sig_str <- ifelse(isTRUE(r$Sig_Quadratic), "NONLINEAR", "linear robust")
    .nl_log(log_info, sprintf("    %s: beta_quad=%.3f, f2=%.4f [%s], p=%s [%s]",
                                r$Relation,
                                ifelse(is.na(r$Beta_Quadratic), 0, r$Beta_Quadratic),
                                ifelse(is.na(r$f2_Quadratic), 0, r$f2_Quadratic),
                                ifelse(is.na(r$f2_Classification), "N/A", r$f2_Classification),
                                fmt_p(r$P_Quadratic),
                                sig_str))
  }

  if (n_sig_quad == 0) {
    .nl_log(log_info, "  Overall: Linear specification is ROBUST — no significant quadratic effects")
  } else {
    .nl_log(log_info, sprintf(
      "  Overall: %d relation(s) show nonlinear evidence — discuss in thesis", n_sig_quad))
  }
  .nl_log(log_info, sprintf("  Time: %.1f minutes", elapsed))

  # --------------------------------------------------------------------------
  # 8. Export f² effect size table (standalone CSV for thesis Table X)
  # --------------------------------------------------------------------------
  f2_df <- results_df[, c("Relation", "X", "Y",
                           "Beta_Quadratic", "CI_Quad_Lo", "CI_Quad_Hi",
                           "Sig_Quadratic", "P_Quadratic",
                           "R2_Linear", "R2_Quadratic", "Delta_R2",
                           "f2_Quadratic", "f2_Classification"), drop = FALSE]
  names(f2_df) <- c("Relation", "X", "Y",
                     "Beta_X2", "CI_Lo", "CI_Hi",
                     "Significant", "p_value",
                     "R2_without_X2", "R2_with_X2", "Delta_R2",
                     "f2", "Effect_Size")
  f2_csv_path <- file.path(output_dir, "f2_quadratic_effects.csv")
  write.csv(f2_df, f2_csv_path, row.names = FALSE)
  .nl_log(log_info, sprintf("f² quadratic effects exported: %s", f2_csv_path))

  invisible(list(
    results      = results_df,
    f2_table     = f2_df,
    config       = nl_cfg,
    elapsed      = elapsed,
    csv_path     = csv_path,
    f2_csv_path  = f2_csv_path,
    md_path      = md_path
  ))
}
