# ==============================================================================
# 13b_gaussian_copula.R — Gaussian Copula Endogeneity Test
# Park & Gupta (2012); Hair et al. (2022, §6.3); Hult et al. (2018)
# ==============================================================================
#
# PURPOSE:
#   Test whether suspected exogenous constructs suffer from endogeneity bias
#   at the STRUCTURAL model level (construct-score regression).
#
# ALGORITHM (Park & Gupta 2012):
#   1. Non-normality prerequisite (PRACTICAL criterion, not just p-value)
#   2. Construct Gaussian Copula term:  c_X = Phi^{-1}(ECDF(X))
#   3. Collinearity pre-screen: cor(X, COP_X) > 0.95 → skip
#   4. ONE-AT-A-TIME augmented regression:  Y ~ predictors + controls + COP_X
#      (only ONE copula term per regression — avoids multi-copula VIF explosion)
#   5. Bootstrap gamma (COP_X coefficient) with percentile CI
#   6. VIF gate in the single-copula augmented model
#   7. Decision: CI excludes 0 → evidence of endogeneity
#
# KEY DESIGN CHOICE — ONE-AT-A-TIME:
#   Adding COP terms for multiple regressors simultaneously inflates VIF
#   because each COP_X is correlated with its X (and with other COP terms).
#   Best practice: test one suspected regressor per augmented equation.
#   For k suspected regressors in one equation → k separate regressions.
#
# INTEGRATION:
#   Called from run_core.R (Phase 4) or standalone.
#   Uses utils_inference.R helpers (fmt_num, infer_sig_ci, build_table_note).
#
# DEPENDENCIES:  base R, stats.  Optional: moments (skewness/kurtosis).
# ==============================================================================

suppressPackageStartupMessages({
  library(stats)
})

# ==============================================================================
# HELPER: Safe skewness / kurtosis (fallback if {moments} not installed)
# ==============================================================================

.safe_skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  if (requireNamespace("moments", quietly = TRUE)) {
    return(moments::skewness(x))
  }
  m <- mean(x); s <- sd(x)
  (n / ((n - 1) * (n - 2))) * sum(((x - m) / s)^3)
}

.safe_kurtosis <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  if (requireNamespace("moments", quietly = TRUE)) {
    return(moments::kurtosis(x))
  }
  m <- mean(x); s <- sd(x)
  (n * (n + 1) / ((n - 1) * (n - 2) * (n - 3))) *
    sum(((x - m) / s)^4) - 3 * (n - 1)^2 / ((n - 2) * (n - 3))
}

# ==============================================================================
# HELPER: Jarque-Bera test (reported for completeness, NOT used for gating)
# ==============================================================================

.jarque_bera <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 5) return(list(statistic = NA_real_, p.value = NA_real_))
  if (requireNamespace("tseries", quietly = TRUE)) {
    jb <- tseries::jarque.bera.test(x)
    return(list(statistic = as.numeric(jb$statistic), p.value = jb$p.value))
  }
  sk <- .safe_skewness(x)
  ku <- .safe_kurtosis(x)
  jb_stat <- (n / 6) * (sk^2 + (ku^2) / 4)
  p_val   <- 1 - pchisq(jb_stat, df = 2)
  list(statistic = jb_stat, p.value = p_val)
}

# ==============================================================================
# HELPER: Manual VIF via OLS tolerance
# ==============================================================================

.compute_vif <- function(model_matrix) {
  p <- ncol(model_matrix)
  if (p < 2) return(setNames(1.0, colnames(model_matrix)))
  vifs <- numeric(p)
  for (j in seq_len(p)) {
    y_j <- model_matrix[, j]
    X_j <- model_matrix[, -j, drop = FALSE]
    fit <- lm(y_j ~ X_j)
    r2  <- summary(fit)$r.squared
    vifs[j] <- 1 / (1 - r2)
  }
  names(vifs) <- colnames(model_matrix)
  vifs
}

# ==============================================================================
# 1. NON-NORMALITY DIAGNOSTICS (PRACTICAL CRITERION)
# ==============================================================================
#
# Gate logic:
#   PRACTICAL non-normal = |skew| >= 1  OR  |excess kurtosis| >= 1
#   This is the ONLY gate for Copula_Feasible.
#   Shapiro-Wilk and JB are reported for completeness but do NOT gate,
#   because with N >= 200 they reject almost everything, which is misleading.
#
#   After copula construction, a secondary gate checks cor(X, COP_X):
#     > 0.95 → SKIP (near-linear transform, guaranteed VIF explosion).
# ==============================================================================

#' Test non-normality of each suspected regressor (construct-score level)
#'
#' @param scores     data.frame of construct scores
#' @param suspected  character vector of column names to test
#' @return data.frame with columns: Variable, N, Skewness, Kurtosis_Excess,
#'         SW_W, SW_p, JB_Stat, JB_p, Practical_NonNormal, Copula_Feasible
check_nonnormality <- function(scores, suspected) {
  rows <- vector("list", length(suspected))

  for (i in seq_along(suspected)) {
    x_name <- suspected[i]
    x <- scores[[x_name]]
    x <- x[!is.na(x)]
    n <- length(x)

    sk  <- .safe_skewness(x)
    ku  <- .safe_kurtosis(x)
    jb  <- .jarque_bera(x)

    # Shapiro-Wilk (reported only, not used for gating)
    sw_w <- NA_real_; sw_p <- NA_real_
    if (n >= 3 && n <= 5000) {
      sw <- shapiro.test(x)
      sw_w <- as.numeric(sw$statistic)
      sw_p <- sw$p.value
    }

    # ---- PRACTICAL criterion ONLY ----
    # |skew| >= 1  OR  |excess kurtosis| >= 1
    practical_nonnormal <- (!is.na(sk) && abs(sk) >= 1) |
                           (!is.na(ku) && abs(ku) >= 1)

    # Copula feasible = practical non-normal
    # (Shapiro/JB reported but intentionally NOT part of the gate)
    copula_feasible <- practical_nonnormal

    rows[[i]] <- data.frame(
      Variable           = x_name,
      N                  = n,
      Skewness           = round(sk, 3),
      Kurtosis_Excess    = round(ku, 3),
      SW_W               = round(sw_w, 4),
      SW_p               = round(sw_p, 4),
      JB_Stat            = round(jb$statistic, 3),
      JB_p               = round(jb$p.value, 4),
      Practical_NonNormal = practical_nonnormal,
      Copula_Feasible    = copula_feasible,
      stringsAsFactors   = FALSE
    )
  }
  do.call(rbind, rows)
}

# ==============================================================================
# 2. COPULA TERM CONSTRUCTION
# ==============================================================================

#' Construct Gaussian Copula term for a single variable
#'
#' Uses rank-based ECDF with offset to avoid u = 0 or u = 1:
#'   u_i = (rank(x_i, ties = "average") - 0.5) / n
#'   c_i = qnorm(u_i)
#'
#' @param x numeric vector (construct scores)
#' @return numeric vector of same length (copula term)
make_copula_term <- function(x) {
  n <- length(x)
  u <- (rank(x, ties.method = "average", na.last = "keep") - 0.5) / n
  qnorm(u)
}

# ==============================================================================
# 3. PARSE STRUCTURAL SPEC
# ==============================================================================

#' Parse a structural spec list into regression formulas
#'
#' @param structural_spec Named list. Each element:
#'   name = endogenous construct, value = character vector of predictors.
#'   A predictor starting with "controls:" is split by comma into control vars.
#'   e.g. list(AQ = c("AJ","ETH","TC","controls:Gen_Dummy,Exp_Ord"))
#' @return list of lists, each with $dv, $predictors, $controls, $all_rhs
parse_structural_spec <- function(structural_spec) {
  parsed <- list()
  for (dv in names(structural_spec)) {
    rhs_raw <- structural_spec[[dv]]
    controls_idx <- grep("^controls:", rhs_raw)
    controls <- character()
    if (length(controls_idx) > 0) {
      ctrl_str  <- sub("^controls:", "", rhs_raw[controls_idx])
      controls  <- trimws(unlist(strsplit(ctrl_str, ",")))
      rhs_raw   <- rhs_raw[-controls_idx]
    }
    predictors <- rhs_raw
    parsed[[dv]] <- list(
      dv         = dv,
      predictors = predictors,
      controls   = controls,
      all_rhs    = c(predictors, controls)
    )
  }
  parsed
}

# ==============================================================================
# 4. BOOTSTRAP REGRESSION
# ==============================================================================

#' Case-resample bootstrap of an OLS regression
#'
#' @param dat      data.frame with all columns needed
#' @param formula  formula object
#' @param B        number of bootstrap samples
#' @param seed     RNG seed
#' @return list(coef_matrix, obs_coef, obs_se, boot_se, t_stat, p_value,
#'              ci_low, ci_high)
boot_regression <- function(dat, formula, B = 5000, seed = 18) {
  set.seed(seed)
  n <- nrow(dat)

  obs_fit  <- lm(formula, data = dat)
  obs_coef <- coef(obs_fit)
  obs_se   <- summary(obs_fit)$coefficients[, "Std. Error"]
  p        <- length(obs_coef)

  coef_mat <- matrix(NA_real_, nrow = B, ncol = p,
                     dimnames = list(NULL, names(obs_coef)))

  for (b in seq_len(B)) {
    idx <- sample.int(n, n, replace = TRUE)
    fit_b <- tryCatch(
      lm(formula, data = dat[idx, , drop = FALSE]),
      error = function(e) NULL
    )
    if (!is.null(fit_b)) {
      cb <- coef(fit_b)
      coef_mat[b, names(cb)] <- cb
    }
  }

  ci_low  <- apply(coef_mat, 2, quantile, probs = 0.025, na.rm = TRUE)
  ci_high <- apply(coef_mat, 2, quantile, probs = 0.975, na.rm = TRUE)
  boot_se <- apply(coef_mat, 2, sd, na.rm = TRUE)
  t_stat  <- obs_coef / boot_se

  p_value <- sapply(seq_len(p), function(j) {
    bj <- coef_mat[, j]
    bj <- bj[!is.na(bj)]
    if (length(bj) == 0) return(NA_real_)
    2 * min(mean(bj >= 0), mean(bj <= 0))
  })
  names(p_value) <- names(obs_coef)

  list(
    coef_matrix = coef_mat,
    obs_coef    = obs_coef,
    obs_se      = obs_se,
    boot_se     = boot_se,
    t_stat      = t_stat,
    p_value     = p_value,
    ci_low      = ci_low,
    ci_high     = ci_high
  )
}

# ==============================================================================
# 5. MAIN FUNCTION
# ==============================================================================

#' Gaussian Copula Endogeneity Test — ONE-AT-A-TIME
#'
#' For each (equation, suspected X) pair, fits a SEPARATE augmented regression
#' with only ONE copula term COP_X.  This avoids the VIF explosion that occurs
#' when multiple copula terms are added simultaneously.
#'
#' @param scores          data.frame of construct scores (+ optionally controls).
#' @param structural_spec Named list: DV = c(predictors, "controls:c1,c2,...").
#' @param suspected       Character vector of predictors to test for endogeneity.
#'                        Should be ONLY variables you genuinely suspect, not all.
#' @param B               Bootstrap resamples (default 5000).
#' @param seed            RNG seed.
#' @param output_dir      Directory for CSV exports.
#' @param policy          Inference policy list (from utils_inference.R). NULL = defaults.
#' @param log_info        Log info list for pipeline logging. NULL = use cat().
#' @param force_normal    Force copula test even if X is ~ normal (default FALSE).
#' @param cor_threshold   Skip copula if cor(X, COP_X) > this value (default 0.95).
#' @return list with: nonnormality, copula_summary, vif_tables, results, decisions
gaussian_copula_test <- function(scores,
                                  structural_spec,
                                  suspected,
                                  B              = 5000,
                                  seed           = 18,
                                  output_dir     = "09_robust/endogeneity",
                                  policy         = NULL,
                                  log_info       = NULL,
                                  force_normal   = FALSE,
                                  cor_threshold  = 0.95) {

  # -------------------------------------------------------------------
  # Logging helpers (pipeline-aware or standalone)
  # -------------------------------------------------------------------
  .log <- function(msg) {
    if (!is.null(log_info) && exists("log_msg", mode = "function")) {
      log_msg(log_info, msg)
    } else {
      cat(msg, "\n")
    }
  }
  .warn <- function(msg) {
    if (!is.null(log_info) && exists("log_warn", mode = "function")) {
      log_warn(log_info, msg)
    } else {
      warning(msg, call. = FALSE)
    }
  }
  .step <- function(msg) {
    if (!is.null(log_info) && exists("log_step", mode = "function")) {
      log_step(log_info, msg)
    } else {
      cat("\n=== ", msg, " ===\n")
    }
  }

  .step("Gaussian Copula Endogeneity Test (Park & Gupta, 2012)")
  .log("  Strategy: ONE copula term per augmented equation (one-at-a-time)")

  # Formatting helpers (policy-aware or standalone)
  .fmt <- function(x, what = "estimate") {
    if (exists("fmt_num", mode = "function") && !is.null(policy)) {
      fmt_num(x, what, policy)
    } else {
      round(x, 3)
    }
  }
  .fmtp <- function(p) {
    if (exists("fmt_p", mode = "function")) {
      fmt_p(p)
    } else {
      ifelse(is.na(p), NA_character_,
             ifelse(p < 0.001, "<0.001", sprintf("%.4f", p)))
    }
  }
  .sig_ci <- function(lo, hi) {
    if (exists("infer_sig_ci", mode = "function")) {
      infer_sig_ci(lo, hi)
    } else {
      if (is.na(lo) || is.na(hi)) return(NA)
      (lo > 0 & hi > 0) | (lo < 0 & hi < 0)
    }
  }

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # -------------------------------------------------------------------
  # Validate inputs
  # -------------------------------------------------------------------
  if (!is.data.frame(scores)) scores <- as.data.frame(scores)
  stopifnot("scores must have > 0 rows" = nrow(scores) > 0)

  n_miss <- sum(!complete.cases(scores))
  if (n_miss > 0) {
    .warn(sprintf("  %d rows with NA in scores — using complete cases (%d -> %d)",
                  n_miss, nrow(scores), nrow(scores) - n_miss))
    scores <- scores[complete.cases(scores), , drop = FALSE]
  }
  n <- nrow(scores)
  .log(sprintf("  Sample size (complete cases): N = %d", n))
  .log(sprintf("  Suspected regressors: %s", paste(suspected, collapse = ", ")))

  parsed <- parse_structural_spec(structural_spec)

  all_predictors <- unique(unlist(lapply(parsed, function(eq) eq$predictors)))
  bad_suspected  <- setdiff(suspected, names(scores))
  if (length(bad_suspected) > 0)
    stop("Suspected variables not in scores: ", paste(bad_suspected, collapse = ", "))
  not_in_model <- setdiff(suspected, all_predictors)
  if (length(not_in_model) > 0)
    .warn(paste("  Suspected variables not in any equation as predictors:",
                paste(not_in_model, collapse = ", ")))

  # =====================================================================
  # STEP 1: Non-normality diagnostics (PRACTICAL criterion)
  # =====================================================================
  .log("--- Step 1: Non-normality prerequisite (practical criterion) ---")
  .log("  Gate: |skewness| >= 1  OR  |excess kurtosis| >= 1")
  .log("  Shapiro-Wilk & JB reported for completeness but do NOT gate.")

  nn_diag <- check_nonnormality(scores, suspected)

  write.csv(nn_diag, file.path(output_dir, "copula_nonnormality.csv"),
            row.names = FALSE)

  for (i in seq_len(nrow(nn_diag))) {
    r <- nn_diag[i, ]
    tag <- if (r$Copula_Feasible) "FEASIBLE" else "NEAR-NORMAL (skip)"
    .log(sprintf("  %-6s: skew=%+.3f, excess_kurt=%+.3f, SW p=%s  [%s]",
                 r$Variable, r$Skewness, r$Kurtosis_Excess,
                 .fmtp(r$SW_p), tag))
  }

  feasible     <- nn_diag$Variable[nn_diag$Copula_Feasible]
  not_feasible <- setdiff(suspected, feasible)

  if (length(not_feasible) > 0 && !force_normal) {
    .log(sprintf("  Near-normal (|skew|<1 & |kurt|<1) -> skipping: %s",
                 paste(not_feasible, collapse = ", ")))
    .log("  (Set force_normal=TRUE to override.)")
  }
  active_suspected <- if (force_normal) suspected else feasible

  if (length(active_suspected) == 0) {
    .log("  ** All suspected variables are near-normal **")
    .log("  Gaussian Copula: SKIPPED — copula terms ~= linear transforms of X")
    .log("  Fallback: strengthen control variables + theoretical argumentation")
    out <- list(
      nonnormality   = nn_diag,
      copula_summary = NULL,
      vif_tables     = NULL,
      results        = NULL,
      decisions      = data.frame(
        Equation = character(), Suspected = character(),
        Decision = character(), Reason = character(),
        stringsAsFactors = FALSE
      )
    )

    # Still write skipped decisions for all suspected x equation pairs
    skip_decisions <- data.frame(
      Equation = character(), Suspected = character(),
      Decision = character(), Reason = character(),
      stringsAsFactors = FALSE
    )
    for (sv in suspected) {
      nn_row <- nn_diag[nn_diag$Variable == sv, ]
      reason <- sprintf("|skew|=%.2f, |kurt|=%.2f — below threshold (near-normal)",
                        abs(nn_row$Skewness), abs(nn_row$Kurtosis_Excess))
      for (eq_name in names(parsed)) {
        if (sv %in% parsed[[eq_name]]$predictors) {
          skip_decisions <- rbind(skip_decisions, data.frame(
            Equation = eq_name, Suspected = sv,
            Decision = "SKIPPED", Reason = reason,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    out$decisions <- skip_decisions

    # Write conclusion even when all skipped
    .write_conclusion(output_dir, n, B, seed, cor_threshold, suspected,
                      skip_decisions, nn_diag, NULL, list(), .log)
    return(out)
  }

  # =====================================================================
  # STEP 2: Construct copula terms + collinearity pre-screen
  # =====================================================================
  .log("--- Step 2: Construct Gaussian Copula terms ---")
  .log(sprintf("  Collinearity pre-screen: cor(X, COP_X) > %.2f -> skip", cor_threshold))

  copula_info <- list()
  cor_rejected <- character()

  for (xname in active_suspected) {
    cop_name <- paste0("COP_", xname)
    scores[[cop_name]] <- make_copula_term(scores[[xname]])

    cor_x_cop <- cor(scores[[xname]], scores[[cop_name]], use = "complete.obs")
    cor_ok    <- abs(cor_x_cop) <= cor_threshold

    copula_info[[xname]] <- data.frame(
      Variable     = xname,
      Copula_Term  = cop_name,
      Cor_X_COP    = round(cor_x_cop, 4),
      Cor_OK       = cor_ok,
      stringsAsFactors = FALSE
    )

    if (cor_ok) {
      .log(sprintf("  %s: cor(X, COP_X) = %.4f  [OK — below %.2f]",
                   xname, cor_x_cop, cor_threshold))
    } else {
      .log(sprintf("  %s: cor(X, COP_X) = %.4f  [REJECT — above %.2f, near-linear]",
                   xname, cor_x_cop, cor_threshold))
      cor_rejected <- c(cor_rejected, xname)
    }
  }
  copula_summary <- do.call(rbind, copula_info)
  write.csv(copula_summary, file.path(output_dir, "copula_terms.csv"),
            row.names = FALSE)

  active_suspected <- setdiff(active_suspected, cor_rejected)

  if (length(active_suspected) == 0) {
    .log("  ** All copula terms have cor > threshold — all near-linear **")
    .log("  Gaussian Copula: SKIPPED — VIF would be too high")
    .log("  Fallback: control variables, theoretical argumentation, specification sensitivity")
  }

  .log(sprintf("  Variables passing both gates: %s",
               if (length(active_suspected) > 0)
                 paste(active_suspected, collapse = ", ") else "(none)"))

  # =====================================================================
  # STEP 3-5: ONE-AT-A-TIME augmented regressions per (equation, suspect)
  # =====================================================================

  all_results   <- list()
  all_vif       <- list()
  all_decisions <- list()

  if (length(active_suspected) > 0) {
    .log("--- Step 3-5: One-at-a-time augmented regressions (bootstrap) ---")

    for (eq_name in names(parsed)) {
      eq <- parsed[[eq_name]]
      dv <- eq$dv

      eq_suspects <- intersect(active_suspected, eq$predictors)
      if (length(eq_suspects) == 0) next

      .log(sprintf("  === Equation: %s ~ %s ===", dv,
                   paste(eq$all_rhs, collapse = " + ")))

      needed_cols <- c(dv, eq$all_rhs)
      missing_cols <- setdiff(needed_cols, names(scores))
      if (length(missing_cols) > 0) {
        .warn(sprintf("  Skipping %s: missing columns: %s",
                      dv, paste(missing_cols, collapse = ", ")))
        next
      }

      # Base regression (without any copula) — run once per equation
      base_formula <- as.formula(paste(dv, "~", paste(eq$all_rhs, collapse = " + ")))
      base_boot <- boot_regression(scores, base_formula, B = B, seed = seed)

      # ONE-AT-A-TIME: each suspected X gets its own augmented equation
      for (xname in eq_suspects) {
        cop_name <- paste0("COP_", xname)

        .log(sprintf("  --- Testing: %s (COP_%s) in %s equation ---",
                     xname, xname, dv))

        aug_rhs <- c(eq$all_rhs, cop_name)
        aug_formula <- as.formula(paste(dv, "~", paste(aug_rhs, collapse = " + ")))
        .log(sprintf("    Formula: %s ~ %s", dv, paste(aug_rhs, collapse = " + ")))

        aug_boot <- boot_regression(scores, aug_formula, B = B, seed = seed)

        # VIF in single-copula augmented model
        aug_model_mat <- model.matrix(aug_formula, data = scores)[, -1, drop = FALSE]
        vif_vals <- .compute_vif(aug_model_mat)
        vif_df <- data.frame(
          Equation  = eq_name,
          Suspected = xname,
          Variable  = names(vif_vals),
          VIF       = .fmt(vif_vals, "vif"),
          Flag_3_3  = vif_vals > 3.3,
          Flag_5_0  = vif_vals > 5.0,
          stringsAsFactors = FALSE
        )
        all_vif[[paste0(eq_name, "_", xname)]] <- vif_df

        cop_vif <- vif_vals[cop_name]
        vif_ok  <- !is.na(cop_vif) && cop_vif < 5.0

        .log(sprintf("    VIF(COP_%s) = %.3f  %s",
                     xname, cop_vif,
                     ifelse(vif_ok, "(OK < 5)",
                            ifelse(cop_vif > 5, "*** SEVERE > 5",
                                   "** caution > 3.3"))))

        # Extract gamma (COP_X) and corrected beta' (X)
        gamma_est   <- aug_boot$obs_coef[cop_name]
        gamma_se    <- aug_boot$boot_se[cop_name]
        gamma_t     <- aug_boot$t_stat[cop_name]
        gamma_p     <- aug_boot$p_value[cop_name]
        gamma_ci_lo <- aug_boot$ci_low[cop_name]
        gamma_ci_hi <- aug_boot$ci_high[cop_name]
        gamma_sig   <- .sig_ci(gamma_ci_lo, gamma_ci_hi)

        beta_corr       <- aug_boot$obs_coef[xname]
        beta_corr_se    <- aug_boot$boot_se[xname]
        beta_corr_t     <- aug_boot$t_stat[xname]
        beta_corr_p     <- aug_boot$p_value[xname]
        beta_corr_ci_lo <- aug_boot$ci_low[xname]
        beta_corr_ci_hi <- aug_boot$ci_high[xname]
        beta_corr_sig   <- .sig_ci(beta_corr_ci_lo, beta_corr_ci_hi)

        beta_orig       <- base_boot$obs_coef[xname]
        beta_orig_se    <- base_boot$boot_se[xname]
        beta_orig_ci_lo <- base_boot$ci_low[xname]
        beta_orig_ci_hi <- base_boot$ci_high[xname]

        # Decision
        if (!vif_ok) {
          decision <- "Inconclusive (VIF > 5 — copula collinearity)"
          decision_short <- "INCONCLUSIVE"
          reason <- sprintf("VIF(COP_%s)=%.1f > 5; copula too collinear with X",
                            xname, cop_vif)
        } else if (is.na(gamma_sig)) {
          decision <- "Inconclusive (missing CI)"
          decision_short <- "INCONCLUSIVE"
          reason <- "Bootstrap CI could not be computed"
        } else if (gamma_sig) {
          decision <- "Evidence of endogeneity (CI excludes 0)"
          decision_short <- "ENDOGENEITY"
          reason <- sprintf("gamma CI [%.4f, %.4f] excludes 0",
                            gamma_ci_lo, gamma_ci_hi)
        } else {
          decision <- "No evidence of endogeneity (CI includes 0)"
          decision_short <- "NO_ENDOGENEITY"
          reason <- sprintf("gamma CI [%.4f, %.4f] includes 0",
                            gamma_ci_lo, gamma_ci_hi)
        }

        # Mismatch check (CI vs t/p)
        alpha_val <- if (!is.null(policy)) policy$alpha else 0.05
        gamma_t_sig <- if (!is.na(gamma_t)) abs(gamma_t) >= qnorm(1 - alpha_val / 2) else NA
        gamma_p_sig <- if (!is.na(gamma_p)) gamma_p < alpha_val else NA
        mismatch <- FALSE
        if (!is.na(gamma_sig)) {
          if (!is.na(gamma_t_sig) && gamma_t_sig != gamma_sig) mismatch <- TRUE
          if (!is.na(gamma_p_sig) && gamma_p_sig != gamma_sig) mismatch <- TRUE
        }

        .log(sprintf("    Gamma: est=%+.4f, SE=%.4f, t=%.3f, p=%s",
                     gamma_est, gamma_se, gamma_t, .fmtp(gamma_p)))
        .log(sprintf("    Gamma CI [%.4f, %.4f] -> %s%s",
                     gamma_ci_lo, gamma_ci_hi,
                     ifelse(isTRUE(gamma_sig), "SIGNIFICANT", "not significant"),
                     ifelse(mismatch, " [MISMATCH]", "")))
        .log(sprintf("    Beta orig: %+.4f CI[%.4f,%.4f]",
                     beta_orig, beta_orig_ci_lo, beta_orig_ci_hi))
        .log(sprintf("    Beta corr: %+.4f CI[%.4f,%.4f]  shift=%+.4f",
                     beta_corr, beta_corr_ci_lo, beta_corr_ci_hi,
                     beta_corr - beta_orig))
        .log(sprintf("    >> DECISION: %s", decision))

        result_row <- data.frame(
          Equation         = eq_name,
          Suspected        = xname,
          Copula_Term      = cop_name,

          Gamma_Est        = .fmt(gamma_est, "estimate"),
          Gamma_Boot_SE    = .fmt(gamma_se, "estimate"),
          Gamma_T_Stat     = .fmt(gamma_t, "t_stat"),
          Gamma_P_Value    = .fmtp(gamma_p),
          Gamma_CI_Low     = .fmt(gamma_ci_lo, "ci"),
          Gamma_CI_High    = .fmt(gamma_ci_hi, "ci"),
          Gamma_Sig        = gamma_sig,
          Gamma_T_Sig      = gamma_t_sig,
          Gamma_P_Sig      = gamma_p_sig,
          Gamma_Mismatch   = mismatch,

          Beta_Corrected     = .fmt(beta_corr, "estimate"),
          Beta_Corr_SE       = .fmt(beta_corr_se, "estimate"),
          Beta_Corr_T        = .fmt(beta_corr_t, "t_stat"),
          Beta_Corr_P        = .fmtp(beta_corr_p),
          Beta_Corr_CI_Low   = .fmt(beta_corr_ci_lo, "ci"),
          Beta_Corr_CI_High  = .fmt(beta_corr_ci_hi, "ci"),
          Beta_Corr_Sig      = beta_corr_sig,

          Beta_Original      = .fmt(beta_orig, "estimate"),
          Beta_Orig_CI_Low   = .fmt(beta_orig_ci_lo, "ci"),
          Beta_Orig_CI_High  = .fmt(beta_orig_ci_hi, "ci"),

          Beta_Shift         = .fmt(beta_corr - beta_orig, "estimate"),

          VIF_COP            = .fmt(cop_vif, "vif"),
          VIF_OK             = vif_ok,
          Cor_X_COP          = copula_summary[copula_summary$Variable == xname, "Cor_X_COP"],

          Decision           = decision,
          Decision_Short     = decision_short,
          Reason             = reason,
          Mismatch_Note      = ifelse(mismatch,
                                      "CI vs t/p mismatch; CI used for decision", ""),

          stringsAsFactors = FALSE
        )
        all_results[[paste0(eq_name, "_", xname)]] <- result_row

        all_decisions[[paste0(eq_name, "_", xname)]] <- data.frame(
          Equation  = eq_name,
          Suspected = xname,
          Decision  = decision_short,
          Reason    = reason,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # =====================================================================
  # STEP 6: Combine and export
  # =====================================================================
  .log("--- Step 6: Export results ---")

  results_df <- if (length(all_results) > 0) do.call(rbind, all_results) else
    data.frame()
  rownames(results_df) <- NULL

  vif_df <- if (length(all_vif) > 0) do.call(rbind, all_vif) else data.frame()
  rownames(vif_df) <- NULL

  decisions_df <- if (length(all_decisions) > 0) do.call(rbind, all_decisions) else
    data.frame(Equation = character(), Suspected = character(),
               Decision = character(), Reason = character(),
               stringsAsFactors = FALSE)
  rownames(decisions_df) <- NULL

  # Append skipped variables (near-normal or cor-rejected) to decisions
  skipped_vars <- setdiff(suspected, active_suspected)
  if (length(skipped_vars) > 0) {
    for (sv in skipped_vars) {
      nn_row <- nn_diag[nn_diag$Variable == sv, ]
      cop_row <- if (!is.null(copula_summary) && sv %in% copula_summary$Variable)
        copula_summary[copula_summary$Variable == sv, ] else NULL

      if (nrow(nn_row) > 0 && !isTRUE(nn_row$Practical_NonNormal)) {
        reason <- sprintf("|skew|=%.2f, |kurt|=%.2f — below threshold (near-normal)",
                          abs(nn_row$Skewness), abs(nn_row$Kurtosis_Excess))
      } else if (!is.null(cop_row) && !isTRUE(cop_row$Cor_OK)) {
        reason <- sprintf("cor(X, COP_X)=%.4f > %.2f — near-linear transform",
                          cop_row$Cor_X_COP, cor_threshold)
      } else {
        reason <- "Skipped (prerequisite failed)"
      }

      for (eq_name in names(parsed)) {
        if (sv %in% parsed[[eq_name]]$predictors) {
          decisions_df <- rbind(decisions_df, data.frame(
            Equation  = eq_name,
            Suspected = sv,
            Decision  = "SKIPPED",
            Reason    = reason,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }

  if (nrow(results_df) > 0)
    write.csv(results_df, file.path(output_dir, "copula_results.csv"), row.names = FALSE)
  if (nrow(vif_df) > 0)
    write.csv(vif_df, file.path(output_dir, "copula_vif.csv"), row.names = FALSE)
  write.csv(decisions_df, file.path(output_dir, "copula_decisions.csv"), row.names = FALSE)

  # Write conclusion
  .write_conclusion(output_dir, n, B, seed, cor_threshold, suspected,
                    decisions_df, nn_diag, copula_summary, all_results, .log)

  out <- list(
    nonnormality   = nn_diag,
    copula_summary = copula_summary,
    vif_tables     = vif_df,
    results        = results_df,
    decisions      = decisions_df
  )
  invisible(out)
}

# ==============================================================================
# INTERNAL: Write conclusion markdown
# ==============================================================================

.write_conclusion <- function(output_dir, n, B, seed, cor_threshold,
                              suspected, decisions_df, nn_diag, copula_summary,
                              all_results, .log) {

  tested_decisions <- decisions_df[decisions_df$Decision != "SKIPPED", , drop = FALSE]
  any_endo         <- any(tested_decisions$Decision == "ENDOGENEITY")
  any_inconclusive <- any(tested_decisions$Decision == "INCONCLUSIVE")
  n_tested         <- nrow(tested_decisions)
  n_skipped        <- sum(decisions_df$Decision == "SKIPPED")

  md_lines <- c(
    "# Gaussian Copula Endogeneity Test — Results",
    "",
    sprintf("Date: %s", Sys.time()),
    sprintf("N = %d, B = %d, seed = %d", n, B, seed),
    "Primary rule: 95% bootstrap percentile CI excludes 0 (Park & Gupta, 2012)",
    "Strategy: one-at-a-time (one COP_X per augmented model)",
    "",
    "## Pre-screening gates",
    "",
    "- **Practical non-normality**: |skewness| >= 1 OR |excess kurtosis| >= 1",
    sprintf("- **Collinearity pre-screen**: cor(X, COP_X) <= %.2f", cor_threshold),
    "- Shapiro-Wilk & JB reported but intentionally NOT used for gating (inflated type-I at large N)",
    sprintf("- Variables tested: %d, Skipped: %d (of %d suspected)",
            n_tested, n_skipped, length(suspected)),
    ""
  )

  if (n_skipped > 0) {
    md_lines <- c(md_lines, "### Skipped variables", "")
    skipped_dec <- decisions_df[decisions_df$Decision == "SKIPPED", , drop = FALSE]
    for (i in seq_len(nrow(skipped_dec))) {
      md_lines <- c(md_lines, sprintf("- **%s** (%s eq): %s",
                                       skipped_dec$Suspected[i],
                                       skipped_dec$Equation[i],
                                       skipped_dec$Reason[i]))
    }
    md_lines <- c(md_lines, "")
  }

  md_lines <- c(md_lines, "## Decisions per tested variable", "")

  if (n_tested > 0) {
    for (i in seq_len(nrow(tested_decisions))) {
      md_lines <- c(md_lines,
        sprintf("- **%s -> %s**: %s (%s)",
                tested_decisions$Suspected[i], tested_decisions$Equation[i],
                tested_decisions$Decision[i], tested_decisions$Reason[i]))
    }
  } else {
    md_lines <- c(md_lines,
      "- No variables passed pre-screening gates — no copula regressions run.")
  }

  md_lines <- c(md_lines, "", "## Interpretation", "")

  if (n_tested == 0) {
    md_lines <- c(md_lines,
      "**All suspected variables were too close to normal** for the Gaussian Copula approach.",
      "The copula term COP_X becomes a near-linear transform of X when the distribution is",
      "approximately normal, leading to severe collinearity (cor > 0.95, VIF > 10+).",
      "",
      "**Recommended fallback actions:**",
      "1. Strengthen theoretical argumentation for causal direction",
      "2. Ensure comprehensive control variables are included in the structural model",
      "3. Report the specification sensitivity analysis (pre vs post QC comparison)",
      "4. Acknowledge endogeneity as a potential limitation and discuss",
      "   likely omitted variable directions",
      "5. If instruments are available, consider 2SLS/IV estimation"
    )
  } else if (any_endo) {
    md_lines <- c(md_lines,
      "**Endogeneity detected** for at least one path. Recommended actions:",
      "1. Report corrected beta' alongside original beta",
      "2. Discuss potential sources (omitted variables, simultaneity, measurement error)",
      "3. Strengthen IV / control variables",
      "4. Consider instrumental variable approach if suitable instruments available"
    )
  } else if (any_inconclusive) {
    md_lines <- c(md_lines,
      "**Inconclusive** for at least one variable (high VIF even in one-at-a-time model).",
      "The copula term COP_X uses rank-based transform: u = (rank(x) - 0.5)/n; COP = \u03a6\u207b\u00b9(u).",
      "When X is moderately non-normal (|skew| ~1\u20132), cor(X, COP_X) can reach 0.90+,",
      "inflating VIF beyond 5 and making the gamma test inconclusive.",
      "",
      "**Supplementary evidence for endogeneity assessment:**",
      "1. Model B robustness (Section E): if substantive paths are stable after adding",
      "   control variables, this reduces omitted-variable-bias concern.",
      "2. Theoretical argumentation for causal direction (cite prior literature).",
      "3. Data sensitivity (Section A): pre- vs post-screening path stability.",
      "4. Consider IV/2SLS if suitable instruments are identifiable.",
      "5. Report as a limitation and qualify causal language accordingly."
    )
  } else {
    md_lines <- c(md_lines,
      "**No evidence of endogeneity** detected for any tested path.",
      "The structural model coefficients can be interpreted as unbiased (subject to other assumptions).",
      "Note: absence of evidence != evidence of absence. The test has limited power",
      "when the distribution is close to normal or sample size is small."
    )
  }

  md_lines <- c(md_lines, "",
    "## References",
    "- Park, S., & Gupta, S. (2012). Handling endogenous regressors by joint estimation using copulas. *Marketing Science*, 31(4), 567-586.",
    "- Hair, J. F., Hult, G. T. M., Ringle, C. M., & Sarstedt, M. (2022). *A Primer on Partial Least Squares Structural Equation Modeling (PLS-SEM)* (3rd ed.). Sage.",
    "- Hult, G. T. M., Hair, J. F., Proksch, D., Sarstedt, M., Pinkwart, A., & Ringle, C. M. (2018). Addressing endogeneity in international marketing applications of partial least squares structural equation modeling. *JIBS*, 49(6), 713-729."
  )

  writeLines(md_lines, file.path(output_dir, "endogeneity_conclusion.md"))

  # Console summary
  .log("")
  .log("  ======== GAUSSIAN COPULA ENDOGENEITY SUMMARY ========")
  .log(sprintf("  Suspected: %d | Tested: %d | Skipped: %d",
               length(suspected), n_tested, n_skipped))
  if (n_tested > 0) {
    for (i in seq_len(nrow(tested_decisions))) {
      r <- tested_decisions[i, ]
      key <- paste0(r$Equation, "_", r$Suspected)
      if (key %in% names(all_results)) {
        rr <- all_results[[key]]
        .log(sprintf("  %s -> %s | gamma=%+.4f CI[%.4f,%.4f] VIF=%.2f | %s",
                     r$Suspected, r$Equation,
                     as.numeric(rr$Gamma_Est),
                     as.numeric(rr$Gamma_CI_Low), as.numeric(rr$Gamma_CI_High),
                     as.numeric(rr$VIF_COP), r$Decision))
      } else {
        .log(sprintf("  %s -> %s | %s", r$Suspected, r$Equation, r$Decision))
      }
    }
  }
  if (n_skipped > 0) {
    .log(sprintf("  Skipped (near-normal or cor>%.2f): %s",
                 cor_threshold,
                 paste(unique(decisions_df$Suspected[decisions_df$Decision == "SKIPPED"]),
                       collapse = ", ")))
  }
  .log("  =====================================================")
}

# ==============================================================================
# 6. CONVENIENCE: Extract scores + controls from PLS model + data
# ==============================================================================

#' Build the scores data.frame from a SEMinR model + control variables
#'
#' @param pls_model  seminr estimated model (from estimate_pls)
#' @param data_final data.frame of the final analysis-ready dataset
#' @param controls   character vector of control variable column names
#' @return data.frame with construct scores + control columns, complete cases
get_copula_scores <- function(pls_model, data_final, controls = character()) {
  scores <- as.data.frame(pls_model$construct_scores)

  if (length(controls) > 0) {
    missing_ctrl <- setdiff(controls, names(data_final))
    if (length(missing_ctrl) > 0)
      warning("Control variables not found in data_final: ",
              paste(missing_ctrl, collapse = ", "))
    avail_ctrl <- intersect(controls, names(data_final))
    if (length(avail_ctrl) > 0) {
      ctrl_df <- data_final[, avail_ctrl, drop = FALSE]
      if (nrow(ctrl_df) == nrow(scores)) {
        scores <- cbind(scores, ctrl_df)
      } else {
        warning("Row mismatch between PLS scores and data_final. ",
                "Aligning by position (first ", nrow(scores), " rows).")
        scores <- cbind(scores, ctrl_df[seq_len(nrow(scores)), , drop = FALSE])
      }
    }
  }
  scores[complete.cases(scores), , drop = FALSE]
}

# ==============================================================================
# HOW TO USE — Example
# ==============================================================================
#
# # 1. Source the module
# source("R/13b_gaussian_copula.R")
# source("R/utils_inference.R")   # Optional, for policy-aware formatting
#
# # 2. Get construct scores + controls
# scores <- get_copula_scores(
#   pls_model  = pls_model,
#   data_final = data_final,
#   controls   = c("Gen_Dummy", "Exp_Ord", "Pos_Ord", "Edu_PostGrad", "CPA_Dummy")
# )
#
# # 3. Define structural equations at the score level
# structural_spec <- list(
#   AQ = c("AJ", "ETH", "TC",
#          "controls:Gen_Dummy,Exp_Ord,Pos_Ord,Edu_PostGrad,CPA_Dummy"),
#   AJ = c("COM", "PS", "MO", "TP", "IT", "CC", "CG",
#          "controls:Gen_Dummy,Exp_Ord,Pos_Ord,Edu_PostGrad,CPA_Dummy")
# )
#
# # 4. Specify ONLY the genuinely suspected regressors (not all!)
# suspected <- c("COM", "PS", "AJ")
#
# # 5. Run the test (one-at-a-time, practical non-normality gate)
# policy <- load_inference_policy()   # Optional
# gc_results <- gaussian_copula_test(
#   scores          = scores,
#   structural_spec = structural_spec,
#   suspected       = suspected,
#   B               = 5000,
#   seed            = 18,
#   output_dir      = "09_robust/endogeneity",
#   policy          = policy,
#   cor_threshold   = 0.95    # skip if copula is near-linear
# )
#
# # 6. Inspect results
# gc_results$nonnormality       # Non-normality diagnostics (practical gate)
# gc_results$copula_summary     # Copula term info + cor pre-screen
# gc_results$vif_tables         # VIF per (equation, suspected) pair
# gc_results$results            # Full results table (gamma, beta', CIs)
# gc_results$decisions          # Decision per tested pair (incl. SKIPPED)
