# ==============================================================================
# utils_inference.R — Centralized Inference Helpers
# ==============================================================================
# Provides a single set of functions that ALL modules use to:
#   1. Determine significance (CI-based primary rule)
#   2. Compute secondary stats (t_sig, p_sig)
#   3. Flag mismatches between primary and secondary
#   4. Format numbers with consistent decimal places
#   5. Build standardised table-note strings
#
# Loaded once in run_core.R; every analysis module calls these helpers.
# ==============================================================================

suppressPackageStartupMessages(library(yaml))

# ==============================================================================
# 1. LOAD POLICY
# ==============================================================================

#' Load inference policy from YAML
#' @param path path to inference_policy.yml
#' @return list with all policy settings
load_inference_policy <- function(path = "config/inference_policy.yml") {
  if (!file.exists(path)) {
    stop("Inference policy file not found: ", path,
         "\nCreate it or copy the template from config/inference_policy.yml")
  }
  policy <- yaml::read_yaml(path)

  # Validate required keys
  required <- c("primary_rule", "alpha", "ci_method", "ci_level",
                 "two_tailed", "decimals")
  missing <- setdiff(required, names(policy))
  if (length(missing) > 0) {
    stop("Inference policy missing keys: ", paste(missing, collapse = ", "))
  }

  policy
}

# ==============================================================================
# 2. SIGNIFICANCE HELPERS
# ==============================================================================

#' Primary significance test: does the CI exclude zero?
#' @param ci_low  lower bound of the confidence interval
#' @param ci_high upper bound of the confidence interval
#' @return logical TRUE if significant (CI excludes 0)
infer_sig_ci <- function(ci_low, ci_high) {
  if (is.na(ci_low) || is.na(ci_high)) return(NA)
  (ci_low > 0 & ci_high > 0) | (ci_low < 0 & ci_high < 0)
}

#' Vectorised version for data.frame columns
#' @param ci_low  numeric vector
#' @param ci_high numeric vector
#' @return logical vector
infer_sig_ci_vec <- function(ci_low, ci_high) {
  mapply(infer_sig_ci, ci_low, ci_high, USE.NAMES = FALSE)
}

#' Full inference pack — compute all stats & flags for one effect
#'
#' @param est     point estimate (beta / weight / indirect)
#' @param boot_se bootstrap standard error
#' @param t_stat  t-statistic
#' @param p_value bootstrap p-value (two-tailed)
#' @param ci_low  CI lower bound
#' @param ci_high CI upper bound
#' @param policy  list from load_inference_policy()
#' @return named list with: sig_primary, t_sig, p_sig, mismatch, note
infer_pack <- function(est, boot_se = NA, t_stat = NA, p_value = NA,
                       ci_low = NA, ci_high = NA, policy = NULL) {

  alpha <- if (!is.null(policy)) policy$alpha else 0.05

  sig_primary <- infer_sig_ci(ci_low, ci_high)
  t_sig       <- if (!is.na(t_stat))  abs(t_stat) >= qnorm(1 - alpha / 2) else NA
  p_sig       <- if (!is.na(p_value)) p_value < alpha else NA

  # Mismatch: primary vs any secondary
  mismatch <- FALSE
  if (!is.na(sig_primary)) {
    if (!is.na(t_sig) && t_sig != sig_primary)  mismatch <- TRUE
    if (!is.na(p_sig) && p_sig != sig_primary)   mismatch <- TRUE
  }

  note <- ""
  if (mismatch) {
    note <- "CI vs t/p mismatch; CI used for decision"
  }

  list(
    sig_primary = sig_primary,
    t_sig       = t_sig,
    p_sig       = p_sig,
    mismatch    = mismatch,
    note        = note
  )
}

#' Vectorised infer_pack — returns a data.frame of flag columns
#'
#' @param df   data.frame that already has columns ci_low, ci_high,
#'             and optionally t_stat, p_value
#' @param cols named list mapping logical column names to df column names.
#'             Defaults: list(ci_low="CI_Low", ci_high="CI_High",
#'                            t_stat="T_Stat", p_value="P_Value")
#' @param policy inference policy list
#' @return data.frame with columns: Sig_Primary, T_Sig, P_Sig, Mismatch, Notes
infer_pack_df <- function(df, cols = NULL, policy = NULL) {

  # Default column mapping
  cm <- list(ci_low = "CI_Low", ci_high = "CI_High",
             t_stat = "T_Stat", p_value = "P_Value")
  if (!is.null(cols)) {
    for (k in names(cols)) cm[[k]] <- cols[[k]]
  }

  alpha <- if (!is.null(policy)) policy$alpha else 0.05
  n <- nrow(df)

  # Extract vectors safely
  get_col <- function(name) {
    if (name %in% names(df)) df[[name]] else rep(NA_real_, n)
  }

  ci_lo <- get_col(cm$ci_low)
  ci_hi <- get_col(cm$ci_high)
  t_val <- get_col(cm$t_stat)
  p_val <- get_col(cm$p_value)

  sig_primary <- infer_sig_ci_vec(ci_lo, ci_hi)
  t_sig <- ifelse(is.na(t_val), NA, abs(t_val) >= qnorm(1 - alpha / 2))
  p_sig <- ifelse(is.na(p_val), NA, p_val < alpha)

  mismatch <- rep(FALSE, n)
  mismatch <- ifelse(!is.na(sig_primary) & !is.na(t_sig) & t_sig != sig_primary,
                     TRUE, mismatch)
  mismatch <- ifelse(!is.na(sig_primary) & !is.na(p_sig)  & p_sig != sig_primary,
                     TRUE, mismatch)

  notes <- ifelse(mismatch, "CI vs t/p mismatch; CI used for decision", "")

  data.frame(
    Sig_Primary = sig_primary,
    T_Sig       = t_sig,
    P_Sig       = p_sig,
    Mismatch    = mismatch,
    Notes       = notes,
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# 2b. EMPIRICAL BOOTSTRAP P-VALUE
# ==============================================================================

#' Compute two-tailed empirical p-value from bootstrap draws
#'
#' Uses continuity correction to avoid exact-zero p-values:
#'   p = 2 * min( (k+1)/(B+1), (B-k+1)/(B+1) )
#' where k = number of draws <= 0 and B = number of valid draws.
#' NA/Inf/NaN draws are filtered before counting.
#'
#' @param draws numeric vector of bootstrap draws (length B)
#' @return numeric p-value in (0, 1]
#' @references Davison & Hinkley (1997); MacKinnon (2008)
compute_empirical_p <- function(draws) {
  # Filter NA/Inf/NaN
  b_draws <- draws[is.finite(draws)]
  B <- length(b_draws)
  if (B < 2) return(NA_real_)

  k <- sum(b_draws <= 0)
  p <- 2 * min((k + 1) / (B + 1), (B - k + 1) / (B + 1))
  # Clamp to [0, 1]
  min(p, 1)
}

# ==============================================================================
# 3. FORMATTING HELPERS
# ==============================================================================

#' Round a numeric value using policy decimals
#' @param x numeric value(s)
#' @param what one of: "estimate", "t_stat", "p_value", "ci", "vif", "r_squared", "vaf"
#' @param policy inference policy list
#' @return rounded numeric
fmt_num <- function(x, what = "estimate", policy = NULL) {
  dec_map <- if (!is.null(policy)) policy$decimals else
    list(estimate = 3, t_stat = 3, p_value = 4, ci = 3, vif = 3, r_squared = 3, vaf = 3)
  d <- dec_map[[what]]
  if (is.null(d)) d <- 3
  round(x, d)
}

#' Format p-value for display: 4 decimals or "<0.001"
#' @param p numeric p-value(s)
#' @return character
fmt_p <- function(p) {
  ifelse(is.na(p), NA_character_,
         ifelse(p < 0.001, "<0.001", sprintf("%.4f", p)))
}

#' Build significance star string
#' @param sig logical (uses sig_primary)
#' @return character: "*" if TRUE, "" otherwise
fmt_star <- function(sig) {
  ifelse(is.na(sig), "", ifelse(sig, "*", ""))
}

# ==============================================================================
# 4. TABLE NOTE BUILDER
# ==============================================================================

#' Build a standardised table note string
#' @param policy   inference policy list
#' @param n        sample size
#' @param B        bootstrap resamples
#' @param seed     random seed
#' @param extra    extra note lines (character vector, appended)
#' @return character string suitable for table footer
build_table_note <- function(policy, n, B, seed, extra = NULL) {
  ci_pct <- as.integer(policy$ci_level * 100)
  parts <- c(
    sprintf("N = %d; Bootstrap B = %d (seed = %d).", n, B, seed),
    sprintf("CI method: %s %d%% two-tailed.", policy$ci_method, ci_pct),
    sprintf("Primary inference rule: %d%% bootstrap CI excludes 0.", ci_pct),
    sprintf("* significant at alpha = %.2f.", policy$alpha),
    "Secondary stats (t, p) reported for reference; CI takes precedence if mismatch."
  )
  if (!is.null(extra)) parts <- c(parts, extra)
  paste(parts, collapse = " ")
}

# ==============================================================================
# 5. SNAPSHOT — write policy to output dir for reproducibility
# ==============================================================================

#' Copy inference policy into report output (audit trail)
#' @param policy_path original policy file path
#' @param output_dir  report output directory
snapshot_policy <- function(policy_path, output_dir) {
  dest <- file.path(output_dir, "config_snapshot_inference_policy.yml")
  file.copy(policy_path, dest, overwrite = TRUE)
  invisible(dest)
}
