# ==============================================================================
# 03_micom.R — Step 2: MICOM via cSEM::testMICOM()
# ==============================================================================
# Reference: Henseler, Ringle & Sarstedt (2016)
# Uses cSEM package for rigorous permutation-based MICOM:
#   Step 1 — Configural invariance  (qualitative; always assumed if same spec)
#   Step 2 — Compositional invariance (permutation test on c correlations)
#   Step 3 — Equal means & variances (permutation test)
#
# Requires: cSEM (>= 0.5.0)
# ==============================================================================

# ---- Main entry point --------------------------------------------------------

#' Run MICOM for the grouping variable produced by clustering
#' @param env backend environment (list from lh_detect_backend)
#' @param cfg module config
#' @param groups integer vector of group assignments (length = N)
#' @param k_opt optimal k
#' @param log_info log object
#' @return list(passed_step2, results_df, all_results, n_pairs, method)
lh_step2_micom <- function(env, cfg, groups, k_opt, log_info) {
  lh_step(log_info, "Step 2 \u2014 MICOM (Measurement Invariance of Composite Models)")

  alpha  <- cfg$micom$alpha
  n_perm <- cfg$micom$permutations
  seed   <- cfg$seed

  lh_log(log_info, sprintf("  Permutations: %d | Alpha: %.2f | Seed: %d",
                            n_perm, alpha, seed))
  lh_log(log_info, "  Method: cSEM::testMICOM() (Henseler et al., 2016)")

  # ---- Guard: cSEM package ---------------------------------------------------
  if (!requireNamespace("cSEM", quietly = TRUE)) {
    lh_log(log_info,
           "  cSEM package not installed. MICOM cannot be assessed.",
           level = "ERROR")
    lh_log(log_info,
           "  Install with: install.packages('cSEM')",
           level = "ERROR")
    construct_names <- sapply(env$base_cfg$constructs, function(cc) cc$name)
    return(list(
      passed_step2 = FALSE,
      results_df   = data.frame(
        Pair = character(), Construct = character(),
        c_original = numeric(), ci_lower_5pct = numeric(),
        p_value = numeric(), decision = character(),
        stringsAsFactors = FALSE
      ),
      all_results  = list(),
      n_pairs      = 0,
      method       = "NOT_ASSESSED"
    ))
  }

  lh_log(log_info, sprintf("  cSEM version: %s",
                            as.character(packageVersion("cSEM"))))

  # ---- Guard: higher-order constructs ----------------------------------------
  if (!is.null(env$base_cfg$higher_order) &&
      length(env$base_cfg$higher_order) > 0) {
    lh_log(log_info,
           paste0("  WARNING: Higher-order constructs detected. ",
                  "cSEM MICOM applied to first-order constructs only."),
           level = "WARN")
  }

  # ---- Build cSEM model syntax -----------------------------------------------
  csem_syntax <- .lh_build_csem_syntax(env$base_cfg, env$active_ind, log_info)

  # ---- Prepare indicator data ------------------------------------------------
  all_ind <- unique(unlist(lapply(env$base_cfg$constructs, function(cc) {
    if (!is.null(env$active_ind) && cc$name %in% names(env$active_ind))
      env$active_ind[[cc$name]] else cc$indicators
  })))
  data_ind <- env$data_raw[, intersect(all_ind, colnames(env$data_raw)),
                            drop = FALSE]

  if (k_opt > 2) {
    lh_log(log_info, sprintf(
      "  k=%d: MICOM will be run for each pairwise group comparison.", k_opt))
  }

  # ---- Pairwise MICOM --------------------------------------------------------
  group_labels <- sort(unique(groups))
  pairs <- combn(group_labels, 2, simplify = FALSE)
  n_pairs_total <- length(pairs)
  lh_log(log_info, sprintf("  Group pairs to test: %d", n_pairs_total))

  all_results <- list()
  all_passed  <- TRUE

  for (pr in pairs) {
    g1 <- pr[1]; g2 <- pr[2]
    pair_label <- sprintf("Group_%d_vs_%d", g1, g2)
    lh_log(log_info, sprintf("\n  --- %s ---", pair_label))

    idx1 <- which(groups == g1)
    idx2 <- which(groups == g2)
    lh_log(log_info, sprintf("    n1=%d, n2=%d", length(idx1), length(idx2)))

    if (length(idx1) < 30 || length(idx2) < 30) {
      lh_log(log_info,
             "    WARNING: Group size < 30. MICOM results may be unreliable.",
             level = "WARN")
    }

    # Prepare multi-group data with group column
    data_pair <- rbind(data_ind[idx1, , drop = FALSE],
                       data_ind[idx2, , drop = FALSE])
    data_pair$lh_group <- c(rep(paste0("G", g1), length(idx1)),
                            rep(paste0("G", g2), length(idx2)))

    # Step 1: Configural invariance (qualitative assumption)
    lh_log(log_info,
           "    Step 1 (Configural): ASSUMED \u2014 same model spec for both groups.")

    # Run cSEM estimation + MICOM
    micom_res <- tryCatch({
      .lh_run_csem_micom(csem_syntax, data_pair, n_perm, alpha, seed, log_info)
    }, error = function(e) {
      lh_log(log_info,
             sprintf("    cSEM MICOM error: %s", e$message),
             level = "ERROR")
      NULL
    })

    if (is.null(micom_res)) {
      # cSEM failed for this pair — return NOT ASSESSED
      construct_names <- sapply(env$base_cfg$constructs, function(cc) cc$name)
      micom_res <- list(
        step2_passed = FALSE,
        step2_df = data.frame(
          Construct     = construct_names,
          c_original    = NA_real_,
          ci_lower_5pct = NA_real_,
          p_value       = NA_real_,
          decision      = "NOT ASSESSED",
          stringsAsFactors = FALSE
        ),
        step3_df = data.frame(
          Construct  = construct_names,
          Mean_Diff  = NA_real_,
          Var_Diff   = NA_real_,
          Mean_p     = NA_real_,
          Var_p      = NA_real_,
          Mean_Equal = NA_character_,
          Var_Equal  = NA_character_,
          stringsAsFactors = FALSE
        )
      )
    }

    micom_res$pair <- pair_label
    all_results[[pair_label]] <- micom_res
    if (!micom_res$step2_passed) all_passed <- FALSE
  }

  # ---- Consolidate -----------------------------------------------------------
  results_df <- .lh_consolidate_micom(all_results)

  lh_gate(log_info, "MICOM Step 2 (Compositional Invariance)",
          all_passed,
          ifelse(all_passed,
                 "\u2014 all pairs OK",
                 "\u2014 at least one pair FAILED"))

  list(
    passed_step2 = all_passed,
    results_df   = results_df,
    all_results  = all_results,
    n_pairs      = n_pairs_total,
    method       = "cSEM"
  )
}

# ---- Build cSEM model syntax ------------------------------------------------

#' Construct a cSEM-compatible model string from base analysis config
#' @param base_cfg list from analysis_config.yaml
#' @param active_ind named list of construct -> indicator vectors (optional)
#' @param log_info log object
#' @return character string in lavaan-style syntax
.lh_build_csem_syntax <- function(base_cfg, active_ind, log_info) {

  # ---- Measurement model ----
  mm_lines <- vapply(base_cfg$constructs, function(cc) {
    inds <- if (!is.null(active_ind) && cc$name %in% names(active_ind))
      active_ind[[cc$name]] else cc$indicators
    op <- if (cc$measurement_type == "reflective") "=~" else "<~"
    sprintf("%s %s %s", cc$name, op, paste(inds, collapse = " + "))
  }, character(1))

  # ---- Structural model (direct effects only; interactions excluded) ----
  # Group IVs by DV for compact equations
  dv_map <- list()
  for (p in base_cfg$structural_paths) {
    dv_map[[p$to]] <- c(dv_map[[p$to]], p$from)
  }
  sm_lines <- vapply(names(dv_map), function(dv) {
    sprintf("%s ~ %s", dv, paste(dv_map[[dv]], collapse = " + "))
  }, character(1))

  syntax <- paste(c(mm_lines, sm_lines), collapse = "\n")

  lh_log(log_info, sprintf(
    "  cSEM model syntax: %d measurement + %d structural equations",
    length(mm_lines), length(sm_lines)))

  syntax
}

# ---- Run cSEM multi-group estimation + testMICOM -----------------------------

#' Estimate cSEM multi-group model and run testMICOM
#' @param syntax character cSEM model string
#' @param data_pair data.frame with indicator columns + .lh_group column
#' @param n_perm number of permutations
#' @param alpha significance level
#' @param seed random seed
#' @param log_info log object
#' @return list(step2_passed, step2_df, step3_df)
.lh_run_csem_micom <- function(syntax, data_pair, n_perm, alpha, seed,
                                log_info) {

  lh_log(log_info, "    Running cSEM multi-group estimation...")

  csem_out <- .lh_quietly(
    cSEM::csem(.model = syntax, .data = data_pair, .id = "lh_group")
  )

  if (is.null(csem_out))
    stop("cSEM::csem() returned NULL \u2014 estimation failed.")

  # Check admissibility (non-fatal)
  tryCatch({
    status <- cSEM::verify(csem_out)
    # verify() returns TRUE if no issues, else a list of issues
    has_issues <- if (is.logical(status)) !all(status) else TRUE
    if (has_issues) {
      lh_log(log_info,
             "    WARNING: cSEM reports admissibility issues in one or both groups.",
             level = "WARN")
    }
  }, error = function(e) {
    lh_log(log_info,
           sprintf("    NOTE: cSEM::verify() check skipped (%s).", e$message),
           level = "INFO")
  })

  # Run MICOM test
  lh_log(log_info,
         sprintf("    Running cSEM::testMICOM() with %d permutations...",
                 n_perm))

  micom_out <- .lh_quietly(
    cSEM::testMICOM(.object = csem_out,
                     .R = n_perm,
                     .seed = seed,
                     .handle_inadmissibles = "replace",
                     .verbose = FALSE)
  )

  if (is.null(micom_out))
    stop("cSEM::testMICOM() returned NULL.")

  lh_log(log_info, "    cSEM MICOM completed.")

  # ---- Parse the testMICOM output --------------------------------------------
  # cSEM 0.6.x returns class "cSEMTestMICOM" with:
  #   $Step2$Test_statistic$<pair_key>   — named numeric vector (c-values)
  #   $Step2$P_value$<adjust>$<pair_key> — named numeric vector
  #   $Step2$Bootstrap_values[[i]]$<pair_key> — named numeric vector per run
  #   $Step3$Mean$Test_statistic$<pair_key>   — named numeric vector
  #   $Step3$Mean$P_value$<adjust>$<pair_key> — named numeric vector
  #   $Step3$Var$Test_statistic$<pair_key>    — named numeric vector
  #   $Step3$Var$P_value$<adjust>$<pair_key>  — named numeric vector

  if (is.null(micom_out$Step2))
    stop("testMICOM output missing Step2.")

  # ---- Helper: extract first vector from a nested 1-deep list ----
  .first_vec <- function(x) {
    if (is.numeric(x)) return(x)
    if (is.list(x)) {
      for (el in x) {
        v <- Recall(el)
        if (!is.null(v)) return(v)
      }
    }
    NULL
  }

  # ---- Step 2: Compositional invariance --------------------------------------
  s2 <- micom_out$Step2
  c_vals <- .first_vec(s2$Test_statistic)
  c_pvals <- .first_vec(s2$P_value)

  if (is.null(c_vals))
    stop("Could not extract Step 2 test statistics from testMICOM output.")

  construct_names <- names(c_vals)
  if (is.null(construct_names))
    construct_names <- paste0("C", seq_along(c_vals))

  # Compute 5th percentile of bootstrap c-values as critical value
  bv_list <- s2$Bootstrap_values
  ci_lower <- rep(NA_real_, length(c_vals))
  if (!is.null(bv_list) && length(bv_list) > 0) {
    bv_mat <- tryCatch({
      sapply(bv_list, function(x) .first_vec(x))
    }, error = function(e) NULL)
    if (!is.null(bv_mat) && is.matrix(bv_mat)) {
      ci_lower <- apply(bv_mat, 1, quantile, probs = 0.05, na.rm = TRUE)
    }
  }

  step2_df <- data.frame(
    Construct     = construct_names,
    c_original    = as.numeric(c_vals),
    ci_lower_5pct = ci_lower,
    p_value       = if (!is.null(c_pvals)) as.numeric(c_pvals) else NA_real_,
    stringsAsFactors = FALSE
  )

  # Decision: c_original >= ci_lower_5pct → INVARIANT
  step2_df$decision <- ifelse(
    !is.na(step2_df$c_original) &
      !is.na(step2_df$ci_lower_5pct) &
      step2_df$c_original >= step2_df$ci_lower_5pct,
    "INVARIANT", "NOT INVARIANT"
  )

  lh_log(log_info, sprintf("    Step 2 c-values: %s",
                            paste(sprintf("%s=%.4f", step2_df$Construct,
                                          step2_df$c_original),
                                  collapse = ", ")))

  step2_passed <- all(step2_df$decision == "INVARIANT", na.rm = TRUE)

  lh_log(log_info, sprintf(
    "    Step 2 result: %s",
    ifelse(step2_passed,
           "PASS (all constructs compositional invariant)",
           "FAIL (some constructs NOT compositional invariant)")))

  # ---- Step 3: Equal means & variances ---------------------------------------
  s3 <- micom_out$Step3
  step3_df <- .lh_parse_step3(s3, construct_names, alpha, log_info)

  list(
    step2_passed = step2_passed,
    step2_df     = step2_df,
    step3_df     = step3_df
  )
}

# ---- Parse Step 3 from cSEM output ------------------------------------------

#' Extract Step 3 (means & variances) from testMICOM Step3 element
#' cSEM 0.6.x structure:
#'   $Step3$Mean$Test_statistic$<pair_key>  — named numeric
#'   $Step3$Mean$P_value$<adjust>$<pair_key> — named numeric
#'   $Step3$Var$Test_statistic$<pair_key>   — named numeric
#'   $Step3$Var$P_value$<adjust>$<pair_key> — named numeric
#' @param s3 list — the $Step3 element from testMICOM
#' @param construct_names character vector of construct names
#' @param alpha significance level
#' @param log_info log object
#' @return data.frame
.lh_parse_step3 <- function(s3, construct_names, alpha, log_info) {

  n <- length(construct_names)

  # Recursive helper to extract the first numeric vector from nested lists
  .first_vec <- function(x) {
    if (is.numeric(x)) return(x)
    if (is.list(x)) {
      for (el in x) {
        v <- Recall(el)
        if (!is.null(v)) return(v)
      }
    }
    NULL
  }

  mean_diff <- rep(NA_real_, n)
  var_diff  <- rep(NA_real_, n)
  mean_p    <- rep(NA_real_, n)
  var_p     <- rep(NA_real_, n)

  if (!is.null(s3)) {
    # ---- Means ----
    md <- s3$Mean
    if (!is.null(md)) {
      v <- .first_vec(md$Test_statistic)
      if (!is.null(v)) mean_diff[seq_along(v)] <- as.numeric(v)
      v <- .first_vec(md$P_value)
      if (!is.null(v)) mean_p[seq_along(v)] <- as.numeric(v)
    }

    # ---- Variances ----
    vd <- s3$Var
    if (!is.null(vd)) {
      v <- .first_vec(vd$Test_statistic)
      if (!is.null(v)) var_diff[seq_along(v)] <- as.numeric(v)
      v <- .first_vec(vd$P_value)
      if (!is.null(v)) var_p[seq_along(v)] <- as.numeric(v)
    }
  }

  step3_df <- data.frame(
    Construct  = construct_names,
    Mean_Diff  = mean_diff,
    Var_Diff   = var_diff,
    Mean_p     = mean_p,
    Var_p      = var_p,
    Mean_Equal = ifelse(is.na(mean_p), NA_character_,
                        ifelse(mean_p > alpha, "YES", "NO")),
    Var_Equal  = ifelse(is.na(var_p), NA_character_,
                        ifelse(var_p > alpha, "YES", "NO")),
    stringsAsFactors = FALSE
  )

  if (!all(is.na(mean_p)) || !all(is.na(var_p))) {
    n_mean_sig <- sum(step3_df$Mean_Equal == "NO", na.rm = TRUE)
    n_var_sig  <- sum(step3_df$Var_Equal == "NO", na.rm = TRUE)
    lh_log(log_info, sprintf(
      "    Step 3: %d constructs differ in means, %d in variances",
      n_mean_sig, n_var_sig))
  }

  step3_df
}

# ---- Consolidate MICOM results across pairs ----------------------------------

.lh_consolidate_micom <- function(all_results) {
  rows <- list()
  for (nm in names(all_results)) {
    res <- all_results[[nm]]
    if (!is.null(res$step2_df) && nrow(res$step2_df) > 0) {
      df2 <- res$step2_df
      df2$Pair <- nm
      rows[[length(rows) + 1]] <- df2[, c("Pair", "Construct", "c_original",
                                           "ci_lower_5pct", "p_value",
                                           "decision")]
    }
  }
  if (length(rows) > 0) {
    do.call(rbind, rows)
  } else {
    data.frame(Pair = character(), Construct = character(),
               c_original = numeric(), ci_lower_5pct = numeric(),
               p_value = numeric(), decision = character(),
               stringsAsFactors = FALSE)
  }
}

# ---- Export MICOM results ----------------------------------------------------

#' Save MICOM tables to CSV
lh_export_micom <- function(micom_result, output_dir, cfg, log_info) {
  tbl_dir <- file.path(output_dir, "tables")

  if (!is.null(micom_result$results_df) &&
      nrow(micom_result$results_df) > 0) {
    write.csv(micom_result$results_df,
              file.path(tbl_dir, "micom_results.csv"), row.names = FALSE)
    lh_log(log_info, "  Saved: tables/micom_results.csv")
  }

  # Save Step 3 per pair
  for (nm in names(micom_result$all_results)) {
    res <- micom_result$all_results[[nm]]
    if (!is.null(res$step3_df) && nrow(res$step3_df) > 0) {
      fname <- paste0("micom_step3_", nm, ".csv")
      write.csv(res$step3_df, file.path(tbl_dir, fname), row.names = FALSE)
    }
  }
  lh_log(log_info, "  MICOM tables saved.")

  invisible(NULL)
}
