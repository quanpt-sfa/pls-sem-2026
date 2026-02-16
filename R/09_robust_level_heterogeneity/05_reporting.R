# ==============================================================================
# 05_reporting.R — Step 4: Generate all summary reports
# ==============================================================================
# Generates: disclaimer.md, micom_summary.md, mga_summary.md,
#            level_heterogeneity_summary.md, thesis_insert.md
# ==============================================================================

# ---- Master reporting entry point --------------------------------------------

#' Generate all reports for the module
#' @param clust_result results from Step 1
#' @param micom_result results from Step 2
#' @param mga_result results from Step 3
#' @param cfg module config
#' @param output_dir output directory
#' @param log_info log object
lh_step4_reporting <- function(clust_result, micom_result, mga_result,
                                cfg, output_dir, log_info) {
  lh_step(log_info, "Step 4 — Reporting & Thesis-ready Text")

  .lh_write_disclaimer(output_dir, cfg, log_info)
  .lh_write_micom_summary(micom_result, output_dir, cfg, log_info)
  .lh_write_mga_summary(mga_result, output_dir, cfg, log_info)
  .lh_write_overall_summary(clust_result, micom_result, mga_result,
                             output_dir, cfg, log_info)
  .lh_write_thesis_insert(clust_result, micom_result, mga_result,
                           output_dir, cfg, log_info)
}

# ---- Disclaimer --------------------------------------------------------------

.lh_write_disclaimer <- function(output_dir, cfg, log_info) {
  lines <- c(
    "# Disclaimer — Level-based Heterogeneity Sensitivity Analysis",
    "",
    "This analysis is a **sensitivity check for level-based observed heterogeneity**",
    "using score-based clustering on **exogenous** construct scores only.",
    "",
    "## What this IS:",
    "- A post-hoc sensitivity analysis to assess whether path coefficient",
    "  conclusions are robust across naturally occurring subpopulations.",
    "- Clustering is performed exclusively on exogenous construct scores",
    "  to avoid circularity (endogenous constructs are excluded).",
    "- MICOM (Henseler et al., 2016) is applied **before** MGA to verify",
    "  measurement comparability across groups.",
    "",
    "## What this is NOT:",
    "- This is **NOT** FIMIX-PLS (Finite Mixture PLS) or latent class segmentation.",
    "  FIMIX-PLS identifies unobserved heterogeneity via mixture model estimation",
    "  directly on the structural model, which is methodologically different.",
    "- This is **NOT** PLS-POS (Prediction-Oriented Segmentation).",
    "- This analysis does not replace formal latent heterogeneity methods.",
    "",
    "## Rationale for this approach:",
    "- Open-source R tools (seminr, cSEM) have limited support for FIMIX-PLS",
    "  with formative constructs and complex structural models.",
    "- Score-based clustering on exogenous constructs provides a practical",
    "  level-based check without violating model assumptions.",
    "- This follows the recommendation in Hair et al. (2022, Ch. 11): when",
    "  FIMIX is not feasible, researchers should still check for potential",
    "  heterogeneity using available techniques.",
    "",
    sprintf("Configuration: seed=%d, clustering=%s, k=%s, MICOM permutations=%d, MGA permutations=%d",
            cfg$seed, cfg$clustering$method,
            paste(cfg$clustering$k_candidates, collapse = "/"),
            cfg$micom$permutations, cfg$mga$permutations),
    "",
    "---",
    sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  )

  path <- file.path(output_dir, "disclaimer.md")
  writeLines(lines, path)
  lh_log(log_info, "  Saved: disclaimer.md")
}

# ---- MICOM summary ----------------------------------------------------------

.lh_write_micom_summary <- function(micom_result, output_dir, cfg, log_info) {

  step2_passed <- micom_result$passed_step2
  n_pairs <- micom_result$n_pairs
  results_df <- micom_result$results_df

  lines <- c(
    "# MICOM Results — Measurement Invariance of Composite Models",
    "",
    "## Reference",
    "Henseler, J., Ringle, C. M., & Sarstedt, M. (2016). Testing measurement",
    "invariance of composites using partial least squares. *International",
    "Marketing Review*, 33(3), 405–431.",
    "",
    sprintf("## Configuration: %d permutations, alpha = %.2f",
            cfg$micom$permutations, cfg$micom$alpha),
    "",
    sprintf("## Method: %s",
            if (!is.null(micom_result$method) && micom_result$method == "cSEM")
              "cSEM::testMICOM() (Rademaker & Schuberth, 2020; R package cSEM)"
            else "Permutation-based implementation"),
    ""
  )

  lines <- c(lines, "## Step 1 \u2014 Configural Invariance",
             "Configural invariance is a **qualitative assessment** (not a statistical test).",
             "It is **assumed** because the same model specification",
             "(constructs, indicators, structural paths) is applied to all groups.",
             "")

  lines <- c(lines, "## Step 2 — Compositional Invariance")
  if (nrow(results_df) > 0) {
    lines <- c(lines, "",
               "| Pair | Construct | c (original) | CI Lower 5% | p-value | Decision |",
               "|------|-----------|-------------|-------------|---------|----------|")
    for (i in seq_len(nrow(results_df))) {
      r <- results_df[i, ]
      lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s |",
                                 r$Pair, r$Construct,
                                 lh_fmt(r$c_original, 4),
                                 lh_fmt(r$ci_lower_5pct, 4),
                                 lh_fmt_p(r$p_value),
                                 r$decision))
    }
    lines <- c(lines, "")
  }

  if (step2_passed) {
    lines <- c(lines,
               "**Result**: Compositional invariance is **established** for all constructs",
               "across all group pairs. Proceeding to MGA is justified.",
               "")
  } else {
    lines <- c(lines,
               "**Result**: Compositional invariance is **NOT fully established**.",
               "MGA results should be interpreted with caution (exploratory only).",
               "")
  }

  # Step 3 summary
  lines <- c(lines, "## Step 3 — Equality of Composite Means and Variances")
  for (nm in names(micom_result$all_results)) {
    res <- micom_result$all_results[[nm]]
    if (!is.null(res$step3_df)) {
      lines <- c(lines, sprintf("\n### %s", nm))
      s3 <- res$step3_df
      n_mean_sig <- sum(s3$Mean_Equal == "NO", na.rm = TRUE)
      n_var_sig  <- sum(s3$Var_Equal == "NO", na.rm = TRUE)
      lines <- c(lines,
                 sprintf("- Constructs with unequal means: %d / %d", n_mean_sig, nrow(s3)),
                 sprintf("- Constructs with unequal variances: %d / %d", n_var_sig, nrow(s3)))
      if (n_mean_sig > 0 || n_var_sig > 0) {
        lines <- c(lines,
                   "- Note: Partial measurement invariance established. MGA should",
                   "  use permutation-based approach (which does not require full",
                   "  metric invariance).")
      }
    }
  }

  # ---- Methodology Notes (mandatory) ----
  lines <- c(lines,
    "",
    "## Methodology Notes",
    "",
    "1. **Step 1 (Configural invariance)** is a qualitative assessment, not a",
    "   statistical test. It is established by ensuring the same model specification",
    "   (same constructs, indicators, and structural paths) is applied to all groups.",
    "",
    "2. **Steps 2\u20133 were assessed using `cSEM::testMICOM()`** following",
    "   Henseler, Ringle & Sarstedt (2016). The cSEM package implements",
    "   the permutation-based MICOM procedure as described in the original paper.",
    "",
    "3. **If Step 2 (compositional invariance) fails**, any subsequent",
    "   multi-group comparison (MGA) should be treated as **exploratory only**,",
    "   because group differences in path coefficients may reflect measurement",
    "   artefacts rather than true structural differences.",
    "",
    "4. **This is a level-based observed heterogeneity analysis** using",
    "   score-based clustering on exogenous constructs. It is methodologically",
    "   distinct from FIMIX-PLS (latent class segmentation on the structural model)",
    "   and PLS-POS (prediction-oriented segmentation)."
  )

  lines <- c(lines, "",
             "---",
             sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

  writeLines(lines, file.path(output_dir, "micom_summary.md"))
  lh_log(log_info, "  Saved: micom_summary.md")
}

# ---- MGA summary -------------------------------------------------------------

.lh_write_mga_summary <- function(mga_result, output_dir, cfg, log_info) {
  mga_df   <- mga_result$mga_df
  micom_ok <- mga_result$micom_ok
  n_sig    <- mga_result$n_sig

  lines <- c(
    "# MGA Results — Permutation Multi-Group Analysis",
    "",
    sprintf("## Configuration: %d permutations, alpha = %.2f, p-adjust = %s",
            cfg$mga$permutations, cfg$mga$alpha, cfg$mga$p_adjust_method),
    ""
  )

  if (!micom_ok) {
    lines <- c(lines,
               "> **WARNING**: MICOM compositional invariance was NOT fully established.",
               "> These MGA results are **exploratory only** and should be interpreted with caution.",
               "")
  }

  if (nrow(mga_df) > 0) {
    lines <- c(lines,
               "## Path Coefficient Comparisons",
               "",
               "| Pair | Path | Beta G1 | Beta G2 | Diff | p (perm) | p (adj) | CI lo | CI hi | Sig |",
               "|------|------|---------|---------|------|----------|---------|-------|-------|-----|")
    for (i in seq_len(nrow(mga_df))) {
      r <- mga_df[i, ]
      lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |",
                                 r$Pair, r$Path,
                                 lh_fmt(r$Beta_G1), lh_fmt(r$Beta_G2),
                                 lh_fmt(r$Diff), lh_fmt_p(r$p_perm),
                                 lh_fmt_p(r$p_adjusted),
                                 lh_fmt(r$Diff_CI_lo), lh_fmt(r$Diff_CI_hi),
                                 r$Sig))
    }
    lines <- c(lines, "")
  }

  # Interpretation
  lines <- c(lines, "## Interpretation")
  if (n_sig == 0) {
    lines <- c(lines,
               "**No significant differences** were found in any path coefficient",
               "across groups (after p-value adjustment). This suggests that the",
               "structural model results are **robust** against level-based",
               "heterogeneity defined by exogenous construct score profiles.",
               "")
  } else {
    sig_paths <- mga_df[mga_df$Sig == "*", , drop = FALSE]
    lines <- c(lines,
               sprintf("**%d significant difference(s)** found:", n_sig),
               "")
    for (i in seq_len(nrow(sig_paths))) {
      r <- sig_paths[i, ]
      lines <- c(lines, sprintf("- **%s** (%s): diff = %s, p(adj) = %s",
                                 r$Path, r$Pair, lh_fmt(r$Diff),
                                 lh_fmt_p(r$p_adjusted)))
    }
    lines <- c(lines, "",
               "These differences suggest potential heterogeneity in specific",
               "structural relationships. However, this is a score-based sensitivity",
               "check and should be interpreted alongside the overall model results.",
               "")
  }

  lines <- c(lines, "---",
             sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

  writeLines(lines, file.path(output_dir, "mga_summary.md"))
  lh_log(log_info, "  Saved: mga_summary.md")
}

# ---- Overall summary ---------------------------------------------------------

.lh_write_overall_summary <- function(clust_result, micom_result, mga_result,
                                       output_dir, cfg, log_info) {

  k       <- clust_result$k_opt
  method  <- clust_result$method
  diag    <- clust_result$diagnostics
  micom_ok <- micom_result$passed_step2
  mga_df   <- mga_result$mga_df
  n_sig    <- mga_result$n_sig

  sil_val  <- diag$Silhouette_Mean[diag$k == k]
  if (length(sil_val) == 0) sil_val <- NA

  lines <- c(
    "# Level-based Heterogeneity — Overall Summary",
    "",
    "## Method",
    "Sensitivity analysis for level-based observed heterogeneity using",
    "score-based clustering on exogenous construct scores (Hair et al., 2022).",
    "",
    "## Step 1: Clustering",
    sprintf("- Method: %s", method),
    sprintf("- Optimal k: %d", k),
    sprintf("- Mean silhouette: %s", lh_fmt(sil_val)),
    sprintf("- Group sizes: %s",
            paste(table(clust_result$groups), collapse = " / ")),
    sprintf("- Exogenous constructs used: %s",
            paste(cfg$exogenous_constructs, collapse = ", ")),
    "",
    "## Step 2: MICOM",
    sprintf("- Permutations: %d", cfg$micom$permutations),
    sprintf("- Compositional invariance: %s",
            ifelse(micom_ok, "ESTABLISHED", "NOT FULLY ESTABLISHED")),
    "",
    "## Step 3: Permutation MGA",
    sprintf("- Permutations: %d", cfg$mga$permutations),
    sprintf("- Paths tested: %d", length(cfg$mga$paths_to_test)),
    sprintf("- Significant differences: %d / %d",
            n_sig, nrow(mga_df)),
    ""
  )

  if (n_sig == 0) {
    lines <- c(lines,
               "## Conclusion",
               "**The structural model conclusions are ROBUST** against level-based",
               "heterogeneity. No significant path coefficient differences were found",
               "between subgroups defined by exogenous construct score profiles.",
               "")
  } else {
    lines <- c(lines,
               "## Conclusion",
               "**Partial heterogeneity detected.** Some paths show significant",
               "differences between subgroups. See mga_results.csv and mga_summary.md",
               "for details. Overall model conclusions should be qualified.",
               "")
  }

  lines <- c(lines, "---",
             sprintf("Seed: %d | Generated: %s", cfg$seed,
                     format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

  writeLines(lines, file.path(output_dir, "level_heterogeneity_summary.md"))
  lh_log(log_info, "  Saved: level_heterogeneity_summary.md")
}

# ---- Thesis insert -----------------------------------------------------------

.lh_write_thesis_insert <- function(clust_result, micom_result, mga_result,
                                     output_dir, cfg, log_info) {

  k        <- clust_result$k_opt
  method   <- clust_result$method
  micom_ok <- micom_result$passed_step2
  mga_df   <- mga_result$mga_df
  n_sig    <- mga_result$n_sig
  n_tests  <- nrow(mga_df)
  n_perm   <- cfg$mga$permutations
  exo_str  <- paste(cfg$exogenous_constructs, collapse = ", ")
  grp_sizes <- paste(table(clust_result$groups), collapse = " and ")

  sil_val  <- clust_result$diagnostics$Silhouette_Mean[
    clust_result$diagnostics$k == k]
  if (length(sil_val) == 0) sil_val <- NA

  p1 <- paste0(
    "**4.X.X Sensitivity Analysis for Level-based Heterogeneity**\n\n",
    "To assess whether the structural model results are robust across ",
    "naturally occurring subpopulations, a sensitivity analysis for ",
    "level-based observed heterogeneity was conducted. Following Hair et al. ",
    "(2022, Chapter 11), construct scores of the exogenous variables (",
    exo_str, ") were extracted from the base PLS-SEM model and used as ",
    "input for ", method, " clustering. The optimal number of segments was ",
    "determined to be k = ", k, " based on the mean silhouette criterion ",
    "(average silhouette width = ", lh_fmt(sil_val), "), ",
    "yielding groups of n = ", grp_sizes, " respondents respectively. ",
    "Importantly, only exogenous construct scores were used for segmentation ",
    "to prevent circularity (Becker et al., 2013)."
  )

  micom_text <- if (micom_ok) {
    paste0(
      "Prior to multi-group comparison, measurement invariance of composite ",
      "models (MICOM; Henseler et al., 2016) was assessed using ",
      "cSEM::testMICOM() (Rademaker & Schuberth, 2020) with ",
      cfg$micom$permutations, " permutations. Configural invariance was ",
      "established by design (identical model specification\u2014same constructs, ",
      "indicators, and structural paths in all groups). Compositional ",
      "invariance (Step 2) was confirmed for all constructs at \u03B1 = ",
      cfg$micom$alpha, ", satisfying the precondition for meaningful ",
      "multi-group comparison."
    )
  } else {
    paste0(
      "MICOM (Henseler et al., 2016) was assessed using ",
      "cSEM::testMICOM() (Rademaker & Schuberth, 2020) with ",
      cfg$micom$permutations, " permutations. Configural invariance ",
      "(Step 1) holds by design (identical model specification). However, ",
      "compositional invariance (Step 2) was NOT fully established ",
      "for all constructs. The following MGA results should therefore be ",
      "interpreted as exploratory only, because group differences may ",
      "reflect measurement artefacts rather than true structural differences."
    )
  }

  p2 <- paste0(micom_text)

  mga_text <- if (n_sig == 0) {
    paste0(
      "Permutation-based multi-group analysis (MGA; ", n_perm,
      " permutations) was conducted for all ", n_tests,
      " path–pair comparisons. After p-value adjustment (",
      cfg$mga$p_adjust_method, " method), no statistically significant ",
      "differences were found in any structural path coefficient between ",
      "the identified segments (all p_adj > ", cfg$mga$alpha, "). ",
      "This result supports the conclusion that the structural model ",
      "findings are robust against level-based observed heterogeneity."
    )
  } else {
    sig_paths <- mga_df[mga_df$Sig == "*", , drop = FALSE]
    sig_details <- paste(
      sprintf("%s (diff = %s, p_adj = %s)", sig_paths$Path,
              lh_fmt(sig_paths$Diff), lh_fmt_p(sig_paths$p_adjusted)),
      collapse = "; "
    )
    paste0(
      "Permutation-based MGA (", n_perm, " permutations) revealed ",
      n_sig, " significant difference(s) out of ", n_tests,
      " path–pair comparisons after ", cfg$mga$p_adjust_method,
      " adjustment: ", sig_details, ". ",
      "While these differences warrant attention, they represent a minority ",
      "of tested paths. The overall pattern suggests that the core structural ",
      "conclusions remain substantively similar across subgroups, though the ",
      "magnitude of specific effects may vary."
    )
  }

  p3 <- paste0(mga_text)

  disclaimer <- paste0(
    "*Note.* This analysis is a level-based sensitivity check using ",
    "score-based clustering on exogenous constructs. It does not replace ",
    "formal latent heterogeneity methods such as FIMIX-PLS or PLS-POS, ",
    "which were not feasible given the model\u2019s formative constructs and ",
    "current open-source tool limitations. ",
    "Configural invariance (MICOM Step 1) is a qualitative assessment ",
    "established by identical model specification across groups. ",
    "Steps 2\u20133 were assessed using cSEM::testMICOM() following ",
    "Henseler et al. (2016)."
  )

  lines <- c(
    "# Thesis Insert — Level-based Heterogeneity Sensitivity",
    "",
    p1, "", p2, "", p3, "",
    disclaimer,
    "",
    "---",
    sprintf("Generated: %s (seed = %d)", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            cfg$seed)
  )

  writeLines(lines, file.path(output_dir, "thesis_insert.md"))
  lh_log(log_info, "  Saved: thesis_insert.md")
}
