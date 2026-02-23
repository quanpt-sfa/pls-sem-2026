# ==============================================================================
# run_compare_com6.R
# Snapshot baseline -> run no-COM6 variant -> snapshot variant -> compare outputs
# ==============================================================================

suppressPackageStartupMessages({
  library(jsonlite)
})

first_existing_col <- function(df, candidates) {
  for (nm in candidates) {
    if (nm %in% names(df)) return(nm)
  }
  NA_character_
}

read_csv_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

copy_if_exists <- function(src, dst) {
  if (!file.exists(src)) {
    warning(sprintf("Missing source file: %s", src), call. = FALSE)
    return(FALSE)
  }
  dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(src, dst, overwrite = TRUE)
  if (!ok) warning(sprintf("Failed to copy: %s -> %s", src, dst), call. = FALSE)
  ok
}

copy_tree_if_exists <- function(src, dst) {
  if (!file.exists(src)) return(FALSE)
  dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(src)) {
    if (dir.exists(dst)) unlink(dst, recursive = TRUE, force = TRUE)
    ok <- file.copy(src, dst, recursive = TRUE)
  } else {
    ok <- file.copy(src, dst, overwrite = TRUE)
  }
  if (!ok) warning(sprintf("Failed to copy tree: %s -> %s", src, dst), call. = FALSE)
  ok
}

restore_tree <- function(backup_path, target_path) {
  if (!file.exists(backup_path)) {
    if (file.exists(target_path)) unlink(target_path, recursive = TRUE, force = TRUE)
    return(invisible(TRUE))
  }

  if (file.exists(target_path)) unlink(target_path, recursive = TRUE, force = TRUE)
  dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(backup_path, target_path, recursive = TRUE)
  if (!ok) warning(sprintf("Failed to restore: %s -> %s", backup_path, target_path), call. = FALSE)
  invisible(ok)
}

to_logical <- function(x) {
  if (is.logical(x)) return(x)
  y <- tolower(trimws(as.character(x)))
  ifelse(y %in% c("true", "t", "1", "yes", "y"), TRUE,
         ifelse(y %in% c("false", "f", "0", "no", "n"), FALSE, NA))
}

extract_first <- function(df, col) {
  if (is.null(df) || nrow(df) == 0 || !(col %in% names(df))) return(NA_character_)
  as.character(df[[col]][1])
}

extract_com_active <- function(instrument_obj) {
  if (is.null(instrument_obj$constructs)) return(character(0))
  for (cc in instrument_obj$constructs) {
    if (!is.null(cc$name) && identical(cc$name, "COM")) {
      return(as.character(unlist(cc$active_indicators)))
    }
  }
  character(0)
}

run_compare_com6 <- function() {
  root <- getwd()
  cat(sprintf("[INFO] Working directory: %s\n", root))

  out_root <- file.path("10_report", "com6_sensitivity")
  baseline_dir <- file.path(out_root, "baseline")
  variant_dir  <- file.path(out_root, "no_com6")
  compare_dir  <- file.path(out_root, "compare")
  backup_dir   <- file.path(out_root, "_backup_main_state")
  dir.create(baseline_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(variant_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(compare_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)

  mutable_paths <- c(
    "01_raw",
    "02_clean",
    "03_qc",
    "04_cmv",
    "04_descriptives",
    "05_measurement",
    "06_structural",
    "07_predict",
    "08_complex",
    "09_robust",
    "logs"
  )

  restored <- FALSE
  restore_main_state <- function() {
    if (restored) return(invisible(TRUE))
    cat("[INFO] Restoring main-state folders (no persistent overwrite)\n")
    for (p in mutable_paths) {
      restore_tree(file.path(backup_dir, p), p)
    }
    restored <<- TRUE
    invisible(TRUE)
  }
  on.exit(restore_main_state(), add = TRUE)

  targets <- c(
    "06_structural/path_coefficients_bootstrap.csv",
    "06_structural/r_squared.csv",
    "06_structural/f_squared.csv",
    "07_predict/prediction_classification.csv",
    "05_measurement/formative/indicator_decisions.csv",
    "10_report/main/instrument_final.json"
  )

  cat("[INFO] Step 1/4: Snapshot baseline outputs\n")
  for (src in targets) {
    copy_if_exists(src, file.path(baseline_dir, basename(src)))
  }

  cat("[INFO] Step 1b/4: Backup shared pipeline folders before no-COM6 run\n")
  for (p in mutable_paths) {
    copy_tree_if_exists(p, file.path(backup_dir, p))
  }

  cat("[INFO] Step 2/4: Run no-COM6 pipeline\n")
  rc <- system2("Rscript", c("run_main_no_com6.R"))
  if (!identical(rc, 0L)) {
    stop(sprintf("No-COM6 run failed with exit code %s", rc))
  }

  cat("[INFO] Step 3/4: Snapshot no-COM6 outputs\n")
  for (src in targets) {
    if (identical(src, "10_report/main/instrument_final.json")) {
      src_variant <- "10_report/main_no_com6/instrument_final.json"
    } else {
      src_variant <- src
    }
    copy_if_exists(src_variant, file.path(variant_dir, basename(src_variant)))
  }

  cat("[INFO] Step 4/4: Build comparison tables\n")

  baseline_path <- read_csv_safe(file.path(baseline_dir, "path_coefficients_bootstrap.csv"))
  variant_path  <- read_csv_safe(file.path(variant_dir, "path_coefficients_bootstrap.csv"))
  if (!is.null(baseline_path) && !is.null(variant_path)) {
    key_col <- first_existing_col(baseline_path, c("Path"))
    key_col2 <- first_existing_col(variant_path, c("Path"))
    beta_col <- first_existing_col(baseline_path, c("Beta"))
    p_col <- first_existing_col(baseline_path, c("P_Value", "p_value"))
    sig_col <- first_existing_col(baseline_path, c("Sig_Primary", "Significant"))

    beta_col2 <- first_existing_col(variant_path, c("Beta"))
    p_col2 <- first_existing_col(variant_path, c("P_Value", "p_value"))
    sig_col2 <- first_existing_col(variant_path, c("Sig_Primary", "Significant"))

    if (!is.na(key_col) && !is.na(key_col2) && !is.na(beta_col) && !is.na(beta_col2)) {
      b <- baseline_path[, unique(c(key_col, beta_col, p_col, sig_col)), drop = FALSE]
      v <- variant_path[, unique(c(key_col2, beta_col2, p_col2, sig_col2)), drop = FALSE]
      names(b)[1] <- "Path"
      names(v)[1] <- "Path"

      cmp <- merge(b, v, by = "Path", all = TRUE, suffixes = c("_Baseline", "_NoCOM6"))
      beta_b <- first_existing_col(cmp, c(paste0(beta_col, "_Baseline"), paste0("Beta", "_Baseline")))
      beta_v <- first_existing_col(cmp, c(paste0(beta_col2, "_NoCOM6"), paste0("Beta", "_NoCOM6")))
      sig_b <- first_existing_col(cmp, c(paste0(sig_col, "_Baseline"), paste0("Sig_Primary", "_Baseline"), paste0("Significant", "_Baseline")))
      sig_v <- first_existing_col(cmp, c(paste0(sig_col2, "_NoCOM6"), paste0("Sig_Primary", "_NoCOM6"), paste0("Significant", "_NoCOM6")))

      cmp$Delta_Beta <- as.numeric(cmp[[beta_v]]) - as.numeric(cmp[[beta_b]])
      cmp$Sig_Flip <- if (!is.na(sig_b) && !is.na(sig_v)) {
        sb <- to_logical(cmp[[sig_b]])
        sv <- to_logical(cmp[[sig_v]])
        ifelse(is.na(sb) | is.na(sv), NA, sb != sv)
      } else {
        NA
      }

      write.csv(cmp, file.path(compare_dir, "path_coefficients_compare.csv"), row.names = FALSE)
    }
  }

  baseline_r2 <- read_csv_safe(file.path(baseline_dir, "r_squared.csv"))
  variant_r2  <- read_csv_safe(file.path(variant_dir, "r_squared.csv"))
  if (!is.null(baseline_r2) && !is.null(variant_r2)) {
    key_col <- first_existing_col(baseline_r2, c("Construct"))
    val_col <- first_existing_col(baseline_r2, c("R_squared"))
    key_col2 <- first_existing_col(variant_r2, c("Construct"))
    val_col2 <- first_existing_col(variant_r2, c("R_squared"))

    if (!is.na(key_col) && !is.na(val_col) && !is.na(key_col2) && !is.na(val_col2)) {
      b <- baseline_r2[, c(key_col, val_col), drop = FALSE]
      v <- variant_r2[, c(key_col2, val_col2), drop = FALSE]
      names(b) <- c("Construct", "R2_Baseline")
      names(v) <- c("Construct", "R2_NoCOM6")
      cmp <- merge(b, v, by = "Construct", all = TRUE)
      cmp$Delta_R2 <- as.numeric(cmp$R2_NoCOM6) - as.numeric(cmp$R2_Baseline)
      write.csv(cmp, file.path(compare_dir, "r_squared_compare.csv"), row.names = FALSE)
    }
  }

  baseline_f2 <- read_csv_safe(file.path(baseline_dir, "f_squared.csv"))
  variant_f2  <- read_csv_safe(file.path(variant_dir, "f_squared.csv"))
  if (!is.null(baseline_f2) && !is.null(variant_f2)) {
    iv <- first_existing_col(baseline_f2, c("IV"))
    dv <- first_existing_col(baseline_f2, c("DV"))
    fv <- first_existing_col(baseline_f2, c("f_squared"))

    iv2 <- first_existing_col(variant_f2, c("IV"))
    dv2 <- first_existing_col(variant_f2, c("DV"))
    fv2 <- first_existing_col(variant_f2, c("f_squared"))

    if (!is.na(iv) && !is.na(dv) && !is.na(fv) && !is.na(iv2) && !is.na(dv2) && !is.na(fv2)) {
      b <- baseline_f2[, c(iv, dv, fv), drop = FALSE]
      v <- variant_f2[, c(iv2, dv2, fv2), drop = FALSE]
      names(b) <- c("IV", "DV", "f2_Baseline")
      names(v) <- c("IV", "DV", "f2_NoCOM6")
      cmp <- merge(b, v, by = c("IV", "DV"), all = TRUE)
      cmp$Delta_f2 <- as.numeric(cmp$f2_NoCOM6) - as.numeric(cmp$f2_Baseline)
      write.csv(cmp, file.path(compare_dir, "f_squared_compare.csv"), row.names = FALSE)
    }
  }

  baseline_pred <- read_csv_safe(file.path(baseline_dir, "prediction_classification.csv"))
  variant_pred  <- read_csv_safe(file.path(variant_dir, "prediction_classification.csv"))
  if (!is.null(baseline_pred) && !is.null(variant_pred)) {
    metrics <- c("Prediction_Level", "Pct_better", "N_RMSE_PLS_better", "N_MAE_PLS_better")
    pred_cmp <- data.frame(
      Metric = metrics,
      Baseline = vapply(metrics, function(m) extract_first(baseline_pred, m), character(1)),
      No_COM6 = vapply(metrics, function(m) extract_first(variant_pred, m), character(1)),
      stringsAsFactors = FALSE
    )
    write.csv(pred_cmp, file.path(compare_dir, "prediction_compare.csv"), row.names = FALSE)
  }

  baseline_form <- read_csv_safe(file.path(baseline_dir, "indicator_decisions.csv"))
  variant_form  <- read_csv_safe(file.path(variant_dir, "indicator_decisions.csv"))
  if (!is.null(baseline_form) && !is.null(variant_form)) {
    b <- baseline_form[baseline_form$Construct == "COM" | grepl("^COM", baseline_form$Indicator), , drop = FALSE]
    v <- variant_form[variant_form$Construct == "COM" | grepl("^COM", variant_form$Indicator), , drop = FALSE]
    b <- b[, intersect(c("Indicator", "Decision", "Reason", "Weight", "Loading"), names(b)), drop = FALSE]
    v <- v[, intersect(c("Indicator", "Decision", "Reason", "Weight", "Loading"), names(v)), drop = FALSE]

    if ("Indicator" %in% names(b) && "Indicator" %in% names(v)) {
      cmp <- merge(b, v, by = "Indicator", all = TRUE, suffixes = c("_Baseline", "_NoCOM6"))
      if ("Decision_Baseline" %in% names(cmp) && "Decision_NoCOM6" %in% names(cmp)) {
        cmp$Decision_Changed <- ifelse(is.na(cmp$Decision_Baseline) | is.na(cmp$Decision_NoCOM6),
                                       NA,
                                       cmp$Decision_Baseline != cmp$Decision_NoCOM6)
      }
      write.csv(cmp, file.path(compare_dir, "formative_com_compare.csv"), row.names = FALSE)
    }
  }

  baseline_inst_path <- file.path(baseline_dir, "instrument_final.json")
  variant_inst_path  <- file.path(variant_dir, "instrument_final.json")
  if (file.exists(baseline_inst_path) && file.exists(variant_inst_path)) {
    b_inst <- jsonlite::fromJSON(baseline_inst_path, simplifyVector = FALSE)
    v_inst <- jsonlite::fromJSON(variant_inst_path, simplifyVector = FALSE)
    b_com <- sort(unique(extract_com_active(b_inst)))
    v_com <- sort(unique(extract_com_active(v_inst)))
    all_ind <- sort(unique(c(b_com, v_com)))
    inst_cmp <- data.frame(
      Indicator = all_ind,
      In_Baseline = all_ind %in% b_com,
      In_NoCOM6 = all_ind %in% v_com,
      Removed_In_NoCOM6 = all_ind %in% setdiff(b_com, v_com),
      Added_In_NoCOM6 = all_ind %in% setdiff(v_com, b_com),
      stringsAsFactors = FALSE
    )
    write.csv(inst_cmp, file.path(compare_dir, "instrument_com_compare.csv"), row.names = FALSE)
  }

  path_cmp <- read_csv_safe(file.path(compare_dir, "path_coefficients_compare.csv"))
  r2_cmp <- read_csv_safe(file.path(compare_dir, "r_squared_compare.csv"))
  pred_cmp <- read_csv_safe(file.path(compare_dir, "prediction_compare.csv"))

  sig_flip_n <- if (!is.null(path_cmp) && "Sig_Flip" %in% names(path_cmp)) {
    sum(path_cmp$Sig_Flip %in% TRUE, na.rm = TRUE)
  } else {
    NA_integer_
  }

  com6_row <- NULL
  if (!is.null(path_cmp) && "Path" %in% names(path_cmp)) {
    com6_row <- path_cmp[grepl("COM", path_cmp$Path), , drop = FALSE]
  }

  summary_lines <- c(
    "COM6 Sensitivity Comparison Summary",
    sprintf("Generated: %s", Sys.time()),
    "",
    sprintf("Path table rows: %s", if (is.null(path_cmp)) "NA" else nrow(path_cmp)),
    sprintf("Significance flips: %s", ifelse(is.na(sig_flip_n), "NA", as.character(sig_flip_n))),
    sprintf("R2 table rows: %s", if (is.null(r2_cmp)) "NA" else nrow(r2_cmp)),
    sprintf("Prediction table rows: %s", if (is.null(pred_cmp)) "NA" else nrow(pred_cmp)),
    sprintf("Main-state restored: %s", ifelse(restored, "YES", "PENDING (on.exit)")),
    "",
    sprintf("Output folder: %s", normalizePath(compare_dir, winslash = "/", mustWork = FALSE))
  )
  writeLines(summary_lines, con = file.path(compare_dir, "summary.txt"))

  restore_main_state()

  cat(sprintf("[DONE] Compare outputs written to: %s\n", normalizePath(compare_dir, winslash = "/", mustWork = FALSE)))
}

if (sys.nframe() == 0) {
  run_compare_com6()
}
