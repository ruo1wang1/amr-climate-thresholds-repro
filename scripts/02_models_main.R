# Main modeling script : Segmented regression Model 1 & Model 2
# - Reads derived anonymized country/region-year datasets under data/derived/
# - Fits strain-specific segmented regression with threshold (segmented::segmented)
# - Exports thresholds and coefficient tables for reproducibility

rm(list = ls())
options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(data.table)
  library(segmented)
  library(lmtest)
  library(sandwich)
})

# ----------------------------
# 0) Paths (portable)
# ----------------------------
ROOT <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (basename(ROOT) == "scripts") ROOT <- normalizePath(file.path(ROOT, ".."), winslash = "/", mustWork = TRUE)

DATA_DERIVED_DIR <- file.path(ROOT, "data", "derived")
OUT_DIR <- file.path(ROOT, "output")
OUT_MODELS_DIR <- file.path(OUT_DIR, "models")
OUT_TABLES_DIR <- file.path(OUT_DIR, "tables")
OUT_LOG_DIR <- file.path(OUT_DIR, "logs")

dir.create(OUT_MODELS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TABLES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_LOG_DIR, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(OUT_LOG_DIR, paste0("run_models_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
sink(log_file, split = TRUE)
cat("Start time:", as.character(Sys.time()), "\n")
cat("Repo root:", ROOT, "\n")

# ----------------------------
# 1) Helpers
# ----------------------------
safe_log <- function(x) {
  # robust log for non-positive values
  x <- as.numeric(x)
  ifelse(is.na(x), NA_real_, log(ifelse(x <= 0, x + 1, x)))
}

make_time_period <- function(year_vec) {
  cut(
    year_vec,
    breaks = c(1998, 2006, 2014, 2022),
    labels = c("1999-2006", "2007-2014", "2015-2022")
  )
}

# Model 1 definition (paper): dummy variables for 2007–2014 and 2015–2022, reference 1999–2006
# See manuscript formula: Y = ... + β3 TimePeriod1 + β4 TimePeriod2 + β5 TMP×TimePeriod1 + β6 TMP×TimePeriod2 + ...
# :contentReference[oaicite:9]{index=9}
add_time_dummies_model1 <- function(dt) {
  dt[, TimePeriod := make_time_period(year)]
  dt[, TimePeriod1 := as.integer(TimePeriod == "2007-2014")]
  dt[, TimePeriod2 := as.integer(TimePeriod == "2015-2022")]
  dt[]
}

# Model 2 definition: categorical TimePeriod (3 levels), plus interactions TMP*TimePeriod
# :contentReference[oaicite:10]{index=10}
add_time_factor_model2 <- function(dt) {
  dt[, TimePeriod := factor(make_time_period(year),
                            levels = c("1999-2006", "2007-2014", "2015-2022"))]
  dt[]
}

# Extract threshold from segmented object
extract_threshold <- function(seg_obj) {
  if (is.null(seg_obj$psi) || nrow(seg_obj$psi) == 0) return(list(threshold = NA_real_, se = NA_real_))
  # psi has columns like "Est.", "St.Err"
  psi <- seg_obj$psi
  est_col <- if ("Est." %in% colnames(psi)) "Est." else colnames(psi)[1]
  se_col  <- if ("St.Err" %in% colnames(psi)) "St.Err" else if ("Std.Err" %in% colnames(psi)) "Std.Err" else NA
  thr <- as.numeric(psi[1, est_col])
  se  <- if (!is.na(se_col)) as.numeric(psi[1, se_col]) else NA_real_
  list(threshold = thr, se = se)
}

# Robust coefficient table (HC1) for lm / segmented objects
robust_coeftable <- function(model_obj) {
  V <- try(sandwich::vcovHC(model_obj, type = "HC1"), silent = TRUE)
  if (inherits(V, "try-error")) {
    # fallback to default vcov
    V <- stats::vcov(model_obj)
  }
  ct <- lmtest::coeftest(model_obj, vcov. = V)
  out <- as.data.table(ct, keep.rownames = "term")
  setnames(out, c("term", "estimate", "std_error", "t_value", "p_value"))
  out[]
}

# ----------------------------
# 2) Strain-specific covariate maps (from your RTF model scripts)
# ----------------------------
# Model 1 covariates
COVS_MODEL1 <- list(
  "3GCREC_merged_anonymized.csv" = c("HEG","OPE","SR","SSR","TCL","HUM","CPI","WASH","log_AMC","IC","HD"), # :contentReference[oaicite:11]{index=11}
  "3GCRKP_merged_anonymized.csv" = c("HEG","log_IT","OPE","log_PMP","SR","SSR","TCL","CPI"),              # :contentReference[oaicite:12]{index=12}
  "CRAB_merged_anonymized.csv"   = c("log_IT","UP","log_PMP","SR","SSR","HUM","CPI","log_AMC","HD"),      # :contentReference[oaicite:13]{index=13}
  "CREC_merged_anonymized.csv"   = c("HEG","OPE","log_PD","UP","PREC","SR","WASH","IC","HD"),             # :contentReference[oaicite:14]{index=14}
  "CRKP_merged_anonymized.csv"   = c("OPE","UP","SR","HUM","CPI","WASH","log_AMC","IC"),                  # :contentReference[oaicite:15]{index=15}
  "CRPA_merged_anonymized.csv"   = c("OPE","log_PD","PREC","SR","TCL","HUM","CPI","WASH","HD")            # :contentReference[oaicite:16]{index=16}
)

# Model 2 uses the same covariate sets as Model 1 (paper), with categorical TimePeriod and interactions
# :contentReference[oaicite:17]{index=17}
COVS_MODEL2 <- COVS_MODEL1

# ----------------------------
# 3) Read and preprocess derived dataset
# ----------------------------
read_panel <- function(path) {
  dt <- fread(path)
  req <- c("NAME", "year", "TMP", "R")
  miss <- setdiff(req, names(dt))
  if (length(miss) > 0) {
    stop("Dataset missing required columns: ", paste(miss, collapse = ", "),
         "\nFile: ", path)
  }
  
  # Ensure numeric
  dt[, R := as.numeric(R) * 100]
  dt[, TMP := as.numeric(TMP)]
  dt[, year := as.integer(year)]
  
  # Standard logs if base variables exist
  if ("AMC" %in% names(dt)) dt[, log_AMC := safe_log(AMC)]
  if ("IT"  %in% names(dt)) dt[, log_IT  := safe_log(IT)]
  if ("PMP" %in% names(dt)) dt[, log_PMP := safe_log(PMP)]
  if ("PD"  %in% names(dt)) dt[, log_PD  := safe_log(PD)]
  
  dt[]
}

# ----------------------------
# 4) Fit Model 1 and Model 2 (segmented)
# ----------------------------
fit_segmented_model1 <- function(dt, covs) {
  dt <- copy(dt)
  dt <- add_time_dummies_model1(dt)
  
  # Build formula: R ~ TMP + TimePeriod1 + TimePeriod2 + TMP:TimePeriod1 + TMP:TimePeriod2 + covariates
  f_rhs <- c(
    "TMP", "TimePeriod1", "TimePeriod2",
    "TMP:TimePeriod1", "TMP:TimePeriod2",
    covs
  )
  f <- as.formula(paste("R ~", paste(f_rhs, collapse = " + ")))
  
  initial_lm <- lm(f, data = dt)
  seg_model <- try(segmented(initial_lm, seg.Z = ~ TMP), silent = TRUE)
  
  list(initial_lm = initial_lm, seg_model = seg_model)
}

fit_segmented_model2 <- function(dt, covs) {
  dt <- copy(dt)
  dt <- add_time_factor_model2(dt)
  
  # Build formula: R ~ TMP * TimePeriod + covariates
  f_rhs <- c("TMP * TimePeriod", covs)
  f <- as.formula(paste("R ~", paste(f_rhs, collapse = " + ")))
  
  initial_lm <- lm(f, data = dt)
  seg_model <- try(segmented(initial_lm, seg.Z = ~ TMP), silent = TRUE)
  
  list(initial_lm = initial_lm, seg_model = seg_model)
}

# ----------------------------
# 5) Run across all 6 derived datasets
# ----------------------------
files <- list.files(DATA_DERIVED_DIR, pattern = "\\.csv$", full.names = TRUE)
if (length(files) == 0) stop("No CSV files found under: ", DATA_DERIVED_DIR)

threshold_rows_m1 <- list()
threshold_rows_m2 <- list()
coef_rows_m1 <- list()
coef_rows_m2 <- list()

for (fp in files) {
  combo_file <- basename(fp)
  cat("\n--- Running:", combo_file, " ---\n")
  
  if (!(combo_file %in% names(COVS_MODEL1))) {
    cat("WARNING: No covariate mapping for ", combo_file, " (skipping)\n")
    next
  }
  
  dt <- read_panel(fp)
  
  covs1 <- COVS_MODEL1[[combo_file]]
  covs2 <- COVS_MODEL2[[combo_file]]
  
  # Check covariate presence; hard-fail if missing (you said they are present)
  miss1 <- setdiff(covs1, names(dt))
  if (length(miss1) > 0) stop("Missing covariates for Model1 in ", combo_file, ": ", paste(miss1, collapse = ", "))
  miss2 <- setdiff(covs2, names(dt))
  if (length(miss2) > 0) stop("Missing covariates for Model2 in ", combo_file, ": ", paste(miss2, collapse = ", "))
  
  # --- Model 1 ---
  m1 <- fit_segmented_model1(dt, covs1)
  saveRDS(m1$initial_lm, file.path(OUT_MODELS_DIR, paste0(combo_file, "_Model1_initial_lm.rds")))
  
  if (!inherits(m1$seg_model, "try-error")) {
    saveRDS(m1$seg_model, file.path(OUT_MODELS_DIR, paste0(combo_file, "_Model1_segmented.rds")))
    thr <- extract_threshold(m1$seg_model)
    threshold_rows_m1[[combo_file]] <- data.table(
      combination = combo_file,
      model = "Model1",
      threshold_TMP = thr$threshold,
      threshold_se = thr$se
    )
    ct1 <- robust_coeftable(m1$seg_model)
  } else {
    cat("Model1 segmented failed for ", combo_file, "\n")
    threshold_rows_m1[[combo_file]] <- data.table(
      combination = combo_file,
      model = "Model1",
      threshold_TMP = NA_real_,
      threshold_se = NA_real_
    )
    # fallback: robust on initial lm
    ct1 <- robust_coeftable(m1$initial_lm)
  }
  ct1[, `:=`(combination = combo_file, model = "Model1")]
  coef_rows_m1[[combo_file]] <- ct1
  
  # --- Model 2 ---
  m2 <- fit_segmented_model2(dt, covs2)
  saveRDS(m2$initial_lm, file.path(OUT_MODELS_DIR, paste0(combo_file, "_Model2_initial_lm.rds")))
  
  if (!inherits(m2$seg_model, "try-error")) {
    saveRDS(m2$seg_model, file.path(OUT_MODELS_DIR, paste0(combo_file, "_Model2_segmented.rds")))
    thr2 <- extract_threshold(m2$seg_model)
    threshold_rows_m2[[combo_file]] <- data.table(
      combination = combo_file,
      model = "Model2",
      threshold_TMP = thr2$threshold,
      threshold_se = thr2$se
    )
    ct2 <- robust_coeftable(m2$seg_model)
  } else {
    cat("Model2 segmented failed for ", combo_file, "\n")
    threshold_rows_m2[[combo_file]] <- data.table(
      combination = combo_file,
      model = "Model2",
      threshold_TMP = NA_real_,
      threshold_se = NA_real_
    )
    ct2 <- robust_coeftable(m2$initial_lm)
  }
  ct2[, `:=`(combination = combo_file, model = "Model2")]
  coef_rows_m2[[combo_file]] <- ct2
}

# ----------------------------
# 6) Write tables
# ----------------------------
thr_m1 <- rbindlist(threshold_rows_m1, use.names = TRUE, fill = TRUE)
thr_m2 <- rbindlist(threshold_rows_m2, use.names = TRUE, fill = TRUE)
coef_m1 <- rbindlist(coef_rows_m1, use.names = TRUE, fill = TRUE)
coef_m2 <- rbindlist(coef_rows_m2, use.names = TRUE, fill = TRUE)

fwrite(thr_m1, file.path(OUT_TABLES_DIR, "thresholds_model1.csv"))
fwrite(thr_m2, file.path(OUT_TABLES_DIR, "thresholds_model2.csv"))
fwrite(coef_m1, file.path(OUT_TABLES_DIR, "coefficients_model1.csv"))
fwrite(coef_m2, file.path(OUT_TABLES_DIR, "coefficients_model2.csv"))

cat("\nDone. Outputs written to:", OUT_DIR, "\n")
cat("End time:", as.character(Sys.time()), "\n")
sink()
