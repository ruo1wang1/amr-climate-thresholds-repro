# scripts/02_models_main.R
# Main modeling script: Model0 / Model1 / Full / Model2
# - Reads derived anonymized country/region-year datasets under data/derived/
# - Runs models per pathogen–drug combination (file-wise)
# - Writes outputs under output/ (tables, model objects, logs)

rm(list = ls())
options(stringsAsFactors = FALSE)

# ----------------------------
# 0) Paths (portable, no personal directories)
# ----------------------------
ROOT <- normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = FALSE)
# If run from repo root, ROOT should be repo root; if sourced from scripts/, adjust:
if (basename(getwd()) == "scripts") {
  ROOT <- normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = TRUE)
} else {
  ROOT <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

DATA_DERIVED_DIR <- file.path(ROOT, "data", "derived")
OUT_DIR <- file.path(ROOT, "output")
OUT_MODELS_DIR <- file.path(OUT_DIR, "models")
OUT_TABLES_DIR <- file.path(OUT_DIR, "tables")
OUT_LOG_DIR <- file.path(OUT_DIR, "logs")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_MODELS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TABLES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_LOG_DIR, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(OUT_LOG_DIR, paste0("run_models_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
sink(log_file, split = TRUE)
cat("Start time:", as.character(Sys.time()), "\n")
cat("Repo root:", ROOT, "\n")

# ----------------------------
# 1) Packages (keep minimal; add as needed)
# ----------------------------
pkgs <- c("data.table")
missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  stop("Missing packages: ", paste(missing, collapse = ", "),
       "\nInstall them first, e.g., install.packages(c(", paste0('"', missing, '"', collapse = ","), "))")
}
library(data.table)

# ----------------------------
# 2) Helper: read dataset + basic checks
# ----------------------------
read_panel <- function(path) {
  dt <- fread(path)
  req <- c("NAME", "year", "TMP", "R")  # adjust if your dataset uses different names
  miss <- setdiff(req, names(dt))
  if (length(miss) > 0) {
    stop("Dataset missing required columns: ", paste(miss, collapse = ", "),
         "\nFile: ", path)
  }
  return(dt)
}

# ----------------------------
# 3) Model placeholders (replace with your real segmented-regression pipeline)
# ----------------------------
fit_model0 <- function(dt) {
  # Example placeholder: simple linear association
  # Replace with your segmented regression / GAM / threshold workflow.
  stats::lm(R ~ TMP, data = dt)
}

fit_model1 <- function(dt) {
  # Placeholder: add covariates if present (only include those that exist)
  cand <- c("SR", "PREC", "HUM", "PMP", "HEG", "OPE", "CPI", "UP", "PD", "AMC", "IC", "WASH", "HD")
  x <- intersect(cand, names(dt))
  f <- as.formula(paste("R ~ TMP +", paste(x, collapse = " + ")))
  stats::lm(f, data = dt)
}

fit_full <- function(dt) {
  # Placeholder: Full model as needed (e.g., + period indicators/interactions)
  # Only run if those columns exist.
  has_period <- all(c("period") %in% names(dt))
  if (!has_period) {
    # fallback
    return(fit_model1(dt))
  }
  cand <- c("SR", "PREC", "HUM", "PMP", "HEG", "OPE", "CPI", "UP", "PD", "AMC", "IC", "WASH", "HD")
  x <- intersect(cand, names(dt))
  f <- as.formula(paste("R ~ TMP * period +", paste(x, collapse = " + ")))
  stats::lm(f, data = dt)
}

fit_model2 <- function(dt) {
  # Placeholder: Model2 (your paper’s additional main model)
  # E.g., alternative outcome transform, alternative covariate set, etc.
  # Here we use logit_R if present; otherwise fall back to R.
  y <- if ("logit_R" %in% names(dt)) "logit_R" else "R"
  cand <- c("SR", "PREC", "HUM", "PMP", "HEG", "OPE", "CPI")
  x <- intersect(cand, names(dt))
  f <- as.formula(paste(y, "~ TMP +", paste(x, collapse = " + ")))
  stats::lm(f, data = dt)
}

extract_summary <- function(fit, model_name, combo_name) {
  s <- summary(fit)
  co <- as.data.table(s$coefficients, keep.rownames = "term")
  setnames(co, c("term","estimate","std_error","t_value","p_value"))
  co[, `:=`(model = model_name, combination = combo_name)]
  co[]
}

# ----------------------------
# 4) Run per derived dataset file
# ----------------------------
files <- list.files(DATA_DERIVED_DIR, pattern = "\\.csv$", full.names = TRUE)
if (length(files) == 0) stop("No CSV files found under: ", DATA_DERIVED_DIR)

all_coef <- list()

for (fp in files) {
  combo <- basename(fp)
  cat("\n--- Running:", combo, " ---\n")
  dt <- read_panel(fp)

  m0 <- fit_model0(dt)
  m1 <- fit_model1(dt)
  mf <- fit_full(dt)
  m2 <- fit_model2(dt)

  # Save model objects (RDS)
  saveRDS(m0, file.path(OUT_MODELS_DIR, paste0(combo, "_Model0.rds")))
  saveRDS(m1, file.path(OUT_MODELS_DIR, paste0(combo, "_Model1.rds")))
  saveRDS(mf, file.path(OUT_MODELS_DIR, paste0(combo, "_Full.rds")))
  saveRDS(m2, file.path(OUT_MODELS_DIR, paste0(combo, "_Model2.rds")))

  # Coeff summaries
  all_coef[[paste0(combo, "_Model0")]] <- extract_summary(m0, "Model0", combo)
  all_coef[[paste0(combo, "_Model1")]] <- extract_summary(m1, "Model1", combo)
  all_coef[[paste0(combo, "_Full")]]   <- extract_summary(mf, "Full", combo)
  all_coef[[paste0(combo, "_Model2")]] <- extract_summary(m2, "Model2", combo)
}

coef_dt <- rbindlist(all_coef, use.names = TRUE, fill = TRUE)
fwrite(coef_dt, file.path(OUT_TABLES_DIR, "coefficients_all_models.csv"))

cat("\nDone. Outputs written to:", OUT_DIR, "\n")
cat("End time:", as.character(Sys.time()), "\n")
sink()
