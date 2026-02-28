
message("Running master pipeline...")

# 1) Main models (Model0/Model1/Model2 segmented regression)
source(file.path("scripts", "02_models_main.R"))

# 2) Main figures (Fig3–Fig4)
source(file.path("scripts", "03_fig3A_forestplot.R"))
source(file.path("scripts", "04_fig3B_effects_by_period.R"))
source(file.path("scripts", "05_fig4_covariates_heatmap.R"))

message("Master pipeline finished.")

