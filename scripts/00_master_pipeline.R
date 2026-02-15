# scripts/00_master_pipeline.R
# One-click pipeline entry point

message("Running master pipeline...")

# 1) (Optional) Data processing stage
# If you later add scripts/01_data_processing.R, uncomment:
# source(file.path("scripts", "01_data_processing.R"))

# 2) Main models (Model0/1/Full/Model2)
source(file.path("scripts", "02_models_main.R"))

# 3) (Optional) Figures/tables stage
# If you later add scripts/03_figures_tables.R, uncomment:
# source(file.path("scripts", "03_figures_tables.R"))

message("Master pipeline finished.")
