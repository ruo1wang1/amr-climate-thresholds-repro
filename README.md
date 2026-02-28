# Scripts

This folder contains the reproducibility scripts used to fit the main models and generate the main figures.

## How to run (one-click)

From the repository root:

```bash
Rscript scripts/00_master_pipeline.R
What each script does
00_master_pipeline.R: one-click entry point (runs models, then generates figures).

02_models_main.R: fits Model 1/Model 2 segmented regression for each dataset in data/derived/.

03_fig3A_forestplot.R: generates Fig 3A → output/figures/Fig3A_forestplot.pdf/.svg.

04_fig3B_effects_by_period.R: generates Fig 3B using data/figures/combined_temperature_effects.csv → output/figures/Fig3B_panel_plot_final.png.

05_fig4_covariates_heatmap.R: generates Fig 4 → output/figures/Fig4_resistance_heatmap.png/.pdf/.svg.

Outputs
All generated outputs are written to the output/ folder (created automatically).
