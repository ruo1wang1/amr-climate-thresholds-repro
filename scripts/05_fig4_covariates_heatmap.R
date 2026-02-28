suppressPackageStartupMessages({
  library(tidyverse)
  library(ComplexHeatmap)
  library(circlize)
  library(grid)
})

# ----------------------------
# Paths
# ----------------------------
out_dir <- file.path("output", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ----------------------------
# Figure-ready data (embedded)
# NOTE: later you can replace this with a read_csv from output/tables
# ----------------------------
data <- data.frame(
  Resistance = c(
    rep("3GCR-Ec", 11),
    rep("3GCR-Kp", 8),
    rep("CR-Ab", 9),
    rep("CR-Ec", 9),
    rep("CR-Kp", 8),
    rep("CR-Pa", 9)
  ),
  Covariate = c(
    "HEG", "OPE", "SR", "SSR", "TCL", "HUM", "CPI", "WASH", "log_AMC", "IC", "HD",
    "HEG", "log_IT", "OPE", "log_PMP", "SR", "SSR", "TCL", "CPI",
    "log_IT", "UP", "log_PMP", "SR", "SSR", "HUM", "CPI", "log_AMC", "HD",
    "HEG", "OPE", "log_PD", "UP", "PREC", "SR", "WASH", "IC", "HD",
    "OPE", "UP", "SR", "HUM", "CPI", "WASH", "log_AMC", "IC",
    "OPE", "log_PD", "PREC", "SR", "TCL", "HUM", "CPI", "WASH", "HD"
  ),
  Estimate = c(
    -0.74263, 0.33775, 33.19465, -10.89550, -0.60132, -0.04009, -0.30413, 0.28523, -1.77453, -0.40315, 0.05725,
    0.34995, -2.32674, 0.15943, 4.39933, 15.24835, -9.61271, 1.87517, -1.10779,
    -1.72741, -0.28044, -12.84746, 38.02794, -43.84556, -0.09110, -1.40043, 13.23304, 0.80498,
    -0.1212842, 0.0580522, 0.4988380, -0.0494057, -0.0010290, 4.9936501, 0.0097225, -0.0630664, -0.1322348,
    0.18327, -0.06000, 14.35350, -0.05882, -0.29806, 0.24926, 15.05291, -0.27012,
    0.108499, -1.055247, 0.009517, -12.870448, 1.170482, -0.067428, -0.585894, -0.144293, 1.760873
  ),
  P = c(
    1.27e-07, 2e-16, 2e-16, 0.000811, 0.439718, 0.000409, 2e-16, 9.20e-13, 0.154756, 2.13e-08, 0.784473,
    0.066016, 1.70e-08, 0.000124, 0.000641, 0.000289, 0.018011, 0.064742, 2e-16,
    0.015165, 0.000342, 7.62e-06, 5.75e-08, 2.43e-07, 0.000609, 2e-16, 1.30e-05, 0.059683,
    0.000588, 2.59e-10, 2.63e-05, 9.70e-09, 8.20e-05, 1.72e-09, 0.408899, 0.000884, 0.021282,
    1.64e-05, 0.15723, 1.98e-05, 3.80e-06, 3.45e-15, 5.67e-06, 2e-16, 0.00275,
    0.000478, 0.011979, 3.24e-07, 2.46e-06, 0.084818, 6.47e-05, 2e-16, 0.000273, 2e-16
  )
)

# Optional: save the figure-ready input table for transparency
write.csv(data, file.path(out_dir, "Fig4_heatmap_input_table.csv"), row.names = FALSE)

# ----------------------------
# Pre-processing: wide matrices
# ----------------------------
# Desired column order (fixed for comparability across strains)
column_order <- c(
  "CPI", "HD", "HEG", "HUM", "IC", "OPE", "PREC", "SR", "SSR",
  "TCL", "UP", "WASH", "log_AMC", "log_IT", "log_PD", "log_PMP"
)

data_wide <- data %>%
  select(Resistance, Covariate, Estimate) %>%
  pivot_wider(names_from = Covariate, values_from = Estimate) %>%
  as.data.frame()

row.names(data_wide) <- data_wide$Resistance

# ensure all columns exist
for (col in column_order) {
  if (!(col %in% names(data_wide))) data_wide[[col]] <- NA_real_
}

data_matrix <- as.matrix(data_wide[, column_order])

# P-value matrix (same shape)
p_wide <- data %>%
  select(Resistance, Covariate, P) %>%
  pivot_wider(names_from = Covariate, values_from = P) %>%
  as.data.frame()

row.names(p_wide) <- p_wide$Resistance
for (col in column_order) {
  if (!(col %in% names(p_wide))) p_wide[[col]] <- NA_real_
}
p_matrix <- as.matrix(p_wide[, column_order])

# ----------------------------
# Significance stars
# ----------------------------
stars <- matrix("", nrow = nrow(p_matrix), ncol = ncol(p_matrix))
p_num <- suppressWarnings(matrix(as.numeric(p_matrix), nrow = nrow(p_matrix), ncol = ncol(p_matrix)))

stars[!is.na(p_num) & p_num < 0.001] <- "***"
stars[!is.na(p_num) & p_num >= 0.001 & p_num < 0.01] <- "**"
stars[!is.na(p_num) & p_num >= 0.01 & p_num < 0.05] <- "*"
stars[!is.na(p_num) & p_num >= 0.05 & p_num < 0.1] <- "."

# ----------------------------
# Color mapping
# ----------------------------
max_abs_value <- max(abs(data_matrix), na.rm = TRUE)
max_abs_value_rounded <- ceiling(max_abs_value)

col_fun <- colorRamp2(
  c(-max_abs_value_rounded, 0, max_abs_value_rounded),
  c("#5DADE2", "white", "#DD5F60")
)

# ----------------------------
# Row annotation
# ----------------------------
row_labels <- rownames(data_matrix)

row_ha <- rowAnnotation(
  Strain = anno_text(
    row_labels,
    gp = gpar(fontsize = 10),
    just = "right"
  ),
  width = max_text_width(row_labels) + unit(10, "mm")
)

# ----------------------------
# Heatmap
# ----------------------------
ht <- Heatmap(
  data_matrix,
  name = "Estimate",
  col = col_fun,
  rect_gp = gpar(col = "grey", lwd = 1),
  na_col = "#D3D3D3A0",
  cell_fun = function(j, i, x, y, width, height, fill) {
    if (!is.na(stars[i, j]) && stars[i, j] != "") {
      grid.text(stars[i, j], x, y, gp = gpar(fontsize = 10))
    }
  },
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  show_row_names = FALSE,
  left_annotation = row_ha,
  column_names_rot = 45,
  heatmap_legend_param = list(
    title = "Estimate",
    title_gp = gpar(fontsize = 12),
    labels_gp = gpar(fontsize = 10),
    legend_height = unit(6, "cm"),
    grid_width = unit(0.5, "cm")
  )
)

# ----------------------------
# Save outputs
# ----------------------------
png_path <- file.path(out_dir, "Fig4_resistance_heatmap.png")
pdf_path <- file.path(out_dir, "Fig4_resistance_heatmap.pdf")
svg_path <- file.path(out_dir, "Fig4_resistance_heatmap.svg")

png(png_path, width = 3000, height = 2000, res = 300)
draw(ht, padding = unit(c(2, 2, 2, 20), "mm"))
dev.off()

pdf(pdf_path, width = 12, height = 8)
draw(ht, padding = unit(c(2, 2, 2, 20), "mm"))
dev.off()

# Use svglite if available; otherwise fallback to base svg device
if (requireNamespace("svglite", quietly = TRUE)) {
  svglite::svglite(svg_path, width = 12, height = 8)
  draw(ht, padding = unit(c(2, 2, 2, 20), "mm"))
  dev.off()
} else {
  svg(svg_path, width = 12, height = 8)
  draw(ht, padding = unit(c(2, 2, 2, 20), "mm"))
  dev.off()
}

message("Saved: ", png_path)
message("Saved: ", pdf_path)
message("Saved: ", svg_path)
