suppressPackageStartupMessages({
  library(dplyr)
  library(grid)
  library(svglite)
  library(forestploter)
})

# Output directory (portable)
out_dir <- file.path("output", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Input: figure-ready summary table (currently embedded) ----
data <- data.frame(
  AMR_Strain = rep(c('3GCR-Ec', '3GCR-Kp', 'CR-Ec', 'CR-Kp', 'CR-Ab', 'CR-Pa'), each = 5),
  Model_Term = rep(c('', 'TMP', 'U1.TMP', 'TMP:TimePeriod1', 'TMP:TimePeriod2'), 6),
  Coefficient = c(
    NA, -0.57088, 1.02903, -0.56625, -0.28182,
    NA, -1.18225, 1.09907, -1.07303, -0.65670,
    NA, 0.0074017, 0.1917451, -0.0861660, -0.1203117,
    NA, -1.41140, 1.65486, -0.25327, -0.33770,
    NA, -1.24888, 1.58682, -1.60907, -0.14065,
    NA, -1.747073, 1.504652, -0.577690, 0.084624
  ),
  CI_Lower = c(
    NA, -0.8631, 0.5648, -0.8454, -0.5147,
    NA, -2.5313, 0.4852, -1.7272, -1.0762,
    NA, -0.0593, 0.0524, -0.1713, -0.1826,
    NA, -2.2532, 0.8256, -0.9138, -0.9410,
    NA, -1.8501, 0.8629, -2.4366, -0.5574,
    NA, -2.3316, 0.6305, -0.9334, -0.1281
  ),
  CI_Upper = c(
    NA, -0.2786, 1.4933, -0.2871, -0.0489,
    NA, -0.0992, 1.7129, -0.4189, -0.2372,
    NA, 0.0741, 0.3311, -0.0009, -0.0578,
    NA, -0.5696, 2.4841, 0.4072, 0.2656,
    NA, -0.6476, 2.3107, -0.7815, 0.2761,
    NA, -1.1625, 2.3788, -0.2220, 0.2973
  ),
  P_value = c(
    NA, 0.000169, 0.004250, 7.90e-05, 0.018005,
    NA, 0.002968, 0.001110, 0.001298, 0.002905,
    NA, 0.827412, 0.00702, 0.045045, 0.000163,
    NA, 0.001290, 0.000155, 0.332453, 0.273731,
    NA, 0.000194, 0.000129, 3.29e-05, 0.519287,
    NA, 0.000313, 0.002120, 0.001539, 0.436099
  )
)

data <- data %>%
  arrange(AMR_Strain, Model_Term) %>%
  group_by(AMR_Strain) %>%
  mutate(
    Model_Term = factor(Model_Term, levels = c('', 'TMP', 'U1.TMP', 'TMP:TimePeriod1', 'TMP:TimePeriod2')),
    row_id = row_number(),
    AMR_Strain_new = ifelse(row_id == 1, as.character(AMR_Strain), ""),
    Model_Term_new = ifelse(Model_Term == '', '', paste0("   ", Model_Term)),
    CI = ifelse(is.na(Coefficient), "", sprintf("%.2f (%.2f, %.2f)", Coefficient, CI_Lower, CI_Upper)),
    space = paste(rep(" ", 20), collapse = " "),
    P_value_new = case_when(
      is.na(P_value) ~ "",
      P_value < 0.001 ~ "<0.001 ***",
      P_value < 0.01 ~ sprintf("%.3f **", P_value),
      P_value < 0.05 ~ sprintf("%.3f *", P_value),
      TRUE ~ sprintf("%.3f", P_value)
    )
  ) %>%
  ungroup()

data_plot <- data[, c("AMR_Strain_new", "Model_Term_new", "CI", "space", "P_value_new")]
colnames(data_plot) <- c("AMR Strain", "Model Term", "Coefficient (95% CI)", " ", "P-value")

tm <- forest_theme(
  base_size = 8,
  refline_gp = gpar(col = "red", lty = 2, lwd = 1),
  arrow_type = "closed",
  footnote_gp = gpar(col = "black", cex = 0.6),
  xlab_gp = gpar(cex = 0.8),
  ci_gp = gpar(cex = 0.8),
  vertex_size = 1.5
)

p <- forest(
  data_plot,
  est = data$Coefficient,
  lower = data$CI_Lower,
  upper = data$CI_Upper,
  sizes = 0.3,
  ci_column = 4,
  ref_line = 0,
  arrow_lab = c("Negative Effect", "Positive Effect"),
  xlim = c(-3, 3),
  ticks_at = c(-3, -2, -1, 0, 1, 2, 3),
  footnote = "*** p<0.001, ** p<0.01, * p<0.05",
  theme = tm
)

g <- edit_plot(p, row = which(!is.na(data$Coefficient)), gp = gpar(fontface = "plain", cex = 0.8))
g <- edit_plot(g, row = which(is.na(data$Coefficient)), gp = gpar(fontface = "bold", cex = 0.8))
g <- add_border(g, part = "body", row = 1, where = "top")
g <- add_border(g, part = "body", row = nrow(data), where = "bottom")

# Save
svg_path <- file.path(out_dir, "Fig3A_forestplot.svg")
pdf_path <- file.path(out_dir, "Fig3A_forestplot.pdf")

svglite(svg_path, width = 180/25.4, height = 200/25.4)
grid.newpage(); grid.draw(g); dev.off()

pdf(pdf_path, width = 180/25.4, height = 200/25.4)
grid.newpage(); grid.draw(g); dev.off()

message("Saved: ", svg_path)
message("Saved: ", pdf_path)
