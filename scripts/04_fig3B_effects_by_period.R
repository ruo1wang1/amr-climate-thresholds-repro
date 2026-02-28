suppressPackageStartupMessages({
  library(tidyverse)
  library(ggsci)
})

# ---- Paths ----
in_path <- file.path("data", "figures", "combined_temperature_effects.csv")
out_dir <- file.path("output", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(in_path)) {
  stop("Input not found: ", in_path,
       "\nPlease place combined_temperature_effects.csv under data/figures/ (recommended).")
}

df <- read.csv(in_path)

# ---- Factor setup ----
df$Period <- factor(df$Period, levels = c("1999-2006", "2007-2014", "2015-2022"))

df$Temperature_Range <- factor(
  df$Temperature_Range,
  levels = c("Below_Threshold", "Above_Threshold"),
  labels = c("Below Threshold", "Above Threshold")
)

# Rename resistance types
df$Resistance_Type <- factor(
  df$Resistance_Type,
  levels = c("AAMR", "CRPA", "CRAB", "CRKP", "CREC", "3GCRKP", "3GCREC")
)

df$Resistance_Type <- forcats::fct_recode(
  df$Resistance_Type,
  "3GCR-Ec" = "3GCREC",
  "3GCR-Kp" = "3GCRKP",
  "CR-Ec"   = "CREC",
  "CR-Kp"   = "CRKP",
  "CR-Ab"   = "CRAB",
  "CR-Pa"   = "CRPA"
)

# ---- Significance marks (kept as you coded; consider generating from p-values later) ----
df$Significance <- "ns"
df$Significance[df$Resistance_Type == "AAMR"] <- "***"
df$Significance[df$Resistance_Type == "3GCR-Kp" & df$Temperature_Range == "Below Threshold" & df$Period == "1999-2006"] <- "."
df$Significance[df$Resistance_Type == "3GCR-Kp" & df$Temperature_Range == "Below Threshold" & df$Period %in% c("2007-2014", "2015-2022")] <- "***"
df$Significance[df$Resistance_Type == "3GCR-Kp" & df$Temperature_Range == "Above Threshold" & df$Period %in% c("2007-2014", "2015-2022")] <- "***"

df$Significance[df$Resistance_Type == "CR-Kp" & df$Temperature_Range == "Below Threshold" & df$Period == "1999-2006"] <- "***"
df$Significance[df$Resistance_Type == "CR-Kp" & df$Temperature_Range == "Below Threshold" & df$Period == "2015-2022"] <- "*"
df$Significance[df$Resistance_Type == "CR-Kp" & df$Temperature_Range == "Above Threshold" & df$Period %in% c("1999-2006", "2015-2022")] <- "***"

df$Significance[df$Resistance_Type == "CR-Pa" & df$Temperature_Range == "Below Threshold" & df$Period %in% c("1999-2006", "2007-2014")] <- "***"
df$Significance[df$Resistance_Type == "CR-Pa" & df$Temperature_Range == "Above Threshold" & df$Period %in% c("1999-2006", "2007-2014")] <- "***"

df$Significance[df$Resistance_Type == "CR-Ab" & df$Temperature_Range == "Below Threshold" & df$Period %in% c("1999-2006", "2007-2014")] <- "***"
df$Significance[df$Resistance_Type == "CR-Ab" & df$Temperature_Range == "Above Threshold"] <- "***"

df$Significance[df$Resistance_Type == "CR-Ec" & df$Temperature_Range == "Below Threshold" & df$Period %in% c("2007-2014", "2015-2022")] <- "***"
df$Significance[df$Resistance_Type == "CR-Ec" & df$Temperature_Range == "Above Threshold"] <- "***"

df$Significance[df$Resistance_Type == "3GCR-Ec"] <- "***"

# ---- Plot ----
plot <- ggplot(df, aes(
  x = Period, y = Temperature_Effect, color = Temperature_Range,
  group = Temperature_Range, shape = Temperature_Range
)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(ymin = Effect_Lower_Bound, ymax = Effect_Upper_Bound),
    width = 0.2, size = 0.6, position = position_dodge(width = 0.3), alpha = 0.6
  ) +
  geom_text(
    aes(label = paste0(round(Temperature_Effect, 1), " ", Significance)),
    vjust = -1.2, hjust = -0.5, size = 3, color = "black",
    position = position_dodge(width = 0.3)
  ) +
  facet_grid(Resistance_Type ~ Temperature_Range, scales = "free_y") +
  labs(x = "Time Period", y = "Temperature Effect") +
  scale_color_lancet(palette = "lanonc", alpha = 0.8) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text = element_text(color = "black", size = 10, face = "bold"),
    axis.title = element_text(color = "black", size = 12, face = "bold"),
    strip.text = element_text(color = "black", size = 11, face = "bold"),
    strip.background = element_rect(fill = "white"),
    plot.margin = margin(0.5, 1, 0.5, 0.5, unit = "cm"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))

out_png <- file.path(out_dir, "Fig3B_panel_plot_final.png")
ggsave(out_png, plot = plot, width = 12, height = 10, units = "in", dpi = 300)

message("Saved: ", out_png)
