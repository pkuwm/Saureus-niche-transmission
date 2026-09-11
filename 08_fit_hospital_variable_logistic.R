#!/usr/bin/env Rscript

# Fit a six-predictor logistic model for transmission and plot its in-sample performance.
# Input: Hospital characteristics in Excel with column names and categories.
# Output: Adjusted ORs and model summary (CSV), ROC, calibration and OR plots (PDF).
# Usage: Rscript 08_fit_hospital_variable_logistic.R INPUT.xlsx OUTPUT_DIR

setwd("/Users/liusi/01project_ec_sau/07manuscript/working_path/")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) stop("Usage: Rscript 08_fit_hospital_variable_logistic.R INPUT.xlsx OUTPUT_DIR", call. = FALSE)
input_file <- normalizePath(args[1], mustWork = TRUE)
output_dir <- args[2]
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (!dir.exists(output_dir)) stop("Could not create output directory.")

# Read English input and set reference levels for unmerged binary predictors.
read_hospital_data <- function(path) {
  d <- readxl::read_excel(path)
  # Preserve the reference levels of unmerged binary predictors.
  for (column in intersect(c("Visiting_hours", "Bed_fixed_or_not"), names(d))) {
    lev <- if (column == "Visiting_hours") c("Open", "Time-limited") else c("No", "Yes")
    d[[column]] <- factor(d[[column]], levels = c(lev, setdiff(unique(d[[column]]), c(lev, NA))))
  }
  d
}

library(readxl)
library(dplyr)
library(pROC)
library(ggplot2)
library(grid)
library(showtext)

showtext_auto()

data <- read_hospital_data(input_file) %>%
  rename(group = Transmission_status, group2 = Transmission_scope)

data$group <- recode(data$group,
                     "Transmission" = 1,
                     "Non-transmission" = 0)
data$group <- factor(data$group)

data$group2 <- recode(data$group2,
                      "Between-hospital" = 2,
                      "Within-hospital" = 1,
                      "Non-transmission" = 0)
data$group2 <- factor(data$group2, levels = c(0, 1, 2), ordered = TRUE)

data <- data %>%
  mutate(
    Toilet_disinfection_type = case_when(
      Toilet_disinfection_type %in% c("A_None", "None") ~ "None",
      TRUE ~ as.character(Toilet_disinfection_type)
    ),
    Public_toilet_handwash_type_merged = case_when(
      Public_toilet_handwash_type == "Sensor-operated" ~ "Sensor-operated",
      Public_toilet_handwash_type %in% c("Manual", "Foot-operated") ~ "Non-sensor-operated",
      TRUE ~ as.character(Public_toilet_handwash_type)
    ),
    Companion_allowed_and_number = case_when(
      Companion_allowed_and_number %in% c("A_Unlimited") ~ "Unlimited",
      TRUE ~ as.character(Companion_allowed_and_number)
    ),
    Patient_activity_area = as.character(Patient_activity_area)
  ) %>%
  mutate(
    Toilet_disinfection_type = factor(
      Toilet_disinfection_type,
      levels = c("None", "Chlorine-based disinfectant", "Oxidizing disinfectant", "Alcohol disinfectant", "Other")
    ),
    Patient_activity_area = factor(Patient_activity_area,
      levels = c("A_Not allowed", "Within hospital", "Within room", "Within department", "Unrestricted")),
    Public_toilet_handwash_type_merged = factor(
      Public_toilet_handwash_type_merged,
      levels = c("Sensor-operated", "Non-sensor-operated")
    ),
    Companion_allowed_and_number = factor(
      Companion_allowed_and_number,
      levels = c("Unlimited", "Prohibited", "One person", "Two people")
    )
  )

candidate_vars <- c(
  "Toilet_disinfection_type",
  "Patient_activity_area",
  "Department_area",
  "Public_toilet_handwash_type_merged",
  "Max_capacity_per_room",
  "Companion_allowed_and_number"
)

df <- data %>%
  dplyr::select(group, all_of(candidate_vars)) %>%
  na.omit()

final_formula <- reformulate(candidate_vars, response = "group")
final_model <- glm(final_formula, data = df, family = binomial)

df$pred_prob <- predict(final_model, type = "response")
roc_obj <- roc(df$group, df$pred_prob, quiet = TRUE)
auc_value <- as.numeric(auc(roc_obj))

est <- summary(final_model)$coefficients
ci <- suppressWarnings(confint.default(final_model))
common_terms <- intersect(rownames(est), rownames(ci))
common_terms <- common_terms[common_terms != "(Intercept)"]

res_tab <- data.frame(
  Variable = common_terms,
  Estimate = est[common_terms, "Estimate"],
  Std.Error = est[common_terms, "Std. Error"],
  Z.value = est[common_terms, "z value"],
  p.value = est[common_terms, "Pr(>|z|)"],
  OR = exp(est[common_terms, "Estimate"]),
  Lower95 = exp(ci[common_terms, 1]),
  Upper95 = exp(ci[common_terms, 2]),
  stringsAsFactors = FALSE
)

res_tab$Significance <- cut(
  res_tab$p.value,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("***", "**", "*", "")
)

write.csv(res_tab, file.path(output_dir, "hospital_six_variable_logistic_results.csv"), row.names = FALSE)

model_summary <- data.frame(
  metric = c("n", "AIC", "AUC"),
  value = c(nrow(df), AIC(final_model), auc_value)
)
write.csv(model_summary, file.path(output_dir, "hospital_six_variable_model_summary.csv"), row.names = FALSE)

cat("Sample size n =", nrow(df), "\n")
cat("AIC =", round(AIC(final_model), 3), "\n")
cat("AUC =", round(auc_value, 3), "\n\n")
print(res_tab)

roc_df <- data.frame(
  fpr = 1 - roc_obj$specificities,
  tpr = roc_obj$sensitivities
)

roc_plot <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(color = "#1f77b4", linewidth = 1.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  theme_minimal(base_size = 14) +
  labs(
    title = paste0("ROC Curve (AUC = ", round(auc_value, 3), ")"),
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  coord_equal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

ggsave(file.path(output_dir, "hospital_six_variable_ROC.pdf"), roc_plot,
       width = 5, height = 5, units = "in", dpi = 300)

cal_df <- df %>%
  mutate(
    group_num = as.numeric(as.character(group)),
    bin = dplyr::ntile(pred_prob, 10)
  ) %>%
  group_by(bin) %>%
  summarise(
    Predicted = mean(pred_prob, na.rm = TRUE),
    Observed = mean(group_num, na.rm = TRUE),
    .groups = "drop"
  )

cal_plot <- ggplot(cal_df, aes(x = Predicted, y = Observed)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.9,
              color = "#2ca02c", linewidth = 1.1) +
  geom_point(color = "#2ca02c", size = 1, alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Calibration Curve",
    x = "Predicted Probability",
    y = "Observed Probability"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

ggsave(file.path(output_dir, "hospital_six_variable_calibration.pdf"), cal_plot,
       width = 5, height = 5, units = "in", dpi = 300)

plot_df <- res_tab %>%
  mutate(
    log2OR = log2(OR),
    log2Lower = log2(pmax(Lower95, 1e-4)),
    log2Upper = log2(pmax(Upper95, 1e-4)),
    abs_log2OR = abs(log2OR)
  )

xlim_cutoff <- 15

plot_df <- plot_df %>%
  mutate(
    log2Lower_capped = pmax(log2Lower, -xlim_cutoff),
    log2Upper_capped = pmin(log2Upper, xlim_cutoff),
    lower_out = log2Lower < -xlim_cutoff,
    upper_out = log2Upper > xlim_cutoff
  ) %>%
  arrange(log2OR) %>%
  mutate(Variable = factor(Variable, levels = Variable))

arrow_len <- xlim_cutoff * 0.25

p_bubble_forest <- ggplot(plot_df, aes(x = log2OR, y = Variable)) +
  geom_errorbarh(aes(xmin = log2Lower_capped, xmax = log2Upper_capped),
                 color = "gray55", height = 0.25, linewidth = 0.9) +
  geom_segment(
    data = subset(plot_df, lower_out),
    aes(x = -xlim_cutoff, xend = -xlim_cutoff + arrow_len, yend = Variable),
    arrow = arrow(length = unit(0.15, "cm"), ends = "first", type = "closed"),
    color = "gray50", linewidth = 0.7
  ) +
  geom_segment(
    data = subset(plot_df, upper_out),
    aes(x = xlim_cutoff, xend = xlim_cutoff - arrow_len, yend = Variable),
    arrow = arrow(length = unit(0.15, "cm"), ends = "last", type = "closed"),
    color = "gray50", linewidth = 0.7
  ) +
  geom_point(aes(size = abs_log2OR, color = p.value), alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  scale_color_gradientn(
    colors = rev(c("#FEE5D9", "#FCAE91", "#FB6A4A", "#CB181D")),
    trans = "log10",
    name = expression(italic(p) ~ value)
  ) +
  scale_size_continuous(
    range = c(3, 10),
    name = expression("|log"[2] * "OR|")
  ) +
  coord_cartesian(xlim = c(-xlim_cutoff - 0.1, xlim_cutoff + 0.1)) +
  labs(
    x = expression(log[2] * "(Odds Ratio)"),
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "right"
  )

ggsave(file.path(output_dir, "hospital_six_variable_OR_plot.pdf"), p_bubble_forest,
       width = 11, height = 4, units = "in", dpi = 300)
