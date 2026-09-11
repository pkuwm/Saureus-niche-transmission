#!/usr/bin/env Rscript

# Compare ward characteristics between transmission and non-transmission groups.
# Input: Hospital characteristics in Excel with column names and categories.
# Output: Group-comparison CSV, significance/residual heatmaps and violin plots.
# Usage: Rscript 14_compare_ward_characteristics.R INPUT.xlsx OUTPUT_DIR

setwd("/Users/liusi/01project_ec_sau/07manuscript/working_path/")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) stop("Usage: Rscript 14_compare_ward_characteristics.R INPUT.xlsx OUTPUT_DIR", call. = FALSE)
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
library(showtext)

showtext_auto()

df <- read_hospital_data(input_file) %>%
  mutate(
    `Transmission_status` = as.character(`Transmission_status`),
    `Transmission_scope` = as.character(`Transmission_scope`),
    Companion_allowed_and_number = recode(as.character(Companion_allowed_and_number), "A_Unlimited" = "Unlimited"),
    Patient_activity_area = recode(as.character(Patient_activity_area), "A_Not allowed" = "Not allowed"),
    Toilet_disinfection_type = recode(as.character(Toilet_disinfection_type), "A_None" = "None"),
    Floor_disinfection_type = recode(as.character(Floor_disinfection_type), "A_Other" = "Other")
  )

y <- "Transmission_status"
df[[y]] <- factor(df[[y]], levels = c("Non-transmission", "Transmission"))

group_levels <- levels(df[[y]])
if (length(group_levels) != 2) {
  stop("The outcome must have exactly two levels")
}
group1 <- group_levels[1]
group2 <- group_levels[2]

categorical_vars <- c(
  "Department_type",
  "Medical_area_ventilation_type",
  "Medical_area_air_disinfection_type",
  "Medical_area_computer_disinfection_type",
  "Nursing_cart_surface_disinfection_type",
  "Public_toilet_handwash_type",
  "Bed_assignment_policy",
  "Bed_fixed_or_not",
  "Hand_disinfection_in_room",
  "Visiting_hours",

  "Companion_allowed_and_number",
  "Patient_activity_area",
  "Bed_surface_disinfection_type",
  "Bedrail_cabinet_disinfection_type",
  "Fixed_equipment_disinfection_type",
  "Floor_disinfection_type",
  "Mobile_equipment_disinfection_type",
  "Toilet_disinfection_type",
  "Furniture_disinfection_type",
  "Door_switch_disinfection_type",
  "Ventilation_type",
  "Air_disinfection_type",
  "Bed_curtain_present"
)

continuous_vars <- c(
  "Independent_room_count","Double_room_count","Triple_room_count","Multi_room_count",
  "Max_capacity_per_room","Single_room_area","Room_area_per_person","Calculated_bed_count",
  "Bed_distance_m","Avg_annual_inpatients","Total_staff","Doctors_count","Nurses_count",
  "Interns_graduates_count","Other_staff_count","Avg_length_of_stay","Department_area",
  "Doctor_uniform_change_per_day","Nurse_uniform_change_per_day","Bed_sheet_change_freq_per_day",
  "single_room_count",

  "Visitor_limit_num"

)

ward_layout <- c(
  "Department_type",
  "Independent_room_count","Double_room_count","Triple_room_count","Multi_room_count",
  "Max_capacity_per_room","Calculated_bed_count",
  "Department_area","Single_room_area","Room_area_per_person",
  "single_room_count"
)

staff_patient_numbers <- c(
  "Total_staff","Doctors_count","Nurses_count",
  "Interns_graduates_count","Other_staff_count","Avg_annual_inpatients",
  "Doctors_Nurses","Other_staff_graduates_count"
)

ward_management <- c(
  "Doctor_uniform_change_per_day","Nurse_uniform_change_per_day",
  "Bed_sheet_change_freq_per_day","Bed_assignment_policy",
  "Bed_fixed_or_not","Avg_length_of_stay",
  "Visiting_hours","Visitor_limit","Companion_allowed_and_number",
  "Patient_activity_area","Public_toilet_handwash_type",
  "Hand_disinfection_in_room","Bed_distance_m",
  "Visitor_limit_num"
)

ventilation_disinfection <- c(

  "Medical_area_ventilation_type",
  "Medical_area_air_disinfection_type",
  "Medical_area_computer_disinfection_type",
  "Nursing_cart_surface_disinfection_type",
  "Fixed_equipment_disinfection_type",
  "Bed_surface_disinfection_type",
  "Bedrail_cabinet_disinfection_type",
  "Floor_disinfection_type",
  "Mobile_equipment_disinfection_type",
  "Toilet_disinfection_type",
  "Furniture_disinfection_type",
  "Door_switch_disinfection_type",
  "Ventilation_type",
  "Air_disinfection_type",
  "Bed_curtain_present"

)

categorical_vars <- categorical_vars[categorical_vars %in% names(df)]
continuous_vars  <- continuous_vars[continuous_vars %in% names(df)]

df[categorical_vars] <- lapply(df[categorical_vars], function(x) as.factor(as.character(x)))
df[continuous_vars]  <- lapply(df[continuous_vars],  function(x) as.numeric(as.character(x)))

results <- data.frame(
  Variable = character(),
  Variable_type = character(),
  Test = character(),
  P_value = numeric(),
  stringsAsFactors = FALSE
)
results[[paste0(group1, "_summary")]] <- character()
results[[paste0(group2, "_summary")]] <- character()
results[["Overall_summary"]] <- character()

for (var in categorical_vars) {
  if (!var %in% names(df)) next

  data_subset <- df[!is.na(df[[var]]) & !is.na(df[[y]]), ]

  tbl <- table(data_subset[[y]], data_subset[[var]])

  if (ncol(tbl) < 2) next

  group1_stats <- character()
  group2_stats <- character()
  overall_stats <- character()

  for (cat in colnames(tbl)) {
    n1 <- tbl[group1, cat]
    n2 <- tbl[group2, cat]
    total1 <- sum(tbl[group1, ])
    total2 <- sum(tbl[group2, ])
    total_overall <- sum(tbl)

    pct1 <- round(n1/total1 * 100, 1)
    pct2 <- round(n2/total2 * 100, 1)
    pct_overall <- round((n1 + n2)/total_overall * 100, 1)

    group1_stats <- c(group1_stats, paste0(cat, ": ", n1, " (", pct1, "%)"))
    group2_stats <- c(group2_stats, paste0(cat, ": ", n2, " (", pct2, "%)"))
    overall_stats <- c(overall_stats, paste0(cat, ": ", n1 + n2, " (", pct_overall, "%)"))
  }

  stats1 <- paste(group1_stats, collapse = "; ")
  stats2 <- paste(group2_stats, collapse = "; ")
  stats_overall <- paste(overall_stats, collapse = "; ")

  if (any(chisq.test(tbl)$expected < 5)) {
    p <- fisher.test(tbl)$p.value
    method <- "Fisher exact test"
  } else {
    p <- chisq.test(tbl)$p.value
    method <- "Chi-square test"
  }

  new_row <- list(
    Variable = var,
    Variable_type = "Categorical",
    Test = method,
    P_value = p
  )

  new_row[[paste0(group1, "_summary")]] <- stats1
  new_row[[paste0(group2, "_summary")]] <- stats2
  new_row[["Overall_summary"]] <- stats_overall

  new_row_df <- as.data.frame(new_row, stringsAsFactors = FALSE, check.names = FALSE)
  results <- rbind(results, new_row_df)
}

for (var in continuous_vars) {
  if (!var %in% names(df)) next

  d <- df[!is.na(df[[var]]) & !is.na(df[[y]]), c(y, var)]

  if (nrow(d) == 0) next
  if (length(unique(d[[y]])) != 2) next

  g1 <- d[d[[y]] == group1, var, drop = TRUE]
  g2 <- d[d[[y]] == group2, var, drop = TRUE]
  g_all <- c(g1, g2)

  median_iqr <- function(x) {
    if (length(x) == 0 || all(is.na(x))) return("NA")
    median_val <- round(median(x, na.rm = TRUE), 2)
    q1 <- round(quantile(x, 0.25, na.rm = TRUE), 2)
    q3 <- round(quantile(x, 0.75, na.rm = TRUE), 2)
    return(paste0(median_val, " (", q1, ", ", q3, ")"))
  }

  stats1 <- median_iqr(g1)
  stats2 <- median_iqr(g2)
  stats_overall <- median_iqr(g_all)

  if (length(g1) >= 3 && length(g2) >= 3) {
    p1 <- suppressWarnings(shapiro.test(g1)$p.value)
    p2 <- suppressWarnings(shapiro.test(g2)$p.value)
  } else {
    p1 <- p2 <- 0
  }

  if (p1 > 0.05 && p2 > 0.05) {
    p <- t.test(g1, g2)$p.value
    method <- "t test"
  } else {
    p <- wilcox.test(g1, g2)$p.value
    method <- "Mann-Whitney U test"
  }

  new_row <- list(
    Variable = var,
    Variable_type = "Continuous",
    Test = method,
    P_value = p
  )

  new_row[[paste0(group1, "_summary")]] <- stats1
  new_row[[paste0(group2, "_summary")]] <- stats2
  new_row[["Overall_summary"]] <- stats_overall

  new_row_df <- as.data.frame(new_row, stringsAsFactors = FALSE, check.names = FALSE)
  results <- rbind(results, new_row_df)
}

results <- results %>% arrange(P_value)

results$Significance <- ifelse(results$P_value < 0.001, "***",
                      ifelse(results$P_value < 0.01, "**",
                             ifelse(results$P_value < 0.05, "*", "")))

results_numeric <- results

format_p_value <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) {
    return(formatC(p, format = "e", digits = 2))
  } else {
    return(round(p, 4))
  }
}

results_export <- results_numeric
results_export$P_value <- sapply(results_export$P_value, format_p_value)

col_order <- c("Variable", "Variable_type", "Test", "P_value", "Significance",
               paste0(group1, "_summary"), paste0(group2, "_summary"), "Overall_summary")
results_export <- results_export[, col_order]

write.csv(results_export, file.path(output_dir, "ward_group_comparisons.csv"), row.names = FALSE)

library(ggplot2)
library(dplyr)
library(reshape2)

results_plot <- results_numeric %>%
  mutate(
    Significance = case_when(
      P_value < 0.001 ~ "***",
      P_value < 0.01  ~ "**",
      P_value < 0.05  ~ "*",
      P_value < 0.1   ~ ".",
      TRUE ~ ""
    ),
    neglogP = -log10(P_value),
    Variable = factor(Variable, levels = results_numeric$Variable[order(results_numeric$P_value)])
  )

results_plot$Group <- case_when(
  results_plot$Variable %in% ward_layout ~ "Ward Layout",
  results_plot$Variable %in% staff_patient_numbers ~ "Staff & Patients",
  results_plot$Variable %in% ward_management ~ "Ward Management",
  results_plot$Variable %in% ventilation_disinfection ~ "Ventilation & Disinfection",
  TRUE ~ "Other"
)

global_max <- max(results_plot$neglogP, na.rm = TRUE)

plot_heatmap <- function(data, group_name, fill_limits) {
  data %>%
    filter(Group == group_name) %>%
    arrange(P_value) %>%
    mutate(Variable = factor(Variable, levels = Variable[order(P_value)])) %>%
    ggplot(aes(x = group_name, y = Variable, fill = neglogP)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Significance), color = "black", size = 4) +
    scale_fill_gradient(
      low = "#FDDBC7", high = "#B2182B",
      name = "-log10(P)",
      limits = c(0, fill_limits)
    ) +
    theme_minimal(base_size = 13) +
    labs(
      title = paste0(group_name, " - Univariate Analysis Heatmap"),
      subtitle = "Darker color indicates smaller P value",
      x = NULL, y = NULL
    ) +
    theme(
      axis.text.y = element_text(size = 9),
      axis.text.x = element_blank(),
      panel.grid = element_blank()
    )
}

p1 <- plot_heatmap(results_plot, "Ward Layout", global_max)
p2 <- plot_heatmap(results_plot, "Staff & Patients", global_max)
p3 <- plot_heatmap(results_plot, "Ward Management", global_max)
p4 <- plot_heatmap(results_plot, "Ventilation & Disinfection", global_max)

p1
p2
p3
p4

ggsave(file.path(output_dir, "heatmap_Ward_Layout.pdf"), p1, width = 4, height = 6)
ggsave(file.path(output_dir, "heatmap_Staff_Patients.pdf"), p2, width = 4, height = 6)
ggsave(file.path(output_dir, "heatmap_Ward_Management.pdf"), p3, width = 4, height = 6)
ggsave(file.path(output_dir, "heatmap_Ventilation_Disinfection.pdf"), p4, width = 6, height = 8)

ggplot(results_plot, aes(x = "Group comparison", y = Variable, fill = neglogP)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Significance), color = "black", size = 4) +
  scale_fill_gradient(low = "#FDDBC7", high = "#B2182B", name = "-log10(P)") +
  theme_minimal(base_size = 13) +
  labs(title = "Unadjusted group-comparison significance", x = NULL, y = NULL,
       subtitle = "Darker colors indicate smaller P values") +
  theme(axis.text.y = element_text(size = 9),
        panel.grid = element_blank(),
        axis.text.x = element_blank())

sig_cat_vars <- results_numeric %>%
  filter(P_value < 0.05,
         Test %in% c("Chi-square test","Fisher exact test"),
         Variable %in% categorical_vars) %>%
  pull(Variable) %>%
  unique()

sig_cat_vars <- sig_cat_vars[sig_cat_vars %in% colnames(df)]
print(sig_cat_vars)

all_resid <- list()

for (var in sig_cat_vars) {
  tbl <- table(df[[y]], df[[var]])
  if (any(dim(tbl) < 2)) next

  chisq_res <- suppressWarnings(chisq.test(tbl))
  resid_mat <- chisq_res$stdres

  resid_df <- melt(resid_mat)
  colnames(resid_df) <- c("Transmission_group", "Category", "Residual")
  resid_df$Variable <- var
  resid_df$P_value <- chisq_res$p.value

  all_resid[[var]] <- resid_df
}

all_resid_df <- bind_rows(all_resid)

if (nrow(all_resid_df) == 0) {
  stop("No significant categorical variables are available for residual plots.")
}

max_abs <- max(abs(all_resid_df$Residual), na.rm = TRUE)

var_order <- all_resid_df %>%
  group_by(Variable) %>%
  summarise(P_min = min(P_value, na.rm = TRUE)) %>%
  arrange(P_min) %>%
  pull(Variable)

all_resid_df$Variable <- factor(all_resid_df$Variable, levels = var_order)

p_all <- ggplot(all_resid_df,
                aes(x = Transmission_group, y = Category, fill = Residual)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Residual, 2)), size = 2.8, color = "black") +
  scale_fill_gradient2(
    low = "#3f72af", mid = "white", high = "#e23e57",
    limits = c(-max_abs, max_abs),
    midpoint = 0,
    name = "Std residual"
  ) +
  facet_wrap(~ Variable, scales = "free_y") +
  theme_minimal(base_size = 11) +
  labs(
    title = "Standardized chi-square residuals for significant variables",
    subtitle = "Red: observed above expected; blue: observed below expected; shared color scale",
    x = "Transmission_status", y = NULL
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 6),
    axis.text.x = element_text(size = 6, angle = 0, vjust = 1),
    axis.text.y = element_text(size = 6),
    panel.grid = element_blank()
  )

print(p_all)

ggsave(file.path(output_dir, "Residual_All_Significant_Variables3.pdf"), plot = p_all,
       width = 10, height = 10)

library(dplyr)
library(tidyr)

sig_cont_vars <- results %>%
  filter(P_value < 0.05,
         Test %in% c("t test","Mann-Whitney U test"),
         Variable %in% continuous_vars) %>%
  pull(Variable) %>%
  unique()

sig_cont_vars <- sig_cont_vars[sig_cont_vars %in% colnames(df)]
if (length(sig_cont_vars) == 0) stop("No significant continuous variables are available for violin plots.")

cont_long <- df %>%
  dplyr::select(all_of(c(y, sig_cont_vars))) %>%
  pivot_longer(cols = all_of(sig_cont_vars),
               names_to = "Variable", values_to = "Value") %>%
  filter(!is.na(.data[[y]]), !is.na(Value))

p_map <- results %>%
  filter(Variable %in% sig_cont_vars,
         Test %in% c("t test","Mann-Whitney U test")) %>%
  group_by(Variable) %>%
  summarise(P_min = min(P_value, na.rm = TRUE),
            Method = dplyr::first(Test), .groups="drop") %>%
  mutate(facet_lab = paste0(Variable, "\n", Method, " p=", formatC(P_min, format="f", digits=3)))

lab_set <- setNames(p_map$facet_lab, p_map$Variable)

p_violin <- ggplot(
  cont_long,
  aes(x = .data[[y]], y = Value, fill = .data[[y]])
) +

  geom_violin(trim = FALSE, width = 0.9, alpha = 0.6, color = "gray40") +

  geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.8, color = "black") +

  scale_fill_manual(
    name = y,
    values = c("#3f72af", "#e23e57")
  ) +

  facet_wrap(
    ~ Variable,
    scales = "free_y",
    labeller = as_labeller(lab_set)
  ) +

  theme_minimal(base_size = 11) +
  labs(
    title = "Significant continuous variables (original units)",
    x = y, y = NULL
  ) +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 9, face = "bold"),
    panel.grid = element_blank(),

    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  )

print(p_violin)

ggsave(file.path(output_dir, "Violin_Significant_Continuous_Variables_raw.pdf"),
       plot = p_violin, width = 10, height = 6)
