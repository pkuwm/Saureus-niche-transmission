#### Figure S10 ####

rm(list = ls())
setwd("/Users/liusi/03_RworkingSpace/02_ec_sau_hospital/")

library(readxl)
library(dplyr)
library(showtext)
showtext_auto()

## 1. Load data
df <- read_excel("transmission_hospital_management.xlsx")

y <- "group"
df[[y]] <- as.factor(df[[y]])

group_levels <- levels(df[[y]])
if (length(group_levels) != 2) {
  stop("The dependent variable must and can only have two levels")
}
group1 <- group_levels[1]
group2 <- group_levels[2]


## 2. Custom variables

# categorical variables
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
  "Visitor_limit",
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

# continuous variables
continuous_vars <- c(
  "Independent_room_count","Double_room_count","Triple_room_count","Multi_room_count",
  "Max_capacity_per_room","Single_room_area","Room_area_per_person","Calculated_bed_count",
  "Bed_distance_m","Avg_annual_inpatients","Total_staff","Doctors_count","Nurses_count",
  "Interns_graduates_count","Other_staff_count","Avg_length_of_stay","Department_area",
  "Doctor_uniform_change_per_day","Nurse_uniform_change_per_day","Bed_sheet_change_freq_per_day",
  "single_room_count", 
  "Doctors_Nurses","Other_staff_graduates_count",
  "Visitor_limit_num"
)

## 3. Custom groups

# Ward Layout
ward_layout <- c(
  "Department_type",
  "Independent_room_count","Double_room_count","Triple_room_count","Multi_room_count",
  "Max_capacity_per_room","Calculated_bed_count",
  "Department_area","Single_room_area","Room_area_per_person",
  "single_room_count"
)

# Staff & Patients
staff_patient_numbers <- c(
  "Total_staff","Doctors_count","Nurses_count",
  "Interns_graduates_count","Other_staff_count","Avg_annual_inpatients",
  "Doctors_Nurses","Other_staff_graduates_count"
)

# Ward Management
ward_management <- c(
  "Doctor_uniform_change_per_day","Nurse_uniform_change_per_day",
  "Bed_sheet_change_freq_per_day","Bed_assignment_policy",
  "Bed_fixed_or_not","Avg_length_of_stay",
  "Visiting_hours","Visitor_limit","Companion_allowed_and_number",
  "Patient_activity_area","Public_toilet_handwash_type",
  "Hand_disinfection_in_room","Bed_distance_m",
  "Visitor_limit_num"
)

# Ventilation & Disinfection
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

## 4. Write results

results <- data.frame(
  variable = character(),
  variable_type = character(),
  test_method = character(),
  P = numeric(),
  stringsAsFactors = FALSE
)
results[[paste0(group1, "_statistics")]] <- character()
results[[paste0(group2, "_statistics")]] <- character()
results[["overall_statistics"]] <- character()

## 5. Statistical test

# 5.1 Categorical variable: Chi-square or Fisher test
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
  
  # Determine Fisher test
  if (any(chisq.test(tbl)$expected < 5)) {
    p <- fisher.test(tbl)$p.value
    method <- "Fisher test"
  } else {
    p <- chisq.test(tbl)$p.value
    method <- "Chi-square"
  }
  
  new_row <- list(
    variable = var,
    variable_type = "Categorical variable",
    test_method = method,
    P = p
  )

  new_row[[paste0(group1, "_statistics")]] <- stats1
  new_row[[paste0(group2, "_statistics")]] <- stats2
  new_row[["overall_statistics"]] <- stats_overall
  
  new_row_df <- as.data.frame(new_row, stringsAsFactors = FALSE)
  results <- rbind(results, new_row_df)
}


# 5.2 Continuous variable: t-test or Mann-Whitney U test
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
  
  # Normality test
  if (length(g1) >= 3 && length(g2) >= 3) {
    p1 <- suppressWarnings(shapiro.test(g1)$p.value)
    p2 <- suppressWarnings(shapiro.test(g2)$p.value)
  } else {
    p1 <- p2 <- 0
  }
  
  if (p1 > 0.05 && p2 > 0.05) {
    p <- t.test(g1, g2)$p.value
    method <- "t-test"
  } else {
    p <- wilcox.test(g1, g2)$p.value
    method <- "Mann-Whitney U test"
  }
  
  new_row <- list(
    variable = var,
    variable_type = "Continuous variable",
    test_method = method,
    P = p
  )

  new_row[[paste0(group1, "_statistics")]] <- stats1
  new_row[[paste0(group2, "_statistics")]] <- stats2
  new_row[["overall_statistics"]] <- stats_overall
  
  new_row_df <- as.data.frame(new_row, stringsAsFactors = FALSE)
  results <- rbind(results, new_row_df)
}


## 6. Output results
results <- results %>% arrange(P)

results$significant <- ifelse(results$P < 0.001, "***",
                      ifelse(results$P < 0.01, "**",
                             ifelse(results$P < 0.05, "*", "")))

format_p_value <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) {
    return(formatC(p, format = "e", digits = 2))
  } else {
    return(round(p, 4))
  }
}

results$P <- sapply(results$P, format_p_value)

col_order <- c("variable", "variable_type", "test_method", "P", "significant", 
               paste0(group1, "_statistics"), paste0(group2, "_statistics"), "overall_statistics")
results <- results[, col_order]
write.csv(results, "univariate_analysis.csv", row.names = FALSE)


## 7. Standardized residuals heatmap

sig_cat_vars <- results %>%
  filter(P < 0.05,
         test_method %in% c("Chi-square","Fisher test"),
         variable %in% categorical_vars) %>%
  pull(variable) %>%
  unique()

sig_cat_vars <- sig_cat_vars[sig_cat_vars %in% colnames(df)]

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

# The color levels are globally uniform
max_abs <- max(abs(all_resid_df$Residual), na.rm = TRUE)

var_order <- all_resid_df %>%
  group_by(Variable) %>%
  summarise(P_min = min(P_value, na.rm = TRUE)) %>%
  arrange(P_min) %>%
  pull(Variable)

all_resid_df$Variable <- factor(all_resid_df$Variable, levels = var_order)

# plot
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
    title = "Standardized residuals heatmap",
    subtitle = "Red: Observed higher than expected; Blue: Observed lower than expected",
    x = "transmission", y = NULL
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 6),
    axis.text.x = element_text(size = 6, angle = 0, vjust = 1),
    axis.text.y = element_text(size = 6),
    panel.grid = element_blank()
  )

p_all

ggsave("Standardized_residuals_Significant_Variables.pdf", plot = p_all, width = 10, height = 10)


## 8. Violin plot

sig_cont_vars <- results %>%
  filter(P < 0.05,
         test_method %in% c("t-test","Mann-Whitney U test"),
         variable %in% continuous_vars) %>%
  pull(variable) %>%
  unique()

sig_cont_vars <- sig_cont_vars[sig_cont_vars %in% colnames(df)]

cont_long <- df %>%
  dplyr::select(all_of(c(y, sig_cont_vars))) %>%
  pivot_longer(cols = all_of(sig_cont_vars),
               names_to = "Variable", values_to = "Value") %>%
  filter(!is.na(.data[[y]]), !is.na(Value))

p_map <- results %>%
  filter(variable %in% sig_cont_vars,
         test_method %in% c("t-test","Mann-Whitney U test")) %>%
  group_by(variable) %>%
  summarise(P_min = min(P值, na.rm = TRUE),
            Method = dplyr::first(test_method), .groups="drop") %>%
  mutate(facet_lab = paste0(variable, "\n", Method, " p=", formatC(P_min, format="f", digits=3)))

lab_set <- setNames(p_map$facet_lab, p_map$variable)

# plot
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
    title = "Violin plot of significant continuous variables",
    x = y, y = NULL
  ) +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 9, face = "bold"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  )

print(p_violin)

ggsave("Violin_Significant_Variables.pdf", plot = p_violin, width = 10, height = 6)