#!/usr/bin/env Rscript

# Screen hospital predictors with univariable logistic regression and LASSO.
# Input: Hospital characteristics in Excel with English column names and categories.
# Output: Outcome counts, univariable regression results and LASSO-selected terms (CSV).
# Usage: Rscript 07_screen_hospital_logistic_predictors.R INPUT.xlsx OUTPUT_DIR

setwd("/Users/liusi/01project_ec_sau/07manuscript/working_path/")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) stop("Usage: Rscript 07_screen_hospital_logistic_predictors.R INPUT.xlsx OUTPUT_DIR", call. = FALSE)
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
library(glmnet)

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
    Toilet_disinfection_type_merged = case_when(
      Toilet_disinfection_type == "Chlorine-based disinfectant" ~ "Chlorine-based disinfectant",
      Toilet_disinfection_type == "None" ~ "None",
      Toilet_disinfection_type %in% c("Other", "Oxidizing disinfectant", "Alcohol disinfectant") ~ "Other disinfection methods",
      TRUE ~ as.character(Toilet_disinfection_type)
    ),
    Companion_allowed_and_number_merged = case_when(
      Companion_allowed_and_number %in% c("Prohibited", "0") ~ "0/Prohibited",
      Companion_allowed_and_number %in% c("One person", "1") ~ "One person",
      Companion_allowed_and_number %in% c("Two people", "2", "Unlimited") ~ "Two or more people",
      TRUE ~ as.character(Companion_allowed_and_number)
    ),
    Patient_activity_area_merged = case_when(
      Patient_activity_area %in% c("Not allowed", "Within room", "Within department") ~ "Restricted",
      Patient_activity_area %in% c("Within hospital", "Unrestricted") ~ "Open",
      TRUE ~ as.character(Patient_activity_area)
    ),
    Public_toilet_handwash_type_merged = case_when(
      Public_toilet_handwash_type == "Sensor-operated" ~ "Sensor-operated",
      Public_toilet_handwash_type %in% c("Manual", "Foot-operated") ~ "Non-sensor-operated",
      TRUE ~ as.character(Public_toilet_handwash_type)
    ),
    Air_disinfection_type_merged = case_when(
      Air_disinfection_type == "None" ~ "None",
      Air_disinfection_type == "Air disinfection unit/recirculating purifier" ~ "Air disinfection unit/recirculating purifier",
      Air_disinfection_type %in% c("UV disinfection", "UV plus filtration") ~ "UV-based",
      TRUE ~ as.character(Air_disinfection_type)
    ),
    Floor_disinfection_type_merged = case_when(
      Floor_disinfection_type == "Chlorine-based disinfectant" ~ "Chlorine-based disinfectant",
      Floor_disinfection_type %in% c("Other", "Oxidizing disinfectant", "Alcohol disinfectant") ~ "Non-chlorine-based",
      TRUE ~ as.character(Floor_disinfection_type)
    )
  ) %>%
  mutate(
    Toilet_disinfection_type_merged = factor(
      Toilet_disinfection_type_merged,
      levels = c("None", "Chlorine-based disinfectant", "Other disinfection methods")
    ),
    Companion_allowed_and_number_merged = factor(
      Companion_allowed_and_number_merged,
      levels = c("0/Prohibited", "One person", "Two or more people")
    ),
    Patient_activity_area_merged = factor(
      Patient_activity_area_merged,
      levels = c("Restricted", "Open")
    ),
    Public_toilet_handwash_type_merged = factor(
      Public_toilet_handwash_type_merged,
      levels = c("Sensor-operated", "Non-sensor-operated")
    ),
    Air_disinfection_type_merged = factor(
      Air_disinfection_type_merged,
      levels = c("None", "Air disinfection unit/recirculating purifier", "UV-based")
    ),
    Floor_disinfection_type_merged = factor(
      Floor_disinfection_type_merged,
      levels = c("Chlorine-based disinfectant", "Non-chlorine-based")
    )
  )

group_summary <- data.frame(
  variable = c("group", "group2"),
  levels = c(
    paste(names(table(data$group)), table(data$group), collapse = "; "),
    paste(names(table(data$group2)), table(data$group2), collapse = "; ")
  )
)
write.csv(group_summary, file.path(output_dir, "hospital_outcome_distribution.csv"), row.names = FALSE)

candidate_vars <- c(
  "Toilet_disinfection_type_merged",
  "Companion_allowed_and_number_merged",
  "Patient_activity_area_merged",
  "Department_area",
  "Public_toilet_handwash_type_merged",
  "Visiting_hours",

  "Air_disinfection_type_merged",
  "Floor_disinfection_type_merged",
  "Bed_fixed_or_not",
  "Room_area_per_person",
  "Bed_distance_m",
  "Max_capacity_per_room"
)

df_screen <- data %>%
  dplyr::select(group, all_of(candidate_vars)) %>%
  na.omit()

get_univariable_results <- function(var_name, df) {
  fit <- glm(reformulate(var_name, response = "group"),
             data = df, family = binomial)
  est <- summary(fit)$coefficients
  ci <- suppressWarnings(confint.default(fit))

  common_terms <- intersect(rownames(est), rownames(ci))
  common_terms <- common_terms[common_terms != "(Intercept)"]

  data.frame(
    variable = var_name,
    term = common_terms,
    estimate = est[common_terms, "Estimate"],
    std_error = est[common_terms, "Std. Error"],
    z_value = est[common_terms, "z value"],
    p_value = est[common_terms, "Pr(>|z|)"],
    OR = exp(est[common_terms, "Estimate"]),
    lower95 = exp(ci[common_terms, 1]),
    upper95 = exp(ci[common_terms, 2]),
    stringsAsFactors = FALSE
  )
}

univariable_results <- bind_rows(lapply(candidate_vars, get_univariable_results, df = df_screen))
write.csv(univariable_results, file.path(output_dir, "hospital_univariable_logistic_results.csv"), row.names = FALSE)

univariable_summary <- univariable_results %>%
  group_by(variable) %>%
  summarise(min_p_value = min(p_value, na.rm = TRUE), .groups = "drop") %>%
  arrange(min_p_value)
write.csv(univariable_summary, file.path(output_dir, "hospital_univariable_screening_summary.csv"), row.names = FALSE)

lasso_formula <- reformulate(candidate_vars, response = "group")
mm <- model.matrix(lasso_formula, data = df_screen)
x <- mm[, -1, drop = FALSE]
y <- as.numeric(as.character(df_screen$group))

set.seed(123)
cvfit <- cv.glmnet(x, y, family = "binomial", alpha = 1, nfolds = 10)

lasso_1se <- rownames(coef(cvfit, s = "lambda.1se"))[
  which(as.numeric(coef(cvfit, s = "lambda.1se")) != 0)
]
lasso_1se <- setdiff(lasso_1se, "(Intercept)")

lasso_min <- rownames(coef(cvfit, s = "lambda.min"))[
  which(as.numeric(coef(cvfit, s = "lambda.min")) != 0)
]
lasso_min <- setdiff(lasso_min, "(Intercept)")

lasso_results <- data.frame(
  rule = c(rep("lambda.1se", length(lasso_1se)), rep("lambda.min", length(lasso_min))),
  selected_term = c(lasso_1se, lasso_min),
  stringsAsFactors = FALSE
)
write.csv(lasso_results, file.path(output_dir, "hospital_LASSO_selected_terms.csv"), row.names = FALSE)

cat("Screening completed.\n")
cat("Binary outcome distribution:\n")
print(table(data$group))
cat("Transmission scope distribution:\n")
print(table(data$group2))
cat("\nVariables ranked by minimum coefficient P value:\n")
print(univariable_summary)
cat("\nLASSO lambda.1se selected terms:\n")
print(lasso_1se)
cat("\nLASSO lambda.min selected terms:\n")
print(lasso_min)
