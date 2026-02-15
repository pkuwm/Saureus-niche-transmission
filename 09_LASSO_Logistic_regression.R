#### Figure 7 & Figure S11 ####

rm(list = ls())
setwd("/Users/liusi/03_RworkingSpace/02_ec_sau/")

library(readxl)
library(dplyr)
library(glmnet)
library(pROC)
library(ggplot2)
library(caret)

## 1. Prepare data
df <- read_excel("transmission_climate.xlsx")
df <- df %>% dplyr::select(group, all_of(make.names(all_vars))) %>% na.omit()

all_vars <- c("temperature","daily_rainfall","monthly_rainfall","humidness",
              "ultraviolet",
              "PM2.5","PM10","CO","NO2","SO2","O3")

# Classified by group
df$group <- as.factor(df$group)
names(df) <- make.names(names(df))


## 2. LASSO

model_formula <- reformulate(all_vars, response = "group")

full_mm <- model.matrix(model_formula, data = df)
x <- full_mm[, -1, drop = FALSE]
y <- as.numeric(as.character(df$group))

# cross-validation
set.seed(123)
cvfit <- cv.glmnet(x, y, family = "binomial", alpha = 1, nfolds = 10)
plot(cvfit); title("LASSO CV", line = 2.5)

# variable names
coef_min <- coef(cvfit, s = "lambda.min")
selected <- rownames(coef_min)[which(as.numeric(coef_min) != 0)]
selected <- setdiff(selected, "(Intercept)")
cat("LASSO selected variables:\n"); print(selected)

base_vars <- attr(terms(model_formula), "term.labels")
base_vars_sorted <- base_vars[order(-nchar(base_vars))]

selected_base <- sapply(selected, function(sv) {
  matches <- base_vars_sorted[sapply(base_vars_sorted, function(b) startsWith(sv, make.names(b)))]
  if (length(matches) > 0) return(matches[1])
  matches2 <- base_vars_sorted[sapply(base_vars_sorted, function(b) grepl(make.names(b), sv, fixed = TRUE))]
  if (length(matches2) > 0) return(matches2[1])
  warning(paste0("Cannot map variables: ", sv, "Return to the original variable name"))
  return(sv)
}, USE.NAMES = FALSE)

selected_vars <- unique(selected_base)
cat("\nLASSO selected variables for logistic regression\n"); print(selected_vars)


## 3. Logistic regression

# 3.1 glm: binary logistic regression

final_formula <- as.formula(
  paste("group ~", paste(selected_vars, collapse = " + "))
)
final_model <- glm(final_formula, data = df, family = binomial)

summary(final_model)

# OR & 95% CI
est <- summary(final_model)$coefficients
ci <- confint.default(final_model)

# Output results
res_tab <- data.frame(
  Variable = rownames(est)[-1],
  Estimate = est[-1, "Estimate"],
  Std.Error = est[-1, "Std. Error"],
  Z.value = est[-1, "z value"],
  p.value = est[-1, "Pr(>|z|)"],
  OR = exp(est[-1, "Estimate"]),
  Lower95 = exp(ci[-1, 1]),
  Upper95 = exp(ci[-1, 2]),
  stringsAsFactors = FALSE
)

res_tab$Significance <- cut(
  res_tab$p.value,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("***", "**", "*", "")
)

write.csv(res_tab, "logistic_results.csv", row.names = FALSE)

# 3.1.1 ROC curve
library(pROC)

df$pred_prob <- predict(final_model, type = "response")
roc_obj <- roc(df$group, df$pred_prob)
auc_val <- auc(roc_obj)

roc_df <- data.frame(
  fpr = 1 - roc_obj$specificities,
  tpr = roc_obj$sensitivities
)

roc_plot <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(color = "#1f77b4", linewidth = 1.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  theme_minimal(base_size = 14) +
  labs(
    title = paste0("ROC Curve (AUC = ", round(auc(roc_obj), 3), ")"),
    x = "1 − Specificity",
    y = "Sensitivity"
  ) +
  coord_equal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8) 
  )
roc_plot

ggsave("logistic_results_roc.pdf", roc_plot, width = 5, height = 5, units = "in", dpi = 300)

# 3.1.2 Calibration curves
library(rms)

dd <- datadist(df)
options(datadist = 'dd')

cal_model <- lrm(final_formula, data = df, x = TRUE, y = TRUE)

set.seed(123)
cal <- calibrate(cal_model, method = "boot", B = 1000)

cal_df <- data.frame(
  Predicted = cal[, "predy"],
  Observed = cal[, "calibrated.corrected"]
)

cal_plot <- ggplot(cal_df, aes(x = Predicted, y = Observed)) +
  geom_line(color = "#2ca02c", linewidth = 1.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Calibration Curve (Bootstrap 1000)",
    x = "Predicted Probability",
    y = "Observed Probability"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8) 
  )
cal_plot

ggsave("logistic_results_cal.pdf", cal_plot, width = 5, height = 5, units = "in", dpi = 300)


# 3.1.3 Forest plots
library(grid) 

# Prepare data for forest plots
forest <- read.csv("logistic_results_for_forest.csv")

plot_df <- forest %>%
  mutate(
    log2OR = log2(OR),
    log2Lower = log2(pmax(Lower95, 0.01)),
    log2Upper = log2(Upper95),
    abs_log2OR = abs(log2(OR))
  )

xlim_cutoff <- 2

plot_df <- plot_df %>%
  mutate(
    log2Lower_capped = pmax(log2Lower, -xlim_cutoff),
    log2Upper_capped = pmin(log2Upper,  xlim_cutoff),
    lower_out = log2Lower < -xlim_cutoff, 
    upper_out = log2Upper >  xlim_cutoff 
  )

plot_df <- plot_df %>%
  arrange(log2OR) %>%
  mutate(Variable = factor(Variable, levels = Variable))

forest_plot <- ggplot(plot_df, aes(x = log2OR, y = Variable)) +
  geom_errorbarh(aes(xmin = log2Lower_capped, xmax = log2Upper_capped),
                 color = "gray50", height = 0.25, linewidth = 0.8) +
  geom_segment(
    data = subset(plot_df, lower_out),
    aes(x = -xlim_cutoff, xend = -xlim_cutoff + 0.3, yend = Variable),
    arrow = arrow(length = unit(0.15, "cm"), ends = "first", type = "closed"),
    color = "gray40", linewidth = 0.7
  ) +
  geom_segment(
    data = subset(plot_df, upper_out),
    aes(x =  xlim_cutoff, xend =  xlim_cutoff - 0.3, yend = Variable),
    arrow = arrow(length = unit(0.15, "cm"), ends = "last", type = "closed"),
    color = "gray40", linewidth = 0.7
  ) +
  geom_point(aes(size = abs_log2OR, color = p.value), alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  scale_color_gradientn(
    colors = rev(c("#FDDBC7","#F4A582","#D6604D","#B2182B")),
    trans = "log10",
    name = "p value",
  ) +
  scale_size_continuous(range = c(2.5, 10)) +
  theme_minimal(base_size = 14) +
  labs(
    x = expression(log[2]("Odds Ratio (with 95% CI)")),
    y = "",
    size = "Effect\n(|log2OR|)",
    color = "p value"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "right"
  ) +
  coord_cartesian(xlim = c(-2, 2))

forest_plot 

ggsave("logistic_results_forest.pdf", forest_plot, width = 6, height = 6, units = "in", dpi = 300)



# 3.2 polr: ordinal logistic regression

library(MASS)

# Custom group levels (inter-provincial / inter-hospital / cross-niche vs. intra-level vs. no transmission)
df$group2 <- factor(df$group2, levels = c(0, 1, 2), ordered = TRUE)
df$group3 <- factor(df$group3, levels = c(0, 1, 2), ordered = TRUE)
df$group4 <- factor(df$group4, levels = c(0, 1, 2), ordered = TRUE)

multi_formula <- as.formula(
  paste("group2 ~", paste(all_vars, collapse = " + "))
  # paste("group3 ~", paste(all_vars, collapse = " + "))
  # paste("group4 ~", paste(all_vars, collapse = " + "))
)
model_ordinal <- polr(multi_formula, data = df)
summary(model_ordinal)

# OR & 95% CI
est <- coef(model_ordinal)
vc  <- vcov(model_ordinal)

se_all <- sqrt(diag(vc))
se <- se_all[names(est)]

zval <- est / se
pval <- 2 * (1 - pnorm(abs(zval)))

OR <- exp(est)
Lower95_Wald <- exp(est - 1.96 * se)
Upper95_Wald <- exp(est + 1.96 * se)
ctable <- summary(model_ordinal)$coefficients

# Output results
ordinal_results <- data.frame(
  Variable = names(est),
  Estimate = est,
  Std.Error = se,
  Z.value = zval,
  p.value = pval,
  OR = exp(est),
  Lower95 = exp(est - 1.96 * se),
  Upper95 = exp(est + 1.96 * se)
)
ordinal_results$Significance <- cut(
  ordinal_results$p.value,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("***", "**", "*", "")
)

print(ordinal_results)
write.csv(ordinal_results, "ordinal_logistic_results.csv", row.names = FALSE)


# 3.2.1 ROC curve

pred_probs <- predict(model_ordinal, type = "probs")

# Custom group levels (inter-provincial / inter-hospital / cross-niche vs. intra-level vs. no transmission)

df$binary_high <- ifelse(as.numeric(df$group2) > 2, 1, 0)
# df$binary_high <- ifelse(as.numeric(df$group3) > 2, 1, 0)
# df$binary_high <- ifelse(as.numeric(df$group4) > 2, 1, 0)

table(df$binary_high)

roc_obj <- roc(response = df$binary_high, predictor = pred_probs[,3])
auc_val <- round(auc(roc_obj), 3)
roc_df <- data.frame(
  FPR = 1 - roc_obj$specificities,
  TPR = roc_obj$sensitivities
)

roc_plot2 <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "#377EB8", linewidth = 1.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  labs(
    title = paste0("ROC Curve: (AUC = ", auc_val, ")"),
    x = expression("1 − Specificity (False Positive Rate)"),
    y = "Sensitivity (True Positive Rate)"
  ) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(face = "bold", size = 13),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )

roc_plot2
ggsave("ordinal_logistic_roc2.pdf", roc_plot2, width = 6, height = 6, dpi = 300)


# 3.2.2 Calibration curves

df$pred_prob_high <- pred_probs[, 3]

dd <- datadist(df)
options(datadist = "dd")

cal_model <- lrm(binary_high ~ pred_prob_high, data = df, x = TRUE, y = TRUE)

set.seed(123)
cal <- calibrate(cal_model, method = "boot", B = 1000)

cal_df <- data.frame(
  Predicted = cal[, "predy"],
  Observed  = cal[, "calibrated.corrected"]
)

cal_plot2 <- ggplot(cal_df, aes(x = Predicted, y = Observed)) +
  geom_line(color = "#2ca02c", linewidth = 1.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Calibration Curve (Bootstrap = 1000)",
    x = "Predicted Probability",
    y = "Observed Probability"
  ) +
  coord_cartesian(xlim = c(0, 0.5), ylim = c(0, 0.5)) + 
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

cal_plot2
ggsave("ordinal_logistic_cal2.pdf", cal_plot2, width = 6, height = 6, dpi = 300)