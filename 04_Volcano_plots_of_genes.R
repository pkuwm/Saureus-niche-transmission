#### Figure 4 ####

rm(list = ls())
setwd("/Users/liusi/04_LinuxWorkingSpace/ec_sau/all_sau_strains/cgsnp/crosshospital/")

library(ggplot2)
library(pheatmap)
library(dplyr)
library(tidyr)

# Load GWAS result file for plotting
gwas_data <- read.csv("Trait1_04_08_2025_1618.results.csv")    # inter vs intra transmission
# gwas_data <- read.csv("Trait1_07_08_2025_1643.results.csv")    # transmission vs no transmission

# Prepare volcano data
volcano_data <- gwas_data %>%
  mutate(
    Freq_Positive = as.numeric(Number_pos_present_in) / 
      (as.numeric(Number_pos_present_in) + as.numeric(Number_pos_not_present_in)),
    Freq_Negative = as.numeric(Number_neg_present_in) / 
      (as.numeric(Number_neg_present_in) + as.numeric(Number_neg_not_present_in)),
    
    Freq_Pos_Neg = as.numeric(Freq_Positive - Freq_Negative),
    abs_Freq_Pos_Neg = abs(Freq_Pos_Neg),
    
    log2_OR = log2(
      (Freq_Positive/(1-Freq_Positive)) / 
        (Freq_Negative/(1-Freq_Negative))
    )
  ) %>%
  mutate(
    log2_OR = ifelse(is.infinite(log2_OR),
                     sign(log2_OR) * max(abs(log2_OR[is.finite(log2_OR)]), na.rm = TRUE),
                     log2_OR),
    abs_log2_OR = as.numeric(abs(log2_OR))
  ) %>%
  mutate(
    Association_Type = case_when(
      log2_OR > 0 ~ "Positive_more",
      log2_OR < 0 ~ "Negative_more",
      TRUE ~ "Not Significant"
    ),
  ) %>%
  mutate(
    Label = case_when(
      Annotation == "hypothetical protein" ~ "",
      !is.na(Non.unique.Gene.name) & Non.unique.Gene.name != "" ~ Non.unique.Gene.name,
      TRUE ~ Gene
    )
  )

write.csv(volcano_data, "volcano_data_inter_vs_intra_transmission.csv")

# Perpared data for label
volcano_data1 <- read.csv("volcano_data_inter_vs_intra_transmission.csv")

volcano_data2 <- volcano_data1 %>% 
  mutate(
         Significance = case_when(
           as.numeric(Benjamini_H_p) < 0.05 & log2_OR > 0 ~ "Positive_more",
           as.numeric(Benjamini_H_p) < 0.05 & log2_OR < 0 ~ "Negative_more",
           TRUE ~ "Not Significant"),
         Label2 = ifelse(Significance != "Not Significant", Label, NA)) %>% 
  filter(!Annotation %in% c("hypothetical protein", "putative protein")) %>% 
  
  arrange(as.numeric(Benjamini_H_p)) %>%
  mutate(
    Significance_factor = factor(
      Significance,levels = c("Positive_more", "Negative_more", "Not Significant"),
      ordered = TRUE
    )) %>%
  arrange(Significance_factor) 

# Custom colors
volcano_data2 <- volcano_data2 %>%
  mutate(
    point_color = case_when(
      Significance == "Not Significant" ~ "#D9D9D9",
      Significance == "Positive_more" & group == "metal" ~ "#d0af4c",
      Significance == "Positive_more" & group == "resistance" ~ "#3e62ad",
      Significance == "Positive_more" & group == "virulence" ~ "#38b48b",
      Significance == "Positive_more" & group == "mge" ~ "#cc7eb1",
      Significance == "Positive_more" & (is.na(group) | group == "") ~ "#D9D9D9",
      TRUE ~ "#D9D9D9"),
  
    custom_label = case_when(
      as.numeric(Benjamini_H_p) < 0.0001 & !is.na(label_new) & label_new != "" ~ label_new,
      TRUE ~ NA_character_ 
    )
  )

# Volcano plot with labels
p1 <- ggplot(volcano_data2, aes(x = log2_OR, 
                          y = -log10(as.numeric(Benjamini_H_p)))) +
  geom_point(aes(size = abs_Freq_Pos_Neg,
                 color = point_color),
             alpha = 0.7) +
  scale_color_identity() +
    scale_size_continuous(
    range = c(1, 7),
    breaks = c(0.1, 0.2, 0.3, 0.4),
    name = "Frequency Difference"
  ) +
  labs(x = "log2(Odds Ratio)", 
       y = "-log10(Adjusted P-value)",
       color = "Association Type",
       size = "Freq_Positive - Freq_Negative",
       ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
  ggrepel::geom_text_repel(
    aes(label = custom_label),
    size = 3,
    box.padding = 0.8,
    max.overlaps = 60,
    segment.color = "black",
    min.segment.length = 0.2
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    legend.position = "right",
    legend.background = element_rect(fill = alpha("white", 0.8)),
    legend.key = element_blank()
  ) 

p1

ggsave("volcano_plot_inter_vs_intra_transmission.pdf", p1, width = 10, height = 8)