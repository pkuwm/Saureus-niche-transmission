#### Figure S6 & S8 ####

rm(list = ls())
setwd("/Users/liusi/04_LinuxWorkingSpace/ec_sau/ST398/ST398_gwas_new/")

library(ggplot2)
library(pheatmap)
library(dplyr)
library(tidyr)

## 1. Prepare gwas data
gwas_data <- read.csv("Trait1_16_07_2025_1353.results.csv")

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

write.csv(volcano_data, "volcano_data.csv")


## 2. Prepare gene_presence_absence matrix

gene_presence_absence <- read.csv("~/04_LinuxWorkingSpace/ec_sau/ST398/ST398_gwas_new/gene_presence_absence.csv", na.strings = "") %>% 
  replace(is.na(.), "na")
gene_presence_absence[gene_presence_absence == "na"] <- NA

filtered_data <- subset(volcano_data,
                        as.numeric(Benjamini_H_p) < 0.0001 &
                        Annotation != c("hypothetical protein", "putative protein")
)
trait <- read.csv("traits4.csv")

gene_name <- filtered_data$Gene
sample_name <- trait$X

subdata_rows <- subset(gene_presence_absence, Gene %in% gene_name)
subdata <- subdata_rows[, c("Gene", sample_name)]
rownames(subdata) <- subdata$Gene
write.csv(subdata,"subdata.csv")


## 3. Prepare binary data

binary_matrix <- subdata %>%
  t() %>%  
  as.data.frame() %>%   
  mutate(across(everything(), ~ ifelse(is.na(.), 0, 1))) 

trait_ordered <- trait %>%
  arrange(desc(Trait1)) %>% 
  filter(X %in% rownames(binary_matrix)) 

annotation_row <- data.frame(
  Group = factor(trait_ordered$Trait1),
  row.names = trait_ordered$X
)

binary_ordered <- binary_matrix[trait_ordered$X, ]

# groups cluster
split_groups <- split(1:nrow(binary_ordered), trait_ordered$Trait1)
ordered_idx <- c()

for (group in names(split_groups)) {
  group_idx <- split_groups[[group]]
  group_data <- binary_ordered[group_idx, ]
  if (length(group_idx) > 1) {
    hc <- hclust(dist(group_data)) 
    group_order <- group_idx[hc$order] 
  } else {
    group_order <- group_idx
  }
  ordered_idx <- c(ordered_idx, group_order)
}

binary_group_clustered <- binary_ordered[ordered_idx, ]

group_lengths <- sapply(split_groups, length) 
cum_lengths_rev <- cumsum(rev(group_lengths)) 
gaps_positions_rev <- cum_lengths_rev[-length(cum_lengths_rev)]

col_labels <- filtered_data$Label[match(colnames(binary_group_clustered), filtered_data$Gene)]

# heatmap plot
p <- pheatmap(binary_group_clustered[rev(rownames(binary_group_clustered)), ],
               cluster_rows = FALSE, 
               cluster_cols = TRUE, 
               gaps_row = gaps_positions_rev, 
               color = c("white", "steelblue"),
               annotation_row = annotation_row,
               labels_col = col_labels,
               show_rownames = nrow(binary_ordered) <= 50,
               show_colnames = ncol(binary_ordered) <= 150,
               border_color = NA,
               cellwidth = 2,
               cellheight = 1
)

p
ggsave("GenePresenceAbsence.pdf", p, width = 10, height = 10)