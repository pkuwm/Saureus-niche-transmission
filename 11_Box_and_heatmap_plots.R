#### Figure S4 ####

rm(list = ls())
setwd("/Users/liusi/04_LinuxWorkingSpace/ec_sau/all_sau_strains/res_vir/")

library(dplyr)
library(ggplot2)
library(readxl)
library(cowplot)
library(tidyr)
library(stringr) 

## 1. Load annotation file
dat <- read_excel("anno_all_new.xlsx")
rownames(dat) <- dat$id 

plot_data <- dat %>%
  mutate(
    res_total = rowSums(select(., 5:28), na.rm = TRUE), 
    vir_total = rowSums(select(., 29:61), na.rm = TRUE),
  ) 

st_counts <- dat %>%
  count(MLST) %>%
  filter(n > 19) %>%
  arrange(desc(n)

plot_data <- plot_data %>%
  filter(MLST %in% st_counts$MLST)

# Custom colors and order
MLST_new_colors <- c(
  "ST59" = "#7ca0d4",    
  "ST398" = "#a48ad3",
  "ST764" = "#f842a0",
  "ST5" = "#d45f68",
  "ST22" = "#e17ce6",
  "ST188" = "#f1ba5a",
  "ST7" = "#4f3381",
  "ST6" = "#22c282",
  "ST15" = "#adda73",
  "ST9" = "#24779d",
  "ST25" = "#26b8d6",
  "ST45" = "#657d4e",
  "others" = "#aaaaaa",
  "unknown" = "#d5d6d5"
)

st_order <- plot_data %>%
  add_count(MLST, name = "n") %>%
  arrange(n)

mlst_levels <- unique(as.character(st_order$MLST)) 

plot_data <- plot_data %>%
  mutate(ST_label = factor(MLST, levels = mlst_levels))

identical(levels(plot_data$ST_label), mlst_levels) 


## 2. Box plots
library(ggbeeswarm)
library(gghalves)

# 2.1 resistancee gene box plot
res_box <- ggplot(plot_data, aes(x = res_total, y = ST_label)) +
  geom_boxplot(
    aes(color = MLST, fill = MLST),
    width = 0.7, alpha = 0.5, outlier.size = 0.5) +
  geom_quasirandom(aes(color = MLST),
    method = "quasirandom", 
    size = 1, width = 0.3, alpha = 1,
    dodge.width = 0.5 
  ) +
  labs(x = "Number of ARGs", y = NULL) +
  scale_fill_manual(values = MLST_new_colors) +
  scale_color_manual(values = MLST_new_colors) +
  scale_x_continuous(
    breaks = seq(0, 12, 2),
    labels = paste0(seq(0, 12, 2)),
    limits = c(0, 13) 
  ) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
    axis.text.y = element_text(size = 9),
    strip.background = element_blank(),
    strip.text.y = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_blank(),
    legend.position = "none"  
  )
res_box
ggsave("res_box.pdf", res_box, width = 4, height = 8)

# 2.2 virulence gene box plot
vir_box <- ggplot(plot_data, aes(x = vir_total, y = ST_label)) +
  geom_boxplot(
    aes(color = MLST, fill = MLST),
    width = 0.7, alpha = 0.5, outlier.size = 0.5) +
  geom_quasirandom(aes(color = MLST),
                   method = "quasirandom",
                   size = 1, width = 0.3, alpha = 1,
                   dodge.width = 0.5
  ) +
  labs(x = "Number of VFGs", y = NULL) +
  scale_fill_manual(values = MLST_new_colors) +
  scale_color_manual(values = MLST_new_colors) +
  scale_x_continuous(
    breaks = seq(8, 24, 4),
    labels = paste0(seq(8, 24, 4)),
    limits = c(8, 24) 
  ) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
    axis.text.y = element_text(size = 9),
    strip.background = element_blank(),
    strip.text.y = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_blank(),
    legend.position = "none"  
  )
vir_box
ggsave("vir_box.pdf", vir_box, width = 4, height = 8)

## 3. Prepare heatmap data

res_genes <- names(dat)[5:28]
vir_genes <- names(dat)[29:61]

heatmap_data <- plot_data %>%
  select(ST_label, 5:61) %>%  
  group_by(ST_label) %>%     
  summarise(across(everything(), 
                   ~ sum(. != 0, na.rm = TRUE) / n(), 
                   .names = "{.col}_percent")) %>%
  rename_with(~ gsub("_percent", "", .x), -ST_label) 

heatmap_long <- heatmap_data %>%
  pivot_longer(
    cols = -ST_label, 
    names_to = "Gene", 
    values_to = "Percentage" 
  ) 
heatmap_long <- heatmap_long %>% 
  mutate(Percentage = Percentage*100)

gene_order <- colnames(heatmap_data)[-1] 

heatmap_long <- heatmap_long %>%
  mutate(Type = case_when(
    Gene %in% res_genes ~ "res", 
    Gene %in% vir_genes ~ "vir",
    TRUE ~ NA_character_ 
  ))

heatmap_long_res <- heatmap_long %>% 
  filter(Type == "res") %>% 
  mutate(Gene = factor(Gene, levels = gene_order))  

heatmap_long_vir <- heatmap_long %>% 
  filter(Type == "vir") %>% 
  mutate(Gene = factor(Gene, levels = gene_order)) 

## 4. Heatmap plots

# 4.1 resistance heatmap plot
res_heatmap <- ggplot(heatmap_long_res, aes(
  x = Gene,
  y = ST_label,
  fill = Percentage
)) +
  geom_tile(color = "grey30", size = 0.2) + 
  scale_fill_gradientn(
    colors = c("white", "#42A5F5","#1565C0"),
    limits = c(0, 100),
    name = "Percentage (%)"
  ) +
  labs(x = NULL, y = NULL) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 1),
    axis.text.y = element_text(size = 9),
    strip.background = element_blank(),
    strip.text.y = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_blank(),
    legend.position = "none"  
  )
res_heatmap
ggsave("res_heatmap.pdf", res_heatmap, width = 8, height = 8)

# 4.2 virulence heatmap plot
vir_heatmap <- ggplot(heatmap_long_vir, aes(
  x = Gene,
  y = ST_label,
  fill = Percentage
)) +
  geom_tile(color = "grey30", size = 0.2) +
  scale_fill_gradientn(
    colors = c("white", "#A5D6A7","#388E3C"),
    limits = c(0, 100),
    name = "Percentage (%)"
  ) +
  labs(x = NULL, y = NULL) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 1),
    axis.text.y = element_text(size = 9),
    strip.background = element_blank(),
    strip.text.y = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_blank(),
    legend.position = "none"  
  )
vir_heatmap
ggsave("vir_heatmap.pdf", vir_heatmap, width = 8, height = 8)