#### Figure 5C ####

rm(list = ls())
setwd("/Users/liusi/04_LinuxWorkingSpace/ec_sau/all_sau_strains/in_hospital/")

library(readxl)
library(tidyverse)
library(ggplot2)
library(ggtree)
library(treeio)
library(ggtreeExtra)
library(ggstar)
library(ggnewscale)
library(CMplot)


## 1. tree plot

tree <- read.tree("398snp.fasta.treefile")
anno <- read_xlsx("anno_for_hbsrmyy.xlsx")
rownames(anno) <- anno$id

main_p=ggtree(
  tree,
  mapping = NULL,
  layout = "rectangular",
  open.angle = 0,
  mrsd = NULL,
  as.Date = FALSE,
  yscale = "none",
  yscale_mapping = NULL,
  ladderize = TRUE,
  right = FALSE,
  branch.length = "branch.length",
  root.position = 0,
  xlim = NULL,
  color="black", size=0.5, linetype=1
) + 
  geom_treescale(
    x = 0.005, 
    y = -0.1,
    width = 0.1, 
    color = "black", 
    fontsize = 5, 
    offset = 0.5
  )

main_p

# custom colors
group_colors <- c(
  "hospital clinic" = "#a698f0",    
  "hospital environment" = "#a3d1e8",
  "non-hopital" = "#ebdfa4"
)

type_colors <- c(
  "clinic" = "#f85f73",    
  "swab" = "#fbe8d3",
  "air" = "#928a97",
  "sewage" = "#283c63"
)

season_colors <- c(
  "spring" = "#8bbd69",    
  "summer" = "#e69da6",
  "autumn" = "#ebb978",
  "winter" = "#91aec1"
)

sample_location_colors <- c("bed rail, bed regulator, bedside table and bed sheet"="#008514",
                "fixed medical equipment"="#58b37b", "mobile medical equipment"="#00a94f",
                "patient-attached device"="#a8f6a3",
                "water faucet and sink"="#0d8ada","door handle and lamp switch"="#b4f0ff",
                "ward furniture"="#77b5e6","ward air"="#44cef6",
                "computer, keyboard and mouse"="#dfc0dd", "nursing cart"="#ca8ba1",
                "nasopharyngeal swabs for medical staff"="#ec8d90","palm swabs for medical staff"="#e86b6a",
                "self-service machine"="#f2c908","elevator button and stair handrails"="#f1a508",
                "seat armrest"="#ebdc56","trash can"="#efb276",
                "outpatient pharmacy and reception"="#ffdfb0","oupatient hall air"="#eacd76",
                "community unit gate"="#bf794e")

sample_area_colors <- c("ward bed area"="#80aba9", "ward public area"="#507ea4", 
                        "medical work area"="#b88884","community"="#8d6449"
)

# tree with annotation
p1 <- main_p %<+% anno + 
  geom_tippoint(
    aes(color = season), 
    alpha = 0.5, 
    size = 10,
    shape = 19
  ) +
  scale_color_manual(
    values = season_colors,
    name = "Group"
  ) +
  new_scale_fill() +
  geom_tiplab(
    align = T,
    linetype = 3,
    linesize = 0.5,
    offset = 0, 
    size = 3, 
    show.legend = FALSE 
   )

p1

p2 <- p1 + 
  geom_fruit(
    geom = geom_tile, 
    mapping = aes(y = id, fill = factor(sample_location)),
    pwidth = 0.2,
    offset = 0.2,
    width = 0.05, 
    alpha = 1,
  ) +
  scale_fill_manual(
    values = sample_location_colors, 
    name = "Sampling site"
  ) +
  new_scale_fill()  +
  
  geom_fruit(
    geom = geom_tile,
    mapping = aes(y = id, fill = factor(sample_area)),
    pwidth = 0.2, 
    offset = 0.1, 
    width = 0.05,
    alpha = 1,
  ) +
  scale_fill_manual(
    values = sample_area_colors,
    name = "Sampling area"
  ) +
  new_scale_fill() + 

  guides(fill = guide_legend(
    override.aes = list(size = 5), 
    keywidth = unit(0.3, "cm"), 
    keyheight = unit(0.3, "cm")
  )) +
    theme(
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      legend.key.height = unit(0.3, "cm"),
      legend.key.width = unit(0.3, "cm"), 
      legend.spacing.y = unit(0.2, "cm")
    ) 
p2    
ggsave("ST398_SNPtree.pdf", p2, width = 8, height = 10)  



## 2. SNP density plot

df <- read_excel("ST398SNP.xlsx")

result <- df %>%
  select(-mutation, -annotation, -gene, -description) %>%
  filter(!is.na(as.numeric(position))) %>%
  mutate(position = as.numeric(position)) %>%
  pivot_longer(
    cols = -position,
    names_to = "chromosome",
    values_to = "mut_status"
  ) %>%
  filter(mut_status == 1) %>%
  select(-mut_status) %>%
  arrange(chromosome, position) %>%
  mutate(id = row_number(), .before = position) %>%
  select(id, chromosome, position)  

# custom order
custom_order <- c("SA1470","SA1467","SA1465","SA1463","SA1462","SA1820","SA1478","SA1475",
                  "SA1485","SA1472","SA1815","SA1814","SA1484",
                  "SA1473","SA1471","SA1812",
                  "SA432","SA429","SA1818","SA1817","SA424","SA1811","SA1810"
                  ) 
reversed_order <- rev(custom_order)

result$chromosome <- factor(
  result$chromosome,
  levels = custom_order)

result_sorted <- result %>%
  arrange(chromosome, position)

# SNP density plot
CMplot(
  result_sorted,
  plot.type = "d",
  chr.labels = reversed_order,
  bin.size = 1e5, 
  chr.den.col = c("#3ec1d3","#ffd460","#f57170"),
  band = 5,
  file.output = FALSE,
  file = "pdf",
  dpi = 300,
  main = "Staphylococcus aureus ST398 genome", 
  verbose = TRUE, 
)

break_positions <- seq(0, 3e6, by = 5e5)
break_labels <- paste0(seq(0, 3, by = 0.5), "Mb")
axis(1, at = break_positions, labels = break_labels, cex.axis = 0.8)