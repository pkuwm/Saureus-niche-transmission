#### Figure 3 ####

rm(list = ls())
setwd("/Users/liusi/04_LinuxWorkingSpace/ec_sau/all_sau_strains/tree/")

library(ggplot2)
library(ggtree)
library(treeio)
library(ggtreeExtra)
library(tidyverse)
library(ggstar)
library(ggnewscale)

tree <- read.tree("del_recom_allsau_tree.fasta.treefile")
anno <- read.csv("/Users/liusi/04_LinuxWorkingSpace/ec_sau/all_sau_strains/tree/anno_for_all_new.csv", header = TRUE)
rownames(anno) <- anno$id 

# resistance and virulence genes
card_gene_cols <- anno[, c(1, 19:42)] 
vfdb_gene_cols <- anno[, c(1, 43:ncol(anno))] 

card_gene_long <- card_gene_cols %>%
  pivot_longer(
    cols = -id,
    names_to = "ARGs",
    values_to = "Presence"
  )

vfdb_gene_long <- vfdb_gene_cols %>%
  pivot_longer(
    cols = -id,
    names_to = "VFGs",
    values_to = "Presence"
  )

# custom order
card_gene_long <- card_gene_long %>%
  mutate(
    ARGs = factor(ARGs, levels = unique(ARGs))
  )

vfdb_gene_long <- vfdb_gene_long %>%
  mutate(
    VFGs = factor(VFGs, levels = unique(VFGs))
  )

# tree plot 
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
    width = 0.001,
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

province_colors <- c(
  "Hebei" = "#2c364f",    
  "Guangdong" = "#c8783a",    
  "Sichuan" = "#8babb2", 
  "Henan" = "#577e89",
  "Zhejiang" = "#d7b966"
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

ward_new_colors <- c(
  "ICU"="#CACCE9", "RICU"="#d6eaf8",
  "EICU"="#aed6f1", "NICU"="#85c1e9",  
  "water inlet"="#4fc3f7", "Out-patient hall"="#fff8e1", 
  "Department of respiratory medicine"="#f9e79f", "Department of pediatrics"="#e6a4c5",
  "Department of geriatric medicine"="#e8daef", "Department of orthopedics"="#eb984e",
  "Department of otolaryngology"="#dcedc2", "Department of general surgery"="#f5b7b1",
  "Department of rehabilitation medicine"="#a3e4d7", "Department of cerebrovascular diseases"="#d2b4de",
  "Department of general family medicine"="#bb8fce", 
  "non-hopital"="white","others"="#aaaaaa", "unknown"="#d5d6d5"
)

clinical_sample_type_new_colors <- c(
  "sputum" = "#d4a5a5",    
  "secreta" = "#ffecda",
  "blood" = "#fbafaf",
  "BALF" = "#a6d0e4",
  "environment" = "white",
  "others"="#aaaaaa", "unknown"="#d5d6d5"
)

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

SCCmec_new_colors <- c(
  "SCCmecIV" = "#d0ebf8",
  "SCCmecV" = "#a0d8f1",
  "SCCmecII" = "#70c7d8",
  "MSSA" = "white",
  "others" = "#aaaaaa",
  "unknown" = "#d5d6d5"
)

spa_colors <- c(
  "t437" = "#FF8080",
  "t309" = "#FFBF80",
  "t011" = "#FFFF80",
  "t189" = "#BFFF80",
  "t1084" = "#9ed048",
  "t091" = "#80FFFF",
  "t899" = "#80BFFF",
  "t1451" = "#8080FF",
  "t304" = "#BF80FF",
  "t441" = "#FF80FF",
  "t4677" = "#FF42A1",
  "others" = "#aaaaaa",
  "unknown" = "#d5d6d5"
)

# tree with annotation
p1 <- main_p %<+% anno + 
  geom_tippoint(
    aes(color = group),
    alpha = 0.5,
    size = 2,
    shape = 19
  ) +
  scale_color_manual(
    values = group_colors,
    name = "Group"
  )

p1

p2 <- p1 + 
  geom_fruit(
    geom = geom_tile,
    mapping = aes(y = id, fill = factor(province)),
    pwidth = 0.2,
    offset = 0.1,
    width = 0.0005,
    alpha = 1,
  ) +
    scale_fill_manual(
      values = province_colors,
      name = "Province"
    ) +
  new_scale_fill() +

  geom_fruit(
    geom = geom_tile,
    mapping = aes(y = id, fill = factor(season)),
    pwidth = 0.2,
    offset = 0.05,
    width = 0.0005,
    alpha = 1,
  ) +
  scale_fill_manual(
    values = season_colors,
    name = "Season"
  ) +
  new_scale_fill()  +
  
  geom_fruit(
    geom = geom_tile,
    mapping = aes(y = id, fill = factor(type)),
    pwidth = 0.2,
    offset = 0.05,
    width = 0.0005,
    alpha = 1,
  ) +
    scale_fill_manual(
      values = type_colors,
      name = "Sample Type"
    ) +
    new_scale_fill() +

  geom_fruit(
    geom = geom_tile,
    mapping = aes(y = id, fill = factor(ward_new)),
    pwidth = 0.2,
    offset = 0.1,
    width = 0.0005,
    alpha = 1,
  ) +
    scale_fill_manual(
      values = ward_new_colors,
      name = "Ward"
    ) +
    new_scale_fill()  +
  
  geom_fruit(
    geom = geom_tile,
    mapping = aes(y = id, fill = factor(clinical_sample_type_new)),
    pwidth = 0.2,
    offset = 0.05,
    width = 0.0005,
    alpha = 1,
  ) +
  scale_fill_manual(
    values = clinical_sample_type_new_colors,
    name = "Clinical sample type"
  ) +
  new_scale_fill()  +
   
  geom_fruit(
    geom = geom_tile,
    mapping = aes(y = id, fill = factor(MLST_new)),
    pwidth = 0.2, 
    offset = 0.1,
    width = 0.0005,
    alpha = 1,
  ) +
  scale_fill_manual(
    values = MLST_new_colors,
    name = "MLST"
  ) +
  new_scale_fill()  +
  
  geom_fruit(
    geom = geom_tile,
    mapping = aes(y = id, fill = factor(spa)),
    pwidth = 0.2, 
    offset = 0.05,
    width = 0.0005,
    alpha = 1,
  ) +
  scale_fill_manual(
    values = spa_colors,
    name = "spa"
  ) +
  new_scale_fill()  +
  
  geom_fruit(
    geom = geom_tile,
    mapping = aes(y = id, fill = factor(SCCmec_new)),
    pwidth = 0.2,
    offset = 0.05,
    width = 0.0005,
    alpha = 1,
  ) +
  scale_fill_manual(
    values = SCCmec_new_colors,
    name = "SCCmec"
  ) +
  new_scale_fill()  + 
  
  geom_fruit(
    data = card_gene_long,
    geom = geom_tile,
    mapping = aes(y = id, x = ARGs, fill = factor(Presence)),
    pwidth = 0.45,
    offset = 0.05,
    width = 0.0002,
    axis.params = list(
      axis = "x", 
      text.angle = 45,
      text.size = 0,
      hjust = 0.5,
      vjust = 0.5,
      line.color = NA,
    ),
    grid.params = list(color = "white")
  ) +
    scale_fill_manual(
      values = c("0" = "white", "1" = "#9db4e3"),
      name = "ARGs"
    ) +
    new_scale_fill()  +
  
    geom_fruit(
      data = vfdb_gene_long,
      geom = geom_tile,
      mapping = aes(y = id, x = VFGs, fill = factor(Presence)),
      pwidth = 0.6,
      offset = 0.05,     
      width = 0.0002,
      axis.params = list(
        axis = "x", 
        text.angle = 45,
        text.size = 0,
        hjust = 0.5,
        vjust = 0.5,
        line.color = NA,
      ),
      grid.params = list(color = "white")
    ) +
    scale_fill_manual(
      values = c("0" = "white", "1" = "#b5dc99"),
      name = "VFGs"
    ) +
  
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

ggsave("tree_with_annotation.pdf", p2, width = 15, height = 15)