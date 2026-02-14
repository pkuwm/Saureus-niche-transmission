#### Figure 6 ####

rm(list = ls())
setwd("/Users/liusi/04_LinuxWorkingSpace/ec_sau/ST398/ST398tree/")

library(ggplot2)
library(ggtree)
library(treeio)
library(ggtreeExtra)
library(tidyverse)
library(ggstar)
library(ggnewscale)

# Load files
tree <- read.tree("del_recom_ST398tree.fasta.treefile")
anno <- read.csv("anno_ST398.csv")
rownames(anno) <- anno$id 


# Custom colors
group_colors <- c(
  "cross_location"="#f67280","intra_location"="#62d2a2","non_transmission"="#fae3d9")

saPI_phage_colors <- c(
  "A"="#ffcfdf","B"="#cadefc","C"="#e0f9b5","others"="#aaaaaa")

# Tree plot
main_p=ggtree(
  tree,
  mapping = NULL,
  layout = "fan",
  open.angle = 20,
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
    x = 0.1,
    y = 0.5,
    width = 0.01,
    color = "black", 
    fontsize = 5, 
    offset = 0.5
  )+ xlim(0, 0.0005) 

main_p <- rotate_tree(main_p, angle = 100)
main_p

# Tree with annotation
p <- main_p %<+% anno +
  geom_fruit(
    geom = geom_tile,
    mapping = aes(y = id, fill = factor(group)),
    pwidth = 0.0002,
    offset = 0.2,
    width = 0.00005,
    alpha = 1,
  ) +
  scale_fill_manual(
    values = group_colors,
    name = "Group"
  ) +
  new_scale_fill()  +

  geom_fruit(
    geom = geom_tile,
    mapping = aes(y = id, fill = factor(saPI_phage)),
    pwidth = 0.0002,
    offset = 0.2, 
    width = 0.00005,
    alpha = 1,
  ) +
  scale_fill_manual(
    values = saPI_phage_colors,
    name = "Sa3int+saPI Type"
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

p 

ggsave("ST398tree.pdf", p, width = 8, height = 8)  