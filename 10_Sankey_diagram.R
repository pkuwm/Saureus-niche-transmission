#### Figure S3 ####

rm(list = ls())
setwd("/Users/liusi/04_LinuxWorkingSpace/ec_sau/all_sau_strains/statistics/three_group/")

library(foreign)
library(ggplot2)
library(ggalluvial)
library(networkD3) 
library(dplyr)
library(ggsci)

df <- read.csv("MLST_SCC_spa.csv")

# Calculate count
data_with_count <- df %>%
  group_by(MLST,SCCmec,spa) %>%
  summarise(count = n())    

# Custom order and color
data_with_count$MLST <- factor(data_with_count$MLST, 
                               levels = c("ST59","ST398","ST22","ST764","ST5","ST6","ST188","ST7"))

custom_colors <- c("ST59" = "#7CA0D4", "ST398" = "#A48AD3", 
                   "ST5" = "#D35F67", "ST22" = "#e17ce6",
                   "ST764" = "#F843A0", "ST188" = "#F1BA5A",
                   "ST6" = "#22c282", "ST7" = "#503381")

# Sankey diagram
p1 <- ggplot(data_with_count,aes(y = count, axis1 = MLST, axis2 = SCCmec, axis3 = spa)) +
  geom_alluvium(aes(fill = MLST), decreasing = FALSE)+
  geom_stratum(width = 1/10, fill = "lightgrey", color = "grey", decreasing = FALSE) +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)), decreasing = FALSE, 
             size = 3, family = "Arial") +
  scale_x_discrete(limits = c("MLST", "SCCmec", "spa"), expand = c(.05, .05)) +
  scale_fill_manual(values = custom_colors)
p1

ggsave("MLST-SCC-spa.pdf", p1, width = 8, height = 10)