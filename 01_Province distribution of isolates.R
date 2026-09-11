#!/usr/bin/env Rscript

# Map the number of isolates collected in each province.
# Input: province_num.csv and the China province boundary GeoJSON.
# Output: province_distribution.pdf.
# Usage: Place the input files in the working directory, then run this script.

setwd("/Users/liusi/01project_ec_sau/07manuscript/working_path/")

rm(list = ls())
library("tidyverse")
library("sf")

# Load China map
china <- sf::st_read("中华人民共和国.json")

# prepare count data
df <- read.csv("province_num.csv" , header = T)

sx_sample <- china %>%
  left_join(df, by = "name")

# Heat map of provincial distribution
p1 <- ggplot(data = sx_sample) + 
  geom_sf(aes(fill = num),
          color = "grey", 
          size =.5) +
  theme_minimal() + 
  coord_sf(datum = NA) +
  scale_fill_gradient(low = "#D9ECFF", 
                      high = "#52A3F4", 
                      na.value = "#F3F3F3",
                      breaks = seq(0, 350, by = 50),
                      limits = c(0, 350)
                      )            
p1
ggsave("province_distribution.pdf", p1, width = 10, height = 10)
