#### Figure S5 ####

rm(list = ls())
setwd("/Users/liusi/04_LinuxWorkingSpace/ec_sau/all_sau_strains/cgsnp/")

library(tidyverse)
library(cutpointr)
library(ggplot2)
library(readxl)

# Load snp distance matrix
snp_data <- read.csv("snp_distance_matrix.csv",
                     header = TRUE, 
                     row.names = 1, 
                     check.names = FALSE)

snp_mat <- as.matrix(snp_data)

snp_long <- snp_mat %>%
  as.data.frame() %>%
  rownames_to_column("sample1") %>%
  pivot_longer(
    cols = -sample1,
    names_to = "sample2",
    values_to = "snp_distance"
  ) %>%
  filter(sample1 < sample2)


# Load metadata
metadata <- read_csv("all_metadata_new.csv") %>%
  mutate(
    date = ymd(date),
  ) 

snp_long <- snp_long %>%
  left_join(metadata %>% dplyr::select(id, province1 = province, location1 = location, hospital1 = hospital, ward1 = ward, type1 = type, date1 = date, MLST1 = MLST, CC1 = CC), by = c("sample1" = "id")) %>%
  left_join(metadata %>% dplyr::select(id, province2 = province, location2 = location, hospital2 = hospital, ward2 = ward, type2 = type, date2 = date, MLST2 = MLST, CC2 = CC), by = c("sample2" = "id")) %>%
  mutate(
    time_diff = as.numeric(difftime(date2, date1, units = "days")),
    same_province = (province1 == province2),
    same_location = (location1 == location2),
    same_hospital = (hospital1 == hospital2),
    same_ward = (hospital1 == hospital2) & (ward1 == ward2),
    same_ST = (MLST1 == MLST2),
    same_CC = (CC1 == CC2)
  ) 

snp_long_filter <- snp_long %>% 
  filter(snp_distance <= 100)

snp_long_tra <- snp_long_filter %>%
  mutate(clonal_transmission = (same_ST == TRUE) & (same_ward == TRUE))

write.csv(snp_long_tra, file = "all_snp_long_tra_new.csv", row.names = FALSE, fileEncoding = "UTF-8")


# Determination of optimal SNP threshold
cp <- cutpointr(
  data = snp_long_tra,
  x = snp_distance,
  class = clonal_transmission,
  pos_class = TRUE, 
  direction = "<=",
  method = maximize_metric,
  metric = youden 
)

summary(cp)
optimal_snp_threshold <- cp$optimal_cutpoint
print(paste("The optimal SNP threshold:", optimal_snp_threshold))

# Cutplot
cutplot = plot(cp) 
ggsave("cutplot_SNPthreshold.pdf", cutplot, width = 8, height = 6)