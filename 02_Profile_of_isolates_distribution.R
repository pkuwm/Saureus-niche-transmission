#### Figure 2 ####

rm(list = ls())
setwd("/Users/liusi/04_LinuxWorkingSpace/ec_sau/all_sau_strains/in_hospital/statistics_in_hospital/")

library(ggplot2)
library(dplyr)
library(tidyr)

df <- read.csv("hospital_environment.csv")

ward_color <- c("bed rail, bed regulator, bedside table and bed sheet"="#008514",
                "fixed medical equipment"="#58b37b", "mobile medical equipment"="#00a94f",
                "patient-attached device"="#a8f6a3", "others"="#cccccc",
                
                "water faucet and sink"="#0d8ada","door handle and lamp switch"="#b4f0ff",
                "ward furniture"="#77b5e6","ward air"="#44cef6",
                
                "computer, keyboard and mouse"="#dfc0dd", "nursing cart"="#ca8ba1",
                "nasopharyngeal swabs for medical staff"="#ec8d90","palm swabs for medical staff"="#e86b6a",
                
                "self-service machine"="#f2c908","elevator button and stair handrails"="#f1a508",
                "seat armrest"="#ebdc56","trash can"="#efb276","bathroom sink"="#c38743",
                "outpatient pharmacy and reception"="#ffdfb0","outpatient hall air"="#c37854",
                
                "hospital inlet"="#758a99", "hospital outlet"="#424c50")

## Figure 2.1 bubble plot

# count
data_with_source <- df %>%
  group_by(province, sample_location, sample_area) %>%
  summarise(
    count = n(),
    hospital_count = n_distinct(hospital),
    ward_count = n_distinct(ward)
  ) %>%
  mutate(
    sample_source = case_when(
      hospital_count > 1 ~ "multi-hospital",
      hospital_count == 1 & ward_count > 1 ~ "multi-ward",
      hospital_count == 1 & ward_count == 1 ~ "single-ward"
    ),
    sample_source = factor(sample_source, 
                           levels = c("multi-hospital", "multi-ward", "single-ward"))
  ) %>%
  select(-hospital_count, -ward_count)

# levels
data_with_source <- data_with_source %>%
  mutate(
    sample_area = factor(sample_area, 
                         levels = c("hospital sewage","out-patient hall","medical work area","ward public area","ward bed area"))
    )

# plot
p1 <- ggplot(data_with_source, 
            aes(x = factor(province, levels = c("Hebei","Guangdong","Henan","Sichuan","Zhejiang")),
                y = factor(sample_location, 
                           levels = c("bed rail, bed regulator, bedside table and bed sheet",
                                      "fixed medical equipment","patient-attached device",
                                      "mobile medical equipment","others",
                                      "ward air","water faucet and sink","ward furniture",
                                      "door handle and lamp switch",
                                      "computer, keyboard and mouse","nursing cart",
                                      "nasopharyngeal swabs for medical staff",
                                      "palm swabs for medical staff",
                                      "self-service machine","outpatient hall air",
                                      "elevator button and stair handrails","bathroom sink",
                                      "seat armrest","trash can","outpatient pharmacy and reception",
                                      "hospital inlet","hospital outlet")),
                size = count,
                color = sample_source)) +
  geom_point(alpha = 0.5) +
  scale_size_continuous(range = c(5, 15),
                        breaks = c(1, 5, 10, 17)) +
  scale_color_manual(values = c("multi-hospital" = "#70a1d7", 
                                "multi-ward" = "#46b7b9",
                                "single-ward" = "#ebcbae")) +
  labs(x = NULL, y = NULL, 
       size = "Count",
       color = "Sample Source") +
  facet_grid(sample_area ~ ., scales = "free_y", space = "free_y") +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
    strip.background = element_blank(),
    strip.text.y = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_blank()
  )
p1

ggsave("bubble_plot.pdf",p, width = 8, height = 10)


## Figure 2.2 line plot

# count
data_with_season <- df %>%
   group_by(sample_location, season, sample_area) %>%
   summarise(count = n())

# levels
data_with_season <- data_with_season %>% 
  mutate(
    season = factor(season, levels = c("spring", "summer", "autumn", "winter")),
    sample_area = factor(sample_area, 
                         levels = c("hospital sewage","out-patient hall","medical work area","ward public area","ward bed area"))
  )
write.csv(data_with_season, "data_with_season.csv")

# plot
p2 <- ggplot(data_with_season, 
       aes(x = factor(season, 
                      levels = c("spring", "summer", "autumn", "winter")),
           y = count,
           group = sample_location,
           color = sample_location))+
  geom_line(linewidth = 1) +
  geom_point(size = 3, shape = 18) +
  labs(x = NULL, y = NULL,) +
  scale_color_manual(values = ward_color) +
  facet_grid(sample_area ~ ., scales = "free_y", space = "free_y") +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
    strip.background = element_blank(),
    strip.text.y = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_blank(),
    legend.position = "bottom"
  )
p2
ggsave("line_plot.pdf",p2, width = 4, height = 10)


## Figure 2.3 bar plot

df2 <- df %>% mutate(SCCmec_new = ifelse(SCCmec_new == "MSSA", "MSSA", "MRSA"))

# count
data_with_mrsa <- df2 %>%
  group_by(sample_location, SCCmec_new, sample_area) %>%
  summarise(count = n())

# levels
data_with_mrsa <- data_with_mrsa %>% 
  mutate(
    SCCmec_new = factor(SCCmec_new, levels = c("MRSA", "MSSA")),
    sample_area = factor(sample_area, 
                         levels = c("hospital sewage","out-patient hall","medical work area","ward public area","ward bed area"))
  )

plot_mrsa <- data_with_mrsa %>%
  mutate(
    direction = ifelse(SCCmec_new == "MRSA", -count, count),
    label_value = ifelse(direction < 0, -direction, direction)
  )

# plot
p3 <- ggplot(plot_mrsa, 
       aes(
         x = factor(sample_location, levels = c(
           "bed rail, bed regulator, bedside table and bed sheet",
           "fixed medical equipment", "patient-attached device",
           "mobile medical equipment", "others",
           "ward air", "water faucet and sink", "ward furniture",
           "door handle and lamp switch",
           "computer, keyboard and mouse", "nursing cart",
           "nasopharyngeal swabs for medical staff",
           "palm swabs for medical staff",
           "self-service machine", "outpatient hall air",
           "elevator button and stair handrails", "bathroom sink",
           "seat armrest", "trash can", "outpatient pharmacy and reception",
           "hospital inlet", "hospital outlet"
         )),
         y = plot_mrsa$direction, 
         fill = SCCmec_new
       )) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_text(
    aes(y = plot_mrsa$direction + ifelse(plot_mrsa$direction > 0, 1, -1), label = label_value),
    size = 3, 
    color = "black",
    hjust = ifelse(plot_mrsa$direction > 0, 0.5, 0.5)
  ) +
  facet_grid(sample_area ~ ., scales = "free_y", space = "free_y") +
  coord_flip() + 
  scale_fill_manual(values = c("MRSA" = "#71c9ce", "MSSA" = "#cbf1f5")) +
  scale_y_continuous(
    breaks = c(-max(plot_mrsa$count), 0, max(plot_mrsa$count)),
    labels = function(x) abs(x) 
  ) +
  labs(x = NULL, y = NULL) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
    strip.background = element_blank(),
    strip.text.y = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_blank(),
    legend.position = "bottom"
  )
p3
ggsave("bar_plot.pdf",p3, width = 6, height = 10)


## Figure 2.4 violin plot

library(ggbeeswarm)
library(gghalves)

# count resistance genes
df_res <- df %>%
  mutate(resistance_count = rowSums(select(., 16:39))) %>%
  select(sample_location, sample_area, resistance_count) %>%
  mutate(
    sample_location = factor(
      sample_location,
      levels = c(
        "bed rail, bed regulator, bedside table and bed sheet",
        "fixed medical equipment", "patient-attached device",
        "mobile medical equipment", "others",
        "ward air", "water faucet and sink", "ward furniture",
        "door handle and lamp switch",
        "computer, keyboard and mouse", "nursing cart",
        "nasopharyngeal swabs for medical staff",
        "palm swabs for medical staff",
        "self-service machine", "outpatient hall air",
        "elevator button and stair handrails", "bathroom sink",
        "seat armrest", "trash can", "outpatient pharmacy and reception",
        "hospital inlet", "hospital outlet"
      )
    ),
    sample_area = factor(
      sample_area,
      levels = c("hospital sewage", "out-patient hall", "medical work area", 
                 "ward public area", "ward bed area")
    )
  )
write.csv(df_res, "df_res.csv")

# count virulence genes
df_vir <- df %>%
  mutate(vir_count = rowSums(select(., 40:72))) %>%
  select(sample_location, sample_area, vir_count) %>%
  mutate(
    sample_location = factor(
      sample_location,
      levels = c(
        "bed rail, bed regulator, bedside table and bed sheet",
        "fixed medical equipment", "patient-attached device",
        "mobile medical equipment", "others",
        "ward air", "water faucet and sink", "ward furniture",
        "door handle and lamp switch",
        "computer, keyboard and mouse", "nursing cart",
        "nasopharyngeal swabs for medical staff",
        "palm swabs for medical staff",
        "self-service machine", "outpatient hall air",
        "elevator button and stair handrails", "bathroom sink",
        "seat armrest", "trash can", "outpatient pharmacy and reception",
        "hospital inlet", "hospital outlet"
      )
    ),
    sample_area = factor(
      sample_area,
      levels = c("hospital sewage", "out-patient hall", "medical work area", 
                 "ward public area", "ward bed area")
    )
  )
write.csv(df_vir, "df_vir.csv")

# plot
p4 <- ggplot(df_res, aes(x = resistance_count, y = sample_location)) +
  # violin plot
  geom_violin(
    aes(color = sample_location, fill = sample_location),
    scale = "width", alpha = 0.3, width = 0.8,
    trim = FALSE
  ) +
  # box plot
  geom_boxplot(
    aes(color = sample_location, fill = sample_location),
    width = 0.15, alpha = 0.5, outlier.size = 0.5) +
  scale_color_manual(values = ward_color) +
  scale_fill_manual(values = ward_color) +
  facet_grid(sample_area ~ ., scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL) +
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
p4

p5 <- ggplot(df_vir, aes(x = vir_count, y = sample_location)) +
  # violin plot
  geom_violin(
    aes(color = sample_location, fill = sample_location),
    scale = "width", alpha = 0.3, width = 0.8,
    trim = FALSE
  ) +
  # box plot
  geom_boxplot(
    aes(color = sample_location, fill = sample_location),
    width = 0.15, alpha = 0.5, outlier.size = 0.5) +
  scale_color_manual(values = ward_color) +
  scale_fill_manual(values = ward_color) +
  facet_grid(sample_area ~ ., scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL) +
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
p5

ggsave("violin_plot_res.pdf", p4, width = 5, height = 10)
ggsave("violin_plot_vir.pdf", p5, width = 5, height = 10)