#### Figure 5AB ####

rm(list = ls())
setwd("/Users/liusi/04_LinuxWorkingSpace/ec_sau/all_sau_strains/in_hospital/")

library(ggraph)
library(igraph)
library(ggplot2)
library(dplyr)
library(stringr)


## 1. Calculate the transmission weight

df <- read.csv("hospital_transmission.csv", header = TRUE, stringsAsFactors = FALSE, quote = "\"")

df <- df %>%
  rowwise() %>%
  mutate(
    sorted_pair = paste(sort(c(sample_location1, sample_location2)), collapse = "|")
  )

weight_df <- df %>%
  group_by(sorted_pair) %>%
  summarise(weight = n()) %>%
  ungroup()

weight_df <- weight_df %>%
  separate(
    col = sorted_pair,
    into = c("location_A", "location_B"),
    sep = "\\|"
  )

write.csv(weight_df,"weight_df.csv")


## 2. Visualize the transmission network using hive plots

# Prepare ST398 intra-ward transmission network data from "weight_df.csv"
node_st398 <- read.csv("node_st398.csv", stringsAsFactors = FALSE, quote = "\"")
edge_st398 <- read.csv("edge_st398.csv", stringsAsFactors = FALSE, quote = "\"")

# custom colors
ward_color <- c("bed rail, bed regulator, bedside table and bed sheet"="#008514",
              "fixed medical equipment"="#58b37b", "mobile medical equipment"="#00a94f",
              "patient-attached device"="#a8f6a3",
              "water faucet and sink"="#0d8ada","door handle and lamp switch"="#b4f0ff",
              "ward furniture"="#77b5e6","ward air"="#44cef6",
              "computer, keyboard and mouse"="#dfc0dd", "nursing cart"="#ca8ba1",
              "nasopharyngeal swabs for medical staff"="#ec8d90","palm swabs for medical staff"="#e86b6a",
              "self-service machine"="#f2c908","elevator button and stair handrails"="#f1a508",
              "seat armrest"="#ebdc56","trash can"="#efb276",
              "outpatient pharmacy and reception"="#ffdfb0","outpatient hall air"="#eacd76")

graph <- graph_from_data_frame(
  d = edge_st398, 
  vertices = node_st398 %>% dplyr::rename(name = "sample_location"),
  directed = FALSE
)

# 2.1 Hive plot for different area
p1 <- ggraph(graph, layout = "hive", axis = axis, sort.by = radius) +
  geom_edge_hive(
    aes(width = weight, color = time_point), 
    strength = 1.5, 
    alpha = 0.7
  ) +
  geom_node_point(
    aes(size = size, fill = label), 
    shape = 21, 
    stroke = 0 
  ) +
  scale_size_continuous(range = c(3, 8)) +
  scale_edge_width(range = c(0.5, 3)) +
  scale_edge_color_manual(values = c(seasonal = "#61c0bf", same_point = "#fae3d9")) +
  scale_fill_manual(values = ward_color, name = "Sample location") +

  geom_node_text(
    aes(label = name),
    size = 3, 
    color = "black", 
    alpha = 0.9,
    nudge_y = 0.05,
    check_overlap = TRUE
  ) +

  coord_fixed() +
  labs(
    title = "network",
    subtitle = "mode",
    caption = "axis: medical | public | ward"
  ) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(), 
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.caption = element_text(hjust = 0.5)
  )

p1

ggsave("intra-ward_transmission_network.pdf", p1, height = 10, width = 10)


# 2.2 Hive plot for ward/public/medical area

# Select the area category (ward/public/medical)
custom_nodes <- node_st398 %>% 
  filter(axis == "ward") %>% 
#  filter(axis == "public") %>% 
#  filter(axis == "medical") %>% 
  select(name = sample_location, size, label)

custom_edges <- edge_st398 %>% 
  filter(from %in% custom_nodes$name & to %in% custom_nodes$name) %>%
  mutate(axis = "ward")
#  mutate(axis = "public")
#  mutate(axis = "medical")

g <- graph_from_data_frame(
  d = custom_edges,
  directed = FALSE,
  vertices = custom_nodes
)

node_order <- sort(V(g)$name)
n_nodes <- length(node_order)
ly <- matrix(c(seq_along(node_order), rep(0, n_nodes)), 
             ncol = 2, 
             dimnames = list(node_order, c("x", "y")))

# hive plot for the selected area
p2 <- ggraph(g, layout = ly) +
  geom_edge_arc(aes(edge_width = weight, color = time_point), 
                strength = 0.01, 
                alpha = 0.8) +
  geom_node_point(aes(size = size), 
                  shape = 21, 
                  fill = "#008514",
                  stroke = 0) +
  geom_node_text(aes(label = name), 
                 size = 3.5, 
                 nudge_y = 0.08,
                 angle = 0,
                 hjust = 0.5) +
  scale_size_continuous(range = c(3, 8)) + 
  scale_edge_width_continuous(range = c(0.5, 3)) +
  scale_edge_color_manual(values = c(seasonal = "#61c0bf", same_point = "#fae3d9")) +
  labs(title = "network") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal"
  )

p2

ggsave("intra-ward_transmission_ward.pdf", p2, width = 8, height = 4)
#ggsave("intra-ward_transmission_public.pdf", p2, width = 8, height = 4)
#ggsave("intra-ward_transmission_medical.pdf", p2, width = 8, height = 4)