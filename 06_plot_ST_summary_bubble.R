#!/usr/bin/env Rscript

# Plot ST-level cluster, pair, isolate and location counts for single-ST clusters.
# Input: Precomputed ST-by-cluster Excel summary; default sheet: ST_cluster_details.
# Output: single_ST_summary_bubble.pdf.
# Usage: Rscript 06_plot_ST_summary_bubble.R INPUT.xlsx OUTPUT_DIR [SHEET]

setwd("/Users/liusi/01project_ec_sau/07manuscript/working_path/")

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(grid)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2 || length(args) > 3) {
  stop("Usage: Rscript 06_plot_ST_summary_bubble.R INPUT.xlsx OUTPUT_DIR [SHEET]", call. = FALSE)
}
input_file <- normalizePath(args[1], mustWork = TRUE)
output_dir <- args[2]
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (!dir.exists(output_dir)) stop("Could not create output directory.")

sheet_name <- if (length(args) >= 3) args[3] else "ST_cluster_details"
if (grepl("^[0-9]+$", sheet_name)) sheet_name <- as.integer(sheet_name)
output_pdf <- file.path(output_dir, "single_ST_summary_bubble.pdf")

trim_to_na <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  x
}

message("Reading sheet: ", sheet_name)
raw_df <- read_excel(input_file, sheet = sheet_name)
required_cols <- c(
  "ST", "ST_status", "cluster", "sample_count_of_ST",
  "pair_count_involving_ST", "cluster_sample_count",
  "transmission_node_count_of_ST", "sample_id_list_of_ST",
  "transmission_node_list_of_ST"
)
missing_cols <- setdiff(required_cols, names(raw_df))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}
st_df <- raw_df %>%
  mutate(
    ST = trim_to_na(ST),
    ST_status = trim_to_na(ST_status),
    sample_count_of_ST = as.numeric(sample_count_of_ST),
    pair_count_involving_ST = as.numeric(pair_count_involving_ST),
    cluster_sample_count = as.numeric(cluster_sample_count),
    transmission_node_count_of_ST = as.numeric(transmission_node_count_of_ST)
  ) %>%
  filter(
    !is.na(ST),
    ST_status == "single ST",
    !is.na(sample_count_of_ST),
    !is.na(pair_count_involving_ST),
    !is.na(cluster_sample_count),
    !is.na(transmission_node_count_of_ST)
  ) %>%
  group_by(ST) %>%
  summarise(
    cluster_count = n_distinct(cluster),
    pair_count_involving_ST = sum(pair_count_involving_ST, na.rm = TRUE),
    isolate_count = n_distinct(unlist(strsplit(paste(sample_id_list_of_ST, collapse = "; "), ";\\s*"))),
    transmission_node_count = n_distinct(unlist(strsplit(paste(transmission_node_list_of_ST, collapse = "; "), ";\\s*"))),
    .groups = "drop"
  ) %>%
  filter(
    !is.na(cluster_count),
    !is.na(pair_count_involving_ST),
    !is.na(isolate_count),
    !is.na(transmission_node_count)
  )

if (nrow(st_df) == 0) stop("No eligible single-ST clusters remain for plotting.")

p <- ggplot(
  st_df,
  aes(
    x = cluster_count,
    y = pair_count_involving_ST,
    size = isolate_count,
    fill = transmission_node_count
  )
) +
  geom_point(shape = 21, color = "#333333", stroke = 0.5, alpha = 0.85) +
  geom_text(
    aes(label = ST),
    size = 3.2,
    nudge_y = max(st_df$pair_count_involving_ST, na.rm = TRUE) * 0.03,
    show.legend = FALSE
  ) +
  scale_size_continuous(
    name = "Isolate count",
    range = c(2, 24),
    breaks = c(10, 30, 60, 90, 120)
  ) +
  scale_fill_gradient(
    name = "Transmission node count",
    low = "#ebf6f7",
    high = "#0094c8"
  ) +
  labs(
    title = "Bubble plot of ST transmission importance",
    subtitle = "Single-ST clusters only",
    x = "Cluster count",
    y = "Pair count involving ST"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    legend.key.height = unit(0.8, "cm"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.background = element_rect(color = "black", fill = "white", linewidth = 0.8)
  ) +
  guides(
    size = guide_legend(
      override.aes = list(fill = "#9ecae1", color = "#333333", alpha = 0.9),
      order = 1
    ),
    fill = guide_colorbar(order = 2)
  )

ggsave(output_pdf, p, width = 10, height = 6, device = grDevices::pdf)

message("Done.")
message("Bubble plot: ", output_pdf)
