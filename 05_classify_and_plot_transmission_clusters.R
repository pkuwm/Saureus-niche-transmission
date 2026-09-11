#!/usr/bin/env Rscript

# Classify existing transmission clusters into five categories and plot their networks.
# Input: Clustered sample pairs in Excel, already filtered to WG-SNP <= 23; hospital suffixes C/E denote clinical/environmental samples.
# Output: Classified Excel workbook, cluster/node/edge TSVs and a network PDF.
# Usage: Rscript 05_classify_and_plot_transmission_clusters.R INPUT.xlsx OUTPUT_DIR

setwd("/Users/liusi/01project_ec_sau/07manuscript/working_path/")

suppressPackageStartupMessages({
  library(readxl)
  library(openxlsx)
  library(dplyr)
  library(readr)
  library(igraph)
  library(ggplot2)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2 || length(args) > 2) {
  stop("Usage: Rscript 05_classify_and_plot_transmission_clusters.R INPUT.xlsx OUTPUT_DIR", call. = FALSE)
}
input_file <- normalizePath(args[1], mustWork = TRUE)
output_dir <- args[2]
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (!dir.exists(output_dir)) stop("Could not create output directory.")

classified_xlsx <- file.path(output_dir, "transmission_clusters_five_categories.xlsx")
cluster_summary_tsv <- file.path(output_dir, "transmission_cluster_summary.tsv")
node_output <- file.path(output_dir, "transmission_nodes.tsv")
edge_output <- file.path(output_dir, "transmission_edges.tsv")
pdf_output <- file.path(output_dir, "transmission_network_five_categories.pdf")
if (normalizePath(classified_xlsx, mustWork = FALSE) == input_file) {
  stop("Input workbook must differ from the output workbook.")
}

default_edge_color <- "#bdbdbd"
highlight_edge_color <- "#d73027"
fallback_node_color <- "#7f7f7f"

location_palette <- c(
  "SC-hospital2C" = "#96514d",
  "SC-hospital1C" = "#c38743",
  "HN-hospital1C" = "#dd7a56",
  "HN-hospital2C" = "#e45e32",
  "HB-hospital1C" = "#c85554",
  "HB-hospital2C" = "#ba2636",
  "HB-hospital3C" = "#a22041",
  "GD-hospital2C" = "#e83929",
  "GD-hospital1C" = "#ec6d71",
  "HB-hospital1E" = "#e198b4",
  "HB-hospital2E" = "#d4acad",
  "HB-hospital3E" = "#cc7eb1",
  "HB-hospital4E" = "#e6cde3",
  "HN-hospital1E" = "#b44c97",
  "HN-hospital2E" = "#f09199",
  "SC-hospital1E" = "#eebbcb",
  "SC-hospital2E" = "#cca6bf",
  "GD-hospital1E" = "#bc64a4",
  "GD-hospital2E" = "#c4a3bf",
  "ZJ-hospital1E" = "#9d5b8b",
  "ZJ-hospital2E" = "#f4b3c2",
  "HB-community1" = "#70f3ff",
  "HB-community2" = "#44cef6",
  "HB-community3" = "#2a83a2",
  "HB-community4" = "#2ca9e1",
  "HB-community5" = "#177cb0",
  "HN-community1" = "#0095d9",
  "HN-community2" = "#a0d8ef",
  "SC-community1" = "#84a2d4",
  "SC-community2" = "#59b9c6",
  "SC-community3" = "#83ccd2",
  "SC-community4" = "#698aab",
  "GD-community1" = "#89c3eb",
  "GD-community2" = "#008899",
  "GD-community3" = "#00a3af",
  "GD-community4" = "#2a83a2",
  "ZJ-community1" = "#5383c3",
  "ZJ-community2" = "#abced8",
  "HB-farm1" = "#93ca76",
  "HB-farm2" = "#00e079",
  "HB-farm3" = "#b9d08b",
  "HB-farm4" = "#769164",
  "HB-farm5" = "#badcad",
  "HB-farm6" = "#97a791",
  "HN-farm1" = "#aacf53",
  "HN-farm2" = "#47885e",
  "HN-farm3" = "#68be8d",
  "HN-farm4" = "#028760",
  "HN-farm5" = "#98d98e",
  "SC-farm1" = "#00552e",
  "ZJ-farm1" = "#93ca76",
  "ZJ-farm2" = "#3eb370",
  "HB-WWTP1" = "#4d5aaf",
  "HB-WWTP2" = "#4a488e",
  "HB-WWTP3" = "#706caa",
  "HN-river1" = "#867ba9",
  "HN-river2" = "#a59aca",
  "SC-river1" = "#745399",
  "SC-WWTP1" = "#283c63",
  "GD-WWTP1" = "#1e50a2",
  "GD-WWTP2" = "#8491c3",
  "GD-river1" = "#68699b",
  "ZJ-WWTP1" = "#5654a2",
  "ZJ-river1" = "#522f60"
)

trim_to_na <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  x
}

make_pair_key <- function(a, b) {
  ifelse(a <= b, paste(a, b, sep = " <-> "), paste(b, a, sep = " <-> "))
}

parse_ecosystem <- function(x) {
  sub("[0-9]+[CE]?$", "", sub("^.*-", "", x))
}

parse_base_location <- function(x) {
  sub("[CE]$", "", x)
}

parse_province <- function(x) {
  sub("-.*$", "", x)
}

is_clinical_location <- function(x) {
  grepl("C$", x)
}

rescale_centered <- function(x, span = 1) {
  if (length(x) == 0) {
    return(numeric(0))
  }
  if (all(is.na(x))) {
    return(rep(0, length(x)))
  }
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) {
    return(rep(0, length(x)))
  }
  ((x - mean(rng)) / diff(rng)) * span
}

cluster_category_levels <- c(
  "1. Between ecosystems",
  "2. Between provinces",
  "3. Within province, between locations",
  "4. Same location: clinical vs environment",
  "5. Same location, same source type"
)

message("Reading input: ", input_file)
raw_df <- readxl::read_excel(input_file)

required_cols <- c(
  "sample1_id", "sample2_id", "wg_distance", "time_diff", "cluster",
  "sample1_Location_specific_CE", "sample2_Location_specific_CE"
)
missing_cols <- setdiff(required_cols, names(raw_df))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

df <- raw_df %>%
  mutate(
    across(
      c(sample1_id, sample2_id, cluster, sample1_Location_specific_CE, sample2_Location_specific_CE),
      trim_to_na
    )
  )

location_long <- bind_rows(
  df %>%
    transmute(
      sample_id = sample1_id,
      Location_specific_CE = sample1_Location_specific_CE
    ),
  df %>%
    transmute(
      sample_id = sample2_id,
      Location_specific_CE = sample2_Location_specific_CE
    )
) %>%
  filter(!is.na(sample_id), !is.na(Location_specific_CE)) %>%
  distinct()

location_consistency <- location_long %>%
  group_by(sample_id) %>%
  summarise(n_location = n_distinct(Location_specific_CE), .groups = "drop")

if (any(location_consistency$n_location > 1)) {
  bad_samples <- location_consistency %>%
    filter(n_location > 1) %>%
    pull(sample_id)
  stop(
    "Some samples map to more than one Location_specific_CE: ",
    paste(head(bad_samples, 10), collapse = ", ")
  )
}

edges_base <- df %>%
  transmute(
    from = sample1_id,
    to = sample2_id,
    cluster = cluster,
    wg_distance = suppressWarnings(as.numeric(wg_distance)),
    time_diff = suppressWarnings(as.numeric(time_diff)),
    transmission_node1 = sample1_Location_specific_CE,
    transmission_node2 = sample2_Location_specific_CE
  ) %>%
  filter(!is.na(from), !is.na(to), !is.na(cluster)) %>%
  mutate(
    transmission_node1 = if_else(is.na(transmission_node1), "Unknown", transmission_node1),
    transmission_node2 = if_else(is.na(transmission_node2), "Unknown", transmission_node2),
    ecosystem1 = parse_ecosystem(transmission_node1),
    ecosystem2 = parse_ecosystem(transmission_node2),
    base_location1 = parse_base_location(transmission_node1),
    base_location2 = parse_base_location(transmission_node2),
    province1 = parse_province(transmission_node1),
    province2 = parse_province(transmission_node2),
    clinical1 = is_clinical_location(transmission_node1),
    clinical2 = is_clinical_location(transmission_node2),
    flag_between_ecosystems = ecosystem1 != ecosystem2,
    flag_between_provinces =
      ecosystem1 == ecosystem2 &
      province1 != province2,
    flag_within_province_between_locations =
      ecosystem1 == ecosystem2 &
      province1 == province2 &
      base_location1 != base_location2,
    flag_same_location_clinical_environment =
      base_location1 == base_location2 &
      xor(clinical1, clinical2),
    flag_same_location_same_source =
      base_location1 == base_location2 &
      (clinical1 == clinical2),
    transmission_node_pair = make_pair_key(transmission_node1, transmission_node2),
    time_diff_abs = abs(time_diff)
  ) %>%
  group_by(cluster, transmission_node_pair) %>%
  arrange(
    wg_distance,
    time_diff_abs,
    pmin(from, to),
    pmax(from, to),
    .by_group = TRUE
  ) %>%
  mutate(
    pair_rank = row_number(),
    is_red = pair_rank == 1,
    edge_color = if_else(is_red, highlight_edge_color, default_edge_color),
    edge_class = if_else(is_red, "red", "grey")
  ) %>%
  ungroup()

cluster_flags <- edges_base %>%
  group_by(cluster) %>%
  summarise(
    has_between_ecosystems = any(flag_between_ecosystems),
    has_between_provinces = any(flag_between_provinces),
    has_within_province_between_locations = any(flag_within_province_between_locations),
    has_same_location_clinical_environment = any(flag_same_location_clinical_environment),
    has_same_location_same_source = any(flag_same_location_same_source),
    .groups = "drop"
  ) %>%
  mutate(
    primary_category = case_when(
      has_between_ecosystems ~ cluster_category_levels[1],
      has_between_provinces ~ cluster_category_levels[2],
      has_within_province_between_locations ~ cluster_category_levels[3],
      has_same_location_clinical_environment ~ cluster_category_levels[4],
      has_same_location_same_source ~ cluster_category_levels[5],
      TRUE ~ NA_character_
    ),
    primary_category = factor(primary_category, levels = cluster_category_levels)
  )

cluster_summary <- bind_rows(
  edges_base %>% select(cluster, sample_id = from),
  edges_base %>% select(cluster, sample_id = to)
) %>%
  distinct(cluster, sample_id) %>%
  count(cluster, name = "node_count") %>%
  left_join(
    edges_base %>% count(cluster, name = "edge_count"),
    by = "cluster"
  ) %>%
  left_join(
    edges_base %>%
      filter(is_red) %>%
      count(cluster, name = "red_edge_count"),
    by = "cluster"
  ) %>%
  mutate(red_edge_count = if_else(is.na(red_edge_count), 0L, red_edge_count)) %>%
  left_join(cluster_flags, by = "cluster") %>%
  arrange(primary_category, desc(node_count), desc(edge_count), cluster)

cluster_membership <- bind_rows(
  edges_base %>% select(cluster, sample_id = from),
  edges_base %>% select(cluster, sample_id = to)
) %>%
  distinct(cluster, sample_id)

nodes_out <- location_long %>%
  group_by(sample_id) %>%
  summarise(Location_specific_CE = first(Location_specific_CE), .groups = "drop") %>%
  mutate(
    node_color = if_else(
      Location_specific_CE %in% names(location_palette),
      unname(location_palette[Location_specific_CE]),
      fallback_node_color
    )
  ) %>%
  left_join(
    cluster_membership %>%
      arrange(sample_id, cluster) %>%
      group_by(sample_id) %>%
      summarise(
        cluster_membership = paste(cluster, collapse = ";"),
        primary_cluster = first(cluster),
        .groups = "drop"
      ),
    by = "sample_id"
  ) %>%
  left_join(
    cluster_summary %>%
      select(cluster, primary_category),
    by = c("primary_cluster" = "cluster")
  ) %>%
  arrange(primary_category, primary_cluster, sample_id)

edges_out <- edges_base %>%
  left_join(
    cluster_summary %>%
      select(
        cluster,
        primary_category,
        cluster_node_count = node_count,
        cluster_edge_count = edge_count,
        cluster_red_edge_count = red_edge_count
      ),
    by = "cluster"
  )

cluster_label_positions <- cluster_summary %>%
  mutate(
    category_rank = as.integer(primary_category)
  ) %>%
  group_by(primary_category) %>%
  arrange(desc(node_count), desc(edge_count), cluster, .by_group = TRUE) %>%
  mutate(
    index_in_category = row_number(),
    n_in_category = n(),
    ncol_layout = pmax(1L, ceiling(sqrt(n_in_category))),
    col_id = (index_in_category - 1L) %% ncol_layout + 1L,
    row_id = (index_in_category - 1L) %/% ncol_layout + 1L
  ) %>%
  ungroup()

category_layout <- cluster_label_positions %>%
  group_by(primary_category) %>%
  summarise(
    category_rank = first(category_rank),
    max_row_id = max(row_id),
    .groups = "drop"
  ) %>%
  arrange(category_rank) %>%
  mutate(
    panel_height = pmax(1, max_row_id) * 8 + 9,
    category_top = -dplyr::lag(cumsum(panel_height + 4), default = 0)
  )

cluster_label_positions <- cluster_label_positions %>%
  left_join(
    category_layout %>% select(primary_category, category_top),
    by = "primary_category"
  ) %>%
  mutate(
    anchor_x = (col_id - 1) * 8,
    anchor_y = category_top - (row_id - 1) * 8
  )

graph_obj <- graph_from_data_frame(
  d = edges_out %>% select(from, to, cluster, wg_distance, transmission_node_pair, is_red, edge_class, edge_color),
  vertices = nodes_out %>% rename(name = sample_id),
  directed = FALSE
)

component_map <- tibble::tibble(
  sample_id = V(graph_obj)$name,
  component_id = components(graph_obj)$membership
) %>%
  left_join(cluster_membership, by = "sample_id") %>%
  group_by(component_id) %>%
  summarise(cluster = first(cluster), .groups = "drop")

layout_parts <- vector("list", length = nrow(component_map))

set.seed(20260511)
for (i in seq_len(nrow(component_map))) {
  component_id <- component_map$component_id[i]
  component_cluster <- component_map$cluster[i]
  vertex_names <- V(graph_obj)$name[components(graph_obj)$membership == component_id]
  subgraph_obj <- induced_subgraph(graph_obj, vids = vertex_names)

  coords <- if (vcount(subgraph_obj) == 1) {
    matrix(c(0, 0), ncol = 2)
  } else if (vcount(subgraph_obj) == 2) {
    matrix(c(-0.6, 0, 0.6, 0), byrow = TRUE, ncol = 2)
  } else {
    layout_with_fr(subgraph_obj, niter = 5000, grid = "nogrid")
  }

  coord_df <- tibble::as_tibble(coords, .name_repair = ~ c("x_local", "y_local")) %>%
    mutate(
      sample_id = V(subgraph_obj)$name,
      cluster = component_cluster
    )

  span <- max(2.5, sqrt(vcount(subgraph_obj)) * 1.3)
  coord_df <- coord_df %>%
    mutate(
      x_local = rescale_centered(x_local, span = span),
      y_local = rescale_centered(y_local, span = span)
    ) %>%
    left_join(
      cluster_label_positions %>% select(cluster, anchor_x, anchor_y),
      by = c("cluster" = "cluster")
    )

  coord_df$x <- coord_df$x_local + coord_df$anchor_x
  coord_df$y <- coord_df$y_local + coord_df$anchor_y

  layout_parts[[i]] <- coord_df
}

layout_df <- bind_rows(layout_parts) %>%
  left_join(nodes_out, by = "sample_id")

cluster_centers <- layout_df %>%
  group_by(cluster) %>%
  summarise(
    x = mean(x),
    y = max(y) + 2.0,
    .groups = "drop"
  ) %>%
  left_join(
    cluster_summary %>% select(cluster, primary_category, node_count, edge_count),
    by = "cluster"
  )

edge_plot_df <- igraph::as_data_frame(graph_obj, what = "edges") %>%
  left_join(layout_df %>% select(sample_id, x, y), by = c("from" = "sample_id")) %>%
  left_join(
    layout_df %>% select(sample_id, x, y),
    by = c("to" = "sample_id"),
    suffix = c("", "_end")
  )

panel_df <- cluster_label_positions %>%
  group_by(primary_category) %>%
  summarise(
    xmin = min(anchor_x) - 4.5,
    xmax = max(anchor_x) + 4.5,
    ymax = max(anchor_y) + 4.5,
    ymin = min(anchor_y) - 4.5,
    n_clusters = n(),
    .groups = "drop"
  ) %>%
  mutate(
    label_x = xmin + 0.4,
    label_y = ymax - 0.5
  )

category_count_df <- cluster_summary %>%
  count(primary_category, name = "cluster_count") %>%
  right_join(
    tibble::tibble(primary_category = factor(cluster_category_levels, levels = cluster_category_levels)),
    by = "primary_category"
  ) %>%
  mutate(cluster_count = if_else(is.na(cluster_count), 0L, cluster_count))

classified_df <- df %>%
  left_join(
    cluster_summary %>%
      select(
        cluster,
        primary_category,
        has_between_ecosystems,
        has_between_provinces,
        has_within_province_between_locations,
        has_same_location_clinical_environment,
        has_same_location_same_source
      ),
    by = "cluster"
  )

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "classified_edges")
openxlsx::writeData(wb, "classified_edges", classified_df)
openxlsx::addWorksheet(wb, "cluster_summary")
openxlsx::writeData(wb, "cluster_summary", cluster_summary)
openxlsx::addWorksheet(wb, "category_counts")
openxlsx::writeData(wb, "category_counts", category_count_df)
openxlsx::saveWorkbook(wb, classified_xlsx, overwrite = TRUE)

write_tsv(cluster_summary, cluster_summary_tsv)
write_tsv(nodes_out, node_output)
write_tsv(edges_out, edge_output)

missing_palette_nodes <- setdiff(unique(nodes_out$Location_specific_CE), names(location_palette))
if (length(missing_palette_nodes) > 0) {
  warning(
    "Locations missing from palette, using fallback color: ",
    paste(missing_palette_nodes, collapse = ", ")
  )
}

p <- ggplot() +
  geom_rect(
    data = panel_df,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "#f8f8f8",
    color = "#d9d9d9",
    linewidth = 0.35
  ) +
  geom_text(
    data = panel_df,
    aes(x = label_x, y = label_y, label = primary_category),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,
    size = 3.5,
    fontface = "bold",
    color = "#333333"
  ) +
  geom_segment(
    data = filter(edge_plot_df, !is_red),
    aes(x = x, y = y, xend = x_end, yend = y_end),
    color = default_edge_color,
    alpha = 0.45,
    linewidth = 0.3,
    lineend = "round"
  ) +
  geom_segment(
    data = filter(edge_plot_df, is_red),
    aes(x = x, y = y, xend = x_end, yend = y_end),
    color = highlight_edge_color,
    alpha = 0.95,
    linewidth = 0.45,
    lineend = "round"
  ) +
  geom_point(
    data = layout_df,
    aes(x = x, y = y, fill = Location_specific_CE),
    size = 2.8,
    shape = 21,
    color = "white",
    stroke = 0.2
  ) +
  geom_text(
    data = cluster_centers,
    aes(x = x, y = y, label = cluster),
    size = 2.1,
    color = "#444444",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(location_palette, "Unknown" = fallback_node_color),
    breaks = sort(unique(nodes_out$Location_specific_CE)),
    drop = FALSE,
    name = "Location_specific_CE"
  ) +
  coord_equal() +
  theme_void(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.position = "right",
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key = element_rect(fill = "white", colour = NA),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 8, colour = "#222222"),
    legend.key.height = grid::unit(0.38, "cm"),
    legend.key.width = grid::unit(0.38, "cm"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, colour = "#4d4d4d"),
    plot.caption = element_text(size = 8, colour = "#636363"),
    plot.margin = margin(8, 12, 8, 8)
  ) +
  labs(
    title = "Transmission network grouped by five cluster categories (WG-SNP <= 23, CE)",
    subtitle = paste(
      "Clusters are grouped by primary category with priority:",
      "between ecosystems > between provinces > within-province between locations >",
      "same-location clinical/environment > same-location same source type."
    ),
    caption = paste0(
      "Nodes: ", nrow(nodes_out),
      " | Edges: ", nrow(edges_out),
      " | Clusters: ", nrow(cluster_summary),
      " | Red edges: minimum wg_distance within cluster and transmission-node pair"
    )
  )

ggsave(pdf_output, p, width = 22, height = 26, device = grDevices::pdf)

message("Done.")
message("Classified workbook: ", classified_xlsx)
message("Cluster summary: ", cluster_summary_tsv)
message("Node table: ", node_output)
message("Edge table: ", edge_output)
message("PDF plot: ", pdf_output)
