#!/usr/bin/env Rscript

# Plot WG/CG SNP distances by epidemiological link strength and mark the Strong-pair 95th percentile.
# Input: Annotated nearest-neighbor pairs in CSV format.
# Output: Four SNP-distance distribution and diversity PDFs.
# Usage: Rscript 04_plot_wg_cg_SNP_epidemiological_links.R INPUT.csv OUTPUT_DIR

setwd("/Users/liusi/01project_ec_sau/07manuscript/working_path/")

suppressPackageStartupMessages(library(ggplot2))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop(paste("Usage: Rscript 04_plot_wg_cg_SNP_epidemiological_links.R",
             "INPUT.csv OUTPUT_DIR"), call. = FALSE)
}
input_file <- normalizePath(args[1], mustWork = TRUE)
output_dir <- args[2]
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (!dir.exists(output_dir)) stop("Could not create output directory.")

data_pairs <- read.csv(
  input_file,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

required_cols <- c(
  "sample_id", "nearest_sample_id", "wgSNP_distance", "cgSNP_distance",
  "Epi_link_strength", "Epi_link_description", "time_diff", "same_province",
  "same_Location_specific", "same_Department", "same_name", "same_name_clean",
  "same_ST"
)

missing_cols <- setdiff(required_cols, colnames(data_pairs))
if (length(missing_cols) > 0) {
  stop(
    paste0(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  )
}

data_pairs$wgSNP_distance <- as.numeric(data_pairs$wgSNP_distance)
data_pairs$cgSNP_distance <- as.numeric(data_pairs$cgSNP_distance)
data_pairs$time_diff <- suppressWarnings(as.numeric(data_pairs$time_diff))

strength_levels <- c("Strong", "Weak", "None")

data_plot_wg <- subset(data_pairs, !is.na(wgSNP_distance) & wgSNP_distance <= 50)
data_plot_wg <- subset(data_plot_wg, Epi_link_strength %in% strength_levels)

if (nrow(data_plot_wg) == 0) {
  stop("No eligible WG-SNP records with distance <= 50 and Strong/Weak/None labels.")
}

data_plot_wg$Epi_link_strength <- factor(
  data_plot_wg$Epi_link_strength,
  levels = strength_levels
)

data_plot_cg <- subset(data_pairs, !is.na(cgSNP_distance) & cgSNP_distance <= 50)
data_plot_cg <- subset(data_plot_cg, Epi_link_strength %in% strength_levels)

if (nrow(data_plot_cg) == 0) {
  stop("No eligible CG-SNP records with distance <= 50 and Strong/Weak/None labels.")
}

data_plot_cg$Epi_link_strength <- factor(
  data_plot_cg$Epi_link_strength,
  levels = strength_levels
)

strong_all_wg <- subset(data_pairs, Epi_link_strength == "Strong" & !is.na(wgSNP_distance))
strong_all_cg <- subset(data_pairs, Epi_link_strength == "Strong" & !is.na(cgSNP_distance))

if (nrow(strong_all_wg) == 0) {
  stop("No Strong records with non-missing WG-SNP distance.")
}

if (nrow(strong_all_cg) == 0) {
  stop("No Strong records with non-missing CG-SNP distance.")
}

size_axis_lines <- 0.3
font <- "Times"
axis_text_size <- 15
axis_title_size <- 20
ann_text_size <- 5
dot_color <- "dimgray"
size_dot <- 1

plot_strength_distribution <- function(data, x_col, x_label, output_file) {
  p <- ggplot(data, aes(x = .data[[x_col]], fill = Epi_link_strength)) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black", linewidth = size_axis_lines),
      axis.ticks = element_line(linewidth = size_axis_lines),
      text = element_text(family = font),
      axis.text = element_text(size = axis_text_size, color = "black"),
      axis.title = element_text(size = axis_title_size),
      title = element_text(size = axis_title_size)
    ) +
    geom_bar() +
    scale_fill_manual(
      values = c(
        "Strong" = "#f38181",
        "Weak" = "#fce38a",
        "None" = "#c5e3f6"
      ),
      breaks = c("Strong", "Weak", "None")
    ) +
    scale_y_continuous(
      breaks = function(x) {
        upper <- x[2]
        step <- if (upper < 50) 10 else 25
        seq(0, ceiling(upper / step) * step, by = step)
      }
    ) +
    coord_cartesian(xlim = c(0, 50)) +
    xlab(x_label) +
    ylab("Number of Pairs") +
    ggtitle("Strength of Epidemiological Link")

  ggsave(
    output_file,
    plot = p,
    device = "pdf",
    width = 8,
    height = 5,
    dpi = 300,
    units = "in"
  )
}

plot_between_host_diversity <- function(data, y_col, title_text, y_label, output_file) {
  if (nrow(data) == 0) {
    return(invisible(NULL))
  }

  ordered_data <- data[order(data[[y_col]], na.last = TRUE), , drop = FALSE]
  percentile_pos <- max(1, round(nrow(ordered_data) * 0.95))
  percentile_value <- as.numeric(ordered_data[[y_col]][percentile_pos])
  ordered_data$plot_y <- ordered_data[[y_col]]

  p <- ggplot(
    ordered_data,
    aes(x = seq_len(nrow(ordered_data)), y = plot_y)
  ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black", linewidth = size_axis_lines),
      axis.ticks = element_line(linewidth = size_axis_lines),
      text = element_text(family = font),
      axis.text = element_text(size = axis_text_size, color = "black"),
      axis.title = element_text(size = axis_title_size),
      title = element_text(size = axis_title_size)
    ) +
    geom_point(shape = 21, colour = dot_color, fill = dot_color, size = size_dot) +
    coord_cartesian(ylim = c(0, 50)) +
    annotate(
      "segment",
      x = 0, y = percentile_value,
      xend = percentile_pos, yend = percentile_value,
      linetype = "dashed",
      linewidth = size_axis_lines
    ) +
    annotate(
      "segment",
      x = percentile_pos, y = 0,
      xend = percentile_pos, yend = percentile_value,
      linetype = "dashed",
      linewidth = size_axis_lines
    ) +
    annotate(
      "text",
      x = max(2, round(nrow(ordered_data) * 0.12)),
      y = percentile_value + 3,
      label = paste("95 percentile =", percentile_value, "SNPs"),
      family = font,
      size = ann_text_size
    ) +
    ylab(y_label) +
    xlab("Pairs") +
    ggtitle(title_text)

  ggsave(
    output_file,
    plot = p,
    device = "pdf",
    width = 6,
    height = 5,
    dpi = 300,
    units = "in"
  )
}

plot_strength_distribution(
  data_plot_wg,
  "wgSNP_distance",
  "Number of wgSNPs",
  file.path(output_dir, "wg_SNP_epidemiological_link_distribution.pdf")
)

plot_strength_distribution(
  data_plot_cg,
  "cgSNP_distance",
  "Number of cgSNPs",
  file.path(output_dir, "cg_SNP_epidemiological_link_distribution.pdf")
)

plot_between_host_diversity(
  strong_all_wg,
  "wgSNP_distance",
  "WG Diversity in Strong All Pairs",
  "Number of SNPs",
  file.path(output_dir, "wg_SNP_strong_link_diversity.pdf")
)

plot_between_host_diversity(
  strong_all_cg,
  "cgSNP_distance",
  "CG Diversity in Strong All Pairs",
  "Number of SNPs",
  file.path(output_dir, "cg_SNP_strong_link_diversity.pdf")
)

cat("Plots saved to: ", output_dir, "\n")
