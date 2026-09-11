#!/usr/bin/env Rscript

# Plot genomic linkage probability by province, sampling location and time interval.
# Input: same_ST_pair_denominators/same_ST_pair_linkage_summary_by_province_location_time.tsv.
# Output: Linkage probability PDF and plot-data TSV.
# Usage: Place the input files in the working directory, then run this script.

setwd("/Users/liusi/01project_ec_sau/07manuscript/working_path/")

rm(list = ls())
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(scales)
})

base_dir <- getwd()

input_file <- file.path(
  base_dir,
  "same_ST_pair_denominators",
  "same_ST_pair_linkage_summary_by_province_location_time.tsv"
)
output_pdf <- file.path(base_dir, "province_location_time_linkage_probability.pdf")
output_png <- file.path(base_dir, "province_location_time_linkage_probability.png")
output_data <- file.path(base_dir, "province_location_time_linkage_plot_data.tsv")

if (!file.exists(input_file)) {
  stop("Input summary file not found: ", input_file)
}

time_levels <- c("<=7 days", "8-30 days", "31-180 days", "181-365 days")

plot_data <- readr::read_tsv(input_file, show_col_types = FALSE) %>%
  dplyr::filter(time_bin %in% time_levels) %>%
  dplyr::mutate(
    time_bin = factor(time_bin, levels = time_levels),
    group = dplyr::case_when(
      province_relation == "same province" &
        location_relation == "same location" ~
        "Same province, same location",
      province_relation == "same province" &
        location_relation == "different locations" ~
        "Same province, different locations",
      province_relation == "different provinces" &
        location_relation == "same location" ~
        "Different provinces, same location",
      province_relation == "different provinces" &
        location_relation == "different locations" ~
        "Different provinces, different locations",
      TRUE ~ paste(province_relation, location_relation, sep = "; ")
    ),
    group = factor(
      group,
      levels = c(
        "Same province, same location",
        "Same province, different locations",
        "Different provinces, same location",
        "Different provinces, different locations"
      )
    ),
    ci_lower = as.numeric(str_extract(linkage_probability_95CI, "^[0-9.]+")),
    ci_upper = as.numeric(str_extract(linkage_probability_95CI, "(?<=-)[0-9.]+$")),
    probability_pct = 100 * linkage_probability,
    ci_lower_pct = 100 * ci_lower,
    ci_upper_pct = 100 * ci_upper,
    pair_label = paste0(linked_pairs_le_23_wgSNP, "/", eligible_same_ST_pairs)
  ) %>%
  dplyr::filter(!is.na(time_bin), !is.na(group))

if (nrow(plot_data) == 0) {
  stop("No plottable rows remain after excluding missing time intervals.")
}

group_colours <- c(
  "Same province, same location" = "#ff7f50",
  "Same province, different locations" = "#404969",
  "Different provinces, same location" = "#56B4E9",
  "Different provinces, different locations" = "#bde4f4"
)

plot_main <- ggplot(
  plot_data,
  aes(x = time_bin, y = probability_pct, colour = group, group = group)
) +
  geom_errorbar(
    aes(ymin = ci_lower_pct, ymax = ci_upper_pct),
    width = 0.11,
    linewidth = 0.45
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.8) +
  scale_colour_manual(values = group_colours, name = NULL) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.02, 0.14))
  ) +
  coord_cartesian(ylim = c(-0.5, 58)) +
  labs(
    title = "Genomic linkage probability by geographic and temporal proximity",
    subtitle = expression(paste("Genomic linkage defined as ", italic("<=23 wgSNPs"),
                                "; all comparisons are within the same ST")),
    x = "Sampling interval",
    y = "Probability of genomic linkage",
    caption = "Error bars show exact binomial 95% confidence intervals. Pairs with missing sampling dates and the sparse >365-day stratum are excluded."
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 10),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8, hjust = 0),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )

plot_main <- plot_main +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE))

ggsave(output_pdf, plot_main, width = 6, height = 6, dpi = 300)
readr::write_tsv(plot_data, output_data)

message("Saved: ", output_pdf)
message("Saved: ", output_data)
