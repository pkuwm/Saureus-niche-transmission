#!/usr/bin/env Rscript

# Plot SNP associations with carrier counts and BH-adjusted significance.
# Input: Pyseer results and gene annotations in st764_pyseer/.
# Output: SNP bubble plots and plot-data CSVs in st764_pyseer2/.
# Usage: Place the input files in the working directory, then run this script.

setwd("/Users/liusi/01project_ec_sau/07manuscript/working_path/")

rm(list = ls())

input_dir <- file.path(getwd(), "st764_pyseer")
output_dir <- file.path(getwd(), "st764_pyseer2")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(ggrepel)
  library(scales)
  library(readr)
})

fdr_cutoff <- 0.05

trait_config <- data.frame(
  trait = c("traits5", "traits6", "traits7"),
  sample_size = c(71L, 58L, 31L),
  panel_title = c(
    "ST764 inter-hospital vs intra-hospital\nlinked isolates (n = 71; 30 vs 41)",
    "ST764 C-E vs C-C linked isolates\n(n = 58; 18 vs 40)",
    "ST764 C-E vs E-E linked isolates\n(n = 31; 18 vs 13)"
  ),
  stringsAsFactors = FALSE
)

functional_color <- c(
  "Transport / uptake / efflux" = "#f8b500",
  "Cell envelope / division" = "#eb6ea5",
  "Antibiotic / metal resistance" = "#3e62ad",
  "Metabolism / biosynthesis" = "#b74242",
  "Virulence / host interaction" = "#38b48b",
  "Information processing / repair / regulation / secretion" = "#745399",
  "Unknown" = "#a0d8ef"
)

clean_text <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("%2C", ",") %>%
    str_replace_all("_+", "_") %>%
    str_squish()
}

is_informative_label <- function(x) {
  !is.na(x) & x != "" &
    !str_detect(x, regex("^hypothetical protein$", ignore_case = TRUE))
}

make_variant_id <- function(contig, position, ref, alt) {
  paste(
    as.character(contig),
    format(as.numeric(position), trim = TRUE, scientific = FALSE),
    as.character(ref),
    as.character(alt),
    sep = "_"
  )
}

for (i in seq_len(nrow(trait_config))) {
  trait <- trait_config$trait[[i]]
  sample_size <- trait_config$sample_size[[i]]

  corrected_file <- file.path(
    input_dir, paste0(trait, ".pyseer.multiple_testing.tsv")
  )
  annotation_file <- file.path(
    input_dir, paste0(trait, ".pyseer.annotated.with_functional_category.xlsx")
  )
  output_csv <- file.path(
    output_dir, paste0(trait, ".snp_carrier_count_plot_data_bh_fdr.csv")
  )
  output_pdf <- file.path(
    output_dir, paste0(trait, ".snp_carrier_count_bubble_plot_bh_fdr.pdf")
  )

  if (!file.exists(corrected_file) || !file.exists(annotation_file)) {
    stop("Missing input for ", trait, ": ", corrected_file, " or ", annotation_file)
  }

  corrected <- readr::read_tsv(corrected_file, show_col_types = FALSE)
  required_corrected <- c("af", "beta", "p_lrt", "q_bh", "p_bonferroni")
  missing_corrected <- setdiff(required_corrected, names(corrected))
  if (length(missing_corrected) > 0L) {
    stop(
      "Missing columns in ", corrected_file, ": ",
      paste(missing_corrected, collapse = ", ")
    )
  }

  if (!"variant" %in% names(corrected)) {
    coordinate_columns <- c("contig", "position", "ref", "alt")
    missing_coordinates <- setdiff(coordinate_columns, names(corrected))
    if (length(missing_coordinates) > 0L) {
      stop(
        "The corrected table contains neither 'variant' nor all coordinate columns: ",
        paste(missing_coordinates, collapse = ", ")
      )
    }
    corrected <- corrected %>%
      dplyr::mutate(variant = make_variant_id(contig, position, ref, alt))
  }

  corrected <- corrected %>%
    dplyr::mutate(
      af = as.numeric(af),
      beta = as.numeric(beta),
      p_lrt = as.numeric(p_lrt),
      q_bh = as.numeric(q_bh),
      p_bonferroni = as.numeric(p_bonferroni)
    ) %>%
    dplyr::select(
      variant, af, beta, p_lrt, q_bh, p_bonferroni
    )

  if (anyDuplicated(corrected$variant)) {
    stop("Duplicate variant IDs in ", corrected_file)
  }

  annotations <- readxl::read_excel(annotation_file) %>%
    dplyr::mutate(
      variant = make_variant_id(contig, position, ref, alt),
      gene_id = clean_text(ID),
      name = clean_text(Name),
      product = clean_text(product),
      functional_category = clean_text(`functional category`),
      position = as.numeric(position),
      start = suppressWarnings(as.numeric(start)),
      end = suppressWarnings(as.numeric(end))
    ) %>%
    dplyr::select(
      variant, contig, position, ref, alt, gene_id, name, product,
      functional_category, start, end
    )

  if (anyDuplicated(annotations$variant)) {
    stop(
      "A SNP maps to more than one annotation row in ", annotation_file,
      ". Resolve the annotation before plotting one point per SNP."
    )
  }

  plot_df <- corrected %>%
    dplyr::inner_join(annotations, by = "variant") %>%
    dplyr::filter(
      is.finite(position), is.finite(af), af >= 0, af <= 1,
      is.finite(beta), is.finite(q_bh), q_bh >= 0,
      !is.na(gene_id), gene_id != ""
    ) %>%
    dplyr::mutate(
      analysis_sample_size = sample_size,
      carrier_count_raw = af * sample_size,
      carrier_count = as.integer(round(carrier_count_raw)),
      carrier_count = pmin(sample_size, pmax(0L, carrier_count)),
      significant_bh_fdr = q_bh < fdr_cutoff,
      functional_category = dplyr::if_else(
        is.na(functional_category) | functional_category == "",
        "Unknown",
        functional_category
      ),
      label_text = dplyr::case_when(
        is_informative_label(name) ~ name,
        is_informative_label(product) ~ product,
        TRUE ~ NA_character_
      ),
      neg_log10_q_bh = -log10(pmax(q_bh, .Machine$double.xmin))
    ) %>%
    dplyr::arrange(position, q_bh)

  unmatched <- sum(!corrected$variant %in% annotations$variant)
  if (unmatched > 0L) {
    warning(unmatched, " tested SNPs lacked annotation rows for ", trait)
  }
  if (nrow(plot_df) == 0L) {
    stop("No plot-ready SNPs for ", trait)
  }

  # pyseer AF is rounded in its text output, so AF*n need not be exactly an
  # integer. A large discrepancy can indicate that sample_size is incorrect or
  # that genotypes are missing for some variants.
  integer_deviation <- abs(plot_df$carrier_count_raw - plot_df$carrier_count)
  if (any(integer_deviation > 0.1, na.rm = TRUE)) {
    warning(
      sum(integer_deviation > 0.1, na.rm = TRUE),
      " SNPs in ", trait,
      " have AF*n more than 0.1 from an integer; verify sample size and missing genotypes."
    )
  }

  # Label each gene at most once, using its most significant SNP.
  label_df <- plot_df %>%
    dplyr::filter(significant_bh_fdr, !is.na(label_text)) %>%
    dplyr::mutate(
      label_group = dplyr::if_else(
        !is.na(gene_id) & gene_id != "", gene_id, variant
      )
    ) %>%
    dplyr::group_by(label_group) %>%
    dplyr::slice_min(order_by = q_bh, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  y_range <- range(plot_df$beta, na.rm = TRUE)
  y_span <- diff(y_range)
  if (!is.finite(y_span) || y_span == 0) y_span <- 0.1
  x_breaks <- scales::pretty_breaks(n = 8)(range(plot_df$position, na.rm = TRUE))

  plot_main <- ggplot(plot_df, aes(x = position, y = beta)) +
    geom_hline(
      yintercept = 0, linetype = "dashed", color = "#9A9A9A", linewidth = 0.4
    ) +
    geom_point(
      data = dplyr::filter(plot_df, !significant_bh_fdr),
      aes(size = carrier_count), shape = 21, fill = "#eaeaea", color = "#9E9E9E",
      stroke = 0.25, alpha = 0.5
    ) +
    geom_point(
      data = dplyr::filter(plot_df, significant_bh_fdr),
      aes(size = carrier_count, fill = functional_category),
      shape = 21, color = "#4D4D4D", stroke = 0.25, alpha = 0.75
    ) +
    geom_text_repel(
      data = label_df, aes(label = label_text), size = 3, box.padding = 0.22,
      point.padding = 0.14, min.segment.length = 0, segment.color = "#5A5A5A",
      segment.size = 0.28, seed = 764, max.overlaps = Inf
    ) +
    scale_size_continuous(
      # name = "SNP carrier count", range = c(3, 6), trans = "sqrt",
      name = "SNP carrier count", range = c(2, 7), trans = "sqrt",
      breaks = scales::pretty_breaks(n = 4)
    ) +
    scale_fill_manual(
      values = functional_color, name = "Functional category", drop = FALSE
    ) +
    scale_x_continuous(
      breaks = x_breaks,
      labels = function(x) sprintf("%.1f Mb", x / 1e6),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    coord_cartesian(
      ylim = c(y_range[1] - 0.12 * y_span, y_range[2] + 0.12 * y_span),
      clip = "off"
    ) +
    labs(
      title = trait_config$panel_title[[i]],
      x = "Genomic position",
      y = "Population structure-adjusted effect size (beta)"
    ) +
    theme_classic(base_size = 12) +
    theme(
      axis.line = element_line(color = "black", linewidth = 0.4),
      axis.ticks = element_line(color = "black", linewidth = 0.35),
      axis.ticks.length = grid::unit(0.16, "cm"),
      legend.position.inside = c(0.1, 0.84),
      legend.box = "vertical",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      plot.title = element_text(size = 12, hjust = 0, lineheight = 0.95),
      panel.grid = element_blank(),
      plot.margin = margin(t = 10, r = 18, b = 15, l = 12)
    )

  ggsave(output_pdf, plot_main, width = 9, height = 3.8, dpi = 300)
  readr::write_csv(
    plot_df %>%
      dplyr::select(
        variant, contig, position, ref, alt, gene_id, name, product,
        functional_category, af, analysis_sample_size, carrier_count_raw,
        carrier_count,
        p_lrt, q_bh, p_bonferroni, neg_log10_q_bh, beta,
        significant_bh_fdr
      ),
    output_csv
  )
  message("Saved ", basename(output_pdf), " and ", basename(output_csv))
}
