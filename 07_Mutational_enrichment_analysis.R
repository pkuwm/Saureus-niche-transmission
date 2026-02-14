#### Figure 5D ####

rm(list = ls())
setwd("/Users/liusi/04_LinuxWorkingSpace/ec_sau/all_sau_strains/in_hospital/")

library(readxl)
library(tidyverse)
library(stringr)
library(rtracklayer)
library(fuzzyjoin)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggbreak)
library(ggforce)
library(patchwork)


## 1. Load gff file

gff_file <- "SA1973.nanopore.gff"
gff <- rtracklayer::import(gff_file)
gff_df <- as.data.frame(gff)

gene_info <- gff_df %>%
  filter(type %in% c("gene", "CDS")) %>%
  select(seqnames, start, end, ID, locus_tag, Name, gene) %>%
  distinct() %>%
  mutate(
    gene_identifier = case_when(
      !is.na(Name) & Name != "" ~ Name,
      !is.na(locus_tag) & locus_tag != "" ~ locus_tag,
      TRUE ~ paste0("GENE_", row_number())
    )
  ) %>%
  dplyr::rename(seq_id = seqnames) %>%
  mutate(gene_length = end - start + 1) %>%
  filter(gene_length >= 300) %>%
  select(seq_id, start, end, gene_identifier, gene_length) %>%
  dplyr::rename(gene = gene_identifier)


## 2. Integrate mutation data

file_path <- "breseq_patient.xlsx"
sheet_names <- excel_sheets(file_path)

# Defined function
is_protein_altering <- function(annotation, mutation) {
  if (str_detect(annotation, "intergenic")) return(FALSE)
  
  if (str_detect(annotation, "^[A-Z]\\d+[A-Z]")) {
    aa_before <- str_sub(annotation, 1, 1)
    aa_after <- str_sub(annotation, str_length(annotation), str_length(annotation))
    return(aa_before != aa_after)
  }

  if (str_detect(annotation, "coding|insertion|deletion")) return(TRUE)
  if (str_detect(mutation, "[+>-]")) return(TRUE)
  
  return(FALSE)
}

# Combine mutation data
combined_data <- map_df(sheet_names, ~{
  df <- read_excel(file_path, sheet = .x, col_types = "text")
  df <- df %>% drop_na(annotation)
  
  sample_cols <- names(df)[7:ncol(df)]
 
  df %>%
    mutate(across(
      all_of(sample_cols), 
      ~ case_when(
        .x %in% c("1", "?", "Δ") ~ 1L, 
        .x == "" ~ 0L,                 
        TRUE ~ suppressWarnings(as.integer(.x))
      )
    )) %>%
    mutate(
      is_snp = map2_lgl(annotation, mutation, is_protein_altering)
    ) %>%
    filter(is_snp) %>%
    select(-is_snp) %>%
    pivot_longer(
      cols = all_of(sample_cols),
      names_to = "sample",
      values_to = "mut_present"
    ) %>% 
    filter(mut_present == 1)
}, .id = "patient"
)

combined_data$gene <- gsub("\\s*[←→]\\s*", "", combined_data$gene)
combined_data <- combined_data %>% dplyr::rename(seq_id = seq)


## 3. Map mutations to gene positions

mutation_gene_mapping <- combined_data %>%
  mutate(mut_start = as.numeric(position), mut_end = as.numeric(position)) %>%
  genome_join(
    gene_info,
    by = c(
      "seq_id", 
      "mut_start" = "start", 
      "mut_end" = "end" 
    ),
    mode = "inner"
  ) %>%
  select(-mut_start, -mut_end)


## 4. Filter out short genes & Calculate the background rate

mut_count_data <- mutation_gene_mapping %>%
  filter(gene_length >= 300) %>% 
  group_by(patient, sample, gene.x, gene_length) %>%
  summarise(gene_mutated = as.integer(n() > 0),
            .groups = "drop") %>%
  group_by(patient, gene.x, gene_length) %>%
  summarise(observed = sum(gene_mutated),
            .groups = "drop")

# genome size of reference
genome_size <- 2731552

# background rate
total_mut_events <- sum(mut_count_data$observed)
background_rate_global <- total_mut_events / genome_size


## 5. Calculate observed expected value & single-tail Poisson test

gene_level_results <- mut_count_data %>%
  group_by(gene.x, gene_length) %>%
  summarize(
    total_observed = sum(observed),
    .groups = "drop"
  ) %>%
  mutate(
    total_expected = background_rate_global * gene_length,
    p_value = ppois(total_observed - 1, lambda = total_expected, lower.tail = FALSE)
  )


## 6. Benjamini–Hochberg correction

combined_results <- gene_level_results %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH"),
    significant = p_adj < 0.05
  ) %>%
  arrange(p_adj)


## 7. Prepare gene data for plot

gene_positions <- mutation_gene_mapping %>%
  select(gene.x, start, end) %>%
  distinct(gene.x, .keep_all = TRUE)%>%
  mutate(midpoint = (start + end) / 2) 

num_gene_patient <- mutation_gene_mapping %>%
  select(gene.x, patient) %>%
  group_by(gene.x) %>% 
  mutate(num_patient = n_distinct(patient)) %>% 
  summarize(num_patient = dplyr::first(num_patient))

gene_data <- combined_results %>%
  left_join(gene_positions, by = "gene.x") %>% 
  left_join(num_gene_patient, by = "gene.x") %>% 
  select(gene = gene.x, start, end, midpoint, gene_length, num_patient,
       total_observed, total_expected, p_value, p_adj, significant)

gene_data <- gene_data %>%
  mutate(
    log_p = -log10(p_adj),
    color_group = ifelse(p_adj < 0.05, "Significant", "Non-significant"),
  )


## 8. Generate genomic location maps

p1 <- ggplot(gene_data, aes(x = midpoint, y = log_p)) +
  geom_point(aes(size = num_patient, color = color_group), alpha = 0.8) +
  geom_text_repel(
    data = subset(gene_data, p_adj < 0.05),
    aes(label = gene),
    size = 2,
    max.overlaps = 20,
    box.padding = 0.5,
    segment.color = "grey50"
  ) +
  geom_hline(
    yintercept = -log10(0.05),
    linetype = "dashed",
    color = "#D32F2F",
    linewidth = 0.5
  ) +
  scale_color_manual(
    values = c("Significant" = "#a6acec", "Non-significant" = "gray70"),
    name = "Significance"
  ) +
  scale_size_continuous(
    range = c(1, 5),
    name = "Number of patient"
  ) +
  labs(
    x = "Genomic Position",
    y = expression("-log"[10]*"(p_adj)"),
  ) +
  scale_x_continuous(
    breaks = seq(0, 3000000, by = 300000),
    labels = function(x) sprintf("%.1f Mb", x / 1000000),
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks.y.right = element_blank(),
    axis.text.y.right = element_blank(),
  ) 

p1

ggsave("mutation_enrichment_output.pdf", plot = p1, width = 9, height = 5, units = "in", dpi = 300)