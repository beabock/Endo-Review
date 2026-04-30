library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)

theme_utils_path <- "scripts/plotting/theme_utils.R"
if (file.exists(theme_utils_path)) {
	source(theme_utils_path)
}

INPUT_FILE <- "data/Ollama_cleaned_synresolved_standardized_final.csv"
GBIF_TAXON_FILE <- "data/Reference_datasets/gbif_backbone/Taxon.tsv"
PBDB_FILE <- "data/Reference_datasets/pbdb_all.csv"
OUTPUT_DIR <- "results/taxonomy_analysis"

resolve_existing_path <- function(candidates) {
	for (p in candidates) {
		if (file.exists(p)) {
			return(p)
		}
	}
	return(candidates[[1]])
}

GBIF_TAXON_FILE <- resolve_existing_path(c(
	GBIF_TAXON_FILE,
	"../data/Reference_datasets/gbif_backbone/Taxon.tsv"
))

PBDB_FILE <- resolve_existing_path(c(
	PBDB_FILE,
	"../data/Reference_datasets/pbdb_all.csv"
))

SUMMARY_FILE <- file.path(OUTPUT_DIR, "plant_species_coverage_summary.csv")
PHYLUM_FILE <- file.path(OUTPUT_DIR, "plant_species_coverage_by_phylum.csv")
GENUS_PHYLUM_FILE <- file.path(OUTPUT_DIR, "plant_genus_coverage_by_phylum.csv")
FAMILY_PHYLUM_FILE <- file.path(OUTPUT_DIR, "plant_family_coverage_by_phylum.csv")
TOP_SPECIES_FILE <- file.path(OUTPUT_DIR, "top_studied_plant_species.csv")
OVERALL_PLOT_FILE <- file.path(OUTPUT_DIR, "plant_species_coverage_overall.png")
SPECIES_PHYLUM_ABS_PLOT_FILE <- file.path(OUTPUT_DIR, "plant_species_representation_by_phylum_absolute.png")
SPECIES_PHYLUM_REL_PLOT_FILE <- file.path(OUTPUT_DIR, "plant_species_representation_by_phylum_relative.png")
GENUS_PHYLUM_PLOT_FILE <- file.path(OUTPUT_DIR, "plant_genus_representation_by_phylum_relative.png")
FAMILY_PHYLUM_PLOT_FILE <- file.path(OUTPUT_DIR, "plant_family_representation_by_phylum_relative.png")

if (!file.exists(INPUT_FILE)) {
	stop("Input file not found: ", INPUT_FILE)
}

if (!file.exists(GBIF_TAXON_FILE)) {
	stop("GBIF Taxon.tsv file not found: ", GBIF_TAXON_FILE)
}

if (!dir.exists(OUTPUT_DIR)) {
	dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
}

message("Loading study-level host data...")
study_data <- read_csv(
	INPUT_FILE,
	show_col_types = FALSE,
	col_select = c(
		paper_id,
		plant_host_resolved,
		plant_host_status,
		plant_host_accepted_ids
	)
)

required_cols <- c("paper_id", "plant_host_resolved", "plant_host_accepted_ids")
missing_cols <- setdiff(required_cols, names(study_data))
if (length(missing_cols) > 0) {
	stop("Missing required columns in input data: ", paste(missing_cols, collapse = ", "))
}

message("Loading GBIF backbone reference species...")
# Build a minimal accepted Plantae taxonomy index for lineage-based phylum backfill.
gbif_taxa_min <- read_tsv(
	GBIF_TAXON_FILE,
	show_col_types = FALSE,
	progress = FALSE,
	col_select = c(taxonID, parentNameUsageID, phylum, taxonomicStatus, kingdom)
) %>%
	mutate(
		taxonID = as.character(taxonID),
		parentNameUsageID = as.character(parentNameUsageID),
		phylum = as.character(phylum),
		taxonomicStatus = str_to_lower(str_trim(taxonomicStatus)),
		kingdom = str_trim(kingdom)
	) %>%
	filter(
		kingdom == "Plantae",
		taxonomicStatus == "accepted",
		!is.na(taxonID),
		taxonID != ""
	) %>%
	mutate(phylum = if_else(is.na(phylum), "", str_squish(phylum)))

parent_lookup <- setNames(gbif_taxa_min$parentNameUsageID, gbif_taxa_min$taxonID)
phylum_lookup <- setNames(gbif_taxa_min$phylum, gbif_taxa_min$taxonID)

resolve_phylum_from_lineage <- function(start_taxon_id, parent_map, phylum_map, max_steps = 40) {
	current <- start_taxon_id
	steps <- 0
	while (!is.na(current) && current != "" && steps < max_steps) {
		p <- unname(phylum_map[current])
		if (length(p) > 0 && !is.na(p) && p != "") {
			return(p)
		}
		next_id <- unname(parent_map[current])
		if (length(next_id) == 0 || is.na(next_id) || next_id == "" || identical(next_id, current)) {
			break
		}
		current <- next_id
		steps <- steps + 1
	}
	""
}

reference_species <- read_tsv(
	GBIF_TAXON_FILE,
	show_col_types = FALSE,
	progress = FALSE,
	col_select = c(taxonID, canonicalName, taxonRank, taxonomicStatus, kingdom, phylum, family, genus)
) %>%
	mutate(
		taxonID = as.character(taxonID),
		taxonRank = str_to_upper(str_trim(taxonRank)),
		taxonomicStatus = str_to_lower(str_trim(taxonomicStatus)),
		kingdom = str_trim(kingdom),
		phylum = if_else(is.na(phylum), "", str_squish(phylum))
	) %>%
	filter(
		kingdom == "Plantae",
		taxonRank == "SPECIES",
		taxonomicStatus == "accepted",
		!is.na(taxonID),
		taxonID != ""
	) %>%
	distinct(taxonID, .keep_all = TRUE)

missing_phylum_before_backfill <- sum(is.na(reference_species$phylum) | reference_species$phylum == "")

if (missing_phylum_before_backfill > 0) {
	resolved_values <- vapply(
		reference_species$taxonID,
		resolve_phylum_from_lineage,
		FUN.VALUE = character(1),
		parent_map = parent_lookup,
		phylum_map = phylum_lookup
	)
	reference_species <- reference_species %>%
		mutate(phylum = if_else(phylum == "" & resolved_values != "", resolved_values, phylum))
}

missing_phylum_after_backfill <- sum(is.na(reference_species$phylum) | reference_species$phylum == "")
phylum_backfilled_count <- missing_phylum_before_backfill - missing_phylum_after_backfill

total_known_plant_species_raw <- nrow(reference_species)

# Exclude extinct species from denominator using PBDB when available.
pbdb_extinct_species_count <- 0L
pbdb_extinct_only_species_count <- 0L
gbif_species_removed_by_pbdb <- 0L

if (file.exists(PBDB_FILE)) {
	message("Loading PBDB extinct taxa to exclude extinct plant species from denominator...")
	pbdb_raw <- read_csv(PBDB_FILE, show_col_types = FALSE, skip = 16)

	if (all(c("taxon_rank", "taxon_name", "accepted_rank", "accepted_name", "is_extant") %in% names(pbdb_raw))) {
		pbdb_species_status <- pbdb_raw %>%
			mutate(
				taxon_rank = str_to_lower(str_trim(taxon_rank)),
				accepted_rank = str_to_lower(str_trim(accepted_rank)),
				is_extant = str_to_lower(str_trim(is_extant)),
				taxon_name = str_squish(str_to_lower(taxon_name)),
				accepted_name = str_squish(str_to_lower(accepted_name))
			) %>%
			transmute(
				species_name = case_when(
					taxon_rank == "species" ~ taxon_name,
					accepted_rank == "species" ~ accepted_name,
					TRUE ~ NA_character_
				),
				is_extant
			) %>%
			filter(!is.na(species_name), species_name != "") %>%
			group_by(species_name) %>%
			summarise(
				any_extinct = any(is_extant == "extinct"),
				any_extant = any(is_extant == "extant"),
				.groups = "drop"
			)

		pbdb_extinct_names <- pbdb_species_status %>%
			filter(any_extinct) %>%
			transmute(candidate_name = species_name)

		pbdb_extinct_only_names <- pbdb_species_status %>%
			filter(any_extinct, !any_extant) %>%
			transmute(candidate_name = species_name)

		pbdb_extinct_species_count <- nrow(pbdb_extinct_names)
		pbdb_extinct_only_species_count <- nrow(pbdb_extinct_only_names)

		reference_species <- reference_species %>%
			mutate(canonical_lc = str_squish(str_to_lower(canonicalName))) %>%
			left_join(
				pbdb_extinct_only_names %>% mutate(pbdb_extinct_match = TRUE),
				by = c("canonical_lc" = "candidate_name")
			)

		gbif_species_removed_by_pbdb <- sum(reference_species$pbdb_extinct_match %in% TRUE, na.rm = TRUE)

		reference_species <- reference_species %>%
			filter(!pbdb_extinct_match %in% TRUE) %>%
			select(-canonical_lc, -pbdb_extinct_match)
	} else {
		warning("PBDB file found but expected columns are missing. Extinct-species filtering was skipped.")
	}
} else {
	message("PBDB file not found; denominator uses GBIF accepted species without extinct filtering.")
}

total_known_plant_species <- nrow(reference_species)

message("Extracting studied plant species IDs...")
study_species_links <- study_data %>%
	mutate(
		paper_id = as.character(paper_id),
		plant_host_accepted_ids = as.character(plant_host_accepted_ids),
		plant_host_resolved = as.character(plant_host_resolved)
	) %>%
	filter(
		!is.na(paper_id),
		paper_id != "",
		!is.na(plant_host_accepted_ids),
		plant_host_accepted_ids != ""
	) %>%
	mutate(accepted_id = str_split(plant_host_accepted_ids, "\\s*;\\s*")) %>%
	unnest_longer(accepted_id) %>%
	mutate(accepted_id = str_squish(accepted_id)) %>%
	filter(!is.na(accepted_id), accepted_id != "") %>%
	distinct(paper_id, plant_host_resolved, accepted_id)

study_species_matched <- study_species_links %>%
	inner_join(reference_species, by = c("accepted_id" = "taxonID"))

studied_species <- study_species_matched %>%
	distinct(accepted_id, canonicalName, phylum, family, genus)

studied_species_count <- nrow(studied_species)
coverage_pct <- if (total_known_plant_species > 0) {
	100 * studied_species_count / total_known_plant_species
} else {
	NA_real_
}

coverage_summary <- tibble(
	dataset_rows = nrow(study_data),
	rows_with_plantae_host = sum(!is.na(study_data$plant_host_accepted_ids) & study_data$plant_host_accepted_ids != "", na.rm = TRUE),
	unique_papers_with_plantae_host_ids = n_distinct(study_species_links$paper_id),
	unique_plantae_accepted_ids_in_dataset = n_distinct(study_species_links$accepted_id),
	unique_plantae_species_matched_to_gbif = studied_species_count,
	gbif_species_missing_phylum_before_backfill = missing_phylum_before_backfill,
	gbif_species_missing_phylum_after_backfill = missing_phylum_after_backfill,
	gbif_species_phylum_backfilled_from_lineage = phylum_backfilled_count,
	total_known_plant_species_gbif_raw = total_known_plant_species_raw,
	pbdb_extinct_species_name_count = pbdb_extinct_species_count,
	pbdb_extinct_only_species_name_count = pbdb_extinct_only_species_count,
	gbif_species_removed_by_pbdb_extinct_filter = gbif_species_removed_by_pbdb,
	total_known_plant_species_gbif = total_known_plant_species,
	coverage_percent = coverage_pct
)

write_csv(coverage_summary, SUMMARY_FILE)

known_by_phylum <- reference_species %>%
	mutate(phylum = if_else(is.na(phylum) | phylum == "", "Unassigned", phylum)) %>%
	count(phylum, name = "known_species")

studied_by_phylum <- studied_species %>%
	mutate(phylum = if_else(is.na(phylum) | phylum == "", "Unassigned", phylum)) %>%
	count(phylum, name = "studied_species")

coverage_by_phylum <- known_by_phylum %>%
	left_join(studied_by_phylum, by = "phylum") %>%
	mutate(
		studied_species = replace_na(studied_species, 0L),
		coverage_percent = 100 * studied_species / known_species
	) %>%
	arrange(desc(known_species))

write_csv(coverage_by_phylum, PHYLUM_FILE)

known_genera_by_phylum <- reference_species %>%
	filter(!is.na(genus), genus != "") %>%
	mutate(phylum = if_else(is.na(phylum) | phylum == "", "Unassigned", phylum)) %>%
	distinct(phylum, genus) %>%
	count(phylum, name = "known_genera")

studied_genera_by_phylum <- studied_species %>%
	filter(!is.na(genus), genus != "") %>%
	mutate(phylum = if_else(is.na(phylum) | phylum == "", "Unassigned", phylum)) %>%
	distinct(phylum, genus) %>%
	count(phylum, name = "studied_genera")

genus_coverage_by_phylum <- known_genera_by_phylum %>%
	left_join(studied_genera_by_phylum, by = "phylum") %>%
	mutate(
		studied_genera = replace_na(studied_genera, 0L),
		coverage_percent = 100 * studied_genera / known_genera
	) %>%
	arrange(desc(known_genera))

write_csv(genus_coverage_by_phylum, GENUS_PHYLUM_FILE)

known_families_by_phylum <- reference_species %>%
	filter(!is.na(family), family != "") %>%
	mutate(phylum = if_else(is.na(phylum) | phylum == "", "Unassigned", phylum)) %>%
	distinct(phylum, family) %>%
	count(phylum, name = "known_families")

studied_families_by_phylum <- studied_species %>%
	filter(!is.na(family), family != "") %>%
	mutate(phylum = if_else(is.na(phylum) | phylum == "", "Unassigned", phylum)) %>%
	distinct(phylum, family) %>%
	count(phylum, name = "studied_families")

family_coverage_by_phylum <- known_families_by_phylum %>%
	left_join(studied_families_by_phylum, by = "phylum") %>%
	mutate(
		studied_families = replace_na(studied_families, 0L),
		coverage_percent = 100 * studied_families / known_families
	) %>%
	arrange(desc(known_families))

write_csv(family_coverage_by_phylum, FAMILY_PHYLUM_FILE)

top_studied_species <- study_species_matched %>%
	distinct(paper_id, accepted_id, canonicalName, phylum, family, genus) %>%
	count(accepted_id, canonicalName, phylum, family, genus, name = "study_count", sort = TRUE) %>%
	slice_head(n = 100)

write_csv(top_studied_species, TOP_SPECIES_FILE)

# Overall coverage plot
overall_plot_data <- tibble(
	category = c("Studied", "Not studied"),
	species_count = c(studied_species_count, max(total_known_plant_species - studied_species_count, 0L))
)

overall_plot <- ggplot(overall_plot_data, aes(x = category, y = species_count, fill = category)) +
	geom_col(width = 0.7) +
	geom_text(aes(label = comma(species_count)), vjust = -0.3, size = 4) +
	scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.08))) +
	scale_fill_endo_discrete() +
	labs(
		title = "Plant Species Coverage in Endophyte Literature",
		subtitle = paste0("Coverage: ", percent(coverage_pct / 100, accuracy = 0.01), " of known GBIF accepted plant species"),
		x = NULL,
		y = "Number of species",
		fill = NULL
	) +
	theme_endo_bw(base_size = 12) +
	theme(legend.position = "none")

ggsave(OVERALL_PLOT_FILE, overall_plot, width = 8, height = 5, dpi = 300)

# Species representation by phylum (absolute + relative)
top_n_phyla <- 12
species_phylum_plot_data <- coverage_by_phylum %>%
	slice_head(n = top_n_phyla) %>%
	mutate(phylum = fct_reorder(phylum, known_species))

species_absolute_plot_data <- species_phylum_plot_data %>%
	select(phylum, known_species, studied_species) %>%
	pivot_longer(cols = c(known_species, studied_species), names_to = "metric", values_to = "value") %>%
	mutate(metric = recode(metric, known_species = "Known species", studied_species = "Studied species"))

species_phylum_abs_plot <- ggplot(species_absolute_plot_data, aes(x = phylum, y = value, fill = metric)) +
	geom_col(position = position_dodge(width = 0.75), width = 0.7) +
	coord_flip() +
	scale_y_continuous(labels = comma) +
	scale_fill_endo_discrete() +
	labs(
		title = "Plant Species Representation by Phylum (Absolute)",
		subtitle = paste0("Top ", top_n_phyla, " phyla by known species richness"),
		x = "Phylum",
		y = "Number of species",
		fill = NULL
	) +
	theme_endo_bw(base_size = 12)

ggsave(SPECIES_PHYLUM_ABS_PLOT_FILE, species_phylum_abs_plot, width = 10, height = 7, dpi = 300)

species_phylum_rel_plot_data <- coverage_by_phylum %>%
	slice_head(n = top_n_phyla) %>%
	mutate(phylum = fct_reorder(phylum, coverage_percent))

species_phylum_rel_plot <- ggplot(species_phylum_rel_plot_data, aes(x = phylum, y = coverage_percent)) +
	geom_col(fill = endo_palette_discrete[1], width = 0.75) +
	geom_text(aes(label = percent(coverage_percent / 100, accuracy = 0.01)), hjust = -0.1, size = 3.3) +
	coord_flip(clip = "off") +
	scale_y_continuous(labels = function(x) percent(x / 100, accuracy = 1), expand = expansion(mult = c(0, 0.12))) +
	labs(
		title = "Plant Species Representation by Phylum (Relative)",
		subtitle = paste0("Top ", top_n_phyla, " phyla by known species richness (GBIF accepted species)"),
		x = "Phylum",
		y = "Species representation"
	) +
	theme_endo_bw(base_size = 12)

ggsave(SPECIES_PHYLUM_REL_PLOT_FILE, species_phylum_rel_plot, width = 10, height = 7, dpi = 300)

# Genus and family representation (% of known taxa represented per phylum)
genus_plot_data <- genus_coverage_by_phylum %>%
	slice_head(n = top_n_phyla) %>%
	mutate(phylum = fct_reorder(phylum, coverage_percent))

genus_phylum_plot <- ggplot(genus_plot_data, aes(x = phylum, y = coverage_percent)) +
	geom_col(fill = endo_palette_discrete[2], width = 0.75) +
	geom_text(aes(label = percent(coverage_percent / 100, accuracy = 0.01)), hjust = -0.1, size = 3.3) +
	coord_flip(clip = "off") +
	scale_y_continuous(labels = function(x) percent(x / 100, accuracy = 1), expand = expansion(mult = c(0, 0.12))) +
	labs(
		title = "Plant Genus Representation by Phylum (Relative)",
		subtitle = paste0("Top ", top_n_phyla, " phyla by known genus richness"),
		x = "Phylum",
		y = "Genus representation"
	) +
	theme_endo_bw(base_size = 12)

ggsave(GENUS_PHYLUM_PLOT_FILE, genus_phylum_plot, width = 10, height = 7, dpi = 300)

family_plot_data <- family_coverage_by_phylum %>%
	slice_head(n = top_n_phyla) %>%
	mutate(phylum = fct_reorder(phylum, coverage_percent))

family_phylum_plot <- ggplot(family_plot_data, aes(x = phylum, y = coverage_percent)) +
	geom_col(fill = endo_palette_discrete[3], width = 0.75) +
	geom_text(aes(label = percent(coverage_percent / 100, accuracy = 0.01)), hjust = -0.1, size = 3.3) +
	coord_flip(clip = "off") +
	scale_y_continuous(labels = function(x) percent(x / 100, accuracy = 1), expand = expansion(mult = c(0, 0.12))) +
	labs(
		title = "Plant Family Representation by Phylum (Relative)",
		subtitle = paste0("Top ", top_n_phyla, " phyla by known family richness"),
		x = "Phylum",
		y = "Family representation"
	) +
	theme_endo_bw(base_size = 12)

ggsave(FAMILY_PHYLUM_PLOT_FILE, family_phylum_plot, width = 10, height = 7, dpi = 300)

message("Taxonomy coverage analysis complete:")
message("  GBIF species missing phylum before lineage backfill: ", comma(missing_phylum_before_backfill))
message("  GBIF species phylum backfilled from lineage: ", comma(phylum_backfilled_count))
message("  GBIF species still missing phylum after backfill: ", comma(missing_phylum_after_backfill))
message("  Known plant species (GBIF accepted, raw): ", comma(total_known_plant_species_raw))
message("  PBDB extinct species names (all extinct records): ", comma(pbdb_extinct_species_count))
message("  PBDB extinct-only species names (not also marked extant): ", comma(pbdb_extinct_only_species_count))
message("  Species removed by PBDB extinct filter: ", comma(gbif_species_removed_by_pbdb))
message("  Known plant species (post-filter denominator): ", comma(total_known_plant_species))
message("  Studied plant species (matched by accepted ID): ", comma(studied_species_count))
message("  Coverage: ", percent(coverage_pct / 100, accuracy = 0.01))
message("  Summary file: ", SUMMARY_FILE)
message("  Phylum table: ", PHYLUM_FILE)
message("  Genus phylum table: ", GENUS_PHYLUM_FILE)
message("  Family phylum table: ", FAMILY_PHYLUM_FILE)
message("  Top species table: ", TOP_SPECIES_FILE)
message("  Overall plot: ", OVERALL_PLOT_FILE)
message("  Species phylum absolute plot: ", SPECIES_PHYLUM_ABS_PLOT_FILE)
message("  Species phylum relative plot: ", SPECIES_PHYLUM_REL_PLOT_FILE)
message("  Genus phylum relative plot: ", GENUS_PHYLUM_PLOT_FILE)
message("  Family phylum relative plot: ", FAMILY_PHYLUM_PLOT_FILE)