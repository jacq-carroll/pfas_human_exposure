# Author: Jacquie Carroll
# Description: Ecological grouping, calculating weighted means
#   & preparing dataset for dose calculation
#   1st script in dose pipeline 
# date created: 15 Jul 2025 
# date last updated: 5 Aug 2025 

# list required packages 
required_packages_install <- c("dplyr",
                               "tidyr",
                               "stringr",
                               "knitr",
                               "ggplot2",
                               "NADA",
                               "purrr",
                               "forcats")
# load required packages 
for (package in required_packages_install) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

# source functions script 
source("r_code/functions.R")

# read in datasets
  # PFAS concentration dataframe
pfas_conc <- read.csv("data/pfas_conc_master.csv")
  # species importance from survey
species_importance <- as.data.frame(read.csv("data/importance.csv"))
  # ecological trait sheet 
eco_traits <- read.csv("data/eco_traits.csv")

#trim each character column 
pfas_conc <- pfas_conc %>% 
  mutate(across(where(is.character), str_trim))

# Determine important summary stats for dataset
# 1. sample size for each species 
df_total_sample_size <- pfas_conc %>%
  mutate(common_name = str_trim(common_name)) %>% 
  mutate(sample_ID = str_trim(sample_ID)) %>%     
  distinct(common_name, sample_ID, sample_size..for.composites.) %>%  # remove repeats due to compound
  group_by(common_name) %>%
  summarise(total_individuals = sum(sample_size..for.composites.),
            total_samples = n_distinct(sample_ID))

# 2. species importance based on survey data 
  #  trim and order dataframe
species_importance <- species_importance %>%
  mutate(across(where(is.character), str_trim))
species_importance <- species_importance[order(-species_importance$importance),]

  # join importance and sample size 
merge_sample_importance <- inner_join(df_total_sample_size, species_importance,
                                      by = c("common_name" = "Species"))
  # order based on importance
ordered_importance <- merge_sample_importance[order(-merge_sample_importance$importance),]

donthave <- anti_join(species_importance, df_total_sample_size,
                      by = c("Species" = "common_name"))

# 3. what proportion of our data is censored?
censor_proportion <- as.data.frame(table(pfas_conc$censored..T.F.))
censor_proportion_table <- censor_proportion %>% 
  mutate(sum = sum(Freq)) %>% 
  mutate(proportion = Freq /sum)
  # view censored data 
kable(censor_proportion_table)

# Standardize compound names across the dataset origin
  # examine unique compounds (all compound names)
pfas_list <- as.vector(unique(c(pfas_conc$compound)))
  # manually create dictionary for abbreviations 
pfas_dict <- c(
  "Perfluorododecanesulfonate" = "PFDoS",
  "N-Methylperfluorooctane sulfonamido ethanol" = "NMeFOSAA",
  "Perfluorooctanoic acid" = "PFOA",
  "Perfluorobutanoate" = "PFBA",
  "FtS 8:2 ion" = "8:2 FTS",
  "4,4,5,5,6,6,7,7,8,8,9,9,10,10,10-Pentadecafluorodec-2-enoic acid" = "7:3 FTCA",
  "Perfluorooctanesulfonamide" = "PFOSA",
  "Perfluoro-3-methoxypropanoic acid" = "PFMPA",
  "FtS 6:2 ion" = "6:2 FTS",
  "N-methyl perfluorooctanesulfonamidoacetic acid" = "NMeFOSAA",
  "Perfluoro(2-ethoxyethane)sulfonic acid" = "PFEESA",
  "Perfluoro-3,6-dioxaheptanoic acid" = "NFDHA",
  "Perfluorodecanoate" = "PFDA",
  "N-Methyl perfluorooctane sulfonamide" = "NMeFOSA",
  "4,8-dioxa-3H-perfluorononanoate" = "ADONA",
  "Perfluoroheptanesulfonate" = "PFHpS",
  "Perfluorodecanesulfonate" = "PFDS",
  "N-Ethylperfluorooctane-1-sulfonamide" = "NEtFOSA",
  "Perfluorotetradecanoate" = "PFTeDA",
  "3:3 Fluorotelomer carboxylate, ion(1-)" = "3:3 FTCA",
  "Perfluorononanesulfonate" = "PFNS",
  "Perfluorooctanesulfonate" = "PFOS",
  "Perfluorohexanesulfonic acid" = "PFHxS",
  "N-ethyl perfluorooctanesulfonamidoacetic acid" = "NEtFOSAA",
  "Perfluorobutanesulfonate" = "PFBS",
  "Perfluorotridecanoate" = "PFTrDA",
  "Perfluoroheptanoate" = "PFHpA",
  "9-Chlorohexadecafluoro-3-oxanonane-1-sulfonic acid" = "9ClPF3ONS",
  "Perfluorononanoate" = "PFNA",
  "Hexafluoropropylene oxide dimer acid" = "HFPO-DA",
  "GenX" = "HFPO-DA",
  "11-chloroeicosafluoro-3-oxaundecane-1-sulfonic acid" = "11ClPF3OUdS",
  "Perfluoro(4-methoxybutanoic) acid" = "PFMBA",
  "Perfluoropentanoate" = "PFPeA",
  "N-Ethyl perfluorooctane sulfonamido ethanol" = "NEtFOSE",
  "Perfluorohexanoate" = "PFHxA",
  "2H,2H,3H,3H-Perfluorooctanoate" = "5:3 FTCA",
  "Perfluorododecanoate" = "PFDoA",
  "Perfluoropentanesulfonate" = "PFPeS",
  "Perfluoroundecanoate" = "PFUnA",
  "FtS 4:2 ion" = "4:2 FTS",
  "Perfluoropropionic acid" = "PFPrA",
  "Perfluoroproprionic acid" = "PFPrA",
  "Perfluorotridecanoic acid (PFTrDA)" = "PFTrDA",
  "Perfluoroheptanesulfonic acid (PFHpS)" = "PFHpS", 
  "Perfluorobutanoic acid (PFBA)" = "PFBA",
  "Perfluorobutanesulfonic acid (PFBS)" = "PFBS", 
  "HFPO-DA (GenX)" = "HFPO-DA",
  "9Cl-PF3ONS" = "9ClPF3ONS", 
  "4,8-Dioxa-3H-perfluorononanoic acid (ADONA)" = "ADONA",
  "11Cl-PF3OUdS" = "11ClPF3OUdS", 
  "Perfluoroundecanoic acid (PFUnA)" = "PFUnA",
  "Perfluorotridecanoic acid (PFTriA)" = "PFTrDA", 
  "Perfluorotetradecanoic acid (PFTeA)" = "PFTeDA",
  "Perfluoropentanoic acid (PFPeA)" = "PFPeA", 
  "Perfluoropentanesulfonic acid (PFPeS)" = "PFPeS",
  "Perfluorooctanoic acid (PFOA)" = "PFOA", 
  "Perfluorooctanesulfonic acid (PFOS)" = "PFOS",
  "Perfluorooctanesulfonamide (FOSA)" = "FOSA", 
  "Perfluorononanoic acid (PFNA)" = "PFNA",
  "Perfluorononanesulfonic acid (PFNS)" = "PFNS", 
  "Perfluorohexanoic acid (PFHxA)" = "PFHxA",
  "Perfluorohexanesulfonic acid (PFHxS)" = "PFHxS", 
  "Perfluoroheptanoic acid (PFHpA)" = "PFHpA",
  "Perfluoroheptanesulfonic Acid (PFHpS)" = "PFHpS", 
  "Perfluorododecanoic acid (PFDoA)" = "PFDoA",
  "Perfluorodecanoic acid (PFDA)" = "PFDA", 
  "Perfluorodecanesulfonic acid (PFDS)" = "PFDS",
  "PPF Acid" = "PFPrA", 
  "PFO3OA" = "PFO3OA",
  "PFO2HxA" = "PFO2HxA", 
  "PFMPA" = "PFMPA",
  "GenX (HFPO-DA)" = "HFPO-DA",
  "NEtFOSAA total" = "NEtFOSAA", 
  "NMeFOSAA total" = "NMeFOSAA",
  "PFOS total" = "PFOS", 
  "PFPHxS total" = "PFPHxS",
  "PFHxS total" = "PFHxS",
  "8-2FTS" = "8:2 FTS", 
  "6-2FTS" = "6:2 FTS",
  "4-2FTS" = "4:2 FTS", 
  "GenX" = "HFPO-DA",
  "PFUnDA" = "PFUnA",
  "PFTriA" = "PFTrDA",
  "PFDoDA" = "PFDoA"
)
  # make sure numeric column is numeric 
pfas_conc$result_measure_value <- as.numeric(pfas_conc$result_measure_value)
# STANDARDIZING NAMES using function
pfas_USE <- standardize_pfas_comp(pfas_conc_df = pfas_conc, pfas_dict = pfas_dict)
# Join ecological traits with pfas concentrations 
  # trim species name for joining (Just going to add trophic level to species
  # we have data for, so that we can examine distribution based on trophic level
  # left join needed for pfas_conc to keep variables)
eco_traits <- eco_traits %>% mutate(across(where(is.character), str_trim))
unique(eco_traits$species)
unique(pfas_USE$species)
# merge pfas_conc and eco traits (add habitat, trophic level & merge by species name) 
eco_and_pfas <- pfas_USE %>% 
  left_join(dplyr::select(eco_traits, species, trophic.level, part.of.bay), by = "species")
unique(eco_and_pfas$part.of.bay)
# add eco_group (trophic level & part of bay) 
eco_and_pfas <- eco_and_pfas %>%
  mutate(trophic_bin = cut(trophic.level,
                           breaks = seq(floor(min(trophic.level, na.rm = TRUE)),
                                        ceiling(max(trophic.level, na.rm = TRUE)),
                                        by = 0.49),
                           include.lowest = TRUE,
                           right = FALSE)) %>%
  mutate(trophic_habitat = paste0("TL:", trophic_bin, "H:", part.of.bay))
# factor eco_group
eco_and_pfas$trophic_habitat <- as.factor(eco_and_pfas$trophic_habitat)
levels(eco_and_pfas$trophic_habitat)

# ensure it worked, since there could be differing species names
table(is.na(eco_and_pfas$trophic_habitat))

# view eco & pfas for presentation 
eco_present <- eco_and_pfas %>% 
  dplyr::select(species, trophic.level, trophic_bin, trophic_habitat)
kable(distinct(eco_present))

# Determine common compounds across labs
  # 1. SEE WHICH COMPOUNDS ARE COMMON ACROSS LABS 
compounds_by_lab <- pfas_USE %>%
  distinct(data_origin, standard_name)
  # 2. count number of data origins 
total_labs <- n_distinct(compounds_by_lab$data_origin)
# make a list of the compounds that are detected across each lab 
common_compounds <- compounds_by_lab %>% 
  group_by(standard_name) %>% 
  summarise(n_labs = n_distinct(data_origin), .groups = "drop") %>% 
  filter(n_labs == total_labs) %>% 
  pull(standard_name)
# look at difference in compounds, these should be different from common_compounds
unique(pfas_USE$standard_name)

# Subset pfas_USE into only commonly detected compounds 
pfas_USE_common <- pfas_USE %>%
  filter(standard_name %in% common_compounds)
# went from 3203 observations to 2533 observations 

# Determine which compounds to move forward with based on average detections 
  # visual aid: create a heatmap 
# examine detects of common compounds per species 
compound_species_detects <- pfas_USE_common %>% 
  group_by(species, standard_name) %>%
  summarise(
    n = n(),
    detects = sum(!censored..T.F.),
    prop_censored = sum(censored..T.F.) / n
  )
# ensure dataframe
compound_species_detects <- as.data.frame(compound_species_detects)
# examine detects of common compounds per ecological group 
compound_trophab_detects <- eco_and_pfas %>% 
  group_by(standard_name, trophic_habitat) %>%
  summarise(
    n = n(),
    detects = sum(!censored..T.F.),
    unique_values = length(unique(result_measure_value[!censored..T.F.])),
    prop_censored = sum(censored..T.F.) / n
  )

# Use mean of prop_censored to determine which threshold to use compounds 
  # 1. calculate average
compound_species_detects <- compound_species_detects %>%
  group_by(standard_name) %>%
  mutate(
    mean_prop_censored = mean(prop_censored, na.rm = TRUE),
    n = n_distinct(species)
  ) %>%
  ungroup() %>%
  # Add both mean and n to the compound labels
  mutate(standard_name_label = paste0(
    standard_name, " (mean=", round(mean_prop_censored, 2), ", n=", n, ")"
  )) %>%
  # Sort by mean_prop_censored
  mutate(standard_name_label = fct_reorder(standard_name_label, 
                                           mean_prop_censored, 
                                           .desc = TRUE))

# Plot heatmap
comp_heatmap <- ggplot(compound_species_detects, 
                       aes(x = species, y = standard_name_label, fill = prop_censored)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "white") +
  labs(
    title = "Detects per Compound and Species",
    subtitle = "Compounds sorted by mean proportion censored",
    x = "Species",
    y = "Compound Name (mean, n)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# top compounds based on heatmap, 15 compounds, mean prop censor threshold = 0.75
  # moving forward with these compounds 
top_compounds <- c("PFUnA",
                  "PFOS",
                  "PFDS",
                  "PFTrDA",
                  "PFDA",
                  "PFDoA",
                  "PFNA",
                  "PFTeDA",
                  "PFOA",
                  "6:2 FTS",
                  "4:2 FTS",
                  "PFHpA",
                  "PFHpS",
                  "PFNS",
                  "PFBS",
                  "PFHxS"
                  )

# Final dataset to use
eco_and_pfas_USE <- eco_and_pfas %>%
  filter(standard_name %in% top_compounds)

# output dataset to calculate weighted means 
eco_pfas_final <- write.csv(eco_and_pfas_USE, "data/eco_15pfas_for_calcs.csv")

# Now move to weighted_calcs.R
