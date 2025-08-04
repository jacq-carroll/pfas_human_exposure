# Author: Jacquie Carroll
# Description: Ecological grouping, grouping of the dataset as well 
# date created: 15 Jul 2025 
# date last updated: 4 Aug 2025 

library(dplyr)
library(tidyr)
library(stringr)
library(knitr)
library(ggplot2)
library(NADA)
library(purrr)
library(forcats)
# read in dataset and group it by species, compound, etc. 
pfas_conc <- read.csv("data/pfas_data_master.csv")
names(pfas)

#trim each character column 
pfas_conc <- pfas_conc %>% 
  mutate(across(where(is.character), str_trim))

# sample size for each, IMPORTANT SUMMARY STATS OF DATASET  
df_total_sample_size <- pfas_conc %>%
  mutate(common_name = str_trim(common_name)) %>% 
  mutate(sample_ID = str_trim(sample_ID)) %>%     
  distinct(common_name, sample_ID, sample_size..for.composites.) %>%  # remove repeats due to compound
  group_by(common_name) %>%
  summarise(total_individuals = sum(sample_size..for.composites.),
            total_samples = n_distinct(sample_ID))

# importance based on survey data 
species_importance <- as.data.frame(read.csv("data/importance.csv"))
species_importance <- species_importance %>%
  mutate(across(where(is.character), str_trim))
species_importance <- species_importance[order(-species_importance$importance),]

merge_sample_importance <- inner_join(df_total_sample_size, species_importance,
                                      by = c("common_name" = "Species"))

ordered_importance <- merge_sample_importance[order(-merge_sample_importance$importance),]
View(ordered_importance)

donthave <- anti_join(species_importance, df_total_sample_size,
                      by = c("Species" = "common_name"))

# what proportion of our data is censored?
censor_proportion <- as.data.frame(table(pfas_conc$censored..T.F.))
censor_proportion_table <- censor_proportion %>% 
  mutate(sum = sum(Freq)) %>% 
  mutate(proportion = Freq /sum)

kable(censor_proportion_table)

# Need to standardize compound names
pfas_list <- as.vector(unique(c(pfas_conc$compound)))
# create dictionary for abbreviations 
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
# STANDARDIZING NAMES, TOTALING LINEAR AND BRANCHED 
clean_pfas_conc <- function(pfas_conc, pfas_dict) {
  
  pfas_conc_clean <- pfas_conc %>%
    # Identify linear/branched
    mutate(
      is_LB = str_detect(compound, "^(?i)(l-|br-)"),
      compound_clean = str_replace(compound, "^(?i)(l-|br-)", "")
    ) %>%
    
    group_by(species, sample_ID, compound_clean) %>%
    
    summarise(
      across(-c(compound, result_measure_value, is_LB), first), # keep metadata
      total_val = if (any(!is_LB)) {
        sum(result_measure_value[!is_LB], na.rm = TRUE)
      } else {
        sum(result_measure_value, na.rm = TRUE)
      },
      total_origin = if (any(!is_LB)) "total" else "summed_LB",
      .groups = "drop"
    ) %>%
    
    # Apply dictionary
    mutate(
      standard_name = ifelse(
        compound_clean %in% names(pfas_dict),
        pfas_dict[compound_clean],
        compound_clean
      )) %>%
    
    rename(result_measure_value = total_val)
  
  return(pfas_conc_clean)
}
pfas_USE <- clean_pfas_conc(pfas_conc = pfas_conc, pfas_dict = pfas_dict)

# check unique names & ensure there are no duplicates 
kable(unique(pfas_USE$standard_name))

# read in ecological trait sheet 
eco_traits <- read.csv("data/eco_traits.csv")
# trim species name for joining (Just going to add trophic level to species
# we have data for, so that we can examine distribution based on trophic level
# left join needed for pfas_conc to keep )
eco_traits <- eco_traits %>% mutate(across(where(is.character), str_trim))
# examine unique species 
unique(pfas_conc$species)
unique(eco_traits$species)
# merge pfas_conc and eco traits (add habitat, trophic level & merge by species name) 
eco_and_pfas <- pfas_USE_final %>% 
  left_join(dplyr::select(eco_traits, species, trophic.level, part.of.bay), by = "species")

unique(eco_and_pfas$standard_name)

unique(eco_traits$species)
unique(pfas_conc$species)

levels(eco_and_pfas$trophic_habitat)

# add eco group (trophic level & part of bay) 
eco_and_pfas <- eco_and_pfas %>%
  mutate(trophic_bin = cut(trophic.level,
                           breaks = seq(floor(min(trophic.level, na.rm = TRUE)),
                                        ceiling(max(trophic.level, na.rm = TRUE)),
                                        by = 0.49),
                           include.lowest = TRUE,
                           right = FALSE)) %>%
  mutate(trophic_habitat = paste0("TL:", trophic_bin, "H:", part.of.bay))

eco_and_pfas$trophic_habitat <- as.factor(eco_and_pfas$trophic_habitat)
levels(eco_and_pfas$trophic_habitat)

# ensure it worked, since there could be differing species names
# hard clam can be na, blue crab can be na
table(is.na(eco_and_pfas$trophic_habitat))

# view eco & pfas for presentation 
eco_present <- eco_and_pfas %>% 
  dplyr::select(species, trophic.level, trophic_bin, trophic_habitat)
kable(distinct(eco_present))

# FIRST STEP: SEE WHICH COMPOUNDS ARE COMMON ACROSS LABS 
compounds_by_lab <- pfas_USE %>%
  distinct(data_origin, standard_name)

total_labs <- n_distinct(compounds_by_lab$data_origin)
# make a list of the compounds that are detected across each lab 
common_compounds <- compounds_by_lab %>% 
  group_by(standard_name) %>% 
  summarise(n_labs = n_distinct(data_origin), .groups = "drop") %>% 
  filter(n_labs == total_labs) %>% 
  pull(standard_name)
# look at difference in compounds
unique(pfas_USE$standard_name)

# Subset pfas_USE into only commonly detected compounds 
pfas_USE_common <- pfas_USE %>%
  filter(standard_name %in% common_compounds)
# went from 3203 observations to 2533 observations 

# examine detects of common compounds per species 
compound_species_detects <- pfas_USE_common %>% 
  group_by(species, standard_name) %>%
  summarise(
    n = n(),
    detects = sum(!censored..T.F.),
    prop_censored = sum(censored..T.F.) / n
  )

# prop censored per trophic group 
kable(compund_detects)
compound_trophab_detects <- eco_and_pfas %>% 
  group_by(standard_name, trophic_habitat) %>%
  summarise(
    n = n(),
    detects = sum(!censored..T.F.),
    unique_values = length(unique(result_measure_value[!censored..T.F.])),
    prop_censored = sum(censored..T.F.) / n
  )


# make sure it is a dataframe 
compound_species_detects <- as.data.frame(compound_species_detects)

# use mean of prop_censored to determine which threshold to use compounds 

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
  mutate(standard_name_label = fct_reorder(standard_name_label, mean_prop_censored, .desc = TRUE))

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
                  "PFHpA",
                  "PFHpS",
                  "9ClPF3ONS",
                  "11ClPF3OUdS",
                  "PFHxS"
                  )
top_compounds <- c("PFOS")

pfas_USE_final <- pfas_USE %>%
  filter(standard_name %in% top_compounds)






# proportions of censored data 
# trophic & habitat 
eco_and_pfas %>% 
  filter(!is.na(trophic_habitat)) %>% # filter based on na in trophic level 
  group_by(trophic_habitat, censored..T.F.) %>% 
  summarise(n = n()) %>% 
  group_by(trophic_habitat) %>% 
  mutate(
    total = sum(n),
    proportion = n / total 
  )
# by species 
species_censor_prop <- eco_and_pfas %>% 
  filter(!is.na(species)) %>% # filter based on na in trophic level 
  group_by(species, censored..T.F.) %>% 
  summarise(n = n()) %>% 
  group_by(species) %>% 
  mutate(
    total = sum(n),
    proportion = n / total 
  )

# see sample size for each trophic level 
sample_size_per_trophic <- eco_and_pfas %>%
  filter(!is.na(trophic_habitat)) %>%
  dplyr::select(sample_ID, trophic_habitat, sample_size..for.composites.) %>%   # Adjust column names as needed
  distinct() %>%   # Remove duplicate rows for the same sample_id + trophic_habitat
  group_by(trophic_habitat) %>%
  summarise(
    total_sample_size = sum(sample_size..for.composites., na.rm = TRUE),
    n_unique_samples = n()
  ) %>%
  arrange(desc(total_sample_size))
kable(sample_size_per_trophic)

print(sample_size_per_trophic)


# begin nada analysis on compounds # let's start with group by trophic level due to censor 

eco_and_pfas$censored..T.F. <- as.logical(eco_and_pfas$censored..T.F.)
eco_and_pfas$result_measure_value <- as.numeric(eco_and_pfas$result_measure_value)
eco_and_pfas$result_detection <- as.numeric(eco_and_pfas$result_detection)


can_fit <- sum(!eco_and_pfas$censored..T.F., na.rm = TRUE) > 0

# could not get nada analysis to work, or atleast taking too much time to get it right 
#   due to not enough data 
#now just doing MDL/root 2 imputation 

calculate_weighted_means <- function(data, missing_species = NULL) {
  # Step 1: Impute censored values using MDL / sqrt(2)
  data_imputed <- data %>%
    mutate(
      result_imputed = ifelse(
        censored..T.F.,
        result_detection / sqrt(2),
        result_measure_value
      )
    )
  
  # Step 2: Collapse to sample-level data
  sample_level <- data_imputed %>%
    group_by(species, standard_name, sample_ID) %>%
    summarise(
      sample_mean = sum(result_imputed * sample_size..for.composites., na.rm = TRUE) /
        sum(sample_size..for.composites., na.rm = TRUE),
      total_weight = sum(sample_size..for.composites., na.rm = TRUE),
      trophic_habitat = first(trophic_habitat)
    )
  
  # Species-level mean and SD
  species_means <- sample_level %>%
    group_by(species, standard_name) %>%
    summarise(
      distinct_samples = n(),
      total_n = sum(total_weight, na.rm = TRUE),
      species_mean = sum(sample_mean * total_weight, na.rm = TRUE) /
        sum(total_weight, na.rm = TRUE),
      species_sd = sqrt(
        sum(total_weight * 
              (sample_mean -
                 sum(sample_mean * total_weight, na.rm = TRUE) /
                 sum(total_weight, na.rm = TRUE))^2, na.rm = TRUE) /
          (distinct_samples - 1)
      ),
      trophic_habitat = first(trophic_habitat)
    )
  
  # Step 4: Ecological-level means
  # apply same weighted mean rule, but to entire group 
  eco_means <- sample_level %>%
    group_by(trophic_habitat, standard_name) %>%
    summarise(
      distinct_samples_eco = n(),
      total_n_eco = sum(total_weight, na.rm = TRUE),
      eco_mean = sum(sample_mean * total_weight, na.rm = TRUE) /
        sum(total_weight, na.rm = TRUE),
      eco_sd = sqrt(
        sum(total_weight * 
              (sample_mean -
                 sum(sample_mean * total_weight, na.rm = TRUE) /
                 sum(total_weight, na.rm = TRUE))^2, na.rm = TRUE) /
          (distinct_samples_eco - 1)),
      eco_species = n(),
      .groups = "drop"
    )
  # Step 5: Merge species and ecological, apply fallback rule
  final_means <- species_means %>%
    left_join(eco_means, by = c("trophic_habitat", "standard_name")) %>%
    mutate(
      weighted_mean = ifelse(distinct_samples > 5, species_mean, eco_mean),
      weighted_sd = ifelse(distinct_samples > 5, species_sd, eco_sd),
      mean_origin = ifelse(distinct_samples > 5, "species", "ecological")
    ) %>%
    dplyr::select(standard_name, species, trophic_habitat, 
           weighted_mean, weighted_sd, mean_origin, 
           distinct_samples, total_n, distinct_samples_eco, total_n_eco)
  
  # Step 6: Add missing species using ecological means
  if (!is.null(missing_species)) {
    all_compounds <- unique(eco_means$standard_name)
    
    missing_species_expanded <- missing_species %>%
      crossing(standard_name = all_compounds) %>%
      left_join(eco_means, by = c("trophic_habitat", "standard_name")) %>%
      mutate(
        weighted_mean = eco_mean,
        weighted_sd = eco_sd,
        mean_origin = "ecological",
        distinct_samples = 0,
        total_n = 0
      ) %>%
      dplyr::select(standard_name, species, trophic_habitat, 
             weighted_mean, weighted_sd, mean_origin, 
             distinct_samples, total_n, eco_mean, eco_sd)
    
    final_means <- bind_rows(final_means, missing_species_expanded)
  }
  
  return(final_means) 
}
weighted_mean_sd <- calculate_weighted_means(eco_and_pfas)

# so now add data to species with missing data
missing_species <- tibble(
  species = c("Tautoga onitis",
              "Archosargus probatocephalus",
              "Pseudopleuronectes americanus",
              "Stenotomus chrysops",
              "Lopholatilus chamaeleonticeps",
              "Lutjanus campechanus",
              "Cynoscion nebulosus",
              "Coryphaena hippurus",
              "Thunnus albacares",
              "Trichiurus lepturus",
              "Paralichtyhs dentatus",
              "Balistes capriscus"
              ),
  trophic_habitat = c("TL:[3.47,3.96)H:lower",
              "TL:[3.47,3.96)H:lower",
              "TL:[3.47,3.96)H:lower",
              "TL:[3.47,3.96)H:lower",
              "TL:[4.45,4.94]H:lower",
              "TL:[4.45,4.94]H:lower",
              "TL:[4.45,4.94]H:lower",
              "TL:[4.45,4.94]H:lower",
              "TL:[4.45,4.94]H:lower",
              "TL:[4.45,4.94]H:lower",
              "TL:[4.45,4.94]H:lower",
              "TL:[4.45,4.94]H:lower"
))



# All compounds from ecological dataset
all_compounds <- unique(weighted_means_ecological$standard_name)

# Create all species × compounds combinations
missing_species_expanded <- missing_species %>%
  crossing(standard_name = all_compounds) %>%
  left_join(weighted_means_ecological, by = c("trophic_habitat", "standard_name")) %>%
  mutate(
    weighted_mean = eco_mean,
    mean_origin = "ecological"
  ) %>%
  dplyr::select(standard_name, species, trophic_habitat, weighted_mean, mean_origin)

final_weighted_means <- weighted_mean_sd %>%
  bind_rows(missing_species_expanded)

# add common name 
final_weighted_means <- final_weighted_means %>% 
  left_join(dplyr::select(eco_traits, species, common.name), by = "species")

unique(final_weighted_means$species)
unique(eco_traits$species)

table(is.na(final_weighted_means$trophic_habitat))

write.csv(final_weighted_means, "data/weighted_means_for_dc.csv", row.names = FALSE)
write.csv(eco_and_pfas, "data/eco_and_pfas_12comps.csv", row.names = FALSE)


eco_and_pfas$trophic_habitat
unique(eco_and_pfas$units)

# plot codes I made that I no longer need 
# let's do histograms instead of density plots 
eco_clean$result_measure_value <- as.numeric(eco_clean$result_measure_value)

hist_troph_hab <- ggplot(eco_clean, aes(x = result_measure_value, fill = trophic_habitat)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +  # bins can be adjusted
  facet_wrap(~ standard_name, scales = "free", drop = TRUE) +
  labs(title = "Detected PFAS Distributions by Trophic Level and Habitat (Top 25 Compounds)",
       fill = "Trophic + Habitat",
       x = "Concentration",
       y = "Count") +
  theme_minimal()

hist_species_dist_6 <- ggplot(eco_clean, aes(x = result_measure_value, fill = species)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +  # bins can be adjusted
  facet_wrap(~ standard_name, scales = "free", drop = TRUE) +
  labs(title = "Detected PFAS Distributions by Species (Top 25 Compounds)",
       fill = "Species",
       x = "Concentration",
       y = "Count") +
  theme_minimal()

hist_trophic_dist_6 <- ggplot(eco_clean, aes(x = result_measure_value, fill = trophic_bin)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +  # bins can be adjusted
  facet_wrap(~ standard_name, scales = "free", drop = TRUE) +
  labs(title = "Detected PFAS Distributions by Trophic Level (Top 25 Compounds)",
       fill = "Trophic Level",
       x = "Concentration",
       y = "Count") +
  theme_minimal()

# do box plots 
box_trophic <- ggplot(eco_clean, aes(x = trophic_habitat, y = result_measure_value, fill = trophic_habitat)) +
  geom_boxplot() +
  facet_wrap(~ standard_name, scales = "free", drop = TRUE) +
  labs(title = "PFAS by Trophic Level and Habitat") +
  theme_minimal()
box_species <- ggplot(eco_clean, aes(x = species, y = result_measure_value, fill = species)) +
  geom_boxplot() +
  facet_wrap(~ standard_name, scales = "free", drop = TRUE) +
  labs(title = "PFAS by Species") +
  theme_minimal()

# box plot for detects for each compound per species 
compound_species_detects <- as.data.frame(compound_species_detects)
comp_species_boxplot <- ggplot(compound_species_detects, aes(x = standard_name,
                                                             y = detects,
                                                             fill = standard_name)) +
  geom_boxplot() + 
  facet_wrap(~ species, scales = "fixed") +
  labs(title = "Number of detects per species") +
  xlab("Compound") +
  ylab("Number of detects") + 
  theme(legend.position = "none")

comp_species_boxplot <- ggplot(compound_species_detects, 
                               aes(x = standard_name, y = detects, fill = standard_name)) + 
  geom_boxplot() + 
  facet_wrap(~ species, ncol = 1, scales = "fixed") +
  labs(title = "Number of detects per species") +
  xlab("Compound") + 
  ylab("Number of detects") +
  coord_flip() +    # Horizontal orientation
  theme(legend.position = "none")

