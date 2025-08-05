# Author: Jacqueline Carroll
# Date created: 5 August 2025
# Date last updated: 5 August 2025 
# Description: This script calculates weighted mean and standard deviation 
#   for a determinist dose calculation
#   2nd script in dose pipeline 

# list required packages 
required_packages_install <- c("dplyr",
                               "tidyr")
# load required packages 
for (package in required_packages_install) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}


# read in output dataset from eco_group.R
data <- read.csv("data/eco_15pfas_for_calcs.csv") 
# source functions 
source("r_code/functions.R")

# impute for nondetects (just doing MDL/root(2))
data_imputed <- impute_mdl_2(data)
data_imputed$result_imputed <- as.numeric(data_imputed$result_imputed)
# calculate weighted means * THis outputs final_means from functions.R
weighted_mean_sd <- calculate_weighted_means(data_imputed)
final_means <- weighted_mean_sd$final_means
eco_means <- weighted_mean_sd$eco_means

# create missing species tibble for weighted_means calculation function
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
  trophic_habitat = c("TL:[3.49,3.98]H:lower",
                      "TL:[3.49,3.98]H:lower",
                      "TL:[3.49,3.98]H:lower",
                      "TL:[3.49,3.98]H:lower",
                      "TL:[4.49,4.98]H:offshore",
                      "TL:[4.49,4.98]H:offshore",
                      "TL:[4.49,4.98]H:lower",
                      "TL:[4.49,4.98]H:offshore",
                      "TL:[4.49,4.98]H:offshore",
                      "TL:[4.49,4.98]H:lower",
                      "TL:[4.49,4.98]H:lower",
                      "TL:[4.49,4.98]H:offshore"
  ))

# create vector of all compounds 
all_compounds <- unique(final_means$standard_name)

# Expand missing species to all compounds and add eco means
missing_species_expanded <- missing_species %>%
  crossing(standard_name = all_compounds) %>%
  left_join(eco_means, by = c("trophic_habitat")) %>%
  mutate(
    weighted_mean = eco_mean,
    weighted_sd = eco_sd,
    mean_origin = "ecological"
  ) %>%
  dplyr::select(standard_name, species, trophic_habitat, weighted_mean, weighted_sd, mean_origin)

# Final means
final_weighted_means <- final_means %>%
  bind_rows(missing_species_expanded)

# add common name for readability
final_weighted_means <- final_weighted_means %>% 
  left_join(dplyr::select(eco_traits, species, common.name), by = "species")

# output is .csv files to be input in dose_calculation.R
write.csv(final_weighted_means, "data/weighted_means_for_dc.csv", row.names = FALSE)
write.csv(eco_and_pfas, "data/eco_and_pfas_12comps.csv", row.names = FALSE)


# go to dose_calculation.R