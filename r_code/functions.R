# Author: Jacqueline Carroll 
# Description: The purpose of this code is to create functions that will be used
#   in the PFAS Human Exposure project 
# Date created: 5 August 2025
# Date last updated: 5 August 2025

# Function to standardize PFAS names based on above dictionary
# STANDARDIZING NAMES
standardize_pfas_comp <- function(pfas_conc_df, pfas_dict) {
  standardize_pfas <- pfas_conc %>%
    group_by(species, sample_ID, compound) %>%
    # Apply dictionary (this will be in script, different dictionaries will apply)
    mutate(
      standard_name = ifelse(
        compound %in% names(pfas_dict),
        pfas_dict[compound],
        compound
      )) %>%
    return(standardize_pfas)
}

# Function to impute MDL/root(2) for non-detects
impute_mdl_2 <- function(data) {
  data_imputed <- data %>% 
    mutate(result_imputed = ifelse(
      censored..T.F., # T/F censored column 
      result_detection / sqrt(2), # result_detection = MDL 
      result_measure_value # value
    ))
  return(data_imputed)
}

# Function to calculate weighted mean and standard deviation 
calculate_weighted_means <- function(data, missing_species = NULL) {
  # 1. collapse df to sample level data
  sample_level <- data_imputed %>%
    group_by(species, standard_name, sample_ID) %>%
    summarise(
      sample_mean = sum(result_imputed * sample_size..for.composites., na.rm = TRUE) /
        sum(sample_size..for.composites., na.rm = TRUE),
      total_weight = sum(sample_size..for.composites., na.rm = TRUE),
      trophic_habitat = first(trophic_habitat)
    )
  # 2. calculate species level means 
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
  # 3. calculate ecological group level means 
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
  # 4. Merge species & ecological groups, create column for origin of weighted value
  final_means <- species_means %>%
    left_join(eco_means, by = c("trophic_habitat", "standard_name")) %>%
    mutate(
      weighted_mean = ifelse(distinct_samples > 5, species_mean, eco_mean),
      weighted_sd = ifelse(distinct_samples > 5, species_sd, eco_sd),
      mean_origin = ifelse(distinct_samples > 5, "species", "ecological")
    ) %>%
    dplyr::select(standard_name, species, trophic_habitat, 
                  weighted_mean, weighted_sd, mean_origin, 
                  distinct_samples, total_n, distinct_samples_eco, total_n_eco,
                  eco_mean, eco_sd)
  return(list(
    final_means = final_means,
    eco_means = eco_means))
}


# Dose calculation function 
calculate_dose <- function(survey, weighted_means_species, 
                           portion_col = "wild_caught_finfish_typical_meal_size", 
                           bw_col = "bodyweight") {
  
  # ID scaled columns (created above)
  sf_cols <- grep("_sf$", names(survey), value = TRUE)
  
  # Make survey long format 
  survey_long <- survey %>%
    pivot_longer(
      cols = all_of(sf_cols),
      names_to = "common.name",
      values_to = "scaled_frequency"
    ) %>%
    mutate(
      common.name = gsub("_sf$", "", common.name),
      common.name = gsub("\\.\\.\\d+$", "", common.name),  # remove ...40 etc.
      common.name = gsub("_", " ", common.name),
      common.name = gsub("\\.$", "", common.name),         # remove trailing dot
      common.name = trimws(tolower(common.name))
    )
  
  # Standardize common names -- make lowercase & trim white space 
  weighted_means_species <- weighted_means_species %>%
    mutate(common.name = trimws(tolower(common.name)))
  
  # Warning in case some species are unmatched 
  unmatched_species <- setdiff(unique(survey_long$common.name), 
                               unique(weighted_means_species$common.name))
  if (length(unmatched_species) > 0) {
    warning(
      "The following species from survey have no match in weighted_means_species: ",
      paste(unmatched_species, collapse = ", ")
    )
  }
  
  # Join survey & weighted means 
  dose_data <- survey_long %>%
    left_join(
      weighted_means_species %>% dplyr::select(common.name, standard_name, weighted_mean),
      by = "common.name"
    )
  
  # Actual dose calculation per SPECIES & COMPOUND per individual (human subject)
  dose_data <- dose_data %>%
    mutate(dose_species = (scaled_frequency * .data[[portion_col]] * weighted_mean) / .data[[bw_col]]) #convert weighted means from ug to ng 
  
  dose_by_compound <- dose_data %>% 
    group_by(response_ID, standard_name) %>% 
    summarise(
      dose = sum(dose_species, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Sum species_dose into total_dose 
  total_dose <- dose_data %>%
    group_by(response_ID) %>%
    summarise(
      total_dose = sum(dose_species, na.rm = TRUE),
      N = sum(!is.na(scaled_frequency) & scaled_frequency > 0),
      .groups = "drop"
    )
  
  return(list(
    dose_data = dose_data,   # dose per species
    total_dose = total_dose,  # summed dose per individual
    compound_dose = dose_by_compound # dose for each compound 
  ))
}




