# author: Jacqueline Carroll
# date created: 27 July 2025
# description: the purpose of this script is to do a determinist dose calculation

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

# source function script 
source("r_code/functions.R")
# read in output files from weighted_calcs.R & survey data
species_means <- read.csv("data/weighted_means_for_dc.csv")

all_pfas <- read.csv("data/eco_and_pfas_12comps.csv")

survey <- read.csv("data/survey_use.csv")

# in the clean_qualtrics_data the weekly numbers are still weekly ie: 1, 2, 3, less than and greater than 
#   did work 
all_numeric_columns <- c("bodyweight",
                         "atlantic_croaker",
                         "black_drum",
                         "black_sea_bass",
                         "blue_crab",
                         "blue_crab_quantity",
                         "blue_crab_size",
                         "bluefish",
                         "blue_catfish",
                         "channel_catfish",
                         "cobia",
                         "dolphinfish...40",
                         "hard_clam",
                         "hard_clam_quantity",
                         "northern_kingfish",
                         "ribbonfish",
                         "scup",
                         "sheepshead",
                         "smooth_dogfish",
                         "spot",
                         "spotted_seatrout",
                         "striped_bass",
                         "summer_flounder",
                         "tautog",
                         "tilefish",
                         "triggerfish",
                         "weakfish",
                         "white_perch",
                         "winter_flounder",
                         "yellowfin_tuna",
                         "other...62",
                         "scallop",
                         "flounder",
                         "swordfish",
                         "halibut",
                         "arctic_char",
                         "dolphinfish...84",
                         "lobster",
                         "mussel",
                         "sardine",
                         "red_snapper",
                         "trout",
                         "shrimp",
                         "shrimp_quantity",
                         "salmon",
                         "tuna",
                         "tilapia",
                         "alaska_pollock",
                         "cod",
                         "crab",
                         "crab_quantity",
                         "catfish",
                         "clam",
                         "clam_quantity",
                         "oyster...105",
                         "oyster_quantity",
                         "other...108")
# make all of these characters 
survey <- survey %>%
  mutate(across(all_of(all_numeric_columns), as.character))

# redo this for some reason the cleaning script did not UGH 
survey <- survey %>% 
  mutate(across(all_of(all_numeric_columns), ~ case_when(
    . == "less than 1 per week" ~ 0.5/7,
    . == "greater than 7" ~ 10/7,
    . == "1" ~ 1/7,
    . == "2" ~ 2/7,
    . == "3" ~ 3/7,
    . == "4" ~ 4/7,
    . == "5" ~ 5/7,
    . == "6" ~ 6/7,
    . == "7" ~ 7/7,
    TRUE ~ suppressWarnings(as.numeric(.))
  )))

# create total wild caught fish consumption column 
wc_columns <- c(
  "atlantic_croaker", "black_drum", "black_sea_bass", "blue_crab", "bluefish",
  "blue_catfish", "channel_catfish", "cobia", "dolphinfish...40", "hard_clam",
  "northern_kingfish", "ribbonfish", "scup", "sheepshead", "smooth_dogfish",
  "spot", "spotted_seatrout", "striped_bass", "summer_flounder", "tautog",
  "tilefish", "triggerfish", "weakfish", "white_perch", "winter_flounder",
  "yellowfin_tuna", "other...62"
)

# create column for how often subjects eat each species per day
survey <- survey %>%
  rowwise() %>%
  mutate(total_wc = sum(c_across(all_of(wc_columns)), na.rm = TRUE)) %>%
  ungroup()

# Scaled-species specific consumption calculation, need to make sure all are here 
  # self reported meal frequency divided by the total number of species 
survey <- survey %>%
  mutate(across(all_of(wc_columns),
                ~ (.x * sr_meal_freq_wc) / total_wc, # .x = species specific, sr_meal_freq = self reported total, total_wc = actual total 
                .names = "{.col}_sf"))

# time for dose calculation 
# calculate dose on filtered surveys
# pilot data removed
# filter survey data to remove before 8/4/24 (want 8/3 removed)
survey_pilot_rmvd <- survey %>% 
  filter(survey$date > "2024-08-04") # anything after Aug 4, 2024
# filter based on part of bay, to do  

# calculate dose 
dose_prof_pilot_rmvd <- calculate_dose(survey = survey_pilot_rmvd, weighted_means_species = species_means)
# create dataframe of profile for visualization 
exp_prof <- as.data.frame(dose_prof_pilot_rmvd$total_dose$total_dose)

comp_dose <- as.data.frame(dose_prof_pilot_rmvd$compound_dose)
# range
range(exp_prof)
# plot distribution of exposure profile 
# PLOT OF TOTAL DOSE 
exposure_profile_determinist <- ggplot(exp_prof, aes(x = dose_prof_pilot_rmvd$total_dose$total_dose)) +
  geom_histogram(fill = "royalblue4") +
  labs(
    x = "Sum PFAS"
  ) + 
  theme_minimal() + 
  theme(axis.title.x = element_text(face = "bold"))
  

# to look at intermediate calculations: 
write.csv(dose_prof_pilot_rmvd$dose_data, "data/dose_profile.csv", row.names = FALSE)
write.csv(dose_prof_pilot_rmvd$total_dose, "data/total_dose.csv", row.names = FALSE)
write.csv(dose_prof_pilot_rmvd$compound_dose, "data/compound_dose.csv", row.names = FALSE)

# debugging for when species were not matching, keeping for reference 
unique_species_survey <- sort(unique(gsub("_sf$", "", grep("_sf$", names(survey), value = TRUE))))
unique_species_means <- sort(unique(tolower(species_means$species)))

print(unique_species_survey)
print(unique_species_means)

# Plots per compound! 
dose_comp_plots <- ggplot(comp_dose, aes(x = dose)) +
  geom_histogram(fill = "royalblue4") + 
  facet_wrap(~ standard_name, scales = "free") +
  labs(
    title = "Exposure Profile by Compound",
    x = "Dose (ng/kg-day)",
    y = "Count"
  ) +
  theme_minimal() + 
  theme(strip.text = element_text(face = "bold"))



