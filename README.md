Making edit to test version control 
# Human exposure survey data analysis

The purpose of this project is to analyze the fish consumption survey data for each season. Points of interest are demographic information, species consumed & self reported frequency. Data is manually cleaned in excel, further cleaned in R, and utilized in exploratory data analysis, as well as statistical comparisons across seasons.

## Table of contents

-   Installation
-   Usage
-   Data description
-   Data dictionary

## Installation

Packages required:

-   dplyr

-   stringr

## Usage

-   run /clean_qualtrics_data.R in terminal via: clean_qualtrics_data.r input_file to process dataset for further analysis

-   exploratory data analysis: survey_EDA_season2.Rmd (not executable)

    -   visualize and compare

-   nada_exploration.R

    -   explore non detect methods

-   ises2025.R

    -   Develop visuals & run analysis for the purposes of presentation at ISES 2025

        -   Map visual

        -   dose calculation

        -   dose simulations

    -   The data compiled & used in this script is below catch limit, Li Lab data is only bivalve MDL, some of these fillets are skin on & pan dressed

-   fish_map.R

    -   create map of location of each compiled datapoint

-   Dose calculation pipeline:

    -   start with: eco_group.R

        -   Prep compiled data to be ready for calculation

            -   PFAS standard name library

            -   joining ecological traits with pfas concentration data

            -   group the species ecologically (to use when species specific data sample size is insufficient)

            -   calculate weighted means for compoundxspecies & compoundxecological group

        -   output:

            -   csv file of weighted means (for each species)

                -   tells you origin of weighted mean (ie: species or ecological group)

            -   csv file of eco traits and pfas combined (dataset weighted means are calculated from)

    -   Move onto dose_calculation.R

        -   create function for dose calculation

        -   any cleaning steps that still need to be done (for various reasons)

        -   calculate other parameters:

            -   species specific scaled frequency

        -   calculate dose based on different datasets/scenarios (survey)

            -   like removing pilot data from survey

            -   subset of demographics

        -   output:

            -   dose_profile.csv: this is the complete dose profile, you can see all of the intermediate values used to calculate dose.

                -   USE this dataset for your manual calculations

            -   total_dose.csv: this has response_ID and total_dose

                -   USE this dataset for examining exposure profile for summed

            -   compound_dose.csv: this has response_id and dose for each compound

                -   USE this dataset for examining exposure profile for specific compounds

## Data description

-   raw data:

    -   season 1:

        -   qualtrics_data/labels/in_person_original.xlsx

        -   qualtrics_data/labels/online_only_original.xlsx

    -   season: 2

        -   qualtrics_data/labels/season_2_inperson_labels_original.xlsx

        -   qualtrics_data/labels/season_2_online_labels_original.xlsx

-   manually cleaned excel data (input file for clean_qualtrics_data.r): /qualtrics_data/labels/seasons_combined_master.csv

-   cleaned output: r_files/seasons_combined_output_processed.csv

## Data dictionary

+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Column name                            | type             | description                                                                                                                                                      |
+========================================+==================+==================================================================================================================================================================+
| user_language                          | chr              | the language the subject used to complete the survey                                                                                                             |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| ip_online                              | chr              | indication of whether the survey was completed in person or online                                                                                               |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| finished                               | logi: TRUE/FALSE | metadata of whether the response is finished. this is used to filter out any incomplete responses                                                                |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| have_you_taken_fish_consumption_survey | chr              | indication of whether the subject has previously taken a fish consumption survey that season. This is used to filter out any unqualified responses.              |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| do_you_consume_fish_from_bays          | chr              | indication of whether the subject does currently fish from the DE bays. this is used to filter out any unqualified responses                                     |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| area_have_you_consumed                 | chr              | areas the subject has fished (check all that apply):                                                                                                             |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   upper bay (new castle county)                                                                                                                                |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   middle bay (kent county)                                                                                                                                     |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   lower bay (sussex county)                                                                                                                                    |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   inland bays (rehoboth bay and/or indian river bay)                                                                                                           |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| gender                                 | chr              | gender indication:                                                                                                                                               |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   man                                                                                                                                                          |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   woman                                                                                                                                                        |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   non-binary/third gender                                                                                                                                      |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   prefer not to answer                                                                                                                                         |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| race                                   | chr              | race indication (check all that apply):                                                                                                                          |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   white                                                                                                                                                        |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   black or african american                                                                                                                                    |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   hispanic or latino                                                                                                                                           |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   asian                                                                                                                                                        |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   american indian or alaska native                                                                                                                             |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   middle eastern or north african                                                                                                                              |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   native hawaiian or pacific islander                                                                                                                          |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   other                                                                                                                                                        |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| age                                    | chr, factor      | age range indication:                                                                                                                                            |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   18-24                                                                                                                                                        |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   25-34                                                                                                                                                        |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   35-44                                                                                                                                                        |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   45-54                                                                                                                                                        |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   55-64                                                                                                                                                        |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   65 and over                                                                                                                                                  |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   prefer not to answer                                                                                                                                         |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| education                              | chr, factor      | education indication:                                                                                                                                            |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   less than high school diploma                                                                                                                                |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   high school graduate or GED                                                                                                                                  |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   associate's degree                                                                                                                                           |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   vocational school                                                                                                                                            |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   bachelor's degree                                                                                                                                            |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   master's degree                                                                                                                                              |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   doctorate                                                                                                                                                    |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   prefer not to answer                                                                                                                                         |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| income                                 | chr, factor      | income range indication:                                                                                                                                         |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   less than \$30,000                                                                                                                                           |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   \$30,000-\$59,999                                                                                                                                            |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   \$60,000-\$89,999                                                                                                                                            |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   \$90,000-\$119,999                                                                                                                                           |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   \$120,000-\$149,999                                                                                                                                          |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   greater than \$150,000                                                                                                                                       |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   prefer not to answer                                                                                                                                         |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| bodyweight_unit                        | chr              | indication of whether the subject filled their bodyweight out in pounds or kilograms                                                                             |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| bodyweight                             | num              | self reported bodyweight (converted to kg in processed data)                                                                                                     |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| wild_caught_finfish_typical_meal_size  | num              | self reported typical portion size of wild caught fish. converted to grams in processed data                                                                     |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| part_of_fish                           | chr              | part of fish the subject typically eats (check all that apply):                                                                                                  |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   fillet                                                                                                                                                       |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   whole fish (gutted)                                                                                                                                          |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   whole fish (not gutted)                                                                                                                                      |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   other (please explain)                                                                                                                                       |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   not applicable. I have only consumed shellfish                                                                                                               |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| wc_species                             | chr              | list of species the subject has consumed within the last month                                                                                                   |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| atlantic_croaker                       | num              | weekly consumption frequency of atlantic croaker                                                                                                                 |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| black_drum                             | num              | weekly consumption frequency of black drum                                                                                                                       |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| black_sea_bass                         | num              | weekly consumption frequency of black sea bass                                                                                                                   |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| blue_catfish                           | num              | weekly consumption frequency of blue catfish                                                                                                                     |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| blue_crab                              | num              | weekly consumption frequency of blue crab                                                                                                                        |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| blue_crab_quantity                     | num              | number of blue crabs usually eaten                                                                                                                               |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| blue_crab_size                         | num              | size of the blue crabs that were eaten                                                                                                                           |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| bluefish                               | num              | weekly consumption frequency of bluefish                                                                                                                         |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| channel_catfish                        | num              | weekly consumption frequency of channel catfish                                                                                                                  |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| cobia                                  | num              | weekly consumption frequency of cobia                                                                                                                            |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| dolphinfish...40                       | num              | weekly consumption frequency of wild caught dolphinfish                                                                                                          |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| hard_clam                              | num              | weekly consumption frequency of hard clams                                                                                                                       |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| hard_clam_quantity                     | num              | amount of hard clams usually eaten                                                                                                                               |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| hard_clam_raw_cooked                   | chr              | indication of if the subject eats the clams raw or cooked or both                                                                                                |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| northern_kingfish                      | num              | weekly consumption frequency of northern kingfish                                                                                                                |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| oyster...45                            | num              | weekly consumption frequency of wild caught oysters (the only indication has been during a time when it is illegal to eat oysters so not sure we should include) |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| ribbonfish                             | num              | weekly consumption frequency of ribbonfish                                                                                                                       |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| scup                                   | num              | weekly consumption frequency of scup                                                                                                                             |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| sheepshead                             | num              | weekly consumption frequency of sheepshead                                                                                                                       |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| smooth_dogfish                         | num              | weekly consumption frequency of smooth dogfish                                                                                                                   |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| spot                                   | num              | weekly consumption frequency of spot                                                                                                                             |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| spotted_seatrout                       | num              | weekly consumption frequency of spotted seatrout                                                                                                                 |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| striped_bass                           | num              | weekly consumption frequency of striped bass                                                                                                                     |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| summer_flounder                        | num              | weekly consumption frequency of summer flounder                                                                                                                  |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| tautog                                 | num              | weekly consumption frequency of tautog                                                                                                                           |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| tilefish                               | num              | weekly consumption frequency of tilefish                                                                                                                         |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| triggerfish                            | num              | weekly consumption frequency of triggerfish                                                                                                                      |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| weakfish                               | num              | weekly consumption frequency of weakfish                                                                                                                         |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| white_perch                            | num              | weekly consumption frequency of white perch                                                                                                                      |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| winter_flounder                        | num              | weekly consumption frequency of winter flounder                                                                                                                  |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| yellowfin_tuna                         | num              | weekly consumption frequency of yellowfin tuna                                                                                                                   |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| yellowfin_tuna_raw_cooked              | chr              | indication of whether the subject ate the tuna raw, cooked or both                                                                                               |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| other...62                             | num              | weekly consumption frequency of species marked "other"                                                                                                           |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| have_you_consumed_store_bought_fish    | chr              | indication of whether the subject consumes store bought fish to continue with that portion of the survey. used to filter                                         |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| sb_meal_size                           | int              | typical meal size of store bought fish. converted to grams in processed                                                                                          |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| sb_part_of_fish                        | chr              | which part of the fish the subject eats for store bought fish                                                                                                    |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   fillet                                                                                                                                                       |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   whole fish (gutted)                                                                                                                                          |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   whole fish (not gutted)                                                                                                                                      |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   other (please explain)                                                                                                                                       |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   not applicable. I have only consumed shellfish                                                                                                               |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| advisory_consideration                 | chr              | if the subject considers DNREC advisories                                                                                                                        |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   yes                                                                                                                                                          |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   no                                                                                                                                                           |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   prefer not to answer                                                                                                                                         |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| share_fish                             | chr              | if the subject shares the fish they catch with their household                                                                                                   |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   yes                                                                                                                                                          |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   no                                                                                                                                                           |
|                                        |                  |                                                                                                                                                                  |
|                                        |                  | -   prefer not to answer                                                                                                                                         |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| members_of_household                   | int              | how many members of their family they share with                                                                                                                 |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| children_consumption                   | chr              | indication of if any of those household members are children under 14                                                                                            |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| sb_species                             | chr              | list of store bought species the subject has consumed                                                                                                            |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| scallop                                | num              | weekly consumption frequency of scallop                                                                                                                          |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| flounder                               | num              | weekly consumption frequency of store bought flounder                                                                                                            |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| swordfish                              | num              | weekly consumption frequency of store bought swordfish                                                                                                           |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| halibut                                | num              | weekly consumption frequency of store bought halibut                                                                                                             |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| arctic_char                            | num              | weekly consumption frequency of store bought halibut                                                                                                             |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| dolphinfish...84                       | num              | weekly consumption frequency of store bought dolphinfish                                                                                                         |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| lobster                                | num              | weekly consumption frequency of lobster                                                                                                                          |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| mussel                                 | num              | weekly consumption frequency of mussel                                                                                                                           |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| sardine                                | num              | weekly consumption frequency of sardine                                                                                                                          |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| red_snapper                            | num              | weekly consumption frequency of red snapper                                                                                                                      |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| trout                                  | num              | weekly consumption frequency of trout                                                                                                                            |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| shrimp                                 | num              | weekly consumption frequency of shrimp                                                                                                                           |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| shrimp_quantity                        | num              | number of shrimp usually consumed                                                                                                                                |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| salmon                                 | num              | weekly consumption frequency of salmon                                                                                                                           |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| salmon_raw_cooked                      | chr              | whether the subject eats salmon raw or cooked                                                                                                                    |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| tuna                                   | num              | weekly consumption frequency of store bought tuna                                                                                                                |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| tuna_raw_cooked                        | num              | whether the subject eats tuna raw or cooked                                                                                                                      |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| tilapia                                | num              | weekly consumption frequency of tilapia                                                                                                                          |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| alaska_polock                          | num              | weekly consumption frequency of alaska pollock                                                                                                                   |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| cod                                    | num              | weekly consumption frequency of cod                                                                                                                              |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| crab                                   | num              | weekly consumption frequency of crab                                                                                                                             |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| crab_quantity                          | num              | how many crabs are usually eaten                                                                                                                                 |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| catfish                                | num              | weekly consumption frequency of store bought catfish                                                                                                             |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| clam                                   | num              | weekly consumption frequency of store bought clams                                                                                                               |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| clam_quantity                          | num              | how many clams are usually eaten                                                                                                                                 |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| clam_raw_cooked                        | chr              | whether the clams are eaten raw or cooked                                                                                                                        |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| oyster...105                           | num              | weekly consumption frequency of store bought oysters                                                                                                             |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| oyster_quantity                        | num              | how many oysters are usually eaten                                                                                                                               |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| oyster_raw_cooked                      | chr              | whether the oysters are eaten raw, cooked or both                                                                                                                |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| other...108                            | num              | weekly consumption frequency of the species indicated "other"                                                                                                    |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| sr_meal_freq_wc                        | num              | wild caught consumption frequency (divided by 7 for daily consumption)                                                                                           |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| sr_meal_freq_sb                        | num              | store bought consumption frequency (divided by 7 for daily consumption)                                                                                          |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| sr_meal_freq                           | num              | sum of store bought and wild consumption, total daily fish consumption frequency                                                                                 |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| date                                   | num              | "start_date" with seconds removed. used for filtering between seasons                                                                                            |
+----------------------------------------+------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------+

: Data dictionary for survey data that includes important columns
