library(targets)

# Set target-specific options such as packages.
targets::tar_option_set(packages = c("here", "tidyverse", "tidytext", "lubridate", 
                                     "textdata", "hash", "sjPlot",
                                     "janitor", "googlesheets4"))

source(here::here("R", "functions.R"))

## Notes on data that has to be in the local folder to make this version of the pipeline work:

# Define targets
targets <- list(
  
  tar_target(study_sample_file, "./data/study-sample-2021-01-21.rds", format = "file"),
  tar_target(study_sample, readRDS(study_sample_file))#,

  #tar_target(data_clean, study_sample %>% clean_master()),

  #tar_target(data_main_vars, clean_data %>% add_vars_master()),
  #tar_target(data_context, data_tidytext %>% context_master()),
  #tar_target(data_discrepancy, data_context %>% discrepancy_master()),

  
  # manual coding reliability
  #tar_target(file_for_state_sample_for_qual_coding, here("data", "sample-of-state-tweets-for-qual-coding.csv")),
  #tar_target(joined_state_sample_for_qual_coding, join_raw_and_google_sheets_data(file_for_state_sample_for_qual_coding)),
  
  #tar_target(agree_df_1_20, access_manual_coding_data(1:20)), # row indices are for the first 20 rows manually coded; will pdate as we 
  #tar_target(agree_statistics_1_20, calculate_manual_agreement(agree_df_1_20)),
  #tar_target(agree_df_21_45, access_manual_coding_data(21:45)),
  #tar_target(agree_statistics_21_45, calculate_manual_agreement(agree_df_21_45)),
  #tar_target(agree_df_states_1_20, access_manual_coding_data_state_data(1:20)),
  #tar_target(agree_statistics_states_1_20, calculate_manual_agreement(agree_df_states_1_20)),
  
  #tar_target(combined_agree_df, dplyr::bind_rows(agree_df_1_20, agree_df_21_45, agree_df_states_1_20)),
  #tar_target(combined_agree_stats, calculate_manual_agreement(combined_agree_df)),

  # consensus codes from manual coding
  #tar_target(consensus_manual_codes, access_consensus_codes_conrad(1:45, 1:71)),
  
  #tar_target(indexs_of_coded_tweets_in_data, find_indexes_master(final_data, consensus_manual_codes)),
  
  # evaluate consensus
  #tar_target(consensus_with_software_ratings, combine_coding_and_software_ratings(consensus_manual_codes, 
   #                     final_data, indexs_of_coded_tweets_in_data)),
  
  #tar_target(validation, consensus_with_software_ratings %>% validation_master)

)

# End with a call to tar_pipeline() to wrangle the targets together.
# This target script must return a pipeline object.
tar_pipeline(targets)
