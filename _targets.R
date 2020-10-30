library(targets)

# Set target-specific options such as packages.
targets::tar_option_set(packages = c("here", "tidyverse", "tidytext", "lubridate", 
                                     "textdata", "hash", "sjPlot",
                                     "janitor", "googlesheets4"))

source(here::here("R", "functions.R"))

## Notes on data that has to be in the local folder to make this version of the pipeline work:

# data-raw: all data files in .rda format including 10 hydrated files for additional tweets, 322 files total
# data: state-based-twitter-hashtags-raw-data.rds
# data-sentiment: teacher_prediction.csv, raw-data-combined-for-liwc-results.csv, 
#                 sentistrength_scale.txt, sentistrength_binary.txt

# Define targets
targets <- list(
  
  tar_target(raw_data_file, create_raw_data(), format = "file"),
  tar_target(raw_data, readRDS(raw_data_file)),

  tar_target(additional_data_file, here::here("data", "state-based-twitter-hashtags-raw-data.rds")),
  tar_target(additional_data, readRDS(additional_data_file)),
  tar_target(additional_data_prepared, additional_data %>% additional_data_prep()),
  
  #tar_target(raw_data_combined, merge_additional_files(raw_data, additional_data_prepared)),
  #tar_target(write_raw_data_combined_for_liwc, raw_data_combined %>% select(status_id, text) %>% write_csv(here("data-sentiment", "raw-data-combined-for-liwc.csv"))),
  
  tar_target(liwc_file_path, here::here("data-sentiment", "raw-data-combined-for-liwc-results.csv"), format = "file"),
  tar_target(liwc_data, read_liwc_and_rename_input_cols(liwc_file_path)),
  
  tar_target(matching_df_liwc, create_matching_df(raw_data, additional_data_prepared)),
  tar_target(add_liwc_df, add_liwc_to_additional_data(matching_df_liwc, liwc_data)),
  tar_target(match_liwc_to_raw_data, raw_data %>% left_join(add_liwc_df, by="status_id")),

  tar_target(ss_scale_file, here::here("data-sentiment", "sentistrength_scale.txt"), format="file"),
  tar_target(ss_binary_file, here::here("data-sentiment", "sentistrength_binary.txt"), format="file"),
  
  tar_target(ss_scale_data, read.table(ss_scale_file, sep="\t", header = T, quote="")),
  tar_target(ss_binary_data, read.table(ss_binary_file, sep="\t", header = T, quote="")),
  
  tar_target(teacher_class_file, here::here("data-sentiment", "teacher_prediction.csv"), format="file"),
  tar_target(teacher_class_data, read.csv(teacher_class_file) %>% rename(is_teacher=prediction_by_keywords)), 
  
  tar_target(raw_with_external, add_external_master(match_liwc_to_raw_data, ss_scale_data, ss_binary_data, teacher_class_data)),
  tar_target(clean_data_file, raw_with_external %>% clean_master(), format = "file"),
  tar_target(clean_data, readRDS(clean_data_file)),

  tar_target(data_main_vars, clean_data %>% add_vars_master()),
  tar_target(data_tidytext, data_main_vars %>% tidytext_master()),
  tar_target(data_context, data_tidytext %>% context_master()),
  tar_target(data_discrepancy, data_context %>% discrepancy_master()),

  tar_target(final_data_file, data_discrepancy %>% save_final_dataset(), format = "file"),
  tar_target(final_data, readRDS(final_data_file)),

  # manual coding reliability
  tar_target(file_for_state_sample_for_qual_coding, here("data", "sample-of-state-tweets-for-qual-coding.csv")),
  tar_target(joined_state_sample_for_qual_coding, join_raw_and_google_sheets_data(file_for_state_sample_for_qual_coding)),
  
  tar_target(agree_df_1_20, access_manual_coding_data(1:20)), # row indices are for the first 20 rows 
  
  tar_target(agree_statistics_1_20, calculate_manual_agreement(agree_df_1_20)),
  tar_target(agree_df_21_45, access_manual_coding_data(21:45)),
  tar_target(agree_statistics_21_45, calculate_manual_agreement(agree_df_21_45)),
  tar_target(agree_df_states_1_20, access_manual_coding_data_state_data(1:20)),
  tar_target(agree_statistics_states_1_20, calculate_manual_agreement(agree_df_states_1_20)),
  
  # consensus codes from manual coding
  tar_target(consensus_manual_codes, access_consensus_codes(1:45, 1:20)),
  
  # evaluate consensus
  tar_target(consensus_with_software_ratings, combine_coding_and_software_ratings(consensus_manual_codes, final_data)),
  
  tar_target(validation, consensus_with_software_ratings %>% validation_master)
  
  # tar_target(agree_df_first_1_20, access_manual_coding_data(1:20)), 
  # tar_target(agree_statistics, calculate_manual_agreement(agree_df_1_20))
  
  
)

# End with a call to tar_pipeline() to wrangle the targets together.
# This target script must return a pipeline object.
tar_pipeline(targets)
