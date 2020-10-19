library(targets)

# Set target-specific options such as packages.
targets::tar_option_set(packages = c("here", "tidyverse", "tidytext", "lubridate", 
                                     "textdata", "hash", "sjPlot",
                                     "janitor", "googlesheets4"))

source(here::here("R", "functions.R"))

# Define targets
targets <- list(
  
  ## Commented out reply gathering, replies are now as .rda in /data-raw
  
  # for recursively adding tweets
  #tar_target(file_name_for_sample_of_tweets, here::here("data", "sample-of-tweets.rds"), format = "file"),
  #tar_target(sample_of_tweets_for_thread_finding, read_rds(file_name_for_sample_of_tweets)),
  #tar_target(extracted_status_ids, extract_status_ids(sample_of_tweets_for_thread_finding)),
  #tar_target(replies_that_were_recursively_searched, get_replies_recursive(extracted_status_ids)),
  
  # for joining added tweets and adding a variable for what thread a tweet is a part of
  #tar_target(uniquely_identified_reply_threads, identify_threads(replies_that_were_recursively_searched)), # JR is not sure this is working correctly
  #tar_target(original_tweets_with_replies_added, combine_original_with_reply_tweets(sample_of_tweets_for_thread_finding, replies_that_were_recursively_searched)),
  #tar_target(original_tweets_with_replies_added_with_id_string, join_id_string),
  
  tar_target(raw_data_file, create_raw_data(), format = "file"),
  tar_target(raw_data, readRDS(raw_data_file)),

  tar_target(ss_scale_file, here::here("data-sentiment", "sentistrength_scale.txt"), format="file"),
  tar_target(ss_binary_file, here::here("data-sentiment", "sentistrength_binary.txt"), format="file"),
  tar_target(liwc_file, here::here("data-sentiment", "liwc_results.csv"), format = "file"),
  tar_target(teacher_class_file, here::here("data-sentiment", "teacher_prediction.csv"), format="file"),
  
  tar_target(ss_scale_data, read.table(ss_scale_file, sep="\t", header = T, quote="")),
  tar_target(ss_binary_data, read.table(ss_binary_file, sep="\t", header = T, quote="")),
  tar_target(liwc_data, read_liwc_and_rename_input_cols(liwc_file)),
  tar_target(teacher_class_data, read.csv(teacher_class_file) %>% rename(is_teacher=prediction_by_keywords)), 
  
  tar_target(raw_with_external, add_external_master(raw_data, ss_scale_data, ss_binary_data, liwc_data, teacher_class_data)),

  tar_target(clean_data_file, raw_with_external %>% clean_master(), format = "file"),
  tar_target(clean_data, readRDS(clean_data_file)),

  tar_target(data_main_vars, clean_data %>% add_vars_master()),
  tar_target(data_tidytext, data_main_vars %>% tidytext_master()),
  tar_target(data_context, data_tidytext %>% context_master()),
  tar_target(data_discrepancy, data_context %>% discrepancy_master()),

  tar_target(final_data_file, data_discrepancy %>% save_final_dataset(), format = "file"),
  tar_target(final_data, readRDS(final_data_file)) ,
  
  # agreement from manual coding
  tar_target(agree_df, access_manual_coding_data(1:20)), # row indices are for the first 20 rows manually coded
  tar_target(agree_statistics, calculate_manual_agreement(agree_df))
  # Descriptives and results in seperate Rmd Files in root folder for now

  #tar_target(descriptives, final_data %>% descriptives_master()),
  #tar_target(analysis, final_data %>% analysis_master())

)

# End with a call to tar_pipeline() to wrangle the targets together.
# This target script must return a pipeline object.
tar_pipeline(targets)
