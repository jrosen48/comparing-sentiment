library(targets)
library(tidyverse)
library(tidytext)
library(lubridate)

source(here::here("R", "functions.R"))

# Set target-specific options such as packages.
tar_option_set(packages = c("here", "tidyverse"))

# Define targets
targets <- list(
  
  tar_target(raw_data_file, create_raw_data(), format = "file"),
  tar_target(raw_data, readRDS(raw_data_file)),
  
  tar_target(ss_scale_file, here::here("data-sentiment", "sentistrength_scale.txt"), format="file"),
  tar_target(ss_binary_file, here::here("data-sentiment", "sentistrength_binary.txt"), format="file"),
  tar_target(liwc_file, here::here("data-sentiment", "liwc_results.csv"), format="file"),
  
  tar_target(ss_scale_data, read.table(ss_scale_file, sep="\t", header = T, quote="")),
  tar_target(ss_binary_data, read.table(ss_binary_file, sep="\t", header = T, quote="")),
  tar_target(liwc_data, read.csv(liwc_file)),  
  
  tar_target(raw_with_external, add_external_master(raw_data, ss_scale_data, ss_binary_data, liwc_data)),
  
  tar_target(clean_data_file, raw_with_external %>% clean_master(), format = "file"),
  tar_target(clean_data, readRDS(clean_data_file)),
  
  tar_target(data_main_vars, clean_data %>% add_vars_master()),  
  tar_target(data_tidytext, data_main_vars %>% tidytext_master()),
  tar_target(data_context, data_tidytext %>% context_master()),
  tar_target(data_discrepancy, data_context %>% discrepancy_master()),
  
  tar_target(final_data_file, data_discrepancy %>% save_final_dataset(), format = "file"),
  tar_target(final_data, readRDS(final_data_file)),
  
  # RESULTS: optionally for different subsets (#NGSSchat, non-chat, ...)
  
  tar_target(descriptives, final_data %>% descriptives_master()),
  tar_target(analysis, final_data %>% analysis_master())
  
)

# End with a call to tar_pipeline() to wrangle the targets together.
# This target script must return a pipeline object.
tar_pipeline(targets)
