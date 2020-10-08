library(targets)
library(tidyverse)
library(tidytext)

source(here::here("R", "functions.R"))

# Set target-specific options such as packages.
tar_option_set(packages = c("here", "tidyverse"))

# Define targets
targets <- list(
  
  tar_target(raw_data_file, create_raw_data(), format = "file"),
  tar_target(raw_data, readRDS(raw_data_file)),
  
  tar_target(clean_data_file, raw_data %>% clean_master(), format = "file"),
  tar_target(clean_data, readRDS(clean_data_file)),
  
  tar_target(data_main_vars, clean_data %>% add_vars_master()),  
  tar_target(data_tidytext, data_main_vars %>% tidytext_master()),
  tar_target(data_context, data_tidytext %>% context_master()),
  
  tar_target(final_data_file, data_context %>% save_final_dataset(), format = "file"),
  tar_target(final_data, readRDS(final_data_file)),
  
  tar_target(descriptives, final_data %>% descriptives_master()),
  tar_target(analysis, final_data %>% analysis_master())
  
)

# End with a call to tar_pipeline() to wrangle the targets together.
# This target script must return a pipeline object.
tar_pipeline(targets)
