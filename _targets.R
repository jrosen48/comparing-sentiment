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
  tar_target(study_sample, readRDS(study_sample_file)),

  tar_target(data_clean, study_sample %>% clean_master()),
  tar_target(data_main_vars, data_clean %>% add_vars_master()),
  tar_target(data_context, data_main_vars %>% context_master()),
  
  # Endpoint A: Validation of classifications (with Python)
  
  tar_target(validation_sample_file, "./data/final-validation-set.rds", format = "file"),
  tar_target(validation_sample, readRDS(validation_sample_file)),
  tar_target(A_validation_export_file, merge_and_export_validation(data_context, validation_sample)),
  
  # Endpoint B: Descriptive data
  
  tar_target(B_descriptives, data_context %>% select_descrptive_vars()),
  
  # Endpoint C: Scale discrepancy analysis
  
  tar_target(C_discrepancy, data_context %>% discrepancy_master()),
  tar_target(full_export_file, C_discrepancy %>% (function(d, path="./data/final.rds"){saveRDS(d,path);path}), format="file")
  
)

# End with a call to tar_pipeline() to wrangle the targets together.
# This target script must return a pipeline object.
tar_pipeline(targets)
