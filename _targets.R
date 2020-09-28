library(targets)

source(here::here("R", "functions.R"))

# Set target-specific options such as packages.
tar_option_set(packages = c("purrr", "readr", "ggplot2", "janitor", "dplyr",
                            "here", "lubridate", "tidyr")
)

# Define targets
targets <- list(
  tar_target(files_ccss, list.files(here::here("data", "ccss"), 
                               full.names = TRUE,
                               pattern = "\\.csv$", 
                               recursive = TRUE), # if data is organized by sub-directories; w/ all data, can take awhile
             format = "file"),
  tar_target(raw_data_ccss, map_df(files_ccss, read_csv_with_col_types)),
  tar_target(d_ccss, prep_data(raw_data_ccss)),
  
  tar_target(files_ngss, list.files(here::here("data", "ngss"), 
                                    full.names = TRUE,
                                    pattern = "\\.csv$", 
                                    recursive = TRUE), # if data is organized by sub-directories; w/ all data, can take awhile
             format = "file"),
  tar_target(raw_data_ngss, map_df(files_ngss, read_csv_with_col_types)),
  tar_target(d_ngss, prep_data(raw_data_ngss))
  
)

# End with a call to tar_pipeline() to wrangle the targets together.
# This target script must return a pipeline object.
tar_pipeline(targets)
