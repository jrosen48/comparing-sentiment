library(targets)
library(tidyverse)
library(stringr)

source(here::here("R", "functions.R"))

# Set target-specific options such as packages.
tar_option_set(packages = c("here", "tidyverse"))

# Define targets
targets <- list(
  
  tar_target(test_target, test_f),
  tar_target(another_test_target, test_f)
  
)

# End with a call to tar_pipeline() to wrangle the targets together.
# This target script must return a pipeline object.
tar_pipeline(targets)
