access_manual_coding_data <- function(row_indices) {
  s1 <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1UOMJP4HUDDVlOs-i0orqyY_9Mf5s13yaZwzOa8yMqko/edit#gid=1614206599",
                                  sheet = 1) %>% 
    janitor::clean_names()
  
  s2 <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1UOMJP4HUDDVlOs-i0orqyY_9Mf5s13yaZwzOa8yMqko/edit#gid=1614206599",
                                  sheet = 2) %>% 
    janitor::clean_names()
  
  tibble(r1_pos = s1$positive_affect_1_5[row_indices], 
         r2_pos = s2$positive_affect_1_5[row_indices],
         r1_neg = s1$negative_affect_1_5[row_indices], 
         r2_neg = s2$negative_affect_1_5[row_indices])
  
}

calculate_agreement <- function(agree_df) {
  
  d1 <- tibble(scale = "pos", 
               agree = irr::agree(agree_df[, 1:2])$value,
               icc = irr::icc(agree_df[, 1:2])$value,
               kappa = irr::kappa2(agree_df[, 1:2], weight = "squared")$value)
  
  d2 <- tibble(scale = "neg", 
               agree = irr::agree(agree_df[, 3:4])$value,
               icc = irr::icc(agree_df[, 3:4])$value,
               kappa = irr::kappa2(agree_df[, 3:4], weight = "squared")$value)
  
  bind_rows(d1, d2)
}