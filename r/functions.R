#d <- tar_read(raw_data)

##### READING IN AND COMBINING RAW DATA FILES #####

create_raw_data <- function(){
  fns <- dir(here::here("data-raw"), pattern=".rda")
  d <- NULL
  for (fn in fns){
    load(here::here("data-raw", fn))
    tweets_dl$dl_at <- file.info(here::here("data-raw", fn)$mtime)
    d <- rbind(d, tweets_dl)
    cat("\014", round(which(fn == fns)/length(fns)*100, 0), "% complete\n")
  }
  saveRDS(here::here("data", "data_raw.rds"))
  return(here::here("data", "data_raw.rds"))
}

##### Cleaning Data #####

remove_variables <- function(d){
  return(
    d %>% select(c("status_id", "user_id", "text", "created_at"))
  )
}

preprocess_text <- function(d){
  d$clean_text <- gsub("[\r\n]", "", d$text)
  return(d)
}

clean_master <- function(d){
  d <- d %>%
       remove_variables() %>%
       preprocess_text()
  saveRDS(d, here::here("data", "data_clean.rds"))
  return(here::here("data", "data_clean.rds"))
}

##### Adding main variables #####
