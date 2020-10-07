#d <- tar_read(raw_data)

##### READING IN AND COMBINING RAW DATA FILES #####

create_raw_data <- function(){
  fns <- dir(here::here("data-raw"), pattern=".rda")
  d <- NULL
  for (fn in fns){
    load(here::here("data-raw", fn))
    tweets_dl$dl_at <- file.info(here::here("data-raw", fn))$mtime
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
  d$text_clean <- gsub("[\r\n]", "", d$text)
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

add_nchar <- function(d){
  d$nchar <- nchar(d$text_clean)
  return(d)
}

add_nwords <- function(d){
  d$nwords <- d$text_clean %>% strsplit("\\W+") %>% sapply(length)
  return(d)
}

add_sentistrength <- function(d){
  scale <- read.table(here::here("data-sentiment", "sentistrength_scale.txt"), 
                      sep="\t", header = T, quote="")
  d$ss_pos <- scale[,1]
  d$ss_neg <- scale[,2]
  d$ss_binary <- read.table(here::here("data-sentiment", "sentistrength_binary.txt")
                            , sep="\t", header = T, quote="")[,1]
  return(d)
}

add_liwc <- function(d){
  liwc <- read.csv(here::here("data-sentiment", "liwc_results.csv"))
  d$liwc_pos <- liwc$posemo
  d$liwc_neg <- liwc$negemo
  return(d)
}

add_vars_master <- function(d){
  return(
    d %>%
      add_nchar() %>%
      add_nwords() %>%
      add_sentistrength() %>%
      add_liwc()
  )
}

##### ADD TIDYTEXT VARIABLES ####