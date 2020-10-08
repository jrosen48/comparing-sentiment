#d <- tar_read(raw_data)

##### READING IN AND COMBINING RAW DATA FILES #####

create_raw_data <- function(){
  fns <- dir(here::here("data-raw"), pattern=".rda")
  d <- NULL
  for (fn in fns){
    load(here::here("data-raw", fn))
    tweets_dl$dl_at <- file.info(here::here("data-raw", fn))$mtime
    d <- rbind(d, tweets_dl)
    cat("\014", round(which(fn == fns)/length(fns)*100, 0), "% files merged\n")
  }
  d <- d[order(d$created_at),]
  d <- d[!duplicated(d$status_id),]
  saveRDS(d, here::here("data", "data_raw.rds"))
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

create_tweet_tokens <- function(d){
 return(
    d %>% 
      select("status_id", "text") %>% 
      unnest_tokens(word, text)
 )
}

add_bing <- function(d, tt){
  res <- tt %>%
    inner_join(get_sentiments("bing")) %>%
    count(index = status_id, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment_scale = positive - negative)
  return(d %>% left_join(res))
}

add_afinn <- function(d, tt){
  res <- tt %>%
    inner_join(get_sentiments("afinn")) %>%
    mutate(pos = ifelse(value>0, value, 0)) %>%
    mutate(neg = ifelse(value<0, value, 0)) %>%
    group_by(status_id) %>%
    summarise(afinn_pos = sum(pos), afinn_neg = sum(neg)) %>%
    mutate(afinn_scale = afinn_pos + afinn_neg)
  return(d %>% left_join(res))
}

add_loughran <- function(d, tt){
  res <- tt %>%
    inner_join(get_sentiments("loughran")) %>%
    count(index = status_id, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment_scale = positive - negative) %>%
    rename(status_id = index, loughran_pos = positive,
           loughran_neg = negative, loughran_scale = sentiment_scale)
  return(d %>% left_join(res))
}

add_nrc <- function(d, tt){
  res <- tt %>%
    inner_join(get_sentiments("nrc")) %>%
    count(index = status_id, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment_scale = positive - negative)
    rename(status_id = index, nrc_pos = positive,
         nrc_neg = negative, nrc_scale = sentiment_scale)
  return(d %>% left_join(res))
}

add_tidytext <- function(d){
  d$tidytext_scale <- apply(cbind(
      d$bing_scale,
      d$afinn_scale,
      d$loughran_scale,
      d$nrc_scale
  ), 1, mean, na.rm=T) # biggest possible mean 
  return(d)
}

scale_sentiment_scales <- function(d){
  d$bing_scale <- scale(d$bing_scale)
  d$afinn_scale <- scale(d$afinn_scale)
  d$loughran_scale <- scale(d$loughran_scale)
  d$nrc_scale <- scale(d$nrc_scale)
  d$tidytext_scale <- scale(d$tidytext_scale)
  return(d)
}

tidytext_master <- function(d){
  tt <- create_tweet_tokens(d)
  return(
    d %>%
      add_bing(tt=tt) %>%
      add_afinn(tt=tt) %>%
      add_loughran(tt=tt) %>%
      add_nrc(tt=tt) %>%
      add_tidytext() %>%
      scale_sentiment_scales()
  )
}

##### ADD TWEET CONTEXT VARIABLE AND IDENTIFY CHATS ####

add_isChat <- function(d){
  return(0)
}

add_q <- function(d){
  text_small <- d$text.tolower()
  has_ngsschat <- grep("\\#ngsschat+", text_small)
  has_ngss <- grep("\\ngss+", text_small)
  return(0)
}

context_master <- function(d){
  return(
    d %>%
      add_isChat() %>%
      add_q() 
  )
}

##### SAVE FINAL DATA SET #####

save_final_dataset <- function(d){
  saveRDS(d, here::here("data", "data_final.rds"))
  return(here::here("data", "data_final.rds"))
}

##### DESCRIPTIVES #####

descriptives_master <- function(d){
  return(0)
}

##### ANALYSIS #####

analysis_master <- function(d){
  return(0)
}
