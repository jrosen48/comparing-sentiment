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
  d$ss_scale <- d$ss_pos + d$ss_neg
  d$ss_scale_scaled <- d$ss_scale %>% scale()
  d$ss_binary <- read.table(here::here("data-sentiment", "sentistrength_binary.txt")
                            , sep="\t", header = T, quote="")[,1]
  return(d)
}

add_liwc <- function(d){
  liwc <- read.csv(here::here("data-sentiment", "liwc_results.csv"))
  d$liwc_pos <- liwc$posemo
  d$liwc_neg <- liwc$negemo
  d$liwc_scale <- d$liwc_pos - d$liwc_neg
  d$liwc_scale_scaled <- d$liwc_scale %>% scale()
  d$liwc_binary <- ifelse(d$liwc_scale >= 0, 1, 0)
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
    mutate(sentiment_scale = positive - negative) %>% 
    rename(status_id = index, bing_pos = positive,
           bing_neg = negative, bing_scale = sentiment_scale)
  cat("*** BING DICTIONARY RESULTS CREATED ***\n")
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
  cat("*** AFINN DICTIONARY RESULTS CREATED ***\n")
  return(d %>% left_join(res))
}

add_loughran <- function(d, tt){
  res <- tt %>%
    inner_join(get_sentiments("loughran")) %>%
    count(index = status_id, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment_scale = positive - negative) %>%
    rename(status_id = index, loughran_pos = positive,
           loughran_neg = negative, loughran_scale = sentiment_scale) %>%
    select(c("status_id", "loughran_pos", "loughran_neg", "loughran_scale"))
  cat("*** LOUGHRAN DICTIONARY RESULTS CREATED ***\n")
  return(d %>% left_join(res))
}

add_nrc <- function(d, tt){
  res <- tt %>%
    inner_join(get_sentiments("nrc")) %>%
    count(status_id, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(nrc_scale = positive - negative) %>% 
    rename(nrc_pos = positive, nrc_neg = negative) %>%
    select(c("status_id", "nrc_pos", "nrc_neg", "nrc_scale"))
  cat("*** NRC DICTIONARY RESULTS CREATED ***\n")
  return(d %>% left_join(res))
}

divide_by_nwords <- function(d){
  d$bing_pos <- d$bing_pos / d$nwords
  d$bing_neg <- d$bing_neg / d$nwords
  d$bing_scale <- d$bing_scale / d$nwords    
  d$afinn_pos <- d$afinn_pos / d$nwords
  d$afinn_neg <- d$afinn_neg / d$nwords     
  d$afinn_scale <- d$afinn_scale / d$nwords
  d$loughran_pos <- d$loughran_pos / d$nwords
  d$loughran_neg <- d$loughran_neg / d$nwords   
  d$loughran_scale <- d$loughran_scale / d$nwords 
  d$nrc_pos <- d$nrc_pos / d$nwords
  d$nrc_neg <- d$nrc_neg / d$nwords       
  d$nrc_scale <- d$nrc_scale / d$nwords
  return(d)
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

create_tidytext_binaries <- function(d){  # for validation of hand-coding
  d$bing_binary <- ifelse(d$bing_scale >= 0, 1, 0)
  d$afinn_binary <- ifelse(d$afinn_scale >= 0, 1, 0)
  d$loughran_binary <- ifelse(d$loughran_scale >= 0, 1, 0)
  d$nrc_binary <- ifelse(d$nrc_scale >= 0, 1, 0)
  d$tidytext_binary <- ifelse(d$tidytext_scale >= 0, 1, 0)
  return(d)
}

scale_tidytext_scales <- function(d){
  d$bing_scale_scaled <- scale(d$bing_scale)
  d$afinn_scale_scaled <- scale(d$afinn_scale)
  d$loughran_scale_scaled <- scale(d$loughran_scale)
  d$nrc_scale_scaled <- scale(d$nrc_scale)
  d$tidytext_scale_scaled <- scale(d$tidytext_scale)
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
      divide_by_nwords() %>%
      add_tidytext() %>%
      create_tidytext_binaries() %>%
      scale_tidytext_scales()
  )
}

##### ADD TWEET CONTEXT VARIABLE AND IDENTIFY CHATS ####

add_q <- function(d){
  text_small <- d$text %>% tolower()
  has_ngsschat <- grep("\\#ngsschat\\b", text_small)
  has_ngss <- grep("\\#ngss\\b|\\bngss\\b", text_small)
  d$q <- rep(NA, nrow(d))
  d$q[has_ngsschat] <- "#NGSSchat"
  d$q[base::setdiff(has_ngss, has_ngsschat)] <- "ngss"  # fill in ngss withough #ngsschat
  d$q[base::setdiff(1:nrow(d), unique(c(has_ngss, has_ngsschat)))] <- "other" # fill in remaining
  d$q <- as.factor(d$q)
  return(d)
}

add_isChat <- function(d){
  d2 <- d[d$q == "#NGSSchat",] %>% 
          select(c("status_id", "created_at", "text")) # subset #NGSSchat data
  
  time <- d2$created_at
  time <- format(time, format="%Y-%m-%d %H")

  freq <- table(time) 
  freq <- sort(freq, decreasing = T)
  freq <- freq %>% head(1000) 
  
  hours <- names(freq) # Select 1,000 most busy hours in data set
  
  # Select tweets at the beginning of these 1,000 hours (+- 5 minutes around edge)
  
  hours <- hours %>%  # format to hour
              sapply(paste, ":00:00 UTC", sep="") %>% 
              as.character() %>% 
              as.POSIXct(tz = "UTC")
  
  # Add minutes around edges of beginning of hours
  
  minutes <- hours
  
  for (i in seq(60, 5*60, 60)){
    minutes <- c(minutes, hours-i) # seconds are added and substracted in steps of 60
    minutes <- c(minutes, hours+i)
  }
  
  # Standardize tweet posting times to minutes in order to obtain tweets around beginning of must busy hours
  
  post_minutes <- d2$created_at %>%
                     round_date(unit="1 minute")
  
  possible_chat_openings <- d2[which(post_minutes %in% minutes),]
  
  # Grab tweets with "Welcome to "NGSSchat" and related terms, openings lines have been manually looked up before
  
  ind <- grep("Welcome to #NGSSchat", possible_chat_openings$text)
  ind <- c(ind, grep("Welcome to the first session of #NGSSchat", possible_chat_openings$text))
  ind <- c(ind, grep("Our #nhed guest moderator for this evening is", possible_chat_openings$text))
  ind <- c(ind, grep("Excited to learn and connect with my #NGSSchat community-- Join us-- happening", possible_chat_openings$text))
  
  ind <- unique(ind)
  
  # Sort out which specific hours are chats based on ind
  
  hours_with_opening_lines <- possible_chat_openings$created_at[ind] %>%
                                  format(format="%Y-%m-%d %H") %>%
                                  sapply(paste, ":00:00 UTC", sep="") %>%
                                  as.character() %>%
                                  as.POSIXct(tz = "UTC")
  
  chat_hours <- hours[which(hours %in% hours_with_opening_lines)]  # declare busiest hours with opening lines as chat hours
  
  # Create Variable "isChat", strict definition of chat as a 1 hour timeframe
  
  time <- d2$created_at %>%
            format(format="%Y-%m-%d %H")
  
  hours <- time %>% 
            sapply(paste, ":00:00 UTC", sep="") %>%
            as.character() %>%
            as.POSIXct(tz = "UTC")
  
  ind <- which(hours %in% chat_hours)
  
  isChat <- rep(0, nrow(d2))
  isChat[ind] <- 1  # 1 if tweets is in chat session
  
  d2$isChat <- isChat

  # Knit back together with full data frame
  
  d$isChat <- rep(NA, nrow(d))
  d$isChat[d$status_id %in% d2$status_id] <- d2$isChat
  d$isChat <- d$isChat %>% as.factor()
  
  return(d)
}

context_master <- function(d){
  return(
    d %>%
      add_q() %>%
      add_isChat() 
  )
}

##### DISCREPANCY VARIABLES #####

get_scales <- function(){
  return(
    c(
      "ss_scale_scaled",
      "liwc_scale_scaled",
      "bing_scale_scaled",
      "afinn_scale_scaled",
      "loughran_scale_scaled",
      "nrc_scale_scaled",
      "tidytext_scale_scaled"
    )
  )
}

add_pairwise_disc <- function(d){
  s_scales <- get_scales()
  
  combinations <- t(combn(s_scales, 2))
  
  for (i in 1:nrow(combinations)){  # iterate through each combination and build squared diff
    name <- paste(
      combinations[i, 1] %>% strsplit("_") %>% unlist() %>% head(1),
      combinations[i, 2] %>% strsplit("_") %>% unlist() %>% head(1), 
      "discrepancy", sep="_"
    )
    d$temp <- (d[,combinations[i, 1]] - d[,combinations[i, 2]])^2
    names(d)[names(d) == "temp"] <- name
  }
  return(d)
}

add_total_disc <- function(d){  # currently: add all available pairs (less disc for less coverage)
  s_scales <- get_scales()
  
  combinations <- t(combn(s_scales, 2))
  
  the_sum <- rep(0, nrow(d))
  
  for (i in 1:nrow(combinations)){  
    temp <- (d[,combinations[i, 1]] - d[,combinations[i, 2]])^2 %>% unlist() %>% as.numeric()
    the_sum[which(!is.na(temp))] <- the_sum[which(!is.na(temp))] + temp[which(!is.na(temp))]
  } 
  
  the_sum[the_sum == 0] <- NA     # reassign NAs for possible 0 coverage
  d$total_discrepancy <- the_sum
  
  return(d)
}

discrepancy_master <- function(d){
  return(
    d %>%
      add_pairwise_disc() %>%
      add_total_disc()
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

analysis_master <- function(d){   # just an example
  print(cor.test(d$total_discrepancy, d$nwords))
}
