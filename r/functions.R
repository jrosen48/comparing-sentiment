##### READING IN AND COMBINING RAW DATA FILES #####

additional_data_prep <- function(d){
  return(
    d %>% select(c("status_id", "created_at"))
  )
}

remove_variables <- function(d){
  return(
    d %>% select(c("status_id", "user_id", "text", "created_at", "dl_at",
                   "is_retweet", "is_quote", 
                   "favorite_count", "retweet_count", "lang", "description" 
                   ))
  )
}

create_raw_data <- function(){
  fns <- dir(here::here("data-raw"), pattern=".rda")
  i <- 1
  object_list <- list()
  for (fn in fns){
    load(here::here("data-raw", fn))
    tweets_dl$dl_at <- file.info(here::here("data-raw", fn))$mtime
    object_list[[i]] <- tweets_dl %>% remove_variables()
    i <- i+1
    cat("\014", round(which(fn == fns)/length(fns)*100, 0), "% files read in\n")
  }
  d <- do.call(rbind, object_list)
  d <- d[order(d$created_at),]
  d <- d[!duplicated(d$status_id),]
  saveRDS(d, here::here("data", "data_raw.rds"))
  return(here::here("data", "data_raw.rds"))
}

merge_additional_files <- function(d_raw, d_additional){
  d_raw <- rbind(d_raw, d_additional)
  d_raw <- d_raw[order(d_raw$created_at),]
  d_raw <- d_raw[!duplicated(d_raw$status_id),]
  return(d_raw)
}

combine_liwc_dfs <- function(d1, d2){
  d1 <- d1 %>% select(status_id, posemo, negemo)
  d2 <- d2 %>% select(status_id, posemo, negemo)
  d1 <- rbind(d1, d2)
  d1 <- d1 <- d1[!duplicated(d1$status_id),]
  return(d1)
}

##### ADD EXTERNAL SENTIMENT DATA #####

add_sentistrength <- function(d, ss_scale_data, ss_binary_data){
  d$ss_pos <- ss_scale_data[,1]
  d$ss_neg <- ss_scale_data[,2]
  d$ss_scale <- d$ss_pos + d$ss_neg
  d$ss_pos_scaled <- d$ss_pos %>% scale()
  d$ss_neg_scaled <- d$ss_neg %>% scale()
  d$ss_scale_scaled <- d$ss_scale %>% scale()
  d$ss_binary <- ss_binary_data[,1]
  return(d)
}

add_liwc <- function(d, liwc_data){
  d <- d %>% 
    left_join(liwc_data) %>%
    rename(liwc_pos = posemo, liwc_neg = negemo)
  d$liwc_scale <- d$liwc_pos - d$liwc_neg
  d$liwc_pos_scaled <- d$liwc_pos %>% scale()
  d$liwc_neg_scaled <- d$liwc_neg %>% scale()  
  d$liwc_scale_scaled <- d$liwc_scale %>% scale()
  d$liwc_binary <- ifelse(d$liwc_scale >= 0, 1, 0)
  return(d)
}

add_is_teacher <- function(d, teacher_class_data){
  d$is_teacher <- teacher_class_data[,1]
  # assign role at random for ambiguous classification due to changed profiles over time
  # seed automatically determined by targets
  dups <- d %>% 
    select(c("user_id", "is_teacher")) %>%
    unique() 
  dups <- dups[duplicated(dups$user_id),] 
  dups$is_teacher <- sample(0:1, nrow(dups), replace=T)
  h <- hash(dups$user_id, dups$is_teacher)
  d$is_teacher[d$user_id %in% keys(h)] <- sapply(d$user_id[d$user_id %in% keys(h)], 
                                                 FUN=function(k){h[k] %>% values() %>% as.numeric()})
  return(d)
}

add_external_master <- function(d, ss_scale_data, ss_binary_data, teacher_class_data){
  return(
    d %>%
      add_sentistrength(ss_scale_data=ss_scale_data, ss_binary_data=ss_binary_data) %>%
      add_is_teacher(teacher_class_data=teacher_class_data)
  )
}

add_liwc_to_additional_data <- function(d, d_liwc){
  d$liwc_pos <- d_liwc$posemo
  d$liwc_neg <- d_liwc$negemo
  d$liwc_scale <- d$liwc_pos - d$liwc_neg
  d$liwc_pos_scaled <- d$liwc_pos %>% scale()
  d$liwc_neg_scaled <- d$liwc_neg %>% scale()
  d$liwc_scale_scaled <- d$liwc_scale %>% scale()
  d$liwc_binary <- ifelse(d$liwc_scale >= 0, 1, 0)
  return(d)
}

create_matching_df <- function(d_raw, d_additional){
  d_raw <- d_raw[d_raw$dl_at < "2020-10-22 00:00:00 CEST",] %>% 
    select(status_id, created_at)
  out <- rbind(d_raw, d_additional)
  out <- out[order(out$created_at),]
  out <- out[!duplicated(out$status_id),]
  return(out %>% select(status_id))
}

##### Cleaning Data #####

preprocess_text <- function(d){
  d$text_clean <- d$text %>% 
    gsub(pattern="https\\S*", replacement="") %>%   # urls
    gsub(pattern="http\\S*", replacement="") %>%    # urls
    gsub(pattern="@\\S*", replacement="") %>%       # tagging
    gsub(pattern="&amp", replacement="") %>%        # ampersand encoding
    gsub(pattern="[\r\n]", replacement="") %>%      # line breaks
    gsub(pattern="[[:punct:]]", replacement="") %>% # punctuation, keep hashtags as words
    gsub(pattern="\\s+", replacement=" ") %>%       # multiple white space to single space
    base::trimws()                                  # remove white space at start and end of tweets
  return(d)
}

remove_langs <- function(d){
  return(
    d[d$lang %in% c("en"),]  # english only
  )
}

clean_master <- function(d){
  d <- d %>%
    preprocess_text() %>%
    remove_langs()
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

add_vars_master <- function(d){
  return(
    d %>%
      add_nchar() %>%
      add_nwords()
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

combine_original_with_reply_tweets <- function(orig, new) {
  
  bind_rows(orig, new) %>% 
    distinct(status_id)
  
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
  d$tidytext_pos <- apply(cbind(
    d$bing_pos,
    d$afinn_pos,
    d$loughran_pos,
    d$nrc_pos
  ), 1, mean, na.rm=T) # biggest possible mean 
  
  d$tidytext_neg <- apply(cbind(
    d$bing_neg,
    d$afinn_neg,
    d$loughran_neg,
    d$nrc_neg
  ), 1, mean, na.rm=T) # biggest possible mean 
  
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
  d$bing_pos_scaled <- scale(d$bing_pos)
  d$bing_neg_scaled <- scale(d$bing_neg)
  d$bing_scale_scaled <- scale(d$bing_scale)
  d$afinn_pos_scaled <- scale(d$afinn_pos)
  d$afinn_neg_scaled <- scale(d$afinn_neg)
  d$afinn_scale_scaled <- scale(d$afinn_scale)
  d$loughran_pos_scaled <- scale(d$loughran_pos)
  d$loughran_neg_scaled <- scale(d$loughran_neg)
  d$loughran_scale_scaled <- scale(d$loughran_scale)
  d$nrc_pos_scaled <- scale(d$nrc_pos)
  d$nrc_neg_scaled <- scale(d$nrc_neg)
  d$nrc_scale_scaled <- scale(d$nrc_scale)
  d$tidytext_pos_scaled <- scale(d$tidytext_pos)
  d$tidytext_neg_scaled <- scale(d$tidytext_neg)
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
  d$q[base::setdiff(has_ngss, has_ngsschat)] <- "ngss"
  d$q[which(d$dl_at > "2020-10-22 00:00:00 CEST")] <- "state-based-hashtags" # recent additions
  d$q[which(is.na(d$q))] <- "other" # fill in remaining
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

get_scales <- function(scale = "scale", scaled = TRUE){
  out <- c(
    "ss",
    "liwc",
    "tidytext"
  )
  if (scale == "pos") {out <- paste0(out, "_pos")}
  else if (scale == "neg") {out <- paste0(out, "_neg")}
  else {out <- paste0(out, "_scale")}
  if (scaled) {out <- paste0(out, "_scaled")}
  return(out)
}

add_pairwise_disc <- function(d, scale = "scale", scaled = TRUE){
  
  scales <- get_scales(scale, scaled)
  
  combinations <- t(combn(scales, 2))
  
  for (i in 1:nrow(combinations)){  # iterate through each combination and build squared diff
    name <- paste(
      combinations[i, 1] %>% strsplit("_") %>% unlist() %>% head(1),
      combinations[i, 2] %>% strsplit("_") %>% unlist() %>% head(1), 
      "discrepancy", scale, sep="_"
    )
    d$temp <- abs(d[,combinations[i, 1]] - d[,combinations[i, 2]])
    names(d)[names(d) == "temp"] <- name
  }
  return(d)
}

add_total_disc <- function(d, scale = "scale", scaled = TRUE){  
  
  scales <- get_scales(scale, scaled)
  
  combinations <- t(combn(scales, 2))
  
  the_sum <- rep(0, nrow(d))
  n_combs <- rep(0, nrow(d))
  
  for (i in 1:nrow(combinations)){  
    temp <- abs(d[,combinations[i, 1]] - d[,combinations[i, 2]]) %>% unlist() %>% as.numeric()
    the_sum[which(!is.na(temp))] <- the_sum[which(!is.na(temp))] + temp[which(!is.na(temp))]
    n_combs[which(!is.na(temp))] <- n_combs[which(!is.na(temp))] + 1 # += 1 for available combination 
  } 
  
  name <- paste(
    "total", "discrepancy", scale, sep="_"
  )
  
  the_sum[the_sum == 0] <- NA     # reassign NAs for possible 0 coverage
  
  d$temp <- the_sum
  d$temp <- d$temp / n_combs  # normalize by number of combinations
  
  names(d)[names(d) == "temp"] <- name
  
  name <- paste(
    "n", "combinations", scale, sep="_"
  )
  
  d$temp <- n_combs  # for later coverage statistics
  
  names(d)[names(d) == "temp"] <- name
  
  return(d)
}

add_ambiguity_measure <- function(d){   # for robustness check and later exploration
  d$ss_ambi <- abs(d$ss_pos) + abs(d$ss_neg)
  d$liwc_ambi <- abs(d$liwc_pos) + abs(d$liwc_neg)
  d$tidytext_ambi <- abs(d$tidytext_pos) + abs(d$tidytext_neg)
  return(d)
}

discrepancy_master <- function(d){
  return(
    d %>%
      add_pairwise_disc(scale = "pos", scaled = TRUE) %>%
      add_total_disc(scale = "pos", scaled = TRUE) %>%
      add_pairwise_disc(scale = "neg", scaled = TRUE) %>%
      add_total_disc(scale = "neg", scaled = TRUE) %>%
      add_pairwise_disc(scale = "scale", scaled = TRUE) %>%
      add_total_disc(scale = "scale", scaled = TRUE) %>%
      add_ambiguity_measure()
  )
}

##### SAVE FINAL DATA SET #####

save_final_dataset <- function(d){
  saveRDS(d, here::here("data", "data_final.rds"))
  return(here::here("data", "data_final.rds"))
}

##### JOSH FUNCTIONS ######

extract_status_ids <- function(d) {
  statuses <- d$status_url %>% str_split("/") %>% map_chr(~.[6])
}

read_liwc_and_rename_input_cols <- function(path) {
  d <- read_csv(path)
  d <- d %>%
    rename(status_id = A,
           text = B) %>%
    select(status_id, posemo, negemo)
  d
}


join_id_string <- function(d, d_with_id) {
  left_join(d, d_with_id, by = "status_id")
}

access_manual_coding_data <- function(row_indices) {
  s1 <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1UOMJP4HUDDVlOs-i0orqyY_9Mf5s13yaZwzOa8yMqko/edit#gid=1614206599",
                                  sheet = 2) %>% 
    janitor::clean_names()
  
  s2 <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1UOMJP4HUDDVlOs-i0orqyY_9Mf5s13yaZwzOa8yMqko/edit#gid=1614206599",
                                  sheet = 3) %>% 
    janitor::clean_names()
  
  tibble(r1_pos = s1$positive_affect_1_5[row_indices], 
         r2_pos = s2$positive_affect_1_5[row_indices],
         r1_neg = s1$negative_affect_1_5[row_indices], 
         r2_neg = s2$negative_affect_1_5[row_indices])
  
}

calculate_manual_agreement <- function(agree_df) {
  
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

write_file_for_liwc <- function(d) {
  select(d, status_id, text) %>% 
    write_csv(here::here("data", "file-to-upload-to-liwc.csv"))
}

access_manual_coding_data_state_data <- function(row_indices) {
  s1 <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1UOMJP4HUDDVlOs-i0orqyY_9Mf5s13yaZwzOa8yMqko/edit#gid=1614206599",
                                  sheet = 7) %>% 
    janitor::clean_names()
  
  tibble(r1_pos = s1$josh_positive_affect_1_5[row_indices], 
         r2_pos = s1$macy_positive_affect_1_5[row_indices],
         r1_neg = s1$josh_negative_affect_1_5[row_indices], 
         r2_neg = s1$macy_negative_affect_1_5[row_indices])
  
}

access_consensus_codes_conrad <- function(ngsschat_indices, state_indices) {
  s1 <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1UOMJP4HUDDVlOs-i0orqyY_9Mf5s13yaZwzOa8yMqko/edit#gid=1614206599",
                                  sheet = 4, col_types = "ccccnnnncnncnncc") %>% 
    janitor::clean_names()
  
  s2 <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1UOMJP4HUDDVlOs-i0orqyY_9Mf5s13yaZwzOa8yMqko/edit#gid=1614206599",
                                  sheet = 7, col_types = "ccccnnnncnncnnc") %>% 
    janitor::clean_names()
  
  ngsschat_tweets <- s1 %>% 
    mutate(status_id = status_url %>% str_extract_all("([^/]+$)") %>% unlist) %>%
    mutate(created_at = NA) %>%
    select(status_id, created_at, text, consensus_pos, consensus_neg) %>% 
    slice(ngsschat_indices) %>% 
    mutate(consensus_pos = as.integer(consensus_pos),
           consensus_neg = as.integer(consensus_neg))
  
  state_tweets <- s2 %>% 
    select(status_id, created_at, text, consensus_pos, consensus_neg) %>% 
    slice(state_indices) %>% 
    mutate(consensus_pos = as.integer(consensus_pos),
           consensus_neg = as.integer(consensus_neg))
  
  bind_rows(ngsschat_tweets, state_tweets)
  
}

combine_coding_and_software_ratings <- function(d_agree, d_all, index_d_all){
  
  d_all <- d_all[index_d_all,]
  
  d_all <- d_all %>% select(status_id, created_at, ss_pos, ss_neg, ss_binary, liwc_binary, tidytext_binary)
  
  d_agree$identifyer <- NA
  
  d_agree$identifyer[!is.na(d_agree$created_at)] <- d_agree$created_at[!is.na(d_agree$created_at)] 
  
  d_agree$identifyer <- d_agree$identifyer %>% gsub(pattern="T", replacement = " ") %>%
    gsub(pattern="Z", replacement = " ") %>%
    as.POSIXct(tz = "UTC") %>% as.numeric()
  
  d_all$identifyer <- d_all$created_at %>% as.numeric()
  
  d_joined_by_time <- d_all %>% inner_join(d_agree, by = "identifyer")
  
  d_joined_by_status_id <- d_all %>% inner_join(d_agree, by = "status_id")
  
  d_joined_by_time <- d_joined_by_time %>% select(
    ss_pos, ss_neg, ss_binary, liwc_binary, tidytext_binary, consensus_pos, consensus_neg
  )
  
  d_joined_by_status_id <- d_joined_by_status_id %>% select(
    ss_pos, ss_neg, ss_binary, liwc_binary, tidytext_binary, consensus_pos, consensus_neg
  )
  
  d_validation <- rbind(d_joined_by_status_id, d_joined_by_time)
  
  d_validation$consensus_binary <- ifelse(d_validation$consensus_pos >= d_validation$consensus_neg, 1, 0)
  
  return(d_validation)
}

validation_master <- function(d){
  d$ss_binary <- factor(d$ss_binary, levels=c(-1, 1), labels=c(0, 1))
  d$liwc_binary <- as.factor(d$liwc_binary)
  d$tidytext_binary <- as.factor(d$tidytext_binary)
  d$consensus_binary <- as.factor(d$consensus_binary)
  print("SENTISTRENGTH")
  caret::confusionMatrix(data=d$consensus_binary, reference=d$ss_binary) %>% print
  print("LIWC")
  caret::confusionMatrix(data=d$consensus_binary, reference=d$liwc_binary) %>% print
  print("TIDYTEXT")
  caret::confusionMatrix(data=d$consensus_binary, reference=d$tidytext_binary) %>% print
  print("SCALE ICC WITH SENTISTRENGTH UNIDIMENSIONAL SCALES")
  print(
    tibble(scale = "pos", 
         agree = irr::agree(d[, c("ss_pos", "consensus_pos")])$value,
         icc = irr::icc(d[, c("ss_pos", "consensus_pos")])$value,
         kappa = irr::kappa2(d[, c("ss_pos", "consensus_pos")], weight = "squared")$value)
  )
  print(
    tibble(scale = "neg", 
           agree = irr::agree(d[, c("ss_neg", "consensus_neg")])$value,
           icc = irr::icc(d[, c("ss_neg", "consensus_neg")])$value,
           kappa = irr::kappa2(d[, c("ss_neg", "consensus_neg")], weight = "squared")$value)
  )
  d2 <- d[!is.na(d$tidytext_binary),]
  d2$sum_binary <- d2[,c("ss_binary", "liwc_binary", "tidytext_binary")] %>% apply(2, as.numeric) %>% rowSums
  d2$combined_binary <- ifelse(d2$sum_binary >= 2, 1, 0) %>% as.factor
  print("COMBINED BINARY RATINGS")
  caret::confusionMatrix(data=d2$consensus_binary, reference=d2$combined_binary) %>% print
}

join_id_string <- function(d, d_with_id) {
  left_join(d, d_with_id, by = "status_id")
}

access_manual_coding_data <- function(row_indices) {
  s1 <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1UOMJP4HUDDVlOs-i0orqyY_9Mf5s13yaZwzOa8yMqko/edit#gid=1614206599",
                                  sheet = 2) %>% 
    janitor::clean_names()
  
  s2 <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1UOMJP4HUDDVlOs-i0orqyY_9Mf5s13yaZwzOa8yMqko/edit#gid=1614206599",
                                  sheet = 3) %>% 
    janitor::clean_names()
  
  tibble(r1_pos = s1$positive_affect_1_5[row_indices], 
         r2_pos = s2$positive_affect_1_5[row_indices],
         r1_neg = s1$negative_affect_1_5[row_indices], 
         r2_neg = s2$negative_affect_1_5[row_indices])
  
}

access_manual_coding_data_state_data <- function(row_indices) {
  s1 <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1UOMJP4HUDDVlOs-i0orqyY_9Mf5s13yaZwzOa8yMqko/edit#gid=1614206599",
                                  sheet = 7) %>% 
    janitor::clean_names()
  
  tibble(r1_pos = s1$josh_positive_affect_1_5[row_indices], 
         r2_pos = s1$macy_positive_affect_1_5[row_indices],
         r1_neg = s1$josh_negative_affect_1_5[row_indices], 
         r2_neg = s1$macy_negative_affect_1_5[row_indices])
  
}

access_consensus_codes <- function(ngsschat_indices, state_indices) {
  s1 <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1UOMJP4HUDDVlOs-i0orqyY_9Mf5s13yaZwzOa8yMqko/edit#gid=1614206599",
                                  sheet = 4) %>% 
    janitor::clean_names()
  
  s2 <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1UOMJP4HUDDVlOs-i0orqyY_9Mf5s13yaZwzOa8yMqko/edit#gid=1614206599",
                                  sheet = 7) %>% 
    janitor::clean_names()
  
  ngsschat_tweets <- s1 %>% 
    select(status_url, consensus_pos, consensus_neg) %>% 
    slice(ngsschat_indices) %>% 
    mutate(consensus_pos = as.integer(consensus_pos),
           consensus_neg = as.integer(consensus_neg))
  
  state_tweets <- s2 %>% 
    select(status_id, consensus_pos, consensus_neg) %>% 
    slice(state_indices) %>% 
    mutate(consensus_pos = as.integer(consensus_pos),
           consensus_neg = as.integer(consensus_neg))
  
  bind_rows(ngsschat_tweets, state_tweets)
  
}

calculate_manual_agreement <- function(agree_df) {
  
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

write_file_for_liwc <- function(d) {
  select(d, status_id, text) %>% 
    write_csv(here::here("data", "file-to-upload-to-liwc.csv"))
}

write_state_hashtags_file_for_liwc <- function(d) {
  select(d, status_id, text) %>% 
    write_csv(here::here("data", "state-hashtags-file-to-upload-to-liwc.csv"))
}

join_raw_and_google_sheets_data <- function(raw_data) {
  s1 <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1UOMJP4HUDDVlOs-i0orqyY_9Mf5s13yaZwzOa8yMqko/edit#gid=1614206599",
                                  sheet = 7) %>% 
    janitor::clean_names() %>% 
    select(-status_id)
  
  raw_data <- read_csv(raw_data,
                       col_types = cols(
                         status_id = col_character()
                       )) %>% 
    select(status_id)
  
  bind_cols(raw_data, s1)
  
}


find_indexes_master <- function(d_all, d_agree){
  
  index <- which(d_all$status_id %in% d_agree$status_id)
  
  d_states <- d_agree[!(is.na(d_agree$created_at)),]
  d_states$created_at <- d_states$created_at %>% gsub(pattern="T", replacement = " ") %>%
                          gsub(pattern="Z", replacement = " ") %>%
                          as.POSIXct(tz = "UTC")
  
  inds <- which(d_all$created_at %in% d_states$created_at)
  
  remove_ambiguous_inds <- d_all$created_at[which(d_all$created_at %in% d_states$created_at)] %>% duplicated %>% which
  
  inds <- inds[-remove_ambiguous_inds]
  
  index <- c(index, inds) %>% unique %>% head(100)   # now we got 100!
  
  return(index)
}
