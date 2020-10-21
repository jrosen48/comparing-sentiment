##### READING IN AND COMBINING RAW DATA FILES #####

additional_data_prep <- function(d){
  d$dl_at <- NA
  return(
    d %>% remove_variables()
  )
}

remove_variables <- function(d){
  return(
    d %>% select(c("status_id", "user_id", "text", "created_at", "dl_at",
                   "is_retweet", "is_quote",
                   "favorite_count", "retweet_count", "lang", 
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
  d$ss_scale_scaled <- d$ss_scale %>% scale()
  d$ss_binary <- ss_binary_data[,1]
  return(d)
}

add_liwc <- function(d, liwc_data){
  d$liwc_pos <- liwc_data$posemo
  d$liwc_neg <- liwc_data$negemo
  d$liwc_scale <- d$liwc_pos - d$liwc_neg
  d$liwc_scale_scaled <- d$liwc_scale %>% scale()
  d$liwc_binary <- ifelse(d$liwc_scale >= 0, 1, 0)
  return(d)
}

add_external_master <- function(d, ss_scale_data, ss_binary_data, liwc_data){
  return(
    d %>%
      add_sentistrength(ss_scale_data=ss_scale_data, ss_binary_data=ss_binary_data) %>%
      add_liwc(liwc_data=liwc_data) 
  )
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

add_total_disc <- function(d){  
  s_scales <- get_scales()
  
  combinations <- t(combn(s_scales, 2))
  
  the_sum <- rep(0, nrow(d))
  n_combs <- rep(0, nrow(d))
  
  for (i in 1:nrow(combinations)){  
    temp <- (d[,combinations[i, 1]] - d[,combinations[i, 2]])^2 %>% unlist() %>% as.numeric()
    the_sum[which(!is.na(temp))] <- the_sum[which(!is.na(temp))] + temp[which(!is.na(temp))]
    n_combs[which(!is.na(temp))] <- n_combs[which(!is.na(temp))] + 1 # += 1 for available combination 
  } 
  
  the_sum[the_sum == 0] <- NA     # reassign NAs for possible 0 coverage
  d$total_discrepancy <- the_sum
  
  d$total_discrepancy <- d$total_discrepancy / n_combs  # normalize by number of combinations
  d$n_combs <- n_combs  # for later coverage statistics
  
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
      add_pairwise_disc() %>%
      add_total_disc() %>%
      add_ambiguity_measure()
  )
}

##### SAVE FINAL DATA SET #####

save_final_dataset <- function(d){
  saveRDS(d, here::here("data", "data_final.rds"))
  return(here::here("data", "data_final.rds"))
}

##### DESCRIPTIVES #####

# to be continued...

# Sample

descriptives_sample <- function(d){
  print("descriptives_sample")
  print(nrow(d))
  print(d$user_id %>% unique() %>% length())
  print(sum(d$q == "#NGSSchat"))
}

# Dictionary coverage

descriptives_coverage <- function(d){
  print("descriptives_coverage")
  print((d$bing_scale %>% is.na() %>% `!` %>% sum() * 100 / nrow(d)) %>% round(2))
  print((d$afinn_scale %>% is.na() %>% `!` %>% sum() * 100 / nrow(d)) %>% round(2))
  print((d$loughran_scale %>% is.na() %>% `!` %>% sum() * 100 / nrow(d)) %>% round(2))
  print((d$nrc_scale %>% is.na() %>% `!` %>% sum() * 100 / nrow(d)) %>% round(2))
  print((d$tidytext_scale %>% is.na() %>% `!` %>% sum() * 100 / nrow(d)) %>% round(2)) # great coverage
}

# Ambiguity statistic and plausibility checks

descriptives_ambiguity <- function(d){
  print("descriptives_ambiguity")
  print(d$ss_ambi %>% summary())
  print(d$liwc_ambi %>% summary())
  print(d$tidytext_ambi %>% summary())  # liwc and tidytext both have outliers, ss truncates
  
  print(cbind(d$ss_ambi, d$liwc_ambi, d$tidytext_ambi) %>% cor(use="pairwise.complete.obs")) # liwc again closer to dicts
}

# Sentiment scale correlations

descriptives_scale_correlations <- function(d){
  print("descriptives_scale_correlations")
  print(cbind(d$ss_scale, d$liwc_scale, d$tidytext_scale) %>% cor(use="pairwise.complete.obs")) # scales closer than ambiguity
}

# Normality checks

# Wordclouds for different scale values

# Hand coding confusion matrices validation, compare to discrepancy and ambiguity checks

# Discrepancy magnitude and order, which scales are closer together?

descriptives_disc_pairs <- function(d){
  print("descriptives_disc_pairs")
  ind <- grep("discrepancy", names(d))
  
  mean_disc <- NULL
  
  for (i in ind){
    mean_disc <- rbind(mean_disc, cbind(names(d)[i], d[,i] %>% unlist %>% mean(na.rm=T)))
  }
  
  mean_disc[,2] <- mean_disc[,2] %>% as.numeric() %>% sqrt()  # to interpret as sd difference, test also leaving out ^2 to the direction of bias
  mean_disc <- mean_disc[order(mean_disc[,2]),]
  
  print(mean_disc)  # ss and liwc rather inconsistent, liwc closer to tidytext than ss, tidytext closest dict to ss+liwc
}

discrepancy_context_by_method <- function(d){
  print("discrepancy_context_by_method")
  print(aggregate(total_discrepancy %>% unlist() ~ q, d, mean))
  print(aggregate(ss_liwc_discrepancy %>% unlist() ~ q, d, mean))
  print(aggregate(ss_tidytext_discrepancy %>% unlist() ~ q, d, mean))
  print(aggregate(liwc_tidytext_discrepancy %>% unlist() ~ q, d, mean))
}

ambiguity_context_by_method <- function(d){
  print("ambiguity_context_by_method")
  print(aggregate(ss_ambi ~ q, d, mean))
  print(aggregate(liwc_ambi ~ q, d, mean))
  print(aggregate(tidytext_ambi ~ q, d, mean))
}

descriptives_master <- function(d){
  d %>%
    descriptives_sample()
  d %>%
    descriptives_coverage()
  d %>%
    descriptives_ambiguity()
  d %>%
    descriptives_scale_correlations()
  d %>%
    descriptives_disc_pairs()
  d %>%
    discrepancy_context_by_method()
  d %>%
    ambiguity_context_by_method()
}

##### ANALYSIS #####

analysis_master <- function(d){   # just an example
  print("analysis_master")
  print(cor.test(d$total_discrepancy, d$nwords))
  print(cor.test(d$total_discrepancy, d$nchar))
  print(cor.test(d$total_discrepancy, d$favorite_count))
  print(cor.test(d$total_discrepancy, d$retweet_count))
  print(cor.test(d$total_discrepancy, d$ss_ambi))
  print(cor.test(d$total_discrepancy, d$liwc_ambi))
  print(cor.test(d$total_discrepancy, d$tidytext_ambi))  # wow! we need to check cook's distance of these
}

extract_status_ids <- function(d) {
  statuses <- d$status_url %>% str_split("/") %>% map_chr(~.[6])
}

read_liwc_and_rename_input_cols <- function(path) {
  d <- read_csv(path)
  d <- d %>%
    rename(status_id = A,
           text = B)
  d
}

get_replies_recursive <- function(statuses) {
  
  statuses <- statuses[!is.na(statuses)]
  
  new_data <- rtweet::lookup_statuses(statuses)
  
  print(paste0("In this iteration, accessed ", nrow(new_data), " new Tweets"))
  
  new_statuses <- new_data$reply_to_status_id[!is.na(new_data$reply_to_status_id)]
  
  if (length(new_statuses) > 0) { # if there are replies to statuses not yet in the data
    new_data_recursive <- get_replies_recursive(new_statuses) # get the tweets that were replied to
    out_data <- bind_rows(new_data, new_data_recursive) # and bind together the replies and the original tweets
  } else { # if there are no replies left to get
    return(new_data) # return the replies
  }
}

thread_finder <- function(status_id, d, out_statuses = NULL) {
  
  status_is_a_reply_to <- as.character(d[d$status_id == status_id, ]$reply_to_status_id)
  
  status_is_a_reply_to <- ifelse(length(status_is_a_reply_to) == 0, NA, status_is_a_reply_to)
  
  if (!is.na(status_is_a_reply_to)) {
    
    if (is.null(out_statuses)) {
      out_statuses <- c(status_id, status_is_a_reply_to)  
    } else {
      out_statuses <- c(out_statuses, status_is_a_reply_to)
    }
    
    thread_finder(status_is_a_reply_to, d, out_statuses)  
  } else {
    out_statuses
  }
}

remove_short_threads <- function(thread, d, i) {
  same_thread <- which(str_detect(d$thread_string, thread))
  
  same_thread_df <- d[same_thread, "thread_string"]
  
  same_thread_df <- same_thread_df %>%
    mutate(length_of_string = nchar(thread_string)) %>%
    arrange(desc(length_of_string))
  
  the_longest_thread <- pull(same_thread_df[1, "thread_string"])
  
  if (!(the_longest_thread == thread)) {
    remove_short_threads(the_longest_thread, d = d, i = i)
  } else {
    the_longest_thread
  }
}

identify_threads <- function(d) {
  threads <- map(d$status_id, thread_finder, d = d)
  
  thread_list <- tibble(ID = 1:length(threads),
                        thread = threads)
  
  thread_df <- thread_list %>% 
    unnest(thread) %>% 
    group_by(ID) %>% 
    mutate(thread_string = toString(thread)) %>% 
    select(ID, thread_string) %>% 
    distinct(thread_string, .keep_all = TRUE) %>% 
    ungroup() %>% 
    mutate(row_number = 1:nrow(.))
  
  # not necessary in this case, but necessary in some - same n as before
  thread_df <- thread_df %>% 
    unique()
  
  # recursively searching for shorter version of longer threads
  # 513 unique threads because some are shorter versions of longer ones
  shorter_thread_list <- map2(.x = thread_df$thread_string, .f = remove_short_threads, d = thread_df, .y = 1:nrow(thread_df))
  
  # this creates a df with every status ID and what thread they're part of
  new_thread_df <- tibble(ID = 1:length(shorter_thread_list),
                          thread_string = map_chr(shorter_thread_list, ~.)) %>% 
    select(-ID) %>% # this gets rid of our old ID
    left_join(thread_df, by = "thread_string") %>% 
    select(ID, thread_string) %>% # this uses the ID from our larger df
    mutate(thread_string = str_split(thread_string, ", ")) %>% 
    unnest(thread_string) %>% 
    rename(status_id = thread_string) %>%
    group_by(status_id) %>% 
    summarize(id_string = toString(ID)) %>% 
    mutate(id_string = str_split(id_string, ", ")) %>% 
    unnest(id_string) %>% 
    mutate(status_id = as.character(status_id)) %>% 
    mutate(id_string = str_pad(id_string, 4, pad = "0")) %>% 
    mutate(status_id = as.character(status_id)) %>% 
    distinct() %>% 
    arrange(id_string)
  
  # putting together the final dataset
  out_df <- d %>% 
    left_join(new_thread_df, by = "status_id") %>% 
    arrange(id_string)
  
  out_df <- out_df %>% 
    select(status_id, id_string)
  
  out_df
}

join_id_string <- function(d, d_with_id) {
  left_join(d, d_with_id, by = "status_id")
}

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