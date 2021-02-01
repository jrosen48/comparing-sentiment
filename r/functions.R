##### Cleaning Data #####

preprocess_text <- function(d){
  d$text_clean <- d$text %>% 
    gsub(pattern="https\\S*", replacement="") %>%   # urls
    gsub(pattern="http\\S*", replacement="") %>%    # urls
    #gsub(pattern="@\\S*", replacement="") %>%       # tagging
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

true_retweets <- function(d){
  d$is_retweet <- ifelse(grepl("rt @", d$text %>% tolower()) | d$is_retweet, 1, 0)
  return(d)
}

fill_dictionaries_with_zero <- function(d) {
  d$bing_pos[is.na(d$bing_pos)] <- 0
  d$bing_neg[is.na(d$bing_neg)] <- 0
  d$bing_scale[is.na(d$bing_scale)] <- 0
  d$bing_binary <- ifelse(d$bing_scale >= 0, 1, 0)
  d$bing_trinary <- ifelse(d$bing_scale>0, 1, ifelse(d$bing_scale<0, -1, 0))
  
  d$afinn_pos[is.na(d$afinn_pos)] <- 0
  d$afinn_neg[is.na(d$afinn_neg)] <- 0
  d$afinn_scale[is.na(d$afinn_scale)] <- 0
  d$afinn_binary <- ifelse(d$afinn_scale >= 0, 1, 0)
  d$afinn_trinary <- ifelse(d$afinn_scale>0, 1, ifelse(d$afinn_scale<0, -1, 0))
  
  d$loughran_pos[is.na(d$loughran_pos)] <- 0
  d$loughran_neg[is.na(d$loughran_neg)] <- 0
  d$loughran_scale[is.na(d$loughran_scale)] <- 0
  d$loughran_binary <- ifelse(d$loughran_scale >= 0, 1, 0)
  d$loughran_trinary <- ifelse(d$loughran_scale>0, 1, ifelse(d$loughran_scale<0, -1, 0))
  
  d$nrc_pos[is.na(d$nrc_pos)] <- 0
  d$nrc_neg[is.na(d$nrc_neg)] <- 0
  d$nrc_scale[is.na(d$nrc_scale)] <- 0
  d$nrc_binary <- ifelse(d$nrc_scale >= 0, 1, 0)
  d$nrc_trinary <- ifelse(d$nrc_scale>0, 1, ifelse(d$nrc_scale<0, -1, 0))
  
  d$tidytext_pos[is.na(d$tidytext_pos)] <- 0
  d$tidytext_neg[is.na(d$tidytext_neg)] <- 0
  d$tidytext_scale[is.na(d$tidytext_scale)] <- 0
  d$tidytext_binary <- ifelse(d$tidytext_scale >= 0, 1, 0)
  d$tidytext_trinary <- ifelse(d$tidytext_scale>0, 1, ifelse(d$tidytext_scale<0, -1, 0))
  
  # 13 VADER responses were erroneous and will be filled with (0, 0)
  
  d$vader_pos[is.na(d$vader_pos)] <- 0
  d$vader_neg[is.na(d$vader_neg)] <- 0
  d$vader_scale[is.na(d$vader_scale)] <- 0
  d$vader_binary <- ifelse(d$vader_scale >= 0, 1, 0)
  d$vader_trinary <- ifelse(d$vader_scale>0, 1, ifelse(d$vader_scale<0, -1, 0))
  
  return(d)
}

clean_master <- function(d){
  d %>%
    preprocess_text() %>%
    remove_langs() %>%
    true_retweets() %>% 
    fill_dictionaries_with_zero()
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

correct_tidytext_by_nwords <- function(d){
  d$tidytext_pos <- d$tidytext_pos/d$nwords
  d$tidytext_neg <- d$tidytext_neg/d$nwords
  d$tidytext_scale <- d$tidytext_scale/d$nwords
  return(d)
}

add_ambiguity_measure <- function(d) {
  d$ss_ambi <- abs(d$ss_pos)+abs(d$ss_neg)
  d$liwc_ambi <- abs(d$liwc_pos)+abs(d$liwc_neg)
  d$tidytext_ambi <- abs(d$tidytext_pos)+abs(d$tidytext_neg)
  d$vader_ambi <- abs(d$vader_pos)+abs(d$vader_neg)
  return(d)
}

add_vars_master <- function(d){
  return(
    d %>%
      add_nchar() %>%
      add_nwords() %>% 
      correct_tidytext_by_nwords() %>%   # this is still technically cleaning, but well...
      add_ambiguity_measure()
  )
}

##### ADD TWEET CONTEXT VARIABLE AND IDENTIFY CHATS ####

add_q <- function(d){
  text_small <- d$text %>% tolower()
  has_ngsschat <- grep("\\#ngsschat\\b", text_small)
  has_ngss <- grep("\\#ngss\\b|\\bngss\\b", text_small)
  d$q <- NA
  d$q[has_ngsschat] <- "#NGSSchat"
  d$q[base::setdiff(has_ngss, has_ngsschat)] <- "ngss"
  d$q[base::setdiff(which(is.na(d$dl_at)), c(has_ngsschat, has_ngss))] <- "state-based-hashtags" # see "create_study_data.R"
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

##### ENDPOINT A VALIDATION #####

merge_and_export_validation <- function(d, v) {
  
  d <- d %>%
    select(status_id, nwords, ss_pos, ss_neg, ends_with("binary"), ends_with("trinary"))
  
  out <- v %>% left_join(d, by="status_id")
  
  out$binary <- ifelse(out$pos>=out$neg, 1, 0)
  out$trinary <- ifelse(out$pos>out$neg, 1, ifelse(out$pos<out$neg, -1, 0))
  
  out <- out %>% select(pos, neg, binary, trinary, everything(), -status_id)
  
  out$ss_binary[out$ss_binary==-1] <- 0 # consistent encoding
  out$ss_neg <- -(out$ss_neg)
  
  path <- "./data/validation.csv"
  
  readr::write_csv(out, path)
  
  return(path)
  
}

##### B SELECT DESCRIPTIVE VARS #####

select_descriptive_vars <- function(d){
  d %>% select(
    ends_with("_id"), ends_with("_at"), starts_with("is_"),
    ends_with("count"), nchar, nwords, q, isChat,
    ends_with("scale"), ends_with("pos"), ends_with("neg"),
    ends_with("ambi")
  )
}


##### C SCALE DISCREPANCY ANALYSIS #####

# Result remains invariant for n>1 applications
fill_this <- function(x) { x[is.na(x)]<-0; x }
scale_this <- function(x) { x<-x%>%unlist()%>%as.numeric(); (x-mean(x, na.rm=T))/sd(x, na.rm=T)%>%as.numeric() }

get_scales <- function(scale = "scale"){
  out <- c(
    "ss",
    "liwc",
    "tidytext",
    "vader"
  )
  if (scale == "pos") {out <- paste0(out, "_pos")}
  else if (scale == "neg") {out <- paste0(out, "_neg")}
  else {out <- paste0(out, "_scale")}
  return(out)
}

add_pairwise_disc <- function(d, scale = "scale", fill_scales=TRUE, scale_scales=TRUE){
  
  scales <- get_scales(scale)
  
  if (fill_scales) for (scale_i in scales) d[,scale_i] <- d[,scale_i] %>% fill_this()
  if (scale_scales) for (scale_i in scales) d[,scale_i] <- d[,scale_i] %>% scale_this()
  
  combinations <- t(combn(scales, 2))
  
  for (i in 1:nrow(combinations)){  
    name <- paste(
      combinations[i, 1] %>% strsplit("_") %>% unlist() %>% head(1),
      combinations[i, 2] %>% strsplit("_") %>% unlist() %>% head(1), 
      "disc", scale, sep="_"
    )
    d$temp <- abs(d[,combinations[i, 1]] - d[,combinations[i, 2]])
    names(d)[names(d) == "temp"] <- name
  }
  return(d)
}

add_total_disc <- function(d, scale = "scale", fill_scales=TRUE, scale_scales=TRUE){  
  
  scales <- get_scales(scale)
  
  if (fill_scales) for (scale_i in scales) d[,scale_i] <- d[,scale_i] %>% fill_this()
  if (scale_scales) for (scale_i in scales) d[,scale_i] <- d[,scale_i] %>% scale_this()
  
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
  d$temp <- d$temp / n_combs  # normalize by number of available combinations
  
  names(d)[names(d) == "temp"] <- name
  
  return(d)
}

add_ambiguity_measure <- function(d){   # for robustness check and later exploration
  d$ss_ambi <- abs(d$ss_pos) + abs(d$ss_neg)
  d$liwc_ambi <- abs(d$liwc_pos) + abs(d$liwc_neg)
  d$tidytext_ambi <- abs(d$tidytext_pos) + abs(d$tidytext_neg)
  d$vader_ambi <- abs(d$vader_pos) + abs(d$vader_neg)
  return(d)
}

discrepancy_master <- function(d){
  return(
    d %>%
      add_pairwise_disc(scale = "pos") %>%
      add_total_disc(scale = "pos") %>%
      add_pairwise_disc(scale = "neg") %>%
      add_total_disc(scale = "neg") %>%
      add_pairwise_disc(scale = "scale") %>%
      add_total_disc(scale = "scale") %>%
      add_ambiguity_measure()
  )
}

##### REFERENCE MIGHT USE LATER ######

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

#  print("SENTISTRENGTH")
#  caret::confusionMatrix(data=d$consensus_binary, reference=d$ss_binary) %>% print
#  print("LIWC")
#  caret::confusionMatrix(data=d$consensus_binary, reference=d$liwc_binary) %>% print
#  print("TIDYTEXT")
#  caret::confusionMatrix(data=d$consensus_binary, reference=d$tidytext_binary) %>% print
#  print("SCALE ICC WITH SENTISTRENGTH UNIDIMENSIONAL SCALES")
#  print(
#    tibble(scale = "pos", 
#         agree = irr::agree(d[, c("ss_pos", "consensus_pos")])$value,
#         icc = irr::icc(d[, c("ss_pos", "consensus_pos")])$value,
#         kappa = irr::kappa2(d[, c("ss_pos", "consensus_pos")], weight = "squared")$value)
#  )
#  print(
#    tibble(scale = "neg", 
#           agree = irr::agree(d[, c("ss_neg", "consensus_neg")])$value,
#           icc = irr::icc(d[, c("ss_neg", "consensus_neg")])$value,
#           kappa = irr::kappa2(d[, c("ss_neg", "consensus_neg")], weight = "squared")$value)
 # )
#  d2 <- d[!is.na(d$tidytext_binary),]
#  d2$sum_binary <- d2[,c("ss_binary", "liwc_binary", "tidytext_binary")] %>% apply(2, as.numeric) %>% rowSums
#  d2$combined_binary <- ifelse(d2$sum_binary >= 2, 1, 0) %>% as.factor
#  d2$combined_binary <- factor(d2$combined_binary, levels=c(1, 0)) 
#  
#  print("COMBINED BINARY RATINGS")
#  caret::confusionMatrix(data=d2$consensus_binary, reference=d2$combined_binary) %>% print
#
