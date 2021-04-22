# 1) Merge all Tweet API responses, save to /data

library(dplyr)

fns <- dir(here::here("data-raw"), pattern=".rda")
  i <- 1
  object_list <- list()
  for (fn in fns){
    load(here::here("data-raw", fn))
    tweets_dl$dl_at <- file.info(here::here("data-raw", fn))$mtime
    object_list[[i]] <- tweets_dl %>% select(c("status_id", "user_id", "text", 
                   "created_at", "dl_at", "is_retweet", "is_quote", 
                   "favorite_count", "retweet_count", "lang"))
    i <- i+1
    cat("\014", round(which(fn == fns)/length(fns)*100, 0), "% files read in\n")
  }
  d <- do.call(rbind, object_list)
  d <- d[!duplicated(d$status_id),]


d2 <- readRDS(here::here("data-raw", "state-based-twitter-hashtags-raw-data.rds"))
d2$dl_at <- NA
d2 <- d2 %>% select(c("status_id", "user_id", "text", 
                   "created_at", "dl_at", "is_retweet", "is_quote", 
                   "favorite_count", "retweet_count", "lang"))


d <- rbind(d, d2); rm(d2)
d <- d[!duplicated(d$status_id),]
d <- d[order(d$created_at),]

# This file will be the reference input for all sentiment measures
saveRDS(d, "./data/raw_all.rds")

# 2) SentiStrength

texts <- readRDS("./data/raw_all.rds")$text
texts <- gsub("[\r\n]", "", texts) # remove line breaks

write.table(texts, "sentistrength_input.txt", row.names = F, col.names = F)

# Make sure to optionally remove old versions before applying the following commands

system('powershell -command "java -jar SentiStrengthCom.jar sentidata C:/SentiStrength_Data/ input sentistrength_input.txt"') # scale
system('powershell -command "ren sentistrength_input0_out.txt sentistrength_scale.txt"')
system('powershell -command "mv sentistrength_scale.txt data"')

system('powershell -command "java -jar SentiStrengthCom.jar sentidata C:/SentiStrength_Data/ input sentistrength_input.txt binary"') # binary
system('powershell -command "ren sentistrength_input0_out.txt sentistrength_binary.txt"')
system('powershell -command "mv sentistrength_binary.txt data"')

system('powershell -command "java -jar SentiStrengthCom.jar sentidata C:/SentiStrength_Data/ input sentistrength_input.txt trinary"') # trinary
system('powershell -command "ren sentistrength_input0_out.txt sentistrength_trinary.txt"')
system('powershell -command "mv sentistrength_trinary.txt data"')

# 3) LIWC

texts <- readRDS("./data/raw_all.rds")$text
texts <- gsub("[\r\n]", "", texts) # remove line breaks

write.csv(texts, "liwc_input.csv", row.names=F, col.names=F)

# Text rating is then executed in LIWC GUI for posemo and negemo ratings
# With LIWC's internal English Dictionary Version 2015 to "data/liwc_output.csv"

# 4) VADER

library(dplyr)
library(vader)

d <- readRDS("./data/raw_all.rds")$text

# Apply vader by chunks of size 500 in order to be able to save progress

chunk <- 1000
n <- length(d)
r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
d <- split(d,r)

res <- list()

# Start from last i, if first session, set start = 1 manually 
# start <- 1

l <- readRDS("vader_current.rds")
res <- l[[1]]; start <- l[[2]]; d <- l[[3]]

apply_vader <- function(d) d %>% vader_df() %>% select(pos, neg, compound)

cat(format(Sys.time(), "%a %b %d %X %Y"))
for (i in start:length(d)) {
    cat(format(Sys.time(), "%a %b %d %X %Y"))
    res[[i]] <- d[[i]] %>% apply_vader()
    cat("\014", i, "out of", length(d), "done\n")
}
cat(format(Sys.time(), "%a %b %d %X %Y"))

# 10 s for 1 out of 1455 chunks
# (10*1455)/3600 ~ 4 hours
# tidytext: 3 minutes on this PC
# (10*1455)/60 = 242 minutes / 3 = 80 times slower

# Optionally stop and start from saved file later
# saveRDS(list(res, i, d), "vader_current.rds")

# check: purrr::map(res, nrow) %>% unlist() %>% table() # all @ 500 if ok

saveRDS(do.call(rbind, res), "./data/vader-sentiment.rds")

# 5) Tidytext

library(dplyr)
library(tidyr)
library(tidytext)

d <- readRDS("./data/raw_all.rds")

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

create_tidytext_trinaries <- function(d){  
  d$bing_trinary <-  ifelse(d$bing_scale>0, 1, ifelse(d$bing_scale<0, -1, 0))
  d$afinn_trinary <- ifelse(d$afinn_scale>0, 1, ifelse(d$afinn_scale<0, -1, 0))
  d$loughran_trinary <- ifelse(d$loughran_scale>0, 1, ifelse(d$loughran_scale<0, -1, 0))
  d$nrc_trinary <- ifelse(d$nrc_scale>0, 1, ifelse(d$nrc_scale<0, -1, 0))
  d$tidytext_trinary <- ifelse(d$tidytext_scale>0, 1, ifelse(d$tidytext_scale<0, -1, 0))
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
      create_tidytext_binaries() %>%
      create_tidytext_trinaries()
  )
}

d <- d %>% tidytext_master() %>%
    select(status_id, starts_with("bing"),
           starts_with("afinn"), starts_with("loughran"), 
           starts_with("nrc"), starts_with("tidytext"))

saveRDS(d, "./data/tidytext-sentiment.rds")

# 6) Create and save final dataset to feed into targets pipeline

library(dplyr)
library(tidyr)
library(stringr)

d <- readRDS("./data/raw_all.rds")

# SentiStrength

temp <- read.table("./data/sentistrength_scale.txt", sep="\t", header = T, quote="")
d$ss_pos <- temp[,1]
d$ss_neg <- temp[,2]
d$ss_scale <- d$ss_pos + d$ss_neg

temp <- read.table("./data/sentistrength_binary.txt", sep="\t", header = T, quote="")
d$ss_binary <- temp[,1]

temp <- read.table("./data/sentistrength_trinary.txt", sep="\t", header = T, quote="")
d$ss_trinary <- temp[,1]

# LIWC

temp <- readLines("./data/liwc_output.csv")[-1] # skip first line
temp <- temp %>% str_extract_all("[0-9]{1,}\\.[0-9]{2}\\,[0-9]{1,}\\.[0-9]{2}$") %>% 
    unlist() 
temp <- reshape2::colsplit(temp, ",", names = c("posemo", "negemo"))

d$liwc_pos <- temp$posemo
d$liwc_neg <- temp$negemo
d$liwc_scale <- d$liwc_pos - d$liwc_neg
d$liwc_binary <- ifelse(d$liwc_scale>=0, 1, 0)
d$liwc_trinary <- ifelse(d$liwc_scale>0, 1, ifelse(d$liwc_scale<0, -1, 0))

# VADER

temp <- readRDS("./data/vader-sentiment.rds")
d$vader_pos <- temp$pos
d$vader_neg <- temp$neg
d$vader_scale <- temp$compound
d$vader_binary<- ifelse(d$vader_scale >= 0, 1, 0)
d$vader_trinary <- ifelse(d$vader_scale>0, 1, ifelse(d$vader_scale<0, -1, 0))

# Tidytext

temp <- readRDS("./data/tidytext-sentiment.rds")

d <- d %>% left_join(temp, by="status_id")

# Save final study sample

saveRDS(d, paste0("./data/study-sample-", Sys.Date(), ".rds"))

