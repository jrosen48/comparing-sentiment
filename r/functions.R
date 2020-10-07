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
  return(d)
}


test_f <- function() {
  "works!"
}