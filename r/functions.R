#d <- tar_read(raw_data)

read_csv_with_col_types <- function(d) {
  read_csv(d, col_types = cols(
    .default = col_character(),
    `Facebook Id` = col_double(),
    Likes = col_double(),
    Comments = col_double(),
    Shares = col_double(),
    Love = col_double(),
    Wow = col_double(),
    Haha = col_double(),
    Sad = col_double(),
    Angry = col_double(),
    Care = col_double(),
    `Post Views` = col_double(),
    `Total Views` = col_double(),
    `Total Views For All Crossposts` = col_double(),
    `Overperforming Score` = col_double()
  )
  )
}

prep_data <- function(data) {
  data %>% 
    clean_names() %>% 
    mutate(created = lubridate::as_datetime(created))
}