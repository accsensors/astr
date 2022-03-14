library(lutz)
library(dplyr)
library(usethis)

unique_tz_list <- lutz::tz_list() %>%
  dplyr::distinct(utc_offset_h, .keep_all = TRUE)

usethis::use_data(unique_tz_list, overwrite = TRUE)
