library(data.table)
library(dplyr)
library(usethis)

filename <- 'PSP00055_LOG_2022-02-24T19_26_03UTC_test1___________----------.txt'
file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)

###########
# upasv2x_header
###########

upasv2x_header <- read_ast_header(file)
usethis::use_data(upasv2x_header, overwrite = TRUE)

###########
# data_ast_raw
###########

data_ast_raw <- data.table::fread(file=file, sep=',',
                                  header = FALSE, fill = TRUE, blank.lines.skip = TRUE,
                                  stringsAsFactors = FALSE)

usethis::use_data(data_ast_raw, overwrite = TRUE)

###########
# upasv2x_header_raw
###########

my_cols <- c('V1','V2')

df_h <- data_ast_raw %>%
  dplyr::select(my_cols)

df_h <- df_h[2:(which(df_h$V1=="SAMPLE LOG")-1),]

remove_names <- c("SAMPLE IDENTIFICATION","SETUP SUMMARY",
                  "SAMPLE IDENTIFICATION","SAMPLE SUMMARY",
                  "MASS FLOW SENSOR CALIBRATION")

df_h <- df_h[ grep(paste(remove_names,collapse="|"), df_h$V1, invert = TRUE) , ]

df_h <- df_h %>%
  t()

df_h <- as.data.frame(df_h)

colnames(df_h) <- df_h[1, ]
df_h <- df_h[-1, ]
rownames(df_h) <- c(1)

upasv2x_header_raw <- df_h %>%
  dplyr::rename('Firmware' = names(df_h[stringr::str_detect(names(df_h), 'firmware')]) )


usethis::use_data(upasv2x_header_raw, overwrite = TRUE)

###########
# upasv2x_log_raw
###########

df_cols <- data_ast_raw %>%
  dplyr::slice(which(data_ast_raw$V1=="SAMPLE LOG")+2) %>%
  unlist(use.names = FALSE)

upasv2x_log_raw <- data_ast_raw %>%
  dplyr::slice(which(data_ast_raw$V1=="SAMPLE LOG")+4:dplyr::n())

colnames(upasv2x_log_raw) <- df_cols

usethis::use_data(upasv2x_log_raw, overwrite = TRUE)

