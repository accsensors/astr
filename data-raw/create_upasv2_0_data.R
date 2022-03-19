library(data.table)
library(dplyr)
library(usethis)

###########
# upasv2_header_raw
###########

filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)

df_raw <- data.table::fread(file=file, sep=',',
                            header = FALSE, fill = TRUE, blank.lines.skip = TRUE,
                            stringsAsFactors = FALSE)

my_cols <- c('V1','V2')

df_h <- df_raw %>%
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

upasv2_header_raw <- df_h

if(sum(stringr::str_detect(names(upasv2_header_raw),'firmware'))==1){
  upasv2_header_raw <- upasv2_header_raw %>%
    dplyr::rename('Firmware' = names(upasv2_header_raw[stringr::str_detect(names(upasv2_header_raw), 'firmware')]) )
  }

usethis::use_data(upasv2_header_raw, overwrite = TRUE)


###########
# upasv2_header
###########
upasv2_filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
upasv2_header <- read_ast_header(upasv2_file, update_names=FALSE)

usethis::use_data(upasv2_header, overwrite = TRUE)

###########
# upasv2_log_raw
###########

filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)

df_raw <- data.table::fread(file=file, sep=',',
                            header = FALSE, fill = TRUE, blank.lines.skip = TRUE,
                            stringsAsFactors = FALSE)

df_cols <- df_raw %>%
  dplyr::slice(which(df_raw$V1=="SAMPLE LOG")+2) %>%
  unlist(use.names = FALSE)

upasv2_log_raw <- df_raw %>%
  dplyr::slice(which(df_raw$V1=="SAMPLE LOG")+4:dplyr::n())

colnames(upasv2_log_raw) <- df_cols

usethis::use_data(upasv2_log_raw, overwrite = TRUE)
