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

upasv2_header_raw <- df_h %>%
  dplyr::mutate_at(c("UPASserial"), as.numeric) %>%
  dplyr::mutate(ast_sampler = sub("-rev.*", "", .data$UPASfirmware),
                firmware_rev = sapply(strsplit(.data$UPASfirmware,"-"), `[`, 2),
                firmware_rev = as.numeric(gsub("rev", "", .data$firmware_rev)))

usethis::use_data(upasv2_header_raw, overwrite = TRUE)
