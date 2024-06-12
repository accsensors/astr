load_all()

filename <- 'PSP00270_LOG_2024-06-10T21_50_55UTC_name____________eng_______.txt'
file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)

###########
# Number of header lines with and without blank lines
###########

header_with_blanks <- readLines(file, n = 200)
header_no_blanks <- header_with_blanks[which(header_with_blanks!="")]
nrow_header_with_blanks <- as.numeric(grep("SAMPLE LOG", header_with_blanks))
nrow_header_no_blanks <- as.numeric(grep("SAMPLE LOG", header_no_blanks))

###########
# data_ast_raw
###########

###########
# ast_header_raw
###########

# Need to use nrow_header_no_blanks because blank.lines.skip=TRUE
ast_header_raw <- data.table::fread(file=file, sep=',', skip = 0, nrows = nrow_header_no_blanks,
                                  header = FALSE, fill = TRUE, blank.lines.skip = TRUE,
                                  stringsAsFactors = FALSE)

###########
# ast_log_raw
###########

# Need to use nrow_header_with_blanks because fread uses blank lines in skip count
ast_log_raw <- data.table::fread(file=file, sep=',', skip = nrow_header_with_blanks+4,
                                   header = FALSE, fill = TRUE, blank.lines.skip = TRUE,
                                   stringsAsFactors = FALSE)

# usethis::use_data(data_ast_raw, overwrite = TRUE)

###########
# upasv2x_header_raw
###########

# my_cols <- c('V1','V2')
#
# df_h <- data_ast_raw %>%
#   dplyr::select(my_cols)
#
# df_h <- df_h[2:(which(df_h$V1=="SAMPLE LOG")-1),]
#
# remove_names <- c("SAMPLE IDENTIFICATION","SETUP SUMMARY",
#                   "SAMPLE IDENTIFICATION","SAMPLE SUMMARY",
#                   "CO2 SENSOR CALIBRATION", "MASS FLOW SENSOR CALIBRATION")
#
# df_h <- df_h[ grep(paste(remove_names,collapse="|"), df_h$V1, invert = TRUE) , ]
#
# df_h <- df_h %>%
#   t()
#
# df_h <- as.data.frame(df_h)
#
# colnames(df_h) <- df_h[1, ]
# df_h <- df_h[-1, ]
# rownames(df_h) <- c(1)
#
# upasv2x_header_raw <- df_h %>%
#   dplyr::rename('Firmware' = names(df_h[stringr::str_detect(names(df_h), 'firmware')]) )
#
#
# usethis::use_data(upasv2x_header_raw, overwrite = TRUE)

###########
# upasv2x_header
###########

# upasv2x_header <- read_ast_header(file)
# usethis::use_data(upasv2x_header, overwrite = TRUE)

###########
# upasv2x_log_raw
###########

# df_cols <- data_ast_raw %>%
#   dplyr::slice(which(data_ast_raw$V1=="SAMPLE LOG")+2) %>%
#   unlist(use.names = FALSE)
#
# upasv2x_log_raw <- data_ast_raw %>%
#   dplyr::slice(which(data_ast_raw$V1=="SAMPLE LOG")+4:dplyr::n())
#
# colnames(upasv2x_log_raw) <- df_cols
#
# usethis::use_data(upasv2x_log_raw, overwrite = TRUE)

###########
# upasv2x_log
###########

# upasv2x_log <- format_upasv2x_log(upasv2x_header, upasv2x_log_raw)
# usethis::use_data(upasv2x_log, overwrite = TRUE)
