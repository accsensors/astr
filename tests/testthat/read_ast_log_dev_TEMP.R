load_all()

upasv2_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)

start.time <- Sys.time()
# upasv2_log_raw <- make_raw_ast_log(upasv2_file)
upasv2_log <- read_ast_log(upasv2_file)
end.time <- Sys.time()
time0 <- end.time - start.time

upasv2x_filename <- 'PSP00020_LOG_2022-08-28T20_22_10UTC_---------------_----------.txt'
upasv2x_file <- system.file("./tests/testthat/logfiles", upasv2x_filename, package = "astr", mustWork = TRUE)

start.time <- Sys.time()
# upasv2x_log_raw <- make_raw_ast_log(upasv2x_file)
upasv2x_log <- read_ast_log(upasv2x_file)
end.time <- Sys.time()
time1 <- end.time - start.time

time0
time1

# nrows_header <- count_header_rows(file)
# if(nrows_header$is_diag == TRUE) {
#   nrow_header_read <- nrows_header$nrow_diag_no_blanks
# }else {
#   # Read only number of non-blank lines to work with blank.line.skip in fread
#   nrow_header_read <- nrows_header$nrow_no_blanks
# }
#
# df_h_raw <- data.table::fread(file,
#                             sep=',',
#                             skip = 0,
#                             nrows = nrow_header_read,
#                             header = FALSE,
#                             fill = TRUE,
#                             blank.lines.skip = TRUE,
#                             stringsAsFactors = FALSE)
#
# firmware <- df_h_raw[grepl("firmware", df_h_raw$V1),]
# firmware <- firmware %>%
#   dplyr::mutate(ASTSampler = sub("-rev.*", "", .data$V2))
#
# if(firmware$ASTSampler != 'UPAS_v2_0'){
#   firmware <- firmware %>%
#     dplyr::mutate(FirmwareRev = sapply(strsplit(.data$V2,"-"), `[`, 2),
#                   FirmwareRev = as.numeric(gsub("rev_", "", .data$FirmwareRev)))
# }else{
#   firmware <- firmware %>%
#     dplyr::mutate(FirmwareRev = sapply(strsplit(.data$V2,"-"), `[`, 2),
#                   FirmwareRev = as.numeric(gsub("rev", "", .data$FirmwareRev)))
# }
#
#
# if(firmware$ASTSampler == 'UPAS_v2_x'){
#   nrow_header_skip <- nrow_header_with_blanks
# }else{
#   nrow_header_skip <- nrow_header_with_blanks
# }
#
# df_log_raw <- data.table::fread(file,
#                                 sep=',',
#                                 skip = nrow_header_skip,
#                                 header = FALSE,
#                                 fill = TRUE,
#                                 blank.lines.skip = TRUE,
#                                 stringsAsFactors = FALSE)
#
#
# # if(any(grepl("DIAGNOSTIC TEST", df_log_raw$V1))){
# #   df_log_raw <- df_log_raw %>%
# #     dplyr::slice(which(df_log_raw$V1=="DIAGNOSTIC TEST")+1:dplyr::n())
# #
# # }else{
# #   df_log_raw <- df_log_raw %>%
# #     dplyr::slice(which(df_log_raw$V1=="SAMPLE LOG")+1:dplyr::n())
#
#
# df_log_raw <- df_log_raw %>%
#   dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
#
# df_raw <- df_h_raw %>%
#   dplyr::bind_rows(df_log_raw) %>%
#   dplyr::distinct(.data$V1, .data$V9, .keep_all = TRUE)

