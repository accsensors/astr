load_all()

upasv2_filename <- 'PS1771_LOG_2024-06-13T21_31_26UTC_DIAGNOSTIC____________.txt'
upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
# upasv2_header_raw <- make_raw_ast_header(upasv2_file)
# upasv2_header_wide <- transpose_raw_ast_header(upasv2_header_raw)
# upasv2_header <- format_upasv2_header(upasv2_header_wide, update_names=FALSE)

start.time <- Sys.time()
upasv2_header <- read_ast_header(upasv2_file, update_names = TRUE)
end.time <- Sys.time()
time0 <- end.time - start.time

upasv2x_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
# upasv2x_header_raw <- make_raw_ast_header(upasv2x_file)
# upasv2x_header_wide <- transpose_raw_ast_header(upasv2x_header_raw)
# upasv2x_header <- format_upasv2x_header(upasv2x_header_wide)

start.time <- Sys.time()
upasv2x_header <- read_ast_header(upasv2x_file)
end.time <- Sys.time()
time1 <- end.time - start.time

time0
time1
