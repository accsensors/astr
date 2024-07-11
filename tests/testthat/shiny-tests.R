load_all()

multiple_upas_headers <- list.files(path = "inst/extdata", pattern="^PS.*.txt$",
                                   full.names = TRUE) %>%
         lapply(read_ast_header, update_names=TRUE) %>%
         dplyr::bind_rows()

upas_sample_summary <- shiny_sample_summary(multiple_upas_headers)
upas_sample_settings <- shiny_sample_settings(multiple_upas_headers)
upas_sample_operation <- shiny_sample_operation(multiple_upas_headers)

multiple_upas_logs <- list.files(path = "inst/extdata", pattern="^PS.*.txt$",
                                 full.names = TRUE) %>%
  lapply(read_ast_log, update_names=TRUE) %>%
  dplyr::bind_rows()

multiple_upas_logs <- list.files(path = "tests/testthat/logfiles", pattern="^PSP.*.txt$",
                                 full.names = TRUE) %>%
  lapply(read_ast_log, update_names=TRUE) %>%
  dplyr::bind_rows()

upasv2x_rev157_filename <- 'PSP00270_LOG_2024-06-25T21_37_48UTC_GPS-in-out______----------.txt'
upasv2x_rev157_file <- system.file("extdata", upasv2x_rev157_filename, package = "astr", mustWork = TRUE)
upasv2x_rev157_log_raw <- fread_ast_log(upasv2x_rev157_file)
upasv2x_rev157_header <- read_ast_header(upasv2x_rev157_file, update_names=FALSE)
upasv2x_rev157_log <- format_upasv2x_log(upasv2x_rev157_log_raw, upasv2x_rev157_header, update_names=FALSE)

upasv2_rev138_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
upasv2_rev138_file <- system.file("extdata", upasv2_rev138_filename, package = "astr", mustWork = TRUE)
upasv2_rev138_log_raw <- fread_ast_log(upasv2_rev138_file)
upasv2_rev138_header <- read_ast_header(upasv2_rev138_file, update_names=FALSE)
upasv2_rev138_log <- format_upasv2_log(upasv2_rev138_log_raw, upasv2_rev138_header)
#
# upas_shiny_log <- shiny_log(multiple_upas_logs)

df_30s <- format_gps_map_data(multiple_upas_logs, variable="CO")
heat_map <- gps_map(multiple_upas_logs, variable="PM2_5MC")
heat_map

