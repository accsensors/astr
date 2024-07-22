###################################
# gps_map
###################################
test_that("gps_map works with standard log and throws error if no mappable data", {
  multiple_upas_logs <- system.file("extdata", package = "astr", mustWork = TRUE) |>
    list.files(pattern="^PS.*.txt$", full.names = TRUE) %>%
    lapply(read_ast_log, update_names=TRUE) %>%
    dplyr::bind_rows()

  expect_message(standard_log_fail <- gps_map(multiple_upas_logs, variable="PM2_5M"))
  expect_true(is.null(standard_log_fail))

  expect_no_message(standard_log_pass <- gps_map(multiple_upas_logs, variable="PM2_5MC"))
  expect_false(is.null(standard_log_pass))

  # UPASv2x variable that exists but no code in place for mapping
  expect_message(standard_log_fail <- gps_map(multiple_upas_logs, variable="PM4MC"))
  expect_true(is.null(standard_log_fail))

  # UPASv2x data without GPS
  upasv2x_filename <- 'PSP00270_LOG_2024-06-14T18_54_44UTC_NoGPS___________----------.txt'
  upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
  noGPS_log <- read_ast_log(upasv2x_file)
  expect_message(noGPS_map <- gps_map(noGPS_log, variable="PM2_5MC"))
  expect_true(is.null(noGPS_map))

  # Only UPASv2 data
  upasv2_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
  upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
  upasv2_log <- read_ast_log(upasv2_file, update_names=TRUE)

  expect_message(upasv2_map <- gps_map(upasv2_log, variable="PM2_5MC"))
  expect_true(is.null(upasv2_map))
})

###################################
# format_gps_map_data
###################################
test_that("format_gps_map_data works with standard log and is NULL if no mappable data", {
  multiple_upas_logs <- system.file("extdata", package = "astr", mustWork = TRUE) |>
    list.files(pattern="^PS.*.txt$", full.names = TRUE) %>%
    lapply(read_ast_log, update_names=TRUE) %>%
    dplyr::bind_rows()

  expect_message(standard_log_fail <- format_gps_map_data(multiple_upas_logs, variable="PM2"))
  expect_true(is.null(standard_log_fail))

  expect_no_message(standard_log_pass <- format_gps_map_data(multiple_upas_logs, variable="PM2_5MC"))
  expect_false(is.null(standard_log_pass))

  # UPASv2x variable that exists but no code in place for mapping
  expect_message(standard_log_fail <- format_gps_map_data(multiple_upas_logs, variable="PM4MC"))
  expect_true(is.null(standard_log_fail))

  # UPASv2x data without GPS
  upasv2x_filename <- 'PSP00270_LOG_2024-06-14T18_54_44UTC_NoGPS___________----------.txt'
  upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
  noGPS_log <- read_ast_log(upasv2x_file)
  expect_message(noGPS_log_fail <- format_gps_map_data(noGPS_log, variable="PM2_5MC"))
  expect_true(is.null(noGPS_log_fail))

  # Only UPASv2 data
  upasv2_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
  upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
  upasv2_log <- read_ast_log(upasv2_file, update_names=FALSE)

  expect_message(upasv2_map_data <- format_gps_map_data(upasv2_log, variable="PM2_5MC"))
  expect_true(is.null(upasv2_map_data))
})
