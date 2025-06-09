###################################
# format_hhb_samples
###################################

test_that("HHB sample data from log files written using with and without support for staggered sampling can be combined", {
  flist  <- c('HHB00032_LOG_2024-07-01T18_20UTC.csv',
              'HHB00087_LOG_2025-06-03T20_55UTC.csv')
  files  <- system.file("extdata", flist, package = "astr", mustWork = T)
  header <- lapply(files, read_ast_header)
  expect_no_error(dplyr::bind_rows(header))
  header <- dplyr::bind_rows(header)
  expect_no_error(format_hhb_samples(header))
  samples <- format_hhb_samples(header)
  expect_equal(nrow(samples), 4 * nrow(header))
})

test_that("format_hhb_samples works with all HHBv2 firmwares", {
  hhb_filename_240111 <- 'HHB00032_LOG_2024-07-01T18_20UTC.csv'
  hhb_file_240111 <- system.file("extdata", hhb_filename_240111, package = "astr", mustWork = TRUE)
  hhb_header_240111 <- read_ast_header(hhb_file_240111)
  expect_snapshot(format_hhb_samples(hhb_header_240111))

  hhb_filename_250529 <- 'HHB00087_LOG_2025-06-03T20_55UTC.csv'
  hhb_file_250529 <- system.file("extdata", hhb_filename_250529, package = "astr", mustWork = TRUE)
  hhb_header_250529 <- read_ast_header(hhb_file_250529)
  expect_snapshot(format_hhb_samples(hhb_header_250529))
})

###################################
# format_hhb_sensors
###################################

test_that("HHB electrochemical sensor data from log files written using with and without support for in-firmware Alphasense algorithm calculations can be combined", {
  flist  <- c('HHB00032_LOG_2024-07-01T18_20UTC.csv',
              'HHB00087_LOG_2025-06-03T20_55UTC.csv')
  files  <- system.file("extdata", flist, package = "astr", mustWork = T)
  header <- lapply(files, read_ast_header)
  log    <- lapply(files, read_ast_log)
  expect_no_error(dplyr::bind_rows(log))
  header <- dplyr::bind_rows(header)
  log    <- dplyr::bind_rows(log)
  expect_no_error(format_hhb_sensors(log, header, temp = "G.SCD30_Temp"))
  sensors <- format_hhb_sensors(log, header, temp = "G.SCD30_Temp")
})

test_that("format_hhb_sensors produces expected temperature-related warnings", {
  fname  <- 'HHB00087_LOG_2025-06-03T20_55UTC.csv'
  file   <- system.file("extdata", fname, package = "astr", mustWork = T)
  header <- read_ast_header(file)
  log    <- read_ast_log(file)
  expect_warning(format_hhb_sensors(log, header, temp = "G.SCD30_RH"),
                 "Invalid variable name specified for temp argument. No temperature data will be included in the returned data frame.")
  expect_warning(format_hhb_sensors(log, header, temp = "M.BMP581_Temp"),
                 "The specified temperature was not measured inside the gas sensor housing. It is recommended that you specify a temperature measured inside the gas sensor housing: 'G.SCD30_Temp', 'G.BMP581_Temp', or 'G.SFA30_Temp'.")
})

test_that("format_hhb_sensors produces expected header-related warnings", {
  flist  <- c('HHB00032_LOG_2024-07-01T18_20UTC.csv',
              'HHB00087_LOG_2025-06-03T20_55UTC.csv')
  files  <- system.file("extdata", flist, package = "astr", mustWork = T)
  header <- lapply(files, read_ast_header)
  log    <- lapply(files, read_ast_log)
  header <- dplyr::bind_rows(header)
  log    <- dplyr::bind_rows(log)
  expect_warning(format_hhb_sensors(log, header[2,]),
                 "The following log files in the log data frame are missing from the header data frame:\nHHB00032_LOG_2024-07-01T18_20UTC.csv\nSensor IDs and calibration constants for these samples will not appear in the data frame returned by the function.")
})

test_that("format_hhb_sensors works with all HHBv2 firmwares", {
  hhb_filename_240111 <- 'HHB00032_LOG_2024-07-01T18_20UTC.csv'
  hhb_file_240111 <- system.file("extdata", hhb_filename_240111, package = "astr", mustWork = TRUE)
  hhb_header_240111 <- read_ast_header(hhb_file_240111)
  hhb_log_240111 <- read_ast_log(hhb_file_240111)
  expect_snapshot(format_hhb_sensors(hhb_log_240111, hhb_header_240111))

  hhb_filename_250529 <- 'HHB00087_LOG_2025-06-03T20_55UTC.csv'
  hhb_file_250529 <- system.file("extdata", hhb_filename_250529, package = "astr", mustWork = TRUE)
  hhb_header_250529 <- read_ast_header(hhb_file_250529)
  hhb_log_250529 <- read_ast_log(hhb_file_250529)
  expect_snapshot(format_hhb_sensors(hhb_log_250529, hhb_header_250529))
})
