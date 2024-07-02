###################################
# fread_ast_log
###################################

test_that("fread_ast_log works with standard UPASv2 file", {
  filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  log_raw <- fread_ast_log(file)
  expect_identical(colnames(log_raw)[1], "SampleTime")
  expect_gt(ncol(log_raw), 3)
})

test_that("fread_ast_log works with DIAGNOSTIC UPASv2 file", {
  filename <- 'PS1771_LOG_2024-06-13T21_31_26UTC_DIAGNOSTIC____________.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  log_raw <- fread_ast_log(file)
  expect_identical(colnames(log_raw)[1], "SampleTime")
  expect_gt(ncol(log_raw), 3)
})

test_that("fread_ast_log works with standard UPASv2x file", {
  filename <- 'PSP00270_LOG_2024-06-14T18_54_44UTC_NoGPS___________----------.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  log_raw <- fread_ast_log(file)
  expect_identical(colnames(log_raw)[1], "SampleTime")
  expect_gt(ncol(log_raw), 3)
})

test_that("fread_ast_log works with DIAGNOSTIC UPASv2x  file", {
  filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  log_raw <- fread_ast_log(file)
  expect_identical(colnames(log_raw)[1], "SampleTime")
  expect_gt(ncol(log_raw), 3)
})

test_that("fread_ast_log works with SHEAR UPASv2x  file", {
  filename <- 'SH00009_LOG_2022-02-14T17_02_32UTC_---------------_5VX__.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  log_raw <- fread_ast_log(file)
  expect_identical(colnames(log_raw)[1], "SampleTime")
  expect_gt(ncol(log_raw), 3)
})

###################################
# read_ast_log
###################################
#test cols_keep and cols_drop (also add to examples)

test_that("If using update names for UPASv2, all applicable column names are updated to match UPASv2x", {
  upasv2_filename <- 'PS1771_LOG_2024-06-13T21_31_26UTC_DIAGNOSTIC____________.txt'
  upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
  upasv2x_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
  upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
  upasv2_log <- read_ast_log(upasv2_file, update_names = TRUE)
  upasv2x_log <- read_ast_log(upasv2x_file)
  upasv2_colnames <- colnames(upasv2_log)
  expect_contains(colnames(upasv2x_log), upasv2_colnames[! upasv2_colnames %in%
                          c("MFlowDelta", "VFlowDelta", "MFSADS", "VInADS", "PumpADS", "LogFileMode")])
})

test_that("format_upasv2_log and read_ast_log have the same output", {
  upasv2_filename <- 'PS1771_LOG_2024-06-13T21_31_26UTC_DIAGNOSTIC____________.txt'
  upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
  upasv2_header <- read_ast_header(upasv2_file)
  upasv2_log_raw <- fread_ast_log(upasv2_file)
  expect_identical(read_ast_log(upasv2_file, update_names = TRUE),
                   format_upasv2_log(upasv2_log_raw, upasv2_header, update_names = TRUE))
})

test_that("format_upasv2x_header and read_ast_header have the same output", {
  upasv2x_filename <- 'PSP00270_LOG_2024-06-25T21_37_48UTC_GPS-in-out______----------.txt'
  upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
  upasv2x_header <- read_ast_header(upasv2x_file)
  upasv2x_log_raw <- fread_ast_log(upasv2x_file)
  expect_identical(read_ast_log(upasv2x_file, update_names = TRUE),
                   format_upasv2x_log(upasv2x_log_raw, upasv2x_header))
})

test_that("tz, cols_keep, and cols_drop arguments work as expected", {
  upasv2_filename <- 'PS1771_LOG_2024-06-13T21_31_26UTC_DIAGNOSTIC____________.txt'
  upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
  upasv2x_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
  upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
  upasv2_log <- read_ast_log(upasv2_file, tz="America/New_York",
                             cols_keep = c("SampleTime", "DateTimeUTC", "DateTimeLocal", "LocalTZ",  "UserTZ"))
  upasv2x_log <- read_ast_log(upasv2x_file, tz="America/New_York", cols_drop = c("AtmoT", "AtmoP", "AtmoRH"))
  expect_identical(unique(c(upasv2_log$LocalTZ, upasv2x_log$LocalTZ)), "America/New_York")
  expect_true(unique(c(upasv2_log$UserTZ, upasv2x_log$UserTZ)))
  expect_identical(colnames(upasv2_log), c("SampleTime", "DateTimeUTC", "DateTimeLocal", "LocalTZ",  "UserTZ"))
  expect_false(grepl(paste(c("AtmoT", "AtmoP", "AtmoRH"), collapse=";"), paste(colnames(upasv2x_log), collapse=";")))
})

