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
