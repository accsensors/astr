###################################
# make_raw_ast_log
###################################

test_that("make_raw_ast_log works with standard UPASv2 file", {
  file <- use_extdata_file('PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt')
  log_raw <- make_raw_ast_log(file)
  expect_identical(log_raw$V1[2], "SampleTime")
  expect_gt(ncol(log_raw), 3)
  expect_type(sapply(log_raw, class), "character")
})

test_that("make_raw_ast_log works with DIAGNOSTIC UPASv2 file", {
  file <- use_extdata_file('PS1771_LOG_2024-06-13T21_31_26UTC_DIAGNOSTIC____________.txt')
  log_raw <- make_raw_ast_log(file)
  expect_identical(log_raw$V1[2], "SampleTime")
  expect_gt(ncol(log_raw), 3)
  expect_type(sapply(log_raw, class), "character")
})

test_that("make_raw_ast_log works with standard UPASv2x file", {
  file <- use_extdata_file('PSP00270_LOG_2024-06-14T18_54_44UTC_NoGPS___________----------.txt')
  log_raw <- make_raw_ast_log(file)
  expect_identical(log_raw$V1[2], "SampleTime")
  expect_gt(ncol(log_raw), 3)
  expect_type(sapply(log_raw, class), "character")
})

test_that("make_raw_ast_log works with DIAGNOSTIC UPASv2x  file", {
  file <- use_extdata_file('PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt')
  log_raw <- make_raw_ast_log(file)
  expect_identical(log_raw$V1[2], "SampleTime")
  expect_gt(ncol(log_raw), 3)
  expect_type(sapply(log_raw, class), "character")
})

test_that("Blank columns/values for different sensor settings show up as blank instead of NA", {
  file <- use_extdata_file('PSP00270_LOG_2024-06-14T18_54_44UTC_NoGPS___________----------.txt')
  log_raw <- make_raw_ast_log(file)
  expect_false(anyNA(log_raw))
})
