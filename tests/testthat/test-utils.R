###################################
# count_header_rows
###################################

test_that("count_header_rows works with standard UPASv2 log file", {
  filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  nrows_header <- count_header_rows(file)
  expect_identical(nrows_header$nrow_header_with_blanks, 57)
  expect_identical(nrows_header$nrow_header_no_blanks, 38)
  expect_identical(nrows_header$nrow_diag_with_blanks, 0)
  expect_identical(nrows_header$nrow_diag_no_blanks, 0)
  expect_false(nrows_header$is_diag)
})

test_that("count_header_rows works with diagnostic UPASv2 log file", {
  filename <- 'PS1771_LOG_2024-06-13T21_31_26UTC_DIAGNOSTIC____________.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  nrows_header <- count_header_rows(file)
  expect_identical(nrows_header$nrow_header_with_blanks, 76)
  expect_identical(nrows_header$nrow_header_no_blanks, 52)
  expect_identical(nrows_header$nrow_diag_with_blanks, 25)
  expect_identical(nrows_header$nrow_diag_no_blanks, 17)
  expect_true(nrows_header$is_diag)
})

test_that("count_header_rows works with standard UPASv2x log file", {
  filename <- 'PSP00270_LOG_2024-06-25T21_37_48UTC_GPS-in-out______----------.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  nrows_header <- count_header_rows(file)
  expect_identical(nrows_header$nrow_header_with_blanks, 112)
  expect_identical(nrows_header$nrow_header_no_blanks, 82)
  expect_identical(nrows_header$nrow_diag_with_blanks, 0)
  expect_identical(nrows_header$nrow_diag_no_blanks, 0)
  expect_false(nrows_header$is_diag)
})

test_that("count_header_rows works with diagnostic UPASv2x log file", {
  filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  nrows_header <- count_header_rows(file)
  expect_identical(nrows_header$nrow_header_with_blanks, 112)
  expect_identical(nrows_header$nrow_header_no_blanks, 82)
  expect_identical(nrows_header$nrow_diag_with_blanks, 25)
  expect_identical(nrows_header$nrow_diag_no_blanks, 17)
  expect_true(nrows_header$is_diag)
})

###################################
# get_tz_string
###################################

test_that("get_tz_string works with standard UPASv2 log file", {
  filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  header <- read_ast_header(file, update_names=FALSE)
  expect_identical(get_tz_string(header$GPSUTCOffset), "Etc/GMT+6")
})

test_that("get_tz_string works with standard UPASv2x log file", {
  filename <- 'PSP00270_LOG_2024-07-11T18_01_22UTC_PM_CO2_Map______----------.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  header <- read_ast_header(file, update_names=FALSE)
  expect_identical(get_tz_string(header$GPSUTCOffset), "Etc/GMT+6")
})

test_that("get_tz_string returns the proper time zone for fractional time zones", {
  filename <- 'PSP01002_LOG_2024-02-28T11_37_58UTC_OnlyRT_3________NA________.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  header <- read_ast_header(file, update_names=FALSE)
  expect_false(is.na(get_tz_string(header$GPSUTCOffset)))
  expect_identical(get_tz_string(header$GPSUTCOffset), "Asia/Kolkata")
})
