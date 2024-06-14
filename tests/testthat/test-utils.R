test_that("count_header_rows works with standard UPASv2 log file", {
  filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  nrows_header <- count_header_rows(file)
  expect_identical(nrows_header$nrow_with_blanks, 57)
  expect_identical(nrows_header$nrow_no_blanks, 38)
  expect_identical(nrows_header$nrow_diag_with_blanks, NA)
  expect_identical(nrows_header$nrow_diag_no_blanks, NA)
  expect_false(nrows_header$is_diag)
})

test_that("count_header_rows works with diagnostic UPASv2 log file", {
  filename <- 'PS1771_LOG_2024-06-13T21_31_26UTC_DIAGNOSTIC____________.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  nrows_header <- count_header_rows(file)
  expect_identical(nrows_header$nrow_with_blanks, 101)
  expect_identical(nrows_header$nrow_no_blanks, 69)
  expect_identical(nrows_header$nrow_diag_with_blanks, 76)
  expect_identical(nrows_header$nrow_diag_no_blanks, 52)
  expect_true(nrows_header$is_diag)
})

test_that("count_header_rows works with standard UPASv2x log file", {
  filename <- 'PSP00270_LOG_2024-06-10T21_50_55UTC_name____________eng_______.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  nrows_header <- count_header_rows(file)
  expect_identical(nrows_header$nrow_with_blanks, 112)
  expect_identical(nrows_header$nrow_no_blanks, 82)
  expect_identical(nrows_header$nrow_diag_with_blanks, NA)
  expect_identical(nrows_header$nrow_diag_no_blanks, NA)
  expect_false(nrows_header$is_diag)
})

test_that("count_header_rows works with diagnostic UPASv2x log file", {
  filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  nrows_header <- count_header_rows(file)
  expect_identical(nrows_header$nrow_with_blanks, 137)
  expect_identical(nrows_header$nrow_no_blanks, 99)
  expect_identical(nrows_header$nrow_diag_with_blanks, 112)
  expect_identical(nrows_header$nrow_diag_no_blanks, 82)
  expect_true(nrows_header$is_diag)
})
