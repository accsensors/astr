test_that("make_raw_ast_header works with standard UPASv2 file", {
  filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  header_raw <- make_raw_ast_header(file)
  expect_identical(tail(header_raw, n=1)[[1]], "SAMPLE LOG")
  expect_equal(ncol(header_raw), 3)
  expect_type(sapply(header_raw, class), "character")
})

test_that("make_raw_ast_header works with DIAGNOSTIC UPASv2 file", {
  filename <- 'PS1771_LOG_2024-06-13T21_31_26UTC_DIAGNOSTIC____________.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  header_raw <- make_raw_ast_header(file)
  expect_identical(tail(header_raw, n=1)[[1]], "SAMPLE LOG")
  expect_gt(ncol(header_raw), 3)
  expect_type(sapply(header_raw, class), "character")
})

test_that("make_raw_ast_header works with standard UPASv2x file", {
  filename <- 'PSP00270_LOG_2024-06-10T21_50_55UTC_name____________eng_______.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  header_raw <- make_raw_ast_header(file)
  expect_identical(tail(header_raw, n=1)[[1]], "SAMPLE LOG")
  expect_equal(ncol(header_raw), 3)
  expect_type(sapply(header_raw, class), "character")
})

test_that("make_raw_ast_header works with DIAGNOSTIC UPASv2x  file", {
  filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  header_raw <- make_raw_ast_header(file)
  expect_identical(tail(header_raw, n=1)[[1]], "SAMPLE LOG")
  expect_gt(ncol(header_raw), 3)
  expect_type(sapply(header_raw, class), "character")
})
