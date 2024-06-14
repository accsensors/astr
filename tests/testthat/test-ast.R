###################################
# make_raw_ast_header
###################################

test_that("make_raw_ast_header works with standard UPASv2 file", {
  file <- use_extdata_file('PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt')
  header_raw <- make_raw_ast_header(file)
  expect_identical(tail(header_raw, n=1)[[1]], "SAMPLE LOG")
  expect_equal(ncol(header_raw), 3)
  expect_type(sapply(header_raw, class), "character")
})

test_that("make_raw_ast_header works with DIAGNOSTIC UPASv2 file", {
  file <- use_extdata_file('PS1771_LOG_2024-06-13T21_31_26UTC_DIAGNOSTIC____________.txt')
  header_raw <- make_raw_ast_header(file)
  expect_identical(tail(header_raw, n=1)[[1]], "SAMPLE LOG")
  expect_gt(ncol(header_raw), 3)
  expect_type(sapply(header_raw, class), "character")
})

test_that("make_raw_ast_header works with standard UPASv2x file", {
  file <- use_extdata_file('PSP00270_LOG_2024-06-10T21_50_55UTC_name____________eng_______.txt')
  header_raw <- make_raw_ast_header(file)
  expect_identical(tail(header_raw, n=1)[[1]], "SAMPLE LOG")
  expect_equal(ncol(header_raw), 3)
  expect_type(sapply(header_raw, class), "character")
})

test_that("make_raw_ast_header works with DIAGNOSTIC UPASv2x  file", {
  file <- use_extdata_file('PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt')
  header_raw <- make_raw_ast_header(file)
  expect_identical(tail(header_raw, n=1)[[1]], "SAMPLE LOG")
  expect_gt(ncol(header_raw), 3)
  expect_type(sapply(header_raw, class), "character")
})

###################################
# transpose_raw_ast_header
###################################

test_that("transpose_raw_ast_header works with standard UPASv2 file", {
  file <- use_extdata_file('PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt')
  header_raw <- make_raw_ast_header(file)
  header_transp <- transpose_raw_ast_header(header_raw)
  expect_equal(nrow(header_transp), 1)
})

test_that("transpose_raw_ast_header works with DIAGNOSTIC UPASv2 file", {
  file <- use_extdata_file('PS1771_LOG_2024-06-13T21_31_26UTC_DIAGNOSTIC____________.txt')
  header_raw <- make_raw_ast_header(file)
  header_transp <- transpose_raw_ast_header(header_raw)
  expect_equal(nrow(header_transp), 1)
  expect_contains(colnames(header_transp), c("MFSDIAGVoutBlocked",
                                             "MFSDIAGVoutMax",
                                             "MFSDIAGVoutMin",
                                             "MFSDIAGMFBlocked",
                                             "MFSDIAGMFMax",
                                             "MFSDIAGMFMin",
                                             "MFSDIAGPumpVBoostMax",
                                             "MFSDIAGPumpVBoostMin",
                                             "MFSDIAGPDeadhead"))
})

test_that("transpose_raw_ast_header works with standard UPASv2x file", {
  file <- use_extdata_file('PSP00270_LOG_2024-06-10T21_50_55UTC_name____________eng_______.txt')
  header_raw <- make_raw_ast_header(file)
  header_transp <- transpose_raw_ast_header(header_raw)
  expect_equal(nrow(header_transp), 1)
})

test_that("transpose_raw_ast_header works with DIAGNOSTIC UPASv2x  file", {
  file <- use_extdata_file('PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt')
  header_raw <- make_raw_ast_header(file)
  header_transp <- transpose_raw_ast_header(header_raw)
  expect_equal(nrow(header_transp), 1)
  expect_contains(colnames(header_transp), c("MFSDIAGVoutBlocked",
                                             "MFSDIAGVoutMax",
                                             "MFSDIAGVoutMin",
                                             "MFSDIAGMFBlocked",
                                             "MFSDIAGMFMax",
                                             "MFSDIAGMFMin",
                                             "MFSDIAGPumpVBoostMax",
                                             "MFSDIAGPumpVBoostMin",
                                             "MFSDIAGPDeadhead"))
})

