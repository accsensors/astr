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
  header_wide <- transpose_raw_ast_header(header_raw)
  expect_equal(nrow(header_wide), 1)
})

test_that("transpose_raw_ast_header works with DIAGNOSTIC UPASv2 file", {
  file <- use_extdata_file('PS1771_LOG_2024-06-13T21_31_26UTC_DIAGNOSTIC____________.txt')
  header_raw <- make_raw_ast_header(file)
  header_wide <- transpose_raw_ast_header(header_raw)
  expect_equal(nrow(header_wide), 1)
  expect_contains(colnames(header_wide), c("MFSDIAGVoutBlocked",
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
  header_wide <- transpose_raw_ast_header(header_raw)
  expect_equal(nrow(header_wide), 1)
})

test_that("transpose_raw_ast_header works with DIAGNOSTIC UPASv2x  file", {
  file <- use_extdata_file('PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt')
  header_raw <- make_raw_ast_header(file)
  header_wide <- transpose_raw_ast_header(header_raw)
  expect_equal(nrow(header_wide), 1)
  expect_contains(colnames(header_wide), c("MFSDIAGVoutBlocked",
                                             "MFSDIAGVoutMax",
                                             "MFSDIAGVoutMin",
                                             "MFSDIAGMFBlocked",
                                             "MFSDIAGMFMax",
                                             "MFSDIAGMFMin",
                                             "MFSDIAGPumpVBoostMax",
                                             "MFSDIAGPumpVBoostMin",
                                             "MFSDIAGPDeadhead"))
})

###################################
# read_ast_header
###################################
# TESTS:
# Test that format_upasv2_header and read_ast_header have the same output
# Test that format_upasv2x_header and read_ast_header have the same output

test_that("All wide header files have only one row", {
  upasv2_file <- use_extdata_file('PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt')
  upasv2x_file <- use_extdata_file('PSP00270_LOG_2024-06-10T21_50_55UTC_name____________eng_______.txt')
  upasv2_header <- read_ast_header(upasv2_file)
  upasv2x_header <- read_ast_header(upasv2x_file)
  expect_equal(nrow(upasv2_header), 1)
  expect_equal(nrow(upasv2x_header), 1)
})

test_that("If using update names for UPASv2, all applicable column names are updated to match UPASv2x", {
  upasv2_file <- use_extdata_file('PS1771_LOG_2024-06-13T21_31_26UTC_DIAGNOSTIC____________.txt')
  upasv2x_file <- use_extdata_file('PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt')
  upasv2_header <- read_ast_header(upasv2_file, update_names = TRUE)
  upasv2x_header <- read_ast_header(upasv2x_file)
  upasv2_colnames <- colnames(upasv2_header)
  expect_contains(colnames(upasv2x_header), upasv2_colnames[! upasv2_colnames %in% c("ProgrammedStartDelay", "LogFileMode")])
})

test_that("format_upasv2_header and read_ast_header have the same output", {
  upasv2_file <- use_extdata_file('PS1771_LOG_2024-06-13T21_31_26UTC_DIAGNOSTIC____________.txt')
  upasv2_header <- read_ast_header(upasv2_file, update_names = TRUE)
  upasv2_header_raw <- make_raw_ast_header(upasv2_file)
  upasv2_header_wide <- transpose_raw_ast_header(upasv2_header_raw)
  expect_identical(read_ast_header(upasv2_file, update_names = TRUE), format_upasv2_header(upasv2_header_wide, update_names = TRUE))
})

test_that("format_upasv2x_header and read_ast_header have the same output", {
  upasv2x_file <- use_extdata_file('PSP00270_LOG_2024-06-10T21_50_55UTC_name____________eng_______.txt')
  upasv2x_header <- read_ast_header(upasv2x_file, update_names = TRUE)
  upasv2x_header_raw <- make_raw_ast_header(upasv2x_file)
  upasv2x_header_wide <- transpose_raw_ast_header(upasv2x_header_raw)
  expect_identical(read_ast_header(upasv2x_file), format_upasv2x_header(upasv2x_header_wide))
})
