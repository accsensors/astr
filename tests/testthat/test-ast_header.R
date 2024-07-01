###################################
# fread_ast_header
###################################

test_that("fread_ast_header works with standard UPASv2 file", {
  file <- use_extdata_file('PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt')
  header_raw <- fread_ast_header(file)
  expect_identical(tail(header_raw$header, n=1)[[1]], "SAMPLE LOG")
  expect_identical(header_raw$diag, NULL)
  expect_equal(ncol(header_raw$header), 3)
  expect_equal(colnames(header_raw$header), c("V1","V2","V3"))
  expect_equal(as.character(header_raw$header[1,]), c("PARAMETER", "VALUE", "UNITS/NOTES"))
  expect_type(sapply(header_raw$header, class), "character")
})

test_that("fread_ast_header works with DIAGNOSTIC UPASv2 file", {
  file <- use_extdata_file('PS1771_LOG_2024-06-13T21_31_26UTC_DIAGNOSTIC____________.txt')
  header_raw <- fread_ast_header(file)
  expect_identical(tail(header_raw$header, n=1)[[1]], "DIAGNOSTIC TEST")
  expect_identical(tail(header_raw$diag, n=1)[[1]], "SAMPLE LOG")
  expect_equal(ncol(header_raw$header), 3)
  expect_equal(colnames(header_raw$header), c("V1","V2","V3"))
  expect_equal(as.character(header_raw$header[1,]), c("PARAMETER", "VALUE", "UNITS/NOTES"))
  expect_equal(ncol(header_raw$diag), 11)
  expect_equal(colnames(header_raw$diag)[1], "V1")
  expect_equal(as.character(header_raw$diag[1,1]), "(hPa)")
  expect_type(sapply(header_raw$header, class), "character")
  expect_type(sapply(header_raw$diag, class), "character")
})

test_that("fread_ast_header works with standard UPASv2x file", {
  file <- use_extdata_file('PSP00270_LOG_2024-06-25T21_37_48UTC_GPS-in-out______----------.txt')
  header_raw <- fread_ast_header(file)
  expect_identical(tail(header_raw$header, n=1)[[1]], "SAMPLE LOG")
  expect_identical(header_raw$diag, NULL)
  expect_equal(ncol(header_raw$header), 3)
  expect_equal(colnames(header_raw$header), c("V1","V2","V3"))
  expect_equal(as.character(header_raw$header[1,]), c("PARAMETER", "VALUE", "UNITS/NOTES"))
  expect_type(sapply(header_raw$header, class), "character")
})

test_that("fread_ast_header works with DIAGNOSTIC UPASv2x  file", {
  file <- use_extdata_file('PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt')
  header_raw <- fread_ast_header(file)
  expect_identical(tail(header_raw$header, n=1)[[1]], "DIAGNOSTIC TEST")
  expect_identical(tail(header_raw$diag, n=1)[[1]], "SAMPLE LOG")
  expect_equal(ncol(header_raw$header), 3)
  expect_equal(colnames(header_raw$header), c("V1","V2","V3"))
  expect_equal(as.character(header_raw$header[1,]), c("PARAMETER", "VALUE", "UNITS/NOTES"))
  expect_equal(ncol(header_raw$diag), 15)
  expect_equal(colnames(header_raw$diag)[1], "V1")
  expect_equal(as.character(header_raw$diag[1,1]), "(hPa)")
  expect_type(sapply(header_raw$header, class), "character")
  expect_type(sapply(header_raw$diag, class), "character")
})


###################################
# transpose_ast_header
###################################

test_that("transpose_ast_header works with standard UPASv2 file", {
  file <- use_extdata_file('PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt')
  header_raw <- fread_ast_header(file)
  header_wide <- transpose_ast_header(header_raw$header, diag = header_raw$diag)
  expect_equal(nrow(header_wide), 1)
})

test_that("transpose_ast_header works with DIAGNOSTIC UPASv2 file", {
  file <- use_extdata_file('PS1771_LOG_2024-06-13T21_31_26UTC_DIAGNOSTIC____________.txt')
  header_raw <- fread_ast_header(file)
  header_wide <- transpose_ast_header(header_raw$header, diag = header_raw$diag)
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

test_that("transpose_ast_header works with standard UPASv2x file", {
  file <- use_extdata_file('PSP00270_LOG_2024-06-25T21_37_48UTC_GPS-in-out______----------.txt')
  header_raw <- fread_ast_header(file)
  header_wide <- transpose_ast_header(header_raw$header, diag = header_raw$diag)
  expect_equal(nrow(header_wide), 1)
})

test_that("transpose_ast_header works with DIAGNOSTIC UPASv2x  file", {
  file <- use_extdata_file('PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt')
  header_raw <- fread_ast_header(file)
  header_wide <- transpose_ast_header(header_raw$header, diag = header_raw$diag)
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

test_that("All wide header files have only one row", {
  upasv2_file <- use_extdata_file('PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt')
  upasv2x_file <- use_extdata_file('PSP00270_LOG_2024-06-25T21_37_48UTC_GPS-in-out______----------.txt')
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
  upasv2_header_raw <- fread_ast_header(upasv2_file)
  upasv2_header_wide <- transpose_ast_header(upasv2_header_raw$header, diag = upasv2_header_raw$diag)
  expect_identical(read_ast_header(upasv2_file, update_names = TRUE), format_upasv2_header(upasv2_header_wide, update_names = TRUE))
})

test_that("format_upasv2x_header and read_ast_header have the same output", {
  upasv2x_file <- use_extdata_file('PSP00270_LOG_2024-06-25T21_37_48UTC_GPS-in-out______----------.txt')
  upasv2x_header <- read_ast_header(upasv2x_file, update_names = TRUE)
  upasv2x_header_raw <- fread_ast_header(upasv2x_file)
  upasv2x_header_wide <- transpose_ast_header(upasv2x_header_raw$header, diag = upasv2x_header_raw$diag)
  expect_identical(read_ast_header(upasv2x_file), format_upasv2x_header(upasv2x_header_wide))
})
