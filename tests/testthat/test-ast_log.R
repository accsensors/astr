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

test_that("fread_ast_log works with UPASv2 file with any empty sample log", {
  filename <- 'PS2061_LOG_2024-11-21T21_40_53UTC_no-log________________.txt'
  file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
  log_raw <- fread_ast_log(file)
  expect_s3_class(log_raw, "data.frame")
  expect_true(nrow(log_raw) == 0)
  expect_identical(colnames(log_raw)[1], "SampleTime")
  expect_gt(ncol(log_raw), 3)
})

test_that("fread_ast_log works with UPASv2x file with any empty sample log", {
    filename <- 'PSP00069_LOG_2024-11-21T21_42_29UTC_no-log__________----------.txt'
    file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
    log_raw <- fread_ast_log(file)
    expect_s3_class(log_raw, "data.frame")
    expect_true(nrow(log_raw) == 0)
    expect_identical(colnames(log_raw)[1], "SampleTime")
    expect_gt(ncol(log_raw), 3)
})

###################################
# read_ast_log
###################################

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

test_that("format_upasv2x_log and read_ast_log have the same output", {
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

test_that("DateTimeLocal is not altered for files with fractional time zone offsets", {
  upasv2x_filename <- 'PSP01002_LOG_2024-02-28T11_37_58UTC_OnlyRT_3________NA________.txt'
  upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
  upasv2x_log <- read_ast_log(upasv2x_file)
  upasv2x_log_tz <- read_ast_log(upasv2x_file, tz = "Asia/Kolkata")
  expect_identical(upasv2x_log$DateTimeLocal[1], as.POSIXct("2024-02-28 17:09:00", "%Y-%m-%d %H:%M:%S", tz="Asia/Kolkata"))
  expect_identical(upasv2x_log$DateTimeLocal[1], upasv2x_log_tz$DateTimeLocal[1])
})

test_that("An empty UPASv2 sample log returns a dataframe with the same columns and types as a UPASv2 sample log that contains data", {
  upasv2_filename    <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
  upasv2_fname_empty <- 'PS2061_LOG_2024-11-21T21_40_53UTC_no-log________________.txt'
  upasv2_file    <- system.file("extdata", upasv2_filename,    package = "astr", mustWork = TRUE)
  upasv2_f_empty <- system.file("extdata", upasv2_fname_empty, package = "astr", mustWork = TRUE)
  upasv2_log       <- read_ast_log(upasv2_file)
  upasv2_log_empty <- read_ast_log(upasv2_f_empty)
  expect_s3_class(upasv2_log_empty, "data.frame")
  expect_true(nrow(upasv2_log_empty) == 0)
  expect_true(ncol(upasv2_log) == ncol(upasv2_log_empty))
  expect_true(unique(sapply(upasv2_log, typeof) == sapply(upasv2_log_empty, typeof)))
})

test_that("An empty UPASv2x sample log returns a dataframe with the same columns and types as a UPASv2x sample log that contains data", {
    upasv2x_filename    <- 'PSP00270_LOG_2024-06-25T21_37_48UTC_GPS-in-out______----------.txt'
    upasv2x_fname_empty <- 'PSP00069_LOG_2024-11-21T21_42_29UTC_no-log__________----------.txt'
    upasv2x_file    <- system.file("extdata", upasv2x_filename,    package = "astr", mustWork = TRUE)
    upasv2x_f_empty <- system.file("extdata", upasv2x_fname_empty, package = "astr", mustWork = TRUE)
    upasv2x_log       <- read_ast_log(upasv2x_file)
    upasv2x_log_empty <- read_ast_log(upasv2x_f_empty)
    expect_s3_class(upasv2x_log_empty, "data.frame")
    expect_true(nrow(upasv2x_log_empty) == 0)
    expect_true(ncol(upasv2x_log) == ncol(upasv2x_log_empty))
    expect_true(unique(sapply(upasv2x_log, typeof) == sapply(upasv2x_log_empty, typeof)))
})

test_that("Binding rows when empty log files are included doesn't add any data to the data frame.", {
  flist <- system.file("extdata", package = "astr", mustWork = TRUE) |>
    list.files(pattern="^PS.*.txt$", full.names = TRUE)
  all_upas_logs <- flist %>%
    lapply(read_ast_log, update_names = TRUE) %>%
    dplyr::bind_rows()
  not_empty_logs <- flist[!grepl("no-log", flist)] %>%
    lapply(read_ast_log, update_names = TRUE) %>%
    dplyr::bind_rows()
  expect_identical(all_upas_logs, not_empty_logs)
})

test_that("Values of -9999 in GPS data columns are replaced with NA.",{
  upasv2x_gpsoff_fname <- "PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt"
  upasv2x_gpsno_fname  <- "PSP00270_LOG_2024-06-14T18_54_44UTC_NoGPS___________----------.txt"
  upasv2x_gps_fname    <- "PSP00270_LOG_2024-06-25T21_37_48UTC_GPS-in-out______----------.txt"
  upasv2x_gpsoff_file  <- system.file("extdata", upasv2x_gpsoff_fname, package = "astr", mustWork = TRUE)
  upasv2x_gpsno_file   <- system.file("extdata", upasv2x_gpsno_fname,  package = "astr", mustWork = TRUE)
  upasv2x_gps_file     <- system.file("extdata", upasv2x_gps_fname,    package = "astr", mustWork = TRUE)
  upasv2x_gpsoff_log   <- read_ast_log(upasv2x_gpsoff_file)
  upasv2x_gpsno_log    <- read_ast_log(upasv2x_gpsno_file)
  upasv2x_gps_log      <- read_ast_log(upasv2x_gps_file)
  expect_equal(sum(upasv2x_gpsoff_log$GPSlat == -9999, na.rm = T), 0)
  expect_equal(sum(upasv2x_gpsno_log$GPSlat  == -9999, na.rm = T), 0)
  expect_equal(sum(upasv2x_gps_log$GPSlat    == -9999, na.rm = T), 0)
  expect_equal(sum(is.na(upasv2x_gpsoff_log$GPSlat)), nrow(upasv2x_gpsoff_log))
  expect_equal(sum(is.na(upasv2x_gpsno_log$GPSlat)),  nrow(upasv2x_gpsno_log))
  expect_equal(sum(is.na(upasv2x_gps_log$GPSlat)), 3)
  expect_equal(sum(!is.na(upasv2x_gps_log$GPSlat)), (nrow(upasv2x_gps_log) - 3))
})


###################################
# read_ast_log backwards compatibility
###################################

test_that("read_ast_log works with all UPASv2 firmwares", {
  upasv2_rev100_filename <- 'PS1422_LOG_2020-06-02T18_26_25UTC_rev100-norm________---.txt'
  upasv2_rev100_file <- system.file("extdata", upasv2_rev100_filename, package = "astr", mustWork = TRUE)
  expect_snapshot(read_ast_log(upasv2_rev100_file, update_names=FALSE))

  upasv2_rev100_diag_filename <- 'PS1422_LOG_2020-06-02T18_29_11UTC_DIAGNOSTIC____________.txt'
  upasv2_rev100_diag_file <- system.file("extdata", upasv2_rev100_diag_filename, package = "astr", mustWork = TRUE)
  expect_snapshot(read_ast_log(upasv2_rev100_diag_file, update_names=FALSE))

  upasv2_rev125_filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
  upasv2_rev125_file <- system.file("extdata", upasv2_rev125_filename, package = "astr", mustWork = TRUE)
  expect_snapshot(read_ast_log(upasv2_rev125_file, update_names=FALSE))

  upasv2_rev130_diag_filename <- 'PS1786_LOG_2023-03-02T21_45_43UTC_DIAGNOSTIC____________.txt'
  upasv2_rev130_diag_file <- system.file("extdata", upasv2_rev130_diag_filename, package = "astr", mustWork = TRUE)
  expect_snapshot(read_ast_log(upasv2_rev130_diag_file, update_names=FALSE))

  upasv2_rev138_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
  upasv2_rev138_file <- system.file("extdata", upasv2_rev138_filename, package = "astr", mustWork = TRUE)
  expect_snapshot(read_ast_log(upasv2_rev138_file, update_names=FALSE))
})

test_that("read_ast_log works with all UPASv2x firmwares", {
  upasv2x_rev81_filename <- 'PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt'
  upasv2x_rev81_file <- system.file("extdata", upasv2x_rev81_filename, package = "astr", mustWork = TRUE)
  expect_snapshot(read_ast_log(upasv2x_rev81_file, update_names=FALSE))

  upasv2x_rev117_filename <- 'PSP00030_LOG_2022-05-11T23_24_01UTC_---------------_----------.txt'
  upasv2x_rev117_file <- system.file("extdata", upasv2x_rev117_filename, package = "astr", mustWork = TRUE)
  expect_snapshot(read_ast_log(upasv2x_rev117_file, update_names=FALSE))

  upasv2x_rev110_diag_filename <- 'PSP00055_LOG_2022-03-24T18_05_32UTC_DIAGNOSTIC________________.txt'
  upasv2x_rev110_diag_file <- system.file("extdata", upasv2x_rev110_diag_filename, package = "astr", mustWork = TRUE)
  expect_snapshot(read_ast_log(upasv2x_rev110_diag_file, update_names=FALSE))

  upasv2x_rev157_filename <- 'PSP00270_LOG_2024-06-25T21_37_48UTC_GPS-in-out______----------.txt'
  upasv2x_rev157_file <- system.file("extdata", upasv2x_rev157_filename, package = "astr", mustWork = TRUE)
  expect_snapshot(read_ast_log(upasv2x_rev157_file, update_names=FALSE))

  upasv2x_rev158_noGPS_filename <- 'PSP00270_LOG_2024-06-14T18_54_44UTC_NoGPS___________----------.txt'
  upasv2x_rev158_noGPS_file <- system.file("extdata", upasv2x_rev158_noGPS_filename, package = "astr", mustWork = TRUE)
  expect_snapshot(read_ast_log(upasv2x_rev158_noGPS_file, update_names=FALSE))

  upasv2x_rev158_diag_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
  upasv2x_rev158_diag_file <- system.file("extdata", upasv2x_rev158_diag_filename, package = "astr", mustWork = TRUE)
  expect_snapshot(read_ast_log(upasv2x_rev158_diag_file, update_names=FALSE))
})

test_that("read_ast_log works with all HHBv2 firmwares", {
  hhb_filename <- 'HHB00032_LOG_2024-07-01T18_20UTC.csv'
  hhb_file <- system.file("extdata", hhb_filename, package = "astr", mustWork = TRUE)
  expect_snapshot(read_ast_log(hhb_file))
})
