#'Read the sample log data from an Access Sensor Technologies air sampler log file
#'
#' @description
#' `read_ast_log` reads in the sample log data frome a log file and applies
#' device-specific formatting to the columns of the resulting data frame.
#' It sets the proper data type for each variable and adds columns that aid in
#' identifying unique log files when data from multiple sample logs have been
#' combined into a single data frame.
#'
#' @param file Any AST air sampler log file name.
#' @param update_names Option to update any deprecated variable names from log files recorded using older firmware versions to the variable names used in the current firmware version.
#'
#' For samples collected using UPAS v2.1 PLUS firmware versions preceding revXXX,
#' the column name "AceelComplianceHrs" will be updated to "AccelComplianceHrs".
#'
#' For samples collected using the UPAS v2, the old names shown on the left will
#' be updated to the names shown on the right:
#' VolumetricFlowRate -> PumpingFlowRate
#' * AtmoRho          -> AtmoDensity
#' * FdPdP            -> FilterDP
#' * PumpT            -> AtmoT
#' * PumpRH           -> AtmoRH
#' * PCBT             -> PCB1T
#' * PumpP            -> PCB2P
#' * PCBP             -> AtmoP
#' * GPShdop          -> GPShDOP
#' * BFGvolt          -> BattVolt
#'
#' For samples collected specifically using UPAS v2 firmware rev100, the old
#' names shown on the left will be updated to the names shown on the right:
#' * UTCDateTime     -> DateTimeUTC
#' * VolFlow         -> VolumetricFlowRate
#' * UPASLogFilename -> LogFilename
#'
#' Variable names cannot be updated for log files written using UPAS v2 firmware
#' versions preceding rev100.
#'
#' @param tz Optional: A character string specifying the tz database time zone that should be used to display local times.
#'
#' Example tz database time zones include: "America/New_York", "America/Denver", and "America/Los_Angeles".
#' For additional information, see: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
#'
#' If your GPSUTCOffset is a whole number of hours, this function will be able
#' to automatically convert the DateTimeLocal variable in the sample log to a
#' POSIXct object displayed in the local time zone, without this tz parameter
#' being specified.  If your GPSUTCOffset is not a whole number of hours, it's
#' better to specify the local time zone as a character string here.
#'
#' @param cols_keep Optional: Provide a character vector specifying the names of a subset of sample log columns to keep.
#' @param cols_drop Optional: Provide a character vector specifying the names of a subset of sample log columns to remove.
#'
#' Column selection will occur in the same order in which the function arguments are specified above.
#' In other words, the columns specified in cols_keep will be selected first.
#' If the cols_keep argument is not specified, all columns will be kept.  Then,
#' The columns specified in cols_drop will be dropped.  If the cols_drop
#' argument is not specified, no columns will be dropped.
#'
#' @param shiny Option to make TRUE if using function with AST shiny app.
#'
#' @return A data frame of of sample log data that are formatted and ready for analysis.
#' This data frame will contain one row for each timestamp in the sample log.
#' Columns with key header data will be appended to the sample log columns to
#' aid in identification and analysis of unique samples.
#'
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2_filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
#' upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
#' upasv2_log <- read_ast_log(upasv2_file)
#' upasv2x_rev81_filename <- 'PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt'
#' upasv2x_rev81_file <- system.file("extdata", upasv2x_rev81_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev81_log <- read_ast_log(upasv2x_rev81_file)
#' filename2 <- 'SH00007_LOG_2021-12-13T13_28_41UTC_---------------_-----.txt'
#' file2 <- system.file("extdata", filename2, package = "astr", mustWork = TRUE)
#' data_ast_log <- read_ast_log(file2)
#' data_ast_log <- read_ast_log(file2, cols_keep = c("SampleTime","UnixTime","DateTimeUTC","DateTimeLocal","PM2_5MC"))
#' upasv2x_diag_filename <- 'PSP00055_LOG_2022-03-24T18_05_32UTC_DIAGNOSTIC________________.txt'
#' upasv2x_diag_file <- system.file("extdata", upasv2x_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2x_diag_log <- read_ast_log(upasv2x_diag_file, update_names=FALSE)
#' upasv2x_rev157_filename <- 'PSP00270_LOG_2024-06-25T21_37_48UTC_GPS-in-out______----------.txt'
#' upasv2x_rev157_file <- system.file("extdata", upasv2x_rev157_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev157_log <- read_ast_log(upasv2x_rev157_file, update_names=FALSE)

read_ast_log = function(file, update_names=FALSE, tz=NA, cols_keep=c(), cols_drop=c(), shiny=FALSE) {

  log <- astr::fread_ast_log(file)

  header <- astr::read_ast_header(file)

  df <- astr::format_ast_log(log, header, update_names, tz, cols_keep, cols_drop, shiny=shiny)

  return(df)
}

#'Use fread to read the sample log data from an Access Sensor Technologies air sampler log file
#'
#' @description
#' `fread_ast_log` reads in the sample log data exactly as it appears in the file.
#'
#' @param file Any Access Sensor Technologies air sampler log file name.
#'
#' @return A data frame of unformatted sample log data
#' @export
#'
#' @examples
#' upasv2_rev125_filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
#' upasv2_rev125_file <- system.file("extdata", upasv2_rev125_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev125_log_raw <- fread_ast_log(upasv2_rev125_file)
#' upasv2_rev130_diag_filename <- 'PS1786_LOG_2023-03-02T21_45_43UTC_DIAGNOSTIC____________.txt'
#' upasv2_rev130_diag_file <- system.file("extdata", upasv2_rev130_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev130_diag_log_raw <- fread_ast_log(upasv2_rev130_diag_file)
#' upasv2_rev138_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
#' upasv2_rev138_file <- system.file("extdata", upasv2_rev138_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev138_log_raw <- fread_ast_log(upasv2_rev138_file)
#' upasv2x_rev81_filename <- 'PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt'
#' upasv2x_rev81_file <- system.file("extdata", upasv2x_rev81_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev81_log_raw <- fread_ast_log(upasv2x_rev81_file)
#' upasv2x_rev117_filename <- 'PSP00030_LOG_2022-05-11T23_24_01UTC_---------------_----------.txt'
#' upasv2x_rev117_file <- system.file("extdata", upasv2x_rev117_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev117_log_raw <- fread_ast_log(upasv2x_rev117_file)
#' upasv2x_rev110_diag_filename <- 'PSP00055_LOG_2022-03-24T18_05_32UTC_DIAGNOSTIC________________.txt'
#' upasv2x_rev110_diag_file <- system.file("extdata", upasv2x_rev110_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev110_diag_log_raw <- fread_ast_log(upasv2x_rev110_diag_file)
#' upasv2x_rev157_filename <- 'PSP00270_LOG_2024-06-25T21_37_48UTC_GPS-in-out______----------.txt'
#' upasv2x_rev157_file <- system.file("extdata", upasv2x_rev157_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev157_log_raw <- fread_ast_log(upasv2x_rev157_file)
#' upasv2x_rev158_diag_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
#' upasv2x_rev158_diag_file <- system.file("extdata", upasv2x_rev158_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev158_diag_log_raw <- fread_ast_log(upasv2x_rev158_diag_file)

fread_ast_log = function(file){

  nrows_header <- astr::count_header_rows(file)

  # Need to account for blank lines when using skip in fread
  nrow_header_skip <- nrows_header$nrow_header_with_blanks +
                      nrows_header$nrow_diag_with_blanks + 2

  # Read sample log data
  df <- data.table::fread(file, sep = ',', header = TRUE, fill = TRUE,
                          skip = nrow_header_skip,
                          blank.lines.skip = TRUE, stringsAsFactors = FALSE)

  if(df$SampleTime[1] == "(HH:MM:SS)"){ # For files with units below header
    df <- df[-1, ] # Remove row with units
  }

  return(df)
}

#'Format the sample log data from an Access Sensor Technologies air sampler log file
#'
#' @description
#' `format_ast_log` Applies device-specific formatting to the columns of a
#' sample log data frame returned by the [fread_ast_log] function. It sets the
#' proper data type for each variable and adds columns that aid in identifying
#' unique log files when data from multiple sample logs have been combined into
#' a single data frame.
#'
#' @param log An unformatted data frame of sample log data.
#' @param header A formatted data frame of header data.
#' @param update_names Option to update any deprecated variable names from log files recorded using older firmware versions to the variable names used in the current firmware version.
#' See [read_ast_log] for additional information.
#' @param tz Optional: A character string specifying the tz database time zone that should be used to display local times.
#' See [read_ast_log] for additional information.
#' @param cols_keep Optional: Provide a character vector specifying the names of a subset of sample log columns to keep.
#' @param cols_drop Optional: Provide a character vector specifying the names of a subset of sample log columns to remove.
#' See [read_ast_log] for additional information.
#' @param shiny Option to make TRUE if using this function with AST shiny app.
#'
#' @return A data frame with formatted sample log data plus key header data appended.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' data_ast_log <- format_ast_log(upasv2x_header, data_ast_raw)

format_ast_log = function(log, header, update_names=FALSE, tz=NA, cols_keep=c(), cols_drop=c(), shiny=FALSE) {

  firmware <- header$Firmware

  if(nrow(log)>0){

    if(grepl("UPAS_v2_x", firmware) | grepl("SHEARv2_7_2", firmware)){

      df <- astr::format_upasv2x_log(log, header, update_names, tz, cols_keep, cols_drop)

    }else if(grepl("UPAS_v2_0", firmware)){

      if(shiny){update_names <- TRUE} #TODO move to own function format_shiny_header so that shiny functionality is not present in normal functions

      df <- astr::format_upasv2_log(log, header, update_names, tz, cols_keep, cols_drop)

    }else if(grepl("HHBv2", firmware)){

      df <- astr::format_hhb_log(log, header, tz, cols_keep, cols_drop)

    }

    if(shiny){df <- astr::shiny_log(df)} #TODO move to own function format_shiny_log so that shiny functionality is not present in normal functions
  }

  return(df)
}
