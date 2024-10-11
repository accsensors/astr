#'Read and format the header data from an Access Sensor Technologies air sampler log file
#'
#' @description
#' `read_ast_header()` reads in the header data from a log file, transposes the
#' data into a wide format, and applies device-specific formatting to the
#' columns of the resulting data frame. This function sets the proper data type
#' for each variable, adds a column to specify the AST sampler type, adds
#' columns to describe the codes associated with the ShutdownMode and
#' PMSensorInterval variables, and can also be directed to update deprecated
#' variable names to the current variable names.
#'
#' Use this function in conjuction with [base::lapply()] or
#' [purrr::map()] to read in header data from any number of log files
#' and combine those data into a single data frame that contains a unique row
#' for each file.
#'
#' @param file Any Access Sensor Technologies air sampler log file name.
#'
#' @param update_names Option to update any deprecated variable names from log
#' files written using older firmware versions to the variable names used in the
#' current firmware version. Variable names cannot be updated for files
#' written using UPAS v2 firmware versions preceding rev100.
#'
#' @param tz Optional: A character string specifying the tz database time zone
#' that should be used to display local times. Example tz database time zones
#' include: "America/New_York", "America/Denver", and "America/Los_Angeles".
#' For additional information, see: \url{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones}
#'
#' If the GPSUTCOffset in your log file is a fraction of an hour, it's
#' best to specify the optional `tz` parameter; otherwise, local times in the log file
#' will be incorrectly labeled as UTC time.
#'
#' @return A data frame with a single row of header data that are formatted and ready for analysis.
#'
#' @details
#' If `update_names = TRUE`, then, for samples collected using UPAS v2 firmware
#' versions beyond rev100, the deprecated names shown on the left will be
#' updated to the current names shown on the right:
#' \tabular{ll}{
#'    \strong{Deprecated name}  \tab \strong{Current name}  \cr
#'    VolumetricFlowRate        \tab FlowRateSetpoint       \cr
#'    DutyCycle                 \tab FlowDutyCycle          \cr
#'    LoggedRuntime             \tab OverallDuration        \cr
#'    SampledRuntime            \tab PumpingDuration        \cr
#'    AverageVolumetricFlowRate \tab PumpingFlowRateAverage \cr
#' }
#'
#' If `update_names = TRUE`, then, for samples collected using UPAS v2 firmware
#' rev100, the deprecated names shown on the left will be updated to the current
#' names shown on the right:
#' \tabular{ll}{
#'    \strong{Deprecated name} \tab \strong{Current name}  \cr
#'    CumulativeSamplingTime   \tab LifetimeSampleRuntime  \cr
#'    StartDateTime            \tab StartDateTimeUTC       \cr
#'    AverageVolumetricFlow    \tab PumpingFlowRateAverage \cr
#' }
#'
#' If the GPSUTCOffset in the file header is a whole number of hours, this
#' function will be able to convert local times to POSIXct format automatically,
#' without the `tz` argument being specified. If the GPSUTCOffset is not a whole
#' number of hours, it's better to specify the local time zone as a character
#' string using the `tz` argument.
#'
#' The data frame returned by this function will include all variables from the
#' log file header. Additionally, for UPAS log files, the following columns will
#' be appended:
#' \itemize{
#'    \item ASTSampler: A string indicating the model of the sampler, e.g., UPAS_v2
#'    \item FirmwareRev: A numeric value indicating the firmware revision number
#'    \item ShutdownReason: A string indicating the meaning of the ShutdownMode number
#'    \item PMSensorOperation: A string indicating the meaning of the PMSensorInterval number (for UPAS v2.1 PLUS only)
#' }
#'
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' # UPAS v2 EXAMPLES
#' upasv2_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
#' upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
#' upasv2_header <- read_ast_header(upasv2_file, update_names=FALSE)
#'
#' # Use of `update_names` with UPAS v2 log file
#' upasv2_header_updatednames <- read_ast_header(upasv2_file, update_names=TRUE)
#'
#' # UPAS v2.1 PLUS EXAMPLES
#' upasv2x_filename <- 'PSP00270_LOG_2024-07-11T18_01_22UTC_PM_CO2_Map______----------.txt'
#' upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
#' upasv2x_header <- read_ast_header(upasv2x_file, update_names=FALSE)
#'
#' # Diagnostic file
#' upasv2x_diag_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
#' upasv2x_diag_file <- system.file("extdata", upasv2x_diag_filename, package = "astr",
#'                                   mustWork = TRUE)
#' upasv2x_diag_header <- read_ast_header(upasv2x_diag_file, update_names=FALSE)
#'
#' # Use `base::lapply()` to read in multiple UPAS files and combine the data
#' # from those files into a single data frame with one row for each file.
#' # The `purrr::map()` function can also be used in place of `lapply()`.
#' multiple_upas_headers <- system.file("extdata", package = "astr", mustWork = TRUE) |>
#'     list.files(pattern="^PS.*.txt$", full.names = TRUE) %>%
#'     lapply(read_ast_header, update_names = TRUE) %>%
#'     dplyr::bind_rows()
#'
#' # To change the type of device log file being read in the above example,
#' # change the `pattern` argument in `list.files()` as follows:
#' # UPAS v2: `pattern = "^PS[1-9].*.txt$"`
#' # UPAS v2.1 and v2.1 PLUS: `pattern = "^PSP.*.txt$"`
#' # HHB v2: `pattern = "^HHB.*.csv$"`
#'
#' # HHB v2 EXAMPLES
#' hhb_filename <- 'HHB00032_LOG_2024-07-01T18_20UTC.csv'
#' hhb_file <- system.file("extdata", hhb_filename, package = "astr", mustWork = TRUE)
#' hhb_header <- read_ast_header(hhb_file)

read_ast_header = function(file, update_names=FALSE, tz=NA) {

  data <- astr::fread_ast_header(file)

  df <- astr::transpose_ast_header(data$header, diag=data$diag)

  df <- astr::format_ast_header(df, update_names=update_names, tz=tz)

  return(df)
}

#'Use fread to read the header data from an Access Sensor Technologies air sampler log file
#'
#' @description
#' `fread_ast_header()` uses fread to read the header data and, if applicable, the
#' diagnostic test data from the specified file.
#'
#' @param file Any Access Sensor Technologies air sampler log file name.
#'
#' @return A list containing two items named `header` and `diag`. The `header`
#' item is a data table that contains the header data and the `diag` item is a
#' data table that contains the diagnostic test data. These data tables contain
#' the data formatted exactly as they appear in the log file.
#' @export
#' @importFrom rlang .data
#' @examples
#' # UPASv2 EXAMPLES
#' upasv2_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
#' upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
#' upasv2_header_list <- fread_ast_header(upasv2_file)
#' upasv2_header <- upasv2_header_list$header
#'
#' # UPASv2x EXAMPLES
#' upasv2x_filename <- 'PSP00270_LOG_2024-07-11T18_01_22UTC_PM_CO2_Map______----------.txt'
#' upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
#' upasv2x_header_list <- fread_ast_header(upasv2x_file)
#' upasv2x_header <- upasv2x_header_list$header
#'
#' # Diagnostic file
#' upasv2x_diag_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
#' upasv2x_diag_file <- system.file("extdata", upasv2x_diag_filename, package = "astr",
#'                                   mustWork = TRUE)
#' upasv2x_diag_header_list <- fread_ast_header(upasv2x_diag_file)
#' upasv2x_diag_header <- upasv2x_diag_header_list$header
#' upasv2x_diag_diag <- upasv2x_diag_header_list$diag
#'
#' # HHB EXAMPLES
#' hhb_filename <- 'HHB00032_LOG_2024-07-01T18_20UTC.csv'
#' hhb_file <- system.file("extdata", hhb_filename, package = "astr", mustWork = TRUE)
#' hhb_header <- fread_ast_header(hhb_file)$header

fread_ast_header = function(file) {

  nrows_header <- astr::count_header_rows(file) # Determine no. of header rows

  # Read header data
  header <- data.table::fread(file, sep = ',', header = FALSE, fill = TRUE,
                              skip  = 0,
                              nrows = nrows_header$nrow_header_no_blanks,
                              blank.lines.skip = TRUE, stringsAsFactors = FALSE)

  # If the file is a diagnostic file, read the diagnostic test data
  if(nrows_header$is_diag == TRUE){

    diag <- data.table::fread(file, sep = ',', header = FALSE, fill = TRUE,
                              skip  = nrows_header$nrow_header_with_blanks + 2,
                              nrows = nrows_header$nrow_diag_no_blanks - 1,
                              blank.lines.skip = TRUE,
                              stringsAsFactors = FALSE)
  }else{
    diag <- NULL
  }

  return(list(header = header, diag = diag))
}

#'Transpose the header data from an Access Sensor Technologies air sampler log
#'file and format the transposed header as a data frame.
#'
#' @description
#' `transpose_ast_header()` Takes the data read using the [astr::fread_ast_header()]
#' function and transposes those data so each variable in the header is a unique
#' column in a data frame.
#'
#' @param header A data table containing header data read using the [astr::fread_ast_header()] function.
#' @param diag An optional data table containing diagnostic test data read using
#' the [astr::fread_ast_header()] function. If the `diag` argument is
#' specified, key metrics from the diagnostic test will be added to the header
#' data frame.
#'
#' @return A data frame of header data.
#' @export
#' @importFrom rlang .data
#' @examples
#' # UPASv2 EXAMPLES
#' upasv2_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
#' upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
#' upasv2_header_list <- fread_ast_header(upasv2_file)
#' upasv2_header_wide <- transpose_ast_header(upasv2_header_list$header)
#'
#' # UPASv2x EXAMPLES
#' upasv2x_filename <- 'PSP00270_LOG_2024-07-11T18_01_22UTC_PM_CO2_Map______----------.txt'
#' upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
#' upasv2x_header_list <- fread_ast_header(upasv2x_file)
#' upasv2x_header_wide <- transpose_ast_header(upasv2x_header_list$header)
#'
#' # Diagnostic file
#' upasv2x_diag_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
#' upasv2x_diag_file <- system.file("extdata", upasv2x_diag_filename, package = "astr",
#'                                 mustWork = TRUE)
#' upasv2x_diag_header_list <- fread_ast_header(upasv2x_diag_file)
#' upasv2x_diag_header_wide <- transpose_ast_header(upasv2x_diag_header_list$header,
#'                                 upasv2x_diag_header_list$diag)
#'
#' # HHB EXAMPLES
#' hhb_filename <- 'HHB00032_LOG_2024-07-01T18_20UTC.csv'
#' hhb_file <- system.file("extdata", hhb_filename, package = "astr", mustWork = TRUE)
#' hhb_header_raw <- fread_ast_header(hhb_file)$header
#' hhb_header_wide <- transpose_ast_header(hhb_header_raw)

transpose_ast_header = function(header, diag = NULL){

  df <- header[!((header$V2 == "") & (header$V3 == "")), 1:2] # Remove subheads
  df <- df[df$V1 != "PARAMETER", ] # Remove PARAMETER,VALUE,UNITS/NOTES header

  df <- t(df) # Transpose
  df <- as.data.frame(df) # Convert into data frame

  colnames(df) <- df[1, ] # Assign column names

  df <- df[-1, ] # Keep only row with data

  rownames(df) <- NULL # Remove row name

  df[df == ''] <- NA # Replace missing values with NA

  # Rename any column that contains 'firmware' to 'Firmware'
  if(sum(grepl("firmware", colnames(df), ignore.case=T))==1){

    colnames(df) <- gsub(colnames(df)[grep("firmware", colnames(df),
                                           ignore.case=T)],
                         "Firmware", colnames(df))}

  if(!is.null(diag)){ # If there are diagnostic test data

    colnames(diag) <- as.character(diag[6,])

    if(grepl("UPAS_v2_0", df$Firmware)){
      # Rename variable names to v2_x and SHEAR variable names
      diag <- dplyr::rename(diag, PCB2P="PCBP", MFSVout="MFSVolt", FilterDP="FdPdP")
    }


    diag <- diag[-which(diag$PCB2P %in% c("","(hPa)","PCBP","PCB2P")),]

    diag <- dplyr::mutate(diag, dplyr::across(dplyr::everything(),
                                              \(x) as.numeric(x)))

    rownames(diag) <- c('noFlow','maxDeadhead','maxFlow','minFlow')

    df <- dplyr::mutate(df,
      MFSDIAGVoutBlocked   = diag[rownames(diag)=="maxDeadhead",]$MFSVout,
      MFSDIAGVoutMax       = diag[rownames(diag)=="maxFlow",]$MFSVout,
      MFSDIAGVoutMin       = diag[rownames(diag)=="minFlow",]$MFSVout,
      MFSDIAGMFBlocked     = diag[rownames(diag)=="maxDeadhead",]$MassFlow,
      MFSDIAGMFMax         = diag[rownames(diag)=="maxFlow",]$MassFlow,
      MFSDIAGMFMin         = diag[rownames(diag)=="minFlow",]$MassFlow,
      MFSDIAGPumpVBoostMax = diag[rownames(diag)=="maxFlow",]$PumpV,
      MFSDIAGPumpVBoostMin = diag[rownames(diag)=="minFlow",]$PumpV,
      MFSDIAGPDeadhead     = diag[rownames(diag)=="maxDeadhead",]$FilterDP)
  }

  return(df)
}


#'Format header data from an Access Sensor Technologies air sampler log file to
#'to make sure each column in the header data frame is the correct data type.
#'
#' @description
#' `format_ast_header()` applies device-specific formatting to the columns in a
#' data frame of header data from an Access Sensor Technologies air sampler
#' log file. This function sets the proper data type for each variable, adds a
#' column to specify the AST sampler type, adds columns to describe the codes
#' wassociated ith the ShutdownMode and PMSensorInterval, and can also be
#' directed to update old variable names to current variable names.
#'
#' @param data A header data frame returned by the [astr::transpose_ast_header()] function.
#' @inheritParams read_ast_header
#'
#' @return A data frame with a single row of header data that are formatted and ready for analysis.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' # UPASv2 EXAMPLES
#' upasv2_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
#' upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
#' upasv2_header_list <- fread_ast_header(upasv2_file)
#' upasv2_header_wide <- transpose_ast_header(upasv2_header_list$header)
#' upasv2_header <- format_ast_header(upasv2_header_wide, update_names=TRUE)
#'
#' # UPASv2x EXAMPLES
#' upasv2x_filename <- 'PSP00270_LOG_2024-07-11T18_01_22UTC_PM_CO2_Map______----------.txt'
#' upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
#' upasv2x_header_list <- fread_ast_header(upasv2x_file)
#' upasv2x_header_wide <- transpose_ast_header(upasv2x_header_list$header)
#' upasv2x_header <- format_ast_header(upasv2x_header_wide, update_names=FALSE)
#'
#' # Diagnostic file
#' upasv2x_diag_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
#' upasv2x_diag_file <- system.file("extdata", upasv2x_diag_filename, package = "astr",
#'                                   mustWork = TRUE)
#' upasv2x_diag_header_list <- fread_ast_header(upasv2x_diag_file)
#' upasv2x_diag_header_wide <- transpose_ast_header(upasv2x_diag_header_list$header,
#'                                   upasv2x_diag_header_list$diag)
#' upasv2x_diag_header <- format_ast_header(upasv2x_diag_header_wide, update_names=FALSE)
#'
#' # HHB EXAMPLES
#' hhb_filename <- 'HHB00032_LOG_2024-07-01T18_20UTC.csv'
#' hhb_file <- system.file("extdata", hhb_filename, package = "astr", mustWork = TRUE)
#' hhb_header_raw <- fread_ast_header(hhb_file)$header
#' hhb_header_wide <- transpose_ast_header(hhb_header_raw)
#' hhb_header <- format_ast_header(hhb_header_wide)

format_ast_header = function(data, update_names=FALSE, tz=NA) {

  firmware <- data$Firmware

  if(grepl("UPAS_v2_x", firmware) | grepl("SHEARv2_7_2", firmware)){

    df_h <- astr::format_upasv2x_header(data, tz=tz)

  }else if(grepl("UPAS_v2_0", firmware)){

    df_h <- astr::format_upasv2_header(data, update_names=update_names, tz=tz)

  }else if(grepl("HHBv2", firmware)){

    df_h <- astr::format_hhb_header(data)

  }

  return(df_h)
}
