#'Read the sample log data from an Access Sensor Technologies air sampler log file
#'
#' @description
#' `read_ast_log()` reads in the sample log data from a log file and applies
#' device-specific formatting to the columns of the resulting data frame.
#' It sets the proper data type for each variable and adds columns that aid in
#' identifying unique log files when data from multiple sample logs have been
#' combined into a single data frame.
#'
#' Use this function in conjuction with [base::lapply()] or
#' [purrr::map()] to read in log data from any number of files and
#' combine those data into a single data frame.
#'
#' @inheritParams read_ast_header
#'
#' @param cols_keep Optional: A character vector specifying the names of a subset of sample log columns to keep.
#' @param cols_drop Optional: A character vector specifying the names of a subset of sample log columns to remove.
#'
#' @return A data frame of of sample log data that are formatted and ready for analysis.
#' This data frame will contain one row for each timestamp in the sample log.
#' If neither `cols_keep` nor `cols_drop` are specified, all variables in the
#' sample log will be included in the data frame. Otherwise, the variables
#' specified using `cols_keep` will be included and the variables specified
#' using `cols_drop` will be excluded. Additionally, columns with key header
#' data will be appended to aid in identification of unique samples.
#'
#' @details
#' If `update_names = TRUE`, and deprecated parameter names in the log file
#' will be updated to the current names:
#' \tabular{ll}{
#'    \strong{Deprecated name} \tab \strong{Current name} \cr
#'    UTCDateTime        \tab DateTimeUTC            \cr
#'    VolFlow            \tab PumpingFlowOffset      \cr
#'    VolumetricFlowRate \tab PumpingFlowOffset      \cr
#'    PumpingFlowRate    \tab PumpingFlowOffset      \cr
#'    OverallFlowRate    \tab OverallFlowOffset      \cr
#'    SampledVolume      \tab SampledVolumeOffset    \cr
#'    FdPdP              \tab FilterDP               \cr
#'    PumpT              \tab AtmoT                  \cr
#'    PCBP               \tab AtmoP                  \cr
#'    PumpRH             \tab AtmoRH                 \cr
#'    AtmoRho            \tab AtmoDensity            \cr
#'    GPShdop            \tab GPShDOP                \cr
#'    AceelComplianceHrs \tab AccelComplianceHrs     \cr
#'    PM2_5SampledMass   \tab PM2_5SampledMassOffset \cr
#'    PCBT               \tab U12T                   \cr
#'    PCB1T              \tab U12T                   \cr
#'    PCB2T              \tab U29T                   \cr
#'    PumpP              \tab U29P                   \cr
#'    PCB2P              \tab U29P                   \cr
#'    MassFlow           \tab MassFlowFactory        \cr
#'    BFGvolt            \tab BattVolt               \cr
#' }
#'
#' If the GPSUTCOffset in the log file header is a whole number of hours, this
#' function will be able to display the DateTimeLocal variable as a POSIXct
#' with the local time zone automatically, without the `tz` argument being
#' specified.  If the GPSUTCOffset is not a whole number of hours, it's better
#' to specify the local time zone as a character string using the `tz` argument.
#'
#' If the `cols_keep` or `cols_drop` arguments are specified, column selection
#' will occur in the same order in which these arguments are specified above.
#' In other words, columns specified in `cols_keep` will be selected first. If
#' the `cols_keep` argument is not specified, all columns will be kept. Then,
#' columns specified in `cols_drop` will be dropped.  If the `cols_drop`
#' argument is not specified, no columns will be dropped.
#'
#' The data frame returned by this function will include all variables from the
#' sample log.  For all log files, the following columns will also be appended:
#' \itemize{
#'    \item SampleName: A string indicating the user-supplied sample name
#'    \item UserTZ: A boolean value indicating whether the `tz` argument was supplied to this function
#'    \item LocalTZ: A string indicating the timezone associated with DateTimeLocal values
#'    \item StartDateTimeUTC: A POSIXct object indicating the date and time when sample started (in coordinated universal time)
#' }
#'
#' For UPAS log files, the following columns will also be appended:
#' \itemize{
#'    \item ASTSampler: A string indicating the model of the sampler, e.g., UPAS_v2
#'    \item UPASserial: A numeric value indicating the UPAS serial ID
#'    \item LogFilename: A string indicating the log filename
#'    \item CartridgeID: A string indicating the cartridge identifier entered by the user into the mobile application
#'    \item LogFileMode: A string indicating whether this is a "normal" log file or a "debug" log file (UPAS v2 only)
#' }
#'
#' For HHB v2 log files, the following columns will be appended:
#' \itemize{
#'    \item DateTimeLocal: DateTimeUTC displayed in the local time zone
#'    \item HHBserial: A string indicating the HHB serial ID
#'    \item LogFileName: A string indicating the log filename
#'    \item G.Alphasense1_ID: Serial number of Alphasense B-series electrochemical sensor in gas sensor housing position 1 (if installed)
#'    \item G.Alphasense2_ID: Serial number of Alphasense B-series electrochemical sensor in gas sensor housing position 2 (if installed)
#'    \item G.Alphasense3_ID: Serial number of Alphasense B-series electrochemical sensor in gas sensor housing position 3 (if installed)
#'    \item G.Alphasense4_ID: Serial number of Alphasense B-series electrochemical sensor in gas sensor housing position 4 (if installed)
#'    \item A.FilterCID: Identifier for filter cartridge installed in Channel A (as entered by the user)
#'    \item B.FilterCID: Identifier for filter cartridge installed in Channel B (as entered by the user)
#'    \item C.SorbentCID: Identifier for sorbent media installed in Channel C (as entered by the user)
#'    \item D.SorbentCID: Identifier for sorbent media installed in Channel D (as entered by the user)
#'    \item A.FilterVolumetricFlowRate: Programmed volumetric sample flow rate for Channel A
#'    \item B.FilterVolumetricFlowRate: Programmed volumetric sample flow rate for Channel B
#'    \item C.SorbentVolumetricFlowRate: Programmed volumetric sample flow rate for Channel C
#'    \item D.SorbentVolumetricFlowRate: Programmed volumetric sample flow rate for Channel D
#' }
#'
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' # UPAS v2 EXAMPLES
#' upasv2_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
#' upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
#' upasv2_log <- read_ast_log(upasv2_file, update_names=FALSE)
#'
#' # Use of `update_names` with UPAS v2 log file
#' upasv2_log_updatednames <- read_ast_log(upasv2_file, update_names=TRUE)
#'
#' # UPAS v2.1 and v2.1 PLUS EXAMPLES
#' upasv2x_filename <- 'PSP00270_LOG_2024-07-11T18_01_22UTC_PM_CO2_Map______----------.txt'
#' upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
#' upasv2x_log <- read_ast_log(upasv2x_file, update_names=FALSE)
#'
#' # Use of cols_drop, cols_keep, and tz  with a UPAS v2.1 PLUS log file
#' upasv2x_log_colsdrop <- read_ast_log(upasv2x_file,
#'                 cols_drop = c("DateTimeLocal", "AtmoT", "AtmoP", "AtmoRH"))
#' upasv2x_log_colskeep <- read_ast_log(upasv2x_file, tz="America/New_York",
#'                 cols_keep = c("SampleTime", "DateTimeUTC", "DateTimeLocal",
#'                                "LocalTZ",  "UserTZ", "AtmoT", "AtmoP", "AtmoRH"))
#'
#' # GPS disabled file
#' upasv2x_noGPS_filename <- 'PSP00270_LOG_2024-06-14T18_54_44UTC_NoGPS___________----------.txt'
#' upasv2x_noGPS_file <- system.file("extdata", upasv2x_noGPS_filename, package = "astr",
#'                                    mustWork = TRUE)
#' upasv2x_noGPS_log <- read_ast_log(upasv2x_noGPS_file, update_names=FALSE)
#'
#' # Diagnostic file
#' upasv2x_diag_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
#' upasv2x_diag_file <- system.file("extdata", upasv2x_diag_filename, package = "astr",
#'                                   mustWork = TRUE)
#' upasv2x_diag_log <- read_ast_log(upasv2x_diag_file, update_names=FALSE)
#'
#' # Use `base::lapply()` to read multiple files and combine the data from those
#' # files into a single data frame. A column with the log filename will be
#' # appended to the sample log data so that each sample can be identified easily.
#' # The `purrr::map()` function can also be used in place of `lapply()`.
#' multiple_upas_logs <- system.file("extdata", package = "astr", mustWork = TRUE) |>
#'     list.files(pattern="^PS.*.txt$", full.names = TRUE) %>%
#'     lapply(read_ast_log, update_names = TRUE) %>%
#'     dplyr::bind_rows()
#'
#' # To change the type of device log file being read in the above example,
#' # change the `pattern` argument in [list.files] as follows:
#' # UPAS v2: `pattern = "^PS[1-9].*.txt$"`
#' # UPAS v2.1 and v2.1 PLUS: `pattern = "^PSP.*.txt$"`
#' # HHB v2: `pattern = "^HHB.*.csv$"`
#'
#' # HHB v2 EXAMPLES
#' hhb_filename <- 'HHB00032_LOG_2024-07-01T18_20UTC.csv'
#' hhb_file <- system.file("extdata", hhb_filename, package = "astr", mustWork = TRUE)
#' hhb_log <- read_ast_log(hhb_file)

read_ast_log = function(file, update_names=FALSE, tz=NA, cols_keep=c(), cols_drop=c()) {

  log <- astr::fread_ast_log(file)

  header <- astr::read_ast_header(file, update_names, tz)

  df <- astr::format_ast_log(log, header, update_names, tz, cols_keep, cols_drop)

  return(df)
}

#'Use fread to read the sample log data from an Access Sensor Technologies air sampler log file
#'
#' @description
#' `fread_ast_log()` reads in the sample log data exactly as it appears in the file.
#'
#' @param file Any Access Sensor Technologies air sampler log file name.
#'
#' @return A data frame of unformatted sample log data
#' @export
#'
#' @examples
#' # UPASv2 EXAMPLES
#' upasv2_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
#' upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
#' upasv2_log_raw <- fread_ast_log(upasv2_file)
#'
#' # UPASv2x EXAMPLES
#' upasv2x_filename <- 'PSP00270_LOG_2024-07-11T18_01_22UTC_PM_CO2_Map______----------.txt'
#' upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
#' upasv2x_log_raw <- fread_ast_log(upasv2x_file)
#'
#' # Diagnostic file
#' upasv2x_diag_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
#' upasv2x_diag_file <- system.file("extdata", upasv2x_diag_filename, package = "astr",
#'                                   mustWork = TRUE)
#' upasv2x_diag_log_raw <- fread_ast_log(upasv2x_diag_file)
#'
#' # HHB EXAMPLES
#' hhb_filename <- 'HHB00032_LOG_2024-07-01T18_20UTC.csv'
#' hhb_file <- system.file("extdata", hhb_filename, package = "astr", mustWork = TRUE)
#' hhb_log_raw <- fread_ast_log(hhb_file)

fread_ast_log = function(file){

  nrows_header <- astr::count_header_rows(file)

  # Need to account for blank lines when using skip in fread
  nrow_header_skip <- nrows_header$nrow_header_with_blanks +
                      nrows_header$nrow_diag_with_blanks + 2

  # Read sample log data
  df <- data.table::fread(file, sep = ',', header = TRUE, fill = TRUE,
                          skip = nrow_header_skip,
                          blank.lines.skip = TRUE, stringsAsFactors = FALSE)

  if(nrow(df) > 0){ # If there is sample log data
    if(df$SampleTime[1] == "(HH:MM:SS)"){ # For files with units below header
      df <- df[-1, ] # Remove row with units
    }
  }

  return(df)
}

#'Format the sample log data from an Access Sensor Technologies air sampler log file
#'
#' @description
#' `format_ast_log()` Applies device-specific formatting to the columns of a
#' sample log data frame returned by the [astr::fread_ast_log()] function. It sets the
#' proper data type for each variable and adds columns that aid in identifying
#' unique log files when data from multiple sample logs have been combined into
#' a single data frame.
#'
#' @param log An unformatted data frame of sample log data returned by the [astr::fread_ast_log()] function.
#' @param header A formatted data frame of header data returned by the [astr::read_ast_header()] function.
#' @inheritParams read_ast_log
#'
#' @return A data frame with formatted sample log data plus key header data appended.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' # UPASv2 EXAMPLES
#' upasv2_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
#' upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
#' upasv2_log_raw <- fread_ast_log(upasv2_file)
#' upasv2_header <- read_ast_header(upasv2_file, update_names=FALSE)
#' upasv2_log <- format_ast_log(upasv2_log_raw, upasv2_header)
#'
#' # UPASv2x EXAMPLES
#' upasv2x_filename <- 'PSP00270_LOG_2024-07-11T18_01_22UTC_PM_CO2_Map______----------.txt'
#' upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
#' upasv2x_log_raw <- fread_ast_log(upasv2x_file)
#' upasv2x_header <- read_ast_header(upasv2x_file, update_names=FALSE)
#' upasv2x_log <- format_ast_log(upasv2x_log_raw, upasv2x_header, update_names=FALSE)
#'
#' # Diagnostic file
#' upasv2x_diag_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
#' upasv2x_diag_file <- system.file("extdata", upasv2x_diag_filename, package = "astr",
#'                                   mustWork = TRUE)
#' upasv2x_diag_log_raw <- fread_ast_log(upasv2x_diag_file)
#' upasv2x_diag_header <- read_ast_header(upasv2x_diag_file, update_names=FALSE)
#' upasv2x_diag_log <- format_ast_log(upasv2x_diag_log_raw, upasv2x_diag_header, update_names=FALSE)
#'
#' # HHB EXAMPLES
#' hhb_filename <- 'HHB00032_LOG_2024-07-01T18_20UTC.csv'
#' hhb_file <- system.file("extdata", hhb_filename, package = "astr", mustWork = TRUE)
#' hhb_log_raw <- fread_ast_log(hhb_file)
#' hhb_header <- read_ast_header(hhb_file)
#' hhb_log <- format_ast_log(hhb_log_raw, hhb_header)

format_ast_log = function(log, header, update_names=FALSE, tz=NA, cols_keep=c(), cols_drop=c()) {

  firmware <- header$Firmware

  if(grepl("UPAS_v2_x", firmware) | grepl("SHEARv2_7_2", firmware)){

    df <- astr::format_upasv2x_log(log, header, update_names, cols_keep, cols_drop)

  }else if(grepl("UPAS_v2_0", firmware)){

    df <- astr::format_upasv2_log(log, header, update_names, cols_keep, cols_drop)

  }else if(grepl("HHBv2", firmware)){

    df <- astr::format_hhb_log(log, header, tz, cols_keep, cols_drop)

  }

  return(df)
}
