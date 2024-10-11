#' Format UPAS v2.1 PLUS header data that have already been transposed to a wide data frame
#'
#' @description
#' `format_upasv2x_header()` formats the UPAS v2.1 PLUS log file header data prior
#' to analysis. This function sets the proper data types for each variable, adds
#' a column to specify the sampler type, and adds a column to describe the
#' shutdown reason associated with the shutdown mode code.
#'
#' @param data A UPASv2.1 PLUS header data frame returned by the [astr::transpose_ast_header()] function
#' @inheritParams read_ast_header
#' @return A data frame with a single row of UPAS v2.1 PLUS header data that are formatted and ready for analysis.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2x_filename <- 'PSP00270_LOG_2024-07-11T18_01_22UTC_PM_CO2_Map______----------.txt'
#' upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
#' upasv2x_header_list <- fread_ast_header(upasv2x_file)
#' upasv2x_header_wide <- transpose_ast_header(upasv2x_header_list$header)
#' upasv2x_header <- format_upasv2x_header(upasv2x_header_wide)
#'
#' # Diagnostic file
#' upasv2x_diag_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
#' upasv2x_diag_file <- system.file("extdata", upasv2x_diag_filename, package = "astr",
#'                                   mustWork = TRUE)
#' upasv2x_diag_header_list <- fread_ast_header(upasv2x_diag_file)
#' upasv2x_diag_header_wide <- transpose_ast_header(upasv2x_diag_header_list$header,
#'                                   upasv2x_diag_header_list$diag)
#' upasv2x_diag_header <- format_upasv2x_header(upasv2x_diag_header_wide)

format_upasv2x_header = function(data, tz=NA) {

  data <- dplyr::mutate(data,
    ASTSampler  = sub("-rev.*", "", .data$Firmware),
    UPASserial = ifelse(.data$ASTSampler == "UPAS_v2_x", ## Change UPASserial based off standard vs. SHEAR UPAS
      sub("(^.*)(PSP.*)_LOG.*", "\\2", .data$LogFilename),
      sub("(^.*)(SH.*)_LOG.*", "\\2", .data$LogFilename)),
    FirmwareRev = sapply(strsplit(.data$Firmware,"-"), `[`, 2),
    FirmwareRev = as.numeric(gsub("rev_", "", .data$FirmwareRev)),
    ProgrammedRuntime = ifelse(.data$ProgrammedRuntime == "indefinite", NA,
                               .data$ProgrammedRuntime),
    StartOnNextPowerUp = dplyr::case_when(
                    .data$StartOnNextPowerUp == 0 ~ "FALSE",
                    .data$StartOnNextPowerUp == 1 ~ "TRUE",
                    .data$StartOnNextPowerUp == 2 ~ "system reset",
                    .data$StartOnNextPowerUp == 4 ~ "always"
    ),
    dplyr::across(dplyr::any_of(c("UPASpcbRev", "GPSUTCOffset",
                                  "DutyCycleWindow",
                                  "GPSEnabled", "PMSensorInterval",
                                  "LogInterval", "SamplerConfiguration",
                                  "PowerSaveMode", "AppLock", "SampledVolume",
                                  "PercentTimeWorn", "ShutdownMode",
                                  "CO2CalTarget", "CO2CalOffset",
                                  "MFSCalPDeadhead",
                                  "MF4", "MF3", "MF2", "MF1", "MF0")),
                  \(x) as.numeric(x)),
    dplyr::across(dplyr::starts_with("Lifetime"),      \(x) as.numeric(x)),
    dplyr::across(dplyr::starts_with("Programmed"),    \(x) as.numeric(x)),
    dplyr::across(dplyr::starts_with("Flow"),          \(x) as.numeric(x)),
    dplyr::across(dplyr::ends_with("SampleState"),     \(x) as.numeric(x)),
    dplyr::across(dplyr::ends_with("Duration"),        \(x) as.numeric(x)),
    dplyr::across(dplyr::ends_with("FlowRateAverage"), \(x) as.numeric(x)),
    dplyr::across(dplyr::contains("Battery"),          \(x) as.numeric(x)),
    dplyr::across(dplyr::starts_with("MFSCalVout"),    \(x) as.numeric(x)),
    dplyr::across(dplyr::starts_with("MFSCalMF"),      \(x) as.numeric(x)),
    dplyr::across(dplyr::starts_with("MFSCalPump"),    \(x) as.numeric(x)),
    dplyr::across(dplyr::any_of(c("GPSEnabled", "PowerSaveMode", "AppLock")),
                  \(x) as.logical(x)),
    dplyr::across(dplyr::ends_with("SampleState"), \(x) as.logical(x)),
    dplyr::across(dplyr::any_of(c("ExternalPowerMode")),
                  \(x) ifelse(x == "F0", T, F)),
    LogFilename = gsub("/sd/", "", .data$LogFilename),
    ShutdownReason = dplyr::case_when(
                   .data$ShutdownMode == 0 ~ "unknown error",
                   .data$ShutdownMode == 1 ~ "user pushbutton stop",
                   .data$ShutdownMode == 2 ~ "depleted battery",
                   .data$ShutdownMode == 3 ~ "completed preset sample duration",
                   .data$ShutdownMode == 4 ~ "thermal protection",
                   .data$ShutdownMode == 5 ~ "max power at initialization",
                   .data$ShutdownMode == 6 ~ "max power during sample",
                   .data$ShutdownMode == 7 ~ "blocked flow",
                   #TODO may need to make botton 2 only applicable below rev200
                   .data$ShutdownMode == 8 ~ "SD card removed",
                   dplyr::between(.data$ShutdownMode, 64, 79) ~ "code freeze",
                   TRUE ~ "RTOS crash"),
    PMSensorOperation = dplyr::case_when(
       .data$PMSensorInterval == "0" ~ "Sensor Disabled",
       .data$PMSensorInterval == "1" ~ "Continuous Measurement",
       .data$PMSensorInterval == "2" ~ "30s Warmup 30s Measurement 60s Sleep",
       .data$PMSensorInterval == "3" ~ "30s Warmup 30s Measurement 120s Sleep",
       .data$PMSensorInterval == "4" ~ "30s Warmup 30s Measurement 180s Sleep",
       .data$PMSensorInterval == "5" ~ "30s Warmup 30s Measurement 240s Sleep",
       .data$PMSensorInterval == "6" ~ "30s Warmup 30s Measurement 300s Sleep",
       .data$PMSensorInterval == "7" ~ "30s Warmup 30s Measurement 360s Sleep",
       .data$PMSensorInterval == "8" ~ "30s Warmup 30s Measurement 420s Sleep",
       .data$PMSensorInterval == "9" ~ "30s Warmup 30s Measurement 480s Sleep",
       .data$PMSensorInterval == "10" ~ "30s Warmup 30s Measurement 520s Sleep",
       .data$PMSensorInterval == "11" ~ "30s Warmup 30s Measurement 580s Sleep",
       .data$PMSensorInterval == "12" ~ "30s Warmup 30s Measurement 640s Sleep",
       .data$PMSensorInterval == "13" ~ "30s Warmup 30s Measurement 700s Sleep",
       .data$PMSensorInterval == "14" ~ "30s Warmup 30s Measurement 760s Sleep",
       .data$PMSensorInterval == "15" ~ "30s Warmup 30s Measurement 820s Sleep",
       .data$PMSensorInterval == "16" ~ "15s Warmup 5s Measurement 10s Sleep",
       .data$PMSensorInterval == "17" ~ "15s Warmup 5s Measurement 40s Sleep",
       .data$PMSensorInterval == "18" ~ "20s Warmup 10s Measurement 30s Sleep",
       TRUE ~ "NA" ),
    dplyr::across(dplyr::any_of(c("StartDateTimeUTC","EndDateTimeUTC",
                                  "CO2CalDate", "MFSCalDate")),
                  \(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")),
    SampleName  = gsub("_+$", "", .data$SampleName),
    SampleName  = gsub("-+$", "", .data$SampleName),
    SampleName  = ifelse(.data$SampleName != "", .data$SampleName, NA),
    CartridgeID = gsub("_+$", "", .data$CartridgeID),
    CartridgeID = gsub("-+$", "", .data$CartridgeID),
    CartridgeID = ifelse(.data$CartridgeID != "", .data$CartridgeID, NA))

  tz_string <- astr::get_tz_string(data$GPSUTCOffset, tz=tz)

  if(!is.na(tz_string)){
    data <- dplyr::mutate(data,
                          StartDateTimeLocal =
                            lubridate::with_tz(.data$StartDateTimeUTC,
                                               tzone = tz_string),
                          EndDateTimeLocal =
                            lubridate::with_tz(.data$EndDateTimeUTC,
                                               tzone = tz_string))
  }

  data <- dplyr::relocate(data, "ASTSampler")
  data <- dplyr::relocate(data, "FirmwareRev",       .after = "Firmware")
  data <- dplyr::relocate(data, "ShutdownReason",    .after = "ShutdownMode")
  data <- dplyr::relocate(data, "PMSensorOperation", .after = "PMSensorInterval")

  return(data)
}

#'Format the sample log data from an Access Sensor Technologies UPAS v2.1 PLUS
#'
#'#' @description
#' `format_upasv2x_log()` Applies device-specific formatting to the columns of a
#' sample log data frame returned by the [astr::fread_ast_log()] function. It sets the
#' proper data type for each variable and adds columns that aid in identifying
#' unique log files when data from multiple sample logs have been combined into
#' a single data frame.
#'
#' @param log A data frame of UPAS v2.1 PLUS sample log data returned by the [astr::fread_ast_log()] function.
#' @param header A data frame of UPAS v2.1 PLUS header data returned by the [astr::read_ast_header()] function.
#' @inheritParams format_ast_log
#'
#' @return A data frame of of UPAS v2.1 PLUS sample log data that are formatted and ready for analysis.
#' This data frame will contain one row for each timestamp in the sample log.
#'
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2x_filename <- 'PSP00270_LOG_2024-07-11T18_01_22UTC_PM_CO2_Map______----------.txt'
#' upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
#' upasv2x_log_raw <- fread_ast_log(upasv2x_file)
#' upasv2x_header <- read_ast_header(upasv2x_file, update_names=FALSE)
#' upasv2x_log <- format_upasv2x_log(upasv2x_log_raw, upasv2x_header, update_names=FALSE)
#'
#' # Use of cols_drop, cols_keep, and tz  with a UPASv2x log file
#' upasv2x_log_colsdrop <- format_upasv2x_log(upasv2x_log_raw, upasv2x_header,
#'                 cols_drop = c("DateTimeLocal", "AtmoT", "AtmoP", "AtmoRH"))
#' upasv2x_log_colskeep <- format_upasv2x_log(upasv2x_log_raw, upasv2x_header, tz="America/New_York",
#'                 cols_keep = c("SampleTime", "DateTimeUTC", "DateTimeLocal",
#'                                "LocalTZ",  "UserTZ", "AtmoT", "AtmoP", "AtmoRH"))
#'
#' # Diagnostic file
#' upasv2x_diag_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
#' upasv2x_diag_file <- system.file("extdata", upasv2x_diag_filename, package = "astr",
#'                                   mustWork = TRUE)
#' upasv2x_diag_log_raw <- fread_ast_log(upasv2x_diag_file)
#' upasv2x_diag_header <- read_ast_header(upasv2x_diag_file, update_names=FALSE)
#' upasv2x_diag_log <- format_upasv2x_log(upasv2x_diag_log_raw, upasv2x_diag_header,
#'                                   update_names=FALSE)

format_upasv2x_log = function(log, header, update_names=FALSE, tz=NA, cols_keep=c(), cols_drop=c()) {

  df_h <- dplyr::select(header, dplyr::any_of(c("ASTSampler","UPASserial",
                                                "UPASlogFilename","LogFilename",
                                                "SampleName","CartridgeID",
                                                "StartDateTimeUTC")))

  df <- dplyr::mutate(log,
    dplyr::across(-dplyr::any_of(c("SampleTime","DateTimeUTC","DateTimeLocal")),
                  \(x) as.numeric(x)),
    dplyr::across(dplyr::any_of(c("PumpsON","Dead","BCS1","BCS2","BC_NPG")),
                  \(x) as.logical(x)),
    SampleTime = ifelse(.data$SampleTime == "99:99:99", NA, .data$SampleTime),
    SampleTime = ifelse(!is.na(.data$SampleTime),strsplit(.data$SampleTime,":"),
                        .data$SampleTime),
    SampleTime = as.difftime(
                    3600*as.numeric(sapply(.data$SampleTime, `[`, 1)) +
                      60*as.numeric(sapply(.data$SampleTime, `[`, 2)) +
                         as.numeric(sapply(.data$SampleTime, `[`, 3)),
                    units="secs"),
    DateTimeUTC = as.POSIXct(.data$DateTimeUTC, format = "%Y-%m-%dT%H:%M:%S",
                             tz = "UTC"),
    UserTZ   = ifelse(!is.na(tz), T, F),
    LocalTZ  = astr::get_tz_string(header$GPSUTCOffset, tz=tz),
    GPSlat   = ifelse(.data$GPSlat   == -9999, NA, .data$GPSlat),
    GPSlon   = ifelse(.data$GPSlon   == -9999, NA, .data$GPSlon),
    GPSalt   = ifelse(.data$GPSalt   == -9999, NA, .data$GPSalt),
    GPSspeed = ifelse(.data$GPSspeed == -9999, NA, .data$GPSspeed),
    GPShDOP  = ifelse(.data$GPShDOP  == -9999, NA, .data$GPShDOP)) %>%
    dplyr::select(-dplyr::starts_with("V1")) # Remove any unnamed columns from firmwares with extra commas in the log

  if(!is.na(unique(df$LocalTZ))){
    df <- dplyr::mutate(df, DateTimeLocal = lubridate::with_tz(.data$DateTimeUTC,
                                                      tzone=unique(df$LocalTZ)))
  }

  df <- dplyr::relocate(df, c("DateTimeLocal","LocalTZ"), .after="DateTimeUTC")

  df <- cbind(df, df_h)

  if(update_names){
    df <- dplyr::rename(df, dplyr::any_of(c(AccelComplianceHrs = "AceelComplianceHrs")))
  }

  df <- df %>%
    dplyr::relocate(dplyr::any_of(c("ASTSampler", "UPASserial", "SampleName", "CartridgeID")))

  if(!is.null(cols_keep)){
    df <- dplyr::select(df, dplyr::all_of(cols_keep))
  }else if(!is.null(cols_drop)){
    df <- dplyr::select(df, -dplyr::all_of(cols_drop))
  }

  return(df)
}
