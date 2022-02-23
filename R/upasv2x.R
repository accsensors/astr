#'Read the header data from an Access Sensor Technologies (AST) air sampler
#'log file
#'
#' @param df_h Pass a upasv2x dataframe from read_ast_header function.
#'
#' @return A modified data frame with header data in wide format.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2x_header <- format_upasv2x_header(upasv2x_header_raw)

format_upasv2x_header = function(df_h) {

  df_h <- df_h %>%
    dplyr::mutate(ProgrammedRuntime = ifelse(.data$ProgrammedRuntime == "indefinite",
                                             NA,.data$ProgrammedRuntime)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("LifetimeSampleCount",
                                                "LifetimeSampleRuntime",
                                                "GPSUTCOffset",
                                                "ProgrammedStartTime",
                                                "ProgrammedRuntime",
                                                "FlowRateSetpoint",
                                                "FlowOffset",
                                                "FlowCheckMeterReadingPreSample",
                                                "FlowCheckMeterReadingPostSample",
                                                "FlowDutyCycle",
                                                "DutyCycleWindow",
                                                "LogInterval",
                                                "OverallDuration",
                                                "PumpingDuration",
                                                "OverallFlowRateAverage",
                                                "PumpingFlowRateAverage",
                                                "SampledVolume",
                                                "StartBatteryCharge",
                                                "EndBatteryCharge",
                                                "StartBatteryVoltage",
                                                "EndBatteryVoltage",
                                                "ShutdownMode",
                                                "MFSCalVoutBlocked",
                                                "MFSCalVoutMin",
                                                "MFSCalVoutMax",
                                                "MFSCalMFBlocked",
                                                "MFSCalMFMin",
                                                "MFSCalMFMax",
                                                "MFSCalPumpVBoostMin",
                                                "MFSCalPumpVBoostMax",
                                                "MFSCalPDeadhead",
                                                "MF4",
                                                "MF3",
                                                "MF2",
                                                "MF1",
                                                "MF0")), as.numeric)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("StartOnNextPowerUp",
                                                "GPSEnabled",
                                                "RTGasSampleState",
                                                "PowerSaveMode",
                                                "AppLock")), as.logical)) %>%
    dplyr::mutate(LogFilename = gsub("/sd/", "", .data$LogFilename),
                  ShutdownReason  = dplyr::case_when(
                    .data$ShutdownMode == 0 ~ "unknown error",
                    .data$ShutdownMode == 1 ~ "user pushbutton stop",
                    .data$ShutdownMode == 2 ~ "depleted battery",
                    .data$ShutdownMode == 3 ~ "completed preset sample duration",
                    .data$ShutdownMode == 4 ~ "thermal protection",
                    .data$ShutdownMode == 5 ~ "max power at initialization",
                    .data$ShutdownMode == 6 ~ "max power during sample",
                    .data$ShutdownMode == 7 ~ "blocked flow",
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
                    TRUE ~ "NA" ))

  df_h  <- df_h %>%
    dplyr::select(.data$ast_sampler,match("UPASserial",colnames(df_h)):ncol(df_h))

  df_h  <- df_h %>%
    dplyr::select(1:match("UPASfirmware",colnames(df_h)), .data$firmware_rev,
                  (match("UPASfirmware",colnames(df_h))+1):ncol(df_h))

  df_h  <- df_h %>%
    dplyr::select(1:match("ShutdownMode",colnames(df_h)), .data$ShutdownReason,
                  (match("ShutdownMode",colnames(df_h))+1):ncol(df_h))
  df_h  <- df_h %>%
    dplyr::select(1:match("PMSensorInterval",colnames(df_h)), .data$PMSensorOperation,
                  (match("PMSensorInterval",colnames(df_h))+1):ncol(df_h)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("StartDateTimeUTC",
                                                "EndDateTimeUTC",
                                                "MFSCalDate")),
                                as.POSIXct, format="%Y-%m-%dT%H:%M:%S",
                                tz="UTC")) %>%
    dplyr::mutate(SampleName  = gsub("_+$", "", .data$SampleName),
                  SampleName  = ifelse(.data$SampleName != "", .data$SampleName, NA),
                  CartridgeID = gsub("_+$", "", .data$CartridgeID),
                  CartridgeID = gsub("-+$", "", .data$CartridgeID),
                  CartridgeID = ifelse(.data$CartridgeID != "", .data$CartridgeID, NA))
  # %>%
  #   dplyr::rename(`Sample Duration (hr)` = .data$OverallDuration)

  return(df_h)

}

#'Read the log data from an Access Sensor Technologies (AST) air sampler
#'log file
#'
#' @param df Pass a upasv2x dataframe from read_ast_header function.
#'
#' @return A modified data frame with all log data.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2x_log <- format_upasv2x_log(upasv2x_log_raw)

format_upasv2x_log = function(df) {


  # # header <- read_csv(file, col_names=TRUE, skip=df_h$n_header_rows,  n_max=1)
  # # df   <- read_csv(file, col_names=FALSE, skip=nskip+2)
  # colnames(df) <- colnames(header)
  #
  # if(nrow(df) > 0){
  #
  #   df <- cbind(df_h, df) %>%
  #     dplyr::mutate(SampleTime=as.character(SampleTime),
  #                   SampleTime = ifelse(SampleTime == "99:99:99", NA, SampleTime),
  #                   SampleTime = ifelse(!is.na(SampleTime), strsplit(SampleTime,":"), SampleTime),
  #                   SampleTime = as.difftime(3600*as.numeric(sapply(SampleTime, `[`, 1)) +
  #                                              60*as.numeric(sapply(SampleTime, `[`, 2)) +
  #                                              as.numeric(sapply(SampleTime, `[`, 3)), units="secs"))
  #
  #
  #   df <- df %>% dplyr::mutate(DateTimeUTC = as.POSIXct(DateTimeUTC, format="%Y-%m-%dT%H:%M:%S", tz="UTC")) %>%
  #     dplyr::mutate(across(c(UnixTime:UnixTimeMCU, PumpingFlowRate:gasPercAcc), ~ as.numeric(.x)))
  #
  # }

  return(df)

}
