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
    dplyr::mutate(ASTSampler = sub("-rev.*", "", .data$Firmware),
                  FirmwareRev = sapply(strsplit(.data$Firmware,"-"), `[`, 2),
                  FirmwareRev = as.numeric(gsub("rev_", "", .data$FirmwareRev)))
  df_h <- df_h %>%
    dplyr::mutate(ProgrammedRuntime = ifelse(.data$ProgrammedRuntime == "indefinite",
                                             NA,.data$ProgrammedRuntime)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("UPASserial",
                                                "LifetimeSampleCount",
                                                "LifetimeSampleRuntime",
                                                "GPSUTCOffset",
                                                "StartOnNextPowerUp",
                                                "ProgrammedStartTime",
                                                "ProgrammedRuntime",
                                                "FlowRateSetpoint",
                                                "FlowOffset",
                                                "FlowCheckMeterReadingPreSample",
                                                "FlowCheckMeterReadingPostSample",
                                                "FlowDutyCycle",
                                                "DutyCycleWindow",
                                                "GPSEnabled",
                                                "PMSensorInterval",
                                                "RTGasSampleState",
                                                "LogInterval",
                                                "PowerSaveMode",
                                                "AppLock",
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
    dplyr::mutate(dplyr::across(dplyr::any_of(c("GPSEnabled",
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
    dplyr::select(.data$ASTSampler,match("UPASserial",colnames(df_h)):ncol(df_h))

  df_h  <- df_h %>%
    dplyr::select(1:match("Firmware",colnames(df_h)), .data$FirmwareRev,
                  (match("Firmware",colnames(df_h))+1):ncol(df_h))

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
                  SampleName  = gsub("-+$", "", .data$SampleName),
                  SampleName  = ifelse(.data$SampleName != "", .data$SampleName, NA),
                  CartridgeID = gsub("_+$", "", .data$CartridgeID),
                  CartridgeID  = gsub("-+$", "", .data$CartridgeID),
                  CartridgeID = ifelse(.data$CartridgeID != "", .data$CartridgeID, NA))
  # %>%
  #   dplyr::rename(`Sample Duration (hr)` = .data$OverallDuration)

  return(df_h)

}

#'Read the log data from an Access Sensor Technologies (AST) air sampler
#'log file
#'
#' @param df_h Pass a upasv2x header dataframe from read_ast_header function.
#' @param df Pass a upasv2x dataframe from read_ast_header function.
#' @param tz_offset Pass an optional timezone offset value.
#' @param cols_keep Specify log file columns to keep.
#' @param cols_drop Specify log file columns to remove.
#'
#' @return A modified data frame with all log data.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2x_log <- format_upasv2x_log(upasv2x_header, upasv2x_log_raw)

format_upasv2x_log = function(df_h, df, tz_offset = NA, cols_keep = c(), cols_drop = c()) {

  df_h_sel <- df_h %>% dplyr::select(dplyr::any_of(c("ASTSampler",
                                                     "UPASserial",
                                                     "LogFilename",
                                                      "SampleName",
                                                      "CartridgeID",
                                                      "VolumetricFlowRateSet",
                                                      "StartDateTimeUTC",
                                                      "GPSUTCOffset")))

  tz_off <- ifelse(is.na(tz_offset),df_h_sel$GPSUTCOffset, tz_offset)


  df_h_sel <- df_h_sel %>%
    dplyr::select(-.data$GPSUTCOffset)

  df[df == 'NULL'] <- NA

  df <- df %>%
    dplyr::mutate(dplyr::across(-dplyr::one_of(c("SampleTime","DateTimeUTC",
                                                 "DateTimeLocal")),
                                as.numeric)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("PumpsON",
                                                "Dead",
                                                "BCS1",
                                                "BCS2",
                                                "BC_NPG")),
                                as.logical)) %>%
    dplyr::mutate(SampleTime = ifelse(.data$SampleTime == "99:99:99",
                                      NA,
                                      .data$SampleTime),
                  SampleTime = ifelse(!is.na(.data$SampleTime),
                                      strsplit(.data$SampleTime,":"),
                                      .data$SampleTime),
                  SampleTime = as.difftime(
                    3600*as.numeric(sapply(.data$SampleTime, `[`, 1)) +
                    60*as.numeric(sapply(.data$SampleTime, `[`, 2)) +
                    as.numeric(sapply(.data$SampleTime, `[`, 3)), units="secs"),
                  DateTimeUTC = as.POSIXct(.data$DateTimeUTC,
                                           format="%Y-%m-%dT%H:%M:%S",
                                           tz="UTC"),
                  tz_value = ifelse(is.na(tz_offset),TRUE,FALSE),
                  DateTimeLocal = dplyr::if_else(.data$tz_value,
                                         as.POSIXct(.data$DateTimeLocal,
                                                    format="%Y-%m-%dT%H:%M:%S",
                                                    tz='UTC'),
                                                    .data$DateTimeUTC+tz_off*3600),
                  TZOffset = tz_off,
                  GPSlat   = ifelse(.data$GPSlat   == -9999, NA, .data$GPSlat),
                  GPSlon   = ifelse(.data$GPSlon   == -9999, NA, .data$GPSlon),
                  GPSalt   = ifelse(.data$GPSalt   == -9999, NA, .data$GPSalt),
                  GPSspeed = ifelse(.data$GPSspeed == -9999, NA, .data$GPSspeed),
                  GPShDOP  = ifelse(.data$GPShDOP  == -9999, NA, .data$GPShDOP))




  df <- df %>%
    dplyr::select(1:match("DateTimeLocal",colnames(df)), .data$TZOffset,
                                  (match("DateTimeLocal",colnames(df))+1):ncol(df)) %>%
    cbind(df_h_sel)

  if(!is.null(cols_keep)){
    df <- df %>%
      dplyr::select(cols_keep)
  }else if(!is.null(cols_drop)){
    df <- df %>%
      dplyr::select(-cols_drop)
  }

  return(df)

}


#'Create sample summary dataframe from an Access Sensor Technologies (AST)
#'air sampler header dataframe
#'
#' @param df_h Pass a upasv2x header dataframe from read_ast_header function.
#' @param df Pass a upasv2x dataframe from read_ast_header function.
#' @param shiny Option to make TRUE if using function with AST shiny app.
#' @param fract_units Boolean to specify if units should be fractional (L min^-1 vs L/min).loa
#'
#' @return A modified data frame with only the sample summary data.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2x_sample_summary <- upasv2x_sample_summary(upasv2x_header, upasv2x_log)
#' upasv2x_sample_summary <- upasv2x_sample_summary(upasv2x_header)

upasv2x_sample_summary = function(df_h, df = NULL, shiny=FALSE, fract_units=FALSE) {
  #TODO move to new function shiny_sample_summary so that shiny functionality is not present in normal functions
  df_h <- astr::shiny_flag(df_h)

  df_h <- df_h %>%
    dplyr::select(dplyr::any_of(c('ASTSampler', 'UPASserial','SampleName','CartridgeID',
                                  'SampleSuccess',
                                  'SampledRuntime', 'OverallDuration', 'PumpingDuration',
                                'PumpingFlowRateAverage', 'OverallFlowRateAverage',
                                'PM25SampledMass',
                                'SampledVolume','ShutdownReason'
                                ))) %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::any_of(c('SampledRuntime',
                                                        'AverageVolumetricFlowRate',
                                   'PM25SampledMass',
                                   'SampledVolume')), .fns = as.numeric),
                  dplyr::across(.cols = dplyr::any_of(c('ASTSampler', 'UPASserial')), .fns = as.factor))

    if(!is.null(df)){
      if(any(grepl('PM2_5SampledMass', names(df)))){
        df_h$PM2_5SampledMass <- max(df$PM2_5SampledMass)
      }
    }

    if(any(grepl('PM25SampledMass', names(df_h)))){
      df_h <- df_h %>%
      dplyr::mutate(PM25Concentration = .data$PM25SampledMass/(.data$SampledVolume/1000),
                    dplyr::across(dplyr::where(is.numeric), ~ round(., digits = 3)))
    }

  if(shiny){
    df_h <- astr::shiny_header(df_h, fract_units=fract_units)
  }

  return(df_h)
}

#'Create sample settings dataframe from an Access Sensor Technologies (AST)
#'air sampler header dataframe
#'
#' @param df_h Pass a upasv2x header dataframe from read_ast_header function.
#' @param shiny Option to make TRUE if using function with AST shiny app.
#' @param fract_units Boolean to specify if units should be fractional (L min^-1 vs L/min).
#'
#' @return A modified data frame with only the sample settings data.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2x_sample_settings <- upasv2x_sample_settings(upasv2x_header)

upasv2x_sample_settings = function(df_h, shiny=FALSE, fract_units=FALSE) {
  #TODO move to new function shiny_sample_settings so that shiny functionality is not present in normal functions
  df_h <- astr::shiny_flag(df_h)

  df_h <- df_h %>%
    dplyr::select(dplyr::any_of(c('ASTSampler', 'UPASserial', 'SampleName', 'CartridgeID',
                                  'StartOnNextPowerUp','ProgrammedStartDelay',
                                  'ProgrammedStartTime','ProgrammedRuntime',
                                  'SizeSelectiveInlet','VolumetricFlowRateSet',
                                  'FlowRateSetpoint','DutyCycle',
                                  'FlowDutyCycle','GPSEnabled',
                                  'PMSensorOperation','RTGasSampleState',
                                  'LogInterval',
                                  'PowerSaveMode','AppVersion',
                                  'SampleSuccess'
                                  ))) %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::any_of(c('ASTSampler', 'UPASserial')), .fns = as.factor))

  if(shiny){
    df_h <- astr::shiny_header(df_h, fract_units=fract_units)
  }

  return(df_h)
}

#'Create sample metadata dataframe from an Access Sensor Technologies (AST)
#'air sampler header dataframe
#'
#' @param df_h Pass a upasv2x header dataframe from read_ast_header function.
#' @param shiny Option to make TRUE if using function with AST shiny app.
#' @param fract_units Boolean to specify if units should be fractional (L min^-1 vs L/min).
#'
#' @return A modified data frame with only the sample settings data.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2x_sample_meta <- upasv2x_sample_meta(upasv2x_header)

upasv2x_sample_meta = function(df_h, shiny=FALSE, fract_units=FALSE) {
  #TODO move to new function shiny_sample_meta so that shiny functionality is not present in normal functions
  df_h <- astr::shiny_flag(df_h)

  df_h <- df_h %>%
    dplyr::select(dplyr::any_of(c('ASTSampler', 'UPASserial','PMSerial','SampleName',
                                'CartridgeID','StartDateTimeUTC',
                  'EndDateTimeUTC','StartBatteryVoltage','EndBatteryVoltage',
                  'StartBatteryCharge','EndBatteryCharge',
                  'GPSUTCOffset','FirmwareRev','ShutdownMode',
                  'SampleSuccess'))) %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::any_of(c('ASTSampler', 'UPASserial')), .fns = as.factor))

  if(shiny){
    df_h <- astr::shiny_header(df_h, fract_units=fract_units)
    }


  return(df_h)
}
