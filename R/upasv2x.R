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
#' @param tz_offset Pass an optional timezone offset value
#'
#' @return A modified data frame with all log data.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2x_log <- format_upasv2x_log(upasv2x_header, upasv2x_log_raw)

format_upasv2x_log = function(df_h, df, tz_offset = NA, cols_keep = c(), cols_drop = c(), units=FALSE) {

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
                  TZOffset = tz_off)




  df <- df %>%
    dplyr::select(1:match("DateTimeLocal",colnames(df)), .data$TZOffset,
                                  (match("DateTimeLocal",colnames(df))+1):ncol(df)) %>%
    cbind(df_h_sel)

  if(units) {
    # units <- c("(HH:MM:SS)","(s)","(s)","(YYYY-MM-DDTHH:MM:SS)","(YYYY-MM-DDTHH:MM:SS)", "",
    #            "(L/min^-1)","(L*min^-1)","(L)",
    #            "(Pa)","(%)","(C)","(hPa)","(%RH)",
    #            "(g*L^-1)","(m ASL)","(-)","(decimalDegree)","(decimalDegree)",
    #            "(m)","(integer)","(m*s^-1)","(-)",
    #            "(mg)","(mg)","(mg)","(mg)","(mg)","(mg)","(mg)","(mg)","(mg)","(mg)","(mg)","(mg)",
    #            "(mdeg*s^-1)","(mdeg*s^-1)","(mdeg*s^-1)","(mdeg*s^-1)","(mdeg*s^-1)","(mdeg*s^-1)",
    #            "(mdeg*s^-1)","(mdeg*s^-1)","(mdeg*s^-1)","(mdeg*s^-1)","(mdeg*s^-1)","(mdeg*s^-1)",
    #            "(%)","(%)","(%)","(%)","(%)","(%)","(#)","(lux)","(-)","(-)","(-)","(-)","(-)","(#)",
    #            "(ug*m^-3)","(ug*m^-3)","(ug*m^-3)","(ug*m^-3)","(ug*m^-3)","(ug*m^-3)","(ug*m^-3)","(ug*m^-3)",
    #            "(#*cm^-3)","(#*cm^-3)","(#*cm^-3)","(#*cm^-3)","(#*cm^-3)","(#*cm^-3)","(#*cm^-3)","(#*cm^-3)",
    #            "(#*cm^-3)","(#*cm^-3)",
    #            "(um)","(um)","(ug)","(C)","(C)","(C)","(C)","(ohm)","(hPa)",
    #            "(integer)","(integer)","(V)","(g*min^-1)","(V)","(integer)","(V)","(V)","(V)",
    #            "(bool)","(bool)","(bool)","(bool)","(bool)",
    #            "(s)","(s)","(s)","(s)","(s)","(s)","(ppm)","(C)","(%)","(-)","(-)")
    df <- df %>%
      dplyr::rename(`SampleTime(HH:MM:SS)` = .data$SampleTime,
                                  `UnixTime(s)` = .data$UnixTime,
                                  `UnixTimeMCU(s)` = .data$UnixTimeMCU,
                                  `DateTimeUTC(YYYY-MM-DDTHH:MM:SS)` = .data$DateTimeUTC,
                                  `DateTimeLocal(YYYY-MM-DDTHH:MM:SS)` = .data$DateTimeLocal,
                                  `TZOffset(hrs)` = .data$TZOffset,
                                  `PumpingFlowRate(LPM)` = .data$PumpingFlowRate,
                                  `OverallFlowRate(LPM)` = .data$OverallFlowRate,
                                  `SampledVolume(L)` = .data$SampledVolume,
                                  `FilterDP(Pa)` = .data$FilterDP,
                                  `BatteryCharge(%)` = .data$BatteryCharge,
                                  `AtmoT(C)` = .data$AtmoT,
                                  `AtmoP(hPa)` = .data$AtmoP,
                                  `AtmoRH(%RH)` = .data$AtmoRH,
                                  `AtmoDensity(g/L)` = .data$AtmoDensity,
                                  `AtmoAlt(m ASL)` = .data$AtmoAlt,
                                  `GPSQual` = .data$GPSQual,
                                  `GPSlat(decimalDegrees)` = .data$GPSlat,
                                  `GPSlon(decimalDegrees)` = .data$GPSlon,
                                  `GPSalt(m)` = .data$GPSalt,
                                  `GPSsat(integer)`= .data$GPSsat,
                                  `GPSspeed(m/s)` = .data$GPSspeed,
                                  `GPShDOP` = .data$GPShDOP,
                                  `AccelX(mg)` = .data$AccelX,
                                  `AccelXVar(mg)` = .data$AccelXVar,
                                  `AccelXMin(mg)` = .data$AccelXMin,
                                  `AccelXMax(mg)` = .data$AccelXMax,
                                  `AccelY(mg)` = .data$AccelY,
                                  `AccelYVar(mg)` = .data$AccelYVar,
                                  `AccelYMin(mg)` = .data$AccelYMin,
                                  `AccelYMax(mg)` = .data$AccelYMax,
                                  `AccelZ(mg)` = .data$AccelZ,
                                  `AccelZVar(mg)` = .data$AccelZVar,
                                  `AccelZMin(mg)` = .data$AccelZMin,
                                  `AccelZMax(mg)` = .data$AccelZMax,
                                  `RotX(mdeg/s)` = .data$RotX,
                                  `RotXVar(mdeg/s)` = .data$RotXVar,
                                  `RotXMin(mdeg/s)` = .data$RotXMin,
                                  `RotXMax(mdeg/s)` = .data$RotXMax,
                                  `RotY(mdeg/s)` = .data$RotY,
                                  `RotYVar(mdeg/s)` = .data$RotYVar,
                                  `RotYMin(mdeg/s)` = .data$RotYMin,
                                  `RotYMax(mdeg/s)` = .data$RotYMax,
                                  `RotZ(mdeg/s)` = .data$RotZ,
                                  `RotZVar(mdeg/s)` = .data$RotZVar,
                                  `RotZMin(mdeg/s)` = .data$RotZMin,
                                  `RotZMax(mdeg/s)` = .data$RotZMax,
                                  `Xup(%)` = .data$Xup,
                                  `XDown(%)` = .data$XDown,
                                  `Yup(%)` = .data$Yup,
                                  `Ydown(%)` = .data$Ydown,
                                  `Zup(%)` = .data$Zup,
                                  `Zdown(%)` = .data$Zdown,
                                  `StepCount(#)` = .data$StepCount,
                                  `LUX(lux)` = .data$LUX,
                                  `UVindex` = .data$UVindex,
                                  `HighVisRaw` = .data$HighVisRaw,
                                  `LowVisRaw` = .data$LowVisRaw,
                                  `IRRaw` = .data$IRRaw,
                                  `UVRaw` = .data$UVRaw,
                                  `PMMeasCnt(#)` = .data$PMMeasCnt,
                                  `PM1MC(ug/m^3)` = .data$PM1MC,
                                  `PM1MCVar(ug/m^3)` = .data$PM1MCVar,
                                  `PM2_5MC(ug/m^3)` = .data$PM2_5MC,
                                  `PM2_5MCVar(ug/m^3)` = .data$PM2_5MCVar,
                                  `PM4MC(ug/m^3)` = .data$PM4MC,
                                  `PM4MCVar(ug/m^3)` = .data$PM4MCVar,
                                  `PM10MC(ug/m^3)` = .data$PM10MC,
                                  `PM10MCVar(ug/m^3)` = .data$PM10MCVar,
                                  `PM0_5NC(#/cm^3)` = .data$PM0_5NC,
                                  `PM0_5NCVar(#/cm^3)` = .data$PM0_5NCVar,
                                  `PM1NC(#/cm^3)` = .data$PM1NC,
                                  `PM1NCVar(#/cm^3)` = .data$PM1NCVar,
                                  `PM2_5NC(#/cm^3)` = .data$PM2_5NC,
                                  `PM2_5NCVar(#/cm^3)` = .data$PM2_5NCVar,
                                  `PM4NC(#/cm^3)` = .data$PM4NC,
                                  `PM4NCVar(#/cm^3)` = .data$PM4NCVar,
                                  `PM10NC(#/cm^3)` = .data$PM10NC,
                                  `PM10NCVar(#/cm^3)` = .data$PM10NCVar,
                                  `PMtypicalParticleSize(um)` = .data$PMtypicalParticleSize,
                                  `PMtypicalParticleSizeVar(um)` = .data$PMtypicalParticleSizeVar,
                                  `PM2_5SampledMass(ug)` = .data$PM2_5SampledMass,
                                  `PCB1T(C)` = .data$PCB1T,
                                  `PCB2T(C)` = .data$PCB2T,
                                  `FdpT(C)` = .data$FdpT,
                                  `AccelT(C)` = .data$AccelT,
                                  `PT100R(ohm)` = .data$PT100R,
                                  `PCB2P(hPa)` = .data$PCB2P,
                                  `PumpPow1(integer)` = .data$PumpPow1,
                                  `PumpPow2(integer)` = .data$PumpPow2,
                                  `PumpV(V)` = .data$PumpV,
                                  `MassFlow(g/min)` = .data$MassFlow,
                                  `MFSVout(V)` = .data$MFSVout,
                                  `BFGenergy(16-bit integer)` = .data$BFGenergy,
                                  `BattVolt(V)` = .data$BattVolt,
                                  `v3_3(V)` = .data$v3_3,
                                  `v5(V)` = .data$v5,
                                  `PumpsON(bool)` = .data$PumpsON,
                                  `Dead(bool)` = .data$Dead,
                                  `BCS1(bool)` = .data$BCS1,
                                  `BCS2(bool)` = .data$BCS2,
                                  `BC_NPG(bool)` = .data$BC_NPG,
                                  `FLOWCTL(s)` = .data$FLOWCTL,
                                  `GPSRT(s)` = .data$GPSRT,
                                  `SD_DATAW(s)` = .data$SD_DATAW,
                                  `SD_HEADW(s)` = .data$SD_HEADW,
                                  `TPumpsOFF(s)` = .data$TPumpsOFF,
                                  `TPumpsON(s)` = .data$TPumpsON,
                                  `IAQStabStat` = .data$IAQStabStat,
                                  `IAQRunIn` = .data$IAQRunIn,
                                  `IAQRes(ohms)` = .data$IAQRes,
                                  `IAQ` = .data$IAQ,
                                  `IAQAcc` = .data$IAQAcc,
                                  `StaticIAQ` = .data$StaticIAQ,
                                  `StaticIAQAcc` = .data$StaticIAQAcc,
                                  `CO2e(ppm)` = .data$CO2e,
                                  `CO2eAcc` = .data$CO2eAcc,
                                  `bVOC(ppm)` = .data$bVOC,
                                  `bVOCAcc` = .data$bVOCAcc,
                                  `gasComp` = .data$gasComp,
                                  `gasCompAcc` = .data$gasCompAcc,
                                  `gasPerc(%)` = .data$gasPerc,
                                  `gasPercAcc` = .data$gasPercAcc,
                                  `tz_value` = .data$tz_value,
                                  #`ASTSampler` = .data$ASTSampler,
                                  # `UPASserial` = .data$UPASserial,
                                  # `SampleName` = .data$SampleName,
                                  # `CartridgeID` = .data$CartridgeID,
                                  `StartDateTimeUTC(YYYY-MM-DDTHH:MM:SS)` = .data$StartDateTimeUTC
                                  # `LogFileMode` = .data$LogFileMode
      )
    #colnames(df) <- paste(colnames(df), units, sep="")
  }

  if(!is.null(cols_keep)){
    df <- df %>%
      dplyr::select(cols_keep)
  }else if(!is.null(cols_drop)){
    df <- df %>%
      dplyr::select(-cols_keep)
  }

  return(df)

}


#'Create sample summary dataframe from an Access Sensor Technologies (AST)
#'air sampler header dataframe
#'
#' @param df_h Pass a upasv2x header dataframe from read_ast_header function.
#'
#' @return A modified data frame with only the sample summary data.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2x_sample_summary <- upas2x_sample_summary(upasv2x_header, upasv2x_log)
#' upasv2x_sample_summary <- upas2x_sample_summary(upasv2x_header)

upas2x_sample_summary = function(df_h, df = NULL) {

  sample_summary_df <- df_h %>%
    dplyr::select(dplyr::any_of(c('UPASserial','SampleName','CartridgeID',
                                  'SampledRuntime', 'OverallDuration', 'PumpingDuration',
                                'OverallFlowRateAverage','PM25SampledMass',
                                'SampledVolume','ShutdownReason'))) %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::any_of(c('SampledRuntime',
                                                        'AverageVolumetricFlowRate',
                                   'PM25SampledMass',
                                   'SampledVolume')), .fns = as.numeric))

    if(!is.null(df)){
      if(any(grepl('PM2_5SampledMass', names(df)))){
        sample_summary_df$PM2_5SampledMass <- max(df$PM2_5SampledMass)
      }
    }

    if(any(grepl('PM25SampledMass', names(sample_summary_df)))){
      sample_summary_df <- sample_summary_df %>%
      dplyr::mutate(PM25Concentration = .data$PM25SampledMass/(.data$SampledVolume/1000),
                    dplyr::across(where(is.numeric), ~ round(., digits = 3)))
    }

  return(sample_summary_df)
}

#'Create sample settings dataframe from an Access Sensor Technologies (AST)
#'air sampler header dataframe
#'
#' @param df_h Pass a upasv2x header dataframe from read_ast_header function.
#'
#' @return A modified data frame with only the sample settings data.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2x_sample_settings <- upas2x_sample_settings(upasv2x_header)

upas2x_sample_settings = function(df_h) {

  sample_settings_df <- df_h %>%
    dplyr::select(dplyr::any_of(c('UPASserial', 'SampleName', 'CartridgeID',
                                  'StartOnNextPowerUp','ProgrammedStartDelay',
                                  'ProgrammedStartTime','ProgrammedRuntime',
                                  'SizeSelectiveInlet','VolumetricFlowRateSet',
                                  'FlowRateSetpoint','DutyCycle',
                                  'FlowDutyCycle','GPSEnabled',
                                  'PMSensorOperation','RTGasSampleState',
                                  'LogInterval',
                                  'PowerSaveMode','AppVersion')))

  return(sample_settings_df)
}

#'Create sample metadata dataframe from an Access Sensor Technologies (AST)
#'air sampler header dataframe
#'
#' @param df_h Pass a upasv2x header dataframe from read_ast_header function.
#'
#' @return A modified data frame with only the sample settings data.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2x_sample_meta <- upas2x_sample_meta(upasv2x_header)

upas2x_sample_meta = function(df_h) {

  sample_meta_df <- df_h %>%
    dplyr::select(dplyr::any_of(c('UPASserial','PMSerial','SampleName',
                                'CartridgeID','StartDateTimeUTC',
                  'EndDateTimeUTC','StartBatteryVoltage','EndBatteryVoltage',
                  'StartBatteryCharge','EndBatteryCharge',
                  'GPSUTCOffset','FirmwareRev','ShutdownMode')))


  return(sample_meta_df)
}
