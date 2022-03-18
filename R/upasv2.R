# Purpose: This function reads just the header data from a UPAS log file.
# If the user just wants to calculate the time-averaged PM concentration
# measured using the filter sample, all of the sample summary information that
# is needed to do so is stored in the header and read by this function.
# This function can be used in conjuction with lapply() or map() to read in
# sample summary data from any number of log files and create a data frame that
# contains a line with the summary information for each sample.

# Inputs:
# (1) The file name
# (2) update_names: a logical variable indicating whether variable names in
#                   log files recorded using firmware version 100
#                   should be updated to match variable names in log files
#                   recorded using firmware versions > 100 (default = FALSE).
#                   If TRUE, "CumulativeSamplingTime" will be updated to
#                   "LifetimeSampleRuntime","StartDateTime" will be updated to
#                   "StartDateTimeUTC", and "AverageVolumetricFlow" will be
#                   updated to"AverageVolumetricFlowRate".

# Output: A data frame with one row and 28 to 34 columns (depending on the
#   firmware version) containing the data in the UPAS sample file header.

#' Read the header data from a UPASv2 log file
#'
#' @param df_h A UPASv2 header dataframe
#' @param update_names Convert old log file column names to match current log
#' file names.
#'
#' @return A data frame.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2_header <- format_upasv2_header(upasv2_header_raw, update_names=FALSE)

format_upasv2_header <- function(df_h, update_names=FALSE){


  df_h <- df_h %>%
    dplyr::mutate(Sampler = sub("-rev.*", "", .data$Firmware),
                  FirmwareRev = sapply(strsplit(.data$Firmware,"-"), `[`, 2),
                  FirmwareRev = as.numeric(gsub("rev", "", .data$FirmwareRev)))

  df_h <- df_h %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("UPASserial",
                                                "GPSUTCOffset",
                                                "StartOnNextPowerUp",
                                                "ProgrammedStartDelay",
                                                "ProgrammedRuntime",
                                                "VolumetricFlowRate",
                                                "DutyCycle",
                                                "DutyCycleWindow",
                                                "GPSEnabled",
                                                "LogFileMode",
                                                "LogInterval",
                                                "AppLock",
                                                "StartBatteryCharge",
                                                "StartBatteryVoltage",
                                                "EndBatteryCharge",
                                                "EndBatteryVoltage",
                                                "ShutdownMode",
                                                "SampledVolume",
                                                "SampledRuntime",
                                                "LoggedRuntime")),
                         as.numeric)) %>%
    dplyr::rename(Serial = .data$UPASserial,
                  LogFilename = .data$UPASlogFilename) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("StartOnNextPowerUp",
                                                "GPSEnabled")), as.logical)) %>%
    dplyr::mutate(Serial = as.numeric(.data$Serial),
                  LogFilename = gsub("/sd/", "", .data$LogFilename),
                  LogFileMode     = ifelse(.data$LogFileMode == 0, "normal", "debug"),
                  ShutdownReason  = dplyr::case_when(ShutdownMode == 0 ~ "unknown error",
                                              ShutdownMode == 1 ~ "user pushbutton stop",
                                              ShutdownMode == 2 ~ "depleted battery",
                                              ShutdownMode == 3 ~ "completed preset sample duration",
                                              ShutdownMode == 4 ~ "thermal protection",
                                              ShutdownMode == 5 ~ "max power at initialization",
                                              ShutdownMode == 6 ~ "max power during sample",
                                              ShutdownMode == 7 ~ "blocked flow")) %>%
    dplyr::select(1:match("ShutdownMode",colnames(df_h)), .data$ShutdownReason, (match("ShutdownMode",colnames(df_h))+1):ncol(df_h))

  if(df_h$FirmwareRev == 100){

    df_h <- df_h %>%
      dplyr::mutate_at(c("PowerCycles","CumulativeSamplingTime","AverageVolumetricFlow"), as.numeric) %>%
      dplyr::mutate(StartDateTime = as.POSIXct(.data$StartDateTime, format="%Y-%m-%dT%H:%M:%SUTC", tz="UTC"))

    if(update_names){

      df_h <- df_h %>% dplyr::rename(LifetimeSampleRuntime = .data$CumulativeSamplingTime,
                                 StartDateTimeUTC          = .data$StartDateTime,
                                 AverageVolumetricFlowRate = .data$AverageVolumetricFlow)}
  }else{

    df_h <- df_h %>%
      dplyr::mutate_at(c("LifetimeSampleCount","LifetimeSampleRuntime","FlowOffset","AverageVolumetricFlowRate"), as.numeric) %>%
      dplyr::mutate_at(c("StartDateTimeUTC", "EndDateTimeUTC"), as.POSIXct, format="%Y-%m-%dT%H:%M:%S", tz="UTC") %>%
      dplyr::mutate(SampleName  = gsub("_+$", "", .data$SampleName),
                    SampleName  = ifelse(.data$SampleName != "", .data$SampleName, NA),
                    CartridgeID = gsub("_+$", "", .data$CartridgeID),
                    CartridgeID = gsub("-+$", "", .data$CartridgeID),
                    CartridgeID = ifelse(.data$CartridgeID != "", .data$CartridgeID, NA))
    }


    if(update_names){
      df_h <- df_h %>%
        dplyr::rename(FlowRateSetpoint = .data$VolumetricFlowRate,
                      FlowDutyCycle = .data$DutyCycle,
                      OverallDuration = .data$SampledRuntime,
                      OverallFlowRateAverage = .data$AverageVolumetricFlowRate)
    }

  return(df_h)
}


#' Read the log data from a UPASv2 log file
#'
#' @param df A UPASv2 dataframe
#' @param update_names Convert old log file column names to match current log
#' file names.
#'
#' @return A data frame.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2_log <- format_upasv2_header(upasv2_header, upasv2_log_raw)

format_upasv2_log = function(df_h, df_raw, tz_offset = NA, update_names=FALSE) {

  # Get header data
  df_h_sel <- df_h %>%
    dplyr::select(dplyr::any_of(c("Sampler",
                           "Serial",
                           "UPASlogFilename",
                           "SampleName",
                           "CartridgeID",
                           "StartDateTimeUTC",
                           "LogFileMode",
                           "GPSUTCOffset")))

  tz_off <- ifelse(is.na(tz_offset),df_h_sel$GPSUTCOffset, tz_offset)

  df_h_sel <- df_h_sel %>%
    dplyr::select(-.data$GPSUTCOffset)

  if(nrow(df_raw) > 0){

    df_raw[df_raw == 'NULL'] <- NA

    df <- df_raw %>%
      dplyr::mutate(SampleTime = ifelse(.data$SampleTime == "99:99:99",
                                        NA,
                                        .data$SampleTime),
                    SampleTime = ifelse(!is.na(.data$SampleTime),
                                        strsplit(.data$SampleTime,":"),
                                        .data$SampleTime),
                    SampleTime = as.difftime(3600*as.numeric(sapply(.data$SampleTime, `[`, 1)) +
                                               60*as.numeric(sapply(.data$SampleTime, `[`, 2)) +
                                               as.numeric(sapply(.data$SampleTime, `[`, 3)), units="secs"))

    if("UTCDateTime" %in% colnames(df)){ # For firmware version 100

      df <- df %>%
        dplyr::mutate(dplyr::across(-dplyr::one_of(c("SampleTime",
                                                      "UTCDateTime",
                                                      "DateTimeLocal")),
                                                  as.numeric),
                                    UTCDateTime = as.POSIXct(.data$UTCDateTime,
                                               format="%Y-%m-%dT%H:%M:%S",
                                               tz="UTC"))

      if(update_names){
        df <- df %>% dplyr::rename(DateTimeUTC = .data$UTCDateTime,
                                   VolumetricFlowRate = .data$VolFlow,
                                   LogFilename = .data$UPASLogFilename,
                                   )}

    }else{ # For firmware version > 100
      df <- df %>%
        dplyr::mutate(dplyr::across(-dplyr::one_of(c("SampleTime",
                                                      "DateTimeUTC",
                                                      "DateTimeLocal")),
                                                              as.numeric),
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
    }

    if(!is.null(df_h$LogFileMode)){
      # For debug files
      if((df_h$LogFileMode == "debug") & ("PumpsON" %in% colnames(df))){

        df <- df %>%
          dplyr::mutate(dplyr::across(dplyr::any_of(c("PumpsON",
                             "Dead",
                             "BCS1",
                             "BCS2",
                             "BC_NPG")), as.logical))

        if(("gpsspeed" %in% colnames(df)) & update_names){
          df <- df %>% dplyr::rename(GPSspeed   = .data$gpsspeed,
                                     GPSquality = .data$gpsquality)}
      }
    }
  }


  if(update_names){
    df <- df %>%
      dplyr::rename(OverallFlowRate = .data$VolumetricFlowRate,
                    AtmoDensity = .data$AtmoRho,
                    FilterDP = .data$FdPdP,
                    AtmoT = .data$PumpT,
                    AtmoRH = .data$PumpRH,
                    PCB1T = .data$PCBT,
                    PCB2P = .data$PumpP,
                    AtmoP = .data$PumpP,
                    GPShDOP = .data$GPShdop,
                    BattVolt = .data$BFGvolt)
  }

  df <- df %>%
    cbind(df_h_sel)

  return(df)
}

