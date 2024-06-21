
#' Formats UPASv2 header data that has already been transposed to a wide
#' data frame
#'
#' @description
#' `format_upasv2_header` completes the UPASv2 log file header formatting prior
#' to running any data analysis. It sets the proper data types for each variable,
#' adds a column to specify the AST sampler type, adds a column to describe the
#' shutdown reason associated with the shutdown mode code, and - if
#' `update_names=TRUE` - updates old log file variable names
#' to match current log file names.
#'
#' @param df_h A UPASv2 header data frame transposed to wide format using
#' [transpose_raw_ast_header]
#' @param update_names If `TRUE`, convert old log file column names to match
#' latest version.
#' If firmware > rev100:
#' * VolumetricFlowRate        -> FlowRateSetpoint
#' * DutyCycle                 -> FlowDutyCycle
#' * SampledRuntime            -> OverallDuration
#' * AverageVolumetricFlowRate -> PumpingFlowRateAverage
#'
#' If firmware is equal to rev100:
#' * CumulativeSamplingTime -> LifetimeSampleRuntime
#' * StartDateTime          -> StartDateTimeUTC
#' * AverageVolumetricFlow  -> PumpingFlowRateAverage
#'
#' Behavior not defined for firmwares < rev100
#'
#' @return A data frame with formatted UPASv2 header data in wide format that
#' is ready for data analysis
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2_rev125_filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
#' upasv2_rev125_file <- system.file("extdata", upasv2_rev125_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev125_header_raw <- make_raw_ast_header(upasv2_rev125_file)
#' upasv2_rev125_header_transp <- transpose_raw_ast_header(upasv2_rev125_header_raw)
#' upasv2_rev125_header <- format_upasv2_header(upasv2_rev125_header_transp, update_names=FALSE)
#' upasv2_rev130_diag_filename <- 'PS1786_LOG_2023-03-02T21_45_43UTC_DIAGNOSTIC____________.txt'
#' upasv2_rev130_diag_file <- system.file("extdata", upasv2_rev130_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev130_diag_header_raw <- make_raw_ast_header(upasv2_rev130_diag_file)
#' upasv2_rev130_diag_header_transp <- transpose_raw_ast_header(upasv2_rev130_diag_header_raw)
#' upasv2_rev130_diag_header <- format_upasv2_header(upasv2_rev130_diag_header_transp, update_names=FALSE)
#' upasv2_rev138_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
#' upasv2_rev138_file <- system.file("extdata", upasv2_rev138_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev138_header_raw <- make_raw_ast_header(upasv2_rev138_file)
#' upasv2_rev138_header_transp <- transpose_raw_ast_header(upasv2_rev138_header_raw)
#' upasv2_rev138_header <- format_upasv2_header(upasv2_rev138_header_transp, update_names=TRUE)
#' upasv2_rev138_diag_filename <- 'PS1771_LOG_2024-06-13T21_31_26UTC_DIAGNOSTIC____________.txt'
#' upasv2_rev138_diag_file <- system.file("extdata", upasv2_rev138_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev138_diag_header_raw <- make_raw_ast_header(upasv2_rev138_diag_file)
#' upasv2_rev138_diag_header_transp <- transpose_raw_ast_header(upasv2_rev138_diag_header_raw)
#' upasv2_rev138_diag_header <- format_upasv2_header(upasv2_rev138_diag_header_transp, update_names=FALSE)

format_upasv2_header <- function(df_h, update_names=FALSE){

  df_h <- dplyr::rename(df_h, LogFilename = "UPASlogFilename")

  df_h <- dplyr::mutate(df_h,
                  ASTSampler = sub("-rev.*", "", .data$Firmware),
                  FirmwareRev = sapply(strsplit(.data$Firmware,"-"), `[`, 2),
                  FirmwareRev = as.numeric(gsub("rev", "", .data$FirmwareRev)),
                  dplyr::across(dplyr::any_of(c("UPASserial",
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
                                \(x) as.numeric(x)),
                  dplyr::across(dplyr::any_of(c("StartOnNextPowerUp",
                                                "GPSEnabled")),
                                \(x) as.logical(x)),
                  LogFilename = gsub("/sd/", "", .data$LogFilename),
                  LogFileMode = ifelse(.data$LogFileMode==0, "normal", "debug"),
                  ShutdownReason  = dplyr::case_when(
                    .data$ShutdownMode == 0 ~ "unknown error",
                    .data$ShutdownMode == 1 ~ "user pushbutton stop",
                    .data$ShutdownMode == 2 ~ "depleted battery",
                    .data$ShutdownMode == 3 ~ "completed preset sample duration",
                    .data$ShutdownMode == 4 ~ "thermal protection",
                    .data$ShutdownMode == 5 ~ "max power at initialization",
                    .data$ShutdownMode == 6 ~ "max power during sample",
                    .data$ShutdownMode == 7 ~ "blocked flow"))

  df_h <- df_h %>%
    dplyr::relocate("ASTSampler") %>%
    dplyr::relocate("FirmwareRev", .after = "Firmware") %>%
    dplyr::relocate("ShutdownReason", .after = "ShutdownMode")

  if(df_h$FirmwareRev == 100){

    df_h <- dplyr::mutate(df_h,
                          across(c("PowerCycles","CumulativeSamplingTime",
                                   "AverageVolumetricFlow"), \(x) as.numeric(x)),
                          StartDateTime = as.POSIXct(.data$StartDateTime,
                                                format = "%Y-%m-%dT%H:%M:%SUTC",
                                                tz = "UTC"))

    if(update_names){

      df_h <- dplyr::rename(df_h,
                            LifetimeSampleRuntime = "CumulativeSamplingTime",
                            StartDateTimeUTC = "StartDateTime",
                            AverageVolumetricFlowRate = "AverageVolumetricFlow")
    }

  }else{

    df_h <- dplyr::mutate(df_h,
      across(c("LifetimeSampleCount","LifetimeSampleRuntime","FlowOffset",
               "AverageVolumetricFlowRate"), \(x) as.numeric(x)),
      across(c("StartDateTimeUTC", "EndDateTimeUTC"),
             \(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")),
      SampleName  = gsub("_+$", "", .data$SampleName),
      SampleName  = ifelse(.data$SampleName != "", .data$SampleName, NA),
      CartridgeID = gsub("_+$", "", .data$CartridgeID),
      CartridgeID = gsub("-+$", "", .data$CartridgeID),
      CartridgeID = ifelse(.data$CartridgeID != "", .data$CartridgeID, NA))
    }


    if(update_names){

      df_h <- dplyr::rename(df_h,
                          FlowRateSetpoint = "VolumetricFlowRate",
                          FlowDutyCycle = "DutyCycle",
                          OverallDuration = "LoggedRuntime",
                          PumpingDuration = "SampledRuntime",
                          PumpingFlowRateAverage = "AverageVolumetricFlowRate")

      if(any(df_h$SampleName == 'DIAGNOSTIC')) {

        df_h <- dplyr::rename(df_h,
                              MFSCalVoutMin = "MFSVoltMin",
                              MFSCalVoutMax = "MFSVoltMax",
                              MFSCalMFMin = "MFSMFMin",
                              MFSCalMFMax = "MFSMFMax",
                              MFSCalDate = "CalDateTime")
        df_h <- dplyr::select(df_h,
                              !c("MFSVoltMaxEst", "MFSMFMaxEst", "CalUNIXTIME"))
      }
    }

  return(df_h)
}

#' Read the log data from a UPASv2 log file
#'
#' @param df_h A UPASv2 header dataframe
#' @param df_log A UPASv2 raw dataframe
#' @param update_names Convert old log file column names to match current log
#' file names.
#' @param tz_offset Pass an option timezone offset.
#' @param update_names Option to update old sampler names to latest version.
#'
#' @return A data frame.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2_log <- format_upasv2_log(upasv2_header, upasv2_log_raw)

format_upasv2_log = function(df_h, df_log, tz_offset = NA, update_names=FALSE){

  tz_off <- ifelse(is.na(tz_offset), df_h$GPSUTCOffset, tz_offset)

  # Get header data
  df_h_sel <- dplyr::select(df_h, dplyr::any_of(c("ASTSampler","UPASserial",
                                                  "UPASlogFilename",
                                                  "SampleName","CartridgeID",
                                                  "StartDateTimeUTC",
                                                  "LogFileMode")))

  if(nrow(df_log) > 0){

    df <- dplyr::mutate(df_log,
            SampleTime = ifelse(.data$SampleTime == "99:99:99", NA,
                                .data$SampleTime),
            SampleTime = ifelse(!is.na(.data$SampleTime),
                                strsplit(.data$SampleTime,":"),
                                .data$SampleTime),
            SampleTime = as.difftime(
                            3600*as.numeric(sapply(.data$SampleTime, `[`, 1)) +
                              60*as.numeric(sapply(.data$SampleTime, `[`, 2)) +
                                 as.numeric(sapply(.data$SampleTime, `[`, 3)),
                                     units="secs"))

    if("UTCDateTime" %in% colnames(df)){ # For firmware version 100

      df <- dplyr::mutate(df,
                          dplyr::across(-dplyr::one_of(c("SampleTime",
                                                         "UTCDateTime",
                                                         "DateTimeLocal")),
                                        \(x) as.numeric(x)),
                          UTCDateTime = as.POSIXct(.data$UTCDateTime,
                                                   format = "%Y-%m-%dT%H:%M:%S",
                                                   tz = "UTC"))

      if(update_names){

        df <- dplyr::rename(df, DateTimeUTC        = .data$UTCDateTime,
                                VolumetricFlowRate = .data$VolFlow,
                                LogFilename        = .data$UPASLogFilename)
        }

    }else{ # For firmware version > 100
      df <- dplyr::mutate(df,
                          dplyr::across(-dplyr::one_of(c("SampleTime",
                                                         "DateTimeUTC",
                                                         "DateTimeLocal")),
                                        \(x) as.numeric(x)),
                          DateTimeUTC = as.POSIXct(.data$DateTimeUTC,
                                                   format = "%Y-%m-%dT%H:%M:%S",
                                                   tz = "UTC"),
                          tz_value = ifelse(!is.na(tz_offset), T, F),
                          DateTimeLocal = .data$DateTimeUTC + (tz_off * 3600),
                          TZOffset = tz_off)
    }

    if(!is.null(df_h$LogFileMode)){
      # For debug files
      if((df_h$LogFileMode == "debug") & ("PumpsON" %in% colnames(df))){

        df <- dplyr::mutate(df,
                            across(any_of(c("PumpsON","Dead","BCS1","BCS2",
                                            "BC_NPG")), \(x) as.logical(x)))

        if(("gpsspeed" %in% colnames(df)) & update_names){
          df <-  dplyr::rename(df, GPSspeed   = .data$gpsspeed,
                                   GPSquality = .data$gpsquality)}
      }
    }
  }


  if(update_names){

    df <- dplyr::rename(df,
                        dplyr::any_of(c(PumpingFlowRate = "VolumetricFlowRate",
                                        AtmoDensity     = "AtmoRho",
                                        FilterDP        = "FdPdP",
                                        AtmoT           = "PumpT",
                                        AtmoRH          = "PumpRH",
                                        PCB1T           = "PCBT",
                                        PCB2P           = "PumpP",
                                        AtmoP           = "PCBP",
                                        GPShDOP         = "GPShdop",
                                        # GPSQual         = "GPSquality",
                                        #TODO convert BGFvolt to a battery percentage for shiny app output
                                        BattVolt        = "BFGvolt")))
  }

  df <- cbind(df, df_h_sel)

  return(df)
}

