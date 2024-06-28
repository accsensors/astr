#' Format UPAS v2 header data that hae already been transposed to a wide data frame
#'
#' @description
#' `format_upasv2_header` formats the header data from a UPAS v2 log file.
#' This function sets the proper data type for each variable, adds a column
#' specifying the AST sampler type, adds a column describing the shutdown reason
#' associated with the shutdown mode code, and can be directed to update old
#' variable names to the current names.
#'
#' @param data A UPASv2 header data frame returned by [transpose_ast_header]
#' @inheritParams read_ast_header
#' @return A data frame with a single row of UPAS v2 header data that are formatted and ready for analysis.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2_rev100_filename <- 'PS1422_LOG_2020-06-02T18_26_25UTC_rev100-norm________---.txt'
#' upasv2_rev100_file <- system.file("extdata", upasv2_rev100_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev100_header_raw <- fread_ast_header(upasv2_rev100_file)
#' upasv2_rev100_header_transp <- transpose_ast_header(upasv2_rev100_header_raw)
#' upasv2_rev100_header <- format_upasv2_header(upasv2_rev100_header_transp, update_names=FALSE)
#' upasv2_rev100_diag_filename <- 'PS1422_LOG_2020-06-02T18_29_11UTC_DIAGNOSTIC____________.txt'
#' upasv2_rev100_diag_file <- system.file("extdata", upasv2_rev100_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev100_diag_header_raw <- fread_ast_header(upasv2_rev100_diag_file)
#' upasv2_rev100_diag_header_transp <- transpose_ast_header(upasv2_rev100_diag_header_raw)
#' upasv2_rev100_diag_header <- format_upasv2_header(upasv2_rev100_diag_header_transp, update_names=FALSE)
#' upasv2_rev125_filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
#' upasv2_rev125_file <- system.file("extdata", upasv2_rev125_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev125_header_raw <- fread_ast_header(upasv2_rev125_file)
#' upasv2_rev125_header_transp <- transpose_ast_header(upasv2_rev125_header_raw)
#' upasv2_rev125_header <- format_upasv2_header(upasv2_rev125_header_transp, update_names=FALSE)
#' upasv2_rev130_diag_filename <- 'PS1786_LOG_2023-03-02T21_45_43UTC_DIAGNOSTIC____________.txt'
#' upasv2_rev130_diag_file <- system.file("extdata", upasv2_rev130_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev130_diag_header_raw <- fread_ast_header(upasv2_rev130_diag_file)
#' upasv2_rev130_diag_header_transp <- transpose_ast_header(upasv2_rev130_diag_header_raw)
#' upasv2_rev130_diag_header <- format_upasv2_header(upasv2_rev130_diag_header_transp, update_names=FALSE)
#' upasv2_rev138_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
#' upasv2_rev138_file <- system.file("extdata", upasv2_rev138_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev138_header_raw <- fread_ast_header(upasv2_rev138_file)
#' upasv2_rev138_header_transp <- transpose_ast_header(upasv2_rev138_header_raw)
#' upasv2_rev138_header <- format_upasv2_header(upasv2_rev138_header_transp, update_names=TRUE)
#' upasv2_rev138_diag_filename <- 'PS1771_LOG_2024-06-13T21_31_26UTC_DIAGNOSTIC____________.txt'
#' upasv2_rev138_diag_file <- system.file("extdata", upasv2_rev138_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev138_diag_header_raw <- fread_ast_header(upasv2_rev138_diag_file)
#' upasv2_rev138_diag_header_transp <- transpose_ast_header(upasv2_rev138_diag_header_raw)
#' upasv2_rev138_diag_header <- format_upasv2_header(upasv2_rev138_diag_header_transp, update_names=FALSE)

format_upasv2_header <- function(data, update_names=FALSE){

  data <- dplyr::rename(data, LogFilename = "UPASlogFilename")

  data <- dplyr::mutate(data,
                  ASTSampler = sub("-rev.*", "", .data$Firmware),
                  FirmwareRev = sapply(strsplit(.data$Firmware,"-"), `[`, 2),
                  FirmwareRev = as.numeric(gsub("rev", "", .data$FirmwareRev)),
                  dplyr::across(dplyr::any_of(c("UPASserial", "GPSUTCOffset",
                                                "StartOnNextPowerUp",
                                                "VolumetricFlowRate",
                                                "GPSEnabled", "LogFileMode",
                                                "LogInterval", "AppLock",
                                                "ShutdownMode",
                                                "SampledVolume",
                                                "PowerCycles",
                                                "CumulativeSamplingTime",
                                                "AverageVolumetricFlow",
                                                "AverageVolumetricFlowRate",
                                                "FlowOffset")),
                                \(x) as.numeric(x)),
                  dplyr::across(starts_with("Lifetime"),   \(x) as.numeric(x)),
                  dplyr::across(starts_with("Programmed"), \(x) as.numeric(x)),
                  dplyr::across(starts_with("DutyCycle"),  \(x) as.numeric(x)),
                  dplyr::across(contains("Battery"),       \(x) as.numeric(x)),
                  dplyr::across(ends_with("Runtime"),      \(x) as.numeric(x)),
                  dplyr::across(dplyr::any_of(c("StartOnNextPowerUp",
                                                "GPSEnabled")),
                                \(x) as.logical(x)),
                  across(dplyr::any_of(c("StartDateTimeUTC", "EndDateTimeUTC",
                                         "StartDateTime")), \(x)
                         as.POSIXct(x, format="%Y-%m-%dT%H:%M:%S", tz="UTC")),
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

  if(data$FirmwareRev != 100){

    data <- dplyr::mutate(data,
                        SampleName  = gsub("_+$", "", .data$SampleName),
                        SampleName  = ifelse(.data$SampleName != "", .data$SampleName, NA),
                        CartridgeID = gsub("_+$", "", .data$CartridgeID),
                        CartridgeID = gsub("-+$", "", .data$CartridgeID),
                        CartridgeID = ifelse(.data$CartridgeID != "", .data$CartridgeID, NA))
  }

  data <- data %>%
    dplyr::relocate("ASTSampler") %>%
    dplyr::relocate("FirmwareRev", .after = "Firmware") %>%
    dplyr::relocate("ShutdownReason", .after = "ShutdownMode")

  if(update_names){

    data <- dplyr::rename(data, dplyr::any_of(
                        c(LifetimeSampleRuntime  = "CumulativeSamplingTime",
                          StartDateTimeUTC       = "StartDateTime",
                          PumpingFlowRateAverage = "AverageVolumetricFlow",
                          FlowRateSetpoint       = "VolumetricFlowRate",
                          FlowDutyCycle          = "DutyCycle",
                          OverallDuration        = "LoggedRuntime",
                          PumpingDuration        = "SampledRuntime",
                          PumpingFlowRateAverage = "AverageVolumetricFlowRate",
                          MFSCalVoutMin          = "MFSVoltMin",
                          MFSCalVoutMax          = "MFSVoltMax",
                          MFSCalMFMin            = "MFSMFMin",
                          MFSCalMFMax            = "MFSMFMax",
                          MFSCalDate             = "CalDateTime")))
    data <- dplyr::select(data, -dplyr::any_of(c("MFSVoltMaxEst","MFSMFMaxEst","CalUNIXTIME")))
  }

  return(data)
}

#'Format the sample log data from an Access Sensor Technologies UPAS v2
#'
#' @param log A data frame of UPAS v2 sample log data returned by the [fread_ast_log] function.
#' @param header A data frame of UPAS v2 header data returned by the [read_ast_header] function.
#' @inheritParams format_ast_log
#'
#' @return A data frame of of UPAS v2 sample log data that are formatted and ready for analysis.
#' This data frame will contain one row for each timestamp in the sample log.
#'
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2_log <- format_upasv2_log(upasv2_header, upasv2_log_raw)

format_upasv2_log = function(log, header, update_names=FALSE, tz=NA, cols_keep=c(), cols_drop=c()){

  # Get header data
  df_h <- dplyr::select(header, dplyr::any_of(c("ASTSampler","UPASserial",
                                                "UPASlogFilename",
                                                "SampleName","CartridgeID",
                                                "StartDateTimeUTC",
                                                "LogFileMode")))

  if(nrow(log) > 0){

    df <- dplyr::mutate(log,
            SampleTime = ifelse(.data$SampleTime == "99:99:99", NA,
                                .data$SampleTime),
            SampleTime = ifelse(!is.na(.data$SampleTime),
                                strsplit(.data$SampleTime,":"),
                                .data$SampleTime),
            SampleTime = as.difftime(
                            3600*as.numeric(sapply(.data$SampleTime, `[`, 1)) +
                              60*as.numeric(sapply(.data$SampleTime, `[`, 2)) +
                                 as.numeric(sapply(.data$SampleTime, `[`, 3)),
                                     units="secs"),
            dplyr::across(dplyr::any_of(c("UTCDateTime", "DateTimeUTC")),
                   \(x) as.POSIXct(x, format="%Y-%m-%dT%H:%M:%S", tz="UTC")),
            dplyr::across(-dplyr::one_of(c("SampleTime","DateTimeUTC",
                                           "UTCDateTime","DateTimeLocal")),
                          \(x) as.numeric(x)),
            UserTZ  = ifelse(!is.na(tz), T, F))

    if("UTCDateTime" %in% colnames(df)){ # For firmware version 100

      df <- dplyr::mutate(df, LocalTZ = ifelse(!is.na(tz), tz, NA))

      if(!is.na(unique(df$LocalTZ))){

        df <- dplyr::mutate(df,
                           DateTimeLocal = lubridate::with_tz(.data$UTCDateTime,
                                                      tzone=unique(df$LocalTZ)))

        df <- dplyr::relocate(df, c("DateTimeLocal","LocalTZ"), .after="UTCDateTime")
      }

    }else{ # For firmware version > 100

      df <- dplyr::mutate(df,
              LocalTZ  = case_when(!is.na(tz) ~ tz,
                                   header$GPSUTCOffset == 0 ~ "UTC",
                           (round(header$GPSUTCOffset) == header$GPSUTCOffset) &
                            (header$GPSUTCOffset < 0) ~
                                sprintf("Etc/GMT+%i", abs(header$GPSUTCOffset)),
                           (round(header$GPSUTCOffset) == header$GPSUTCOffset) &
                             (header$GPSUTCOffset > 0) ~
                                sprintf("Etc/GMT-%i", abs(header$GPSUTCOffset)),
                           T ~ NA))

      if(!is.na(unique(df$LocalTZ))){
        df <- dplyr::mutate(df,
                           DateTimeLocal = lubridate::with_tz(.data$DateTimeUTC,
                                                      tzone=unique(df$LocalTZ)))
      }else{
        df <- dplyr::mutate(df, DateTimeLocal = as.character(DateTimeLocal))
      }

      df <- dplyr::relocate(df, c("DateTimeLocal","LocalTZ"), .after="DateTimeUTC")
    }

    if(!is.null(header$LogFileMode)){
      # For debug files
      if((header$LogFileMode == "debug") & ("PumpsON" %in% colnames(df))){

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
                        dplyr::any_of(c(DateTimeUTC     = "UTCDateTime",
                                        PumpingFlowRate = "VolFlow",
                                        LogFilename     = "UPASLogFilename",
                                        PumpingFlowRate = "VolumetricFlowRate",
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

  df <- cbind(df, df_h)

  if(!is.null(cols_keep)){
    df <- dplyr::select(df, cols_keep)
  }else if(!is.null(cols_drop)){
    df <- dplyr::select(df, -cols_drop)
  }

  return(df)
}
