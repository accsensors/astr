#'Format the header data from an Access Sensor Technologies Home Health Box log file
#'
#' @description
#' `format_hhb_header()` formats the header data from a HHB v2 log file by setting
#' the proper data type for each variable.
#'
#' @param df A HHB v2 header data frame returned by [astr::transpose_ast_header()]
#'
#' @return A data frame with a single row of HHB v2 header data that are formatted and ready for analysis.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' hhb_filename <- 'HHB00087_LOG_2025-06-03T20_55UTC.csv'
#' hhb_file <- system.file("extdata", hhb_filename, package = "astr", mustWork = TRUE)
#' hhb_header_raw <- fread_ast_header(hhb_file)$header
#' hhb_header_wide <- transpose_ast_header(hhb_header_raw)
#' hhb_header <- format_hhb_header(hhb_header_wide)

format_hhb_header = function(df) {

  df <- dplyr::mutate(df,
    Firmware = as.character(gsub(" ", "_", .data$Firmware)),
    dplyr::across(dplyr::contains(c("CalVoutMin", "CalVoutMax", "CalMFMin",
              "CalMFMax", "MF4","MF3", "MF2", "MF1", "MF0", "UTCOffset",
              "Runtime", "Volume", "FlowRate", "DutyCycle", "ShutdownMode",
              "_ID", "Gain", "_WE", "_AE", "Sensitivity", "StartDelay")),
           \(x) as.numeric(x)),
    dplyr::across(dplyr::contains(c("_ID")), \(x) as.character(x)),
    dplyr::across(dplyr::contains(c("StartDateTimeUTC", "EndDateTimeUTC",
                                    "CalDate", "StartTime")),
                    \(x) as.POSIXct(x, format="%Y-%m-%dT%H:%M:%S", tz="UTC")))

  return(df)
}


#'Format the sample log data from an Access Sensor Technologies Home Health Box log file
#'
#'
#' @param log A data frame of HHB v2 sample log data returned by the [astr::fread_ast_log()] function.
#' @param header A data frame of HHB v2 header data returned by the [astr::read_ast_header()] function.
#' @param tz Optional: A character string specifying the tz database time zone that should be used to display local times.
#' See [read_ast_log] for additional information.
#' @param cols_keep Optional: Provide a character vector specifying the names of a subset of sample log columns to keep.
#' @param cols_drop Optional: Provide a character vector specifying the names of a subset of sample log columns to remove.
#' See [read_ast_log] for additional information.
#'
#' @return A data frame of of HHB v2 sample log data that are formatted and ready for analysis.
#' This data frame will contain one row for each timestamp in the sample log.
#'
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' hhb_filename <- 'HHB00087_LOG_2025-06-03T20_55UTC.csv'
#' hhb_file <- system.file("extdata", hhb_filename, package = "astr", mustWork = TRUE)
#' hhb_log_raw <- fread_ast_log(hhb_file)
#' hhb_header <- read_ast_header(hhb_file)
#' hhb_log <- format_hhb_log(hhb_log_raw, hhb_header)
#'
#' # Use of cols_drop, cols_keep, and tz  with a UPASv2x log file
#' hhb_log_colsdrop <- format_hhb_log(hhb_log_raw, hhb_header,
#'                 cols_drop = c("AccelX", "AccelY", "AccelZ"))
#' hhb_log_colskeep <- format_hhb_log(hhb_log_raw, hhb_header, tz="America/New_York",
#'                 cols_keep = c("SampleTime", "DateTimeUTC", "DateTimeLocal",
#'                               "LocalTZ",  "UserTZ",
#'                               grep("^SEN55", colnames(hhb_log), value=TRUE)))

format_hhb_log = function(log, header, tz=NA, cols_keep=c(), cols_drop=c()) {

  df_h <- dplyr::select(header,
                        dplyr::any_of(c("HHBserial","LogFileName","SampleName")))
  df_h <- dplyr::mutate(df_h,
                        UserTZ  = ifelse(!is.na(tz), T, F),
                        LocalTZ = astr::get_tz_string(header$UTCOffset, tz=tz))

    df <- dplyr::mutate(log,
            SampleTime = ifelse(.data$SampleTime == "99:99:99", NA,
                                strsplit(.data$SampleTime,":")),
            SampleTime = as.difftime(
                          3600*as.numeric(sapply(.data$SampleTime, `[`, 1)) +
                            60*as.numeric(sapply(.data$SampleTime, `[`, 2)) +
                            as.numeric(sapply(.data$SampleTime, `[`, 3)),
                          units="secs"),
            dplyr::across(-dplyr::any_of(c("SampleTime","DateTimeUTC","SEN55_Status")),
                          \(x) as.numeric(x)),
            dplyr::across(dplyr::any_of(c("SEN55_I2C_Error")),
                          \(x) as.logical(x)))

    df <- cbind(df, df_h)

    if(!is.na(unique(df$LocalTZ))){
      df <- dplyr::mutate(df, DateTimeLocal = lubridate::with_tz(
                                   .data$DateTimeUTC, tzone=unique(df$LocalTZ)))
    }

  df <- dplyr::relocate(df, dplyr::any_of(c("DateTimeLocal","LocalTZ")),
                        .after = "DateTimeUTC")

  if (!is.null(cols_keep)){
    df <- dplyr::select(df, dplyr::all_of(cols_keep))
  }else if (!is.null(cols_drop)){
    df <- dplyr::select(df, -dplyr::all_of(cols_drop))
  }

  # If there were actually zero rows in he sample log and now there is 1 row
  # in the data frame (with the values appended from the header; all others NA),
  # filter the dataframe so that it contains zero rows.
  if(nrow(df) == 1){
    df <- dplyr::filter(df, !is.na(.data$SampleTime))
  }

  return(df)
}

#'Convert sample data from a Home Health Box log file header to long format.
#'
#' @description
#' `format_hhb_samples()` Selects the filter (particulate matter) and sorbent
#' (gas) sample data from a HHB v2 log file header and converts it to a long
#' format to facilitate matching of sample times, durations, and volumes with
#' sample mass or composition data from another data file.
#'
#' @param header A HHB v2 header data frame returned by [astr::read_ast_header()] or [astr::format_hhb_header()]
#'
#' @return A data frame with a four rows of HHB v2 sample data for each row in `header`.
#'
#' @details
#' The "HHBslot#" variables from the header data frame will be renamed "PumpPCB"
#' associated with the appropriate sample channel in the data frame returned by
#' this function.
#'
#' Additionally, the ProgrammedStartTime, ProgrammedRuntime, StartDateTimeUTC,
#' and EndDateTimeUTC variables associated with each sample channel (A-D) in the
#' SETUP SUMMARY and SAMPLE SUMMARY sections of the log file header will be
#' renamed to SampleProgrammedStartTime, SampleProgrammedRuntime,
#' SampleStartDateTimeUTC, and SampleEndDateTimeUTC in the data frame returned
#' by this function to distinguish them from the ProgrammedStartTime,
#' ProgrammedRuntime, StartDateTimeUTC, and EndDateTimeUTC variables associated
#' with the overall HHB sample.
#'
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' hhb_filename <- 'HHB00087_LOG_2025-06-03T20_55UTC.csv'
#' hhb_file <- system.file("extdata", hhb_filename, package = "astr", mustWork = TRUE)
#' hhb_header <- read_ast_header(hhb_file)
#' hhb_samples <- format_hhb_samples(hhb_header)

format_hhb_samples = function(header) {

  # Sample metadata variables to retain
  meta_vars <- c("HHBserial","HHBslot1","HHBslot2","HHBslot4","LogFileName",
                 "SampleName")

  # Select sample data (but do not include calibration data)
  df <- dplyr::select(header,
                      dplyr::any_of(meta_vars), dplyr::ends_with("CID"),
                      dplyr::starts_with("A."), dplyr::starts_with("B."),
                      dplyr::starts_with("C."), dplyr::starts_with("D."))
  df <- dplyr::select(df, -dplyr::contains("Cal"), -dplyr::contains("MF"))
  df <- dplyr::rename(df, A.FilterPumpPCB  = "HHBslot2",
                          B.FilterPumpPCB  = "HHBslot4",
                          C.SorbentPumpPCB = "HHBslot1")
  df <- dplyr::mutate(df, D.SorbentPumpPCB = .data$C.SorbentPumpPCB)

  # Identify calibrating sampling channels included in HHB
  channels <- colnames(dplyr::select(header, dplyr::contains("Cal")))
  channels <- sapply(X = channels, FUN = strsplit, "\\.")
  channels <- lapply(channels, "[[", 1)
  channels <- unlist(channels, use.names = F)
  channels <- unique(channels)

  # Separate sample data associated with each pumping channel
  df_a <- dplyr::select(df, dplyr::any_of(meta_vars), dplyr::starts_with("A."))
  df_a <- dplyr::mutate(df_a, Channel = "A", ChannelType = "Filter")

  df_b <- dplyr::select(df, dplyr::any_of(meta_vars), dplyr::starts_with("B."))
  df_b <- dplyr::mutate(df_b, Channel = "B", ChannelType = "Filter")

  df_c <- dplyr::select(df, dplyr::any_of(meta_vars), dplyr::starts_with("C."))
  df_c <- dplyr::mutate(df_c, Channel = "C", ChannelType = "Sorbent")

  df_d <- dplyr::select(df, dplyr::any_of(meta_vars), dplyr::starts_with("D."))
  df_d <- dplyr::mutate(df_d, Channel = "D", ChannelType = "Sorbent")

  # Harmonize column names
  colnames(df_a) <- gsub("^A.Filter",  "", colnames(df_a))
  colnames(df_b) <- gsub("^B.Filter",  "", colnames(df_b))
  colnames(df_c) <- gsub("^C.Sorbent", "", colnames(df_c))
  colnames(df_d) <- gsub("^D.Sorbent", "", colnames(df_d))

  # Recombine data from each channel
  # Rename variables that also apply to full HHB sample
  df <- dplyr::bind_rows(df_a, df_b, df_c, df_d)
  df <- dplyr::rename(df, dplyr::any_of(c(
                            SampleProgrammedStartTime = "ProgrammedStartTime",
                            SampleProgrammedRuntime   = "ProgrammedRuntime",
                            SampleStartDateTimeUTC    = "StartDateTimeUTC",
                            SampleEndDateTimeUTC      = "EndDateTimeUTC")))
  df <- dplyr::relocate(df, c("Channel","ChannelType"), .after = "SampleName")
  df <- dplyr::filter(df, .data$Channel %in% channels)

  return(df)
}

#'Convert sample data from a Home Health Box log file header to long format.
#'
#' @description
#' `format_hhb_sample_log()` Selects the filter and sorbent sample data from a
#' HHB v2 sample log and converts it to a long format to facilitate evaluation
#' of sample flow rates over time.
#'
#' @param log A HHB v2 sample log data frame returned by [astr::read_ast_log()] or [astr::format_hhb_log()]
#'
#' @return A data frame with a four rows of HHB v2 sample log data for each row in `log`.
#'
#' @details
#' The returned data frame will start with the following columns:
#'
#' \itemize{
#'    \item HHBserial: A string indicating the serial number of the Home Health Box
#'    \item LogFileName: A string indicating log filename as saved on the microSD card in the HHB
#'    \item SampleName: A string indicating the user-supplied sample name
#'    \item Channel: A string indicating the pump channel ("A", "B", "C", or "D")
#'    \item ChannelType: A string indicating the sample type ("Filter" or "Sorbent")
#'    \item SampleTime: A difftime object indicating the time elapsed since the sample started
#'    \item DateTimeUTC: A POSIXct object indicating the timestamp in Coordinated Universal Time
#'    \item DateTimeLocal: A POSIXct object indicating the timestamp in the user-specified local time zone (if present in `log`)
#' }
#'
#' After the columns listed above, data from all columns in the sample log
#' that start with "1.", "A.", "B.", "C.", or "D." will appear.  In the returned
#' data frame, the "1.", "A.", "B.", "C.", or "D." prefix will be removed from
#' each parameter name. Additionally, some names will be updated as shown in the
#' table below.
#'
#' \tabular{ll}{
#'    \strong{Name in sample log}  \tab \strong{Name in returned data frame} \cr
#'    1.BMP390_Press      \tab ExtPress \cr
#'    1.BMP390_Temp       \tab ExtTemp  \cr
#'    D.BMP581_Press      \tab IntPress \cr
#'    D.BMP581_Temp       \tab IntTemp  \cr
#'    D.TotalSorbentVol   \tab TotalCartridgeVol \cr
#'    D.SampledSorbentVol \tab SampledCartridgeVol \cr
#'    C.BMP581_Press      \tab IntPress \cr
#'    C.BMP581_Temp       \tab IntTemp  \cr
#'    C.TotalSorbentVol   \tab TotalCartridgeVol \cr
#'    C.SampledSorbentVol \tab SampledCartridgeVol \cr
#'    A.BMP581Int_Press   \tab IntPress \cr
#'    A.BMP581Int_Temp    \tab IntTemp  \cr
#'    A.BMP581Ext_Press   \tab ExtPress \cr
#'    A.BMP581Ext_Temp    \tab ExtTemp  \cr
#'    B.BMP581Int_Press   \tab IntPress \cr
#'    B.BMP581Int_Temp    \tab IntTemp  \cr
#'    B.BMP581Ext_Press   \tab ExtPress \cr
#'    B.BMP581Ext_Temp    \tab ExtTemp  \cr
#'  }
#'
#'  In the returned data frame, "ExtPress" and "ExtTemp" refer to pressures and
#'  temperatures measured outside of each pump manifold, respectively.
#'  "IntPress" and "IntTemp" refer to pressures and temperatures measured inside
#'  of each pump manifold. All pressures are in units of Pascals and all
#'  temperatures are in units of degrees Celsius.
#'
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' hhb_filename <- 'HHB00087_LOG_2025-06-03T20_55UTC.csv'
#' hhb_file <- system.file("extdata", hhb_filename, package = "astr", mustWork = TRUE)
#' hhb_log <- read_ast_log(hhb_file)
#' hhb_sample_logs <- format_hhb_sample_log(hhb_log)

format_hhb_sample_log = function(log) {

  # Sample metadata variables to retain
  meta_vars <- c("HHBserial","LogFileName","SampleName","SampleTime",
                 "DateTimeUTC","DateTimeLocal")

  # Separate sample data associated with each pumping channel
  df_a <- dplyr::select(log, dplyr::any_of(meta_vars), dplyr::starts_with("A."))
  df_a <- dplyr::mutate(df_a, Channel = "A", ChannelType = "Filter")

  df_b <- dplyr::select(log, dplyr::any_of(meta_vars), dplyr::starts_with("B."))
  df_b <- dplyr::mutate(df_b, Channel = "B", ChannelType = "Filter")

  df_c <- dplyr::select(log, dplyr::any_of(meta_vars), dplyr::starts_with("C."),
                        dplyr::starts_with("1."))
  df_c <- dplyr::mutate(df_c, Channel = "C", ChannelType = "Sorbent")

  df_d <- dplyr::select(log, dplyr::any_of(meta_vars), dplyr::starts_with("D."),
                        dplyr::starts_with("1."))
  df_d <- dplyr::mutate(df_d, Channel = "D", ChannelType = "Sorbent")

  # Harmonize column names
  colnames(df_a) <- gsub("^A\\.",  "", colnames(df_a))
  colnames(df_a) <- gsub("BMP581", "", colnames(df_a))
  colnames(df_a) <- gsub("_",      "", colnames(df_a))

  colnames(df_b) <- gsub("^B\\.",  "", colnames(df_b))
  colnames(df_b) <- gsub("BMP581", "", colnames(df_b))
  colnames(df_b) <- gsub("_",      "", colnames(df_b))

  colnames(df_c) <- gsub("^C\\.",   "", colnames(df_c))
  colnames(df_c) <- gsub("^1\\.",   "", colnames(df_c))
  colnames(df_c) <- gsub("Sorbent", "Cartridge", colnames(df_c))
  colnames(df_c) <- gsub("BMP390_", "Ext", colnames(df_c))
  colnames(df_c) <- gsub("BMP581_", "Int", colnames(df_c))

  colnames(df_d) <- gsub("^D\\.",   "", colnames(df_d))
  colnames(df_d) <- gsub("^1\\.",   "", colnames(df_d))
  colnames(df_d) <- gsub("Sorbent", "Cartridge", colnames(df_d))
  colnames(df_d) <- gsub("BMP390_", "Ext", colnames(df_d))
  colnames(df_d) <- gsub("BMP581_", "Int", colnames(df_d))

  meta_vars <- c(meta_vars, "Channel", "ChannelType")

  # Get the number of non-metadata columns in each data frame to determine
  # which pumps channels were actually active during the sample
  n_cols <- c("A" = ncol(dplyr::select(df_a, -dplyr::any_of(meta_vars))),
              "B" = ncol(dplyr::select(df_b, -dplyr::any_of(meta_vars))),
              "C" = ncol(dplyr::select(df_c, -dplyr::any_of(meta_vars))),
              "D" = ncol(dplyr::select(df_d, -dplyr::any_of(meta_vars))))

  # Recombine data from each channel
  # Remove channels that were not active during the sample
  df <- dplyr::bind_rows(df_a, df_b, df_c, df_d)
  df <- dplyr::relocate(df, c("Channel","ChannelType"), .after="SampleName")
  df <- dplyr::filter(df, .data$Channel %in% names(n_cols[n_cols > 0]))

  return(df)
}

#'Convert Alphasense electrochemical sensor data from a Home Health Box sample log to long format.
#'
#' @description
#' `format_hhb_sensors()` Selects the Alphasense electrochemical gas sensor data
#' from a HHB v2 sample log and converts it to a long format.
#'
#' @param log A HHB v2 log data frame returned by [astr::read_ast_log()] or [astr::format_hhb_log()]
#' @param header A HHB v2 header data frame returned by [astr::read_ast_header()] or [astr::format_hhb_header()]. Each sample log present in `log` must also be present in `header`.
#' @param temp A character string containing the name of the temperature variable to be used to apply any temperature corrections to the Alphasense electrochemical sensor data.
#' The default is "G.SCD30_Temp".
#'
#' @return A data frame of with one row for each timestamp, Alphasense electrochemical sensor, and algorithm in `log`.
#'
#' @details
#' The data frame returned by this function will include the following columns:
#' \tabular{llll}{
#'    \strong{Column name} \tab \strong{Class}  \tab \strong{Units} \tab \strong{Description} \cr
#'    HHBserial     \tab character \tab -      \tab Serial identifier for the Home Health Box \cr
#'    LogFileName   \tab character \tab -      \tab Log filename \cr
#'    Position      \tab numeric   \tab -      \tab Position in which the sensor was installed in the gas sensor housing (1, 2, 3, or 4) \cr
#'    ID            \tab character \tab -      \tab Serial identifier for the electrochemical (EC) sensor \cr
#'    Type          \tab character \tab -      \tab Model of the EC sensor \cr
#'    ISB_Gain      \tab numeric   \tab mV/nA  \tab Gain on the Individual Sensor Board (ISB) to which the EC sensor was mounted \cr
#'    Sensitivity   \tab numeric   \tab nA/ppm \tab Working electrode (WE) sensitivity to the pollutant of interest (a calibration constant for the EC sensor) \cr
#'    WEt           \tab numeric   \tab mV     \tab Total WE zero offset (a calibration constant for the EC sensor) \cr
#'    AEt           \tab numeric   \tab mV     \tab Total auxiliary electrode (AE) zero offset (a calibration constant for the EC sensor)  \cr
#'    WEe           \tab numeric   \tab mV     \tab WE electronic offset on the ISB (a calibration constant for the EC sensor) \cr
#'    AEe           \tab numeric   \tab mV     \tab AE electronic offset on the ISB (a calibration constant for the EC sensor) \cr
#'    SampleTime    \tab difftime  \tab s      \tab Time elapsed since the sample started \cr
#'    DateTimeUTC   \tab POSIXct   \tab -      \tab Timestamp in Coordinated Universal Time \cr
#'    G.SCD30_Temp  \tab numeric   \tab C      \tab Temperature measured inside the gas sensor housing by the Sensirion SCD30 sensor \cr
#'    WE            \tab numeric   \tab V      \tab Working electrode voltage \cr
#'    AUX           \tab numeric   \tab V      \tab Auxiliary electrode voltage \cr
#'    Alg           \tab numeric   \tab -      \tab Algorithm used to calculate ppb estimate \cr
#'    ppb           \tab numeric   \tab ppb    \tab Estimated mixing ratio of pollutant estimated using the specified algorithm \cr
#' }
#'
#' If the `temp` argument was `NA` or was specified as either: (a) a column that
#' did not contain temperature data or (b) a column was not present in `log`,
#' the returned data frame will not contain the "G.SCD30_Temp" column nor any
#' other column of temperature data.
#'
#' If the `temp` argument was specified as something other than "G.SCD30_Temp,"
#' that column of temperature data will appear in the data frame in the place
#' of the "G.SCD30_Temp" column listed in the table above.
#'
#' It is recommended that you specify `temp` as a temperature that was measured
#' inside the gas sensor housing, for example: "G.SCD30_Temp", "G.BMP581_Temp",
#' or "G.SFA30_Temp".
#'
#' The temperature variable "SEN55_Temp" is measured inside then Sensirion SEN55
#' optical particulate matter sensor and, if this sensor was operated in
#' RH/T/Gas/PMS mode, in which the fan inside the SEN55 draws ambient air
#' through the SEN55 sensor continuously, the "SEN55_Temp" variable might be the
#' temperature in `log` that most closely reflects the ambient temperature.
#'
#' The temperature variable "M.BMP581_Temp" is measured on the surface of the
#' main Home Health Box PCB by a sensor that is vented to the inside of the
#' sorbent media compartment.
#'
#' The following temperature variables are measured by sensors that outside the
#' gas sensor housing and mounted on circuit boards that are enclosed in the
#' main Home Health Box housing. We recommend using these temperatures for
#' interpretation of the electrochemical gas sensor data only as a last resort
#' if none of the other temperature measurements described above are available:
#' Battery_Temp","M.BMP581_Temp","SEN55_Temp","1.BMP390_Temp","D.BMP581_Temp",
#' "C.BMP581_Temp","A.BMP581Int_Temp","A.BMP581Ext_Temp","B.BMP581Int_Temp",
#' "B.BMP581Ext_Temp".
#'
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' hhb_filename <- 'HHB00087_LOG_2025-06-03T20_55UTC.csv'
#' hhb_file <- system.file("extdata", hhb_filename, package = "astr", mustWork = TRUE)
#' hhb_log <- read_ast_log(hhb_file)
#' hhb_header <- read_ast_header(hhb_file)
#' hhb_ec <- format_hhb_sensors(hhb_log, hhb_header)

format_hhb_sensors = function(log, header, temp="G.SCD30_Temp") {

  # Check that the specified 'temp' variable is the name of a HHB log variable
  if (!(temp %in% c("Battery_Temp","M.BMP581_Temp","SEN55_Temp","1.BMP390_Temp",
                    "D.BMP581_Temp","C.BMP581_Temp","A.BMP581Int_Temp",
                    "A.BMP581Ext_Temp","B.BMP581Int_Temp","B.BMP581Ext_Temp",
                    "G.BMP581_Temp","G.SCD30_Temp","G.SFA30_Temp"))){
    warning("Invalid variable name specified for temp argument. No temperature data will be included in the returned data frame.")
  }else{
    # Check that the specified 'temp' variable is present in the log
    if (!(temp %in% colnames(log))){
      warning("The column name specified as the temp argument is not present in the supplied log. No temperature data will be included in the returned data frame.")
    }
  }

  # Check whether the specified temperature variable was measured inside the
  # gas sensor housing or by the SEN55.
  if (temp %in% c("Battery_Temp","M.BMP581_Temp","SEN55_Temp","1.BMP390_Temp",
                  "D.BMP581_Temp","C.BMP581_Temp","A.BMP581Int_Temp",
                  "A.BMP581Ext_Temp","B.BMP581Int_Temp","B.BMP581Ext_Temp")){
    warning("The specified temperature was not measured inside the gas sensor housing. It is recommended that you specify a temperature measured inside the gas sensor housing: 'G.SCD30_Temp', 'G.BMP581_Temp', or 'G.SFA30_Temp'.")
  }

  # Select header data
  df_h <- dplyr::select(header, "LogFileName", dplyr::starts_with("G.Alphasense"))
  df_h <- dplyr::select(df_h, -dplyr::ends_with("Runtime"))

  # Separate header data associated with each sensor
  df1 <- dplyr::select(df_h, "LogFileName", dplyr::contains("1_"))
  df1 <- dplyr::mutate(df1, Position = 1)

  df2 <- dplyr::select(df_h, "LogFileName", dplyr::contains("2_"))
  df2 <- dplyr::mutate(df2, Position = 2)

  df3 <- dplyr::select(df_h, "LogFileName", dplyr::contains("3_"))
  df3 <- dplyr::mutate(df3, Position = 3)

  df4 <- dplyr::select(df_h, "LogFileName", dplyr::contains("4_"))
  df4 <- dplyr::mutate(df4, Position = 4)

  # Harmonize column names
  colnames(df1) <- gsub("^G.Alphasense[1-4]_",  "", colnames(df1))
  colnames(df2) <- gsub("^G.Alphasense[1-4]_",  "", colnames(df2))
  colnames(df3) <- gsub("^G.Alphasense[1-4]_",  "", colnames(df3))
  colnames(df4) <- gsub("^G.Alphasense[1-4]_",  "", colnames(df4))

  # Recombine data from each channel
  df_h <- dplyr::bind_rows(df1, df2, df3, df4)
  df_h <- dplyr::relocate(df_h, "Position", .before = "ID")

  # Sample log variables to retain in addition to electrochemical sensor vars
  meta_vars <- c("HHBserial", "LogFileName", "SampleTime", "DateTimeUTC", temp)

  names_we <- dplyr::cross_join(data.frame(elec = c("WE","AUX")),
                                data.frame(Position = c(1,2,3,4)))
  names_we <- dplyr::mutate(names_we,
                            name = paste0("G.", .data$elec, .data$Position))

  # Select working and auxiliary electrode data
  df_we <- dplyr::select(log, dplyr::any_of(meta_vars),
                              dplyr::starts_with("G.WE"),
                              dplyr::starts_with("G.AUX"))
  df_we <- tidyr::pivot_longer(df_we, cols = -dplyr::any_of(meta_vars))
  df_we <- dplyr::left_join(df_we, names_we, by=c("name"))
  df_we <- dplyr::select(df_we, -"name")
  df_we <- tidyr::pivot_wider(df_we, names_from = "elec", values_from = "value")

  # Select pollutant mixing ratios
  df <- dplyr::select(log, dplyr::any_of(meta_vars),
                           dplyr::contains("Algorithm"))

  if(ncol(df) > length(meta_vars)){
    df <- tidyr::pivot_longer(df, cols = -dplyr::any_of(meta_vars),
                                  values_to = "ppb")
    df <- dplyr::mutate(df,
                        name = gsub("^G.Alphasense", "", .data$name),
                        name  = strsplit(.data$name, "_"),
                        Position = sapply(.data$name, "[[", 1),
                        Position = as.numeric(.data$Position),
                        Alg = sapply(.data$name, "[[", 2),
                        Alg = gsub("^Algorithm", "", .data$Alg),
                        Alg = as.numeric(.data$Alg))
    df <- dplyr::select(df, -"name")
    df <- dplyr::relocate(df, "ppb", .after = "Alg")
    df <- dplyr::group_by(df, .data$LogFileName, .data$Position, .data$Alg)
    df <- dplyr::filter(df, !all(is.na(.data$ppb)))
    df <- dplyr::ungroup(df)
  }

  df <- dplyr::left_join(df_we, df,
                         by=c(meta_vars[meta_vars %in% colnames(df_we)],
                              c("Position")[c("Position") %in% colnames(df)]))
  df <- dplyr::left_join(df, df_h, by=c("LogFileName","Position"))
  df <- dplyr::relocate(df, "Position", .after = "LogFileName")
  df <- dplyr::relocate(df,
                        dplyr::any_of(c("ID","Type","ISB_Gain","Sensitivity",
                                        "WEt","WEe","AEt","AEe")),
                        .after = "Position")

  missing <- unique(log$LogFileName)[!(unique(log$LogFileName) %in%
                                                          header$LogFileName)]

  # Check whether there are any files in log that are not in header
  if (length(missing) > 0){

    warn_text <- paste(c("The following log files in the log data frame are missing from the header data frame:",
                         missing,
                         "Sensor IDs and calibration constants for these samples will not appear in the data frame returned by the function."), collapse = "\n")

    warning(warn_text)
  }

  rm(df_h, df_we)

  return(df)
}
