#'Create sample summary data frame from an Access Sensor Technologies (AST)
#'UPAS v2 or UPAS v2.1 PLUS header data frame.
#'
#' @description
#' `shiny_sample_summary` modifies an Access Sensor Technologies UPAS v2 or UPAS v2.1 PLUS
#' header data frame
#' for use with the Sample Summary tab in the online shinyAST data analysis app.
#' This function sets the proper data type for each variable,
#' appends a sample PASS/FAIL flag column,
#' and adds units to the applicable variable names.
#'
#' @param df_h A header data frame returned by the [read_ast_header] function
#' with the argument `update_names = TRUE`.
#' @param fract_units Boolean to specify if units should be fractional (L min^-1 vs L/min).
#'
#' @return A modified single row header data frame with select sample summary data.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' multiple_upas_headers <- list.files(path = "inst/extdata", pattern="^PS.*.txt$",
#' full.names = TRUE) %>%
#'     lapply(read_ast_header, update_names=TRUE) %>%
#'     dplyr::bind_rows()
#'
#' sample_summary <- shiny_sample_summary(multiple_upas_headers, fract_units=TRUE)

shiny_sample_summary = function(df_h, fract_units=FALSE) {

  df_h <- astr::shiny_success_flag(df_h)

  df_h <- dplyr::select(df_h,
                        dplyr::any_of(c('ASTSampler', 'UPASserial', 'SampleName', 'CartridgeID',
                                        'SampleSuccess', 'OverallDuration',
                                        'PumpingDuration', 'PumpingFlowRateAverage',
                                        'OverallFlowRateAverage', 'SampledVolume',
                                        'ShutdownReason'))) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c('OverallDuration',
                                                'PumpingDuration',
                                                'PumpingFlowRateAverage',
                                                'OverallFlowRateAverage',
                                                'SampledVolume')),
                                \(x) as.numeric(x)),
                  dplyr::across(dplyr::any_of(c('ASTSampler', 'UPASserial', 'SampleName')),
                                \(x) as.factor(x)))

    df_h <- astr::shiny_header(df_h, fract_units=fract_units)

  return(df_h)
}

#'Create sample settings data frame from an Access Sensor Technologies (AST)
#'UPAS v2 or UPAS v2.1 PLUS header data frame.
#'
#' @description
#' `shiny_sample_settings` modifies an Access Sensor Technologies UPAS v2 or UPAS v2.1 PLUS
#' header data frame
#' for use with the Sample Settings tab in the online shinyAST data analysis app.
#' This function sets the proper data type for each variable,
#' appends a sample PASS/FAIL flag column,
#' and adds units to the applicable variable names.
#'
#' @inheritParams shiny_sample_summary
#'
#' @return A modified single row header data frame with select sample settings data.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' multiple_upas_headers <- list.files(path = "inst/extdata", pattern="^PS.*.txt$",
#' full.names = TRUE) %>%
#'     lapply(read_ast_header, update_names=TRUE) %>%
#'     dplyr::bind_rows()
#'
#' sample_settings <- shiny_sample_settings(multiple_upas_headers, fract_units=TRUE)

shiny_sample_settings = function(df_h, fract_units=FALSE) {

  df_h <- astr::shiny_success_flag(df_h)

  df_h <- dplyr::select(df_h,
                        dplyr::any_of(c('ASTSampler', 'UPASserial', 'SampleName', 'CartridgeID',
                                        'StartOnNextPowerUp','ProgrammedStartDelay',
                                        'ProgrammedStartTime','ProgrammedRuntime',
                                        'SizeSelectiveInlet','VolumetricFlowRateSet',
                                        'FlowRateSetpoint','DutyCycle','FlowDutyCycle','GPSEnabled',
                                        'PMSensorOperation','RTGasSampleState','LogInterval',
                                        'PowerSaveMode','AppVersion','SampleSuccess'))) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c('ASTSampler', 'UPASserial', 'SampleName')),
                                \(x) as.factor(x)))

    df_h <- astr::shiny_header(df_h, fract_units=fract_units)

  return(df_h)
}

#'Create sample operation data frame from an Access Sensor Technologies (AST)
#'UPAS v2 or UPAS v2.1 PLUS header data frame.
#'
#' @description
#' `shiny_sample_operation` modifies an Access Sensor Technologies UPAS v2 or UPAS v2.1 PLUS
#' header data frame
#' for use with the UPAS Operation tab in the online shinyAST data analysis app.
#' This function sets the proper data type for each variable,
#' appends a sample PASS/FAIL flag column,
#' and adds units to the applicable variable names.
#'
#' @inheritParams shiny_sample_summary
#'
#' @return A modified single row header data frame with select sample operation data.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' multiple_upas_headers <- list.files(path = "inst/extdata", pattern="^PS.*.txt$",
#' full.names = TRUE) %>%
#'     lapply(read_ast_header, update_names=TRUE) %>%
#'     dplyr::bind_rows()
#'
#' sample_operation <- shiny_sample_operation(multiple_upas_headers, fract_units=TRUE)

shiny_sample_operation = function(df_h, fract_units=FALSE) {

  df_h <- astr::shiny_success_flag(df_h)

  df_h <- dplyr::select(df_h,
                        dplyr::any_of(c('ASTSampler','UPASserial', 'SampleName',
                                        'CartridgeID','StartDateTimeUTC','EndDateTimeUTC',
                                        'StartBatteryVoltage','EndBatteryVoltage',
                                        'StartBatteryCharge','EndBatteryCharge','GPSUTCOffset',
                                        'FirmwareRev','ShutdownReason','SampleSuccess'))) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c('ASTSampler', 'UPASserial', 'SampleName')),
                                \(x) as.factor(x)))

    df_h <- astr::shiny_header(df_h, fract_units=fract_units)

  return(df_h)
}

#'Rename and format Access Sensor Technologies (AST) UPAS v2 or UPAS v2.1 PLUS
#'header data frame columns and add units
#'for the online shinyAST app
#'
#' @description
#' `shiny_header` modifies an Access Sensor Technologies UPAS v2 or UPAS v2.1 PLUS
#' header data frame
#' for use with the online shinyAST data analysis app.
#' This function adds units to the applicable variable names.
#'
#' @inheritParams shiny_sample_summary
#'
#' @return A modified single row header data frame with column names and units
#' for the online shinyAST app.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' multiple_upas_headers <- list.files(path = "inst/extdata", pattern="^PS.*.txt$",
#'                                   full.names = TRUE) %>%
#'         lapply(read_ast_header, update_names = TRUE) %>%
#'         dplyr::bind_rows()
#'
#' upas_shiny_header <- shiny_header(multiple_upas_headers)

shiny_header = function(df_h, fract_units = FALSE) {

  if(("ProgrammedRuntime") %in% colnames(df_h)){
    df_h <- df_h %>%
      dplyr::mutate(ProgrammedRuntime =
                      ifelse(.data$ASTSampler=="UPAS_v2_0",
                             # convert to hours for UPASv2
                             ifelse(.data$ProgrammedRuntime==360000000, "indefinite", .data$ProgrammedRuntime/3600),
                             # Since ProgrammedRuntime is numeric, indefinite values for UPASv2x are normally displayed as NA,
                             # but for the shinyAST app these should be displayed as "indefinite"
                             ifelse(is.na(.data$ProgrammedRuntime), "indefinite", .data$ProgrammedRuntime)),
                    # Make sure this column is a character to prevent row binding issues when reading multiple files
                    ProgrammedRuntime = as.character(.data$ProgrammedRuntime))
  }

  df_h <- dplyr::rename(df_h, dplyr::any_of(
    c("LifetimeSampleCount (#)"           = "LifetimeSampleCount",
      "LifetimeSampleRuntime (Hr)"        = "LifetimeSampleRuntime",
      "LifetimeBatteryRuntime (Hr)"       = "LifetimeBatteryRuntime",
      "LifetimeSamplePumptime (Hr)"       = "LifetimeSamplePumptime",
      "LifetimePMSensorFanStartCount (#)" = "LifetimePMSensorFanStartCount",
      "LifetimePMSensorFanHours (Hr)"     = "LifetimePMSensorFanHours",
      "LifetimePMSensorPMMC (mg)"         = "LifetimePMSensorPMMC",
      "LifetimeCO2SensorHours (Hr)"       = "LifetimeCO2SensorHours",
      "LifetimeVOCSensorHours (Hr)"       = "LifetimeVOCSensorHours",

      # SETUP SUMMARY
      "GPSUTCOffset (Hr)"                 = "GPSUTCOffset",
      "StartOnNextPowerUp (0=no 1=yes)"   = "StartOnNextPowerUp",
      "ProgrammedStartTime (sec since 1/1/1970)" = "ProgrammedStartTime",
      "ProgrammedRuntime (Hr)"            = "ProgrammedRuntime",
      "FlowRateSetpoint (L min^-1)"       = "FlowRateSetpoint",
      "FlowOffset (%)"                    = "FlowOffset",
      "FlowDutyCycle (%)"                 = "FlowDutyCycle",
      "DutyCycleWindow (s)"               = "DutyCycleWindow",
      "LogInterval (s)"                   = "LogInterval",

      # SAMPLE SUMMARY
      # "StartDateTimeUTC (YYYY-MM-DDTHH:MM:SS)"   = "StartDateTimeUTC",
      # "EndDateTimeUTC (YYYY-MM-DDTHH:MM:SS)"     = "EndDateTimeUTC",
      # "StartDateTimeLocal (YYYY-MM-DDTHH:MM:SS)" = "StartDateTimeLocal",
      # "EndDateTimeLocal (YYYY-MM-DDTHH:MM:SS)"   = "EndDateTimeLocal",
      "FlowCheckMeterReadingPreSample (L min^-1)"  = "FlowCheckMeterReadingPreSample",
      "FlowCheckMeterReadingPostSample (L min^-1)" = "FlowCheckMeterReadingPostSample",
      "OverallDuration (Hr)"              = "OverallDuration",
      "PumpingDuration (Hr)"              = "PumpingDuration",
      "OverallFlowRateAverage (L min^-1)" = "OverallFlowRateAverage",
      "PumpingFlowRateAverage (L min^-1)" = "PumpingFlowRateAverage",
      "SampledVolume (L)"                 = "SampledVolume",
      "PercentTimeWorn (%)"               = "PercentTimeWorn",
      "StartBatteryCharge (%)"            = "StartBatteryCharge",
      "EndBatteryCharge (%)"              = "EndBatteryCharge",
      "StartBatteryVoltage (V)"           = "StartBatteryVoltage",
      "EndBatteryVoltage (V)"             = "EndBatteryVoltage",

      # CO2 SENSOR CALIBRATION
      # "CO2CalDate (YYYY-MM-DDTHH:MM:SS)" = "CO2CalDate",
      "CO2CalTarget (ppm)"                = "CO2CalTarget",
      "CO2CalOffset (ppm)"                = "CO2CalOffset",

      # MASS FLOW SENSOR CALIBRATION
      # "MFSCalDate (YYYY-MM-DDTHH:MM:SS)" = "MFSCalDate",
      "MFSCalVoutBlocked (V)"     = "MFSCalVoutBlocked",
      "MFSCalVoutMin (V)"         = "MFSCalVoutMin",
      "MFSCalVoutMax (V)"         = "MFSCalVoutMax",
      "MFSCalMFBlocked (g min^-1)"= "MFSCalMFBlocked",
      "MFSCalMFMin (g min^-1)"    = "MFSCalMFMin",
      "MFSCalMFMax (g min^-1)"    = "MFSCalMFMax",
      "MFSCalPumpVBoostMin (V)"   = "MFSCalPumpVBoostMin",
      "MFSCalPumpVBoostMax (V)"   = "MFSCalPumpVBoostMax",
      "MFSCalPDeadhead (Pa)"      = "MFSCalPDeadhead",
      "MF4 (coefficient)"         = "MF4",
      "MF3 (coefficient)"         = "MF3",
      "MF2 (coefficient)"         = "MF2",
      "MF1 (coefficient)"         = "MF1",
      "MF0 (coefficient)"         = "MF0",

      #UPASv2 Specific when update_names = FALSE
      "ProgrammedStartDelay (s)"      = "ProgrammedStartDelay",
      "VolumetricFlowRate (L min^-1)" = "VolumetricFlowRate",
      "DutyCycle (%)"                 = "DutyCycle",
      "SampledRuntime (Hr)"           = "SampledRuntime",
      "LoggedRuntime (Hr)"            = "LoggedRuntime",
      "AverageVolumetricFlowRate (L min^-1)" = "AverageVolumetricFlowRate"
      )))

  if(fract_units) {
    colnames(df_h) <- shiny_units(colnames(df_h))
  }

  return(df_h)
}

#'Format Access Sensor Technologies (AST) UPAS v2 or UPAS v2.1 PLUS
#'log data frame columns
#'for the online shinyAST app
#'
#' @description
#' `shiny_log` modifies an Access Sensor Technologies UPAS v2 or UPAS v2.1 PLUS
#' log data frame
#' for use with the online shinyAST data analysis app.
#' This function removes and reorders some columns to create a better user interface.
#'
#' @param df A log data frame returned by the [read_ast_log] function
#' with the argument `update_names = TRUE`.
#'
#' @return A modified log data frame with extraneous variables removed,
#' more used variables brought to the front of the data frame,
#' and SampleTime formatted to hours for the online shinyAST app plots.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' multiple_upas_logs <- list.files(path = "inst/extdata", pattern="^PS.*.txt$",
#'     full.names = TRUE) %>%
#'     lapply(read_ast_log, update_names=TRUE) %>%
#'     dplyr::bind_rows()
#'
#' upas_shiny_log <- shiny_log(multiple_upas_logs)

shiny_log = function(df) {

  df <- df %>%
    dplyr::select(!dplyr::any_of(c("tz_value",
                   "LocalTZ",
                   "UnixTimeMCU",
                   "ASTSampler",
                   "SampleName",
                   "CartridgeID",
                   "StartDateTimeUTC",
                   "LogFileMode",
                   "LogFilename",
                   "IAQStabStat",
                   "IAQRunIn",
                   "IAQRes",
                   "IAQ",
                   "IAQAcc",
                   "StaticIAQ",
                   "StaticIAQAcc",
                   "CO2e",
                   "CO2eAcc",
                   "bVOC",
                   "bVOCAcc",
                   "gasComp",
                   "gasCompAcc",
                   "gasPerc",
                   "gasPercAcc",
                   "UserTZ"))) %>%
    dplyr::relocate(dplyr::any_of(c("SampleTime",
                    "DateTimeUTC",
                    "DateTimeLocal",
                    "PumpingFlowRate",
                    "CO2",
                    "VOCRaw",
                    "NOXRaw",
                    "PM1MC",
                    "PM2_5MC",
                    "PM4MC",
                    "PM10MC",
                    "AtmoT",
                    "AtmoP",
                    "AtmoRH",
                    "AtmoDensity",
                    "AtmoAlt")))

  if("SampleTime" %in% colnames(df)){
    df <- df %>% dplyr::mutate(SampleTime = as.numeric(.data$SampleTime, units="hours"))
  }

  return(df)
}

#'Rename an Access Sensor Technologies (AST) UPAS v2 or UPAS v2.1 PLUS
#'log file single column name and add units for the online shinyAST app
#'
#' @param col_name A UPAS v2 or UPAS v2.1 PLUS log column name from a data frame
#' returned by the [read_ast_log] function
#' with the argument `update_names = TRUE`.
#' @param fract_units Boolean to specify if units should be fractional (L min^-1 vs L/min).
#'
#' @return A modified column name for plot axis labels in the online shinyAST app.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2x_filename <- 'PSP00270_LOG_2024-06-25T21_37_48UTC_GPS-in-out______----------.txt'
#' upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
#' upasv2x_log <- read_ast_log(upasv2x_file, update_names=TRUE) %>%
#' shiny_log()
#'
#' upasv2x_clm_names <- colnames(upasv2x_log)
#'
#' plotx_label <- shiny_axis("SampleTime", fract_units = FALSE)
#' ploty_label <- shiny_axis("PumpingFlowRate", fract_units = TRUE)

shiny_axis = function(col_name, fract_units = FALSE){

  df <- data.frame(SampleTime = c("Sample Time", "(Hr)"),
                  UnixTime = c("Unix Time", "(s)"),
                  UnixTimeMCU = c("Unix Time MCU", "(s)"),
                  DateTimeUTC = c("Date Time UTC", ""),
                  DateTimeLocal = c("Date Time Local", ""),
                  LocalTZ = c("Local Timezone", ""),
                  PumpingFlowRate = c("Pumping Flow Rate", "(L min^-1)"),
                  OverallFlowRate = c("Overall Flow Rate", "(L min^-1)"),
                  SampledVolume = c("Sampled Volume", "(L)"),
                  FilterDP = c("Filter Differential Pressure", "(Pa)"),
                  BatteryCharge = c("Battery Charge", "(%)"),
                  AtmoT = c("Atmospheric T", "(C)"),
                  AtmoP = c("Atmospheric P", "(hPa)"),
                  AtmoRH = c("Atmospheric RH", "(%RH)"),
                  AtmoDensity = c("Atmospheric Density", "(g L^-1)"),
                  AtmoAlt = c("Altitude Above Sea Level", "(m)"),
                  GPSQual = c("GPS Signal Quality", "(NMEA Standard)"),
                  GPSlat = c("GPS Latitude", "(decimalDegrees)"),
                  GPSlon = c("GPS Longitude", "(decimalDegrees)"),
                  GPSalt = c("GPS Altitude", "(m)"),
                  GPSsat = c("GPS Satellite Signals Received", "(#)"),
                  GPSspeed = c("GPS Measured Speed", "(m s^-1)"),
                  GPShDOP = c("GPS Horizontal Dilution of Precision", ""),

                  AccelX = c("X Acceleration", "(mg)"),
                  AccelXVar = c("X Acceleration Variance", "(mg)"),
                  AccelXMin = c("X Acceleration Minimum", "(mg)"),
                  AccelXMax = c("X Accleration Maximum", "(mg)"),
                  AccelY = c("Y Acceleration", "(mg)"),
                  AccelYVar = c("Y Acceleration Variance", "(mg)"),
                  AccelYMin = c("Y Acceleration Minimum", "(mg)"),
                  AccelYMax = c("Y Acceleration Maximum", "(mg)"),
                  AccelZ = c("Z Acceleration", "(mg)"),
                  AccelZVar = c("Z Acceleration Variance", "(mg)"),
                  AccelZMin = c("Z Acceleration Minimum", "(mg)"),
                  AccelZMax = c("Z Acceleration Maximum", "(mg)"),
                  RotX = c("Rotational X Acceleration", "(mdeg s^-1)"),
                  RotXVar = c("Rotational X Acceleration Variance", "(mdeg s^-1)"),
                  RotXMin = c("Rotational X Acceleration Minimum", "(mdeg s^-1)"),
                  RotXMax = c("Rotational X Acceleration Maximum", "(mdeg s^-1)"),
                  RotY = c("Rotational Y Acceleration", "(mdeg s^-1)"),
                  RotYVar = c("Rotational Y Acceleration Variance", "(mdeg s^-1)"),
                  RotYMin = c("Rotational Y Acceleration Minimum", "(mdeg s^-1)"),
                  RotYMax = c("Rotational Y Acceleration Maximum", "(mdeg s^-1)"),
                  RotZ = c("Rotational Z Acceleration", "(mdeg s^-1)"),
                  RotZVar = c("Rotational Z Acceleration Variance", "(mdeg s^-1)"),
                  RotZMin = c("Rotational Z Acceleration Minimum", "(mdeg s^-1)"),
                  RotZMax = c("Rotational Z Acceleration Maximum", "(mdeg s^-1)"),
                  Xup = c("Percentage of Time X Up", "(%)"),
                  XDown = c("Percentage of Time X Down", "(%)"),
                  Yup = c("Percentage of Time Y Up", "(%)"),
                  Ydown = c("Percentage of Time Y Down", "(%)"),
                  Zup = c("Percentage of Time Z Up", "(%)"),
                  Zdown = c("Percentage of Time Z Down", "(%)"),
                  StepCount = c("Step Count", "(#)"),
                  LUX = c("Light Sensor Illuminance", "(lux)"),
                  UVindex = c("UV Index", "(0 to 11+)"),
                  HighVisRaw = c("High-Wavelength Light Sensor Raw Output"),
                  LowVisRaw = c("Low Wavelength Light Sensor Raw Output"),
                  IRRaw = c("Infrared Light Sensor Raw Output"),
                  UVRaw = c("UV Light Sensor Raw Output"),
                  PMMeasCnt = c("PM Measurement Count", "(# per log interval)"),
                  PM1MC = c("PM1.0 Mass Concentration", "(ug m^-3)"),
                  PM1MCVar = c("PM1.0 Mass Concentration Variance", "(ug m^-3)"),
                  PM2_5MC = c("PM2.5 Mass Concentration", "(ug m^-3)"),
                  PM2_5MCVar = c("PM2.5 Mass Concentration Variance", "(ug m^-3)"),
                  PM4MC = c("PM4.0 Mass Concentration", "(ug m^-3)"),
                  PM4MCVar = c("PM4.0 Mass Concentration Variance", "(ug m^-3)"),
                  PM10MC = c("PM10 Mass Concentration", "(ug m^-3)"),
                  PM10MCVar = c("PM10 Mass Concentration Variance", "(ug m^-3)"),
                  PM0_5NC = c("PM0.5 Particle Concentration", "(# cm^-3)"),
                  PM0_5NCVar = c("PM0.5 Particle Concentration Variance", "(#cm^-3)"),
                  PM1NC = c("PM1.0 Particle Concentration", "(# cm^-3)"),
                  PM1NCVar = c("PM1.0 Particle Concentration Variance", "(# cm^-3)"),
                  PM2_5NC = c("PM2.5 Particle Concentration", "(# cm^-3)"),
                  PM2_5NCVar = c("PM2.5 Particle Concentration Variance", "(# cm^-3)"),
                  PM4NC = c("PM4.0 Particle Concentration", "(# cm^-3)"),
                  PM4NCVar = c("PM4.0 Particle Concentration Variance", "(# cm^-3)"),
                  PM10NC = c("PM10 Particle Concentration", "(# cm^-3)"),
                  PM10NCVar = c("PM10 Particle Concentration Variance", "(# cm^-3)"),
                  PMtypicalParticleSize = c("PM Typical Particle Size", "(um)"),
                  PMtypicalParticleSizeVar = c("PM Typical Particle Size Variance", "(um)"),
                  PM2_5SampledMass = c("PM2.5 Sampled Mass", "(ug)"),
                  PCB1T = c("PCB1 T", "(C)"),
                  PCB2T = c("PCB2 T", "(C)"),
                  FdpT = c("Downstream of Filter T", "(C)"),
                  AccelT = c("Accelerometer T", "(C)"),
                  PT100R = c("RTD Sensor Resistance", "(ohm)"),
                  PCB2P = c("PCB2 P", "(hPa)"),
                  PumpPow1 = c("Main Pump Power Level", "(integer)"),
                  PumpPow2 = c("Secondary Pump Power Level", "(integer)"),
                  PumpV = c("Pump Voltage Input", "(V)"),
                  MassFlow = c("Mass Flow Rate", "(g min^-1)"),
                  MFSVout = c("Mass Flow Sensor Output", "(V)"),
                  BFGenergy = c("Battery Fuel Gauge Output", "(16-bit integer)"),
                  BattVolt = c("Battery Voltage", "(V)"),
                  v3_3 = c("3.3V Rail Voltage", "(V)"),
                  v5 = c("5V Rail Voltage", "(V)"),
                  PumpsON = c("Pumps Operational State", "(bool)"),
                  Dead = c("Dead(bool)"),
                  BCS1 = c("Battery Charge Indicator 1", "(bool)"),
                  BCS2 = c("Battery Charge Indicator 2", "(bool)"),
                  BC_NPG = c("External Power Indicator", "(bool)"),
                  FLOWCTL = c("Time to Read/Write Log File Line", "(s)"),
                  GPSRT = c("Time to Read GPS Data", "(s)"),
                  SD_DATAW = c("Time to Write SD Card Log File Line", "(s)"),
                  SD_HEADW = c("Time to Update SD Card Log File Header", "(s)"),
                  TPumpsOFF = c("Time Pumps OFF Per Log Interval", "(s)"),
                  TPumpsON = c("Time Pumps ON Per Log Interval", "(s)"),

                  CO2 = c("CO2 Concentration", "(ppm)"),
                  SCDT = c("CO2 Sensor Temperature", "(C)"),
                  SCDRH = c("CO2 Sensor RH", "(%RH)"),
                  VOCRaw = c("VOC Sensor Raw Output", ""),
                  NOXRaw = c("NOx Sensor Raw Output", ""),
                  AccelComplianceCnt = c("Accelerometer Compliance Count", "(#)"),
                  AccelComplianceHrs = c("Accelerometer Compliance Time", "(hrs)"),
                  PMReadingErrorCnt = c("Accelerometer Compliance Time", "(hrs)"),
                  PMFanErrorCnt = c("SPS30 Fan Errors per Logging Period", "(#)"),
                  PMLaserErrorCnt = c("SPS30 Laser Errors per Logging Period", "(#)"),
                  PMFanSpeedWarn = c("SPS30 Fan Speed Errors per Logging Period", "(#)"),
                  row.names = c("axis_name", "unit")
                  )

    df_sel <- df %>%
      dplyr::select(dplyr::any_of(col_name))

    col_name <- paste(df_sel["axis_name",], df_sel["unit",], sep=" ")

    if(fract_units){col_name <- shiny_units(col_name)}

  return(col_name)
}

#'Reformat Access Sensor Technologies (AST) UPAS v2 or UPAS v2.1 PLUS
#'data frame units to be fractional for online shinyAST app
#'
#' @param col_names_vect Pass a vector of column names.
#' A UPAS v2 or UPAS v2.1 PLUS vector of column names from a data frame
#' returned by the [read_ast_header] or [read_ast_log] function
#' with the argument `update_names = TRUE`.
#'
#' @return A vector of column names with fractional units (L/min) instead of the
#' standard UPASv2 and UPASv2+ log file unit format (L min^-1).
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' multiple_upas_headers <- list.files(path = "inst/extdata", pattern="^PS.*.txt$",
#'                                   full.names = TRUE) %>%
#'         lapply(read_ast_header, update_names = TRUE) %>%
#'         dplyr::bind_rows()
#'
#' upas_shiny_header <- shiny_header(multiple_upas_headers)
#' upas_standard_units <- colnames(upas_shiny_header)
#' upas_shiny_units <- shiny_units(upas_standard_units)

shiny_units = function(col_names_vect){
  col_names_vect <- gsub("L min^-1", "L/min", fixed=TRUE,
          gsub("(g L^-1)", "(g/L)", fixed=TRUE,
          gsub("(m s^-1)", "(m/s)", fixed=TRUE,
          gsub("mdeg s^-1)", "(mdeg/s)", fixed=TRUE,
          gsub("(ug m^-3)", "(ug/m^3)", fixed=TRUE,
          gsub("(# cm^-3)", "(#/cm^3)", fixed=TRUE,
          gsub("(g min^-1)", "(g/min)", fixed=TRUE,
               x=col_names_vect)))))))

  return(col_names_vect)
}

#'Add a flag to a UPAS v2 or UPAS v2.1 PLUS header data frame to indicate PASS/FAIL for a sample.
#'
#' @param df_h A formatted data frame of UPAS v2 or UPAS v2.1 PLUS header data returned by the [read_ast_header] function.
#'
#' @return A data frame of UPAS v2 or UPAS v2.1 PLUS header data with an added column to indicate sample PASS/FAIL
#' based off the sample ShutdownMode.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' multiple_upas_headers <- list.files(path = "inst/extdata", pattern="^PS.*.txt$",
#'                                   full.names = TRUE) %>%
#'         lapply(read_ast_header, update_names = TRUE) %>%
#'         dplyr::bind_rows()
#'
#' upas_headers_flagged <- shiny_success_flag(multiple_upas_headers)

shiny_success_flag = function(df_h) {

  df_h <- dplyr::mutate(df_h,
                SampleSuccess = dplyr::case_when(
                                      df_h$ShutdownMode == 1  ~ "PASS",
                                      df_h$ShutdownMode == 3 ~ "PASS",
                                      .default = "FAIL"
                                            ))
  return(df_h)
}

#'Calculate 30 second averages for select variables
#'
#' @param df Pass a UPAS v2 or v2+ log data frame from 'read_ast_log' function.
#'
#' @return A modified data frame with 30 second mean data for select variables.
#' @export
#' @importFrom rlang .data
#' @importFrom stats var
#'
#' @examples
#' # upasv2x_30s_mean <- get_30s_mean(upasv2x_log)

get_30s_mean = function(df) {

  df_30s_mean <- df %>%
    dplyr::select(
      dplyr::any_of(c("UPASserial",
                      "DateTimeLocal",
                      "PM2_5MC",
                      "AccelX",
                      "AccelY",
                      "AccelZ",
                      "CO2",
                      "GPSlat",
                      "GPSlon")))%>%
    dplyr::mutate(datetime_local_rounded = lubridate::floor_date(.data$DateTimeLocal, "30 sec")) %>%
    dplyr::group_by(.data$UPASserial, .data$datetime_local_rounded)%>%
    #TODO make the mutate check if the variable exists so no errors are thrown
          # for past firmware versions
    dplyr::mutate(mean30PM2_5MC = mean(.data$PM2_5MC, na.rm = T),
                  var30PM2_5MC = stats::var(.data$PM2_5MC, na.rm = T),
                  mean30AccelX = mean(.data$AccelX, na.rm = T),
                  var30AccelX = stats::var(.data$AccelX, na.rm = T),
                  mean30AccelY = mean(.data$AccelY, na.rm = T),
                  var30AccelY = stats::var(.data$AccelY, na.rm = T),
                  mean30AccelZ = mean(.data$AccelZ, na.rm = T),
                  var30AccelZ = stats::var(.data$AccelZ, na.rm = T),
                  # mean30CO2 = base::mean(.data$CO2, na.rm = T),
                  # var30CO2 = stats::var(.data$CO2, na.rm = T),
                  mean30GPSlat = base::mean(.data$GPSlat, na.rm = T),
                  mean30GPSlon = base::mean(.data$GPSlon, na.rm = T)) %>%
    dplyr::select(.data$UPASserial, .data$datetime_local_rounded, .data$mean30PM2_5MC:.data$mean30GPSlon) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$UPASserial) %>%
    dplyr::mutate(compliance = ifelse((.data$var30AccelX > 100) | (.data$var30AccelY > 100) | (.data$var30AccelZ > 100), 1, 0),
                  compliance_rollmean = ifelse(
                    as.numeric(zoo::rollapply(.data$compliance, width=20,  FUN = mean, align = "center", na.rm = TRUE, partial=F, fill = NA)) > 0, 1, 0))
  # dplyr::mutate(mean30AccelX = zoo::rollapply(AccelX, width=30,  FUN = mean, align = "center", na.rm = TRUE, partial=F, fill = NA),
  #               var30AccelX = zoo::rollapply(AccelX, width=30,  FUN = var, align = "center", na.rm = TRUE, partial=F, fill = NA),
  #               mean30AccelY = zoo::rollapply(AccelY, width=30,  FUN = mean, align = "center", na.rm = TRUE, partial=F, fill = NA),
  #               var30AccelY = zoo::rollapply(AccelY, width=30,  FUN = var, align = "center", na.rm = TRUE, partial=F, fill = NA),
  #               mean30AccelZ = zoo::rollapply(AccelZ, width=30,  FUN = mean, align = "center", na.rm = TRUE, partial=F, fill = NA),
  #               var30AccelZ = zoo::rollapply(AccelZ, width=30,  FUN = var, align = "center", na.rm = TRUE, partial=F, fill = NA))

  # df_30s_mean <- df_30s_mean %>%
  #   dplyr::group_by(UPASserial) %>%
  #   dplyr::summarise(compliance_hours = sum(compliance_rollmean, na.rm = T)/2/60,
  #                    compliance_percent = sum(compliance_rollmean, na.rm = T)/n())

  return(df_30s_mean)
}

#'Generate a gps map from a data frame with time-averaged data
#'
#' @param df Pass a UPAS v2+ log data frame from 'get_30s_mean' function.
#'
#' @return A leaflet map of 30s averages of the selected variable.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#'

#TODO add variable input so user can specify variable to be mapped (PM or CO2 and more)
gps_map = function(df) {

  if(("mean30PM2_5MC") %in% colnames(df)){
  gpsPMPlot_data <- df %>%
    dplyr::select(.data$UPASserial, .data$datetime_local_rounded, .data$mean30GPSlat, .data$mean30GPSlon, .data$mean30PM2_5MC) %>%
    dplyr::mutate(aqi = as.factor(dplyr::case_when(
      .data$mean30PM2_5MC<12.0 ~ "Good",
      .data$mean30PM2_5MC<35.4 ~ "Moderate",
      .data$mean30PM2_5MC<55.4 ~ "USG",
      .data$mean30PM2_5MC<150.4 ~ "Unhealthy",
      .data$mean30PM2_5MC<250.4 ~ "Very Unhealthy",
      TRUE ~ "Hazardous"))) %>%
    dplyr::filter(!is.na(.data$mean30PM2_5MC), !is.na(.data$mean30GPSlat), !is.na(.data$mean30GPSlon))

  sp::coordinates(gpsPMPlot_data)<- ~mean30GPSlon + mean30GPSlat
  # crs(gpsPMPlot_data) <- CRS("+init=epsg:4326")

  pal <- leaflet::colorBin(
    palette = c("#47AF22", "#EEEE22", "#FF8B14","#FF0000","#800080","#581D00"),
    domain = gpsPMPlot_data$mean30PM2_5MC,
    bins = c(0, 12.0, 35.4, 55.4, 150.4, 250.4, Inf)
  )

  pm25_leaflet <- leaflet::leaflet(gpsPMPlot_data) %>% leaflet::addTiles()

  pm25_leaflet <- pm25_leaflet %>%
    leaflet::addCircleMarkers(
      color=~pal(mean30PM2_5MC),
      popup=paste("PM2.5 (&#181g/m<sup>3</sup>):", round(gpsPMPlot_data$mean30PM2_5MC, digits=2),
                  "<br>","UPAS:", gpsPMPlot_data$UPASserial,
                  "<br>","Local Time:", gpsPMPlot_data$datetime_local_rounded), stroke = FALSE,
      radius = 7.5, fillOpacity = 0.7 , group = as.factor(gpsPMPlot_data$UPASserial)) %>%
    leaflet::addLayersControl(overlayGroups = (as.factor(gpsPMPlot_data$UPASserial)),
                      options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
    leaflet::addLegend("topright",
                       pal = pal,
                       values = gpsPMPlot_data$mean30PM2_5MC,
                       title = "PM2.5 (&#181g/m<sup>3</sup>)",
                       opacity = 0.9)

  # return(gpsPMPlot_data)
  return(pm25_leaflet)
  }

  # Throw error if no 30s averaged PM data to map
  else{
    error <- "No PM data in log file"

    return(error)

  }

}


