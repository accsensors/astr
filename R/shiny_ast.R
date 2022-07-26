#'Reformat units
#'to be more user friendly for the Shiny app
#'
#' @param df_h Pass a UPAS v2 or v2+ data frame from 'read_ast_header' function.
#'
#' @return A modified data frame with units and user friendly column names for header data.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2x_header_shiny <- shiny_header(upasv2x_header)
#' upasv2_header_shiny <- shiny_header(upasv2_header)

shiny_units = function(df){
  colnames(df) <- gsub("L min^-1", "L/min",
                  gsub("(g L^-1)", "(g/L)",
                  gsub("(m s^-1)", "(m/s)",
                  gsub("mdeg s^-1)", "(mdeg/s)",
                  gsub("(ug m^-3)", "(ug/m^3)"),
                  gsub("(# cm^-3)", "(#/cm^3)",
                       colnames(df), fixed=TRUE)))))

  # colnames(df) %>%
  #   stringr::str_replace_all(
  #     "L min^-1", "L/min")
  df
}
#'Rename UPAS header file data frame columns
#'to be more user friendly for the Shiny app
#'
#' @param df_h Pass a UPAS v2 or v2+ data frame from 'read_ast_header' function.
#'
#' @return A modified data frame with units and user friendly column names for header data.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2x_header_shiny <- shiny_header(upasv2x_header)
#' upasv2_header_shiny <- shiny_header(upasv2_header)

shiny_header = function(df_h, fract_units = FALSE) {

  df_h <- df_h %>%
    dplyr::rename_with(
      ~ dplyr::case_when(
        . == "OverallDuration" ~ "OverallDuration (Hr)",
        . == "SampledVolume" ~ "SampledVolume (L)",
        . == "SampledRuntime" ~ "SampledRuntime (Hr)",
        . == "PumpingDuration" ~ "PumpingDuration (Hr)",
        . == "ProgrammedStartDelay" ~ "ProgrammedStartDelay (Hr)",
        . == "ProgrammedStartTime" ~ "ProgrammedStartTime",
        . == "ProgrammedRuntime" ~ "ProgrammedRuntime (Hr)",
        . == "FlowRateSetpoint" ~ "FlowRateSetpoint (L)",
        . == "DutyCycle" ~ "DutyCycle (%)",
        . == "FlowDutyCycle" ~ "FlowDutyCycle (%)",
        . == "LogInterval" ~ "LogInterval (s)",
        . == "StartDateTimeUTC" ~ "StartDateTimeUTC",
        . == "EndDateTimeUTC" ~ "EndDateTimeUTC",
        . == "StartBatteryVoltage" ~ "StartBatteryVoltage (V)",
        . == "EndBatteryVoltage" ~ "EndBatteryVoltage (V)",
        . == "GPSUTCOffset" ~ "GPSUTCOffset (Hr)",
        TRUE ~ .))

  if(fract_units) {
    df_h <- shiny_units(df_h)
  }

  # df_h <- df_h %>%
  #   dplyr::rename(`AST Sampler` = .data$ASTSampler,
  #                  `UPAS Serial` = .data$UPASserial,
  #                  `Sample Name` = .data$SampleName,
  #                  `Cartridge ID` = .data$CartridgeID,
  #                  `Overall Duration` = .data$OverallDuration,
  #                  `Firmware Rev` = .data$FirmwareRev,
  #                  `Shutdown Mode` = .data$ShutdownMode
  #                  )
  #
  # if(stringr::str_detect(df_h$Firmware, 'UPAS_v2_x')) {
  #   df_h <- df_h
  #     dplyr::rename( `Overall Flow Rate Average` = .data$OverallFlowRateAverage,
  #                  `Sampled Volume` = .data$SampledVolume,
  #                  `Sampled Runtime` = .data$SampledRuntime,
  #                  `Pumping Duration` = .data$PumpingDuration,
  #                  `Shutdown Reason` = .data$ShutdownReason,
  #                  #Settings
  #                  `Start On Next Power Up` = .data$StartOnNextPowerUp,
  #                  `Programmed Start Delay` = .data$ProgrammedStartDelay,
  #                  `Programmed Start Time` = .data$ProgrammedStartTime,
  #                  `Programmed Runtime` = .data$ProgrammedRuntime,
  #                  `Size Selective Inlet` = .data$SizeSelectiveInlet,
  #                  `Flow Rate Setpoint` = .data$FlowRateSetpoint,
  #                  `Duty Cycle` = .data$DutyCycle,
  #                  `Flow Duty Cycle` = .data$FlowDutyCycle,
  #                  `GPS Enabled` = .data$GPSEnabled,
  #                  `PM Sensor Operation` = .data$PMSensorOperation,
  #                  `RT Gas Sample State` = .data$RTGasSampleState,
  #                  `Log Interval` = .data$LogInterval,
  #                  `Power Save Mode` = .data$PowerSaveMode,
  #                  `App Version` = .data$AppVersion,
  #                  #Meta
  #                  `Start Date Time UTC` = .data$StartDateTimeUTC,
  #                  `End Date Time UTC` = .data$EndDateTimeUTC,
  #                  `Start Battery Voltage` = .data$StartBatteryVoltage,
  #                  `End Battery Voltage` = .data$EndBatteryVoltage,
  #                  `Start Battery Charge` = .data$StartBatteryCharge,
  #                  `End Battery Charge` = .data$EndBatteryCharge,
  #                  `GPS UTC Offset` = .data$GPSUTCOffset
  #                 )
  # }
  return(df_h)
}

#'Rename UPAS log file data frame columns
#'to be more user friendly for the Shiny app
#'
#' @param df Pass a UPAS v2 or v2+ log data frame from 'read_ast_log' function.
#' @param df_h Pass a UPAS v2 or v2+ header data frame from 'read_ast_log' function.
#'
#' @return A modified data frame with units and user friendly column names for log data.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2x_log_shiny <- shiny_log(upasv2x_log, upasv2x_header)
#' upasv2_log_shiny <- shiny_log(upasv2_log, upasv2_header)

shiny_log = function(df, df_h) {
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
  #colnames(df) <- paste(colnames(df), units, sep="")

  # df <- df %>%
  #   #TODO figure out how to micro sign in in ug units
  #   #TODO figure out how to make ^3 into superscript
  #   dplyr::rename(`Sample Time (s)` = .data$SampleTime,
  #                 `Unix Time (s)` = .data$UnixTime,
  #                 `Date Time UTC (YYYY-MM-DDTHH:MM:SS)` = .data$DateTimeUTC,
  #                 `Date Time Local (YYYY-MM-DDTHH:MM:SS)` = .data$DateTimeLocal,
  #                 `Time Zone Offset (hrs)` = .data$TZOffset,
  #                 `Pumping Flow Rate (L*min^-1)` = .data$PumpingFlowRate,
  #                 `Sampled Volume (L)` = .data$SampledVolume,
  #                 `Filter Differential Pressure (Pa)` = .data$FilterDP,
  #                 `Atmospheric T (C)` = .data$AtmoT,
  #                 `Atmospheric P (hPa)` = .data$AtmoP,
  #                 `Atmospheric RH (%RH)` = .data$AtmoRH,
  #                 `Atmospheric Density (g*L^-1)` = .data$AtmoDensity,
  #                 #`GPSQual` = .data$GPSQual,
  #                 `GPS Latitude (decimalDegrees)` = .data$GPSlat,
  #                 `GPS Longitude (decimalDegrees)` = .data$GPSlon,
  #                 `GPS Altitude (m)` = .data$GPSalt,
  #                 `GPS Satellite Signals Received (#)`= .data$GPSsat,
  #                 `GPS Measured Speed (m*s^-1)` = .data$GPSspeed,
  #                 `GPS Horizontal Dilution of Precision` = .data$GPShDOP,)
  #
  # # if(df_h$ASTSampler=='UPAS_v2_x'){
  # if(stringr::str_detect(df_h$Firmware, 'UPAS_v2_x')) {
  #   df <- df %>%
  #     dplyr::rename(`Unix Time MCU (s)` = .data$UnixTimeMCU,
  #                 `Overall Flow Rate (L*min^-1)` = .data$OverallFlowRate,
  #                 `Battery Charge (%)` = .data$BatteryCharge,
  #                 `GPS Signal Quality (NMEA Standard)` = .data$GPSQual,
  #                 `Altitude Above Sea Level (m)` = .data$AtmoAlt,
  #                 `X Acceleration (mg)` = .data$AccelX,
  #                 `X Acceleration Variance (mg)` = .data$AccelXVar,
  #                 `X Acceleration Minimum (mg)` = .data$AccelXMin,
  #                 `X Accleration Maximum (mg)` = .data$AccelXMax,
  #                 `Y Acceleration (mg)` = .data$AccelY,
  #                 `Y Acceleration Variance (mg)` = .data$AccelYVar,
  #                 `Y Acceleration Minimum (mg)` = .data$AccelYMin,
  #                 `Y Acceleration Maximum (mg)` = .data$AccelYMax,
  #                 `Z Acceleration (mg)` = .data$AccelZ,
  #                 `Z Acceleration Variance (mg)` = .data$AccelZVar,
  #                 `Z Acceleration Minimum (mg)` = .data$AccelZMin,
  #                 `Z Acceleration Maximum (mg)` = .data$AccelZMax,
  #                 `Rotational X Acceleration (mdeg*s^-1)` = .data$RotX,
  #                 `Rotational X Acceleration Variance (mdeg*s^-1)` = .data$RotXVar,
  #                 `Rotational X Acceleration Minimum (mdeg*s^-1)` = .data$RotXMin,
  #                 `Rotational X Acceleration Maximum (mdeg*s^-1)` = .data$RotXMax,
  #                 `Rotational Y Acceleration (mdeg*s^-1)` = .data$RotY,
  #                 `Rotational Y Acceleration Variance (mdeg*s^-1)` = .data$RotYVar,
  #                 `Rotational Y Acceleration Minimum (mdeg*s^-1)` = .data$RotYMin,
  #                 `Rotational Y Acceleration Maximum (mdeg*s^-1)` = .data$RotYMax,
  #                 `Rotational Z Acceleration (mdeg*s^-1)` = .data$RotZ,
  #                 `Rotational Z Acceleration Variance (mdeg*s^-1)` = .data$RotZVar,
  #                 `Rotational Z Acceleration Minimum (mdeg*s^-1)` = .data$RotZMin,
  #                 `Rotational Z Acceleration Maximum (mdeg*s^-1)` = .data$RotZMax,
  #                 `Percentage of Time X Up (%)` = .data$Xup,
  #                 `Percentage of Time X Down (%)` = .data$XDown,
  #                 `Percentage of Time Y Up (%)` = .data$Yup,
  #                 `Percentage of Time Y Down (%)` = .data$Ydown,
  #                 `Percentage of Time Z Up (%)` = .data$Zup,
  #                 `Percentage of Time Z Down (%)` = .data$Zdown,
  #                 `Step Count (#)` = .data$StepCount,
  #                 `Light Sensor Illuminance (lux)` = .data$LUX,
  #                 `UV Index (0 to 11+)` = .data$UVindex,
  #                 `High-Wavelength Light Sensor Raw Output` = .data$HighVisRaw,
  #                 `Low Wavelength Light Sensor Raw Output` = .data$LowVisRaw,
  #                 `Infrared Light Sensor Raw Output` = .data$IRRaw,
  #                 `UV Light Sensor Raw Output` = .data$UVRaw,
  #                 `PM Measurement Count (# per log interval)` = .data$PMMeasCnt,
  #                 `PM1.0 Mass Concentration (ug*m^-3)` = .data$PM1MC,
  #                 `PM1.0 Mass Concentration Variance (ug*m^-3)` = .data$PM1MCVar,
  #                 `PM2.5 Mass Concentration (ug*m^-3)` = .data$PM2_5MC,
  #                 `PM2.5 Mass Concentration Variance (ug*m^-3)` = .data$PM2_5MCVar,
  #                 `PM4.0 Mass Concentration (ug*m^-3)` = .data$PM4MC,
  #                 `PM4.0 Mass Concentration Variance (ug*m^-3)` = .data$PM4MCVar,
  #                 `PM10 Mass Concentration (ug*m^-3)` = .data$PM10MC,
  #                 `PM10 Mass Concentration Variance (ug*m^-3)` = .data$PM10MCVar,
  #                 `PM0.5 Particle Concentration (#*cm^-3)` = .data$PM0_5NC,
  #                 `PM0.5 Particle Concentration Variance (#*cm^-3)` = .data$PM0_5NCVar,
  #                 `PM1.0 Particle Concentration (#*cm^-3)` = .data$PM1NC,
  #                 `PM1.0 Particle Concentration Variance (#*cm^-3)` = .data$PM1NCVar,
  #                 `PM2.5 Particle Concentration (#*cm^-3)` = .data$PM2_5NC,
  #                 `PM2.5 Particle Concentration Variance (#*cm^-3)` = .data$PM2_5NCVar,
  #                 `PM4.0 Particle Concentration (#*cm^-3)` = .data$PM4NC,
  #                 `PM4.0 Particle Concentration Variance (#*cm^-3)` = .data$PM4NCVar,
  #                 `PM10 Particle Concentration (#*cm^-3)` = .data$PM10NC,
  #                 `PM10 Particle Concentration Variance (#*cm^-3)` = .data$PM10NCVar,
  #                 `PM Typical Particle Size (um)` = .data$PMtypicalParticleSize,
  #                 `PM Typical Particle Size Variance (um)` = .data$PMtypicalParticleSizeVar,
  #                 `PM2.5 Sampled Mass (ug)` = .data$PM2_5SampledMass,
  #                 `PCB1 T (C)` = .data$PCB1T,
  #                 `PCB2 T (C)` = .data$PCB2T,
  #                 `Downstream of Filter T (C)` = .data$FdpT,
  #                 `Accelerometer T (C)` = .data$AccelT,
  #                 `RTD Sensor Resistance (ohm)` = .data$PT100R,
  #                 `PCB2 P (hPa)` = .data$PCB2P,
  #                 `Main Pump Power Level (integer)` = .data$PumpPow1,
  #                 `Secondary Pump Power Level (integer)` = .data$PumpPow2,
  #                 `Pump Voltage Input (V)` = .data$PumpV,
  #                 `Mass Flow Rate (g/min)` = .data$MassFlow,
  #                 `Mass Flow Sensor Output (V)` = .data$MFSVout,
  #                 `Battery Fuel Gauge Output (16-bit integer)` = .data$BFGenergy,
  #                 `Battery Voltage (V)` = .data$BattVolt,
  #                 `3.3V Rail Voltage (V)` = .data$v3_3,
  #                 `5V Rail Voltage (V)` = .data$v5,
  #                 `Pumps Operational State (bool)` = .data$PumpsON,
  #                 #`Dead(bool)` = .data$Dead,
  #                 `Battery Charge Indicator 1 (bool)` = .data$BCS1,
  #                 `Battery Charge Indicator 2 (bool)` = .data$BCS2,
  #                 `External Power Indicator (bool)` = .data$BC_NPG,
  #                 `Time to Read/Write Log File Line (s)` = .data$FLOWCTL,
  #                 `Time to Read GPS Data (s)` = .data$GPSRT,
  #                 `Time to Write SD Card Log File Line (s)` = .data$SD_DATAW,
  #                 `Time to Update SD Card Log File Header (s)` = .data$SD_HEADW,
  #                 `Time Pumps OFF Per Log Interval (s)` = .data$TPumpsOFF,
  #                 `Time Pumps ON Per Log Interval (s)` = .data$TPumpsON)
  #
  #   if(df_h$FirmwareRev>=110) {
  #     df <- df %>%
  #       dplyr::rename(`CO2 Concentration (ppm)` = .data$CO2,
  #                   `CO2 Sensor Temperature (C)` = .data$SCDT,
  #                   `CO2 Sensor RH (%RH)` = .data$SCDRH,
  #                   `VOC Sensor Raw Output` = .data$VOCRaw,
  #                   `NOx Sensor Raw Output` = .data$NOXRaw
  #
  #     )
  #   }
  # }

  return(df)
}

#'Rename UPAS log file single column name
#'to be more user friendly for the Shiny app plot
#'
#' @param clm_name Pass a UPAS v2 or v2+ log column name from a formatted data frame.
#' @param fract_units Boolean to specify if units should be fractional (L min^-1 vs L/min).
#'
#' @return A modified column name for plot axis label in shinyAST app.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' plot_label <- shiny_axis(column_name)

shiny_axis = function(clm_name, fract_units = FALSE){

  #TODO Check that this covers all possible log file variables (might be nice to add standard v2 log file titles so this can be used with standard v2 file)
  df <- data.frame(SampleTime = c("Sample Time", "(s)"),
                   UnixTime = c("Unix Time", "(s)"),
                   DateTimeUTC = c("Date Time UTC", "(YYYY-MM-DDTHH:MM:SS)"),
                   DateTimeLocal = c("Date Time Local", "(YYYY-MM-DDTHH:MM:SS)"),
                   TZOffset = c("Time Zone Offset", "(hrs)"),
                   PumpingFlowRate = c("Pumping Flow Rate", "(L min^-1)"),
                   SampledVolume = c("Sampled Volume", "(L)"),
                   FilterDP = c("Filter Differential Pressure", "(Pa)"),
                   AtmoT = c("Atmospheric T", "(C)"),
                   AtmoP = c("Atmospheric P", "(hPa)"),
                   AtmoRH = c("Atmospheric RH", "(%RH)"),
                   AtmoDensity = c("Atmospheric Density", "(g L^-1)"),
                   GPSQual = c("GPS Signal Quality", "(NMEA Standard)"),
                  GPSlat = c("GPS Latitude", "(decimalDegrees)"),
                  GPSlon = c("GPS Longitude", "(decimalDegrees)"),
                  GPSalt = c("GPS Altitude", "(m)"),
                  GPSsat = c("GPS Satellite Signals Received", "(#)"),
                  GPSspeed = c("GPS Measured Speed", "(m s^-1)"),
                  GPShDOP = c("GPS Horizontal Dilution of Precision", ""),

                  UnixTimeMCU = c("Unix Time MCU", "(s)"),
                  OverallFlowRate = c("Overall Flow Rate", "(L min^-1)"),
                  BatteryCharge = c("Battery Charge", "(%)"),
                  AtmoAlt = c("Altitude Above Sea Level", "(m)"),
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
                  row.names = c("axis_name", "unit")
                  )

    # df_sel <- df %>%
    #   dplyr::select(dplyr::all_of(clm_name))
    #
    # clm_name <- paste(df_sel["var",], df_sel["unit",], sep=" ")

    df_long <- df %>%
      t()

    df_long <- cbind(rownames(df_long), data.frame(df_long, row.names=NULL)) %>%
      dplyr::rename(`var` = `rownames(df_long)`)

    if(fract_units){
      df_long <- df_long %>%
        dplyr::mutate(unit =
                        dplyr::case_when(unit == "(L min^-1)" ~ "(LPM)",
                                  unit == "(g L^-1)" ~ "(g/L)",
                                  unit == "(m s^-1)" ~ "(m/s)",
                                  unit == "(mdeg s^-1)" ~ "(mdeg/s)",
                                  unit == "(ug m^-3)" ~ "(ug/m^3)",
                                  unit == "(# cm^-3)" ~ "(#/cm^3)",
                                  TRUE ~ unit)
        )
    }

    clm_name <- df_long %>%
      dplyr::filter(var == clm_name)

    clm_name <- paste(clm_name$axis_name, clm_name$unit, sep=" ")

  return(clm_name)
}
