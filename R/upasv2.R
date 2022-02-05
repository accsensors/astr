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
#' @param file A UPASv2 log file
#' @param update_names Convert old log file column names to match current log
#' file names.
#'
#' @return A data frame.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
#' file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
#' read_upasv2_header(file, update_names=FALSE)

read_upasv2_header <- function(file, update_names=FALSE){

  header_lines <- readLines(file, n=103)

  n_max    <- grep("AverageVolumetricFlow", header_lines)
  n_cal    <- grep("CALIBRATION COEFFICIENTS", header_lines)
  n_cal    <- ifelse(length(n_cal) == 0, n_max, n_cal)
  n_setup  <- grep("SETUP SUMMARY", header_lines)

  header_lines <- unique(c(header_lines[1:n_cal], header_lines[n_setup:n_max]))
  header_lines <- header_lines[!is.na(header_lines)]
  header_lines <- header_lines[!(header_lines %in% c("",
                                                     "PARAMETER,VALUE,UNITS/NOTES",
                                                     "SAMPLE IDENTIFICATION",
                                                     "SETUP SUMMARY",
                                                     "SAMPLE SUMMARY",
                                                     "CALIBRATION COEFFICIENTS"))]

  df <- sapply(strsplit(header_lines,","), `[`, 2) %>%
    t() %>%
    data.frame(stringsAsFactors=F)

  colnames(df) <- sapply(strsplit(header_lines,","), `[`, 1)

  df <- df %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("UPASserial","GPSUTCOffset","StartOnNextPowerUp","ProgrammedStartDelay","ProgrammedRuntime",
                                  "VolumetricFlowRate","DutyCycle","DutyCycleWindow",
                                  "GPSEnabled","LogFileMode","LogInterval","AppLock",
                                  "StartBatteryCharge","StartBatteryVoltage","EndBatteryCharge","EndBatteryVoltage",
                                  "ShutdownMode","SampledVolume","SampledRuntime","LoggedRuntime")),
                         as.numeric)) %>%
    dplyr::mutate_at(c("StartOnNextPowerUp", "GPSEnabled"), as.logical) %>%
    dplyr::mutate(UPASfirmware    = sapply(strsplit(.data$UPASfirmware,"-"), `[`, 2),
                  UPASfirmware    = as.numeric(gsub("rev", "", .data$UPASfirmware)),
                  UPASlogFilename = gsub("/sd/", "", .data$UPASlogFilename),
                  LogFileMode     = ifelse(.data$LogFileMode == 0, "normal", "debug"),
                  ShutdownReason  = dplyr::case_when(ShutdownMode == 0 ~ "unknown error",
                                              ShutdownMode == 1 ~ "user pushbutton stop",
                                              ShutdownMode == 2 ~ "depleted battery",
                                              ShutdownMode == 3 ~ "completed preset sample duration",
                                              ShutdownMode == 4 ~ "thermal protection",
                                              ShutdownMode == 5 ~ "max power at initialization",
                                              ShutdownMode == 6 ~ "max power during sample",
                                              ShutdownMode == 7 ~ "blocked flow")) %>%
    dplyr::select(1:match("ShutdownMode",colnames(df)), .data$ShutdownReason, (match("ShutdownMode",colnames(df))+1):ncol(df))

  if(df$UPASfirmware == 100){

    df <- df %>%
      dplyr::mutate_at(c("PowerCycles","CumulativeSamplingTime","AverageVolumetricFlow"), as.numeric) %>%
      dplyr::mutate(StartDateTime = as.POSIXct(.data$StartDateTime, format="%Y-%m-%dT%H:%M:%SUTC", tz="UTC"))

    if(update_names){

      df <- df %>% dplyr::rename(LifetimeSampleRuntime     = .data$CumulativeSamplingTime,
                                 StartDateTimeUTC          = .data$StartDateTime,
                                 AverageVolumetricFlowRate = .data$AverageVolumetricFlow)}
  }else{

    df <- df %>%
      dplyr::mutate_at(c("LifetimeSampleCount","LifetimeSampleRuntime","FlowOffset","AverageVolumetricFlowRate"), as.numeric) %>%
      dplyr::mutate_at(c("StartDateTimeUTC", "EndDateTimeUTC"), as.POSIXct, format="%Y-%m-%dT%H:%M:%S", tz="UTC") %>%
      dplyr::mutate(SampleName  = gsub("_+$", "", .data$SampleName),
                    SampleName  = ifelse(.data$SampleName != "", .data$SampleName, NA),
                    CartridgeID = gsub("_+$", "", .data$CartridgeID),
                    CartridgeID = gsub("-+$", "", .data$CartridgeID),
                    CartridgeID = ifelse(.data$CartridgeID != "", .data$CartridgeID, NA))}

  return(df)
}
