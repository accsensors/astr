#'Read the header data from an Access Sensor Technologies (AST) air sampler
#'log file
#'
#' @param df Pass a upasv2x dataframe from read_ast_header function.
#'
#' @return A modified data frame with header data in wide format.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' read_upasv2x_header(upasv2x_header)

read_upasv2x_header = function(df) {

  df <- df %>%
    dplyr::mutate_at(c("LifetimeSampleCount","LifetimeSampleRuntime","GPSUTCOffset",
                       "FlowOffset","OverallFlowRateAverage"), as.numeric) %>%
    dplyr::mutate_at(c("StartOnNextPowerUp"), as.logical) %>%
    dplyr::mutate(LogFilename = gsub("/sd/", "", .data$LogFilename),
                  ShutdownReason  = dplyr::case_when(.data$ShutdownMode == 0 ~ "unknown error",
                                              .data$ShutdownMode == 1 ~ "user pushbutton stop",
                                              .data$ShutdownMode == 2 ~ "depleted battery",
                                              .data$ShutdownMode == 3 ~ "completed preset sample duration",
                                              .data$ShutdownMode == 4 ~ "thermal protection",
                                              .data$ShutdownMode == 5 ~ "max power at initialization",
                                              .data$ShutdownMode == 6 ~ "max power during sample",
                                              .data$ShutdownMode == 7 ~ "blocked flow",
                                              .data$ShutdownMode == 8 ~ "SD card removed",
                                              dplyr::between(.data$ShutdownMode, 64, 79) ~ "code freeze",
                                              TRUE ~ "RTOS crash")) %>%
    dplyr::select(1:match("ShutdownMode",colnames(df)), .data$ShutdownReason, (match("ShutdownMode",colnames(df))+1):ncol(df)) %>%
    dplyr::mutate_at(c("StartDateTimeUTC", "EndDateTimeUTC"), as.POSIXct, format="%Y-%m-%dT%H:%M:%S", tz="UTC") %>%
    dplyr::mutate(SampleName  = gsub("_+$", "", .data$SampleName),
                  SampleName  = ifelse(.data$SampleName != "", .data$SampleName, NA),
                  CartridgeID = gsub("_+$", "", .data$CartridgeID),
                  CartridgeID = gsub("-+$", "", .data$CartridgeID),
                  CartridgeID = ifelse(.data$CartridgeID != "", .data$CartridgeID, NA),
                  MFSCalFlowDeadhead = NA,
                  MFSCalFlowMax = NA,
                  MFSCalFlowMin = NA,
                  dplyr::across(!c(.data$LogFilename:.data$CartridgeID, .data$ProgrammedRuntime:.data$SizeSelectiveInlet,
                                   .data$AppVersion:.data$EndDateTimeLocal, .data$ShutdownReason:.data$MFSCalPerson), ~ as.numeric(.x))) %>%
    dplyr::rename(`Sample Duration (hr)` = .data$OverallDuration)

  return(df)

}
