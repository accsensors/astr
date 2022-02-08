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
#' filename <- 'PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt'
#' file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
#' read_ast_header(file)

read_upasv2x_header = function(df) {
#                 LogFilename = gsub("/sd/", "", LogFilename),
#                 ShutdownReason  = case_when(ShutdownMode == 0 ~ "unknown error",
#                                             ShutdownMode == 1 ~ "user pushbutton stop",
#                                             ShutdownMode == 2 ~ "depleted battery",
#                                             ShutdownMode == 3 ~ "completed preset sample duration",
#                                             ShutdownMode == 4 ~ "thermal protection",
#                                             ShutdownMode == 5 ~ "max power at initialization",
#                                             ShutdownMode == 6 ~ "max power during sample",
#                                             ShutdownMode == 7 ~ "blocked flow",
#                                             ShutdownMode == 8 ~ "SD card removed",
#                                             between(ShutdownMode, 64, 79) ~ "code freeze",
#                                             # ShutdownMode >= 80, "RTOS crash",
#                                             TRUE ~ "RTOS crash"))
#
# df <- df %>%
#   dplyr::select(1:match("ShutdownMode",colnames(df)), ShutdownReason, (match("ShutdownMode",colnames(df))+1):ncol(df))


#                    ,"LifetimeSampleCount",
#                    "LifetimeSampleRuntime","GPSUTCOffset","FlowOffset",
#                    "OverallFlowRateAverage"), as.numeric) %>%
# dplyr::mutate_at(c("StartOn"), as.logical) %>%
# dplyr::mutate(UPASfirmware    = sapply(strsplit(UPASfirmware,"-"), `[`, 2),
#               UPASfirmware    = as.numeric(gsub("rev_", "", UPASfirmware)),
#               LogFilename = gsub("/sd/", "", LogFilename),
#               ShutdownReason  = case_when(ShutdownMode == 0 ~ "unknown error",
#                                           ShutdownMode == 1 ~ "user pushbutton stop",
#                                           ShutdownMode == 2 ~ "depleted battery",
#                                           ShutdownMode == 3 ~ "completed preset sample duration",
#                                           ShutdownMode == 4 ~ "thermal protection",
#                                           ShutdownMode == 5 ~ "max power at initialization",
#                                           ShutdownMode == 6 ~ "max power during sample",
#                                           ShutdownMode == 7 ~ "blocked flow",
#                                           ShutdownMode == 8 ~ "SD card removed",
#                                           between(ShutdownMode, 64, 79) ~ "code freeze",
#                                           ShutdownMode >= 80, "RTOS crash")) %>%
# dplyr::select(1:match("ShutdownMode",colnames(df)), ShutdownReason, (match("ShutdownMode",colnames(df))+1):ncol(df)) %>%
# dplyr::mutate_at(c("StartDateTimeUTC", "EndDateTimeUTC"), as.POSIXct, format="%Y-%m-%dT%H:%M:%S", tz="UTC") %>%
# dplyr::mutate(SampleName  = gsub("_+$", "", SampleName),
#               SampleName  = ifelse(SampleName != "", SampleName, NA),
#               CartridgeID = gsub("_+$", "", CartridgeID),
#               CartridgeID = gsub("-+$", "", CartridgeID),
#               CartridgeID = ifelse(CartridgeID != "", CartridgeID, NA),
#               MFSCalFlowDeadhead = NA,
#               MFSCalFlowMax = NA,
#               MFSCalFlowMin = NA,
#               across(!c(LogFilename:CartridgeID, ProgrammedRuntime:SizeSelectiveInlet,
#                         AppVersion:EndDateTimeLocal, ShutdownReason:MFSCalPerson), ~ as.numeric(.x))) %>%
# dplyr::rename(`Sample Duration (hr)` = OverallDuration)

}
