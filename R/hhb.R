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
#' hhb_filename <- 'HHB00032_LOG_2024-07-01T18_20UTC.csv'
#' hhb_file <- system.file("extdata", hhb_filename, package = "astr", mustWork = TRUE)
#' hhb_header_raw <- fread_ast_header(hhb_file)$header
#' hhb_header_wide <- transpose_ast_header(hhb_header_raw)
#' hhb_header <- format_hhb_header(hhb_header_wide)

format_hhb_header = function(df) {

  df$Firmware <- as.character(gsub(" ", "_", df$Firmware))

  df$ProgrammedRuntime <- ifelse(df$ProgrammedRuntime == "indefinite",
                                 as.numeric(NA),
                                 as.numeric(df$ProgrammedRuntime))

  df <- dplyr::mutate(df,
    dplyr::across(dplyr::contains(c("CalVoutMin", "CalVoutMax", "CalMFMin", "CalMFMax", "MF4",
                      "MF3", "MF2", "MF1", "MF0", "UTCOffset", "Runtime",
                      "Volume", "FlowRate", "DutyCycle", "ShutdownMode")),
           \(x) as.numeric(x)),
    dplyr::across(dplyr::contains(c("StartDateTimeUTC", "EndDateTimeUTC", "CalDate")),
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
#' hhb_filename <- 'HHB00032_LOG_2024-07-01T18_20UTC.csv'
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
#'                                "LocalTZ",  "UserTZ",
#'                                grep("^SEN55", colnames(hhb_log), value=TRUE)))

format_hhb_log = function(log, header, tz=NA, cols_keep=c(), cols_drop=c()) {

  df_h <- dplyr::select(header,
                        dplyr::any_of(c("HHBserial","LogFileName","SampleName",
                                        "StartDateTimeUTC")),
                        dplyr::contains(c("ID","VolumetricFlowRate"),
                                        ignore.case=F))
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
                          units="secs"))

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
    df <- dplyr::filter(df, !is.na(.data$UnixTime))
  }

  return(df)
}
