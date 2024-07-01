#'Format the header data from an Access Sensor Technologies Home Health Box log file
#'
#' @description
#' `format_hhb_header` formats the header data from a HHB v2 log file by setting
#' the proper data type for each variable.
#'
#' @param df A HHB v2 header data frame returned by [transpose_ast_header]
#'
#' @return A data frame with a single row of HHB v2 header data that are formatted and ready for analysis.
#' @export
#' @importFrom rlang .data
#'
#' @examples

format_hhb_header = function(df) {

  df$Firmware <- as.character(gsub(" ", "_", df$Firmware))

  df$ProgrammedRuntime <- ifelse(df$ProgrammedRuntime == "indefinite", NA,
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
#' @param log A data frame of HHB v2 sample log data returned by the [fread_ast_log] function.
#' @param header A data frame of HHB v2 header data returned by the [read_ast_header] function.
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

format_hhb_log = function(log, header, tz=NA, cols_keep=c(), cols_drop=c()) {

  if (nrow(log) > 0) {

    df_h <- dplyr::select(header,
                          dplyr::any_of(c("HHBserial","LogFileName","SampleName","StartDateTimeUTC")),
                          dplyr::contains(c("ID","VolumetricFlowRate"), ignore.case=F))

    df <- dplyr::mutate(log,
            SampleTime = ifelse(.data$SampleTime == "99:99:99", NA,
                                strsplit(.data$SampleTime,":")),
            SampleTime = as.difftime(
                          3600*as.numeric(sapply(.data$SampleTime, `[`, 1)) +
                            60*as.numeric(sapply(.data$SampleTime, `[`, 2)) +
                            as.numeric(sapply(.data$SampleTime, `[`, 3)),
                          units="secs"),
            UserTZ  = ifelse(!is.na(tz), T, F),
            LocalTZ  = dplyr::case_when(!is.na(tz) ~ tz,
                                 header$UTCOffset == 0 ~ "UTC",
                                 (round(header$UTCOffset) == header$UTCOffset) &
                                  (header$UTCOffset < 0) ~
                                   sprintf("Etc/GMT+%i", abs(header$UTCOffset)),
                                 (round(header$UTCOffset) == header$UTCOffset) &
                                  (header$UTCOffset > 0) ~
                                   sprintf("Etc/GMT-%i", abs(header$UTCOffset)),
                                 T ~ NA))

    if(!is.na(unique(df$LocalTZ))){
      df <- dplyr::mutate(df,
                          DateTimeLocal = lubridate::with_tz(.data$DateTimeUTC,
                                                      tzone=unique(df$LocalTZ)))
    }else{
      df <- dplyr::mutate(df, DateTimeLocal = as.character(.data$DateTimeLocal))
    }

    df <- dplyr::relocate(df,
                          c("DateTimeLocal","LocalTZ"), .after="DateTimeUTC")
    df <- cbind(df, df_h)

    if (!is.null(cols_keep)){
      df <- dplyr::select(df, cols_keep)
    }else if (!is.null(cols_drop)){
      df <- dplyr::select(df, -cols_drop)
    }
  }

  return(df)
}
