#'Format the header data from an Access Sensor Technologies (AST) Home Health
#'Box (HHB) log file
#'
#' @param df_h_raw Any unformatted HHB header data frame.
#'
#' @return A data frame with header data in wide format.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' data_hhb_header <- format_ast_header(data_hhb_raw)

format_hhb_header = function(df_h) {

  df_h$Firmware <- as.character(gsub(" ", "_", df_h$Firmware))

  df_h$ProgrammedRuntime <- ifelse(df_h$ProgrammedRuntime == "indefinite", NA,
                                   as.numeric(df_h$ProgrammedRuntime))

  df_h <- dplyr::mutate(df_h,
    across(contains(c("CalVoutMin", "CalVoutMax", "CalMFMin", "CalMFMax", "MF4",
                      "MF3", "MF2", "MF1", "MF0", "UTCOffset", "Runtime",
                      "Volume", "FlowRate", "DutyCycle", "ShutdownMode")),
           \(x) as.numeric(x)),
    across(contains(c("StartDateTimeUTC", "EndDateTimeUTC", "CalDate")),
                    \(x) as.POSIXct(x, format="%Y-%m-%dT%H:%M:%S", tz="UTC")))

  return(df_h)
}


#'Format the sample log data from an Access Sensor Technologies (AST) Home
#'Health Box (HHB) log file
#'
#' @param df_h A formatted AST HHB log file header data frame.
#' @param df_raw An unformatted AST HHB sample log data frame.
#' @param tz_offset Pass an optional timezone offset.
#' @param cols_keep Optional: Provide a character vector specifying the names of a subset of sample log columns to keep.
#' @param cols_drop Optional: Provide a character vector specifying the names of a subset of sample log columns to remove.
#'
#' @return A data frame with all log data.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' data_ast_log <- format_ast_log(hhb_header, data_hhb_raw)

format_hhb_log = function(df_h, df_log, tz_offset=NA, cols_keep=c(), cols_drop=c()) {

  if (nrow(df_log) > 0) {

    tz_off <- ifelse(is.na(tz_offset), df_h$UTCOffset, tz_offset)

    df_h_sel <- dplyr::select(df_h,
                              any_of(c("HHBserial","LogFileName","SampleName","StartDateTimeUTC")),
                              dplyr::contains(c("ID","VolumetricFlowRate"), ignore.case = FALSE))

    df <- dplyr::mutate(df_log,
                        SampleTime = ifelse(SampleTime == "99:99:99", NA,
                                            strsplit(SampleTime,":")),
                        SampleTime = as.difftime(
                          3600*as.numeric(sapply(.data$SampleTime, `[`, 1)) +
                            60*as.numeric(sapply(.data$SampleTime, `[`, 2)) +
                            as.numeric(sapply(.data$SampleTime, `[`, 3)),
                          units="secs"),
                        tz_value = ifelse(!is.na(tz_offset), T, F),
                        DateTimeLocal = DateTimeUTC + (tz_off * 3600),
                        TZOffset = tz_off)

    df <- dplyr::select(df,
                        1:match("DateTimeLocal", colnames(df)), TZOffset,
                        (match("DateTimeLocal",colnames(df))+1):ncol(df))
    df <- cbind(df, df_h_sel)

    if (!is.null(cols_keep)){
      df <- dplyr::select(df, cols_keep)
    }else if (!is.null(cols_drop)){
      df <- dplyr::select(df, -cols_drop)
    }
  }

  return(df)
}
