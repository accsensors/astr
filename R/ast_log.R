#'Read the full log data from an Access Sensor Technologies (AST) air sampler
#'log file
#'
#' @param file Any AST air sampler log file name.
#' @param tz_offset Pass an option timezone offset.
#' @param update_names Option to update old sampler names to latest version.
#' @param cols_keep Specify log file columns to keep.
#' @param cols_drop Specify log file columns to remove.
#' @param shiny Option to make TRUE if using function with AST shiny app.
#'
#' @return A data frame with all log data plus some header data appended.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2_filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
#' upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
#' upasv2_log <- read_ast_log(upasv2_file)
#' upasv2x_rev81_filename <- 'PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt'
#' upasv2x_rev81_file <- system.file("extdata", upasv2x_rev81_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev81_log <- read_ast_log(upasv2x_rev81_file)
#' filename2 <- 'SH00007_LOG_2021-12-13T13_28_41UTC_---------------_-----.txt'
#' file2 <- system.file("extdata", filename2, package = "astr", mustWork = TRUE)
#' data_ast_log <- read_ast_log(file2)
#' data_ast_log <- read_ast_log(file2, cols_keep = c("SampleTime","UnixTime","DateTimeUTC","DateTimeLocal","PM2_5MC"))
#' upasv2x_diag_filename <- 'PSP00055_LOG_2022-03-24T18_05_32UTC_DIAGNOSTIC________________.txt'
#' upasv2x_diag_file <- system.file("extdata", upasv2x_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2x_diag_log <- read_ast_log(upasv2x_diag_file, update_names=FALSE)
#' upasv2x_rev158_filename <- 'PSP00270_LOG_2024-06-10T21_50_55UTC_name____________eng_______.txt'
#' upasv2x_rev158_file <- system.file("extdata", upasv2x_rev158_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev158_log <- read_ast_log(upasv2x_rev158_file, update_names=FALSE)

read_ast_log = function(file, tz_offset = NA, update_names = FALSE, cols_keep = c(), cols_drop = c(), shiny=FALSE) {

  df_log_raw <- astr::make_raw_ast_log(file)

  df_h <- astr::read_ast_header(file)

  df <- astr::format_ast_log(df_h, df_log_raw, tz_offset, update_names, cols_keep, cols_drop, shiny=shiny)

  return(df)

}

#'Generate a data frame of unformatted log column data from an Access Sensor
#'Technologies (AST) air sampler log file
#'
#' @description
#' `make_raw_ast_log`reads in the log data exactly as it
#' appears in the raw log file.
#'
#' @inheritParams read_ast_log
#'
#' @return A data frame of unformatted log data
#' @export
#'
#' @examples
#' upasv2_rev125_filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
#' upasv2_rev125_file <- system.file("extdata", upasv2_rev125_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev125_log_raw <- make_raw_ast_log(upasv2_rev125_file)
#' upasv2_rev130_diag_filename <- 'PS1786_LOG_2023-03-02T21_45_43UTC_DIAGNOSTIC____________.txt'
#' upasv2_rev130_diag_file <- system.file("extdata", upasv2_rev130_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev130_diag_log_raw <- make_raw_ast_log(upasv2_rev130_diag_file)
#' upasv2_rev138_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
#' upasv2_rev138_file <- system.file("extdata", upasv2_rev138_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev138_log_raw <- make_raw_ast_log(upasv2_rev138_file)
#' upasv2x_rev81_filename <- 'PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt'
#' upasv2x_rev81_file <- system.file("extdata", upasv2x_rev81_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev81_log_raw <- make_raw_ast_log(upasv2x_rev81_file)
#' upasv2x_rev117_filename <- 'PSP00030_LOG_2022-05-11T23_24_01UTC_---------------_----------.txt'
#' upasv2x_rev117_file <- system.file("extdata", upasv2x_rev117_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev117_log_raw <- make_raw_ast_log(upasv2x_rev117_file)
#' upasv2x_rev110_diag_filename <- 'PSP00055_LOG_2022-03-24T18_05_32UTC_DIAGNOSTIC________________.txt'
#' upasv2x_rev110_diag_file <- system.file("extdata", upasv2x_rev110_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev110_diag_log_raw <- make_raw_ast_log(upasv2x_rev110_diag_file)
#' upasv2x_rev158_filename <- 'PSP00270_LOG_2024-06-10T21_50_55UTC_name____________eng_______.txt'
#' upasv2x_rev158_file <- system.file("extdata", upasv2x_rev158_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev158_log_raw <- make_raw_ast_log(upasv2x_rev158_file)
#' upasv2x_rev158_diag_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
#' upasv2x_rev158_diag_file <- system.file("extdata", upasv2x_rev158_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev158_diag_log_raw <- make_raw_ast_log(upasv2x_rev158_diag_file)


make_raw_ast_log = function(file){

  nrows_header <- count_header_rows(file)
  # Need to account for blank lines when using skip in fread
  nrow_header_skip <- nrows_header$nrow_with_blanks+1

  df_log_raw <- data.table::fread(file,
                              sep=',',
                              skip = nrow_header_skip,
                              header = FALSE,
                              fill = TRUE,
                              blank.lines.skip = TRUE,
                              stringsAsFactors = FALSE)

  # firmware <- df_raw[grepl("firmware", df_raw$V1),]
  # firmware <- firmware %>%
  #   dplyr::mutate(ASTSampler = sub("-rev.*", "", .data$V2))
  #
  # if(firmware$ASTSampler != 'UPAS_v2_0'){
  #   firmware <- firmware %>%
  #     dplyr::mutate(FirmwareRev = sapply(strsplit(.data$V2,"-"), `[`, 2),
  #                   FirmwareRev = as.numeric(gsub("rev_", "", .data$FirmwareRev)))
  # }else{
  #   firmware <- firmware %>%
  #     dplyr::mutate(FirmwareRev = sapply(strsplit(.data$V2,"-"), `[`, 2),
  #                   FirmwareRev = as.numeric(gsub("rev", "", .data$FirmwareRev)))
  # }
  #
  # if(any(grepl("DIAGNOSTIC TEST", df_raw$V1)) |
  #    any(grepl("CO2", df_raw$V1)) ){
  #
  #   if(firmware$ASTSampler == 'UPAS_v2_x' & firmware$FirmwareRev>=127){
  #
  #     df_raw_log <- data.table::fread(file,
  #                                     sep=',',
  #                                     skip = nrow(df_raw)+10,
  #                                     header = FALSE,
  #                                     fill = TRUE,
  #                                     blank.lines.skip = TRUE,
  #                                     stringsAsFactors = FALSE)
  #
  #   }else{
  #
  #     df_raw_log <- data.table::fread(file,
  #                                     sep=',',
  #                                     skip = nrow(df_raw),
  #                                     header = FALSE,
  #                                     fill = TRUE,
  #                                     blank.lines.skip = TRUE,
  #                                     stringsAsFactors = FALSE)
  #   }
  #
  #   if(any(grepl("DIAGNOSTIC TEST", df_raw_log$V1))){
  #     df_raw_log <- df_raw_log %>%
  #       dplyr::slice(which(df_raw_log$V1=="DIAGNOSTIC TEST")+1:dplyr::n())
  #
  #   }else{
  #     df_raw_log <- df_raw_log %>%
  #       dplyr::slice(which(df_raw_log$V1=="SAMPLE LOG")+1:dplyr::n())
  #
  #   }
  #
  #   df_raw_log <- df_raw_log %>%
  #     dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  #
  #   df_raw <- df_raw %>%
  #     dplyr::bind_rows(df_raw_log) %>%
  #     dplyr::distinct(.data$V1, .data$V9, .keep_all = TRUE)
  #
  #
  # }

  return(df_log_raw)
}

#'Extract only the log data from an Access Sensor Technologies (AST) air sampler
#'log file
#'
#' @param df_h An AST air sampler formatted log file header dataframe.
#' @param df_raw Any AST air sampler unformatted log file dataframe.
#' @param tz_offset Pass an option timezone offset.
#' @param update_names Option to update old sampler names to latest version.
#' @param cols_keep Specify log file columns to keep.
#' @param cols_drop Specify log file columns to remove.
#' @param shiny Option to make TRUE if using function with AST shiny app.
#'
#' @return A data frame with all log data.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' data_ast_log <- format_ast_log(upasv2x_header, data_ast_raw)

format_ast_log = function(df_h, df_log_raw, tz_offset = NA, update_names = FALSE, cols_keep = c(), cols_drop = c(), shiny=FALSE) {

  df_cols <- df_log_raw[2,] %>%
    # dplyr::slice(which(df_log_raw=="SampleTime")) %>%
    unlist(use.names = FALSE)


  if(stringr::str_detect(df_h$Firmware, 'UPAS_v2_x')){
    df_log <- df_log_raw %>%
      dplyr::slice(-1:-3)
    # df_log <- df_log_raw %>%
    #   dplyr::slice(which(df_log_raw$V1=="SampleTime")+2:dplyr::n())
  }else if(stringr::str_detect(df_h$Firmware, 'SHEARv2_7_2') | stringr::str_detect(df_h$Firmware, 'UPAS_v2_0')){
    df_log <- df_log_raw %>%
      dplyr::slice(-1:-2)
    # df_log <- df_log_raw %>%
    #   dplyr::slice(which(df_log_raw$V1=="SampleTime")+1:dplyr::n())
  }


  if(nrow(df_log)>0){
    colnames(df_log) <- df_cols

    df_log <- df_log[ , colSums(is.na(df_log)) < nrow(df_log)]

    if(any(stringr::str_detect(names(df_h),'ASTSampler'))){
      #if(df_h$ASTSampler == 'UPAS_v2_x'){
      if(stringr::str_detect(df_h$Firmware, 'UPAS_v2_x') | stringr::str_detect(df_h$Firmware, 'SHEARv2_7_2')){
        df_log <- astr::format_upasv2x_log(df_h, df_log, tz_offset, cols_keep, cols_drop)

      }else if(df_h$ASTSampler == "UPAS_v2_0"){
        if(shiny) {update_names=TRUE}
        df_log <- astr::format_upasv2_log(df_h, df_log, update_names=update_names)

      }else{

      }
    }
    if(shiny){df_log <- astr::shiny_log(df_log)} #TODO move to own function format_shiny_log so that shiny functionality is not present in normal functions
  }

  return(df_log)

}
