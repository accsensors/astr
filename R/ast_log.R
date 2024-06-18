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

  # Read in first 200 lines of file and find the number of header rows
  # with and without blank lines included
  header_with_blanks <- readLines(file, n = 200)
  header_no_blanks <- header_with_blanks[which(header_with_blanks!="")]
  nrow_header_with_blanks <- as.numeric(grep("SAMPLE LOG", header_with_blanks))
  nrow_header_no_blanks <- as.numeric(grep("SAMPLE LOG", header_no_blanks))

  df_raw <- data.table::fread(file,
                              sep=',',
                              skip = 0,
                              nrows = nrow_header_no_blanks,
                              header = FALSE,
                              fill = TRUE,
                              blank.lines.skip = TRUE,
                              stringsAsFactors = FALSE)

  firmware <- df_raw[grepl("firmware", df_raw$V1),]
  firmware <- firmware %>%
    dplyr::mutate(ASTSampler = sub("-rev.*", "", .data$V2))

  if(firmware$ASTSampler != 'UPAS_v2_0'){
    firmware <- firmware %>%
      dplyr::mutate(FirmwareRev = sapply(strsplit(.data$V2,"-"), `[`, 2),
                    FirmwareRev = as.numeric(gsub("rev_", "", .data$FirmwareRev)))
  }else{
    firmware <- firmware %>%
      dplyr::mutate(FirmwareRev = sapply(strsplit(.data$V2,"-"), `[`, 2),
                    FirmwareRev = as.numeric(gsub("rev", "", .data$FirmwareRev)))
  }

  if(any(grepl("DIAGNOSTIC TEST", df_raw$V1)) |
     any(grepl("CO2", df_raw$V1)) ){
    # # |
    #  (firmware$ASTSampler == 'UPAS_v2_x' & firmware$FirmwareRev>=127)){

    if(firmware$ASTSampler == 'UPAS_v2_x' & firmware$FirmwareRev>=127){

      df_raw_log <- data.table::fread(file,
                                      sep=',',
                                      skip = nrow(df_raw)+10,
                                      header = FALSE,
                                      fill = TRUE,
                                      blank.lines.skip = TRUE,
                                      stringsAsFactors = FALSE)

    }else{

      df_raw_log <- data.table::fread(file,
                                      sep=',',
                                      skip = nrow(df_raw),
                                      header = FALSE,
                                      fill = TRUE,
                                      blank.lines.skip = TRUE,
                                      stringsAsFactors = FALSE)
    }

    if(any(grepl("DIAGNOSTIC TEST", df_raw_log$V1))){
      df_raw_log <- df_raw_log %>%
        dplyr::slice(which(df_raw_log$V1=="DIAGNOSTIC TEST")+1:dplyr::n())

    }else{
      df_raw_log <- df_raw_log %>%
        dplyr::slice(which(df_raw_log$V1=="SAMPLE LOG")+1:dplyr::n())

    }

    df_raw_log <- df_raw_log %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    df_raw <- df_raw %>%
      dplyr::bind_rows(df_raw_log) %>%
      dplyr::distinct(.data$V1, .data$V9, .keep_all = TRUE)


  }

  df_h <- astr::format_ast_header(df_raw)

  df <- astr::format_ast_log(df_h, df_raw, tz_offset, update_names, cols_keep, cols_drop, shiny=shiny)

  return(df)

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

format_ast_log = function(df_h, df_raw, tz_offset = NA, update_names = FALSE, cols_keep = c(), cols_drop = c(), shiny=FALSE) {

  df_cols <- df_raw %>%
    dplyr::slice(which(df_raw$V1=="SAMPLE LOG")+2) %>%
    unlist(use.names = FALSE)


  if(stringr::str_detect(df_h$Firmware, 'UPAS_v2_x')){
    df <- df_raw %>%
      dplyr::slice(which(df_raw$V1=="SAMPLE LOG")+4:dplyr::n())
  }else if(stringr::str_detect(df_h$Firmware, 'SHEARv2_7_2') | stringr::str_detect(df_h$Firmware, 'UPAS_v2_0')){
    df <- df_raw %>%
      dplyr::slice(which(df_raw$V1=="SAMPLE LOG")+3:dplyr::n())
  }


  if(nrow(df)>0){
    colnames(df) <- df_cols

    df <- df[ , colSums(is.na(df)) < nrow(df)]

    if(any(stringr::str_detect(names(df_h),'ASTSampler'))){
      #if(df_h$ASTSampler == 'UPAS_v2_x'){
      if(stringr::str_detect(df_h$Firmware, 'UPAS_v2_x') | stringr::str_detect(df_h$Firmware, 'SHEARv2_7_2')){
        df <- astr::format_upasv2x_log(df_h, df, tz_offset, cols_keep, cols_drop)

      }else if(df_h$ASTSampler == "UPAS_v2_0"){
        if(shiny) {update_names=TRUE}
        df <- astr::format_upasv2_log(df_h, df, update_names=update_names)

      }else{

      }
    }
    if(shiny){df <- astr::shiny_log(df)} #TODO move to own function format_shiny_log so that shiny functionality is not present in normal functions
  }

  return(df)

}
