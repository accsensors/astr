#'Read the header data from an Access Sensor Technologies (AST) air sampler
#'log file
#'
#' @param file Any AST air sampler  log file name.
#' @param update_names Option to update old sampler names to latest version.
#'
#' @return A data frame with header data in wide format.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2_filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
#' upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
#' upasv2_header <- read_ast_header(upasv2_file, update_names=FALSE)
#' upasv2x_filename <- 'PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt'
#' upasv2x_filename <- 'PSP00030_LOG_2022-05-11T23_24_01UTC_---------------_----------.txt'
#' upasv2x_filename <- 'PSP00010_LOG_2022-05-13T17_29_29UTC_DIAG#___________----------.txt'
#' upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
#' upasv2x_header <- read_ast_header(upasv2x_file, update_names=FALSE)
#' upasv2x_diag_filename <- 'PSP00055_LOG_2022-03-24T18_05_32UTC_DIAGNOSTIC________________.txt'
#' upasv2x_diag_file <- system.file("extdata", upasv2x_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2x_diag_header <- read_ast_header(upasv2x_diag_file, update_names=FALSE)

read_ast_header = function(file, update_names=FALSE) {

  df_h_raw <- data.table::fread(file=file, skip = 0, nrows=100, sep=',',
                          header = FALSE, fill = TRUE, blank.lines.skip = TRUE,
                          stringsAsFactors = FALSE)


  if(any(grepl("DIAGNOSTIC TEST", df_h_raw$V1)) | any(grepl("CO2", df_h_raw$V1))){

    df_raw_log <- data.table::fread(file=file,
                                    sep=',',
                                    skip = nrow(df_h_raw),
                                    header = FALSE,
                                    fill = TRUE,
                                    blank.lines.skip = TRUE,
                                    stringsAsFactors = FALSE)

    if(any(grepl("DIAGNOSTIC TEST", df_raw_log$V1))){
      df_raw_log <- df_raw_log %>%
        dplyr::slice(which(df_raw_log$V1=="DIAGNOSTIC TEST")+2:dplyr::n())
    }else{
      df_raw_log <- df_raw_log %>%
        dplyr::slice(which(df_raw_log$V1=="SAMPLE LOG")+1:dplyr::n())
    }

    df_raw_log <- df_raw_log %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    df_h_raw <- df_h_raw %>%
      dplyr::bind_rows(df_raw_log) %>%
      dplyr::distinct(V1,V9, .keep_all = TRUE)


  }

  df_h <- astr::format_ast_header(df_h_raw, update_names)

  return(df_h)
}


#'Format the header data from an Access Sensor Technologies (AST) air sampler
#'log file
#'
#' @param df_h_raw Any AST air sampler unformatted header dataframe.
#' @param update_names Option to update old sampler names to latest version.
#'
#' @return A data frame with header data in wide format.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' data_ast_header <- format_ast_header(data_ast_raw, update_names=FALSE)

format_ast_header = function(df_h_raw, update_names=FALSE) {

  my_cols <- c('V1','V2')

  df_h <- df_h_raw %>%
    dplyr::select(my_cols)

  # df_h <- df_h[df_h$V1 != "",]

  if(any(df_h$V1=='DIAGNOSTIC TEST')){ #
    df_h <- df_h[2:(which(df_h$V1=="DIAGNOSTIC TEST")-1),]

  }else{
    df_h <- df_h[2:(which(df_h$V1=="SAMPLE LOG")-1),]
  }

  remove_names <- c("SAMPLE IDENTIFICATION","SETUP SUMMARY",
                    "SAMPLE IDENTIFICATION","SAMPLE SUMMARY",
                    "MASS FLOW SENSOR CALIBRATION")

  df_h <- df_h[ grep(paste(remove_names,collapse="|"), df_h$V1, invert = TRUE) , ]

  df_h <- df_h %>%
    t()

  df_h <- as.data.frame(df_h)

  colnames(df_h) <- df_h[1, ]
  df_h <- df_h[-1, ]
  rownames(df_h) <- c(1)

  if(sum(stringr::str_detect(names(df_h),'firmware'))==1){
    df_h <- df_h %>%
      dplyr::rename('Firmware' = names(df_h[stringr::str_detect(names(df_h), 'firmware')]) )

      if(stringr::str_detect(df_h$Firmware, 'UPAS_v2_x') | stringr::str_detect(df_h$Firmware, 'SHEARv2_7_2')){

        if(any(df_h_raw$V1=='DIAGNOSTIC TEST')){
          df_h_diag <- as.data.frame(df_h_raw[(which(df_h_raw$V1=="DIAGNOSTIC TEST")+2):(which(df_h_raw$V1=="SAMPLE LOG")-1),]) %>%
            dplyr::distinct(V1,V9, .keep_all = TRUE)


          colnames(df_h_diag) <- df_h_diag[6,]

          df_h_diag <- df_h_diag[c(3,7,9,11),c(1:15)] %>%
            dplyr::mutate(across(everything(), ~ as.numeric(.x)))

          rownames(df_h_diag) <- c('noFlow','maxDeadhead','minFlow','maxFlow')

          df_h <- df_h %>%
            dplyr::mutate(MFSDIAGVoutBlocked = df_h_diag$MFSVout[2],
                          MFSDIAGVoutMin = df_h_diag$MFSVout[3],
                          MFSDIAGVoutMax = df_h_diag$MFSVout[4],
                          MFSDIAGMFBlocked = df_h_diag$MassFlow[2],
                          MFSDIAGMFMin = df_h_diag$MassFlow[3],
                          MFSDIAGMFMax = df_h_diag$MassFlow[4],
                          MFSDIAGPumpVBoostMin = df_h_diag$PumpV[3],
                          MFSDIAGPumpVBoostMax = df_h_diag$PumpV[4],
                          MFSDIAGPDeadhead = df_h_diag$FilterDP[2])
        }

        df_h <- astr::format_upasv2x_header(df_h)

      }else if(stringr::str_detect(df_h$Firmware, 'UPAS_v2_0')){

        df_h <- astr::format_upasv2_header(df_h, update_names)

      }
    # else if(stringr::str_detect(df_h$Firmware, 'SHEARv2_7_2')){
    #
    #   }

  }
  return(df_h)
}


#'Read the full log data from an Access Sensor Technologies (AST) air sampler
#'log file
#'
#' @param file Any AST air sampler  log file name.
#' @param tz_offset Pass an option timezone offset

#' @return A data frame with all log data plus some header data appended.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2_filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
#' upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
#' upasv2_log <- read_ast_log(upasv2_file)
#' upasv2x_filename <- 'PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt'
#' upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
#' upasv2x_log <- read_ast_log(upasv2x_file)
#' filename2 <- 'SH00007_LOG_2021-12-13T13_28_41UTC_---------------_-----.txt'
#' file2 <- system.file("extdata", filename2, package = "astr", mustWork = TRUE)
#' data_ast_log <- read_ast_log(file2)
#' data_ast_log <- read_ast_log(file2, cols_keep = c("SampleTime","UnixTime","DateTimeUTC","DateTimeLocal","PM2_5MC"))

read_ast_log = function(file, tz_offset = NA, update_names = FALSE, cols_keep = c(), cols_drop = c()) {

  df_raw <- data.table::fread(file=file,
                              sep=',',
                              header = FALSE,
                              fill = TRUE,
                              blank.lines.skip = TRUE,
                              stringsAsFactors = FALSE)

  if(any(grepl("DIAGNOSTIC TEST", df_raw$V1)) | any(grepl("CO2", df_raw$V1))){

    df_raw_log <- data.table::fread(file=file,
                                    sep=',',
                                    skip = nrow(df_raw),
                                    header = FALSE,
                                    fill = TRUE,
                                    blank.lines.skip = TRUE,
                                    stringsAsFactors = FALSE)

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
      dplyr::distinct(V1,V9, .keep_all = TRUE)


  }

  df_h <- astr::format_ast_header(df_raw)

  df <- astr::format_ast_log(df_h, df_raw, tz_offset, update_names, cols_keep, cols_drop)

  return(df)

}


#'Extract only the log data from an Access Sensor Technologies (AST) air sampler
#'log file
#'
#' @param df_h An AST air sampler formatted log file header dataframe.
#' @param df_raw Any AST air sampler unformatted log file dataframe.
#' @param tz_offset Pass an option timezone offset
#'
#' @return A data frame with all log data.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' data_ast_log <- format_ast_log(upasv2x_header, data_ast_raw)

format_ast_log = function(df_h, df_raw, tz_offset = NA, update_names = FALSE, cols_keep = c(), cols_drop = c()) {

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
        df <- astr::format_upasv2_log(df_h, df, update_names = update_names)

      }else{

      }
    }
  }

  return(df)

}
