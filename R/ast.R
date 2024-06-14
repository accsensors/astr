#'Read the header data from an Access Sensor Technologies (AST) air sampler
#'log file
#'
#' @param file Any AST air sampler log file name.
#' @param update_names Option to update old sampler names to latest version.
#' @param shiny Option to make TRUE if using function with AST shiny app.
#'
#' @return A data frame with header data in wide format.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2_rev125_filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
#' upasv2_rev125_file <- system.file("extdata", upasv2_rev125_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev125_header <- read_ast_header(upasv2_rev125_file, update_names=FALSE)
#' upasv2_rev130_filename <- 'PS1786_LOG_2023-03-02T21_45_43UTC_DIAGNOSTIC____________.txt'
#' upasv2_rev130_file <- system.file("extdata", upasv2_rev130_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev130_header <- read_ast_header(upasv2_rev130_file, update_names=FALSE)
#' upasv2_rev138_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
#' upasv2_rev138_file <- system.file("extdata", upasv2_rev138_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev138_header <- read_ast_header(upasv2_rev138_file, update_names=FALSE)
#' upasv2x_rev81_filename <- 'PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt'
#' upasv2x_rev81_file <- system.file("extdata", upasv2x_rev81_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev81_header <- read_ast_header(upasv2x_rev81_file, update_names=FALSE)
#' upasv2x_rev117_filename <- 'PSP00030_LOG_2022-05-11T23_24_01UTC_---------------_----------.txt'
#' upasv2x_rev117_file <- system.file("extdata", upasv2x_rev117_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev117_header <- read_ast_header(upasv2x_rev117_file, update_names=FALSE)
#' upasv2x_rev110_diag_filename <- 'PSP00055_LOG_2022-03-24T18_05_32UTC_DIAGNOSTIC________________.txt'
#' upasv2x_rev110_diag_file <- system.file("extdata", upasv2x_rev110_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev110_diag_header <- read_ast_header(upasv2x_rev110_diag_file, update_names=FALSE)
#' upasv2x_rev158_filename <- 'PSP00270_LOG_2024-06-10T21_50_55UTC_name____________eng_______.txt'
#' upasv2x_rev158_file <- system.file("extdata", upasv2x_rev158_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev158_header <- read_ast_header(upasv2x_rev158_file, update_names=FALSE)
#' upasv2x_rev158_diag_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
#' upasv2x_rev158_diag_file <- system.file("extdata", upasv2x_rev158_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev158_diag_header <- read_ast_header(upasv2x_rev158_diag_file, update_names=FALSE)

read_ast_header = function(file, update_names=FALSE, shiny=FALSE) {

  df_h_raw <- astr::make_raw_ast_header(file)

  df_h <- astr::format_ast_header(df_h_raw, update_names=update_names, shiny=shiny)

  return(df_h)
}

#'Generate an data frame of  unformatted header data from an Access Sensor
#'Technologies (AST) air sampler log file
#'
#' @param file Any AST air sampler log file name.
#'
#' @return A data frame of unformatted header data with blank rows removed.
#' @export
#'
#' @examples
#' upasv2_rev125_filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
#' upasv2_rev125_file <- system.file("extdata", upasv2_rev125_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev125_header_raw <- make_raw_ast_header(upasv2_rev125_file)
#' upasv2_rev130_diag_filename <- 'PS1786_LOG_2023-03-02T21_45_43UTC_DIAGNOSTIC____________.txt'
#' upasv2_rev130_diag_file <- system.file("extdata", upasv2_rev130_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev130_diag_header_raw <- make_raw_ast_header(upasv2_rev130_diag_file)
#' upasv2_rev138_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
#' upasv2_rev138_file <- system.file("extdata", upasv2_rev138_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev138_header_raw <- make_raw_ast_header(upasv2_rev138_file)
#' upasv2x_rev81_filename <- 'PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt'
#' upasv2x_rev81_file <- system.file("extdata", upasv2x_rev81_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev81_header_raw <- make_raw_ast_header(upasv2x_rev81_file)
#' upasv2x_rev117_filename <- 'PSP00030_LOG_2022-05-11T23_24_01UTC_---------------_----------.txt'
#' upasv2x_rev117_file <- system.file("extdata", upasv2x_rev117_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev117_header_raw <- make_raw_ast_header(upasv2x_rev117_file)
#' upasv2x_rev110_diag_filename <- 'PSP00055_LOG_2022-03-24T18_05_32UTC_DIAGNOSTIC________________.txt'
#' upasv2x_rev110_diag_file <- system.file("extdata", upasv2x_rev110_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev110_diag_header_raw <- make_raw_ast_header(upasv2x_rev110_diag_file)
#' upasv2x_rev158_filename <- 'PSP00270_LOG_2024-06-10T21_50_55UTC_name____________eng_______.txt'
#' upasv2x_rev158_file <- system.file("extdata", upasv2x_rev158_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev158_header_raw <- make_raw_ast_header(upasv2x_rev158_file)
#' upasv2x_rev158_diag_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
#' upasv2x_rev158_diag_file <- system.file("extdata", upasv2x_rev158_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev158_diag_header_raw <- make_raw_ast_header(upasv2x_rev158_diag_file)

make_raw_ast_header = function(file) {

  nrows_header <- astr::count_header_rows(file)
  if(nrows_header$is_diag == TRUE) {
    # Read only number of non-blank lines to work with blank.line.skip in fread
    # Must add 1 to also read in first header below "DIAGNOSTIC TEST"
    nrow_header_read <- nrows_header$nrow_diag_no_blanks+1
  }else {
    nrow_header_read <- nrows_header$nrow_no_blanks
  }

  df_h_raw <- data.table::fread(file,
                                sep=',',
                                skip = 0,
                                nrows = nrow_header_read,
                                header = FALSE,
                                fill = TRUE,
                                blank.lines.skip = TRUE,
                                stringsAsFactors = FALSE)

  if(nrows_header$is_diag == TRUE){

    # Must add 2 to start on first line of comma delineated diag summary for
    # fread to work properly
    nrow_diag_start <- nrows_header$nrow_diag_with_blanks+2
    # Must subtract 1 from diag length to account for starting on comma
    # delineated portion of diag summary
    nrow_diag_read <- nrows_header$length_diag_no_blanks-1
    df_h_diag <- data.table::fread(file,
                                   sep=',',
                                   skip = nrow_diag_start,
                                   nrows = nrow_diag_read,
                                   header = FALSE,
                                   fill = TRUE,
                                   blank.lines.skip = TRUE,
                                   stringsAsFactors = FALSE)

    # if(any(grepl("DIAGNOSTIC TEST", df_raw_log$V1))){
    #   df_raw_log <- df_raw_log %>%
    #     dplyr::slice(which(df_raw_log$V1=="DIAGNOSTIC TEST")+2:dplyr::n())
    # }else{
    #   df_raw_log <- df_raw_log %>%
    #     dplyr::slice(which(df_raw_log$V1=="SAMPLE LOG")+1:dplyr::n())
    # }
    #
    df_h_diag <- df_h_diag %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    df_h_raw <- df_h_raw %>%
      dplyr::bind_rows(df_h_diag)
  }

  return(df_h_raw)
}


#'Transpose an unformatted header data frame from an Access Sensor
#'Technologies (AST) air sampler log file into wide data
#'
#' @param df_h_raw Any AST air sampler unformatted header data frame created
#' with [make_raw_ast_header()]
#'
#' @return A data frame with header data in wide format.
#' @export
#'
#' @examples
#' upasv2_rev125_filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
#' upasv2_rev125_file <- system.file("extdata", upasv2_rev125_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev125_header_raw <- make_raw_ast_header(upasv2_rev125_file)
#' upasv2_rev125_header_transp <- transpose_raw_ast_header(upasv2_rev125_header_raw)
#' upasv2_rev130_diag_filename <- 'PS1786_LOG_2023-03-02T21_45_43UTC_DIAGNOSTIC____________.txt'
#' upasv2_rev130_diag_file <- system.file("extdata", upasv2_rev130_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev130_diag_header_raw <- make_raw_ast_header(upasv2_rev130_diag_file)
#' upasv2_rev130_diag_header_transp <- transpose_raw_ast_header(upasv2_rev130_diag_header_raw)
#' upasv2_rev138_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
#' upasv2_rev138_file <- system.file("extdata", upasv2_rev138_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev138_header_raw <- make_raw_ast_header(upasv2_rev138_file)
#' upasv2_rev138_header_transp <- transpose_raw_ast_header(upasv2_rev138_header_raw)
#' upasv2x_rev81_filename <- 'PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt'
#' upasv2x_rev81_file <- system.file("extdata", upasv2x_rev81_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev81_header_raw <- make_raw_ast_header(upasv2x_rev81_file)
#' upasv2x_rev81_header_transp <- transpose_raw_ast_header(upasv2x_rev81_header_raw)
#' upasv2x_rev117_filename <- 'PSP00030_LOG_2022-05-11T23_24_01UTC_---------------_----------.txt'
#' upasv2x_rev117_file <- system.file("extdata", upasv2x_rev117_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev117_header_raw <- make_raw_ast_header(upasv2x_rev117_file)
#' upasv2x_rev117_header_transp <- transpose_raw_ast_header(upasv2x_rev117_header_raw)
#' upasv2x_rev110_diag_filename <- 'PSP00055_LOG_2022-03-24T18_05_32UTC_DIAGNOSTIC________________.txt'
#' upasv2x_rev110_diag_file <- system.file("extdata", upasv2x_rev110_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev110_diag_header_raw <- make_raw_ast_header(upasv2x_rev110_diag_file)
#' upasv2x_rev110_diag_header_transp <- transpose_raw_ast_header(upasv2x_rev110_diag_header_raw)
#' upasv2x_rev158_filename <- 'PSP00270_LOG_2024-06-10T21_50_55UTC_name____________eng_______.txt'
#' upasv2x_rev158_file <- system.file("extdata", upasv2x_rev158_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev158_header_raw <- make_raw_ast_header(upasv2x_rev158_file)
#' upasv2x_rev158_header_transp <- transpose_raw_ast_header(upasv2x_rev158_header_raw)
#' upasv2x_rev158_diag_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
#' upasv2x_rev158_diag_file <- system.file("extdata", upasv2x_rev158_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev158_diag_header_raw <- make_raw_ast_header(upasv2x_rev158_diag_file)
#' upasv2x_rev158_diag_header_transp <- transpose_raw_ast_header(upasv2x_rev158_diag_header_raw)

transpose_raw_ast_header = function(df_h_raw) {

  my_cols <- c('V1','V2')

  df_h <- df_h_raw %>%
    dplyr::select(dplyr::all_of(my_cols))

  # df_h <- df_h[df_h$V1 != "",]

  if(any(df_h$V1=='DIAGNOSTIC TEST')){ #
    df_h <- df_h[2:(which(df_h$V1=="DIAGNOSTIC TEST")-1),]

  }else{
    df_h <- df_h[2:(which(df_h$V1=="SAMPLE LOG")-1),]
  }

  remove_names <- c("SAMPLE IDENTIFICATION","SETUP SUMMARY",
                    "SAMPLE IDENTIFICATION","SAMPLE SUMMARY",
                    "CO2 SENSOR CALIBRATION",
                    "MASS FLOW SENSOR CALIBRATION", "CALIBRATION COEFFICIENTS")

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
  }


  if(any(df_h_raw$V1=='DIAGNOSTIC TEST')){

    df_h_diag <- as.data.frame(df_h_raw[(which(df_h_raw$V1=="DIAGNOSTIC TEST")+2):(which(df_h_raw$V1=="SAMPLE LOG")-1),])

    rownames_diag <- c('noFlow','maxDeadhead','maxFlow','minFlow')

    if(stringr::str_detect(df_h$Firmware, 'UPAS_v2_x') | stringr::str_detect(df_h$Firmware, 'SHEARv2_7_2')){

      df_h_diag <- df_h_diag %>%
        dplyr::distinct(.data$V1, .data$V9, .keep_all = TRUE)

      colnames(df_h_diag) <- df_h_diag[6,]

      df_h_diag <- df_h_diag[c(3,7,9,11),c(1:15)] %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.numeric(.x)))

      rownames(df_h_diag) <- rownames_diag

      df_h <- df_h %>%
        dplyr::mutate(MFSDIAGVoutBlocked = df_h_diag$MFSVout[2],
                      MFSDIAGVoutMax = df_h_diag$MFSVout[3],
                      MFSDIAGVoutMin = df_h_diag$MFSVout[4],
                      MFSDIAGMFBlocked = df_h_diag$MassFlow[2],
                      MFSDIAGMFMax = df_h_diag$MassFlow[3],
                      MFSDIAGMFMin = df_h_diag$MassFlow[4],
                      MFSDIAGPumpVBoostMax = df_h_diag$PumpV[3],
                      MFSDIAGPumpVBoostMin = df_h_diag$PumpV[4],
                      MFSDIAGPDeadhead = df_h_diag$FilterDP[2])

    }else if(stringr::str_detect(df_h$Firmware, 'UPAS_v2_0')){
      df_h_diag <- df_h_diag %>%
        dplyr::distinct(.data$V1, .data$V4, .keep_all = TRUE)

      colnames(df_h_diag) <- df_h_diag[6,]

      df_h_diag <- df_h_diag[c(3,7,9,11),c(1:11)] %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.numeric(.x)))

      rownames(df_h_diag) <- rownames_diag

      df_h <- df_h %>%
        dplyr::mutate(MFSDIAGVoutBlocked = df_h_diag$MFSVolt[2],
                      MFSDIAGVoutMax = df_h_diag$MFSVolt[3],
                      MFSDIAGVoutMin = df_h_diag$MFSVolt[4],
                      MFSDIAGMFBlocked = df_h_diag$MassFlow[2],
                      MFSDIAGMFMax = df_h_diag$MassFlow[3],
                      MFSDIAGMFMin = df_h_diag$MassFlow[4],
                      MFSDIAGPumpVBoostMax = df_h_diag$PumpV[3],
                      MFSDIAGPumpVBoostMin = df_h_diag$PumpV[4],
                      MFSDIAGPDeadhead = df_h_diag$FdPdP[2])
    }else {}
  }

  return(df_h)
}

#'Format the header data from an Access Sensor Technologies (AST) air sampler
#'log file
#'
#' @param df_h_raw Any AST air sampler unformatted header data frame created
#' with [make_raw_ast_header()]
#' @param update_names Option to update old sampler names to latest version.
#' @param shiny Option to make TRUE if using function with AST shiny app.
#'
#' @return A data frame with header data in wide format.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' data_ast_header <- format_ast_header(data_ast_raw, update_names=FALSE) #TODO replace this example with just header file
#' upasv2x_rev158_filename <- 'PSP00270_LOG_2024-06-10T21_50_55UTC_name____________eng_______.txt'
#' upasv2x_rev158_file <- system.file("extdata", upasv2x_rev158_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev158_header_raw <- make_raw_ast_header(upasv2x_rev158_file)
#' upasv2x_rev158_header <- format_ast_header(upasv2x_rev158_header_raw, update_names=FALSE)
#' upasv2x_rev158_diag_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
#' upasv2x_rev158_diag_file <- system.file("extdata", upasv2x_rev158_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev158_diag_header_raw <- make_raw_ast_header(upasv2x_rev158_diag_file)
#' upasv2x_rev158_diag_header <- format_ast_header(upasv2x_rev158_diag_header_raw, update_names=FALSE)

format_ast_header = function(df_h_raw, update_names=FALSE, shiny=FALSE) {

  df_h <- astr::transpose_raw_ast_header(df_h_raw)

  if(stringr::str_detect(df_h$Firmware, 'UPAS_v2_x') | stringr::str_detect(df_h$Firmware, 'SHEARv2_7_2')){
    df_h <- astr::format_upasv2x_header(df_h)

  }else if(stringr::str_detect(df_h$Firmware, 'UPAS_v2_0')){
    if(shiny) {update_names=TRUE} #TODO move to own function format_shiny_header so that shiny functionality is not present in normal functions
    df_h <- astr::format_upasv2_header(df_h, update_names=update_names)
  }

  return(df_h)
}


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
