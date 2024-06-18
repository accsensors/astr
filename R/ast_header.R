#'Read and format the header data from an Access Sensor Technologies (AST) air sampler
#'log file
#'
#' @description
#' `read_ast_header` reads a raw log file, transposes the log file header to a
#' wide format, and completes device specific formatting. It sets the proper
#' data types for each variable, adds a column to specify the AST sampler type,
#' adds columns to describe the codes associated with variables such as
#' ShutdownMode and PMSensorInterval, and - if `update_names=TRUE` - updates old
#' log file variable names to match the latest variable names.
#'
#' This function can be used in conjuction with lapply() or map() to read in
#' header data from any number of log files and create a data frame that
#' contains a line with the summary information for each sample.
#'
#' @param file Any AST air sampler log file name.
#' @param update_names Option to update old sampler names to latest version.
#' See [format_upasv2_header] for more information.
#' @param shiny Option to make TRUE if using function with AST shiny app.
#'
#' @return A data frame with formatted header data in wide format that is ready
#' for data analysis
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

#'Generate a data frame of unformatted header data from an Access Sensor
#'Technologies (AST) air sampler log file
#'
#' @description
#' `make_raw_ast_header`reads the header data exactly as it
#' appears in the raw log file, except blank rows are removed.
#' The header is NOT transposed to a wide format.
#'
#' @inheritParams read_ast_header
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
#' upasv2x_rev81_header_wide <- transpose_raw_ast_header(upasv2x_rev81_header_raw)
#' upasv2x_rev117_filename <- 'PSP00030_LOG_2022-05-11T23_24_01UTC_---------------_----------.txt'
#' upasv2x_rev117_file <- system.file("extdata", upasv2x_rev117_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev117_header_raw <- make_raw_ast_header(upasv2x_rev117_file)
#' upasv2x_rev117_header_wide <- transpose_raw_ast_header(upasv2x_rev117_header_raw)
#' upasv2x_rev110_diag_filename <- 'PSP00055_LOG_2022-03-24T18_05_32UTC_DIAGNOSTIC________________.txt'
#' upasv2x_rev110_diag_file <- system.file("extdata", upasv2x_rev110_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev110_diag_header_raw <- make_raw_ast_header(upasv2x_rev110_diag_file)
#' upasv2x_rev110_diag_header_wide <- transpose_raw_ast_header(upasv2x_rev110_diag_header_raw)
#' upasv2x_rev158_filename <- 'PSP00270_LOG_2024-06-10T21_50_55UTC_name____________eng_______.txt'
#' upasv2x_rev158_file <- system.file("extdata", upasv2x_rev158_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev158_header_raw <- make_raw_ast_header(upasv2x_rev158_file)
#' upasv2x_rev158_header_wide <- transpose_raw_ast_header(upasv2x_rev158_header_raw)
#' upasv2x_rev158_diag_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
#' upasv2x_rev158_diag_file <- system.file("extdata", upasv2x_rev158_diag_filename, package = "astr", mustWork = TRUE)
#' upasv2x_rev158_diag_header_raw <- make_raw_ast_header(upasv2x_rev158_diag_file)
#' upasv2x_rev158_diag_header_wide <- transpose_raw_ast_header(upasv2x_rev158_diag_header_raw)

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

#'Format a raw header data frame from an Access Sensor
#'Technologies (AST) air sampler log file
#'
#' @description
#' `format_ast_header` transposes the the AST log file header to a wide format
#' and completes device specific formatting. It sets the proper data types for
#' each variable, adds a column to specify the AST sampler type, adds columns to
#' describe the codes associated with variables such as ShutdownMode and
#' PMSensorInterval, and - if `update_names=TRUE` - updates old log file
#' variable names to match the latest variable names.
#'
#' @inheritParams read_ast_header
#' @inheritParams transpose_raw_ast_header
#'
#' @return A data frame with formatted AST air sampler header data in wide
#' format that is ready for data analysis
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' upasv2_rev138_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
#' upasv2_rev138_file <- system.file("extdata", upasv2_rev138_filename, package = "astr", mustWork = TRUE)
#' upasv2_rev138_header_raw <- make_raw_ast_header(upasv2_rev138_file)
#' upasv2_rev138_header <- format_ast_header(upasv2_rev138_header_raw, update_names=TRUE)
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


