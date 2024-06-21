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

  nrows_header <- astr::count_header_rows(file) # Determine no. of header rows

  # Read header data
  df_h <- data.table::fread(file, sep = ',', header = TRUE, fill = TRUE,
                            skip  = 0,
                            nrows = nrows_header$nrow_header_no_blanks - 1,
                            blank.lines.skip = TRUE, stringsAsFactors = FALSE)

  df_h <- df_h[df_h$`UNITS/NOTES` != "", 1:2] # Remove rows with subheadings

  df_h <- t(df_h) # Transpose
  df_h <- as.data.frame(df_h) # Convert into data frame

  colnames(df_h) <- df_h[1, ] # Assign column names

  df_h <- df_h[-1, ] # Keep only row with data

  rownames(df_h) <- NULL # Remove row name

  df_h[df_h == ''] <- NA # Replace missing values with NA

  # Rename any column that contains 'firmware' to 'Firmware'
  if(sum(grepl("firmware", colnames(df_h), ignore.case=T))==1){

    colnames(df_h) <- gsub(colnames(df_h)[grep("firmware", colnames(df_h),
                                               ignore.case=T)],
                           "Firmware", colnames(df_h))}

  # If the file is a diagnostic file, read the diagnostic test data
  if(nrows_header$is_diag == TRUE){ # If it is a diagnostic file

    diag <- data.table::fread(file, sep = ',', header = FALSE, fill = TRUE,
                              skip  = nrows_header$nrow_header_with_blanks + 2,
                              nrows = nrows_header$nrow_diag_no_blanks - 1,
                              blank.lines.skip = TRUE,
                              stringsAsFactors = FALSE)

    colnames(diag) <- as.character(diag[6,])

    if(grepl("UPAS_v2_0", df_h$Firmware)){
      # Rename variable names to v2_x and SHEAR variable names
      diag <- dplyr::rename(diag, PCB2P=PCBP, MFSVout=MFSVolt, FilterDP=FdPdP)
    }

    diag <- diag[!which(diag$PCB2P %in% c("","(hPa)","PCBP","PCB2P")),]

    diag <- dplyr::mutate(diag, dplyr::across(dplyr::everything(),
                                              \(x) as.numeric(x)))

    rownames(diag) <- c('noFlow','maxDeadhead','maxFlow','minFlow')

    df_h <- dplyr::mutate(df_h,
      MFSDIAGVoutBlocked   = diag[rownames(diag)=="maxDeadhead",]$MFSVout,
      MFSDIAGVoutMax       = diag[rownames(diag)=="maxFlow",]$MFSVout,
      MFSDIAGVoutMin       = diag[rownames(diag)=="minFlow",]$MFSVout,
      MFSDIAGMFBlocked     = diag[rownames(diag)=="maxDeadhead",]$MassFlow,
      MFSDIAGMFMax         = diag[rownames(diag)=="maxFlow",]$MassFlow,
      MFSDIAGMFMin         = diag[rownames(diag)=="minFlow",]$MassFlow,
      MFSDIAGPumpVBoostMax = diag[rownames(diag)=="maxFlow",]$PumpV,
      MFSDIAGPumpVBoostMin = diag[rownames(diag)=="minFlow",]$PumpV,
      MFSDIAGPDeadhead     = diag[rownames(diag)=="maxDeadhead",]$FilterDP)
  }

  df_h <- astr::format_ast_header(df_h, update_names=update_names, shiny=shiny)

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

  firmware <- df_h_raw$Firmware

  if(grepl("UPAS_v2_x", firmware) | grepl("SHEARv2_7_2", firmware)){

    df_h <- astr::format_upasv2x_header(df_h_raw)

  }else if(grepl("UPAS_v2_0", firmware)){

    if(shiny){update_names <- TRUE} #TODO move to own function format_shiny_header so that shiny functionality is not present in normal functions

    df_h <- astr::format_upasv2_header(df_h_raw, update_names = update_names)

  }else if(grepl("HHBv2", firmware)){

    df_h <- astr::format_hhb_header(df_h_raw)

  }

  return(df_h)
}
