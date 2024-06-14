
#' Count header rows in an Access Sensor Technologies (AST) air sampler log file
#'
#' @description
#' `count_header_rows` is a quick way to find where the log file header ends
#' before reading the file into a data frame.
#' It finds the number of rows with and without blank rows included. This is
#' useful for knowing the amount of rows to read or skip when using file read
#' functions from other packages such as fread, read_csv, and read.csv. If
#' working with diagnostic files, the function also indicates the starting row and
#' length of the diagnostic summary. `count_header_rows` also returns a boolean
#' value to indicate if the file is diagnostic.
#'
#' @param file Any AST air sampler raw log file name
#'
#' @return A dataframe including:
#' * Header row counts with and without blank rows
#' * Row count to the start of diagnostic summary if applicable
#' * Number of rows in diagnostic summary if applicable
#' * Boolean indicating if the file is diagnostic
#' @export
#'
#' @examples
#' # Using a standard UPASv2x log file
#' upas_filename <- 'PSP00270_LOG_2024-06-10T21_50_55UTC_name____________eng_______.txt'
#' file <- system.file("extdata", upas_filename, package = "astr", mustWork = TRUE)
#' count_header_rows(file)
#'
#' # Using a diagnostic UPASv2x log file
#' upas_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
#' file <- system.file("extdata", upas_filename, package = "astr", mustWork = TRUE)
#' count_header_rows(file)

count_header_rows = function(file) {
  # Read in first 200 lines of file and find the number of header rows
  # with and without blank lines included
  with_blanks <- readLines(file, n = 200, warn = FALSE)
  no_blanks <- with_blanks[which(with_blanks!="")]
  is_diag <- FALSE

  nrow_with_blanks <- as.numeric(grep("SAMPLE LOG", with_blanks))
  nrow_no_blanks <- as.numeric(grep("SAMPLE LOG", no_blanks))

  if(any(grepl("DIAGNOSTIC TEST", no_blanks))){
    is_diag <- TRUE
    nrow_diag_with_blanks <- as.numeric(grep("DIAGNOSTIC TEST", with_blanks))
    nrow_diag_no_blanks <- as.numeric(grep("DIAGNOSTIC TEST", no_blanks))
    length_diag_with_blanks <- nrow_with_blanks - nrow_diag_with_blanks # includes "SAMPLE LOG" heading
    length_diag_no_blanks <- nrow_no_blanks - nrow_diag_no_blanks # includes "SAMPLE LOG" heading

  }else{
    nrow_diag_with_blanks <- NA
    nrow_diag_no_blanks <- NA
    length_diag_with_blanks <- NA
    length_diag_no_blanks <- NA
  }


  df_row_count <- data.frame(nrow_with_blanks,
                              nrow_no_blanks,
                              nrow_diag_with_blanks,
                              nrow_diag_no_blanks,
                              length_diag_with_blanks,
                              length_diag_no_blanks,
                              is_diag
                              )

  return(df_row_count)
}


