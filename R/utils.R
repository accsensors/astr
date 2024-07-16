
#' Count header rows in an Access Sensor Technologies (AST) air sampler log file
#'
#' @description
#' `count_header_rows()` is a quick way to find where the log file header ends
#' before reading the file into a data frame.
#' It finds the number of rows in the log file header with and without blank
#' rows included. This information is useful for determining the number of
#' header rows to read or skip when using file read functions from other
#' packages such as fread, read_csv, and read.csv. For diagnostic files, this
#' function indicates the starting row and length of the diagnostic summary too.
#' `count_header_rows()` also returns a boolean value to indicate if the file is
#' diagnostic.
#'
#' @param file Any AST air sampler log file name
#'
#' @return A data frame including:
#' * Header row counts with and without blank rows
#' * Row count to the start of diagnostic summary, if applicable
#' * Number of rows in diagnostic summary, if applicable
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
  with_blanks <- readLines(file, n = 200, warn = FALSE) # blank lines included
  no_blanks   <- with_blanks[which(with_blanks!="")]    # blank lines excluded

  # Number of rows before SAMPLE LOG header is reached
  nrow_header_with_blanks <- as.numeric(grep("SAMPLE LOG", with_blanks))
  nrow_header_no_blanks   <- as.numeric(grep("SAMPLE LOG", no_blanks))

  # Assuming the file is not a diagnostic file
  nrow_diag_with_blanks <- 0
  nrow_diag_no_blanks   <- 0
  is_diag <- FALSE

  if(any(grepl("DIAGNOSTIC TEST", no_blanks))){ # If it is a diagnostic file

    is_diag <- TRUE

    # Row where DIAGNOSTIC TEST appears
    nrow_test_with_blanks <- as.numeric(grep("DIAGNOSTIC TEST", with_blanks))
    nrow_test_no_blanks   <- as.numeric(grep("DIAGNOSTIC TEST", no_blanks))

    # Number of rows in DIAGNOSTIC TEST section
    nrow_diag_with_blanks <- nrow_header_with_blanks - nrow_test_with_blanks
    nrow_diag_no_blanks   <- nrow_header_no_blanks   - nrow_test_no_blanks

    # Number of rows to end of header/start of diagnostic test
    nrow_header_with_blanks <- nrow_test_with_blanks
    nrow_header_no_blanks   <- nrow_test_no_blanks
  }

  df <- data.frame(is_diag, nrow_header_with_blanks, nrow_header_no_blanks,
                   nrow_diag_with_blanks, nrow_diag_no_blanks)

  return(df)
}
