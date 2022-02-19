#'Read the header data from an Access Sensor Technologies (AST) air sampler
#'log file
#'
#' @param file Any AST air sampler  log file name.
#'
#' @return A data frame with header data in wide format.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' filename <- 'PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt'
#' file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
#' data_ast_header <- read_ast_header(file)

read_ast_header = function(file) {

  my_cols <- c('V1','V2')

  df <- data.table::fread(file=file, skip = 0, nrows=100, sep=',',
                          header = FALSE, fill = TRUE, select = my_cols,
                          blank.lines.skip = TRUE)

  df <- df[2:(which(df$V1=="SAMPLE LOG")-1),]

  n_row_header = nrow(df)+3

  remove_names <- c("SAMPLE IDENTIFICATION","SETUP SUMMARY",
                    "SAMPLE IDENTIFICATION","SAMPLE SUMMARY",
                    "MASS FLOW SENSOR CALIBRATION")

  df <- df[ grep(paste(remove_names,collapse="|"), df$V1, invert = TRUE) , ]

  df <- df %>%
    t()

  df <- as.data.frame(df)

  colnames(df) <- df[1, ]
  df <- df[-1, ]
  rownames(df) <- c(1)

  df <- df %>%
    dplyr::mutate_at(c("UPASserial"), as.numeric) %>%
    dplyr::mutate(n_header_rows = n_row_header,
                  ast_sampler = sub("-rev.*", "", .data$UPASfirmware),
                  firmware_rev    = sapply(strsplit(.data$UPASfirmware,"-"), `[`, 2),
                  firmware_rev    = as.numeric(gsub("rev_", "", .data$firmware_rev)))

  if(df$ast_sampler == 'UPAS_v2_x'){
      df <- astr::read_upasv2x_header(df)
  }

  return(df)
}


#'Read the full log data from an Access Sensor Technologies (AST) air sampler
#'log file
#'
#' @param file Any AST air sampler  log file name.
#'
#' @return A data frame with all log data plus some header data appended.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' filename <- 'PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt'
#' file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
#' data_ast_log <- read_ast_log(file)

read_ast_log = function(file) {

  df = astr::read_ast_header(file=file)

  df = astr::read_upasv2x_log(df)

}
