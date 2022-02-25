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

  df_h_raw <- data.table::fread(file=file, skip = 0, nrows=100, sep=',',
                          header = FALSE, fill = TRUE, blank.lines.skip = TRUE)

  df_h <- astr::format_ast_header(df_h_raw)

  return(df_h)
}


#'Format the header data from an Access Sensor Technologies (AST) air sampler
#'log file
#'
#' @param df_h_raw Any AST air sampler unformatted header dataframe.
#'
#' @return A data frame with header data in wide format.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' data_ast_header <- format_ast_header(data_ast_raw)

format_ast_header = function(df_h_raw) {

  my_cols <- c('V1','V2')

  df_h <- df_h_raw %>%
    dplyr::select(my_cols)

  # df_h <- df_h[df_h$V1 != "",]

  df_h <- df_h[2:(which(df_h$V1=="SAMPLE LOG")-1),]

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

  df_h <- df_h %>%
    dplyr::mutate_at(c("UPASserial"), as.numeric) %>%
    dplyr::mutate(ast_sampler = sub("-rev.*", "", .data$UPASfirmware),
                  firmware_rev    = sapply(strsplit(.data$UPASfirmware,"-"), `[`, 2),
                  firmware_rev    = as.numeric(gsub("rev_", "", .data$firmware_rev)))

  if(df_h$ast_sampler == 'UPAS_v2_x'){
    df_h <- astr::format_upasv2x_header(df_h)
  }else if(df_h$ast_sampler == "SHEARv2_7_2"){

  }

  return(df_h)
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
#' filename2 <- 'PSP00055_LOG_2022-02-24T19_26_03UTC_test1___________----------.txt'
#' filename <- 'SH00007_LOG_2021-12-13T13_28_41UTC_---------------_-----.txt'
#' file <- system.file("extdata", filename, package = "astr", mustWork = TRUE)
#' data_ast_log <- read_ast_log(file)
#' file2 <- system.file("extdata", filename2, package = "astr", mustWork = TRUE)
#' data_ast_log <- read_ast_log(file2)

read_ast_log = function(file) {

  df_raw <- data.table::fread(file=file, sep=',',
                          header = FALSE, fill = TRUE, blank.lines.skip = TRUE)

  df_h <- astr::format_ast_header(df_raw)


  df <- astr::format_ast_log(df_h=df_h, df=df_raw)

  return(df)

}


#'Extract only the log data from an Access Sensor Technologies (AST) air sampler
#'log file
#'
#' @param df_h An AST air sampler formatted log file header dataframe.
#' @param df_raw Any AST air sampler unformatted log file dataframe.
#'
#' @return A data frame with all log data.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' data_ast_log <- format_ast_log(upasv2x_header, data_ast_raw)

format_ast_log = function(df_h, df_raw) {

  df_cols <- df_raw %>%
    dplyr::slice(which(df_raw$V1=="SAMPLE LOG")+2) %>%
    unlist(use.names = FALSE)

  df <- df_raw %>%
    dplyr::slice(which(df_raw$V1=="SAMPLE LOG")+4:dplyr::n())

  colnames(df) <- df_cols

  if(df_h$ast_sampler == 'UPAS_v2_x'){
    df <- astr::format_upasv2x_log(df_h, df)
  }else if(df_h$ast_sampler == "SHEARv2_7_2"){

  }

  return(df)

}
