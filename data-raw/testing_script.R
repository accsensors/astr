# library(data.table)
# library(dplyr)
# library(usethis)
# library(astr)

#TODO update examples to more recent log files and firmware
#TODO log file averaging and mapping functions from shiny_ast.R

load_all()

upasv2_filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
upasv2_header <- read_ast_header(upasv2_file, update_names=FALSE)
upasv2_header_updated_names <- read_ast_header(upasv2_file, update_names=TRUE)
upasv2_header_shiny <- read_ast_header(upasv2_file, shiny=TRUE)

upasv2x_filename <- 'PSP00066_LOG_2022-09-02T18_03_56UTC_bike_test_1s____----------.txt'
upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
upasv2x_header <- read_ast_header(upasv2x_file, update_names=FALSE)
upasv2x_header_shiny <- read_ast_header(upasv2x_file, shiny=TRUE)


upasv2_filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
upasv2_log <- read_ast_log(upasv2_file)
upasv2_log_updated_names <- read_ast_log(upasv2_file,update_names = TRUE)
upasv2_log_shiny <- read_ast_log(upasv2_file, update_names = TRUE, shiny = TRUE)

upasv2x_filename <- 'PSP00066_LOG_2022-09-02T18_03_56UTC_bike_test_1s____----------.txt'
upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
upasv2x_log <- read_ast_log(upasv2x_file)
upasv2x_log_shiny <-  read_ast_log(upasv2x_file, shiny=TRUE)

ast_header <- upasv2x_header %>%
  dplyr::bind_rows(upasv2_header)

ast_log <- upasv2x_log %>%
  dplyr::bind_rows(upasv2_log)

ast_header_updated_names <- upasv2x_header %>%
  dplyr::bind_rows(upasv2_header_updated_names)

ast_log_updated_names <- upasv2x_log %>%
  dplyr::bind_rows(upasv2_log_updated_names)

ast_log_shiny <- upasv2x_log_shiny %>%
  dplyr::bind_rows(upasv2_log_shiny)

ast_header_shiny <- upasv2x_header_shiny %>%
  dplyr::bind_rows(upasv2_header_shiny)

ast_shiny_summary <- upasv2x_sample_summary(ast_header, shiny=TRUE)
ast_shiny_meta <- upasv2x_sample_meta(ast_header, shiny=TRUE)
ast_shiny_settings <- upasv2x_sample_settings(ast_header, shiny=TRUE)

setdiff(names(upasv2_header),names(upasv2x_header))
setdiff(names(upasv2_header_updated_names),names(upasv2x_header))
# intersect(names(upasv2_header),names(upasv2x_header))
setdiff(names(upasv2_log),names(upasv2x_log))
setdiff(names(upasv2_log_updated_names),names(upasv2x_log))

df_30s <- get_30s_mean(upasv2x_log_shiny)
heat_map <- gps_map(df_30s)
heat_map

