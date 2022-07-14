library(data.table)
library(dplyr)
library(usethis)

load_all()

upasv2_filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
upasv2_header <- read_ast_header(upasv2_file, update_names=FALSE)
upasv2_header_updated_names <- read_ast_header(upasv2_file, update_names=TRUE)

upasv2x_filename <- 'PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt'
upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
upasv2x_header <- read_ast_header(upasv2x_file, update_names=FALSE)

upasv2_filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
upasv2_log <- read_ast_log(upasv2_file)
upasv2_log_updated_names <- read_ast_log(upasv2_file,update_names = TRUE)
upasv2_log_units <- read_ast_log(upasv2_file, update_names = TRUE, shiny = FALSE)

upasv2x_filename <- 'PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt'
upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
upasv2x_log <- read_ast_log(upasv2x_file)
upasv2x_log_units <-  read_ast_log(upasv2x_file, shiny=TRUE)

ast_header <- upasv2x_header %>%
  dplyr::bind_rows(upasv2_header)

ast_log <- upasv2x_log %>%
  dplyr::bind_rows(upasv2_log)

ast_header_updated_names <- upasv2x_header %>%
  dplyr::bind_rows(upasv2_header_updated_names)

ast_log_updated_names <- upasv2x_log %>%
  dplyr::bind_rows(upasv2_log_updated_names)

ast_log_units <- upasv2x_log_units %>%
  dplyr::bind_rows(upasv2_log_units)

ast_shiny_summary <- upasv2x_sample_summary(ast_header)
ast_shiny_meta <- upasv2x_sample_meta(ast_header)
ast_shiny_settings <- upasv2x_sample_settings(ast_header)

setdiff(names(upasv2_header),names(upasv2x_header))
setdiff(names(upasv2_header_updated_names),names(upasv2x_header))
# intersect(names(upasv2_header),names(upasv2x_header))
setdiff(names(upasv2_log),names(upasv2x_log))
setdiff(names(upasv2_log_updated_names),names(upasv2x_log))
