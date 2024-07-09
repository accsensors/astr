#TODO log file averaging and mapping functions from shiny_ast.R

load_all()

upasv2_filename <- 'PS1771_LOG_2024-06-13T21_31_26UTC_DIAGNOSTIC____________.txt'
upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
upasv2_header <- read_ast_header(upasv2_file, update_names=FALSE)
upasv2_header_updatednames <- read_ast_header(upasv2_file, update_names=TRUE)
upasv2_header_shiny <- read_ast_header(upasv2_file, shiny=TRUE)

upasv2x_filename <- 'PSP00270_LOG_2024-06-10T21_50_55UTC_name____________eng_______.txt'
upasv2x_file <- system.file("tests/testthat/logfiles", upasv2x_filename, package = "astr", mustWork = TRUE)
upasv2x_header <- read_ast_header(upasv2x_file, shiny=TRUE)
upasv2x_shiny_summary <- shiny_sample_summary(upasv2x_header)

upasv2_filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
upasv2_file <- system.file("extdata", upasv2_filename, package = "astr", mustWork = TRUE)
upasv2_log <- read_ast_log(upasv2_file)
upasv2_log_updated_names <- read_ast_log(upasv2_file,update_names = TRUE)
upasv2_log_shiny <- read_ast_log(upasv2_file, update_names = TRUE, shiny = TRUE)

upasv2x_filename <- 'PSP00270_LOG_2024-06-10T21_50_55UTC_name____________eng_______.txt'
upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
upasv2x_log <- read_ast_log(upasv2x_file)
upasv2x_log_shiny <-  read_ast_log(upasv2x_file, shiny=TRUE)

multiple_upas_headers <- list.files(path = "inst/extdata", pattern="^PS.*.txt$",
                                   full.names = TRUE) %>%
         lapply(read_ast_header, update_names=TRUE) %>%
         dplyr::bind_rows()

upas_sample_summary <- shiny_sample_summary(multiple_upas_headers)
upas_sample_settings <- shiny_sample_settings(multiple_upas_headers)
upas_sample_operation <- shiny_sample_operation(multiple_upas_headers)

multiple_upas_logs <- list.files(path = "inst/extdata", pattern="^PS.*.txt$",
                                 full.names = TRUE) %>%
  lapply(read_ast_log, update_names=TRUE) %>%
  dplyr::bind_rows()
upas_shiny_log <- shiny_log(multiple_upas_logs)


ast_header <- dplyr::bind_rows(upasv2x_header, upasv2_header)

ast_log <- dplyr::bind_rows(upasv2x_log, upasv2_log)

ast_header_updated_names <- dplyr::bind_rows(upasv2x_header, upasv2_header_updated_names)

ast_log_updated_names <- dplyr::bind_rows(upasv2x_log, upasv2_log_updated_names)

ast_log_shiny <- dplyr::bind_rows(upasv2x_log_shiny, upasv2_log_shiny)

ast_header_shiny <- dplyr::bind_rows(upasv2x_header_shiny, upasv2_header_shiny)

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

