# Allows developer to choose from various UPASv2 firmware versions to generate example data output.
# Different example sets can also be run individually for interactive testing.
# Written by: Gabe Neymark

# NOTE: Currently have not used yet for generating public facing example data outputs
## in the "data" folder because it has made more sense to write documentation examples
## with raw log files from "inst/extdata"

load_all()

#######################
# upasv2 header output
#######################

upasv2_rev100_filename <- 'PS1422_LOG_2020-06-02T18_26_25UTC_rev100-norm________---.txt'
upasv2_rev100_file <- system.file("extdata", upasv2_rev100_filename, package = "astr", mustWork = TRUE)
upasv2_rev100_header_list <- fread_ast_header(upasv2_rev100_file)
upasv2_rev100_header_raw <- upasv2_rev100_header_list$header
upasv2_rev100_header_wide <- transpose_ast_header(upasv2_rev100_header_list$header)
upasv2_rev100_header <- format_upasv2_header(upasv2_rev100_header_wide)

upasv2_rev100_diag_filename <- 'PS1422_LOG_2020-06-02T18_29_11UTC_DIAGNOSTIC____________.txt'
upasv2_rev100_diag_file <- system.file("extdata", upasv2_rev100_diag_filename, package = "astr", mustWork = TRUE)
upasv2_rev100_diag_header_list <- fread_ast_header(upasv2_rev100_diag_file)
upasv2_rev100_diag_header_raw <- upasv2_rev100_diag_header_list$header
upasv2_rev100_diag_diag_raw <- upasv2_rev100_diag_header_list$diag
upasv2_rev100_diag_header_wide <- transpose_ast_header(upasv2_rev100_diag_header_list$header, upasv2_rev100_diag_header_list$diag)
upasv2_rev100_diag_header <- format_upasv2_header(upasv2_rev100_diag_header_wide)

upasv2_rev125_filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
upasv2_rev125_file <- system.file("extdata", upasv2_rev125_filename, package = "astr", mustWork = TRUE)
upasv2_rev125_header_list <- fread_ast_header(upasv2_rev125_file)
upasv2_rev125_header_raw <- upasv2_rev125_header_list$header
upasv2_rev125_header_wide <- transpose_ast_header(upasv2_rev125_header_list$header)
upasv2_rev125_header <- format_upasv2_header(upasv2_rev125_header_wide)

upasv2_rev130_diag_filename <- 'PS1786_LOG_2023-03-02T21_45_43UTC_DIAGNOSTIC____________.txt'
upasv2_rev130_diag_file <- system.file("extdata", upasv2_rev130_diag_filename, package = "astr", mustWork = TRUE)
upasv2_rev130_diag_header_list <- fread_ast_header(upasv2_rev130_diag_file)
upasv2_rev130_diag_header_raw <- upasv2_rev130_diag_header_list$header
upasv2_rev130_diag_diag_raw <- upasv2_rev130_diag_header_list$diag
upasv2_rev130_diag_header_wide <- transpose_ast_header(upasv2_rev130_diag_header_list$header, upasv2_rev130_diag_header_list$diag)
upasv2_rev130_diag_header <- format_upasv2_header(upasv2_rev130_diag_header_wide)

upasv2_rev138_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
upasv2_rev138_file <- system.file("extdata", upasv2_rev138_filename, package = "astr", mustWork = TRUE)
upasv2_rev138_header_list <- fread_ast_header(upasv2_rev138_file)
upasv2_rev138_header_raw <- upasv2_rev138_header_list$header
upasv2_rev138_header_wide <- transpose_ast_header(upasv2_rev138_header_list$header)
upasv2_rev138_header <- format_upasv2_header(upasv2_rev138_header_wide)

upasv2_header_raw <- upasv2_rev138_header_raw
upasv2_header_wide <- upasv2_rev138_header_wide
upasv2_header <- upasv2_rev138_header
upasv2_diag_header <- upasv2_rev130_diag_header

usethis::use_data(upasv2_header_raw, overwrite = TRUE)
usethis::use_data(upasv2_header_wide, overwrite = TRUE)
usethis::use_data(upasv2_header, overwrite = TRUE)
usethis::use_data(upasv2_diag_header, overwrite = TRUE)

#######################
# upasv2 log output
#######################

upasv2_rev100_filename <- 'PS1422_LOG_2020-06-02T18_26_25UTC_rev100-norm________---.txt'
upasv2_rev100_file <- system.file("extdata", upasv2_rev100_filename, package = "astr", mustWork = TRUE)
upasv2_rev100_log_raw <- fread_ast_log(upasv2_rev100_file)
upasv2_rev100_header <- read_ast_header(upasv2_rev100_file, update_names=TRUE)
upasv2_rev100_log <- format_upasv2_log(upasv2_rev100_log_raw, upasv2_rev100_header, update_names=TRUE)

upasv2_rev100_diag_filename <- 'PS1422_LOG_2020-06-02T18_29_11UTC_DIAGNOSTIC____________.txt'
upasv2_rev100_diag_file <- system.file("extdata", upasv2_rev100_diag_filename, package = "astr", mustWork = TRUE)
upasv2_rev100_diag_log_raw <- fread_ast_log(upasv2_rev100_diag_file)
upasv2_rev100_diag_header <- read_ast_header(upasv2_rev100_diag_file, update_names=FALSE)
upasv2_rev100_diag_log <- format_upasv2_log(upasv2_rev100_diag_log_raw, upasv2_rev100_diag_header)

upasv2_rev125_filename <- 'PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt'
upasv2_rev125_file <- system.file("extdata", upasv2_rev125_filename, package = "astr", mustWork = TRUE)
upasv2_rev125_log_raw <- fread_ast_log(upasv2_rev125_file)
upasv2_rev125_header <- read_ast_header(upasv2_rev125_file, update_names=TRUE)
upasv2_rev125_log <- format_upasv2_log(upasv2_rev125_log_raw, upasv2_rev125_header, update_names=TRUE)

upasv2_rev130_diag_filename <- 'PS1786_LOG_2023-03-02T21_45_43UTC_DIAGNOSTIC____________.txt'
upasv2_rev130_diag_file <- system.file("extdata", upasv2_rev130_diag_filename, package = "astr", mustWork = TRUE)
upasv2_rev130_diag_log_raw <- fread_ast_log(upasv2_rev130_diag_file)
upasv2_rev130_diag_header <- read_ast_header(upasv2_rev130_diag_file, update_names=FALSE)
upasv2_rev130_diag_log <- format_upasv2_log(upasv2_rev130_diag_log_raw, upasv2_rev130_diag_header)

upasv2_rev138_filename <- 'PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt'
upasv2_rev138_file <- system.file("extdata", upasv2_rev138_filename, package = "astr", mustWork = TRUE)
upasv2_rev138_log_raw <- fread_ast_log(upasv2_rev138_file)
upasv2_rev138_header <- read_ast_header(upasv2_rev138_file, update_names=FALSE)
upasv2_rev138_log <- format_upasv2_log(upasv2_rev138_log_raw, upasv2_rev138_header)

upasv2_log_raw <- upasv2_rev138_log_raw
upasv2_log <- upasv2_rev138_log
upasv2_diag_log <- upasv2_rev130_diag_log

usethis::use_data(upasv2_log_raw, overwrite = TRUE)
usethis::use_data(upasv2_log, overwrite = TRUE)
usethis::use_data(upasv2_diag_log, overwrite = TRUE)
