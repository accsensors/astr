# Allows developer to choose from various UPASv2x firmware versions to generate example data output.
# Different example sets can also be run individually for interactive testing.
# Written by: Gabe Neymark

# NOTE: Currently have not used yet for generating public facing example data outputs
## in the "data" folder because it has made more sense to write documentation examples
## with raw log files from "inst/extdata"

load_all()

#######################
# upasv2x header output
#######################

upasv2x_shear_filename <- 'SH00009_LOG_2022-02-14T17_02_32UTC_---------------_5VX__.txt'
upasv2x_shear_file <- system.file("extdata", upasv2x_shear_filename, package = "astr", mustWork = TRUE)
upasv2x_shear_header_list <- fread_ast_header(upasv2x_shear_file)
upasv2x_shear_header_raw <- upasv2x_shear_header_list$header
upasv2x_shear_header_wide <- transpose_ast_header(upasv2x_shear_header_list$header)
upasv2x_shear_header <- format_upasv2x_header(upasv2x_shear_header_wide)

upasv2x_rev81_filename <- 'PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt'
upasv2x_rev81_file <- system.file("extdata", upasv2x_rev81_filename, package = "astr", mustWork = TRUE)
upasv2x_rev81_header_list <- fread_ast_header(upasv2x_rev81_file)
upasv2x_rev81_header_raw <- upasv2x_rev81_header_list$header
upasv2x_rev81_header_wide <- transpose_ast_header(upasv2x_rev81_header_list$header)
upasv2x_rev81_header <- format_upasv2x_header(upasv2x_rev81_header_wide)

upasv2x_rev110_diag_filename <- 'PSP00055_LOG_2022-03-24T18_05_32UTC_DIAGNOSTIC________________.txt'
upasv2x_rev110_diag_file <- system.file("extdata", upasv2x_rev110_diag_filename, package = "astr", mustWork = TRUE)
upasv2x_rev110_diag_header_list <- fread_ast_header(upasv2x_rev110_diag_file)
upasv2x_rev110_diag_header_raw <- upasv2x_rev110_diag_header_list$header
upasv2x_rev110_diag_diag_raw <- upasv2x_rev110_diag_header_list$diag
upasv2x_rev110_diag_header_wide <- transpose_ast_header(upasv2x_rev110_diag_header_list$header, upasv2x_rev110_diag_header_list$diag)
upasv2x_rev110_diag_header <- format_upasv2x_header(upasv2x_rev110_diag_header_wide)

upasv2x_rev117_filename <- 'PSP00030_LOG_2022-05-11T23_24_01UTC_---------------_----------.txt'
upasv2x_rev117_file <- system.file("extdata", upasv2x_rev117_filename, package = "astr", mustWork = TRUE)
upasv2x_rev117_header_list <- fread_ast_header(upasv2x_rev117_file)
upasv2x_rev117_header_raw <- upasv2x_rev117_header_list$header
upasv2x_rev117_header_wide <- transpose_ast_header(upasv2x_rev117_header_list$header)
upasv2x_rev117_header <- format_upasv2x_header(upasv2x_rev117_header_wide)

upasv2x_rev157_filename <- 'PSP00270_LOG_2024-06-25T21_37_48UTC_GPS-in-out______----------.txt'
upasv2x_rev157_file <- system.file("extdata", upasv2x_rev157_filename, package = "astr", mustWork = TRUE)
upasv2x_rev157_header_list <- fread_ast_header(upasv2x_rev157_file)
upasv2x_rev157_header_raw <- upasv2x_rev157_header_list$header
upasv2x_rev157_header_wide <- transpose_ast_header(upasv2x_rev157_header_list$header)
upasv2x_rev157_header <- format_upasv2x_header(upasv2x_rev157_header_wide)

upasv2x_rev158_diag_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
upasv2x_rev158_diag_file <- system.file("extdata", upasv2x_rev158_diag_filename, package = "astr", mustWork = TRUE)
upasv2x_rev158_diag_header_list <- fread_ast_header(upasv2x_rev158_diag_file)
upasv2x_rev158_diag_header_raw <- upasv2x_rev158_diag_header_list$header
upasv2x_rev158_diag_diag_raw <- upasv2x_rev158_diag_header_list$diag
upasv2x_rev158_diag_header_wide <- transpose_ast_header(upasv2x_rev158_diag_header_list$header, upasv2x_rev158_diag_header_list$diag)
upasv2x_rev158_diag_header <- format_upasv2x_header(upasv2x_rev158_diag_header_wide)

upasv2x_header_raw <- upasv2x_rev157_header_raw
upasv2x_header_wide <- upasv2x_rev157_header_wide
upasv2x_header <- upasv2x_rev157_header
upasv2x_diag_header <- upasv2x_rev158_diag_header

usethis::use_data(upasv2x_header_raw, overwrite = TRUE)
usethis::use_data(upasv2x_header_wide, overwrite = TRUE)
usethis::use_data(upasv2x_header, overwrite = TRUE)
usethis::use_data(upasv2x_diag_header, overwrite = TRUE)

#######################
# upasv2x log output
#######################

upasv2x_rev81_filename <- 'PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt'
upasv2x_rev81_file <- system.file("extdata", upasv2x_rev81_filename, package = "astr", mustWork = TRUE)
upasv2x_rev81_log_raw <- fread_ast_log(upasv2x_rev81_file)
upasv2x_rev81_header <- read_ast_header(upasv2x_rev81_file, update_names=FALSE)
upasv2x_rev81_log <- format_upasv2x_log(upasv2x_rev81_log_raw, upasv2x_rev81_header, update_names=FALSE)

upasv2x_rev110_diag_filename <- 'PSP00055_LOG_2022-03-24T18_05_32UTC_DIAGNOSTIC________________.txt'
upasv2x_rev110_diag_file <- system.file("extdata", upasv2x_rev110_diag_filename, package = "astr", mustWork = TRUE)
upasv2x_rev110_diag_log_raw <- fread_ast_log(upasv2x_rev110_diag_file)
upasv2x_rev110_diag_header <- read_ast_header(upasv2x_rev110_diag_file, update_names=FALSE)
upasv2x_rev110_diag_log <- format_upasv2x_log(upasv2x_rev110_diag_log_raw, upasv2x_rev110_diag_header, update_names=FALSE)

upasv2x_rev117_filename <- 'PSP00030_LOG_2022-05-11T23_24_01UTC_---------------_----------.txt'
upasv2x_rev117_file <- system.file("extdata", upasv2x_rev117_filename, package = "astr", mustWork = TRUE)
upasv2x_rev117_log_raw <- fread_ast_log(upasv2x_rev117_file)
upasv2x_rev117_header <- read_ast_header(upasv2x_rev117_file, update_names=FALSE)
upasv2x_rev117_log <- format_upasv2x_log(upasv2x_rev117_log_raw, upasv2x_rev117_header, update_names=FALSE)

upasv2x_rev157_filename <- 'PSP00270_LOG_2024-07-11T18_01_22UTC_PM_CO2_Map______----------.txt'
upasv2x_rev157_file <- system.file("extdata", upasv2x_rev157_filename, package = "astr", mustWork = TRUE)
upasv2x_rev157_log_raw <- fread_ast_log(upasv2x_rev157_file)
upasv2x_rev157_header <- read_ast_header(upasv2x_rev157_file, update_names=FALSE)
upasv2x_rev157_log <- format_upasv2x_log(upasv2x_rev157_log_raw, upasv2x_rev157_header, update_names=FALSE)

upasv2x_rev158_diag_filename <- 'PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt'
upasv2x_rev158_diag_file <- system.file("extdata", upasv2x_rev158_diag_filename, package = "astr", mustWork = TRUE)
upasv2x_rev158_diag_log_raw <- fread_ast_log(upasv2x_rev158_diag_file)
upasv2x_rev158_diag_header <- read_ast_header(upasv2x_rev158_diag_file, update_names=FALSE)
upasv2x_rev158_diag_log <- format_upasv2x_log(upasv2x_rev158_diag_log_raw, upasv2x_rev158_diag_header, update_names=FALSE)

upasv2x_log_raw <- upasv2x_rev157_log_raw
upasv2x_log <- upasv2x_rev157_log
upasv2x_diag_log <- upasv2x_rev158_diag_log

usethis::use_data(upasv2x_log_raw, overwrite = TRUE)
usethis::use_data(upasv2x_log, overwrite = TRUE)
usethis::use_data(upasv2x_diag_log, overwrite = TRUE)
