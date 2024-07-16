# Allows developer to choose from various HHBv2 firmware versions to generate example data output.
# Different example sets can also be run individually for interactive testing.
# Written by: Gabe Neymark

# NOTE: Currently have not used yet for generating public facing example data outputs
## in the "data" folder because it has made more sense to write documentation examples
## with raw log files from "inst/extdata"

load_all()

#######################
# hhbv2 header output
#######################

hhb_jan11_filename <- 'HHB00032_LOG_2024-07-01T18_20UTC.csv'
hhb_jan11_file <- system.file("extdata", hhb_jan11_filename, package = "astr", mustWork = TRUE)
hhb_jan11_header_raw <- fread_ast_header(hhb_jan11_file)$header
hhb_jan11_header_wide <- transpose_ast_header(hhb_jan11_header_raw)
hhb_jan11_header <- format_hhb_header(hhb_jan11_header_wide)

hhb_header_raw <- hhb_jan11_header_raw
hhb_header_wide <- hhb_jan11_header_wide
hhb_header <- hhb_jan11_header

usethis::use_data(hhb_header_raw, overwrite = TRUE)
usethis::use_data(hhb_header_wide, overwrite = TRUE)
usethis::use_data(hhb_header, overwrite = TRUE)

#######################
# hhbv2 log output
#######################

hhb_jan11_filename <- 'HHB00032_LOG_2024-07-01T18_20UTC.csv'
hhb_jan11_file <- system.file("extdata", hhb_jan11_filename, package = "astr", mustWork = TRUE)
hhb_jan11_log_raw <- fread_ast_log(hhb_jan11_file)
hhb_jan11_header <- read_ast_header(hhb_jan11_file)
hhb_jan11_log <- format_hhb_log(hhb_jan11_log_raw, hhb_jan11_header)

hhb_log_raw <- hhb_jan11_log_raw
hhb_log <- hhb_jan11_log

usethis::use_data(hhb_log_raw, overwrite = TRUE)
usethis::use_data(hhb_log, overwrite = TRUE)
