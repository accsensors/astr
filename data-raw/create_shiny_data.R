# Allows developer to create example shiny data
# Different example sets can also be run individually for interactive testing.
# Written by: Gabe Neymark

# NOTE: Currently have not used yet for generating public facing example data outputs
## in the "data" folder because it has made more sense to write documentation examples
## with raw log files from "inst/extdata". Shiny data examples also do not need
## to be public facing.

load_all()

multiple_upas_headers <- system.file("extdata", package = "astr", mustWork = TRUE) |>
  list.files(pattern="^PS.*.txt$", full.names = TRUE) %>%
  lapply(read_ast_header, update_names=TRUE) %>%
  dplyr::bind_rows()

upas_shiny_sample_summary <- shiny_sample_summary(multiple_upas_headers)
upas_shiny_sample_settings <- shiny_sample_settings(multiple_upas_headers)
upas_shiny_sample_operation <- shiny_sample_operation(multiple_upas_headers)

usethis::use_data(upas_shiny_sample_summary, overwrite = TRUE)
usethis::use_data(upas_shiny_sample_settings, overwrite = TRUE)
usethis::use_data(upas_shiny_sample_operation, overwrite = TRUE)
