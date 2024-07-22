# Allows developer to create an example gps map
# Different example sets can also be run individually for interactive testing.
# Written by: Gabe Neymark

# NOTE: Currently have not used yet for generating public facing example data outputs
## in the "data" folder because it has made more sense to write documentation examples
## with raw log files from "inst/extdata"

load_all()

# Shorter log files from extdata examples
multiple_upas_logs <- system.file("extdata", package = "astr", mustWork = TRUE) |>
  list.files(pattern="^PS.*.txt$", full.names = TRUE) %>%
  lapply(read_ast_log, update_names=TRUE) %>%
  dplyr::bind_rows()

# Longer log files not built with the shiny app
# multiple_upas_logs <- list.files(path = "tests/testthat/logfiles", pattern="^PSP.*.txt$",
#                                  full.names = TRUE) %>%
#   lapply(read_ast_log, update_names=TRUE) %>%
#   dplyr::bind_rows()

gps_map_data <- format_gps_map_data(multiple_upas_logs, variable="PM2_5MC")

heat_map <- gps_map(gps_map_data, variable="PM2_5MC")
heat_map

