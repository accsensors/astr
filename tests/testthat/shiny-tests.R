load_all()

multiple_upas_headers <- list.files(path = "inst/extdata", pattern="^PS.*.txt$",
                                   full.names = TRUE) %>%
         lapply(read_ast_header, update_names=TRUE) %>%
         dplyr::bind_rows()

upas_sample_summary <- shiny_sample_summary(multiple_upas_headers)
upas_sample_settings <- shiny_sample_settings(multiple_upas_headers)
upas_sample_operation <- shiny_sample_operation(multiple_upas_headers)

multiple_upas_logs <- list.files(path = "inst/extdata", pattern="^PSP.*.txt$",
                                 full.names = TRUE) %>%
  lapply(read_ast_log, update_names=TRUE) %>%
  dplyr::bind_rows()

upas_shiny_log <- shiny_log(multiple_upas_logs)

df_30s <- get_30s_mean(multiple_upas_logs)
heat_map <- gps_map(df_30s)
heat_map

