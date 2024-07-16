###################################
# shiny_header
###################################
test_that("shiny_header has no unnamed columns", {
  multiple_upas_headers <- system.file("extdata", package = "astr", mustWork = TRUE) |>
    list.files(pattern="^PS.*.txt$", full.names = TRUE) %>%
    lapply(read_ast_header, update_names=TRUE) %>%
    dplyr::bind_rows()
  upas_shiny_header <- shiny_header(multiple_upas_headers)
  expect_false(any(grepl("V1", colnames(upas_shiny_header))))
  expect_snapshot(upas_shiny_header)
})

###################################
# shiny_log
###################################
test_that("shiny_log has no unnamed columns", {
  multiple_upas_logs <- system.file("extdata", package = "astr", mustWork = TRUE) |>
    list.files(pattern="^PS.*.txt$", full.names = TRUE) %>%
    lapply(read_ast_log, update_names=TRUE) %>%
    dplyr::bind_rows()

  upas_shiny_log <- shiny_log(multiple_upas_logs)
  expect_false(any(grepl("V1", colnames(upas_shiny_log))))
  expect_snapshot(multiple_upas_logs)
})

###################################
# shiny_axis
###################################
test_that("fract_units = TRUE in shiny_axis changes the units", {
   upasv2x_filename <- 'PSP00270_LOG_2024-06-25T21_37_48UTC_GPS-in-out______----------.txt'
   upasv2x_file <- system.file("extdata", upasv2x_filename, package = "astr", mustWork = TRUE)
   upasv2x_log <- read_ast_log(upasv2x_file, update_names=TRUE) %>%
   shiny_log()
   upasv2x_clm_names <- colnames(upasv2x_log)

  expect_true(grepl("L min\\^-1", shiny_axis("PumpingFlowRate", fract_units = FALSE)))
  expect_true(grepl("L/min", shiny_axis("PumpingFlowRate", fract_units = TRUE)))
})

###################################
# shiny_success_flag
###################################
test_that("shiny_success_flag outputs a mix of FAIL and PASS when all files read in", {
  multiple_upas_headers <- system.file("extdata", package = "astr", mustWork = TRUE) |>
    list.files(pattern="^PS.*.txt$", full.names = TRUE) %>%
    lapply(read_ast_header, update_names=TRUE) %>%
    dplyr::bind_rows()

  upas_headers_flagged <- shiny_success_flag(multiple_upas_headers)
  expect_contains(upas_headers_flagged$SampleSuccess, c("PASS", "FAIL"))

})



