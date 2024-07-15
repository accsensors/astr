#'Formats an Access Sensor Technologies (AST) UPAS v2.1 PLUS log data frame for
#'generating a gps map
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' `format_gps_map_data` formats data from an Access Sensor Technologies
#' UPAS v2.1 PLUS log data frame for the [gps_map] function.
#' This function calculates 30 second averaged PM2.5 or CO2 data and removes
#' variables not necessary for generating the map.
#' If UPASv2 data is provided, data will not be generated for that device.
#' This function calculates a 30 second average for any data logged
#' at a less than 30 second log interval.
#' The UPAS must have been outside with GPS reception and collecting PM2.5 or CO2 data.
#'
#' @inheritParams gps_map
#'
#' @return A modified data frame with 30 second mean data for select variables.
#' @export
#' @importFrom rlang .data
#' @importFrom stats var
#'
#' @examples
#' multiple_upas_logs <- system.file("extdata", package = "astr", mustWork = TRUE) |>
#'     list.files(pattern="^PS.*.txt$", full.names = TRUE) %>%
#'     lapply(read_ast_log, update_names=TRUE) %>%
#'     dplyr::bind_rows()
#'
#' gps_map_data <- format_gps_map_data(multiple_upas_logs, variable="PM2_5MC")
#' gps_map_data <- format_gps_map_data(multiple_upas_logs, variable="CO2")

format_gps_map_data = function(df, variable=c("PM2_5MC", "CO2")) {

  gps_map_data <- NULL

  if(variable %in% colnames(df) & any(variable == c("PM2_5MC", "CO2")) ) {

    gps_map_data <- df %>%
      dplyr::filter(.data$ASTSampler == "UPAS_v2_x") %>%
      dplyr::select(dplyr::any_of(
        c("UPASserial",
          "SampleName",
          "DateTimeLocal",
          "PM2_5MC",
          "CO2",
          "GPSlat",
          "GPSlon")
      )) %>%
      dplyr::mutate(DateTimeLocal_Rounded = lubridate::floor_date(.data$DateTimeLocal, "30 sec")) %>%
      dplyr::group_by(.data$UPASserial,
                      .data$SampleName,
                      .data$DateTimeLocal_Rounded) %>%
      dplyr::mutate(
        mean30PM2_5MC = mean(.data$PM2_5MC, na.rm = T),
        var30PM2_5MC = stats::var(.data$PM2_5MC, na.rm = T),
        mean30CO2 = mean(.data$CO2, na.rm = T),
        var30CO2 = stats::var(.data$CO2, na.rm = T),
        mean30GPSlat = mean(.data$GPSlat, na.rm = T),
        mean30GPSlon = mean(.data$GPSlon, na.rm = T)
      ) %>%
      dplyr::select(-"DateTimeLocal") %>%
      dplyr::distinct()

    if (variable == "PM2_5MC") {
      gps_map_data <- gps_map_data %>%
        dplyr::mutate(aqi = as.factor(
          dplyr::case_when(
            .data$mean30PM2_5MC < 12.0  ~ "Good",
            .data$mean30PM2_5MC < 35.4  ~ "Moderate",
            .data$mean30PM2_5MC < 55.4  ~ "USG",
            .data$mean30PM2_5MC < 150.4 ~ "Unhealthy",
            .data$mean30PM2_5MC < 250.4 ~ "Very Unhealthy",
            TRUE ~ "Hazardous"
          ))) %>%
        dplyr::filter(!is.na(.data$mean30PM2_5MC), !is.na(.data$mean30GPSlat),
          !is.na(.data$mean30GPSlon))

    } else if(variable == "CO2") {
      gps_map_data <- gps_map_data %>%
        # dplyr::mutate(CO2Level = as.factor(
        #   dplyr::case_when(
        #     .data$mean30CO2 < 400  ~ "Average Outdoor",
        #     .data$mean30CO2 < 1000  ~ "Good",
        #     .data$mean30CO2 < 2000  ~ "Moderate",
        #     .data$mean30CO2 < 5000 ~ "Unhealthy",
        #     .data$mean30CO2 < 40000 ~ "Very Unhealthy",
        #     TRUE ~ "Hazardous"
        #   ))) %>%
        dplyr::filter(!is.na(.data$mean30CO2), !is.na(.data$mean30GPSlat),
                      !is.na(.data$mean30GPSlon))
    } else {}

    if(nrow(gps_map_data)==0) {gps_map_data <- NULL}
  }

  if (is.null(gps_map_data)) {
    message("Invalid variable or log file for GPS mapping.
    Use either 'PM2_5MC' or 'CO2' with a UPASv2x log file")
    }

  return(gps_map_data)
}

#'Generate a gps map from an Access Sensor Technologies (AST) UPAS v2.1 PLUS log data frame
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' `gps_map` generates a gps map of 30 second averaged PM2.5 or CO2 data from
#' an Access Sensor Technologies
#' UPAS v2.1 PLUS log data frame.
#' If UPASv2 data is provided, a map will not be generated for that device.
#' This function calculates a 30 second average for any data logged
#' at a less than 30 second log interval.
#' The UPAS must have been outside with GPS reception and collecting PM2.5 or CO2 data.
#'
#' @param df A log data frame returned by the [read_ast_log] function
#' with the argument `update_names = TRUE`.
#' @param variable A column name from the log file. Choose "PM2_5MC" for PM2.5,
#' and "CO2" for CO2 data.
#'
#' @return A gps map of 30 second averaged data for the selected variable.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' multiple_upas_logs <- system.file("extdata", package = "astr", mustWork = TRUE) |>
#'     list.files(pattern="^PS.*.txt$", full.names = TRUE) %>%
#'     lapply(read_ast_log, update_names=TRUE) %>%
#'     dplyr::bind_rows()
#'
#' upasv2x_pm25_map <- gps_map(multiple_upas_logs, variable="PM2_5MC")
#' upasv2x_CO2_map <- gps_map(multiple_upas_logs, variable="CO2")

gps_map = function(df, variable=c("PM2_5MC", "CO2")) {

  gps_map_data <- format_gps_map_data(df, variable=variable)

  if(!is.null(gps_map_data)) {

    sp::coordinates(gps_map_data)<- ~mean30GPSlon + mean30GPSlat
    # crs(gpsPMPlot_data) <- CRS("+init=epsg:4326")

    if((variable == "PM2_5MC")){

      pal <- leaflet::colorBin(
        palette = c("#47AF22", "#EEEE22", "#FF8B14","#FF0000","#800080","#581D00"),
        domain = gps_map_data$mean30PM2_5MC,
        bins = c(0, 12.0, 35.4, 55.4, 150.4, 250.4, Inf)
      )

      gps_leaflet <- leaflet::leaflet(gps_map_data) %>% leaflet::addTiles()

      gps_leaflet <- gps_leaflet %>%
        leaflet::addCircleMarkers(
          color=~pal(mean30PM2_5MC),
          popup=paste("PM2.5 (&#181g/m<sup>3</sup>):", round(gps_map_data$mean30PM2_5MC, digits=2),
                      "<br>","UPAS:", gps_map_data$UPASserial,
                      "<br>","Sample Name:", gps_map_data$SampleName,
                      "<br>","Local Time:", gps_map_data$DateTimeLocal_Rounded), stroke = FALSE,
          radius = 7.5, fillOpacity = 0.7 , group = as.factor(gps_map_data$UPASserial)) %>%
        leaflet::addLayersControl(overlayGroups = (as.factor(gps_map_data$UPASserial)),
                                  options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
        leaflet::addLegend("topright",
                           pal = pal,
                           values = gps_map_data$mean30PM2_5MC,
                           title = "PM2.5 (&#181g/m<sup>3</sup>)",
                           opacity = 0.9)

    } else if((variable == "CO2")){

      pal <- leaflet::colorBin(
        palette = c("#47AF22", "#EEEE22", "#FF8B14","#FF0000","#800080","#581D00"),
        domain = gps_map_data$mean30CO2,
        bins = c(0, 400, 1000, 2000, 5000, 40000, Inf)
      )

      gps_leaflet <- leaflet::leaflet(gps_map_data) %>% leaflet::addTiles()

      gps_leaflet <- gps_leaflet %>%
        leaflet::addCircleMarkers(
          color=~pal(mean30CO2),
          popup=paste("CO2 (ppm):", round(gps_map_data$mean30CO2, digits=2),
                      "<br>","UPAS:", gps_map_data$UPASserial,
                      "<br>","Sample Name:", gps_map_data$SampleName,
                      "<br>","Local Time:", gps_map_data$DateTimeLocal_Rounded), stroke = FALSE,
          radius = 7.5, fillOpacity = 0.7 , group = as.factor(gps_map_data$UPASserial)) %>%
        leaflet::addLayersControl(overlayGroups = (as.factor(gps_map_data$UPASserial)),
                                  options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
        leaflet::addLegend("topright",
                           pal = pal,
                           values = gps_map_data$mean30CO2,
                           title = "CO2 (ppm)",
                           opacity = 0.9)

    } else {gps_leaflet <- NULL}

  } else { # Output error message if no mappable data
    message("No gps mappable data in log file. Must use a UPASv2x log file that
    was collecting PM2.5 or CO2 data and outside with GPS reception.")

    gps_leaflet <- NULL
  }

  return(gps_leaflet)

}

# # Potential future development for compliance checks
####################################################
# check_compliance = function(df) {
#
#   df_30s_mean <- df %>%
#     dplyr::select(
#       dplyr::any_of(c("UPASserial",
#                       "SampleName",
#                       "DateTimeLocal",
#                       "PM2_5MC",
#                       "AccelX",
#                       "AccelY",
#                       "AccelZ",
#                       "CO2",
#                       "GPSlat",
#                       "GPSlon")))%>%
#     dplyr::mutate(datetime_local_rounded = lubridate::floor_date(.data$DateTimeLocal, "30 sec")) %>%
#     dplyr::group_by(.data$UPASserial, .data$SampleName, .data$datetime_local_rounded)%>%
#   #TODO make the mutate check if the variable exists so no errors are thrown for past firmware versions
#   dplyr::mutate(mean30PM2_5MC = mean(.data$PM2_5MC, na.rm = T),
#                 var30PM2_5MC = stats::var(.data$PM2_5MC, na.rm = T),
#                 mean30AccelX = mean(.data$AccelX, na.rm = T),
#                 var30AccelX = stats::var(.data$AccelX, na.rm = T),
#                 mean30AccelY = mean(.data$AccelY, na.rm = T),
#                 var30AccelY = stats::var(.data$AccelY, na.rm = T),
#                 mean30AccelZ = mean(.data$AccelZ, na.rm = T),
#                 var30AccelZ = stats::var(.data$AccelZ, na.rm = T),
#                 mean30CO2 = base::mean(.data$CO2, na.rm = T),
#                 var30CO2 = stats::var(.data$CO2, na.rm = T),
#                 mean30GPSlat = base::mean(.data$GPSlat, na.rm = T),
#                 mean30GPSlon = base::mean(.data$GPSlon, na.rm = T)) %>%
#     dplyr::select(.data$UPASserial, .data$SampleName, .data$datetime_local_rounded, .data$mean30PM2_5MC:.data$mean30GPSlon) %>%
#     dplyr::distinct() %>%
#     dplyr::ungroup() %>%
#     dplyr::group_by(.data$UPASserial, .data$SampleName) %>%
#     dplyr::mutate(compliance = ifelse((.data$var30AccelX > 100) | (.data$var30AccelY > 100) | (.data$var30AccelZ > 100), 1, 0),
#                   compliance_rollmean = ifelse(
#                     as.numeric(zoo::rollapply(.data$compliance, width=20,  FUN = mean, align = "center", na.rm = TRUE, partial=F, fill = NA)) > 0, 1, 0))
#   # dplyr::mutate(mean30AccelX = zoo::rollapply(AccelX, width=30,  FUN = mean, align = "center", na.rm = TRUE, partial=F, fill = NA),
#   #               var30AccelX = zoo::rollapply(AccelX, width=30,  FUN = var, align = "center", na.rm = TRUE, partial=F, fill = NA),
#   #               mean30AccelY = zoo::rollapply(AccelY, width=30,  FUN = mean, align = "center", na.rm = TRUE, partial=F, fill = NA),
#   #               var30AccelY = zoo::rollapply(AccelY, width=30,  FUN = var, align = "center", na.rm = TRUE, partial=F, fill = NA),
#   #               mean30AccelZ = zoo::rollapply(AccelZ, width=30,  FUN = mean, align = "center", na.rm = TRUE, partial=F, fill = NA),
#   #               var30AccelZ = zoo::rollapply(AccelZ, width=30,  FUN = var, align = "center", na.rm = TRUE, partial=F, fill = NA))
#
#   # df_30s_mean <- df_30s_mean %>%
#   #   dplyr::group_by(UPASserial) %>%
#   #   dplyr::summarise(compliance_hours = sum(compliance_rollmean, na.rm = T)/2/60,
#   #                    compliance_percent = sum(compliance_rollmean, na.rm = T)/n())
#
#   return(df_30s_mean)
# }
