#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server_station <- function( input, output, session ) {
  # List the first level callModules here
  values <- reactiveValues()
  callModule(mod_station_server, "station_ui_1", values = values)
}
