#' Run the Shiny Application
#'
#' @param app A \code{character} indicating what app to run.
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(app,
  ...
) {
  
  if(missing(app)){stop("Need an app")}
  
  if(app == 'streamstats'){
    with_golem_options(
    app = shinyApp(
      ui = app_ui_ss, 
      server = app_server_ss
    ), 
    golem_opts = list(...)
  )
  } else if (app == 'usgs_stats'){
    
    with_golem_options(
      app = shinyApp(
        ui = app_ui_station, 
        server = app_server_station
      ), 
      golem_opts = list(...))
  } else if (app == 'snotel_stats') {
    
    with_golem_options(
      app = shinyApp(
        ui = app_ui_snotel, 
        server = app_server_snotel
      ), 
      golem_opts = list(...))
  } else if (app == 'nldi') {
    with_golem_options(
      app = shinyApp(
        ui = app_ui_nldi, 
        server = app_server_nldi
      ), 
      golem_opts = list(...))
    
  } else if (app == 'nhdplus') {
    with_golem_options(
      app = shinyApp(
        ui = app_ui_hydro, 
        server = app_server_hydro
      ), 
      golem_opts = list(...))
    
  } else if (app == 'happ'){
    with_golem_options(
    app = shinyApp(
   ui = app_ui_happ,
   server = app_server_happ
    ), golem_opts = list(...))
  }
  
}
