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
  }
  
}
