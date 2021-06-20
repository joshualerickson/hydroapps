#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    if (is.null(suppressMessages(webshot:::find_phantom()))) { webshot::install_phantomjs() },
    # List the first level UI elements here 
    fluidPage(
      tabsetPanel(id = "tabchart_ss",
                  tabPanel("Map", style = "height:92vh;",
      mod_culvert_map_ui("culvert_map_ui_1")),
      tabPanel("Peak Plot", style = "height:92vh;",
      mod_culvert_peak_plot_ui("culvert_peak_plot_ui_1")),
      tabPanel("Culvert Size", style = "height:92vh",tags$style(type = 'text/css', '#culvert_plot {height: calc(100vh - 250px) !important;}'),
      mod_culvert_plotly_ui("culvert_plotly_ui_1")),
      tabPanel("Report", style = "height:92vh",
               mod_culvert_report_ui("culvert_report_ui_1"))
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'hydroapps'
    ),
    shinyjs::useShinyjs()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

