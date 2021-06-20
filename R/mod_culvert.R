#' culvert UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_culvert_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(id = "tabchart_ss",
                tabPanel("Map", style = "height:92vh;",tags$style(type = 'text/css',
                                                                  '#ss_maps {cursor: crosshair !important;}'),
                         leaflet::leafletOutput(ns("ss_maps"), width = "100%", height = "100%"),
                         DT::dataTableOutput(ns("ss_table"))
                ),
                tabPanel("Peak Plot", style = "height:92vh;",
                         plotOutput(ns("ss_peak")),
                         DT::dataTableOutput(ns("ss_peak_table")),
                         box(actionButton(ns("peak"), label = "Peak Flow"))),
                
                tabPanel("Culvert Size", style = "height:92vh",tags$style(type = 'text/css', '#culvert_plot {height: calc(100vh - 250px) !important;}'),
                         column(4,box(selectInput(ns("use_reg"), "Do you want to use known bankfull widths?", choices = c("Yes", "No")),
                                      numericInput(ns("drain_area"), "Enter Drainage Area (sq.mi)", 2),
                                      numericInput(ns("precip_drain"), "Enter Precipitation (in)", 20),
                                      numericInput(ns("for_known"), "Enter Percent Forested", 95),
                                      numericInput(ns("bf_known"), "Enter Bankfull Width", 5),
                                      numericInput(ns("acw_known"), "Enter Active Channel Width", 5),
                                      numericInput(ns("geo_known"), "Enter Geographic Factor", 1),
                                      actionButton(ns("calculate_culvert"), label = "Calculate"))),
                         column(8,plotly::plotlyOutput(ns("culvert_plot"))), DT::dataTableOutput("culvert_table")),
                tabPanel("Report", style = "height:92vh",
                         textInput(ns("author"), "Enter Author."),
                         textInput(ns("drain_name"), "Enter a Name or ID for the drain point location."),
                         radioButtons(ns('format'), 'Document format', c('HTML', 'Word'),
                                      inline = TRUE),
                         downloadButton(ns("report_culvert"), "Generate report"))
                
    )
  )
}
    
#' culvert Server Function
#'
#' @noRd 
mod_culvert_server <- function(input, output, session, ss_list, cul){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_culvert_ui("culvert_ui_1")
    
## To be copied in the server
# callModule(mod_culvert_server, "culvert_ui_1")
 
