#' culvert_plotly UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_culvert_plotly_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    column(4,shinydashboard::box(selectInput(ns("use_reg"), "Do you want to use known bankfull widths?", choices = c("Yes", "No")),
                 numericInput(ns("drain_area"), "Enter Drainage Area (sq.mi)", 2),
                 numericInput(ns("precip_drain"), "Enter Precipitation (in)", 20),
                 numericInput(ns("for_known"), "Enter Percent Forested", 95),
                 numericInput(ns("bf_known"), "Enter Bankfull Width", 5),
                 numericInput(ns("acw_known"), "Enter Active Channel Width", 5),
                 numericInput(ns("geo_known"), "Enter Geographic Factor", 1),
                 actionButton(ns("calculate_culvert"), label = "Calculate"))),
    column(8,plotly::plotlyOutput(ns("culvert_plot"))), DT::dataTableOutput(ns("culvert_table"))
  )
}
    
#' culvert_plotly Server Function
#'
#' @noRd 
mod_culvert_plotly_server <- function(input, output, session, cul, ss_list, peak){
  ns <- session$ns
  
  observeEvent(ss_list$stats,{
    
    precipUpdate <- ss_list$stats %>%
      dplyr::filter(stringr::str_detect(code, "PRECIP"))
    
    daUpdate <- ss_list$stats %>%
      dplyr::filter(stringr::str_detect(code, "DRNAREA|CONTDA"))
    
    
    forUpdate <- ss_list$stats %>%
      dplyr::filter(stringr::str_detect(code, 'FOREST|CANOPY_PCT'))
    
    # This will change the value of input$inText, based on x
    updateTextInput(session, "precip_drain", value = precipUpdate$value)
    updateTextInput(session, "drain_area", value = daUpdate$value)
    updateTextInput(session, "for_known", value = forUpdate$value)
    
  })
  observeEvent(input$calculate_culvert, {
    culvert <- reactive({
      
      drain_area <- isolate(input$drain_area)
      
      precip_drain <- isolate(input$precip_drain)
      
      bf_known <- isolate(input$bf_known)
      
      bd_known <- isolate(input$bd_known)
      
      acw_known <- isolate(input$acw_known)
      
      geo_known <- isolate(input$geo_known)
      
      for_known <- isolate(input$for_known)
      
      
      
      bf_regres <- if(precip_drain < 30) {
        3.99*drain_area^0.441
      } else if (precip_drain > 45) {
        
        7.7*drain_area^0.441
      } else {
        
        6.04*drain_area^0.441
      }
      
      
      if (isolate(input$use_reg) == "Yes") {
        Omang_parrett_hull_flows <- data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                                               basin_char = c(0.037*(drain_area^0.95)*(precip_drain^1.52)*geo_known,
                                                              0.324*(drain_area^0.84)*(precip_drain^1.26)*geo_known,
                                                              0.451*(drain_area^0.82)*(precip_drain^1.22)*geo_known,
                                                              0.594*(drain_area^0.8)*(precip_drain^1.2)*geo_known),
                                               bankfull_width = c(0.041*(drain_area^0.47)*(precip_drain^0.86)*(bf_known^1.14),
                                                                  0.465*(drain_area^0.4)*(precip_drain^0.61)*(bf_known^1.02),
                                                                  0.663*(drain_area^0.38)*(precip_drain^0.58)*(bf_known^1.01),
                                                                  0.899*(drain_area^0.37)*(precip_drain^0.55)*(bf_known^1)),
                                               source = c("Omang, Parrett and Hull"))
        
        parrett_and_johnson <-  data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                                           basin_char = c(0.268*(drain_area^0.927)*(precip_drain^1.6)*(for_known+1)^(-0.508),
                                                          8.5*(drain_area^0.835)*(precip_drain^1.14)*(for_known+1)^(-0.639),
                                                          13.2*(drain_area^0.823)*(precip_drain^1.09)*(for_known+1)^(-0.652),
                                                          18.7*(drain_area^0.812)*(precip_drain^1.06)*(for_known+1)^(-0.664)),
                                           bankfull_width = c(0.281*(bf_known^1.98),
                                                              1.75*(bf_known^1.72),
                                                              2.34*(bf_known^1.69),
                                                              2.99*(bf_known^1.66)),
                                           active_width = c(1.11*(acw_known^1.74),
                                                            5.81*(acw_known^1.51),
                                                            7.61*(acw_known^1.48),
                                                            9.57*(acw_known^1.45)),
                                           source = c("Parrett & Johnson"))
      } else {
        
        Omang_parrett_hull_flows <- data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                                               basin_char = c(0.037*(drain_area^0.95)*(precip_drain^1.52)*geo_known,
                                                              0.324*(drain_area^0.84)*(precip_drain^1.26)*geo_known,
                                                              0.451*(drain_area^0.82)*(precip_drain^1.22)*geo_known,
                                                              0.594*(drain_area^0.8)*(precip_drain^1.2)*geo_known),
                                               bankfull_width = c(0.041*(drain_area^0.47)*(precip_drain^0.86)*(bf_regres^1.14),
                                                                  0.465*(drain_area^0.4)*(precip_drain^0.61)*(bf_regres^1.02),
                                                                  0.663*(drain_area^0.38)*(precip_drain^0.58)*(bf_regres^1.01),
                                                                  0.899*(drain_area^0.37)*(precip_drain^0.55)*(bf_regres^1)),
                                               source = c("Omang, Parrett and Hull"))
        
        parrett_and_johnson <-  data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                                           basin_char = c(0.268*(drain_area^0.927)*(precip_drain^1.6)*(for_known+1)^(-0.508),
                                                          8.5*(drain_area^0.835)*(precip_drain^1.14)*(for_known+1)^(-0.639),
                                                          13.2*(drain_area^0.823)*(precip_drain^1.09)*(for_known+1)^(-0.652),
                                                          18.7*(drain_area^0.812)*(precip_drain^1.06)*(for_known+1)^(-0.664)),
                                           bankfull_width = c(0.281*(bf_regres^1.98),
                                                              1.75*(bf_regres^1.72),
                                                              2.34*(bf_regres^1.69),
                                                              2.99*(bf_regres^1.66)),
                                           active_width = c("No Calculation"),
                                           source = c("Parrett & Johnson"))
      }
      
      #read in some files for reporting and finishing the culvert estimations
      
      culvert_usgs <- cul$peak
      
      stats_usgs_cul <- ss_list$stats
      
      
      if (is.null(culvert_usgs)) {
        
        culvert_usgs <- data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                                   basin_char = rep(NA_real_))
        
      } else {
        
        
        culvert_usgs <- culvert_usgs %>% dplyr::select(basin_char = Value, ReturnInterval) %>%
          dplyr::mutate(source = "USGS Regression") %>% dplyr::filter(ReturnInterval %in% c(2, 25, 50, 100))}
      
      together <- plyr::rbind.fill(Omang_parrett_hull_flows, parrett_and_johnson, culvert_usgs)
      
      together <- together %>% dplyr::mutate(RI = readr::parse_number(ReturnInterval))
  
      if (isolate(input$use_reg) == "No") {
        
        together_long <- together %>% tidyr::pivot_longer(cols = c("basin_char", "bankfull_width"), names_to = "Method") %>%
          dplyr::mutate(dplyr::across(is.numeric, round, 0)) %>% dplyr::mutate(Size = culvert_size(value))
        
      } else {
        together_long <- together %>% tidyr::pivot_longer(cols = c("basin_char", "bankfull_width", "active_width"), names_to = "Method") %>%
          dplyr::mutate(dplyr::across(is.numeric, round, 0)) %>% dplyr::mutate(Size = culvert_size(value))}
      
      
      
    list(together_long=together_long)
    
      })
    output$culvert_plot <- plotly::renderPlotly({
      
      print(plotly::ggplotly(culvert()$together_long %>% ggplot(aes(RI, value, color = source)) +
                       geom_point() +
                       geom_line() +
                       facet_wrap(~Method)+
                       theme_bw()))
      
      
    })
    
    output$culvert_table <- DT::renderDataTable({
      
      DT::datatable(culvert()$together_long %>% 
                      dplyr::filter(RI %in% c(50,100)) %>%
                        dplyr::select(ReturnInterval,Source = source, Method, value, Size), options = list(pageLength = 25))
      
      })
    
    
    ss_list$together_long <- culvert()$together_long
    
    })

  
}
    
## To be copied in the UI
# mod_culvert_plotly_ui("culvert_plotly_ui_1")
    
## To be copied in the server
# callModule(mod_culvert_plotly_server, "culvert_plotly_ui_1")
 
