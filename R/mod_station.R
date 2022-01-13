#' station UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom dplyr filter select mutate slice_max ungroup rename
#' @importFrom ggplot2 geom_smooth geom_point geom_line aes
#' @importFrom grDevices hcl.colors
#' @importFrom promises "%>%" future_promise
#' 
mod_station_ui <- function(id){
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns('leaf_map'), height = 950) %>% shinycssloaders::withSpinner()
  )
}
    
#' station Server Function
#'
#' @noRd 
mod_station_server <- function(input, output, session, values){
  ns <- session$ns
  
  #add a path we'll keep referencing
  
  #starting leaflet map
  output$leaf_map <- leaflet::renderLeaflet({
    
    base_map() %>%
      leaflet::addControl(html = shinyWidgets::pickerInput(
        ns('location_map'), 'Select a state to get started!',
        choices = c("", as.character(state.name)),
                    choicesOpt = list(
                      style = rep(("font-weight: bold;font-family: 'Montserrat', sans-serif;"),51)))) %>%
      leaflet::setView(lat = 37.0902, lng = -95.7129, zoom = 5)  %>%
      leaflet::hideGroup(group = 'Hydrography') %>%
      leaflet::addLayersControl(baseGroups = c("Esri.WorldImagery", "CartoDB.Positron",
                                      "OpenStreetMap", "CartoDB.DarkMatter", "OpenTopoMap"),
                       overlayGroups = c("Hydrography"))
    
 
    
  })
  
  #leaflet proxy to update map with stations
  
  observeEvent(input$location_map, {
    req(nchar(input$location_map)>1)
    withProgress(value = 0.5, message = paste0('loading ', input$location_map, ' gauging stations...'),{
    state_names <- data.frame(stn = state.name, stab = state.abb)
    state_select <- state_names %>% filter(stn == input$location_map) %>% dplyr::pull(stab)
    
    current <- dataRetrieval::whatNWISsites(stateCD = state_select) %>% filter(nchar(site_no) <= 8)
    
    current <- current %>% filter(site_no %in% hydroapps::usgs_sites_df$site_no)
    
    labs_current <- as.list(current$station_nm)
    
    leaflet::leafletProxy("leaf_map", session) %>%
      leaflet::addAwesomeMarkers(data = current,lng = current$dec_long_va, 
                          lat = current$dec_lat_va,
                          icon = leaflet::makeAwesomeIcon('tint', library = 'fa'),
                          label = lapply(labs_current, HTML),
                          layerId = ~current$site_no,
                          group = 'current') %>%
      leaflet::groupOptions("current", zoomLevels = 5:18) %>%
      leaflet::hideGroup(group = 'Hydrography') %>%
      leaflet::addLayersControl(baseGroups = c("Esri.WorldImagery", "CartoDB.Positron",
                                               "OpenStreetMap", "CartoDB.DarkMatter", "OpenTopoMap"),
                                overlayGroups = c("Hydrography", 'current'))
  })
  })
  
  #updates values when params are changed, e.g. month, water year, flow 
  #also updates the 'nwis_sites_df' which is used in the tabs
  
  observeEvent(input$change_params,  {
    req(input$leaf_map_marker_click$id)
    
    isolate({
      if(input$select_or_slider == "Slider") {
      values$maxwy <- max(input$wy_slider, na.rm = T)
      values$minwy <- min(input$wy_slider, na.rm = T)
      values$minMonth <- min(input$month_slider, na.rm = T)
      values$maxMonth <- max(input$month_slider, na.rm = T)
      values$minq <- min(input$q_slider, na.rm = T)
      values$maxq <- max(input$q_slider, na.rm = T)
      values$nwis_sites_df <- usgs_ggplot_data_not_filtered() %>%
          filter(
            month %in% values$minMonth:values$maxMonth,
            wy >= values$minwy ,
            wy <= values$maxwy,
            Flow >= values$minq,
            Flow <= values$maxq
          )
      
      values$all_months <- values$minMonth == 1 && values$maxMonth == 12
      
      } else if (input$select_or_slider == "Selection"){
        values$maxwy <- max(input$wy_selection, na.rm = T)
        values$minwy <- min(input$wy_selection, na.rm = T)
        values$minMonth <- min(as.integer(input$month_selection), na.rm = T)
        values$maxMonth <- max(as.integer(input$month_selection), na.rm = T)
        values$minMonth <- as.character(values$minMonth)
        values$maxMonth <- as.character(values$maxMonth)
        print(input$month_selection)
        values$minq <- min(input$q_slider, na.rm = T)
        values$maxq <- max(input$q_slider, na.rm = T)
        values$nwis_sites_df <- usgs_ggplot_data_not_filtered() %>%
            filter(
              month %in% as.integer(input$month_selection),
              wy %in% input$wy_selection,
              Flow >= values$minq,
              Flow <= values$maxq
            )
        
        months_test <- input$month_selection
        char_test <- as.character(1:12)
        values$all_months <- isTRUE(all.equal(months_test,char_test))
        
      }
      
      
    })
   
  })


  #og data that gets downloaded (first)   
  usgs_ggplot_data_not_filtered <- reactive({
    site <- input$leaf_map_marker_click$id
   
     
      nwis_sites_df <- wildlandhydRo::batch_USGSdv(sites = site) %>% 
          mutate(month_day = paste0('0000-',month_day),
                 month_day = lubridate::as_date(month_day))
      
      if(nwis_sites_df[1,]$Station %in% "Tobacco River at Eureka, MT"){
        nwis_sites_df <- wildlandhydRo::batch_USGSdv(sites = '12301300') %>%
          mutate(month_day = paste0('0000-',month_day),
                 month_day = lubridate::as_date(month_day)) %>%
          dplyr::bind_rows(nwis_sites_df)

        nwis_sites_df <- nwis_sites_df[!duplicated(nwis_sites_df$Date),]

        nwis_sites_df <- nwis_sites_df %>% dplyr::mutate(Station = 'Tobacco River near Eureka MT',
                                                         site_no = '12301250')}
     

        nwis_sites_df
      
      
    })
  
  #getting an idea of full years
  
  usgs_ggplot_data <- reactive({
    usgs_ggplot_data_not_filtered() %>%
      dplyr::add_count(wy) %>%
      dplyr::filter(n >= 355)
  })
  
  
    

  #the first building of the modal and it's values
  
  observeEvent(input$leaf_map_marker_click$id,{
    
   withProgress(message = 'downloading station daily values...', value = 1/2, { 
     
     usgs_ggplot_data_not_filtered()
     
    values$maxwy_station <- max(usgs_ggplot_data_not_filtered()$wy, na.rm = T)
    values$minwy_station <- min(usgs_ggplot_data_not_filtered()$wy, na.rm = T)

    
    values$maxq_station <- max(usgs_ggplot_data_not_filtered()$Flow, na.rm = T)
    values$minq_station <- min(usgs_ggplot_data_not_filtered()$Flow, na.rm = T)
    
    values$maxwy <- values$maxwy_station
    values$minwy <- values$minwy_station
    
    values$maxq <- values$maxq_station
    values$minq <- values$minq_station
    
    values$minMonth <- 1
    values$maxMonth <- 12
    
    
    values$nwis_sites_df <- usgs_ggplot_data_not_filtered() %>%
        filter(
          month %in% values$minMonth:values$maxMonth,
          wy >= values$minwy ,
          wy <= values$maxwy,
          Flow >= values$minq,
          Flow <= values$maxq
        )
    
    values$all_months <- TRUE
    
    incProgress(amount = 3/4, 'rendering stats')
    
    rmarkdown::render(app_sys('app/www/usgs_stats.Rmd'))
    
  })
    

  })

 
    # These render the .html files for the modal
  output$frame <- renderUI({
    stats_html <-  tags$iframe(seamless = 'seamless', 
                               src="www/usgs_stats.html",
                               height=600, width=1248,
                               frameBorder="0")
    stats_html
  })
  
  #Modal that pops up
  
  observeEvent(input$leaf_map_marker_click$id, {
    
    showModal(modalDialog(
      title = "Explore the Station",
      easyClose = FALSE,
      footer = actionButton(ns('done'),'Done'),
      tags$style(
        type = 'text/css',
        '.modal-dialog {
    width: fit-content !important;
    margin: 100px;}'
      ),
        tags$style(
          type = 'text/css',
          '.modal-body {
        position: relative;
        padding: 10px;
        min-height: 700px;
      }'),
      shinydashboard::box(width = 2,
                          conditionalPanel(condition = "input.select_or_slider == 'Slider'",
                          shiny::sliderInput(ns('month_slider'), label = 'Choose a month', 
                                                       min = 1, 
                                                       max = 12, 
                                                       value = c(1,12)),
                          sliderInput(ns('wy_slider'), label = 'Filter by water year',
                                      min = values$minwy_station,
                                      max = values$maxwy_station,
                                      step = 1,
                                      value = c(values$minwy_station,values$maxwy_station), sep=''),
                          sliderInput(ns('q_slider'), label = 'Filter by discharge',
                                      min = values$minq_station,
                                      max = values$maxq_station,
                                      value = c(values$minq_station,values$maxq_station), sep=''),ns=ns),
                          
                          conditionalPanel(condition = "input.select_or_slider  == 'Selection'",
                                           shinyWidgets::pickerInput(ns('month_selection'),'Choose a month', 
                                                       options = list(`actions-box` = TRUE), 
                                                       choices = seq(1,12,1),
                                                       multiple = TRUE, selected = 1:12),
                                           shinyWidgets::pickerInput(ns('wy_selection'), 'Filter by water year', 
                                                                     options = list(`actions-box` = TRUE), 
                                                                     choices = seq(values$minwy_station,
                                                                                   values$maxwy_station, 1),
                                                                     multiple = TRUE,
                                                                     selected = values$minwy_station:values$maxwy_station),
                                           sliderInput(ns('q_slider'), label = 'Filter by discharge',
                                                       min = values$minq_station,
                                                       max = values$maxq_station,
                                                       value = c(values$minq_station,values$maxq_station), sep=''), ns = ns),
                          radioButtons(ns('select_or_slider'), 'Filtering Method', choices = c('Selection', 'Slider'),
                                       inline = TRUE, selected = 'Slider'),
                          actionButton(ns('change_params'), 'Submit Changes', class = 'btn-submit')),
                          
      shinydashboard::box(width=10,fluidPage(tabsetPanel(id = 'exploring_hydrograph',              
                                         tabPanel(title = "Summary Stats", value = 'ss',
                                                  htmlOutput(ns('frame')) %>%
                                                    shinycssloaders::withSpinner()
                                                  ),                       
                                         tabPanel(title = "Hydrograph-Daily", 
                                                  plotly::plotlyOutput(ns('hydrograph'),  height = 600) %>%
                                                    shinycssloaders::withSpinner(),
                                                  downloadButton(ns('hd'),'download csv')),
                                         tabPanel(title = 'Hydrograph-Monthly', 
                                                  plotly::plotlyOutput(ns('ts_plot'), height = 600) %>%
                                                    shinycssloaders::withSpinner(),
                                                  shinyWidgets::pickerInput(ns("usgs_metric"), "Pick a metric", selected = "Maximum",  options = list(`actions-box` = TRUE), 
                                                              choices = c("Maximum", "Minimum", "Mean", "Median")),
                                                  downloadButton(ns('hm'),'download csv')),
                                         tabPanel(title = 'BFI', 
                                                  plotly::plotlyOutput(ns('bf_plot'), height = 600) %>%
                                                    shinycssloaders::withSpinner(),
                                                  downloadButton(ns('bfi'),'download csv')),
                                         tabPanel(title = 'Flow Duration Curve', 
                                                  plotly::plotlyOutput(ns('fdc'), height = 600) %>%
                                                    shinycssloaders::withSpinner(),
                                                  downloadButton(ns('fdc_dl'),'download csv')),
                                         tabPanel(title = "Flood Frequency",
                                                  plotly::plotlyOutput(ns('freq'), height = 600) %>%
                                                    shinycssloaders::withSpinner(),
                                                  radioButtons(ns('ff_sel'), 'Choose graph type', choices = c('Time Series', 'Return Interval'), selected = 'Time Series',
                                                               inline = TRUE),
                                                  downloadButton(ns('ff'),'download csv')),
                                         tabPanel(title = 'Forecast',
                                                  uiOutput(ns('forecast')))),
      ))
    ))
    
    

  })
  
  observeEvent(input$done, {
    
    removeModal()
    values$nwis_sites_df <- NULL
    values$nwis_sites_df_month <- NULL
    values$bflow <- NULL
    values$peak_df <- NULL
    values$fdc <- NULL
    values$freq <- NULL
    
    rm(list=ls())
    gc()
    
  })

  
  dlHandler_cust <- function(event) {
    observe(
      {
        
  output[[event]] <- downloadHandler(
    
    filename = function(){
      if(event == 'ss'){
        'myfile.html'
      } else {'myfile.csv'}
      },
    
    content = function(file) {
     
     switch(event,
     hd = write.csv(values$nwis_sites_df, file),
     hm = write.csv(values$nwis_sites_df_month, file),
     bfi = write.csv(values$bflow, file),
     fdc_dl = write.csv(values$fdc, file),
     ff = write.csv(values$freq, file)
      )
    }
  )
      })
  }
  
  dlHandler_cust('hd')
  dlHandler_cust('hm')
  dlHandler_cust('bfi')
  dlHandler_cust('fdc_dl')
  dlHandler_cust('ff')
  
  #inline radio buttons (reactive)
  
  ff_sel_reac <- reactive(input$ff_sel)
  
  #Hydrograph-plot
  

      output$hydrograph <- plotly::renderPlotly({
      hydrograph_plot <- plotly::ggplotly((values$nwis_sites_df %>%
                                             ggplot() + 
                                             geom_line(aes(Date, Flow,label = Date), size = .5) +
                                             labs(title = paste0(values$nwis_sites_df$Station))+
                                             theme_bw() 
      ), tooltip=c("Flow", "Date"))
      
      print(hydrograph_plot) })
      
 
  
  #TS-Plot
  
  output$ts_plot <- plotly::renderPlotly({
    
    values$nwis_sites_df_month <- values$nwis_sites_df %>% wildlandhydRo::wymUSGS()

          if (input$usgs_metric == "Maximum")  {

            print(plotly::ggplotly(ggplot(values$nwis_sites_df_month, aes(year_month, Maximum)) +
              geom_line(size = .5) +
              geom_point() + geom_smooth(alpha = 0.1,se = TRUE) +
              labs(title = paste0(values$nwis_sites_df_month$Station[1], " Monthly Maximum Discharge (cfs)"),
                   y = "Maximum Discharge (cfs) per Month", x = "Water Year")+ theme_bw()))

          } else if (input$usgs_metric == "Minimum") {

            print(plotly::ggplotly(ggplot(values$nwis_sites_df_month, aes(year_month, Minimum)) +
              geom_line(size = .5) +
              geom_point() + geom_smooth(alpha = 0.1, se = TRUE) +
              labs(title = paste0(values$nwis_sites_df_month$Station[1], " Monthly Minimum Discharge (cfs)"),
                   y = "Minimum Discharge (cfs) per Month", x = "Water Year")+ theme_bw()))
          } else if (input$usgs_metric == "Mean") {

            print(plotly::ggplotly(ggplot(values$nwis_sites_df_month, aes(year_month, Mean)) +
              geom_line(size = .5) +
              geom_point() + geom_smooth(alpha = 0.1,  se = TRUE) +
              labs(title = paste0(values$nwis_sites_df_month$Station[1], " Monthly Mean Discharge (cfs)"),
                   y = "Mean Discharge (cfs) per Month", x = "Water Year")+ theme_bw()))

          } else if (input$usgs_metric == "Median") {

            print(plotly::ggplotly(ggplot(values$nwis_sites_df_month, aes(year_month, Median)) +
              geom_line(size = .5) +
              geom_point() + geom_smooth(alpha = 0.1,  se = TRUE) +
              labs(title = paste0(values$nwis_sites_df_month$Station[1], " Monthly Median Discharge (cfs)"),
                   y = "Median Discharge (cfs) per Month", x = "Water Year")+ theme_bw()))
          } else {"SOL"}
    
  })
  
  #BFI-Plot
  
  output$bf_plot <- plotly::renderPlotly({
    # 
    
    if(values$all_months){
    bflow <- values$nwis_sites_df  %>% 
      dplyr::add_count(wy) %>% dplyr::filter(n >= 355)
    } else {
      bflow <- values$nwis_sites_df  %>% 
      mutate(bf = lfstat::baseflow(Flow))
    }
    values$bflow <- bflow %>% 
      mutate(bf = lfstat::baseflow(Flow)) %>% 
      dplyr::group_by(wy) %>% 
      dplyr::mutate(bfi = bf/Flow) %>% 
      dplyr::summarise(bfi = mean(bfi, na.rm = T)) 
    
    bf_flow_mk <- values$bflow %>% 
      dplyr::pull(bfi) %>% 
      Kendall::MannKendall() %>% broom::tidy() %>% 
      mutate(dplyr::across(is.numeric,round, 4))
    
    validate(need(input$leaf_map_marker_click$id, 'Waiting for a Station to be clicked...'))
    
    bf_plot <- plotly::ggplotly(
      ggplot(values$bflow, aes(wy, bfi)) + 
        geom_point() + 
        geom_line() + 
        geom_smooth(method = 'lm') +
        labs(title = paste('Mean Baseflow Index (BFI): Mann-Kendall NHST (p.value): ', 
                           as.numeric(bf_flow_mk$p.value)),
             x = 'Water Year', 
             y = 'BFI') +
        theme_bw() 
        
      )
    
    print(bf_plot)
    
    
  })
  
  #FDC-plot
  
  output$fdc <- plotly::renderPlotly({

    validate(need(input$leaf_map_marker_click$id, 'Waiting for a Station to be clicked...'))
    
    get_fdc <- wildlandhydRo::plot_USGSfdc(values$nwis_sites_df)
    values$fdc <- get_fdc$data
    fdc_plot <- plotly::ggplotly(get_fdc +
                     labs(title = paste('Flow Duration Curve')))
    print(fdc_plot)
  })
  
  #Freq-plot
  
  output$freq <- plotly::renderPlotly({
    
    if(values$all_months){
      
      values$peak_df <- values$nwis_sites_df %>% 
        dplyr::add_count(wy) %>% dplyr::filter(n >= 355) %>% wildlandhydRo::wyUSGS()
      
      peak_flow_mk <- values$peak_df %>% 
        dplyr::pull(Peak) %>% 
        Kendall::MannKendall() %>% broom::tidy() %>% 
        mutate(dplyr::across(is.numeric,round, 4))
      
      values$freq <-  values$peak_df %>%
        dplyr::filter(!is.na(Peak)) %>% 
        wildlandhydRo::batch_frequency(Peak)
      
    } else {
      
      nwis_sites_df <- values$nwis_sites_df %>%  wildlandhydRo::wymUSGS()
      
      values$peak_df <-  nwis_sites_df %>% 
        rename(Peak = 'Maximum', peak_dt = 'year_month')%>% dplyr::group_by(wy) %>% 
        slice_max(Peak,with_ties = FALSE) %>% 
        ungroup()
      
      peak_flow_mk <- values$peak_df  %>% 
        dplyr::pull(Peak) %>% 
        Kendall::MannKendall() %>% broom::tidy() %>% 
        mutate(dplyr::across(is.numeric,round, 4))
      
      values$freq <-  values$peak_df %>%
        dplyr::filter(!is.na(Peak)) %>% 
        wildlandhydRo::batch_frequency(Peak)
      
    }
    
    validate(need(input$leaf_map_marker_click$id, 'Waiting for a Station to be clicked...'))
   if(ff_sel_reac() == 'Return Interval') {
     
      peak_plot <- plotly::ggplotly( ggplot(data = values$freq,aes(ReturnInterval, Value, color = Distribution)) + 
                              geom_line(size = .5) +
                              geom_point(size = .75) +
                              ggplot2::scale_y_log10(label = scales::comma_format()) +
                              ggplot2::scale_x_log10() +
                              theme_bw() + 
                              ggplot2::scale_color_manual(values = hcl.colors(n = 7, palette = 'Zissou 1'))+
                             labs(title = paste('Flood Frequency'),
                                  y = 'Discharge (cfs)', x = 'Return Interval (years)'))
       
          print(peak_plot)
   } else if (ff_sel_reac() == 'Time Series'){
     
     peak_plot <- plotly::ggplotly( ggplot(data =  values$peak_df,
                                           aes(peak_dt, Peak)) + 
                                      geom_line() +
                                      geom_point() +
                                      geom_smooth(method = 'lm') +
                                      theme_bw() + 
                                      labs(title = paste('Maximum Flows: Mann-Kendall NHST (p.value): ', 
                           as.numeric(peak_flow_mk$p.value)),
                                           y = 'Discharge (cfs)', x = 'Water Years'))
     
     print(peak_plot)
   }
   
  
    })
  
  #Forecast
  
  observeEvent(input$leaf_map_marker_click$id, {
        
        output$forecast <-renderUI({
          site <- input$leaf_map_marker_click$id
        for_rep <- paste0("https://waterwatch.usgs.gov/wwapps/ww_chart.php?i=ahps&vt=uv&site_no=", site)
          
          tags$div(img(src=for_rep,height='500px', width = '600px'), style="text-align: center; margin-top: 75px;")
          
          })
        
         })
   
 
  
}
    
## To be copied in the UI
# mod_station_ui("station_ui_1")
    
## To be copied in the server
# callModule(mod_station_server, "station_ui_1")
#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui_station <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # List the first level UI elements here 
    fluidPage(
      mod_station_ui("station_ui_1")
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

#' USGS sites.
#'
#' A dataset containing all the USGS sites (historic and current) from all 50 states.
#'
#' @format A data frame with 8348 rows and 24 variables:
#' \describe{
#'   \item{status}{shows whether current or historic}
#'   ...
#' }
"usgs_sites_df"