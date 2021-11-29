#' station UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom dplyr filter select mutate slice_max ungroup rename slice group_by
#' @importFrom ggplot2 geom_smooth geom_point geom_line aes
#' @importFrom rlang .data 
#' @importFrom grDevices hcl.colors
#' @importFrom stringr str_c
#' 
mod_snotel_ui <- function(id){
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns('leaf_map'), height = 950) %>% shinycssloaders::withSpinner()
  )
}

#' station Server Function
#'
#' @noRd 
mod_snotel_server <- function(input, output, session, values){
  ns <- session$ns
  
 
  #starting leaflet map
  output$leaf_map <- leaflet::renderLeaflet({
    state <- meta_snotel %>% dplyr::distinct(state) %>% dplyr::pull(state)
    state.name.filt <- state.name[match(state,state.abb)] %>% sort()
    base_map() %>%
      leaflet::addControl(html = shinyWidgets::pickerInput(
        ns('location_map'), 'Select a state to get started!',
        choices = c("", as.character(state.name.filt)),
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
    withProgress(value = 0.5, message = paste0('loading ', input$location_map, ' snotel stations...'),{
      state_names <- data.frame(stn = state.name, stab = state.abb)
      state_select <- state_names %>% filter(stn == input$location_map) %>% dplyr::pull(stab)
      
      current <- meta_snotel %>% filter(state %in% state_select) 
      
      labs_current <- as.list(stringr::str_to_title(current$site_name))
      
      leaflet::leafletProxy("leaf_map", session) %>%
        leaflet::addAwesomeMarkers(data = current,lng = current$longitude, 
                                   lat = current$latitude,
                                   icon = leaflet::makeAwesomeIcon('snowflake', library = 'fa'),
                                   label = lapply(labs_current, HTML),
                                   layerId = ~current$site_id,
                                   group = 'current') %>%
        leaflet::groupOptions("current", zoomLevels = 5:18) %>%
        leaflet::hideGroup(group = 'Hydrography') %>%
        leaflet::addLayersControl(baseGroups = c("Esri.WorldImagery", "CartoDB.Positron",
                                                 "OpenStreetMap", "CartoDB.DarkMatter", "OpenTopoMap"),
                                  overlayGroups = c("Hydrography", 'current'))
    })
  })
  
  #updates values when params are changed, e.g. month, water year, swe 
  #also updates the 'snotel_sites_df' which is used in the tabs
  
  observeEvent(input$change_params,  {
    req(input$leaf_map_marker_click$id)
    
    isolate({
      if(input$select_or_slider == "Slider") {
        values$maxwy <- max(input$wy_slider, na.rm = T)
        values$minwy <- min(input$wy_slider, na.rm = T)
        print(values$maxwy)
        print(values$minwy)
        values$minMonth <- min(input$month_slider, na.rm = T)
        values$maxMonth <- max(input$month_slider, na.rm = T)
        values$minq <- min(input$q_slider, na.rm = T)
        values$maxq <- max(input$q_slider, na.rm = T)
        values$snotel_sites_df <- snotel_ggplot_data_not_filtered() %>%
          filter(
            month %in% values$minMonth:values$maxMonth,
            wy >= values$minwy ,
            wy <= values$maxwy,
            snow_water_equivalent >= values$minq,
            snow_water_equivalent <= values$maxq
          )
        
        values$all_months <- values$minMonth == 1 && values$maxMonth == 12
        
      } else if (input$select_or_slider == "Selection"){
        values$maxwy <- max(input$wy_selection, na.rm = T)
        values$minwy <- min(input$wy_selection, na.rm = T)
        values$minMonth <- min(as.integer(input$month_selection), na.rm = T)
        values$maxMonth <- max(as.integer(input$month_selection), na.rm = T)
        values$minMonth <- as.character(values$minMonth)
        values$maxMonth <- as.character(values$maxMonth)
        print(values$minMonth)
        print(values$maxMonth)
        values$minq <- min(input$q_slider, na.rm = T)
        values$maxq <- max(input$q_slider, na.rm = T)
        values$snotel_sites_df <- snotel_ggplot_data_not_filtered() %>%
          filter(
            month %in% input$month_selection,
            wy %in% input$wy_selection,
            snow_water_equivalent >= values$minq,
            snow_water_equivalent <= values$maxq
          )
        months_test <- input$month_selection
        char_test <- as.character(1:12)
        values$all_months <- isTRUE(all.equal(months_test,char_test))
      }
      
      
    })
    
  })
  
  
  #og data that gets downloaded (first)   
  snotel_ggplot_data_not_filtered <- reactive({
    site <- input$leaf_map_marker_click$id
    
    
    snotel_sites_df <- wildlandhydRo::batch_SNOTELdv(sites = site) %>% 
      mutate(day = lubridate::day(Date),
             month_day = str_c(month, day, sep = "-"),
             month_day = paste0('0000-',month_day),
             month_day = lubridate::as_date(month_day),
             wy = dataRetrieval::calcWaterYear(Date))
    
  })
  
  #filter out incomplete years
  snotel_ggplot_data <- reactive({
    site <- input$leaf_map_marker_click$id
    
    snotel_ggplot_data_not_filtered() %>% 
      dplyr::add_count(wy) %>% 
      filter(n >= 355)
    
  })
  #getting an idea of full years
  
  phenology <- reactive({
    phen_data <- snotel_ggplot_data_not_filtered() %>% 
      dplyr::add_count(wy) %>% 
      filter(n >=355) %>% 
      snotelr::snotel_phenology()
    
    if(is.null(phen_data)){
      phen_data
    } else {
      
      phen_data %>% 
      mutate(max_swe = max_swe*0.0393701,
             max_swe_doy_d = as.Date(max_swe_doy, origin = '1970-01-01') %>% 
               stringr::str_remove( "1970-") %>% str_c(year,.data, sep = "-") %>% lubridate::parse_date_time(orders = c("%y-%m-%d", "%y%m%d", "%y-%m-%d %H:%M")),
             first_snow_acc_d = as.Date(first_snow_acc, origin = "1970-01-01") %>%
               stringr::str_remove( "1970-") %>% str_c(year,.data, sep = "-") %>% lubridate::parse_date_time(orders = c("%y-%m-%d", "%y%m%d", "%y-%m-%d %H:%M")),
             first_snow_melt_d = as.Date(first_snow_melt, origin = "1970-01-01") %>%
               stringr::str_remove( "1970-") %>% str_c(year,.data, sep = "-") %>% lubridate::parse_date_time( orders = c("%y-%m-%d", "%y%m%d", "%y-%m-%d %H:%M")),
             last_snow_melt_d = as.Date(last_snow_melt, origin = "1970-01-01") %>%
               stringr::str_remove( "1970-") %>% str_c(year,.data, sep = "-") %>% lubridate::parse_date_time( orders = c("%y-%m-%d", "%y%m%d", "%y-%m-%d %H:%M")),
             cont_snow_acc_d = as.Date(cont_snow_acc, origin = "1970-01-01") %>%
               stringr::str_remove( "1970-") %>% str_c(year,.data, sep = "-") %>% lubridate::parse_date_time( orders = c("%y-%m-%d", "%y%m%d", "%y-%m-%d %H:%M")),
             first_snow_acc_m = as.Date(first_snow_acc, origin = "1970-01-01") %>%
               stringr::str_remove( "1970-"),
             first_snow_melt_m = as.Date(first_snow_melt, origin = "1970-01-01") %>%
               stringr::str_remove( "1970-"),
             last_snow_melt_m = as.Date(last_snow_melt, origin = "1970-01-01") %>%
               stringr::str_remove( "1970-"),
             cont_snow_acc_m = as.Date(cont_snow_acc, origin = "1970-01-01"))
    }
  })
  
  
  
  
  #the first building of the modal and it's values
  
  observeEvent(input$leaf_map_marker_click$id,{
    
    withProgress(message = 'downloading station daily values...', value = 1/2, { 
      
      snotel_ggplot_data_not_filtered()
      
      values$maxwy_station <- max(snotel_ggplot_data_not_filtered()$wy, na.rm = T)
      values$minwy_station <- min(snotel_ggplot_data_not_filtered()$wy, na.rm = T)
      
      
      values$maxq_station <- max(snotel_ggplot_data_not_filtered()$snow_water_equivalent, na.rm = T)
      values$minq_station <- min(snotel_ggplot_data_not_filtered()$snow_water_equivalent, na.rm = T)
      
      values$maxwy <- values$maxwy_station
      values$minwy <- values$minwy_station
      
      values$maxq <- values$maxq_station
      values$minq <- values$minq_station
      
      values$minMonth <- 1
      values$maxMonth <- 12
      
      incProgress(amount = 3/4, 'rendering stats')
      
      rmarkdown::render(system.file('app/www', 'snotel_stats.Rmd', package = 'hydroapps'),
                        output_format = rmarkdown::html_document())
      # rmarkdown::render('inst/app/www/bf_sum.Rmd',
      #                   output_format = rmarkdown::html_document())
      # rmarkdown::render('inst/app/www/peak_rep.Rmd',
      #                   output_format = rmarkdown::html_document())
      
      values$snotel_sites_df <- snotel_ggplot_data_not_filtered() %>%
        filter(
          month %in% values$minMonth:values$maxMonth,
          wy >= values$minwy ,
          wy <= values$maxwy,
          snow_water_equivalent >= values$minq,
          snow_water_equivalent <= values$maxq
        )
      
      values$all_months <- TRUE
      
    })
  })
  
  # These render the .html files for the modal
  output$frame <- renderUI({
    
    html_path <- paste0(system.file('app/www', 'snotel_stats.html', package = 'hydroapps'))
    stats_html <-  tags$iframe(src=html_path, height=600, width=1248,frameBorder="0")
    stats_html
  })
  
  # output$bf_sum <- renderUI({
  #   
  #   bf_path <- paste('www/bf_sum.html')
  #   bf_html <- tags$iframe(src=bf_path, height=600, width = 1248, frameBorder='0')
  #   bf_html
  #   
  # })
  # 
  # output$peak_rep <- renderUI({
  #   
  #   freq_path <- paste('www/peak_rep.html')
  #   freq_html <- tags$iframe(src=freq_path, height=200, width = 1248, frameBorder='0')
  #   freq_html
  #   
  # })
  
  #Modal that pops up
  
  observeEvent(input$leaf_map_marker_click$id, {
    
    showModal(modalDialog(
      title = "Explore the Station",
      easyClose = TRUE,
      footer = NULL,
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
                                           sliderInput(ns('q_slider'), label = 'Filter by SWE',
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
                                           sliderInput(ns('q_slider'), label = 'Filter by SWE',
                                                       min = values$minq_station,
                                                       max = values$maxq_station,
                                                       value = c(values$minq_station,values$maxq_station), sep=''), ns = ns),
                          radioButtons(ns('select_or_slider'), 'Filtering Method', choices = c('Selection', 'Slider'),
                                       inline = TRUE, selected = 'Slider'),
                          actionButton(ns('change_params'), 'Submit Changes', class = 'btn-submit')),
      
      shinydashboard::box(width=10,fluidPage(tabsetPanel(id = 'exploring_swegraph',              
                                                         tabPanel(title = "Summary Stats",
                                                                  htmlOutput(ns("frame")) %>%
                                                                    shinycssloaders::withSpinner()),                       
                                                         tabPanel(title = "SWE Graph",
                                                                  plotly::plotlyOutput(ns('swegraph'),  height = 600) %>%
                                                                    shinycssloaders::withSpinner(),
                                                                  radioButtons(ns('sweg_sel'), 'Choose graph type', choices = c('Compact', 'Long'), selected = 'Compact',
                                                                               inline = TRUE)),
                                                         tabPanel(title = 'Time-Series',
                                                                  plotly::plotlyOutput(ns('ts_plot'), height = 600) %>%
                                                                    shinycssloaders::withSpinner(),
                                                                  shinyWidgets::pickerInput(ns("swe_metric"), "Pick a metric", selected = "Maximum",  options = list(`actions-box` = TRUE), 
                                                                                            choices = c("Maximum", "Mean"))),
                                                         tabPanel(title = 'SWE Duration Curve',
                                                                  plotly::plotlyOutput(ns('fdc'), height = 600) %>%
                                                                    shinycssloaders::withSpinner()),
                                                         tabPanel(title = 'Phenology',
                                                                  plotly::plotlyOutput(ns('phen'), height = 600) %>%
                                                                    shinycssloaders::withSpinner(),
                                                                  shinyWidgets::pickerInput(ns("phen_metric"), "Pick a Time", selected = "First Snow",  options = list(`actions-box` = TRUE), 
                                                                                            choices = c("First Snow Accumulation", "Last Snow Melt"))),
                                                         tabPanel(title = "SWE Frequency",
                                                                  plotly::plotlyOutput(ns('freq'), height = 600) %>%
                                                                    shinycssloaders::withSpinner(),
                                                                  radioButtons(ns('ff_sel'), 'Choose graph type', choices = c('Time Series', 'Return Interval'), selected = 'Time Series',
                                                                               inline = TRUE)),
                                                         tabPanel(title = 'Forecast',
                                                                  DT::dataTableOutput(ns('nws_table')))),
      )),
      tags$div(class = 'btn-modal',actionButton(ns('done'), 'Done', class = 'btn-modal'))
    ))
    
  })
  
  observeEvent(input$done, {
    removeModal()
  })
  
  #inline radio buttons (reactive)
  
  ff_sel_reac <- reactive(input$ff_sel)
  swe_sel_reac <- reactive(input$sweg_sel)
  
  #Hydrograph-plot
  
  output$swegraph <- plotly::renderPlotly({
    validate(need(input$leaf_map_marker_click$id, 'Waiting for a Station to be clicked...'))
    
    
    if(swe_sel_reac() == 'Compact'){
      swegraph_plot <- plotly::ggplotly((values$snotel_sites_df %>%
                                             ggplot() + 
                                             geom_line(aes(month_day, snow_water_equivalent, group = wy,color = wy,label = Date), size = .5) +
                                             labs(title = paste0(values$snotel_sites_df$site_name),
                                                  y = 'Snow Water Equivalent (SWE) (in)')+
                                             theme_bw() + 
                                           ggplot2::scale_color_gradientn(colors = hcl.colors(n = 11, palette = 'Zissou 1')) +
                                           ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10.5),
                                                          axis.title.x = ggplot2::element_text(size = 12.5),
                                                          axis.text.y = ggplot2::element_text(size = 10.5),
                                                          strip.text = ggplot2::element_text(size = 10.5),
                                                          plot.title = ggplot2::element_text(size = 12.5),
                                                          axis.title.y = ggplot2::element_text(size = 12.5),
                                                          plot.subtitle = ggplot2::element_text(size = 10.5))
      ), tooltip=c("snow_water_equivalent", "Date"))
      
      print(swegraph_plot)
      
    } else if (swe_sel_reac() == 'Long'){
      
      swegraph_plot <- plotly::ggplotly((values$snotel_sites_df %>%
                                             ggplot() + 
                                             geom_line(aes(Date, snow_water_equivalent,label = snow_water_equivalent), size = .5) +
                                             labs(title = paste0(values$snotel_sites_df$site_name,
                                                                 y = 'Snow Water Equivalent (SWE) (in)'))+
                                             theme_bw() +
                                           ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10.5),
                                                          axis.title.x = ggplot2::element_text(size = 12.5),
                                                          axis.text.y = ggplot2::element_text(size = 10.5),
                                                          strip.text = ggplot2::element_text(size = 10.5),
                                                          plot.title = ggplot2::element_text(size = 12.5),
                                                          axis.title.y = ggplot2::element_text(size = 12.5),
                                                          plot.subtitle = ggplot2::element_text(size = 10.5))
      ), tooltip=c("snow_water_equivalent", "Date"))
      
      print(swegraph_plot)
    }
    
  })
  
  #TS-Plot
  
  output$ts_plot <- plotly::renderPlotly({
    
    snotel_sites_df <- values$snotel_sites_df %>% 
      group_by(wy,month) %>% 
      mutate(swe_max = max(snow_water_equivalent),
             swe_mean = mean(snow_water_equivalent)) %>% 
      slice(n=1) %>% 
      ungroup
    
    if (input$swe_metric == "Maximum")  {
      
      print(plotly::ggplotly(ggplot(snotel_sites_df, aes(Date, swe_max)) +
                               geom_line(size = .5) +
                               geom_point() + geom_smooth(alpha = 0.1,se = TRUE) +
                               labs(title = paste0(snotel_sites_df$site_name[1], " Monthly Maximum SWE (in)"),
                                    y = "Maximum SWE (in) per Month", x = "Water Year")+ theme_bw()+
                               ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10.5),
                                              axis.title.x = ggplot2::element_text(size = 12.5),
                                              axis.text.y = ggplot2::element_text(size = 10.5),
                                              strip.text = ggplot2::element_text(size = 10.5),
                                              plot.title = ggplot2::element_text(size = 12.5),
                                              axis.title.y = ggplot2::element_text(size = 12.5),
                                              plot.subtitle = ggplot2::element_text(size = 10.5))))
      
    } else if (input$swe_metric == "Mean") {
      
      print(plotly::ggplotly(ggplot(snotel_sites_df, aes(Date, swe_mean)) +
                               geom_line(size = .5) +
                               geom_point() + geom_smooth(alpha = 0.1,  se = TRUE) +
                               labs(title = paste0(snotel_sites_df$site_name[1], " Monthly Mean SWE (in)"),
                                    y = "Mean SWE (in) per Month", x = "Water Year")+ theme_bw()+
                               ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10.5),
                                              axis.title.x = ggplot2::element_text(size = 12.5),
                                              axis.text.y = ggplot2::element_text(size = 10.5),
                                              strip.text = ggplot2::element_text(size = 10.5),
                                              plot.title = ggplot2::element_text(size = 12.5),
                                              axis.title.y = ggplot2::element_text(size = 12.5),
                                              plot.subtitle = ggplot2::element_text(size = 10.5))))
      
    }  else {"SOL"}
    
  })
  
  #Phenology-Plot
  
  output$phen <- plotly::renderPlotly({
    if(is.null(phenology())){shinyalert::shinyalert(
      title = "Location Error",
      text = "Insufficient Data, sorry.",
      size = "s",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE)
    } else {
    if (input$phen_metric == "First Snow Accumulation")  {
      
      values$phen_swe_mk <- phenology()  %>% 
        dplyr::pull(first_snow_acc) %>% 
        Kendall::MannKendall() %>% broom::tidy() %>% 
        mutate(dplyr::across(is.numeric,round, 4))
      
      print(plotly::ggplotly(phenology()  %>% 
        ggplot(aes(year,first_snow_acc, label = first_snow_acc_d)) +
        geom_point() + 
        geom_line() +
          geom_smooth(method = 'lm') +
          theme_bw() +
          labs(title = paste('Mann-Kendall NHST (p.value): ', 
                                        as.numeric(values$phen_swe_mk$p.value)),
                          y = 'DOY (julian)', x = 'Year')
          ))
      
    } else if (input$phen_metric == 'Last Snow Melt'){
      
      values$phen_swe_mk <- phenology()  %>% 
        dplyr::pull(last_snow_melt) %>% 
        Kendall::MannKendall() %>% broom::tidy() %>% 
        mutate(dplyr::across(is.numeric,round, 4))
      
      print(plotly::ggplotly(phenology()  %>% 
                               ggplot(aes(year,last_snow_melt, label = last_snow_melt_d)) +
                               geom_point() + 
                               geom_line()+
                               geom_smooth(method = 'lm') +
                               theme_bw() +
                               labs(title = paste('Mann-Kendall NHST (p.value): ', 
                                                  as.numeric(values$phen_swe_mk$p.value)),
                                    y = 'DOY (julian)', x = 'Year')+
                               ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10.5),
                                              axis.title.x = ggplot2::element_text(size = 12.5),
                                              axis.text.y = ggplot2::element_text(size = 10.5),
                                              strip.text = ggplot2::element_text(size = 10.5),
                                              plot.title = ggplot2::element_text(size = 12.5),
                                              axis.title.y = ggplot2::element_text(size = 12.5),
                                              plot.subtitle = ggplot2::element_text(size = 10.5))))
    }
    }
  })
  #FDC-plot
  
  output$fdc <- plotly::renderPlotly({
    
    validate(need(input$leaf_map_marker_click$id, 'Waiting for a Station to be clicked...'))
    
    fdc_plot <- plotly::ggplotly(wildlandhydRo::plot_USGSfdc(values$snotel_sites_df  %>%
                                                               mutate(Flow = snow_water_equivalent)) + 
                                   labs(y = 'SWE (in)',
                                        title = 'SWE Duration Curve')+
                                   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10.5),
                                                  axis.title.x = ggplot2::element_text(size = 12.5),
                                                  axis.text.y = ggplot2::element_text(size = 10.5),
                                                  strip.text = ggplot2::element_text(size = 10.5),
                                                  plot.title = ggplot2::element_text(size = 12.5),
                                                  axis.title.y = ggplot2::element_text(size = 12.5),
                                                  plot.subtitle = ggplot2::element_text(size = 10.5)))
    
    print(fdc_plot)
  })
  

  
  #Freq-plot
  
  output$freq <- plotly::renderPlotly({
    
    if(values$all_months){
      
      values$peak_df <- values$snotel_sites_df   %>% 
        dplyr::add_count(wy) %>% 
        dplyr::filter(n >= 355)%>% 
        group_by(wy,month) %>% 
        slice_max(snow_water_equivalent) %>% 
        rename(Peak = 'snow_water_equivalent') %>% 
        ungroup() %>% dplyr::group_by(wy) %>% 
        slice_max(Peak) %>% slice(n=1) %>% 
        ungroup()
      
      values$peak_swe_mk <- values$peak_df %>% 
        dplyr::pull(Peak) %>% 
        Kendall::MannKendall() %>% broom::tidy() %>% 
        mutate(dplyr::across(is.numeric,round, 4))
      
      values$freq <-  values$peak_df %>%
        dplyr::filter(!is.na(Peak)) %>% 
        wildlandhydRo::batch_frequency(Peak)
      
    } else {
      
      values$peak_df <- values$snotel_sites_df %>% 
        group_by(wy,month) %>% 
        filter(wy %in% snotel_ggplot_data()$wy) %>% 
        slice_max(snow_water_equivalent) %>% 
        rename(Peak = 'snow_water_equivalent') %>% 
        ungroup() %>% dplyr::group_by(wy) %>% 
        slice_max(Peak) %>% slice(n=1) %>% 
        ungroup()
  
      values$peak_swe_mk <- values$peak_df  %>% 
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
                                       labs(title = paste('SWE Frequency'),
                                            y = 'SWE (in)', x = 'Return Interval (years)')+
                                       ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10.5),
                                                      axis.title.x = ggplot2::element_text(size = 12.5),
                                                      axis.text.y = ggplot2::element_text(size = 10.5),
                                                      strip.text = ggplot2::element_text(size = 10.5),
                                                      plot.title = ggplot2::element_text(size = 12.5),
                                                      axis.title.y = ggplot2::element_text(size = 12.5),
                                                      plot.subtitle = ggplot2::element_text(size = 10.5)))
      
      print(peak_plot)
    } else if (ff_sel_reac() == 'Time Series'){
      
      peak_plot <- plotly::ggplotly( ggplot(data =  values$peak_df,
                                            aes(Date, Peak)) + 
                                       geom_line() +
                                       geom_point() +
                                       geom_smooth(method = 'lm') +
                                       theme_bw() + 
                                       labs(title = paste('Mann-Kendall NHST (p.value): ', 
                                                          as.numeric(values$peak_swe_mk$p.value)),
                                            y = 'SWE (in)', x = 'Water Years')+
                                       ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10.5),
                                                      axis.title.x = ggplot2::element_text(size = 12.5),
                                                      axis.text.y = ggplot2::element_text(size = 10.5),
                                                      strip.text = ggplot2::element_text(size = 10.5),
                                                      plot.title = ggplot2::element_text(size = 12.5),
                                                      axis.title.y = ggplot2::element_text(size = 12.5),
                                                      plot.subtitle = ggplot2::element_text(size = 10.5)))
      
      print(peak_plot)
    }
    
    
  })
  
  #Forecast
  observeEvent(input$leaf_map_marker_click$id, {
    
    click <- snotel_ggplot_data_not_filtered() %>% slice(n=1)
    clat <- click$latitude
    clng <- click$longitude
    
    df1 <- httr::GET(url = paste0(
        "https://api.weather.gov/points/",
        
        clat, ",",
        
        clng))
      
      
      
      df <- jsonlite::fromJSON(url(df1$url, "rb"))
      
      city <- df$properties$relativeLocation$properties$city
      state <- df$properties$relativeLocation$properties$state
      location <- paste(city, state, sep = ", ")
      
      
      
      df <- df$properties$forecast
      
      df <- httr::GET(url = paste0(df))
      
      df <- jsonlite::fromJSON(url(df$url, "rb"))
      
      df <- df$properties$periods
      
      df <- df %>% mutate(temp = stringr::str_c(temperature, temperatureUnit, sep = "-"),
                          wind = str_c(windSpeed, windDirection, sep = "-"),
                          location = location,
                          Date = lubridate::as_date(endTime))
      df <- df %>% select(name,Date, temp, wind, detailedForecast, location)
    
    
    output$nws_table = DT::renderDataTable({DT::datatable(df, options = list(pageLength = 25))})
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
app_ui_snotel <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # List the first level UI elements here 
    fluidPage(
      mod_snotel_ui("snotel_ui_1")
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
app_server_snotel <- function( input, output, session ) {
  # List the first level callModules here
  values <- reactiveValues()
  
  callModule(mod_snotel_server, "snotel_ui_1", values = values)
  
  
}
