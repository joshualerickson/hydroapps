#' culvert_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom magrittr "%>%"
mod_culvert_map_ui <- function(id){
  ns <- NS(id)
  tagList(leaflet::leafletOutput(ns("ss_maps"), width = '100%', height = '100%'),
         DT::dataTableOutput(ns("ss_table"))
  )
}

#' culvert_map Server Function
#'
#' @noRd 
mod_culvert_map_server <- function(input, output, session, ss_list, shape){
  ns <- session$ns
  
  output$ss_maps <- leaflet::renderLeaflet({
    
    base_map() %>% 
      leaflet::setView(lat = 48.91167, lng = -114.90246, zoom = 7) %>% 
      leaflet::addControl(html = tags$div(title = 'click to add shapefile',actionButton('shape', 'add shapefile')),layerId = 'shape_but', className = 'btn-cust') %>% 
      leaflet::addControl(html = tags$div(title = 'help',actionButton('button', 'get help')),layerId = 'help', className = 'btn-cust') %>% 
      
      leaflet.extras::addDrawToolbar(
        targetGroup = "drain_points",
        markerOptions = leaflet.extras::drawMarkerOptions(markerIcon = leaflet::makeAwesomeIcon('tint', library = 'fa')),
        polylineOptions = F, circleOptions = F,circleMarkerOptions = F,
        rectangleOptions = F,
        polygonOptions = F)  %>%
      leaflet::addMiniMap(tiles = 'Esri.WorldImagery', toggleDisplay = TRUE,
                          position = "bottomleft") %>%
      htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));})

    }")  
  })
  
  lf_prox <- leaflet::leafletProxy("ss_maps")
  observe({
    
    if(class(shape$dat)[[1]] %in% "SpatialPolygonsDataFrame") {
      
      lf_prox %>% leaflet::addPolygons(data = shape$dat, group = 'user_shape',popup =  leafpop::popupTable(shape$dat, className = 'my-popup')) %>%
        leaflet::addLayersControl(overlayGroups = c('Hydrography','user_shape','poly', 'drain_points'), baseGroups = c("Esri.WorldImagery",
                                                                                                                       "CartoDB.Positron",
                                                                                                                       "OpenStreetMap",
                                                                                                                       "CartoDB.DarkMatter",
                                                                                                                       "OpenTopoMap"))
      
    } else if (class(shape$dat)[[1]] %in% 'SpatialPointsDataFrame'){
      
      
      lf_prox %>% leaflet::addMarkers(data = shape$dat, group = 'user_shape',popup =  leafpop::popupTable(shape$dat, className = 'my-popup')) %>%
        leaflet::addLayersControl(overlayGroups = c('Hydrography','user_shape','poly', 'drain_points'), baseGroups = c("Esri.WorldImagery",
                                                                                                                       "CartoDB.Positron",
                                                                                                                       "OpenStreetMap",
                                                                                                                       "CartoDB.DarkMatter",
                                                                                                                       "OpenTopoMap"))
      
    } else if (class(shape$dat)[[1]] %in% 'SpatialLinesDataFrame'){
      
      lf_prox %>% leaflet::addPolylines(data = shape$dat,group = 'user_shape',popup =  leafpop::popupTable(shape$dat, className = 'my-popup')) %>%
        leaflet::addLayersControl(overlayGroups = c('Hydrography','user_shape','poly', 'drain_points'), baseGroups = c("Esri.WorldImagery",
                                                                                                                       "CartoDB.Positron",
                                                                                                                       "OpenStreetMap",
                                                                                                                       "CartoDB.DarkMatter",
                                                                                                                       "OpenTopoMap"))
      
    }
  })
  
  
  observeEvent(input$ss_maps_draw_new_feature, {
    
    #make sure they reset to NULL or start that way
    ss_list$stats <- NULL
    ss_list$state <- NULL
    ss_list$wkID <- NULL
    ss_list$df_poly <- NULL
    
    #capture the coordinates from the users click
    click <- input$ss_maps_draw_new_feature
    clat <- click$geometry$coordinates[[2]]
    clng <- click$geometry$coordinates[[1]]
    ss_list$clat = clat
    ss_list$clng = clng
    content <- paste(
      "<b>Lat: ",round(clat, 5),"</b>",
      "<br>",
      "<b>Long: ", round(clng,5), "</b>"    )
    
    # Now use {streamstats} to delineate watershed
    ss_stats <- reactive({
      
      state <- AOI::geocode_rev(c(clat,clng)) %>% dplyr::select(state)
      state <-  state.abb[grep(paste(state$state), state.name)]
      
      if(length(nchar(state))==0){shinyalert::shinyalert(
        title = "Location Error",
        text = "Must be in the United States, please try again.",
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
        animation = TRUE)}
      req(state)
      
      df1 <- streamstats::delineateWatershed(clng,clat, rcode = state, crs = 4326)
      
      req(df1)
      
      df_poly <- df1 %>%
        streamstats::writeGeoJSON(.data, file.path(tempdir(),"ss_tmp.json"))
      
      error <- tryCatch({
        df_poly %>% geojsonsf::geojson_sf()
      },
      error = function(e) {
        'error'
      })
      
      
      
      if(class(error)[[1]] != 'sf'){shinyalert::shinyalert(
        title = "Server Error",
        text = "No watershed returned for this point, please try again.",
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
        animation = TRUE)}
      
      req(class(error)[[1]] == 'sf')
      
      df_poly <- df_poly %>% geojsonsf::geojson_sf() %>%
        sf::st_as_sf() %>% dplyr::mutate(ID = state, long = clng, lat = clat)
      
      wkID <- df1$workspaceID
      
      incProgress(detail = paste("Computing Basin Characteristics"))
      
      # Now use {streamstats} to compute basin characteristics
      
      stats <- streamstats::computeChars(workspaceID = wkID, rcode = state)
      
      # Tidying
      stats <- stats$parameters %>% dplyr::mutate(ID = state, workspaceID = wkID)
      
      flow_stats <- stats %>% dplyr::select(ID, code, value) %>%
        tidyr::pivot_wider(names_from = "code")
      
      df_poly <- df_poly %>% dplyr::select(Shape_Area, ID, lat, long) %>% dplyr::left_join(flow_stats, by = "ID")
      
      
      
      
      
      # save to reactiveValues
      ss_list$stats <- stats
      ss_list$state <- state
      ss_list$wkID <- wkID
      ss_list$df_poly <- df_poly
     
    })
    
    # dt <- reactive(DT::datatable(ss_list$stats, options = list(pageLength = 25)))
    # mapview::mapshot(x = dt(),
    #                  file = file.path(tempdir(), "dt.png"))
    #Render datatable
    output$ss_table = DT::renderDataTable({

      DT::datatable(ss_list$stats, options = list(pageLength = 25))
      })
     
     
 
    
  
    
    # progress information for when the above code begins
    withProgress(
      
      message = 'Calculation in progress',
      detail = 'This may take about a minute...', value = 0,{
        
        
        setProgress(1/3, detail = paste("Delineating Watershed"))
        
        ss_stats()
        
      }
    )
   
    # set up reactive leaflet map
    #map_leaf <- reactive({
    observe({
     
      if(class(ss_list$df_poly)[[1]] == 'sf'){
        
        lf_prox %>%
          leaflet::addCircleMarkers(data = ss_list$df_poly,
                                    lng = ss_list$df_poly$long,
                                    lat = ss_list$df_poly$lat,
                                    layerId = ~paste("Drain Point"),
                                    color = "red") %>%
          leaflet::addPolygons(data = ss_list$df_poly,
                               popup =  paste0(
                                 "<p style=line-height:30px;margin:0px;>",
                                 "<b>Drainage Area: </b>", paste(attribute_filter_darea(ss_list), " sq.mi"),
                                 "<br>","<b>Precipitation: </b>",ss_list$df_poly$PRECIP,
                                 "<br>","<b>Forest (per): </b>",attribute_filter_forest(ss_list),
                                 "<br>","<b>Temperature: </b>",ss_list$df_poly$TEMP,
                                 "<br>","<b>Max Elevation: </b>",ss_list$df_poly$ELEVMAX,
                                 "<br>","<b>Slope abv 30% (per): </b>",ss_list$df_poly$SLOP30_30M,
                                 "<br>","<b>Slope abv 50% (per): </b>",ss_list$df_poly$SLOP50_30M), 
                               group = "poly") %>% 
          leaflet::addLayersControl(overlayGroups = c("poly", "Hydrography",'user_shape', 'drain_points'),
                                    baseGroups = c("Esri.WorldImagery", "CartoDB.Positron", 
                                                   "OpenStreetMap", "CartoDB.DarkMatter", "OpenTopoMap"))
        
        
      } else {
        
        lf_prox %>%
          leaflet::setView(lat = ss_list$clat, lng = ss_list$clng, zoom = 12) %>% 
          leaflet::addLayersControl(overlayGroups = c("Hydrography", 'user_shape', 'drain_points'),
                                    baseGroups = c("Esri.WorldImagery", "CartoDB.Positron", 
                                                   "OpenStreetMap", "CartoDB.DarkMatter", "OpenTopoMap"))
      }
    })
    
    #for map image in report
    map_leaf <- reactive({
      base_map() %>%
        leaflet::addCircleMarkers(data = ss_list$df_poly,
                                  lng = ss_list$df_poly$long,
                                  lat = ss_list$df_poly$lat,
                                  layerId = ~paste("Drain Point"),
                                  color = "red") %>%
        leaflet::addPolygons(data = ss_list$df_poly,
                             popup =  paste0(
                               "<p style=line-height:30px;margin:0px;>",
                               "<b>Drainage Area: </b>", paste(attribute_filter_darea(ss_list), " sq.mi"),
                               "<br>","<b>Precipitation: </b>",ss_list$df_poly$PRECIP,
                               "<br>","<b>Forest (per): </b>",attribute_filter_forest(ss_list),
                               "<br>","<b>Temperature: </b>",ss_list$df_poly$TEMP,
                               "<br>","<b>Max Elevation: </b>",ss_list$df_poly$ELEVMAX,
                               "<br>","<b>Slope abv 30% (per): </b>",ss_list$df_poly$SLOP30_30M,
                               "<br>","<b>Slope abv 50% (per): </b>",ss_list$df_poly$SLOP50_30M),
                             group = "poly") %>% 
        leaflet::addLayersControl(overlayGroups = c("poly", "Hydrography"),
                                  baseGroups = c("Esri.WorldImagery", "CartoDB.Positron", 
                                                 "OpenStreetMap", "CartoDB.DarkMatter", "OpenTopoMap"))
    })
    # progress letting user know that mapshot is working
    withProgress(
      
      message = 'Calculation in progress',
      detail = 'This may take about a minute...', value = 0,{
        
        
        setProgress(1/2, detail = paste("Generating Map Image"))
        
        
        mapview::mapshot(x = map_leaf()
                         , file = file.path(tempdir(), "customleaf.png")
        )
        
      }
    )
    
  })
  
}
  
  #' culvert_peak_plot UI Function
  #'
  #' @description A shiny Module.
  #'
  #' @param id,input,output,session Internal parameters for {shiny}.
  #'
  #' @noRd 
  #'
  #' @importFrom shiny NS tagList 
  #' @importFrom ggplot2 geom_line geom_ribbon aes ggplot geom_point theme_light labs theme element_text facet_wrap theme_bw annotate
  mod_culvert_peak_plot_ui <- function(id){
    ns <- NS(id)
    tagList(
      plotOutput(ns("ss_peak")),
      DT::dataTableOutput(ns("ss_peak_table")),
      shinydashboard::box(actionButton(ns("peak"), label = "Peak Flow")))
    
  }
  
  #' culvert_peak_plot Server Function
  #'
  #' @noRd 
  mod_culvert_peak_plot_server <- function(input, output, session, ss_list, cul){
    ns <- session$ns
    
    #user must observe event to get going
    observeEvent(input$peak, {
      
      req(ss_list$wkID)
      wkID <- ss_list$wkID
      
      state <- ss_list$state
      
      #calling the streamstats API to get peak flow data
      #only maintaining for Montana right now
      
      ss_peak_ <- reactive({
        base_url <- paste0(
          "https://streamstats.usgs.gov/streamstatsservices/flowstatistics.json?rcode=",state,"&workspaceID=",
          wkID,
          "&includeflowtypes=true"
        )
        
        
        # try to download the data
        error <- httr::GET(url = base_url,
                           httr::write_disk(path = file.path(tempdir(),
                                                             "peak_tmp.json"),overwrite = TRUE))
        
        if(error$status_code == 500){shinyalert::shinyalert(
          title = "Server Error 500",
          text = "Internal Server Error, please try again later (1-2 minutes).",
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
          
        } else if(error$status_code == 400){shinyalert::shinyalert(
          title = "Server Error 400",
          text = "Bad Request, request data cannot be read.",
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
          
        } else if(error$status_code == 404){shinyalert::shinyalert(
          title = "Server Error 404",
          text = "Not Found, resource is not available.",
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
          
        }
        
        req(error$status_code == 200)
        
        peak <- jsonlite::fromJSON(file.path(tempdir(),"peak_tmp.json"))
        
        #tidy nested json file
        peak <- (peak$RegressionRegions[[1]])[[6]][[1]] %>% 
          dplyr::mutate(ReturnInterval = 1/(readr::parse_number(Name)*0.01), code = forcats::fct_reorder(code, ReturnInterval))
        
        #put in reactiveValues
        cul$peak <- peak
        
      })
      
      #progress indicating we started the API
      withProgress(
        
        message = 'Calculation in progress',
        detail = 'This may take about a minute...', value = 0,{
          
          
          setProgress(1/2, detail = paste("Calculating Peak Flow"))
          
          ss_peak_()
        })
      
      #plot the results of the API request
      
      output$ss_peak <- renderPlot({
        
        #checking for interval bounds
        if (length(cul$peak) > 7) {
          
          ggplot(cul$peak , aes(ReturnInterval, Value)) + geom_point() +
            geom_line() + geom_ribbon(aes(ymin=IntervalBounds$Lower, ymax=IntervalBounds$Upper), linetype=1, alpha=0.3)+
            theme_light() +
            labs(x = "Return Interval", y = "Discharge (cfs)", title = "USGS RRE")+theme(text = element_text(size=20))
        } else {
          
          ggplot(cul$peak , aes(ReturnInterval, Value)) + geom_point(size = 2) +
            geom_line(size = 1.5) + 
            theme_light() +
            annotate("text", x = 200, y = max(cul$peak$Value*0.35), label = "One or more of the parameters \n is outside the suggested range. \n Estimates were extrapolated with unknown errors.", size = 9, color = "red") +
            labs(x = "Return Interval", y = "Discharge (cfs)", title = "USGS RRE")+theme(text = element_text(size=20))
          
        }
      })
      
      #provide table of the API request
      output$ss_peak_table = DT::renderDataTable({
        
        #checking for interval bounds
        if (length(cul$peak) > 7) {
          DT::datatable(cul$peak %>% dplyr::select(Name,ReturnInterval,Description,IntervalBounds, Value), options = list(pageLength = 25))
          
        } else {
          
          DT::datatable(cul$peak %>% dplyr::select(Name,ReturnInterval,Description, Value), options = list(pageLength = 25))
          
        }
        
      })
      
    })
    
  }
  
  
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
                                   actionButton(ns("calculate_culvert"), label = "Calculate"),
                                   actionButton(ns('methods_culvert'), label = 'Methods'))),
      column(8,plotly::plotlyOutput(ns("culvert_plot"))), DT::dataTableOutput(ns("culvert_table"))
    )
  }
  
  #' culvert_plotly Server Function
  #'
  #' @noRd 
  mod_culvert_plotly_server <- function(input, output, session, cul, ss_list, peak){
    ns <- session$ns
    
    observeEvent(input$methods_culvert, {
      showModal(tags$div(class = 'body-methods', modalDialog(
        title = "Methods",
        HTML(paste(readLines("inst/app/www/methods.html",encoding = 'UTF-8'))),
easyClose = TRUE,
footer = NULL,
tags$div(class = 'btn-modal',actionButton('dis2', 'Done', class = 'btn-modal'))
      )))
      
    })
    
    observeEvent(ss_list$stats,{
      
      precipUpdate <- ss_list$stats %>%
        dplyr::filter(stringr::str_detect(code, "PRECIP"))

      daUpdate <- attribute_filter_darea(ss_list)


      forUpdate <- attribute_filter_forest(ss_list)
      
      # This will change the value of input$inText, based on x
      updateTextInput(session, "precip_drain", value = precipUpdate$value)
      updateTextInput(session, "drain_area", value = daUpdate)
      updateTextInput(session, "for_known", value = forUpdate)
      
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
        
        culvert_method <- plyr::rbind.fill(Omang_parrett_hull_flows, parrett_and_johnson)
        
        #read in some files for reporting and finishing the culvert estimations
        
        culvert_usgs <- cul$peak
        
        stats_usgs_cul <- ss_list$stats
        
        
        if (is.null(culvert_usgs)) {
          
          culvert_usgs <- data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                                     basin_char = rep(NA_real_))
          
        } else {
          
          
          culvert_usgs <- culvert_usgs %>% dplyr::select(basin_char = Value, ReturnInterval) %>%
            dplyr::mutate(source = "USGS Regression") %>% dplyr::filter(ReturnInterval %in% c(2, 25, 50, 100))}
        
        together <- plyr::rbind.fill(culvert_method, culvert_usgs)
        
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
  
  
  
  #' culvert_report UI Function
  #'
  #' @description A shiny Module.
  #'
  #' @param id,input,output,session Internal parameters for {shiny}.
  #'
  #' @noRd 
  #'
  #' @importFrom shiny NS tagList 
  mod_culvert_report_ui <- function(id){
    ns <- NS(id)
    tagList(
      textInput(ns("author"), "Enter Author."),
      textInput(ns("drain_name"), "Enter a Name or ID for the drain point location."),
      radioButtons(ns('format'), 'Document format', c('HTML', 'Word'),
                   inline = TRUE),
      downloadButton(ns("report_culvert"), "Generate report")
    )
  }
  
  #' culvert_report Server Function
  #'
  #' @noRd 
  mod_culvert_report_server <- function(input, output, session, ss_list){
    ns <- session$ns
    
    final_cul <- reactive({
      
      customleaf <- file.path(tempdir(), "customleaf.png")
      
      stats_usgs_cul <- ss_list$stats
      
      together_long <- ss_list$together_long
      
      drain_name <- input$drain_name
      
      list(together_long = together_long,
           stats_usgs_cul = stats_usgs_cul, drain_name = drain_name,
           customleaf = customleaf)
    })
    
    output$report_culvert <- downloadHandler(
      filename = function() {
        paste('my-report', sep = '.', switch(
          input$format, HTML = 'html', Word = 'docx'
        ))
      },
      
      content = function(file) {
        if(input$format == 'HTML'){
          
          src <- normalizePath(system.file('app/www', 'report.Rmd', package = 'hydroapps'))
          
        } else {
          
          src <- normalizePath(system.file('app/www', 'report_word.Rmd', package = 'hydroapps'))
          
        }
        
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        if(input$format == 'HTML'){
          file.copy(src, 'report.Rmd', overwrite = TRUE)
          
          out <- rmarkdown::render('report.Rmd',
                                   params = list(set_author = input$author),
                                   
                                   switch(
                                     input$format,
                                     HTML = rmarkdown::html_document(), Word = rmarkdown::word_document()
                                   ))
          
        } else {
          file.copy(src, 'report_word.Rmd', overwrite = TRUE)
          
          out <- rmarkdown::render('report_word.Rmd',
                                   params = list(set_author = input$author),
                                   
                                   switch(
                                     input$format,
                                     HTML = rmarkdown::html_document(), Word = rmarkdown::word_document()
                                   ))
        }
        
        
        
        file.rename(out, file)
      }
    )
  }
  
  
  #' The application server-side
  #' 
  #' @param input,output,session Internal parameters for {shiny}. 
  #'     DO NOT REMOVE.
  #' @import shiny
  #' @noRd
  app_server_ss <- function( input, output, session ) {
    # List the first level callModules here
    
    ss_list <- reactiveValues()
    cul <- reactiveValues()
    shape <- reactiveValues()
    shape_names <- reactiveValues()
    
    observeEvent(input$shape,{
      
      showModal(modalFileInput())
      
    })
    
    observeEvent(input$shapefile,  {
      
      
      shpdf <- input$shapefile
      
      if(is.null(shpdf)){
        return()
      }
      previouswd <- getwd()
      uploaddirectory <- dirname(shpdf$datapath[1])
      setwd(uploaddirectory)
      for(i in 1:nrow(shpdf)){
        file.rename(shpdf$datapath[i], shpdf$name[i])
      }
      setwd(previouswd)
      
      map <- rgdal::readOGR(paste(uploaddirectory, shpdf$name[grep(pattern="*.shp$", shpdf$name)], sep="/"))
      
      if(is.na(map@proj4string)) {
        sp::proj4string(map) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        map <- sp::spTransform(map, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
      } else {
        
        map <- sp::spTransform(map, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
      }
      
      shape$dat <- map
      
      #output$text <- renderText(shape$shape_id <- input$shape_id)
    })
    
    observeEvent(input$shapefile, {
      removeModal()
    })
    
    observeEvent(input$button, {
      showModal(modalDialog(
        title = "Quick demo (only works in browser)",
        HTML('<center>
               <video width="1200" height="700" controls>
               <source src="https://www.hydroblog.org/slides/hydroapp_vid/hydroapps_streamstats.mp4" type="video/mp4">
               </video>
               </center>'),
        easyClose = TRUE,
        footer = NULL,
        tags$div(class = 'btn-modal',actionButton('dis', 'Done', class = 'btn-modal'))
      ))
      
    })
    
    observeEvent(input$dis, {
      removeModal()
    })
    
    
    observeEvent(input$dis2, {
      removeModal()
    })
    
    callModule(mod_culvert_map_server, "culvert_map_ui_1", 
               ss_list = ss_list,
               shape = shape)
    callModule(mod_culvert_peak_plot_server, "culvert_peak_plot_ui_1", 
               ss_list = ss_list,
               cul = cul)
    callModule(mod_culvert_plotly_server, "culvert_plotly_ui_1", 
               ss_list = ss_list,
               cul = cul,
               peak = reactive(input$peak))
    callModule(mod_culvert_report_server, "culvert_report_ui_1",
               ss_list = ss_list)
  }
  #' The application User-Interface
  #' 
  #' @param request Internal parameter for `{shiny}`. 
  #'     DO NOT REMOVE.
  #' @import shiny
  #' @noRd
  app_ui_ss <- function(request) {
    tagList(
      # Leave this function for adding external resources
      golem_add_external_resources(),
      
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
  
  