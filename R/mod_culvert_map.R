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
  tagList(leaflet::leafletOutput(ns("ss_maps"), width = "100%", height = "100%"),
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
      leaflet::addControl(html = actionButton('shape', 'add shapefile'),layerId = 'shape_but', className = 'btn-cust') %>% 
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
        streamstats::writeGeoJSON(., file.path(tempdir(),"ss_tmp.json"))
      
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
    
    #Render datatable
    output$ss_table = DT::renderDataTable({DT::datatable(ss_list$stats, options = list(pageLength = 25))})
    
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
                             popup = paste0(
          "<p style=line-height:30px;margin:0px;>",
          "<b>Drainage Area: </b>", paste(ss_list$df_poly$CONTDA, " sq.mi"),
          "<br>","<b>Precipitation: </b>",ss_list$df_poly$PRECIP,
          "<br>","<b>Forest (per): </b>",ss_list$df_poly$FOREST,
          "<br>","<b>Temperature: </b>",ss_list$df_poly$TEMP,
          "<br>","<b>Max Elevation: </b>",ss_list$df_poly$ELEVMAX,
          "<br>","<b>Slope abv 30% (per): </b>",ss_list$df_poly$SLOP30_30M,
          "<br>","<b>Slope abv 50% (per): </b>",ss_list$df_poly$SLOP50_30M), group = "poly") %>% 
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
                             popup = paste0(
                               "<p style=line-height:30px;margin:0px;>",
                               "<b>Drainage Area: </b>", paste(ss_list$df_poly$CONTDA, " sq.mi"),
                               "<br>","<b>Precipitation: </b>",ss_list$df_poly$PRECIP,
                               "<br>","<b>Forest (per): </b>",ss_list$df_poly$FOREST,
                               "<br>","<b>Temperature: </b>",ss_list$df_poly$TEMP,
                               "<br>","<b>Max Elevation: </b>",ss_list$df_poly$ELEVMAX,
                               "<br>","<b>Slope abv 30% (per): </b>",ss_list$df_poly$SLOP30_30M,
                               "<br>","<b>Slope abv 50% (per): </b>",ss_list$df_poly$SLOP50_30M), group = "poly") %>% 
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
## To be copied in the UI
# mod_culvert_map_ui("culvert_map_ui_1")
    
## To be copied in the server
# callModule(mod_culvert_map_server, "culvert_map_ui_1")
 
