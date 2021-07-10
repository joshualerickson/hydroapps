#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
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
    
    
    # map <- rgdal::readOGR(paste(uploaddirectory, shpdf$name[grep(pattern="*.shp$", shpdf$name)], sep="/"))#,  delete_null_obj=TRUE)
    # map <- sp::spTransform(map, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    # shape$dat <- map
    
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
