base_map <- function () {
  grp <- c("Esri.WorldImagery", "CartoDB.Positron", 
           "OpenStreetMap", "CartoDB.DarkMatter", "OpenTopoMap", 
           "Hydrography")
  att <- paste0("<a href='https://www.usgs.gov/'>", "U.S. Geological Survey</a> | ", 
                "<a href='https://www.usgs.gov/laws/policies_notices.html'>", 
                "Policies</a>")
  GetURL <- function(service, host = "basemap.nationalmap.gov") {
    sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", 
            host, service)
  }
  map <- leaflet::leaflet()
  map <- leaflet::addProviderTiles(map = map, provider = grp[[1]], 
                                   group = grp[[1]])
  map <- leaflet::addProviderTiles(map = map, provider = grp[[2]], 
                                   group = grp[[2]])
  map <- leaflet::addProviderTiles(map = map, provider = grp[[3]], 
                                   group = grp[[3]])
  map <- leaflet::addProviderTiles(map = map, provider = grp[[4]], 
                                   group = grp[[4]])
  map <- leaflet::addProviderTiles(map = map, provider = grp[[5]], 
                                   group = grp[[5]])
  opt <- leaflet::WMSTileOptions(format = "image/png", 
                                 transparent = TRUE)
  map <- leaflet::addWMSTiles(map, GetURL("USGSHydroCached"), 
                              group = grp[6], options = opt, layers = "0", attribution = att)
  opt <- leaflet::layersControlOptions(collapsed = TRUE)
  map <- leaflet::addLayersControl(map, baseGroups = grp[1:5], 
                                   overlayGroups = grp[6], options = opt)
  map %>% leafem::addMouseCoordinates(epsg = "EPSG:4326", 
                                      proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
}


culvert_size <- function(x) {
  ifelse(x < 11, "(24 in)",
         ifelse(x >= 11 & x < 30,"(36 in)",
                ifelse(x >= 30 & x < 65,"(48 in)",
                       ifelse(x >= 65 & x <110,"(60 in)",
                              ifelse(x >= 110 & x < 180,"(72 in)",
                                     ifelse(x >= 180 & x < 290,"(84 in)",
                                            ifelse(x >= 290 & x < 400,"(96 in)","(Bridge or Big Culvert!)")))))))}


modalFileInput <- function(failed = FALSE) {
  
  modalDialog(
    fileInput('shapefile', label='Add shapefile',
              multiple = TRUE,
              accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj"),
              buttonLabel = "Browse...", placeholder = "No file selected"),
    textInput('shape_id', 'shape ID')
  )
}