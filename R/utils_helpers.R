#generic leaflet map with layers

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

#if else used to give a size for culvert
culvert_size <- function(x) {
  ifelse(x < 11, "(24 in)",
         ifelse(x >= 11 & x < 30,"(36 in)",
                ifelse(x >= 30 & x < 65,"(48 in)",
                       ifelse(x >= 65 & x <110,"(60 in)",
                              ifelse(x >= 110 & x < 180,"(72 in)",
                                     ifelse(x >= 180 & x < 290,"(84 in)",
                                            ifelse(x >= 290 & x < 400,"(96 in)","(Bridge or Big Culvert!)")))))))}

#function used to add file input in modal
modalFileInput <- function(failed = FALSE) {
  
  modalDialog(
    fileInput('shapefile', label='Add shapefile',
              multiple = TRUE,
              accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj"),
              buttonLabel = "Browse...", placeholder = "No file selected"),
    #textInput('shape_id', 'shape ID')
  )
}


#function used to filter through different names but same result

attribute_filter_darea = function(ss_list){
  
  if(ss_list$state %in% c('ID', 'OR', 'WA')){
    
    ss_list$df_poly$DRNAREA
    
  } else if (ss_list$state %in% 'MT'){
    
    ss_list$df_poly$CONTDA
  } else{}
  
}

attribute_filter_forest = function(ss_list){
  
  if(ss_list$state %in% c( 'WA')){
    
    ss_list$df_poly$CANOPY_PCT
    
  } else if (ss_list$state %in% c('OR', 'MT', 'ID')){
    
    ss_list$df_poly$FOREST
  } else{}
  
}


plot_reportUSGS_custom <- function(report, time = "daily", smooth.span = NULL) {
  
  if(missing(report))stop("Need a report dataframe.")
  
  
  if(time == "daily") {
    
    sty.rep <- function(data, breaks, title) {
      
      data +
        theme_bw() +
        labs(title= title,
             y = paste("Discharge")) +
        scale_fill_manual(name="Percentiles",breaks = breaks, labels = c("25<sup>th</sup> - 75<sup>th</sup>",
                                                                         "10<sup>th</sup> - 25<sup>th</sup>",
                                                                         "5<sup>th</sup> - 10<sup>th</sup>",
                                                                         "0 - 5<sup>th</sup>"),
                          values = rev(c("#FF0000","#FFA500","#FFFF00","#006400")))+
        guides(fill = guide_legend(override.aes = list(alpha = .15))) +
        scale_color_manual(name = "", values = "black") +
        theme(legend.position="bottom",
              legend.text = ggtext::element_markdown(),
              axis.ticks.x=element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
    
    percentiles <- report %>%
      dplyr::rename_with(.cols = contains('_va'),.fn = ~stringr::str_replace_all(., "_va", "")) %>%
      mutate(month_day = forcats::fct_reorder(month_day, Date, .desc = T),
             day.of.year = as.numeric(strftime(Date,
                                               format = "%j")))
    if(!is.null(smooth.span)){
      percentiles <- percentiles %>%
        group_by(Station) %>%
        nest() %>%
        mutate(smooth = purrr::map(data, ~smooth_func(.,smooth.span))) %>%
        unnest('smooth') %>% ungroup()
    }
    title.text <- paste0(unique(percentiles$Station), "\n",
                         "Date of plot = ",min(percentiles$Date),' to ', max(percentiles$Date))
    
    label.text <- c("Normal","Drought Watch","Drought Warning",'Drought Emergency')
    
    simple.plot <- ggplot(data = percentiles, aes(x = Date)) +
      geom_ribbon(aes(ymin = p25, ymax = p75, fill = "Normal"), alpha = 0.5) +
      geom_ribbon(aes(ymin = p10, ymax = p25, fill = "Drought Watch"), alpha = 0.5) +
      geom_ribbon(aes(ymin = p05, ymax = p10, fill = "Drought Warning"), alpha = 0.5) +
      geom_ribbon(aes(ymin = min, ymax = p05, fill = 'Drought Emergency')) +
      scale_y_log10(labels = scales::comma) +
      geom_line( aes(x=Date, y=current_daily_mean_flow, color = 'Daily Flow'),size = 0.75)
    
    styled.plot <- simple.plot +
      theme_bw() +
      labs(title= title.text,
           y = paste("Discharge")) +
      scale_fill_manual(name="Percentiles",breaks = label.text, labels = c("25<sup>th</sup> - 75<sup>th</sup>",
                                                                       "10<sup>th</sup> - 25<sup>th</sup>",
                                                                       "5<sup>th</sup> - 10<sup>th</sup>",
                                                                       "0 - 5<sup>th</sup>"),
                        values = rev(c("#FF0000","#FFA500","#FFFF00","#006400")))+
      guides(fill = guide_legend(override.aes = list(alpha = .15))) +
      scale_color_manual(name = "", values = "black") +
      theme(legend.position="bottom",
            legend.text = ggtext::element_markdown(),
            axis.ticks.x=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    
    
    if(length(unique(percentiles$Station))>1){
      styled.plot +
        facet_wrap(~Station, scales = "free") +
        labs(title = '')
    } else {
      
      styled.plot
    }
    
  } else if (time == "month") {
    
    
    per <- c("p05_va","p10_va","p20_va","p25_va","p50_va","p75_va","p80_va","p90_va","p95_va")
    percentiles <- report %>%
      pivot_longer(cols = all_of(per), names_to = "Percentile") %>%
      mutate(Percentile = str_replace_all(Percentile, "_va|p", ""))
    
    if(length(unique(percentiles$Station))>1){
      ggplot(percentiles, aes(Percentile, value))+
        geom_col(fill = "white", aes(color = Station)) +
        geom_hline(aes(yintercept = current_mean_monthly_flow, color = Station), linetype = 3, size = 1) +
        theme_bw() +
        labs(color = "Current Year Reading", title = "Monthly Stats: POR Percentiles and Current Year Stat.",
             y = "Monthly Flow Stats") +
        facet_wrap(~month, scales = "free")
    } else {
      
      ggplot(percentiles, aes(Percentile, value))+
        geom_col(fill = "white", color = "black") +
        geom_hline(aes(yintercept = current_mean_monthly_flow, color = Station), linetype = 3, size = 1) +
        theme_bw() +
        labs(color = "Current Year Reading", title = "Monthly Stats: POR Percentiles and Current Year Stat.",
             y = "Monthly Flow Stats") +
        facet_wrap(~month, scales = "free")
    }
    
    
  }
  
}