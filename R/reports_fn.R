#' Get SNOTEL Report
#'
#' @param site A snotel site id.
#'
#' @return A rendered HTML report
#' @export
#' @importFrom utils browseURL
#'
get_snotel_report <- function(site){
  
  snotel_data <- wildlandhydRo::batch_SNOTELdv(sites = site) %>% 
    mutate(day = lubridate::day(Date),
           month_day = str_c(month, day, sep = "-"),
           month_day = paste0('0000-',month_day),
           month_day = lubridate::as_date(month_day),
           wy = dataRetrieval::calcWaterYear(Date))
  
  phen_data <- snotel_data %>% 
    dplyr::add_count(wy) %>% 
    filter(n >=355) %>% 
    snotelr::snotel_phenology()
  
  if(is.null(phen_data)){
    phenology <- phen_data
  } else {
    
    phenology <- phen_data %>% mutate(max_swe = max_swe*0.0393701,
                                      max_swe_doy_d = as.Date(max_swe_doy, origin = paste0(year,'-01-01')) ,
                                      first_snow_acc_d = as.Date(first_snow_acc, origin = paste0(year,'-01-01')) ,
                                      first_snow_melt_d = as.Date(first_snow_melt, origin = paste0(year,'-01-01')) ,
                                      last_snow_melt_d = as.Date(last_snow_melt, origin = paste0(year,'-01-01')),
                                      cont_snow_acc_d = as.Date(cont_snow_acc, origin = paste0(year,'-01-01')) ,
                                      first_snow_acc_m = as.Date(first_snow_acc, origin = paste0(year,'-01-01')) ,
                                      first_snow_melt_m = as.Date(first_snow_melt, origin = paste0(year,'-01-01')) ,
                                      last_snow_melt_m = as.Date(last_snow_melt, origin = paste0(year,'-01-01')) ,
                                      cont_snow_acc_m = as.Date(cont_snow_acc, origin = paste0(year,'-01-01')))
  }
  
  
  rmarkdown::render(system.file('app/www', 'snotel_stats_static.Rmd', package = 'hydroapps'),
                    output_format = rmarkdown::html_document())
  browseURL(system.file('app/www', 'snotel_stats_static.html', package = 'hydroapps'))
}


#' Get USGS Report
#'
#' @param site A USGS site id.
#'
#' @return A rendered HTML report.
#' @export
#' @importFrom utils browseURL
#'
#'

get_usgs_report <- function(site){
  
  
  nwis_sites_df <- wildlandhydRo::batch_USGSdv(sites = site) %>% 
    mutate(month_day = paste0('0000-',month_day),
           month_day = lubridate::as_date(month_day))
  
  if(nwis_sites_df[1,]$Station %in% "Tobacco River at Eureka, MT"){
    nwis_sites_df <- wildlandhydRo::batch_USGSdv(sites = '12301300') %>% 
      mutate(month_day = paste0('0000-',month_day),
             month_day = lubridate::as_date(month_day)) %>% 
      dplyr::bind_rows(nwis_sites_df)
    
    nwis_sites_df <- nwis_sites_df[!duplicated(nwis_sites_df$Date),]
    
    nwis_sites_df <- nwis_sites_df %>% dplyr::mutate(Station = 'Tobacco River near Eureka MT')
  } 
  
  rmarkdown::render(system.file('app/www', 'usgs_stats_static.Rmd', package = 'hydroapps'),
                    output_format = rmarkdown::html_document())
  
  browseURL(system.file('app/www', 'usgs_stats_static.html', package = 'hydroapps'))
}