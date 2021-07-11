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
        dplyr::mutate(ReturnInterval = c(1.5,2,2.33,5,10,25,50,100,200,500), code = forcats::fct_reorder(code, ReturnInterval))
      
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
    
## To be copied in the UI
# mod_culvert_peak_plot_ui("culvert_peak_plot_ui_1")
    
## To be copied in the server
# callModule(mod_culvert_peak_plot_server, "culvert_peak_plot_ui_1")
 
