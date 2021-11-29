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
        
        src <- normalizePath('inst/app/www/report.Rmd')
        
      } else {
        
        src <- normalizePath('inst/app/www/report_word.Rmd')
        
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
    
## To be copied in the UI
# mod_culvert_report_ui("culvert_report_ui_1")
    
## To be copied in the server
# callModule(mod_culvert_report_server, "culvert_report_ui_1")
 
