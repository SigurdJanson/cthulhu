#' StandardRoll UI Function
#'
#' @description Provides a tile for default rolls, incl. "Fertigkeiten" and other
#' rolls that can be modified.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_StandardRoll_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("ModuleUI"))
  )
}
    
#' StandardRoll Server Functions
#'
#' @noRd 
mod_StandardRoll_server <- function(id, die = 100, moddie = 10){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    RollResult <- reactiveVal(0)
    
    observeEvent(input$btnRoll, {
      RollResult(sample.int(100, 1))
    })
    observeEvent(input$btnModRoll, {
      Result <- (sample.int(10, 1)-1) * 10 + max(sample.int(10, 2))
      RollResult(Result)
    })
    
    output$ModuleUI <- renderUI({
      label <- paste0("1d", die)
      
      tagList(
        wellPanel(
          actionButton(ns("btnRoll"), label),
          actionButton(ns("btnModRoll"), paste(label, "mod")),
          h1(RollResult())
        )
      )
    })
    
  })
}
    
## To be copied in the UI
# mod_StandardRoll_ui("StandardRoll_ui_1")
    
## To be copied in the server
# mod_StandardRoll_server("StandardRoll_ui_1")
