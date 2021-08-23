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
mod_StandardRoll_server <- function(id, Roller){
  if (!isTruthy(Roller) || !R6::is.R6(Roller)) stop("Module needs a valid roller")
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    RollResult <- reactiveVal(0)
    
    observeEvent(input$btnRoll, {
      RollResult(Roller$Roll())
    })
    observeEvent(input$btnBonusRoll, {
      RollResult(Roller$AddModifier(RollResult(), Roller$ModifyType["Bonus"]))
    })
    observeEvent(input$btnMalusRoll, {
      RollResult(Roller$AddModifier(RollResult(), Roller$ModifyType["Malus"]))
    })
    
    output$ModuleUI <- renderUI({
      btnRoll <- actionButton(ns("btnRoll"), Roller$Label)
      if (Roller$ModsAllowed) {
        btnBonusRoll <- actionButton(ns("btnBonusRoll"), paste("Bonus Roll"))
        btnMalusRoll <- actionButton(ns("btnMalusRoll"), paste("Malus Roll"))
      }
      else
        btnBonusRoll <- btnMalusRoll <- NULL

      Result <- tagList(btnRoll, btnBonusRoll, btnMalusRoll, h1(RollResult()))
      return(Result)
    })
    
  })
}
    
## To be copied in the UI
# mod_StandardRoll_ui("StandardRoll_ui_1")
    
## To be copied in the server
# mod_StandardRoll_server("StandardRoll_ui_1")
