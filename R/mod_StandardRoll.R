library(shinyjs)

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
mod_StandardRoll_server <- function(id, Roller, i18n, Logger = NULL){
  if (!isTruthy(Roller) || !R6::is.R6(Roller)) 
    stop("Module needs a valid roller")
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ButtonIcons <- c(d2 = "dice-two", d3 = "dice-three", 
                     d4 = "dice-four", d5 = "dice-five", d6 = "dice-six", 
                     d20 = "dice-d20", d100 = "octopus-deploy")
    myIcon <- ButtonIcons[paste0("d", Roller$DieSides)]
    myIcon <- ifelse(is.null(myIcon), "", myIcon)
        
    NotifyStateChange <- reactiveVal(0)
    
    RollResult <- reactiveVal(0)
    
    observeEvent(input$btnRoll, {
      RollResult(Roller$Roll())
      
      Logger$Log(paste0(sprintf("Roll %s: %d", Roller$Label, RollResult())))
      NotifyStateChange(NotifyStateChange()+1)
    })
    observeEvent(input$btnBonusRoll, {
      RollResult(Roller$AddModifier(RollResult(), Roller$ModifyType["Bonus"]))

      Logger$Log(paste0(sprintf("Modified %s: %d", Roller$Label, RollResult())))
      NotifyStateChange(NotifyStateChange()+1)
    })
    observeEvent(input$btnMalusRoll, {
      RollResult(Roller$AddModifier(RollResult(), Roller$ModifyType["Malus"]))
    
      Logger$Log(paste0(sprintf("Modified %s: %d", Roller$Label, RollResult())))
      NotifyStateChange(NotifyStateChange()+1)
    })
    
    output$ModuleUI <- renderUI({
      print(myIcon)
      btnRoll <- actionButton(ns("btnRoll"), 
                              i18n$t(Roller$Label),
                              icon(myIcon))
      
      if (Roller$ModsAllowed) {
        if (RollResult() < 1 || RollResult() > Roller$DieSides || !isTruthy(RollResult())) {
          btnBonusRoll <- shinyjs::disabled(actionButton(ns("btnBonusRoll"), 
                                                         i18n$t("Bonus Roll")))
          btnMalusRoll <- shinyjs::disabled(actionButton(ns("btnMalusRoll"), 
                                                         i18n$t("Malus Roll")))
        } else {
          btnBonusRoll <- actionButton(ns("btnBonusRoll"), 
                                       i18n$t("Bonus Roll"),
                                       icon = icon("plus"))
          btnMalusRoll <- actionButton(ns("btnMalusRoll"), 
                                       i18n$t("Malus Roll"),
                                       icon = icon("minus"))
          
        }
      }
      else
        btnBonusRoll <- btnMalusRoll <- NULL

      Result <- tagList(btnRoll, btnBonusRoll, btnMalusRoll, h1(RollResult()))
      return(Result)
    })
    
    return(NotifyStateChange)
  })
}
    
## To be copied in the UI
# mod_StandardRoll_ui("StandardRoll_ui_1")
    
## To be copied in the server
# mod_StandardRoll_server("StandardRoll_ui_1")
