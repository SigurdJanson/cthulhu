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
mod_StandardRoll_server <- function(id, Roller, i18n, ActiveLang, Logger = NULL){
  if (!isTruthy(Roller) || !R6::is.R6(Roller)) 
    stop("Module needs a valid roller")
  
  
  GetSuccessIcon <- function(Roll, Show, Type = c("Hard", "Extreme")) {
    if (!isTRUE(Show)) return(list())

    if (Type == "Hard") {
      value <- Roller$MaxHardSuccess(Roll)
      SuccessIcon <- "check"
    }
    else if (Type == "Extreme") {
      value <- Roller$MaxExtremeSuccess(Roll)    
      SuccessIcon <- "check-double"
    } else
      return(list())
    
    if (isTruthy(value)) {
      Success <- list(icon(SuccessIcon), span("<="), value)
      Tooltip <- sprintf(i18n$t("%s success if skill <= %d"), i18n$t(Type), value)
    } else {
      Success <- list(icon(SuccessIcon, class="impossible"))
      Tooltip <- sprintf(i18n$t("%s success not possible with this result"), i18n$t(Type))
    }
    
    Success <- tags$div(
      title=Tooltip, 
      Success)

    return(Success)
  }


  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Identify if there is an icon available for the module's die
    ButtonIcons <- c(d2 = "dice-two", d3 = "dice-three", 
                     d4 = "dice-four", d5 = "dice-five", d6 = "dice-six", 
                     d20 = "dice-d20", d100 = "octopus-deploy")
    myIcon <- ButtonIcons[paste0("d", Roller$DieSides)]
    myIcon <- ifelse(is.null(myIcon), "", myIcon)
    
    # Change this value to notify the module caller of a change
    NotifyStateChange <- reactiveVal(0)
    
    # The value that contains the most recent roll
    RollResult <- reactiveVal(0)
    
    # Roll the dice
    observeEvent(input$btnRoll, {
      RollResult(Roller$Roll())
      
      Logger$Log(paste0(sprintf(i18n$t("Roll %s: %d"), 
                                i18n$t(Roller$Label), 
                                RollResult())))
      NotifyStateChange(NotifyStateChange()+1)
    })
    # Modify last roll by bonus roll
    observeEvent(input$btnBonusRoll, {
      RollResult(Roller$AddModifier(RollResult(), Roller$ModifyType["Bonus"]))

      Logger$Log(paste0(sprintf(i18n$t("Modified %s: %d"), 
                                i18n$t(Roller$Label), 
                                RollResult())))
      NotifyStateChange(NotifyStateChange()+1)
    })
    # Modify last roll by malus roll
    observeEvent(input$btnMalusRoll, {
      RollResult(Roller$AddModifier(RollResult(), Roller$ModifyType["Malus"]))
    
      Logger$Log(paste0(sprintf(i18n$t("Modified %s: %d"), 
                                i18n$t(Roller$Label), 
                                RollResult())))
      NotifyStateChange(NotifyStateChange()+1)
    })
    
    # Display the whole thing: buttons and roll result
    output$ModuleUI <- renderUI({
      # Make sure that this output reacts to changes of ActiveLang
      validate(need(ActiveLang(), "Language must be set"))
      
      btnRoll <- actionButton(ns("btnRoll"), 
                              i18n$t(Roller$Label),
                              icon(myIcon))
      
      # Shall modifier buttons be displayed?
      if (Roller$ModsAllowed) {
        if (RollResult() < 1 || RollResult() > Roller$DieSides || !isTruthy(RollResult())) {
          btnBonusRoll <- shinyjs::disabled(actionButton(ns("btnBonusRoll"), 
                                                         i18n$t("Bonus")))
          btnMalusRoll <- shinyjs::disabled(actionButton(ns("btnMalusRoll"), 
                                                         i18n$t("Malus")))
        } else {
          btnBonusRoll <- actionButton(ns("btnBonusRoll"), 
                                       i18n$t("Bonus"),
                                       icon = icon("plus"))
          btnMalusRoll <- actionButton(ns("btnMalusRoll"), 
                                       i18n$t("Malus"),
                                       icon = icon("minus"))
          
        }
      }
      else
        btnBonusRoll <- btnMalusRoll <- NULL
      
      # Add hard and extreme success
      HardSuccess <- GetSuccessIcon(RollResult(), Roller$IsSkillRoller, "Hard")
      ExtremeSuccess <- GetSuccessIcon(RollResult(), Roller$IsSkillRoller, "Extreme")

      Result <- tagList(btnRoll, btnBonusRoll, btnMalusRoll, 
                        h1(RollResult()), 
                        HardSuccess, ExtremeSuccess)
      return(Result)
    })
    
    # Inform caller of a change
    return(NotifyStateChange)
  })
}
    
## To be copied in the UI
# mod_StandardRoll_ui("StandardRoll_ui_1")
    
## To be copied in the server
# mod_StandardRoll_server("StandardRoll_ui_1")
