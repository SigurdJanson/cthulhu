#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("en")
  
  # keep track of language object as a reactive
  i18n_r <- reactive({
    i18n
  })
  # change language
  observeEvent(input$lang, {
    shiny.i18n::update_lang(session, input$lang)
    i18n_r()$set_translation_language(input$lang)
  })

  # Logging
  Logger <- RollLogger$new()

  # Roller modules
  Roller100 <- CthulhuRoller$new(100, "D100", 10)
  OnRoll100 <- mod_StandardRoll_server("Roll100", Roller100, i18n, Logger)
  
  Roller10 <- CthulhuRoller$new(10, "D10")
  OnRoll10 <- mod_StandardRoll_server("Roll10", Roller10, i18n, Logger)
  
  Roller6 <- CthulhuRoller$new(6, "D6")
  OnRoll6 <- mod_StandardRoll_server("Roll6", Roller6, i18n, Logger)
  
  Roller4 <- CthulhuRoller$new(4, "D4")
  OnRoll4 <- mod_StandardRoll_server("Roll4", Roller4, i18n, Logger)

  Roller3 <- CthulhuRoller$new(3, "D3")
  OnRoll3 <- mod_StandardRoll_server("Roll3", Roller3, i18n, Logger)
  
  
  
  
  output$RollLog <- renderUI({
    req(OnRoll100(), OnRoll10(), OnRoll6(), OnRoll4(), OnRoll3())
    
    Style <- c("height:200px", 
               "background:#0C090A", "color:white",
               "opacity:0.5")
    Style <- paste0(Style, collapse = ";")
    
    HTML(
      paste0(
        c("<pre style=\"", Style, "\">", 
          Logger$AsHtml(), 
          "</pre>"),
        collapse = ""
      )
    )
  })
  
}
