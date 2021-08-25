#library(shiny.i18n)

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
  
  Roller100 <- CthulhuRoller$new(100, "D100", 10)
  mod_StandardRoll_server("Roll100", Roller100, i18n)
  
  Roller10 <- CthulhuRoller$new(10, "D10")
  mod_StandardRoll_server("Roll10", Roller10, i18n)
  
  Roller6 <- CthulhuRoller$new(6, "D6")
  mod_StandardRoll_server("Roll6", Roller6, i18n)
  
  Roller4 <- CthulhuRoller$new(4, "D4")
  mod_StandardRoll_server("Roll4", Roller4, i18n)

  Roller3 <- CthulhuRoller$new(3, "D3")
  mod_StandardRoll_server("Roll3", Roller3, i18n)
  
  output$imgLogo <- renderImage({
    filename <- normalizePath(
      file.path('./inst/app/www', paste0("cthulhu_logo", '.svg')))
    #print(filename)
    # Return a list containing the file name and alt text
    list(src = filename, 
         contentType = "image/svg+xml",
         height = "200px",
         alt = paste("Cthulhu logo"))
  }, deleteFile = FALSE)
  
  output$RollLog <- renderUI(
    HTML(
      paste0(
        c("<pre style=\"height = \"400px\"\">", 
          "capture.output(print(summary(m0)))", 
          "</pre>"),
        collapse = "<br>"
      )
    )  
  )
  
}
