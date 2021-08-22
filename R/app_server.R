#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  Roller100 <- CthulhuRoller$new(100, "W100", 10)
  mod_StandardRoll_server("Roll100", Roller100)
  
  Roller10 <- CthulhuRoller$new(10, "W10")
  mod_StandardRoll_server("Roll10", Roller10)
  
  Roller6 <- CthulhuRoller$new(6, "W6")
  mod_StandardRoll_server("Roll6", Roller6)
  
  Roller4 <- CthulhuRoller$new(4, "W4")
  mod_StandardRoll_server("Roll4", Roller4)

  Roller3 <- CthulhuRoller$new(3, "W3")
  mod_StandardRoll_server("Roll3", Roller3)
  
  output$imgLogo <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(
      file.path('./www', paste0("cthulhu_logo", '.svg')))
    
    # Return a list containing the filename and alt text
    list(src = filename, alt = paste("Cthulhu logo"))
    
  }, deleteFile = FALSE)
  
}
