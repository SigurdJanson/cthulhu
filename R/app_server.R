#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  Roller100 <- CthulhuRoller$new(100, "black", 10)
  mod_StandardRoll_server("Roll100", Roller100)
  Roller10 <- CthulhuRoller$new(10, "blue")
  mod_StandardRoll_server("Roll10", Roller10)
}
