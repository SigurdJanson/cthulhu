library(shinydashboard)

#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody
#' box tabBox
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Your application UI logic 
    dashboardPage(skin = "black",
      dashboardHeader(title = "Cthulhu"),
      #, tags$li(imageOutput("imgLogo"), class = "dropdown")
      dashboardSidebar(disable = TRUE),
      dashboardBody(
        fluidPage(
          fluidRow(
            box(
              title = "Fertigkeiten", solidHeader = TRUE, #status = "primary", 
              width = 4, height = "220px", background = "black",
              mod_StandardRoll_ui("Roll100")
            ),
            box(
              title = "W10", solidHeader = TRUE,
              width = 4, height = "220px", background = "black",
              collapsible = FALSE,
              mod_StandardRoll_ui("Roll10")
            ),
            box(width = 4, height = "220px",
                background = "black",
                imageOutput("imgLogo"))
          ),
          fluidRow(          
              tabBox(
                width = 12,
                tabPanel("W3", mod_StandardRoll_ui("Roll3")),
                tabPanel("W4", mod_StandardRoll_ui("Roll4")),
                tabPanel("W6", mod_StandardRoll_ui("Roll6"))
              )
          ),
          tags$footer(
            hr(),
            a("Webseite mit Basisregeln", target="_blank",
              href="https://www.chaotisch-neutral.de/spielmaterial/cthulhu/regelzusammenfassung-7-edition"),
            align = "center"
          )#footer
          
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Cthulhu'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

