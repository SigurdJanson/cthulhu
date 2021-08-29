#library(shinydashboard)
library(shiny.i18n)
#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody
#' box tabBox
#' @noRd
app_ui <- function(request) {
  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Your application UI logic 
    dashboardPage(skin = "black",
      dashboardHeader(title = "Cthulhu", disable = TRUE),
      dashboardSidebar(disable = TRUE),
      dashboardBody(
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        useShinyjs(),
        
        fluidPage(
          usei18n(i18n),
          fluidRow(
            box(
              title = i18n$t("Skills (D100)"), #sprintf("%s (%s)", i18n$t("Skill"), i18n$t("D100")), 
              solidHeader = TRUE,
              width = 4, height = "220px", #background = "black",
              mod_StandardRoll_ui("Roll100")
            ),
            box(title = i18n$t("D3"), solidHeader = TRUE, 
                width = 2, height = "220px",
                mod_StandardRoll_ui("Roll3")),
            box(title = i18n$t("D4"), solidHeader = TRUE,
                width = 2, height = "220px",
                mod_StandardRoll_ui("Roll4")),
            box(title = i18n$t("D6"), solidHeader = TRUE, 
                width = 2, height = "220px",
                mod_StandardRoll_ui("Roll6")),
            box(
              title = i18n$t("D10"), solidHeader = TRUE,
              width = 2, height = "220px",
              collapsible = FALSE,
              mod_StandardRoll_ui("Roll10")
            )
          ),
          fluidRow(
              box(title = i18n$t("Log"), solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  uiOutput("RollLog")
              )
          ),
          tags$footer(
            div(
              a(i18n$t("WWW Site with Core Rules"), 
                target="_blank",
                href="https://www.chaotisch-neutral.de/spielmaterial/cthulhu/regelzusammenfassung-7-edition"),
            ),
            div(
              radioButtons(
                inputId = "lang",
                label = i18n$t("Select language"),
                inline = TRUE,
                choices = i18n$get_languages(),
                selected = i18n$get_translation_language()
              )
            ),
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

