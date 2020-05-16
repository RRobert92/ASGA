################################################################################
# Shiny UI-Home
#
# Author: Robert Kiewisz
# Created: 2020-05-16 
################################################################################


# Shiny UI-Home  ---------------------------------------------------------------

homeUI <- function(id){
  ns <- NS(id)
  
 tags$div(class = "splash-container",
         tags$div(class = "splash",
           tags$h1(class = "splash-head",
             "Automatic Spatial Graph Analysis"),
           tags$p(class = "splash-subhead",
             "ASGA is an open-source cluster of tools with its ongoing development for a meiotic and mitotic spindle analysis."),
           actionButton('GetStarted', 'Get Started', class = "pure-button pure-button-primary"),
           actionButton('Wiki', 'Wiki', class = "pure-button pure-button-primary"))
         )
}

# UI-Footnote  ---------------------------------------------------------------


footnoteUI <- function(id){
  ns <- NS(id)
  
  tags$div(class = "footer l-box is-center",
          tags$p("The app is under the GPL V3.0 @ 2019 license")
          )
}
