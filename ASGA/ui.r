################################################################################
# Shiny UI-Home
#
# Author: Robert Kiewisz
# Created: 2020-05-16 
################################################################################


# Shiny UI-Home  ---------------------------------------------------------------
fluidPage(
  includeCSS("www/css/pure-mini.css"),
  includeCSS("www/css/grids-responsive-min.css"),
  includeCSS("https://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.css"),
  includeCSS("www/css/style.css"),
  
  useShinyalert(),
  
  navbarPage(title = "ASGA - Automati Spatial Graph Analysis",
             collapsible = TRUE,
             inverse = TRUE,
             position = "fixed-top",
             id = "innavbar",
             selected = "Home",
             footer = footnoteUI("footnore"),
             tabPanel("GetStarted",
                      GetStarted_UI("Home")
             ),
             
             tabPanel("Home",
                      fluidRow(
                        homeUI("Home")
                      )
             ), 
             
             tabPanel("Wiki",
                      fluidRow()
             ),
             
             tabPanel("About",
                      fluidRow()
             )
  )
)