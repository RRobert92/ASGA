################################################################################
# Shiny UI-Home
#
# Author: Robert Kiewisz
# Created: 2020-05-16 
################################################################################


# Shiny UI-Home  ---------------------------------------------------------------

GetStarted_UI <- function(id){
  ns <- NS(id)
  navbarPage(
    title = "ASGA",
    collapsible = TRUE,
    inverse = TRUE,
    position = "fixed-top",
    id = "innavbar-GS",
    selected = "UploadData",

# UI-Home-Home  ---------------------------------------------------------------

    tabPanel("Home", "Home"),

# UI-Home-UploadData  ---------------------------------------------------------

    tabPanel("Upload Data", value = "UploadData",
             tags$div(class = "splash-container-GS",
                      tags$div(class = "splash-GS",
                               tags$h1(class = "splash-head-GS",
                                       "Upload your files"),
                               tags$p(class = "splash-subhead-GS",
                                      "Upload multiple .xlsx files. For mor information see Wiki."),
                               tags$div(fileInput(ns("file"), label = "",
                                                  multiple = TRUE,
                                                  accept = c(".xlsx", 
                                                             "All"))   
                               )
                      )
             )
    ),

# UI-Home-Settings  ----------------------------------------------------------

    tabPanel("Settings", value = "Settings",
             tags$div(class = "splash-container-GS",
                      tags$div(class = "splash-GS",
                               tags$h1(class = "splash-head-GS",
                                       "Set-Up the analysis")),
                      tags$div(class = "table-GS",
                               fluidRow(Setting_Buttons_UI("Home"),
                                        column(7, 
                                               uiOutput(ns("ToolInfo")))),
                               tags$div(class = "table-GS-Center",
                                        fluidRow(
                                 actionBttn(
                                   inputId = "Submit",
                                   label = "Start Analysis", 
                                   style = "material-flat",
                                   color = "primary"
                                 )
                               )
                               )
                      )
             )
    )
  )
}
