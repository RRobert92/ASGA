################################################################################
# Shiny UI-GetStarted
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-21
################################################################################


# UI-GetStarted  ---------------------------------------------------------------

GetStarted_UI <- function(id){
  ns <- NS(id)
  navbarPage(
    title = apptitle,
    collapsible = TRUE,
    inverse = TRUE,
    position = "fixed-top",
    id = "innavbar-GS",
    selected = "UploadData",
    
# UI-GetStarted-Home  ----------------------------------------------------------

tabPanel("Home", "Home"),

# UI-GetStarted-UploadData  ----------------------------------------------------

tabPanel("Upload Data", value = "UploadData",
         tags$div(class = "splash-container-GS",
                  tags$div(class = "splash-GS",
                           tags$h1(class = "splash-head-GS",
                                   "Upload your files"),
                           tags$p(class = "splash-subhead-GS",
                                  "Upload multiple .xlsx files. For more information see Wiki page."),
                           tags$div(class = "splash-input-GS-row",
                                    tags$div(class ="splash-input-GS",
                                             "Upload Amira file for analysis:",
                                             fileInput(ns("file"), label = "",
                                                       multiple = TRUE,
                                                       accept = c(".xlsx", 
                                                                  "All"))),
                                    tags$div(class ="splash-input-GS",
                                             "Upload analyzed data:",
                                             fileInput(ns("file1"), label = "",
                                                       multiple = TRUE,
                                                       accept = c(".xlsx", 
                                                                  "All")))
                           )
                  )
         )
),

# UI-GetStarted-Settings  -----------------------------------------------------

tabPanel("Settings", value = "Settings",
         tags$div(class = "splash-container-GS",
                  tags$div(class = "splash-GS",
                           tags$h1(class = "splash-head-GS",
                                   "Set-Up the analysis")),
                  tags$div(class = "table-GS",
                           fluidRow(Setting_Buttons_UI("Home"),
                                    column(7, 
                                           uiOutput(ns("Tool_Info")
                                           )
                                    )
                           ),
                           tags$div(class = "table-GS-Center",
                                    fluidRow(
                                      actionBttn(
                                        inputId = "Submit",
                                        label = "Start Analysis", 
                                        style = "material-flat",
                                        color = "primary"
                                      ),
                                      tags$div(class = "asga-button asga-button-primary",
                                               uiOutput(ns("Download_Button")))
                                    )
                           )
                  )
         )
),

# UI-GetStarted-Settings  -----------------------------------------------------

tabPanel("Report", value = "Report",
         tags$div(class = "splash-container-GS",
                  tags$div(class = "splash-GS",
                           tags$h1(class = "splash-head-GS",
                                   "Analysis Report")
                  )
         )
)
  )
}