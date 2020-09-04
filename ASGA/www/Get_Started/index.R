################################################################################
# Shiny UI-GetStarted
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-21
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
################################################################################

# UI-GetStarted  ---------------------------------------------------------------

GetStarted_UI <- function(id) {
  ns <- NS(id)
  navbarPage(
    title = App_title,
    collapsible = TRUE,
    inverse = TRUE,
    position = "fixed-top",
    id = "innavbar-GS",
    selected = "UploadData",

    # UI-GetStarted-Home  ----------------------------------------------------------

    tabPanel("Home", "Home"),

    # UI-GetStarted-UploadData  ----------------------------------------------------

    tabPanel("Upload Data",
      value = "UploadData",
      tags$div(
        class = "splash-container-GS",
        tags$div(
          class = "splash-GS",
          tags$h1(
            class = "splash-head-GS",
            "Upload your files"
          ),
          tags$p(
            class = "splash-subhead-GS",
            "Upload multiple .xlsx files. For more information see Wiki page."
          ),
          tags$div(
            class = "splash-input-GS-row",
            tags$div(
              class = "splash-input-GS",
              "Upload Amira file for analysis:",
              fileInput(ns("file"),
                label = "",
                multiple = TRUE,
                accept = c(
                  ".xlsx",
                  "All"
                )
              )
            ),
            tags$div(
              class = "splash-input-GS",
              "Upload analyzed data:",
              fileInput(ns("file1"),
                label = "",
                multiple = TRUE,
                accept = c(
                  ".xlsx",
                  "All"
                )
              )
            )
          ),
          tags$div(
            class = "splash-input-GS-row",
            tags$div(
              class = "splash-input-GS-test",
              actionBttn(
                inputId = "Test_unit",
                label = "Test",
                style = "material-flat",
                color = "primary"
              )
            )
          )
        ),
        tags$div(
          class = "footer l-box is-center",
          tags$p("The app is under the GPL V3.0 @ 2019 license")
        )
      )
    ),

    # UI-GetStarted-Settings  -----------------------------------------------------

    tabPanel("Settings",
      value = "Settings",
      tags$div(
        class = "splash-container-GS",
        tags$div(
          class = "splash-GS",
          tags$h1(
            class = "splash-head-GS",
            "Set-Up the analysis"
          )
        ),
        tags$div(
          class = "table-GS",
          fluidRow(
            Setting_Buttons_UI("Home"),
            column(
              4,
              uiOutput(ns("Tool_Info_1"))
            ),
            column(
              4,
              uiOutput(ns("Tool_Info_2"))
            )
          ),
          tags$div(
            class = "table-GS-Center",
            fluidRow(
              actionBttn(
                inputId = "Submit",
                label = "Start Analysis",
                style = "material-flat",
                color = "primary"
              ),
              tags$div(
                class = "asga-button asga-button-primary",
                uiOutput(ns("Download_Button"))
              )
            )
          )
        ),
        tags$div(
          class = "footer l-box is-center",
          tags$p("The app is under the GPL V3.0 @ 2019 license")
        )
      )
    ),

    # UI-GetStarted-Settings  -----------------------------------------------------

    tabPanel("Report",
      value = "Report",
      tags$div(
        class = "splash-container-report",
        tags$h1(
          class = "splash-head-GS",
          "Analysis Report"
        )
      ),
      tags$div(
        class = "splash-content-report",
        tags$p(
          class = "splash-subhead-Report",
          "Settings for the plots"
        ),
        uiOutput(ns("Plot_Settings")),
        uiOutput(ns("Report_Page"))
      ),
      tags$div(
        class = "footer-report l-box is-center",
        tags$p("The app is under the GPL V3.0 @ 2019 license")
      )
    )
  )
}
