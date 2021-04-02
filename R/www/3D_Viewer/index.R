################################################################################
# Shiny UI-3D viewer
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-04-02
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
################################################################################

# UI-3D viewer  ---------------------------------------------------------------
Viewer_UI <- function(id) {
  ns <- NS(id)
  navbarPage(
    title = APP_TITLE,
    collapsible = TRUE,
    inverse = TRUE,
    position = "fixed-top",
    id = "innavbar-3D",
    selected = "UploadData",

    # UI-3D viewer-Home  ------------------------------------------------------

    tabPanel(
      title = "Home",
      value = "Home"
    ),
    tabPanel(
      title = "3D Module",
      value = "3D_Data_Select",
      tags$div(
        class = "splash-container-GS",
        tags$div(
          class = "splash-GS",
          tags$h1(
            class = "splash-head-GS",
            "3D module"
          ),
          tags$p(
            class = "splash-subhead-GS",
            "Select data to visualized from one of our paper or load your own Amira .am files."
          ),
          tags$div(
            class = "splash-input-3D-row",
            tags$div(
              class = "splash-input-3D",
              "Select publication",
              tags$div(
                class = "btn-default-3D",
                actionButton("3D_Viewer_Pub_1", "Test pub 1", width = "100%")
              ),
              tags$div(
                class = "btn-default-3D",
                actionButton("3D_Viewer_Pub_2", "Test pub 2", width = "100%")
              ),
            ),
            tags$div(
              class = "splash-input-3D",
              tags$p(),
              fileInput(ns("file_3D"),
                label = "Load 3D model",
                multiple = FALSE,
                accept = ".am"
              )
            ),
            tags$div(
              class = "splash-input-3D",
              actionBttn(
                inputId = "Demo_3D_View",
                label = "Demo",
                style = "material-flat",
                color = "primary"
              )
            )
          )
        ),
        tags$div(
          class = "footer l-box is-center",
          tags$p(CC)
        )
      )
    ),
    tabPanel(
      title = "3D Viewer",
      value = "3D_Viewer",
      fluidRow(
        tags$div(
          class = "table-3D",
          column(
            2,
            offset = 0,
            tags$div(
              class = "setting_main", style = "margin: 10px",
              sliderInput(ns("MT_NO"), "Number of MT to visualize", value = 1, min = 1, max = 100)
            )
          ),
          column(
            10,
            offset = 0, style = "padding:0px;",
            rglwidgetOutput("wdg", width = "100%", height = WINDOW_HEIGHT) %>% withSpinner()
          )
        )
      )
    ),
    tabPanel(
      title = "Wiki",
      value = "Wiki"
    ),
    tabPanel(
      title = "About",
      value = "About"
    )
  )
}
