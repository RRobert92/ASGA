Viewer_UI <- function(id) {
  ns <- NS(id)

    tabPanel("3D Viewer", value = "3D Viewer",
  fluidRow(
    tags$div(class = "table-3D",
    column(2,
           tags$div(class = "setting_main", style = "margin: 10px",
                    sliderInput("MT_NO", "Number of MT to visualize", value = 1, min = 1, max = 100)
           )),
    column(10,
             rglwidgetOutput("wdg", width = "100%", height = "640px")
           )
    )
  )
  )
}
