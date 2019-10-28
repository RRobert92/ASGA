#library for this app
library(shiny)
library(readxl)
library(dplyr)

## Define UI for application
ui <- fluidPage(
  # App title ----
  titlePanel("Uploading Amira excel SpatialGraph"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select a sigle file ----
      fileInput(
        "Amirafile",
        "Choose .xlsx File",
        multiple = FALSE,
        accept = c(
          "text/xlsx",
          "text/comma-separated-values,text/plain",
          ".xlsx"
        )
      ),
      
      # Horizontal space line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons(
        "disp",
        "Display",
        choices = c(Head = "head", All = "all"),
        selected = "head"
      ),
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
              
              tabsetPanel(
                tabPanel("Nodes",  tableOutput("data.node")),
                tabPanel("Points", tableOutput("data.point")),
                tabPanel("Segments", tableOutput("data.segment"))
              )),
  )
)

## Define server logic to read selected file ----
server <- function(input, output) {
  output$data.node <- renderTable({
    req(input$Amirafile)
    
    # this will load specific datasheet from the Amira file - file have to be converted from XML to xlsx file
    tryCatch({
      Nodes <- read_excel(input$Amirafile$datapath, sheet = "Nodes")
    })
    
    #Display table with only few first line or, whole table ----
    if (input$disp == "head") {
      return(head(Nodes %>% select(
        "Node ID", "X Coord", "Y Coord", "Z Coord"
      )))
    }
    if (input$disp == "all") {
      return(Nodes)
    }
  })
  output$data.point <- renderTable({
    req(input$Amirafile)
    
    # this will load specific datasheet from the Amira file - file have to be converted from XML to xlsx file
    tryCatch({
      Points <-
        read_excel(input$Amirafile$datapath, sheet = "Points")
    })
    
    #Display table with only few first line or, whole table ----
    if (input$disp == "head") {
      return(head(
        Points %>% select("Point ID", "X Coord", "Y Coord", "Z Coord")
      ))
    }
    if (input$disp == "all") {
      return(Points)
    }
  })
  output$data.segment <- renderTable({
    req(input$Amirafile)
    
    # this will load specific datasheet from the Amira file - file have to be converted from XML to xlsx file
    tryCatch({
      Segments <-
        read_excel(input$Amirafile$datapath, sheet = "Segments")
    })
    
    #Display table with only few first line or, whole table ----
    
    if (input$disp == "head") {
      return(head(
        Segments %>% select("Segment ID", "X Coord", "Y Coord", "Z Coord")
      ))
    }
    if (input$disp == "all") {
      return(Segments)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
