#library for this app
library(shiny)
library(readxl)
library(plyr)
library(openxlsx)
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
      
      # Input: Select which sheet to display
      radioButtons(
        "sht",
        "Display",
        choices = c(
          Nodes = "nodes",
          Points = "points",
          Segments = 'segments'
        ),
        selected = "nodes"
      ),
      
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
    mainPanel(# Output: Data file ----
              tableOutput("contents"))
  )
)

## Define server logic to read selected file ----
server <- function(input, output) {
  output$contents <- renderTable({
    req(input$Amirafile)
    
    # this will load specific datasheet from the Amira file - file have to be converted from XML to xlsx file
    tryCatch({
      Nodes <-
        read_excel(input$Amirafile$datapath, sheet = "Nodes")
      Points <-
        read_excel(input$Amirafile$datapath, sheet = "Points")
      Segments <-
        read_excel(input$Amirafile$datapath, sheet = "Segments")
    })
    
    #Display table with only few first line or, whole table ----
   if (input$disp == "head" && input$sht == "nodes"){
    return(head(Nodes))
   }
    if (input$disp == "all" && input$sht == "nodes"){
      return(Nodes)
    }
    if (input$disp == "head" && input$sht == "points"){
      return(head(Points))
    }
    if (input$disp == "all" && input$sht == "points"){
      return(Points)
    }
    if (input$disp == "head" && input$sht == "segments"){
      return(head(Segments))
    }
    if (input$disp == "all" && input$sht == "segments"){
      return(Segments)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
