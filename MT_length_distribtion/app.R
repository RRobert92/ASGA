#library for this app
library(shiny)
library(shinythemes)
library(readxl)
library(dplyr)

## Define UI for application
ui <- fluidPage(
  
  # App title ----
  navbarPage("MTs Length distribution", theme = shinytheme("slate"),
             tabPanel("Info", "This panel is intentionally left blank"),
             tabPanel("Upload", 
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          
                          # Input: Select a sigle file ----
                          fileInput("Amirafile", "Choose .xlsx File", multiple = FALSE,
                                    accept = c("text/xlsx", "text/comma-separated-values,text/plain", ".xlsx")),
                          
                          # Horizontal space line ----
                          tags$hr(),
                          
                          # Input: Select number of rows to display ----
                          radioButtons("disp", "Display", choices = c(Head = "head", All = "all"), selected = "head"),
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(tabsetPanel(
                          tabPanel("Nodes", tableOutput("data.node")),
                          tabPanel("Points", tableOutput("data.point")),
                          tabPanel("Segments", tableOutput("data.segment"))
                        )),
                      )),
             
             tabPanel("Data", "This panel is intentionally left blank"),
             tabPanel("Export", "This panel is intentionally left blank")
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
    
    #Display table with only few first line for Nodes sheet
    if (input$disp == "head") {
      return(head(Nodes %>% select("Node ID", "X Coord", "Y Coord", "Z Coord")))
    }
    
    else{
      return(Nodes)
    }
  })
  
  output$data.point <- renderTable({
    req(input$Amirafile)
    
    tryCatch({
      Points <- read_excel(input$Amirafile$datapath, sheet = "Points")
    })
    
    #Display table with only few first line for Points sheet
    if (input$disp == "head") {
      return(head(Points %>% select("Point ID", "X Coord", "Y Coord", "Z Coord")))
    }
    
    else{
      return(Points)
    }
  })
  
  output$data.segment <- renderTable({
    req(input$Amirafile)
    
    tryCatch({
      Segments <- read_excel(input$Amirafile$datapath, sheet = "Segments")
    })
    
    #Display table with only few first line for Nodes sheet
    if (input$disp == "head") {
      return(head(Segments %>% select("Segment ID", "length", "Node ID #1", "Node ID #2")))
    }
    
    else{
      return(Segments)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)