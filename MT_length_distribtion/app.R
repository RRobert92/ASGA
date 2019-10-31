#library for this app
library(shiny)
library(shinythemes)
library(readxl)
library(tidyverse)
##library(shinydashboard)

## Set file size lmit to 500mb 
options(shiny.maxRequestSize = 500*1024^2)
## Define UI for application
ui <- fluidPage(
  
  # App title ----
  navbarPage("MTs Length distribution", theme = shinytheme("slate"),
             tabPanel("Info", 
                        navlistPanel(
                          tabPanel("About", includeHTML("about.html")),
                          tabPanel("User Manual", includeHTML("manual.html")),
                          tabPanel("Resource", includeHTML("resource.html"))
                        )),
                    
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
             
             tabPanel("Data",
                      sidebarPanel(
                       
                                 selectInput("analysis", h3("Select Analysis type"), 
                                             choices = list("Histogram Distribtuion" = 1,
                                                            "LOgaritmic Disribtuin" = 2), selected = 1),
                                 conditionalPanel(
                                   condition = "input.analysis == 1",
                                               numericInput("bin.min", "Bin start from (um):", value = 0),
                                               numericInput("bin.max", "Bin stop at (um):", value = 10),
                                               numericInput("bin.step", "Bin every (um);", value = 0.5)
                                 )
                                 ),
                                 
                      
                      mainPanel(
                        
                        plotOutput(outputId = "length.plot_kmts"),
                        plotOutput(outputId = "length.plot_nonkmts")
                      )
                    ),
                      
             tabPanel("Export", "Work in progress")
  ))

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
  
  

output$length.plot_kmts <- renderPlot({
  req(input$Amirafile)
  req(input$analysis)
  req(input$bin.min)
  req(input$bin.max)
  req(input$bin.step)
  tryCatch({
    Segments <- read_excel(input$Amirafile$datapath, sheet = "Segments")
  })
  kmts <- Segments %>% filter_at(vars(starts_with("POle")), any_vars(.>= 1))
  non_kmts <- setdiff(Segments, kmts)
  if(input$analysis == 1){
  xkmts <- kmts$length/10000
  xnon_kmts <- non_kmts$length/10000
  bins <- seq(min(input$bin.min), max(input$bin.max), length.out = (input$bin.max/input$bin.step) + 1)
  hist(xnon_kmts, breaks = bins, col = "yellow", xlab = "Length (um)")
  hist(xkmts, breaks = bins, col = "red",add=T)
  }
  if(input$analysis == 2){
    
  }
})
}
# Run the application
shinyApp(ui = ui, server = server)