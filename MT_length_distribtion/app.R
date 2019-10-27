#library for this app
library(shiny)
library(readxl)
library(plyr)
library(tibble)
# Define UI for application that draws a histogram
# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Amira excel SpatialGraph"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a sigle file ----
      fileInput("Amirafile", "Choose .xlsx File",
                multiple =FALSE,
                accept = c("text/xlsx",
                           "text/comma-separated-values,text/plain",
                           ".xlsx")),
      # Horizontal space line ----
     tags$hr(),
     
     # Input: select if Excel file has multiple sheets ----
     radioButtons("multi","Multiple excel sheets?",
                  choices = c(Yes = "yes",
                              No = "no"),
                  selected = "yes"
       
     ),
     
     # Horizontal space line ----
     tags$hr(),
     
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$contents <- renderTable({
    
    
    req(input$Amirafile)
    
    # This will check if the excel file has multiple or single sheets, and will read it acordingly ----
    tryCatch(
      {
        if(input$multi == "yes") {
          return(df <- lapply(input$Amirafile$datapath,
                              function(x) read_excel(path = input$Amirafile$datapath, sheet = x))
          )
        }
        else {
          return(df <- lapply(input$Amirafile$datapath,
                              function(x) read_excel(path = input$Amirafile$datapath))
            
          )
        }
        
        
      },
    )

    #Display table with only few first line or, whole table ----
    if(input$disp == "head") {
      return(head(df))
    }
    
    else {
      return(df)
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
  