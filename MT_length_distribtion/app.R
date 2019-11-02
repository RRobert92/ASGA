library(shinydashboard)
library(shiny)
library(readxl)
library(tidyverse)

##Maximum size of memory used by R, set to 500mb
options(shiny.maxRequestSize = 500*1024^2)

ui <- dashboardPage(
  dashboardHeader(title = "Length Distribtion"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "menu_about", icon = icon("info-circle")),
      menuItem("Instruction", tabName = "menu_instruction", icon = icon("chalkboard-teacher")),
      menuItem("Upload", tabName = "menu_upload", icon = icon("upload")),
      menuItem("Analysis", tabName = "menu_analysis", icon = icon("diagnoses")),
      menuItem("Export", tabName = "menu_export", icon = icon("download")),
      menuItem("Resource", tabName = "menu_resource", icon = icon("github"))
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "menu_about",
              includeHTML("about.html")
      ),
      
      tabItem(tabName = "menu_instruction",
              includeHTML("manual.html")
      ),
      
      tabItem(tabName = "menu_upload",
              fluidRow(
                box(title = "Upload file", width = 3,
                    fileInput("amirafile", "Choose .xlsx File", multiple = FALSE,
                              accept = c("text/xlsx", "text/comma-separated-values,text/plain", ".xlsx")),
                ),
                box(title = "Lodate data:", width = 9,
                    radioButtons("disp", "Display", choices = c(Head = "head", All = "all"), selected = "head"),
                    tabsetPanel(
                      tabPanel("Nodes", tableOutput("data.node")),
                      tabPanel("Points", tableOutput("data.point")),
                      tabPanel("Segments", tableOutput("data.segment"))
                    )))
      ),
      
      tabItem(tabName = "menu_analysis",
              fluidRow(
                box(title = "Analysis Parameaters", width = 2, style = "height:350px;",
                    selectInput("analysis", h3("Select Analysis type"), 
                                choices = list("Histogram Distribtuion" = 1,
                                               "Logaritmic Disribtuin" = 2), selected = 1),
                    conditionalPanel(
                      condition = "input.analysis == 1",
                      numericInput("bin.min", "Bin start from (um):", value = 0),
                      numericInput("bin.max", "Bin stop at (um):", value = 10),
                      numericInput("bin.step", "Bin every (um);", value = 0.5)),
                ),
                box(title = "Graph Type", width = 2,style = "height:350px;"),
                box(title = "Graph Parameaters", width = 2, style = "height:350px;"),
                valueBoxOutput("avg.length.kmts", width = 3),
                valueBoxOutput("avg.length.non.kmts", width = 3)
              ),
              fluidRow(
                box(title = "Graphs", width = 12,
                    plotOutput(outputId = "length.plot_kmts"))
              )
      ),
      tabItem(tabName = "menu_export",
              
      ),
      tabItem(tabName = "menu_resource",
              includeHTML("manual.html")
      )
    )
  )
)

server <- function(input, output) {
  
  output$data.node <- renderTable({
    req(input$amirafile)
    
    tryCatch({
      Nodes <- read_excel(input$amirafile$datapath, sheet = "Nodes")
    }) 
    
    if (input$disp == "head") {
      return(head(Nodes %>% select("Node ID", "X Coord", "Y Coord", "Z Coord")))
    }
    else{
      return(Nodes)
    }
  })
  
  output$data.point <- renderTable({
    req(input$amirafile)
    
    tryCatch({
      Points <- read_excel(input$amirafile$datapath, sheet = "Points")
    })
    
    if (input$disp == "head") {
      return(head(Points %>% select("Point ID", "X Coord", "Y Coord", "Z Coord")))
    }
    else{
      return(Points)
    }
  })
  
  output$data.segment <- renderTable({
    req(input$amirafile)
    
    tryCatch({
      Segments <- read_excel(input$amirafile$datapath, sheet = "Segments")
    })
    
    if (input$disp == "head") {
      return(head(Segments %>% select("Segment ID", "length", "Node ID #1", "Node ID #2")))
    }
    else{
      return(Segments)
    }
  })
  
  output$length.plot_kmts <- renderPlot({
    req(input$amirafile)
    req(input$analysis)
    req(input$bin.min)
    req(input$bin.max)
    req(input$bin.step)
    
    tryCatch({
      Segments <- read_excel(input$amirafile$datapath, sheet = "Segments")
    })
    
    kmts <- Segments %>% filter_at(vars(starts_with("Pole")), any_vars(.>= 1))
    non_kmts <- setdiff(Segments, kmts)
    if(input$analysis == 1){
      xkmts <- kmts$length/10000
      xnon_kmts <- non_kmts$length/10000
      bins <- seq(min(input$bin.min), max(input$bin.max), length.out = (input$bin.max/input$bin.step) + 1)
      hist(xnon_kmts, breaks = bins, col = "yellow", xlab = "Length (um)", ylab = "No. of MTs")
      hist(xkmts, breaks = bins, col = "red",add=T)
    }
    if(input$analysis == 2){
      
    }
  })
  
  output$avg.length.kmts <- renderValueBox({
    req(input$amirafile)
    
    tryCatch({
      Segments <- read_excel(input$amirafile$datapath, sheet = "Segments")
    })
    
    kmts <- Segments %>% filter_at(vars(starts_with("Pole")), any_vars(.>= 1))
    length.kmts <- round(mean(kmts$length/10000), 2)
    sd.kmts <- round(sd(kmts$length/10000),2)
    
    valueBox(
      paste(length.kmts, "±", sd.kmts), "Avg. KMTs length", icon = icon("calculator"), color = "red")
  })
  
  output$avg.length.non.kmts <- renderValueBox({
    req(input$amirafile)
    
    tryCatch({
      Segments <- read_excel(input$amirafile$datapath, sheet = "Segments")
    })
    
    kmts <- Segments %>% filter_at(vars(starts_with("Pole")), any_vars(.>= 1))
    non_kmts <- setdiff(Segments, kmts)
    length.non_kmts <- round(mean(non_kmts$length/10000), 2)
    sd.non.kmts <- round(sd(non_kmts$length/10000),2)
    
    valueBox(
      paste(length.non_kmts, "±", sd.non.kmts), "Avg. Non-KMTs length", icon = icon("calculator"), color = "yellow")
  })
}

shinyApp(ui = ui, server = server)
