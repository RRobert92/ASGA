library(shinydashboardPlus)
library(shinydashboard)
library(shiny)
library(readxl)
library(tidyverse)

##Maximum size of memory used by R, set to 500mb
options(shiny.maxRequestSize = 500*1024^2)

ui <- dashboardPagePlus(
  
  dashboardHeaderPlus(title = "Length Distribtion"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", 
               tabName = "menu_about", 
               icon = icon("info-circle")
      ),
      menuItem("Instruction", 
               tabName = "menu_instruction", 
               icon = icon("chalkboard-teacher")
      ),
      menuItem("Upload", 
               tabName = "menu_upload", 
               icon = icon("upload")
      ),
      menuItem("Analysis", 
               tabName = "menu_analysis", 
               icon = icon("diagnoses")
      ),
      menuItem("Export", 
               tabName = "menu_export", 
               icon = icon("download")
      )
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
                boxPlus(title = "Upload file", 
                        width = 3,
                        status = "warning", 
                        closable = FALSE,
                        p(fileInput("amirafile", "Choose .xlsx File", multiple = FALSE,
                                    accept = c("text/xlsx", "text/comma-separated-values,text/plain", ".xlsx")
                        )
                        )
                ),
                boxPlus(title = "Lodate data:", 
                        width = 9,
                        status = "primary", 
                        closable = FALSE,
                        p(radioButtons("disp",
                                       "Display", 
                                       choices = c(Head = "head", 
                                                   All = "all"), 
                                       selected = "head"),
                          tabsetPanel(
                            tabPanel("Nodes", 
                                     tableOutput("data.node")
                            ),
                            tabPanel("Points", 
                                     tableOutput("data.point")
                            ),
                            tabPanel("Segments", 
                                     tableOutput("data.segment")
                            )
                          )
                        )
                )
              )
      ),
      
      tabItem(tabName = "menu_analysis",
              fluidRow(
                column(width = 2,
                       box(title = "Analysis Parameaters", 
                           width = NULL,
                           style = "height:320px;", 
                           status = "warning", 
                           closable = FALSE,
                           selectInput("analysis", 
                                       "Select Analysis type", 
                                       choices = list("Histogram Distribution" = 1,
                                                      "Density Distribtuion" = 2,
                                                      "Logaritmic Disribtuin" = 3), 
                                       selected = 1),
                           conditionalPanel(
                             condition = "input.analysis == 1",
                             numericInput("bin.min", "Bin start from (um):",
                                          value = 0),
                             numericInput("bin.max", "Bin stop at (um):", 
                                          value = 10),
                             numericInput("bin.step", "Bin every (um);", 
                                          value = 0.5)
                           )
                       ),
                       box(title = "Parameaters",
                           width = NULL,
                           style = "height:380px;",
                           status = "warning",
                           collapsible = FALSE,
                           closable = FALSE,
                           accordion(
                             accordionItem(
                               id = 2, 
                               title = "what to plot?", 
                               color = "warning", 
                               selectInput("display.on.plot", 
                                           "Select what to display",
                                           choices = c("All" = 1, 
                                                       "KMTs" = 2,
                                                       "Non_KMTs" = 3), 
                                           selected = 1)
                             ),
                             accordionItem(
                               id = 1,
                               title = "Graph Type",
                               color = "warning",
                               selectInput("select.graph",
                                           "",
                                           choices = list("Bar Graph" = 1,
                                                          "Line Graph" = 2, 
                                                          "Area Graph" = 3), 
                                           selected = 1)
                             ),
                             accordionItem(id = 3, 
                                           title = "Axis Labes", 
                                           color = "warning",
                                           textInput("x.label", 
                                                     "Label for the X axis", 
                                                     value = "MT Length (µm)"),
                                           textInput("y.label", 
                                                     "Label for the Y axis", 
                                                     value = "No. of MTs")
                             ),
                             accordionItem(id = 4,
                                           title = "Colors",
                                           color = "warning",
                                           textInput("color.kmts",
                                                     "Color for the KMTs",
                                                     value = "red"),
                                           textInput("color.non.kmts",
                                                     "Color for Non_KMTs",
                                                     value = "yellow")
                             )
                           )
                       ),
                ),
                column(width = 7,
                       boxPlus(title = "Graphs", 
                               width = NULL, 
                               style = "height:762px;", 
                               status = "primary", 
                               closable = FALSE,
                               plotOutput(outputId = "length.plot_kmts")
                       )
                ),
                column(width = 3,
                       valueBoxOutput("avg.length.kmts",
                                      width = NULL),
                       valueBoxOutput("avg.length.non.kmts",
                                      width = NULL)  
                )
              )
      ),
      tabItem(tabName = "menu_export")
    )
  )
)

server <- function(input, output) {
  
  output$data.node <- renderTable({
    req(input$amirafile)
    
    tryCatch({
      Nodes <- read_excel(input$amirafile$datapath, 
                          sheet = "Nodes")
    }) 
    
    if (input$disp == "head") {
      return(head(Nodes %>% select("Node ID", 
                                   "X Coord", 
                                   "Y Coord", 
                                   "Z Coord")
                  )
             )
    }
    else{
      return(Nodes)
    }
  })
  
  output$data.point <- renderTable({
    req(input$amirafile)
    
    tryCatch({
      Points <- read_excel(input$amirafile$datapath, 
                           sheet = "Points")
    })
    
    if (input$disp == "head") {
      return(head(Points %>% select("Point ID", 
                                    "X Coord", 
                                    "Y Coord", 
                                    "Z Coord")
                  )
             )
    }
    else{
      return(Points)
    }
  })
  
  output$data.segment <- renderTable({
    req(input$amirafile)
    
    tryCatch({
      Segments <- read_excel(input$amirafile$datapath, 
                             sheet = "Segments")
    })
    
    if (input$disp == "head") {
      return(head(Segments %>% select("Segment ID", 
                                      "length", 
                                      "Node ID #1", 
                                      "Node ID #2")))
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
    req(input$color.kmts)
    req(input$color.non.kmts)
    tryCatch({
      Segments <- read_excel(input$amirafile$datapath, 
                             sheet = "Segments")
    })
    
    kmts <- Segments %>% filter_at(vars(starts_with("Pole")), 
                                   any_vars(.>= 1))
    non_kmts <- setdiff(Segments, kmts)
    xkmts <- kmts$length/10000
    xnon_kmts <- non_kmts$length/10000
    ## Histogram
    if(input$analysis == 1){
      bins <- seq(min(input$bin.min), 
                  max(input$bin.max), 
                  length.out = (input$bin.max/input$bin.step) + 1)
      if (input$display.on.plot == 1){
      hist(xnon_kmts, 
           breaks = bins, 
           col = input$color.non.kmts, 
           xlab = "Length (um)", 
           ylab = "No. of MTs")
      hist(xkmts, 
           breaks = bins, 
           col = input$color.kmts,
           add = T)
      }
      if (input$display.on.plot == 2){
        hist(xkmts,
             breaks = bins,
             col = input$color.kmts,
             xlab = "Length (um)", 
             ylab = "No. of MTs")
      }
      if (input$display.on.plot == 3){
        hist(xnon_kmts,
             breaks = bins,
             col = input$color.non.kmts,
             xlab = "Length (um)", 
             ylab = "No. of MTs")
      }
    }
    if(input$analysis == 2){
      ## density
    }
    if (input$analysis == 3){
      ##logaritmic
    }
  })
  
  output$avg.length.kmts <- renderValueBox({
    req(input$amirafile)
    req(input$color.kmts)
    
    tryCatch({
      Segments <- read_excel(input$amirafile$datapath, 
                             sheet = "Segments")
    })
    
    kmts <- Segments %>% filter_at(vars(starts_with("Pole")), 
                                   any_vars(.>= 1))
    length.kmts <- round(mean(kmts$length/10000), 
                         2)
    sd.kmts <- round(sd(kmts$length/10000),
                     2)
    
    valueBox(
      paste(length.kmts, "±", sd.kmts), 
      "Avg. KMTs length", 
      icon = icon("calculator"), 
      color = input$color.kmts)
  })
  
  output$avg.length.non.kmts <- renderValueBox({
    req(input$amirafile)
    req(input$color.non.kmts)
    
    tryCatch({
      Segments <- read_excel(input$amirafile$datapath, 
                             sheet = "Segments")
    })
    
    kmts <- Segments %>% filter_at(vars(starts_with("Pole")), 
                                   any_vars(.>= 1))
    non_kmts <- setdiff(Segments, kmts)
    length.non_kmts <- round(mean(non_kmts$length/10000), 
                             2)
    sd.non.kmts <- round(sd(non_kmts$length/10000),
                         2)
    
    valueBox(
      paste(length.non_kmts, "±", sd.non.kmts), 
      "Avg. Non-KMTs length", 
      icon = icon("calculator"), 
      color = input$color.non.kmts)
  })
}

shinyApp(ui = ui, server = server)
