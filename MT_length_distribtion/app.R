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
                column(width = 3,
                boxPlus(title = "Upload file #1", 
                        width = NULL,
                        status = "warning", 
                        closable = FALSE,
                        p(fileInput("amirafile1", "Choose .xlsx File", multiple = FALSE,
                                    accept = c("text/xlsx", "text/comma-separated-values,text/plain", ".xlsx")
                        )
                        )
                ),
                boxPlus(title = "Upload file #2", 
                        width = NULL,
                        status = "warning", 
                        closable = FALSE,
                        p(fileInput("amirafile2", "Choose .xlsx File", multiple = FALSE,
                                    accept = c("text/xlsx", "text/comma-separated-values,text/plain", ".xlsx")
                        )
                        )
                ),
                boxPlus(title = "Upload file #3", 
                        width = NULL,
                        status = "warning", 
                        closable = FALSE,
                        p(fileInput("amirafile3", "Choose .xlsx File", multiple = FALSE,
                                    accept = c("text/xlsx", "text/comma-separated-values,text/plain", ".xlsx")
                        )
                        )
                ),
                boxPlus(title = "Upload file #4", 
                        width = NULL,
                        status = "warning", 
                        closable = FALSE,
                        p(fileInput("amirafile4", "Choose .xlsx File", multiple = FALSE,
                                    accept = c("text/xlsx", "text/comma-separated-values,text/plain", ".xlsx")
                        )
                        )
                )
                ),
                column(width = 9,
                boxPlus(title = "Lodate data:", 
                        width = NULL,
                        status = "primary", 
                        closable = FALSE,
                        p(radioButtons("disp",
                                       "Display", 
                                       choices = c(Head = "head", 
                                                   All = "all"), 
                                       selected = "head"),
                          tabsetPanel(
                            tabPanel("Data_1_Segments", 
                                     tableOutput("data1.segment")),
                            tabPanel("Data_2_Segments",
                                     tableOutput("data2.segment")),
                            tabPanel("Data_3_Segments",
                                     tableOutput("data3.segment")),
                            tabPanel("Data_4_Segments",
                                     tableOutput("data4.segment"))
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
      tabItem(tabName = "menu_export",
              fluidPage(
                column(width = 2,
                       boxPlus(title = "Exprt analysed data",
                               width = NULL,
                               style = "height:350px;",
                               status = "info",
                               closable = FALSE,
                               downloadButton("downloadData", 
                                              "Download")
                       )
                       
                       
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  
  output$data1.segment <- renderTable({
    req(input$amirafile1)
    tryCatch({
      Segments <- read_excel(input$amirafile1$datapath, sheet = "Segments")
    })
    if (input$disp == "head") {
      return(head(Segments %>% select("Segment ID", "length")))
    }
    else{
      return(Segments)
    }
  })
  
  output$data2.segment <- renderTable({
    req(input$amirafile2)
    tryCatch({
      Segments <- read_excel(input$amirafile2$datapath, sheet = "Segments")
    })
    if (input$disp == "head") {
      return(head(Segments %>% select("Segment ID", "length")))
    }
    else{
      return(Segments)
    }
  })
  
  output$data3.segment <- renderTable({
    req(input$amirafile3)
    tryCatch({
      Segments <- read_excel(input$amirafile3$datapath, sheet = "Segments")
    })
    if (input$disp == "head") {
      return(head(Segments %>% select("Segment ID", "length")))
    }
    else{
      return(Segments)
    }
  })
  
  output$data4.segment <- renderTable({
    req(input$amirafile4)
    tryCatch({
      Segments <- read_excel(input$amirafile4$datapath, sheet = "Segments")
    })
    if (input$disp == "head") {
      return(head(Segments %>% select("Segment ID","length")))
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
    xkmts <- data.frame(MTs = kmts$length/10000)
    xnon_kmts <- data.frame(MTs = non_kmts$length/10000)
    ## Histogram
    if(input$analysis == 1){
      bins <- (input$bin.max/input$bin.step) + 1
      
      if (input$display.on.plot == 1){
        p <-  ggplot() + geom_histogram(data = xnon_kmts, 
                                        aes(x = MTs), 
                                        bins = bins, 
                                        position = "identity", 
                                        alpha = 1, 
                                        fill = input$color.non.kmts) +
          geom_histogram(data = xkmts, 
                         aes(x = MTs), 
                         bins = bins, 
                         position = "identity", 
                         alpha = 0.8,
                         fill = input$color.kmts)
        print(p)
      }
      
      if (input$display.on.plot == 2){
        p <- ggplot() + geom_histogram(data = xkmts, 
                                       aes(x = MTs), 
                                       bins = bins,
                                       alpha = 1, 
                                       fill = "red")
        print(p)
      }
      
      if (input$display.on.plot == 3){
        p <- ggplot() + geom_histogram(data = xnon_kmts, 
                                       aes(x = MTs), 
                                       bins = bins,
                                       alpha = 1,
                                       fill = "yellow")
        print(p)
      }
    }
    if(input$analysis == 2){
      ## density
      bins <- (input$bin.max/input$bin.step) + 1
      
      if (input$display.on.plot == 1){
        p <-  ggplot() + geom_density(data = xnon_kmts, 
                                      aes(x = MTs), 
                                      bins = bins, 
                                      position = "identity", 
                                      alpha = 1, 
                                      fill = input$color.non.kmts) +
          geom_density(data = xkmts, 
                       aes(x = MTs), 
                       bins = bins, 
                       position = "identity", 
                       alpha = 0.8,
                       fill = input$color.kmts) +
          labs(x = input$x.label, y = input$y.label)
        print(p)
      }
      
      if (input$display.on.plot == 2){
        p <- ggplot() + geom_density(data = xkmts, 
                                     aes(x = MTs), 
                                     bins = bins,
                                     alpha = 1, 
                                     fill = "red") +
          labs(x = input$x.label, y = input$y.label)
        print(p)
      }
      
      if (input$display.on.plot == 3){
        p <- ggplot() + geom_density(data = xnon_kmts, 
                                     aes(x = MTs), 
                                     bins = bins,
                                     alpha = 1,
                                     fill = "yellow") +
          labs(x = input$x.label, y = input$y.label)
        print(p)
      }
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
