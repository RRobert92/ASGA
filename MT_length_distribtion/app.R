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
                        p(radioButtons("display",
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
                                                      "% of MTs" = 2), 
                                       selected = 1),
                           condition = "input.analysis == 1",
                           numericInput("bin.min", "Bin start from (um):",
                                        value = 0),
                           numericInput("bin.max", "Bin stop at (um):", 
                                       value = 10),
                           numericInput("bin.step", "Bin every (um);", 
                                       value = 0.25)
                       ),
                       
                       box(title = "Other Parameaters",
                           width = NULL,
                           style = "height:380px;",
                           status = "warning",
                           collapsible = FALSE,
                           closable = FALSE,
                               selectInput("display.on.plot", 
                                           "Select what to display",
                                           choices = c("All" = 1, 
                                                       "KMTs" = 2,
                                                       "Non_KMTs" = 3), 
                                           selected = 1),
                               checkboxInput("show.avg", "Show Avg. Value", value = TRUE)
                           )
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
                       valueBoxOutput("avg.length.kmts1",
                                      width = NULL),
                       valueBoxOutput("avg.length.kmts2",
                                      width = NULL),
                       valueBoxOutput("avg.length.kmts3",
                                      width = NULL),
                       valueBoxOutput("avg.length.kmts4",
                                      width = NULL),
                       valueBoxOutput("avg.length.non.kmts",
                                      width = NULL),
                       valueBoxOutput("avg.length.non.kmts1",
                                      width = NULL),
                       valueBoxOutput("avg.length.non.kmts2",
                                      width = NULL),
                       valueBoxOutput("avg.length.non.kmts3",
                                      width = NULL),
                       valueBoxOutput("avg.length.non.kmts4",
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
  
 ## load files upload by user to check if data are loaded correctly and use for global enviroment (<<-)
  output$data1.segment <- renderTable({
    req(input$amirafile1)
    tryCatch({
      Segments_1 <<- read_excel(input$amirafile1$datapath, sheet = "Segments")
    })
    if (input$display == "head") {
      return(head(Segments_1 %>% select("Segment ID", "length")))
    }
    else{
      return(Segments_1)
    }
  })
  
  output$data2.segment <- renderTable({
    req(input$amirafile2)
    tryCatch({
      Segments_2 <<- read_excel(input$amirafile2$datapath, sheet = "Segments")
    })
    if (input$display == "head") {
      return(head(Segments_2 %>% select("Segment ID", "length")))
    }
    else{
      return(Segments_2)
    }
  })
  
  output$data3.segment <- renderTable({
    req(input$amirafile3)
    tryCatch({
      Segments_3 <<- read_excel(input$amirafile3$datapath, sheet = "Segments")
    })
    if (input$display == "head") {
      return(head(Segments_3 %>% select("Segment ID", "length")))
    }
    else{
      return(Segments_3)
    }
  })
  
  output$data4.segment <- renderTable({
    req(input$amirafile4)
    tryCatch({
      Segments_4 <<- read_excel(input$amirafile4$datapath, sheet = "Segments")
    })
    if (input$display == "head") {
      return(head(Segments_4 %>% select("Segment ID","length")))
    }
    else{
      return(Segments_4)
    }
  })

## Creat table used for all analyis, and export -> histogram and % of MTs analysis
  output$length.plot_kmts <- renderPlot({
    req(input$amirafile1)
    req(input$bin.min)
    req(input$bin.max)
    req(input$bin.step)
    
    ##Dataset_1
    kmts_1 <- Segments_1 %>% filter_at(vars(starts_with("Pole")), 
                                   any_vars(.>= 1))
    non_kmts_1 <- setdiff(Segments_1, kmts_1)
    xkmts_1 <- data.frame(KMTs_1 = kmts_1$length/10000) ## Lengh in (um) for KMTs
    xnon_kmts_1 <- data.frame(NonKTs_1 = non_kmts_1$length/10000) ## Lengh in (um) for Non_KMTs
    
    ##Bins set up by user
    bins = c(input$bin.min, 
             seq(input$bin.min + input$bin.step, input$bin.max, input$bin.step))
    
    ##Creat data.frame of histogram data for global use with the name setb by "id"
    Hist_Segment_1 <<- hist(xkmts_1$KMTs_1, 
               xlim = c(input$bin.min, input$bin.max),
               breaks = bins)
    Hist_Segment_1 <<- data.frame(Bins = c(Hist_Segment_1$breaks), KMTs_1 = c(0,Hist_Segment_1$counts))
    
    ##Marge dataset in one tabel for plot and export
    if (exists("Hist_Segment_2")){
      full_data_kmts <<- merge(Hist_Segment_1, Hist_Segment_2)
    } else if (exists("Hist_Segment_3")){
      full_data_kmts <<- merge(Hist_Segment_1, Hist_Segment_2, Hist_Segment_3)
    }else if (exists("Hist_Segment_4")){
      full_data_kmts <<- merge(Hist_Segment_1, Hist_Segment_2, Hist_Segment_3, Hist_Segment_4)
    } else {
      full_data_kmts <<- Hist_Segment_1
    }
    
    ## Histogram
    if(input$analysis == 1){
      if (input$display.on.plot == 1){
        plot(full_data_kmts$Bins, 
             full_data_kmts$KMTs_1, 
             xlim=c(input$bin.min, input$bin.max), 
             type = "o", 
             col = "red", 
             xlab = "Length (um)", ylab = "No. of KMTs",
             main = "KMTs Length distribution")
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
                                      fill = "yellow") +
          geom_density(data = xkmts, 
                       aes(x = MTs), 
                       bins = bins, 
                       position = "identity", 
                       alpha = 0.8,
                       fill = "red") +
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
  })
  
  output$avg.length.kmts <- renderValueBox({
    req(input$amirafile1)
    
    kmts <- Segments_1 %>% filter_at(vars(starts_with("Pole")), 
                                   any_vars(.>= 1))
    length.kmts <- round(mean(kmts$length/10000), 
                         2)
    sd.kmts <- round(sd(kmts$length/10000),
                     2)
    
    valueBox(
      paste(length.kmts, "±", sd.kmts), 
      "Avg. KMTs length", 
      icon = icon("calculator"), 
      color = "red")
  })
  
  output$avg.length.non.kmts <- renderValueBox({
    req(input$amirafile1)
    
    kmts <- Segments_1 %>% filter_at(vars(starts_with("Pole")), 
                                   any_vars(.>= 1))
    non_kmts <- setdiff(Segments_1, kmts)
    length.non_kmts <- round(mean(non_kmts$length/10000), 
                             2)
    sd.non.kmts <- round(sd(non_kmts$length/10000),
                         2)
    
    valueBox(
      paste(length.non_kmts, "±", sd.non.kmts), 
      "Avg. Non-KMTs length", 
      icon = icon("calculator"), 
      color = "yellow")
  })
}

shinyApp(ui = ui, server = server)
