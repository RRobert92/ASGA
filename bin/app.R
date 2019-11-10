library(shinydashboardPlus)
library(shinydashboard)
library(shiny)
library(readxl)
library(tidyverse)
library(plyr)

##Maximum size of memory used by R, set to 1024mb
options(shiny.maxRequestSize = 1024*1024^2)

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
              includeHTML("html/about.html")
      ),
      tabItem(tabName = "menu_instruction",
              includeHTML("html/manual.html")
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
                                   tabPanel("Data_Segments",
                                            fluidRow(
                                              column(width = 2, "Data set #1",
                                                     tableOutput("data1.segment")
                                              ),
                                              column(width = 2, "Data set #2",
                                                     tableOutput("data2.segment")
                                              ),
                                              column(width = 2, "Data set #3",
                                                     tableOutput("data3.segment")
                                              ),
                                              column(width = 2, "Data set #4",
                                                     tableOutput("data4.segment")
                                              )
                                            )
                                   )
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
                                       selected = 1)
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
                       valueBoxOutput("avg.length.non.kmts",
                                      width = NULL)
                )
              )
      ),
      tabItem(tabName = "menu_export",
              fluidPage(
                column(width = 2,
                       boxPlus(title = "Exprt data",
                               width = NULL,
                               style = "height:762pxpx;",
                               status = "info",
                               closable = FALSE,
                               selectInput("data.download", "Choose a analysis:",
                                           choices = c("Histogram", "% of MTs")),
                               downloadButton("downloadData1", 
                                              "Download")
                       )
                ),
                column(width = 10,
                       boxPlus(title = "Exported table",
                               width = NULL,
                               style = "height:400px;",
                               status = "info",
                               closable = FALSE,
                               fluidRow(
                                 tableOutput("excel.data")
                               )
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
    
    ##Function for computing
    FilterForPole <- function(data.set){
      tryCatch({
        data.set %>% filter_at(vars(starts_with("Pole")),
                               any_vars(.>= 1))
      },
      error = function(e) {
        data.set %>% filter_at(vars(starts_with("KMT")),
                               any_vars(.>= 1))
      })
    }
    
    FindNonKMTs <- function(data.set, set1){
      setdiff(data.set, set1)
    }
    
    CreatHist <- function(data.set) {
      hist(data.set, 
           xlim = c(input$bin.min, input$bin.max),
           breaks = bins)
    }
    ##Bins set up by user
    bins = c(input$bin.min, 
             seq(input$bin.min + input$bin.step, input$bin.max, input$bin.step))
    
    ##Dataset_1
    kmts_1 <<- FilterForPole(Segments_1)
    non_kmts_1 <<- FindNonKMTs(Segments_1, kmts_1)
    xkmts_1 <<- data.frame(KMTs_1 = kmts_1$length/10000)  ## Lengh in (um) for KMTs_1
    xnon_kmts_1 <<- data.frame(Non_KMTs_1 = non_kmts_1$length/10000)## Lengh in (um) for non_KMTs_1
    ##Creat data.frame of histogram data for global use with the name setb by "id"
    Hist_Segment_KMTs_1 <<- CreatHist(xkmts_1$KMTs_1)
    Hist_Segment_KMTs_1 <<- data.frame(Bins = c(Hist_Segment_KMTs_1$breaks), 
                                       KMTs_1 = c(0,Hist_Segment_KMTs_1$counts))
    Hist_Segment_Non_KMTs_1 <<- CreatHist(xnon_kmts_1$Non_KMTs_1)
    Hist_Segment_Non_KMTs_1 <<- data.frame(Bins = c(Hist_Segment_Non_KMTs_1$breaks), 
                                           Non_KMTs_1 = c(0,Hist_Segment_Non_KMTs_1$counts))
    
    if(exists("Segments_2")){
      kmts_2 <<- FilterForPole(Segments_2)
      non_kmts_2 <<- FindNonKMTs(Segments_2, kmts_2)
      xkmts_2 <<- data.frame(KMTs_2 = kmts_2$length/10000)  ## Lengh in (um) for KMTs_1
      xnon_kmts_2 <<- data.frame(Non_KMTs_2 = non_kmts_2$length/10000)## Lengh in (um) for non_KMTs_1
      ##Creat data.frame of histogram data for global use with the name setb by "id"
      Hist_Segment_KMTs_2 <<- CreatHist(xkmts_2$KMTs_2)
      Hist_Segment_KMTs_2 <<- data.frame(Bins = c(Hist_Segment_KMTs_2$breaks), 
                                         KMTs_2 = c(0,Hist_Segment_KMTs_2$counts))
      Hist_Segment_Non_KMTs_2 <<- CreatHist(xnon_kmts_2$Non_KMTs_2)
      Hist_Segment_Non_KMTs_2 <<- data.frame(Bins = c(Hist_Segment_Non_KMTs_2$breaks), 
                                             Non_KMTs_2 = c(0,Hist_Segment_Non_KMTs_2$counts))
    }
    if(exists("Segments_3")){
      kmts_3 <<- FilterForPole(Segments_3)
      non_kmts_3 <<- FindNonKMTs(Segments_3, kmts_3)
      xkmts_3 <<- data.frame(KMTs_3 = kmts_3$length/10000)  ## Lengh in (um) for KMTs_1
      xnon_kmts_3 <<- data.frame(Non_KMTs_3 = non_kmts_3$length/10000)## Lengh in (um) for non_KMTs_1
      ##Creat data.frame of histogram data for global use with the name setb by "id"
      Hist_Segment_KMTs_3 <<- CreatHist(xkmts_3$KMTs_3)
      Hist_Segment_KMTs_3 <<- data.frame(Bins = c(Hist_Segment_KMTs_3$breaks), 
                                         KMTs_3 = c(0,Hist_Segment_KMTs_3$counts))
      Hist_Segment_Non_KMTs_3 <<- CreatHist(xnon_kmts_3$Non_KMTs_3)
      Hist_Segment_Non_KMTs_3 <<- data.frame(Bins = c(Hist_Segment_Non_KMTs_3$breaks), 
                                             Non_KMTs_3 = c(0,Hist_Segment_Non_KMTs_3$counts))
    }
    if(exists("Segments_4")){
      kmts_4 <<- FilterForPole(Segments_4)
      non_kmts_4 <<- FindNonKMTs(Segments_4, kmts_4)
      xkmts_4 <<- data.frame(KMTs_4 = kmts_4$length/10000)  ## Lengh in (um) for KMTs_1
      xnon_kmts_4 <<- data.frame(Non_KMTs_4 = non_kmts_4$length/10000)## Lengh in (um) for non_KMTs_1
      ##Creat data.frame of histogram data for global use with the name setb by "id"
      Hist_Segment_KMTs_4 <<- CreatHist(xkmts_4$KMTs_4)
      Hist_Segment_KMTs_4 <<- data.frame(Bins = c(Hist_Segment_KMTs_4$breaks), 
                                         KMTs_4 = c(0,Hist_Segment_KMTs_4$counts))
      Hist_Segment_Non_KMTs_4 <<- CreatHist(xnon_kmts_4$Non_KMTs_4)
      Hist_Segment_Non_KMTs_4 <<- data.frame(Bins = c(Hist_Segment_Non_KMTs_4$breaks), 
                                             Non_KMTs_4 = c(0,Hist_Segment_Non_KMTs_4$counts))
    }
    
    ##Marge dataset for KMTs in one tabel for plot and export
    if (exists("Hist_Segment_KMTs_2") && !exists("Hist_Segment_KMTs_3")){
      avg_kmts = c((Hist_Segment_KMTs_1$KMTs_1 + Hist_Segment_KMTs_2$KMTs_2)/2)
      avg_kmts <- data.frame(Bins = c(Hist_Segment_KMTs_1$Bins),
                             avg_kmts)
      avg_non_kmts = c((Hist_Segment_Non_KMTs_1$Non_KMTs_1 + Hist_Segment_Non_KMTs_2$Non_KMTs_2)/2)
      avg_non_kmts <- data.frame(Bins = c(Hist_Segment_Non_KMTs_1$Bins),
                                 avg_non_kmts)
      full_data_hist <<- join_all(list(Hist_Segment_KMTs_1,
                                       Hist_Segment_Non_KMTs_1,
                                       Hist_Segment_KMTs_2,
                                       Hist_Segment_Non_KMTs_2,
                                       avg_kmts,
                                       avg_non_kmts), 
                                  by = "Bins", type = "full")
    } else if (exists("Hist_Segment_KMTs_3") && !exists("Hist_Segment_KMTs_4")){
      avg_kmts = c((Hist_Segment_KMTs_1$KMTs_1 + Hist_Segment_KMTs_2$KMTs_2 + Hist_Segment_KMTs_3$KMTs_3)/3)
      avg_kmts <- data.frame(Bins = c(Hist_Segment_KMTs_1$Bins),
                             avg_kmts)
      avg_non_kmts = c((Hist_Segment_Non_KMTs_1$Non_KMTs_1 + Hist_Segment_Non_KMTs_2$Non_KMTs_2 + Hist_Segment_Non_KMTs_3$Non_KMTs_3)/3)
      avg_non_kmts <- data.frame(Bins = c(Hist_Segment_Non_KMTs_1$Bins),
                                 avg_non_kmts)
      full_data_hist <<- join_all(list(Hist_Segment_KMTs_1,
                                       Hist_Segment_Non_KMTs_1,
                                       Hist_Segment_KMTs_2,
                                       Hist_Segment_Non_KMTs_2,
                                       Hist_Segment_KMTs_3,
                                       Hist_Segment_Non_KMTs_3,
                                       avg_kmts,
                                       avg_non_kmts), by = "Bins", type = "full")
    }else if (exists("Hist_Segment_KMTs_4")){
      avg_kmts = c((Hist_Segment_KMTs_1$KMTs_1 + Hist_Segment_KMTs_2$KMTs_2 + Hist_Segment_KMTs_3$KMTs_3 + Hist_Segment_KMTs_4)/4)
      avg_kmts <- data.frame(Bins = c(Hist_Segment_KMTs_1$Bins),
                             avg_kmts)
      avg_non_kmts = c((Hist_Segment_Non_KMTs_1$Non_KMTs_1 + Hist_Segment_Non_KMTs_2$Non_KMTs_2 + Hist_Segment_Non_KMTs_3$Non_KMTs_3 + Hist_Segment_Non_KMTs_4)/4)
      avg_non_kmts <- data.frame(Bins = c(Hist_Segment_Non_KMTs_1$Bins),
                                 avg_non_kmts)
      full_data_hist <<- join_all(list(Hist_Segment_KMTs_1,
                                       Hist_Segment_Non_KMTs_1,
                                       Hist_Segment_KMTs_2,
                                       Hist_Segment_Non_KMTs_2,
                                       Hist_Segment_KMTs_3,
                                       Hist_Segment_Non_KMTs_3,
                                       Hist_Segment_KMTs_4,
                                       Hist_Segment_Non_KMTs_4,
                                       avg_kmts,
                                       avg_non_kmts), by = "Bins", type = "full")
    } else {
      full_data_hist <<- merge(Hist_Segment_KMTs_1,
                               Hist_Segment_Non_KMTs_1)
    }
    
    ##calculateig % of MTs
    if(exists("Segments_2") && !exists("Segments_3")){
      sum_kmts_1 <- sum(full_data_hist$KMTs_1)
      sum_kmts_2 <- sum(full_data_hist$KMTs_2)
      sum_non_kmts_1 <- sum(full_data_hist$Non_KMTs_1)
      sum_non_kmts_2 <- sum(full_data_hist$Non_KMTs_2)
      
      full_data_perc <<- data.frame(Bins = c(Hist_Segment_KMTs_1$Bins), 
                                    KMTs_1 = c(round((full_data_hist$KMTs_1*100)/sum_kmts_1, 2)),
                                    KMTs_2 = c(round((full_data_hist$KMTs_2*100)/sum_kmts_2, 2)),
                                    Non_KMTs_1 = c(round((full_data_hist$Non_KMTs_1*100)/sum_non_kmts_1, 2)),
                                    Non_KMTs_2 = c(round((full_data_hist$Non_KMTs_2*100)/sum_non_kmts_2, 2))
      )
      
      avg_kmts = c((full_data_perc$KMTs_1 + full_data_perc$KMTs_2)/2)
      avg_non_kmts = c((full_data_perc$Non_KMTs_1 + full_data_perc$Non_KMTs_2)/2)
      
      full_data_perc <<- data.frame(Bins = c(Hist_Segment_KMTs_1$Bins), 
                                    KMTs_1 = c(round((full_data_hist$KMTs_1*100)/sum_kmts_1, 2)),
                                    KMTs_2 = c(round((full_data_hist$KMTs_2*100)/sum_kmts_2, 2)),
                                    Non_KMTs_1 = c(round((full_data_hist$Non_KMTs_1*100)/sum_non_kmts_1, 2)),
                                    Non_KMTs_2 = c(round((full_data_hist$Non_KMTs_2*100)/sum_non_kmts_2, 2)),
                                    avg_kmts = avg_kmts,
                                    avg_non_kmts = avg_non_kmts
      )
    } else if(exists("Segments_3") && !exists("Segments_4")){
      sum_kmts_1 <- sum(full_data_hist$KMTs_1)
      sum_kmts_2 <- sum(full_data_hist$KMTs_2)
      sum_kmts_3 <- sum(full_data_hist$KMTs_3)
      sum_non_kmts_1 <- sum(full_data_hist$Non_KMTs_1)
      sum_non_kmts_2 <- sum(full_data_hist$Non_KMTs_2)
      sum_non_kmts_3 <- sum(full_data_hist$Non_KMTs_3)
      
      full_data_perc <<- data.frame(Bins = c(Hist_Segment_KMTs_1$Bins), 
                                    KMTs_1 = c(round((full_data_hist$KMTs_1*100)/sum_kmts_1, 2)),
                                    KMTs_2 = c(round((full_data_hist$KMTs_2*100)/sum_kmts_2, 2)),
                                    KMTs_3 = c(round((full_data_hist$KMTs_3*100)/sum_kmts_3, 2)),
                                    Non_KMTs_1 = c(round((full_data_hist$Non_KMTs_1*100)/sum_non_kmts_1, 2)),
                                    Non_KMTs_2 = c(round((full_data_hist$Non_KMTs_2*100)/sum_non_kmts_2, 2)),
                                    Non_KMTs_3 = c(round((full_data_hist$Non_KMTs_3*100)/sum_non_kmts_3, 2))
      )
      
      avg_kmts = c((full_data_perc$KMTs_1 + full_data_perc$KMTs_2 + full_data_perc$KMTs_3)/3)
      avg_non_kmts = c((full_data_perc$Non_KMTs_1 + full_data_perc$Non_KMTs_2 + full_data_perc$Non_KMTs_3)/3)
      
      full_data_perc <<- data.frame(Bins = c(Hist_Segment_KMTs_1$Bins), 
                                    KMTs_1 = c(round((full_data_hist$KMTs_1*100)/sum_kmts_1, 2)),
                                    KMTs_2 = c(round((full_data_hist$KMTs_2*100)/sum_kmts_2, 2)),
                                    KMTs_3 = c(round((full_data_hist$KMTs_3*100)/sum_kmts_3, 2)),
                                    Non_KMTs_1 = c(round((full_data_hist$Non_KMTs_1*100)/sum_non_kmts_1, 2)),
                                    Non_KMTs_2 = c(round((full_data_hist$Non_KMTs_2*100)/sum_non_kmts_2, 2)),
                                    Non_KMTs_3 = c(round((full_data_hist$Non_KMTs_3*100)/sum_non_kmts_3, 2)),
                                    avg_kmts = avg_kmts,
                                    avg_non_kmts = avg_non_kmts
      )
    } else if(exists("Segments_4")){
      sum_kmts_1 <- sum(full_data_hist$KMTs_1)
      sum_kmts_2 <- sum(full_data_hist$KMTs_2)
      sum_kmts_3 <- sum(full_data_hist$KMTs_3)
      sum_kmts_4 <- sum(full_data_hist$KMTs_3)
      sum_non_kmts_1 <- sum(full_data_hist$Non_KMTs_1)
      sum_non_kmts_2 <- sum(full_data_hist$Non_KMTs_2)
      sum_non_kmts_3 <- sum(full_data_hist$Non_KMTs_3)
      sum_non_kmts_4 <- sum(full_data_hist$Non_KMTs_4)
      
      full_data_perc <<- data.frame(Bins = c(Hist_Segment_KMTs_1$Bins), 
                                    KMTs_1 = c(round((full_data_hist$KMTs_1*100)/sum_kmts_1, 2)),
                                    KMTs_2 = c(round((full_data_hist$KMTs_2*100)/sum_kmts_2, 2)),
                                    KMTs_3 = c(round((full_data_hist$KMTs_3*100)/sum_kmts_3, 2)),
                                    KMTs_4 = c(round((full_data_hist$KMTs_4*100)/sum_kmts_4, 2)),
                                    Non_KMTs_1 = c(round((full_data_hist$Non_KMTs_1*100)/sum_non_kmts_1, 2)),
                                    Non_KMTs_2 = c(round((full_data_hist$Non_KMTs_2*100)/sum_non_kmts_2, 2)),
                                    Non_KMTs_3 = c(round((full_data_hist$Non_KMTs_3*100)/sum_non_kmts_3, 2)),
                                    Non_KMTs_4 = c(round((full_data_hist$Non_KMTs_4*100)/sum_non_kmts_4, 2))
      )
      
      avg_kmts = c((full_data_perc$KMTs_1 + full_data_perc$KMTs_2 + full_data_perc$KMTs_3 + full_data_perc$KMTs_4)/4)
      avg_non_kmts = c((full_data_perc$Non_KMTs_1 + full_data_perc$Non_KMTs_2 + full_data_perc$Non_KMTs_3 + full_data_perc$Non_KMTs_4)/4)
      
      full_data_perc <<- data.frame(Bins = c(Hist_Segment_KMTs_1$Bins), 
                                    KMTs_1 = c(round((full_data_hist$KMTs_1*100)/sum_kmts_1, 2)),
                                    KMTs_2 = c(round((full_data_hist$KMTs_2*100)/sum_kmts_2, 2)),
                                    KMTs_3 = c(round((full_data_hist$KMTs_3*100)/sum_kmts_3, 2)),
                                    KMTs_4 = c(round((full_data_hist$KMTs_4*100)/sum_kmts_4, 2)),
                                    Non_KMTs_1 = c(round((full_data_hist$Non_KMTs_1*100)/sum_non_kmts_1, 2)),
                                    Non_KMTs_2 = c(round((full_data_hist$Non_KMTs_2*100)/sum_non_kmts_2, 2)),
                                    Non_KMTs_3 = c(round((full_data_hist$Non_KMTs_3*100)/sum_non_kmts_3, 2)),
                                    Non_KMTs_4 = c(round((full_data_hist$Non_KMTs_4*100)/sum_non_kmts_4, 2)),
                                    avg_kmts = avg_kmts,
                                    avg_non_kmts = avg_non_kmts
      )
    } else {
      sum_kmts_1 <- sum(full_data_hist$KMTs_1)
      sum_non_kmts_1 <- sum(full_data_hist$Non_KMTs_1)
      full_data_perc <<- data.frame(Bins = c(Hist_Segment_KMTs_1$Bins),
                                    KMTs_1 = c(round((full_data_hist$KMTs_1*100)/sum_kmts_1, 2)),
                                    Non_KMTs_1 = c(round((full_data_hist$Non_KMTs_1*100)/sum_non_kmts_1, 2)))
    }
    
    
    ## Histogram
    if(input$analysis == 1){
      if (input$display.on.plot == 1){
        if(exists("Hist_Segment_KMTs_2") || exists("Hist_Segment_KMTs_3") || exists("Hist_Segment_KMTs_4")){
          plot(full_data_hist$Bins,
               full_data_hist$avg_non_kmts,
               type = "l", 
               col = "yellow", 
               xlab = "Length (um)", 
               ylab = "No. of KMTs",
               main = "Avg. MT Length distribution",
               lwd = 3)
          lines(full_data_hist$Bins,
                full_data_hist$avg_kmts,
                lwd = 3,
                col = "red")
        } else {
          plot(full_data_hist$Bins,
               full_data_hist$Non_KMTs_1,
               type = "l", 
               col = "yellow", 
               xlab = "Length (um)", 
               ylab = "No. of KMTs",
               main = "MT Length distribution",
               lwd = 3)
          lines(full_data_hist$Bins,
                full_data_hist$KMTs_1,
                col = "red",
                lwd = 3)
        }
      }
      
      ## if only KMTs -> plot
      if (input$display.on.plot == 2){
        if(exists("Hist_Segment_KMTs_2")){
          plot(full_data_hist$Bins,
               full_data_hist$avg_kmts,
               type = "l", 
               col = "red", 
               xlab = "Length (um)", 
               ylab = "No. of KMTs",
               ylim=c(0,60),
               main = "KMTs Length distribution",
               lwd = 3)
          lines(full_data_hist$Bins,
                full_data_hist$KMTs_1,
                col = "gray50",
                lwd = 1,
                lty = 2)
          lines(full_data_hist$Bins,
                full_data_hist$KMTs_2,
                col = "gray28",
                lwd = 1,
                lty = 3)
          if(exists("Hist_Segment_KMTs_3")){
            lines(full_data_hist$Bins,
                  full_data_hist$KMTs_3,
                  col = "gray15",
                  lwd = 1,
                  lty = 4)
          } else if(exists("Hist_Segment_KMTs_4")){
            lines(full_data_hist$Bins,
                  full_data_hist$KMTs_4,
                  col = "gray10",
                  lwd = 1,
                  lty = 5)
          }
        } else{
          plot(full_data_hist$Bins,
               full_data_hist$KMTs_1,
               type = "l", 
               col = "red", 
               xlab = "Length (um)", 
               ylab = "No. of KMTs",
               main = "KMTs Length distribution",
               lwd = 3)
        }
      }
      
      ## if only Non-KMTs -> plot
      if (input$display.on.plot == 3){
        if(exists("Hist_Segment_KMTs_2")){
          plot(full_data_hist$Bins,
               full_data_hist$avg_non_kmts,
               type = "l", 
               col = "yellow", 
               xlab = "Length (um)", 
               ylab = "No. of KMTs",
               ylim = c(0,800),
               main = "KMTs Length distribution",
               lwd = 3)
          lines(full_data_hist$Bins,
                full_data_hist$Non_KMTs_1,
                col = "gray50",
                lwd = 1,
                lty = 2)
          lines(full_data_hist$Bins,
                full_data_hist$Non_KMTs_2,
                col = "gray28",
                lwd = 1,
                lty = 3)
          if(exists("Hist_Segment_KMTs_3")){
            lines(full_data_hist$Bins,
                  full_data_hist$Non_KMTs_3,
                  col = "gray15",
                  lwd = 1,
                  lty = 4)
          }
          else if(exists("Hist_Segment_KMTs_4")){
            lines(full_data_hist$Bins,
                  full_data_hist$Non_KMTs_4,
                  col = "gray10",
                  lwd = 1,
                  lty = 5)
          }
        } else{
          plot(full_data_hist$Bins,
               full_data_hist$Non_KMTs_1,
               type = "l", 
               col = "yellow", 
               xlab = "Length (um)", 
               ylab = "No. of KMTs",
               main = "KMTs Length distribution",
               lwd = 3)
        }
      }
    }
    
    ## % of MTs
    if(input$analysis == 2){
      if (input$display.on.plot == 1){
        
        ##if more then one data exist, show only avg value, otherwish show only one data
        if(exists("Hist_Segment_KMTs_2") || exists("Hist_Segment_KMTs_3") || exists("Hist_Segment_KMTs_4")){
          plot(full_data_perc$Bins,
               full_data_perc$avg_non_kmts,
               type = "l", 
               col = "yellow", 
               xlab = "Length (um)", 
               ylab = "% of MTs",
               main = "Avg. MT Length distribution",
               lwd = 3)
          lines(full_data_perc$Bins,
                full_data_perc$avg_kmts,
                lwd = 3,
                col = "red")
        } else {
          plot(full_data_perc$Bins,
               full_data_perc$Non_KMTs_1,
               type = "l", 
               col = "yellow", 
               xlab = "Length (um)", 
               ylab = "% of MTs",
               main = "MT Length distribution",
               lwd = 3)
          lines(full_data_perc$Bins,
                full_data_perc$KMTs_1,
                col = "red",
                lwd = 3)
        }
      }
      
      ## if only KMTs to plot
      if (input$display.on.plot == 2){
        if(exists("Hist_Segment_KMTs_2")){
          plot(full_data_perc$Bins,
               full_data_perc$avg_kmts,
               type = "l", 
               col = "red", 
               xlab = "Length (um)", 
               ylab = "% of MTs",
               ylim = c(0,15),
               main = "KMTs Length distribution",
               lwd = 3)
          lines(full_data_perc$Bins,
                full_data_perc$KMTs_1,
                col = "gray50",
                lwd = 1,
                lty = 2)
          lines(full_data_perc$Bins,
                full_data_perc$KMTs_2,
                col = "gray28",
                lwd = 1,
                lty = 3)
          if(exists("Hist_Segment_KMTs_3")){
            lines(full_data_perc$Bins,
                  full_data_perc$KMTs_3,
                  col = "gray15",
                  lwd = 1,
                  lty = 4)
          }
          if(exists("Hist_Segment_KMTs_4")){
            lines(full_data_perc$Bins,
                  full_data_perc$KMTs_4,
                  col = "gray10",
                  lwd = 1,
                  lty = 5)
          }
        } else{
          plot(full_data_perc$Bins,
               full_data_perc$KMTs_1,
               type = "l", 
               col = "red", 
               xlab = "Length (um)", 
               ylab = "% of MTs",
               main = "KMTs Length distribution",
               lwd = 3)
        }
      }
      
      ## if only Non-KMTs -> plot
      if (input$display.on.plot == 3){
        if(exists("Hist_Segment_KMTs_2")){
          plot(full_data_perc$Bins,
               full_data_perc$avg_non_kmts,
               type = "l", 
               col = "yellow", 
               xlab = "Length (um)", 
               ylab = "% of MTs",
               ylim=c(0,30),
               main = "KMTs Length distribution",
               lwd = 3)
          lines(full_data_perc$Bins,
                full_data_perc$Non_KMTs_1,
                col = "gray50",
                lwd = 1,
                lty = 2)
          lines(full_data_perc$Bins,
                full_data_perc$Non_KMTs_2,
                col = "gray28",
                lwd = 1,
                lty = 3)
          if(exists("Hist_Segment_KMTs_3")){
            lines(full_data_perc$Bins,
                  full_data_perc$Non_KMTs_3,
                  col = "gray15",
                  lwd = 1,
                  lty = 4)
          } else if(exists("Hist_Segment_KMTs_4")){
            lines(full_data_perc$Bins,
                  full_data_perc$Non_KMTs_4,
                  col = "gray10",
                  lwd = 1,
                  lty = 5)
          }
        } else {
          plot(full_data_perc$Bins,
               full_data_perc$Non_KMTs_1,
               type = "l", 
               col = "yellow", 
               xlab = "Length (um)", 
               ylab = "% of MTs",
               main = "KMTs Length distribution",
               lwd = 3)
        }
      }
    }
  })
  
  output$avg.length.kmts <- renderValueBox({
    req(input$amirafile1)
    
    show_avg <- function(){
      valueBox(
        paste(length.kmts, "±", sd.kmts), 
        "Avg. KMTs length", 
        icon = icon("calculator"), 
        color = "red")
    }
      tryCatch({
        if(exists("Segments_2") && !exists("Segments_3")){
          length.kmts <- round((mean(xkmts_1$KMTs_1) + mean(xkmts_2$KMTs_2))/2, 
                               2)
          sd.kmts <- round((sd(xkmts_1$KMTs_1) + sd(xkmts_2$KMTs_2))/2, 
                           2)
          show_avg()
          
        } else if (exists("Segments_3") && !exists("Segments_4")){
          length.kmts <- round((mean(xkmts_1$KMTs_1) + mean(xkmts_2$KMTs_2) + mean(xkmts_3$KMTs_3))/3, 
                               2)
          sd.kmts <- round((sd(xkmts_1$KMTs_1) + sd(xkmts_2$KMTs_2) + sd(xkmts_3$KMTs_3))/3, 
                           2)
          show_avg()
          
        } else if (exists("Segments_4")){
          length.kmts <- round((mean(xkmts_1$KMTs_1) + mean(xkmts_2$KMTs_2) + mean(xkmts_3$KMTs_3) + mean(xkmts_4$KMTs_4)/4), 
                               2)
          sd.kmts <- round((sd(xkmts_1$KMTs_1) + sd(xkmts_2$KMTs_2) + sd(xkmts_3$KMTs_3) + sd(xkmts_4$KMTs_4))/4, 
                           2)
          show_avg()
          
        }
      },
      error = function(e) {
        length.non.kmts <- round(mean(xkmts_1$KMTs_1), 
                             2)
        sd.kmts <- round(sd(xkmts_1$KMTs_1),
                         2)
        show_avg()
      })
  
  })
  
  output$avg.length.non.kmts <- renderValueBox({
    req(input$amirafile1)
    
    show_avg <- function(){
      valueBox(
        paste(length.non.kmts, "±", sd.non.kmts), 
        "Avg. Non-KMTs length", 
        icon = icon("calculator"), 
        color = "yellow")
    }
    
    tryCatch({
    if(exists("Segments_2") && !exists("Segments_3")){
      length.non.kmts <- round((mean(xnon_kmts_1$Non_KMTs_1) + mean(xnon_kmts_2$Non_KMTs_2))/2, 
                           2)
      sd.non.kmts <- round((sd(xnon_kmts_1$Non_KMTs_1) + sd(xnon_kmts_2$Non_KMTs_2))/2, 
                       2)
      show_avg()
    } else if (exists("Segments_3") && !exists("Segments_4")){
      length.non.kmts <- round((mean(xnon_kmts_1$Non_KMTs_1) + mean(xnon_kmts_2$Non_KMTs_2) + mean(xnon_kmts_3$Non_KMTs_3))/3, 
                           2)
      sd.non.kmts <- round((sd(xnon_kmts_1$Non_KMTs_1) + sd(xnon_kmts_2$Non_KMTs_2) + sd(xnon_kmts_3$Non_KMTs_3))/3, 
                       2)
      show_avg()
    } else if (exists("Segments_4")){
      length.non.kmts <- round((mean(xnon_kmts_1$Non_KMTs_1) + mean(xnon_kmts_2$Non_KMTs_2) + mean(xnon_kmts_3$Non_KMTs_3) + mean(xnon_kmts_4$Non_KMTs_4)/4), 
                           2)
      sd.non.kmts <- round((sd(xnon_kmts_1$Non_KMTs_1) + sd(xnon_kmts_2$Non_KMTs_2) + sd(xnon_kmts_3$Non_KMTs_3) + sd(xnon_kmts_4$Non_KMTs_4))/4, 
                       2)
      show_avg()
    }
      },
    error = function(e) {
      length.non.kmts <- round(mean(xnon_kmts_1$Non_KMTs_1), 
                           2)
      sd.non.kmts <- round(sd(xnon_kmts_1$Non_KMTs_1),
                       2)
      show_avg()
    })
    })
  
  ##show selected full_data talbe 
  data.download.Input <- reactive({
    switch(input$data.download,
           "Histogram" = full_data_hist,
           "% of MTs" = full_data_perc
    )
  })
  output$excel.data <- renderTable({
    req(input$amirafile1)
    data.download.Input()
  })
  
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste(input$dataset, "full_data.csv", sep = "")
    },
    content = function(file) {
      write.csv(data.download.Input(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
