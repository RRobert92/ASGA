################################################################################
# Module Report
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-21
################################################################################

Data_Plot_Settings <- function(input,output, session){

 tags$div(class = "splash-report-code",
          fluidRow(column(6,
                          lapply(1:numfiles, function(i){
                            textInput(inputId = paste("Data label", i, sep = "_"), label = paste("Data", i, sep = "_"), value = paste("label", i, sep = "_"))
                          })),
                   column(6,
                          selectInput(inputId = 'xcol',
                                      label = 'X Variable',
                                      choices = names(iris)))
          )
 )
}

Report_Plot_KMT_No <- function (input, output, session){
  plotOutput("Home-plot_KMT_No")
}

Report_Plot <- function(input, output, session){
  
  output$`plot_KMT_No` <- renderPlot({
    tryCatch({
      
      Plot_Data <<- File_name[File_name$V2 == "KMT_No",]
      
      for(i in 1:nrow(Plot_Data)){
        assign(paste("Data_", Plot_Data[i,"V1"], "_", Plot_Data[i, "V2"], sep = ""),
             data.frame(Data = paste("Metaphase #", i, sep = ""),
                        get(paste("Data_", Plot_Data[i,"V1"], "_", Plot_Data[i, "V2"], sep = ""))["KMTs_per_kinetochore"]),
             envir = .GlobalEnv)
      }
      
      P1 <<- ggplot(get(paste("Data_", Plot_Data[1,"V1"], "_", Plot_Data[1, "V2"], sep = "")), 
                   aes_string("Data", "KMTs_per_kinetochore")) + 
        geom_boxplot(fill = "grey20", color = "black") + theme_classic() +
        xlab("Data-set names") + ylab("Number of KMTs per kinetochore")
      
      tryCatch({
         for(i in 2:nrow(Plot_Data)){
        
        P1 <<- P1 + geom_boxplot(data = get(paste("Data_", Plot_Data[i,"V1"], "_", Plot_Data[i, "V2"], sep = "")), aes_string("Data", "KMTs_per_kinetochore"), 
                                fill = "grey30", color = "black")
      }
      
      All_KMT_No <<- get(paste("Data_", Plot_Data[i,"V1"], "_", Plot_Data[i, "V2"], sep = ""))
      
      for(i in 2:nrow(Plot_Data)){
        assign("All_KMT_No",
               rbind(All_KMT_No["KMTs_per_kinetochore"],
                     get(paste("Data_", Plot_Data[i,"V1"], "_", Plot_Data[i, "V2"], sep = ""))["KMTs_per_kinetochore"]),
               envir = .GlobalEnv)
      }
      
      All_KMT_No <<- data.frame(Data = "Average",
                                All_KMT_No["KMTs_per_kinetochore"])
      
      P1 <<- P1 + geom_boxplot(data = All_KMT_No, aes("Avg.", KMTs_per_kinetochore), fill = "darkred", color = "black", outlier.alpha = 0) + 
        geom_jitter(data = All_KMT_No, aes("Avg.", KMTs_per_kinetochore), alpha = 0.1, size = 1, width = 0.25)
      },
      error = function(e){})
      
      print(P1)
    },
    error = function(e){})
  })
}