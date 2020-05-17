################################################################################
# Module Setting_Buttons UI/Server
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-16 
################################################################################

# Setting_BUttons_UI  ----------------------------------------------------------
Setting_Buttons_UI <- function(id){
  ns <- NS(id)
  column(4,
         materialSwitch(
           inputId = ns("AllAnaysis"),
           label = "All",
           value = TRUE, 
           right = TRUE,
           status = "info"
         ),
         materialSwitch(
           inputId = ns("KMTatthePole"),
           label = "No of KMTs at the Pole",
           value = FALSE, 
           right = TRUE,
           status = "info"
         ),
         materialSwitch(
           inputId = ns("KMTperKcore"),
           label = "No of KMTs per kinetochore",
           value = FALSE, 
           right = TRUE,
           status = "info"
         )
  )
  
}

# Setting_BUttons_Server  -------------------------------------------------------
Setting_Buttons_Server <- function (input, output, session){
  
  observeEvent(input$`KMTatthePole`,{
    output$`ToolInfo` <- renderUI({
      if(input$`KMTatthePole` == TRUE){
        "This tool will analyse how many of KMTs has their (-) ends in a distance of 1 um from the centrosome. 
        For more information see 'Wiki' page"
      }
    })
  })
  
  observeEvent(input$`KMTperKcore`,{
    output$`ToolInfo` <- renderUI({
      if(input$`KMTatthePole` == TRUE){
        "This tool will analyse how many KMTs can be found on each kinetochore. 
        For more information see 'Wiki' page"
      }
    })
  })
  
  observeEvent(input$`AllAnaysis`,{
    output$`ToolInfo` <- renderUI({
      if(input$`AllAnaysis` == TRUE){
        "All analysis will be run. For more information see 'Wiki' page"
      } else {
        "Please select what any option!"
      }
    })
  })
}
