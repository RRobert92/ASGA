################################################################################
# Shiny Server
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-21
################################################################################


# Shiny Server  ----------------------------------------------------------------
function(input, output, session) {
  
  # Hide pages  ----------------------------------------------------------------
  hideTab(inputId = "innavbar", target = "GetStarted")
  hideTab(inputId = "innavbar-GS", target = "Settings")  
  
  # Get_Started button  --------------------------------------------------------
  observeEvent(input$GetStarted, {
    if(!exists("numfiles")){
      updateTabsetPanel(session, "innavbar", selected = "GetStarted")
      showTab(inputId = "innavbar", target = "GetStarted")
      updateTabsetPanel(session, "innavbar-GS", selected = "UploadData")      
    } else {
      updateTabsetPanel(session, "innavbar", selected = "GetStarted")
      showTab(inputId = "innavbar", target = "GetStarted")
      updateTabsetPanel(session, "innavbar-GS", selected = "Settings")
    }
    
  })
  # Wiki button  ---------------------------------------------------------------
  observeEvent(input$Wiki, {
    updateTabsetPanel(session, "innavbar", selected = "Wiki")
    hideTab(inputId = "innavbar", target = "GetStarted")
  })
  
  # Get file and Load data  ----------------------------------------------------
  callModule(Getfiles_Server, "Home")
  
  # Upload data UI  ------------------------------------------------------------
  output$Upload <- renderUI({
    UploadData_UI("GetStarted")
  })
  
  # Download zip files ---------------------------------------------------------
  output$downloadData  <- downloadHandler(
    filename = function() {"ASGA_Data.zip"},
    content = function(fname){
      setwd("Data/")
      on.exit(setwd("../"))
      Zip_Files <- list.files(path = getwd(), pattern = ".xlsx$")
      zipr(zipfile = fname, files = Zip_Files)
      file.remove(Zip_Files)
    })

  # Page relativity after loading data  ----------------------------------------
  observeEvent(input$`Home-file`,{
    showTab(inputId = "innavbar-GS", target = "Settings")
    if(DataTest == 1){
      updateTabsetPanel(session, "innavbar-GS", selected = "Settings")
    }else {
      updateTabsetPanel(session, "innavbar", selected = "Home")
    }
  })

  # Page relativity after loading data  ----------------------------------------
  observeEvent(input$`Home-file1`,{
    showTab(inputId = "innavbar-GS", target = "Settings")
      updateTabsetPanel(session, "innavbar-GS", selected = "Settings")
  })
  
  # Relativity for the Home and GS button  -------------------------------------
  observe({
    if (req(input$`innavbar-GS`) == "Home"){
      updateTabsetPanel(session, "innavbar", selected = "Home")
      hideTab(inputId = "innavbar", target = "GetStarted")
    }else if (req(input$`innavbar-GS`) == "UploadData"){
      updateTabsetPanel(session, "innavbar-GS", selected = "UploadData")
    }  else if (req(input$`innavbar-GS`) == "Settings"){
      updateTabsetPanel(session, "innavbar-GS", selected = "Settings")
    }  
  })
  
  # Relativity for the Settings button  ----------------------------------------
  callModule(Setting_Buttons_Server, "Home")
  
  # Relativity for Pre-Analysis  -----------------------------------------------
  observeEvent(input$`Submit`,{
      withProgress(message = "Analyzing:", value = 1, {
        
        for (y in 1:numfiles) {
          current_data <<- y
          incProgress(1/numfiles, detail = paste("Data set no.", y, sep = " "))
          Sys.sleep(0.1)
          
          callModule(Load_Data, "Home")
          callModule(Pre_Analysis, "Home")
          
          if(input$`Home-All_Anaysis` == TRUE){
            callModule(A_KMT_number, "Home")
            callModule(A_IKD, "Home")
            callModule(A_Curvature, "Home")
            callModule(A_End_Morphology, "Home")
            callModule(A_Fiber_Area, "Home")
            callModule(A_KMT_Minus_End_Seeds, "Home")
          }
          
          if(input$`Home-KMT_number` == TRUE){
            callModule(A_KMT_number, "Home")
          }
          
          if(input$`Home-IKD` == TRUE){
            callModule(A_IKD, "Home")
          }
          
          if(input$`Home-Curvature` == TRUE){
            callModule(A_Curvature, "Home")
          }
          
          if(input$`Home-End_Morphology` == TRUE){
            callModule(A_End_Morphology, "Home")
          }
          
          if(input$`Home-Fiber_Area` == TRUE){
            callModule(A_Fiber_Area, "Home")
          }
          
          if(input$`Home-KMT_Minus_End_Seeds` == TRUE){
            callModule(A_KMT_Minus_End_Seeds, "Home")
          }
          callModule(Save_Data ,"Home")
        }
      })
    
    output$`Home-Download_Button` <- renderUI({
     downloadBttn(
       "downloadData",
       label = "Download",
       style = "material-flat",
       color = "success"
     )
    })
  })
}