################################################################################
# Shiny Server
#
# Author: Robert Kiewisz
# Created: 2020-05-17
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
  
  # Page relativity after loading data  ----------------------------------------
  observeEvent(input$`Home-file`,{
    showTab(inputId = "innavbar-GS", target = "Settings")
    if(DataTest == 1){
      updateTabsetPanel(session, "innavbar-GS", selected = "Settings")
    }
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
    callModule(PreAnalysis, "Home")
  })
}