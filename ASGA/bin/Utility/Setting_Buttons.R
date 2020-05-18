################################################################################
# Module Setting_Buttons UI/Server
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-18
################################################################################

# Setting_BUttons_UI  ----------------------------------------------------------
Setting_Buttons_UI <- function(id){
  ns <- NS(id)
  column(4,
         materialSwitch(
           inputId = ns("All_Anaysis"),
           label = "All",
           value = TRUE, 
           right = TRUE,
           status = "info"
         ),
         materialSwitch(
           inputId = ns("KMT_number"),
           label = "No of KMTs at a Pole and a K-core",
           value = FALSE, 
           right = TRUE,
           status = "info"
         ),
         materialSwitch(
           inputId = ns("IKD"),
           label = "Inter-kinetochore Distance",
           value = FALSE, 
           right = TRUE,
           status = "info"
         ),
         materialSwitch(
           inputId = ns("Curvature"),
           label = "KMT curvature",
           value = FALSE, 
           right = TRUE,
           status = "info"
         ),
         materialSwitch(
           inputId = ns("End_Morphology"),
           label = "End Morphology",
           value = FALSE, 
           right = TRUE,
           status = "info"
         ),
         materialSwitch(
           inputId = ns("Fiber_Area"),
           label = "Fiber Area & Neighorhood Densit",
           value = FALSE, 
           right = TRUE,
           status = "info"
         ),
         materialSwitch(
           inputId = ns("KMT_Minus_End_Seeds"),
           label = "KMTs Nucleation of MT",
           value = FALSE, 
           right = TRUE,
           status = "info"
         )
  )
}

# Setting_BUttons_Server  -------------------------------------------------------
Setting_Buttons_Server <- function (input, output, session){
    
  observeEvent(input$`KMT_Minus_End_Seeds`,{
    output$`Tool_Info` <- renderUI({
      if(input$`KMT_Minus_End_Seeds` == TRUE){
        "This tool will analyze the position of all (-) ends with respect to the KMT, and 
        determining the position of MT which nucleated from KMT.
        For more information see 'Wiki' page"
      }
    })
  })
  
  observeEvent(input$`Fiber_Area`,{
    output$`Tool_Info` <- renderUI({
      if(input$`Fiber_Area` == TRUE){
        "This tool will analyze the area of each fiber based on the polygon approach, and
        the neighborhood density along Kinetochore-Pole axis.
        For more information see 'Wiki' page"
      }
    })
  })
  
  observeEvent(input$`End_Morphology`,{
    output$`Tool_Info` <- renderUI({
      if(input$`End_Morphology` == TRUE){
        "This tool will analyze end types distribution.
        For more information see 'Wiki' page"
      }
    })
  })
  
  observeEvent(input$`Curvature`,{
    output$`Tool_Info` <- renderUI({
      if(input$`Curvature` == TRUE){
        "This tool will analyze the total and the local curvature for each KMT. 
        For more information see 'Wiki' page"
      }
    })
  })
  
  observeEvent(input$`IKD`,{
    output$`Tool_Info` <- renderUI({
      if(input$`IKD` == TRUE){
        "This tool will analyze a distance between each pair of sister-kinetochore. 
        For more information see 'Wiki' page"
      }
    })
  })
  
  
  observeEvent(input$`KMT_number`,{
    output$`Tool_Info` <- renderUI({
      if(input$`KMT_number` == TRUE){
        "This tool will analyze how many KMTs can be found on each kinetochore. 
        And how many of KMTs has their (-) ends in a distance of 1 um from the centrosome.
        For more information see 'Wiki' page"
      }
    })
    
    if(input$`KMT_number` == TRUE){
      updateMaterialSwitch(session, "All_Anaysis", FALSE)
    }else if (input$`All_Anaysis` == FALSE &&
              input$`KMT_number` == FALSE &&
              input$`IKD` == FALSE &&
              input$`Curvature` == FALSE &&
              input$`End_Morphology` == FALSE &&
              input$`Fiber_Area` == FALSE &&
              input$`KMT_Minus_End_Seeds` == FALSE){
      updateMaterialSwitch(session, "All_Anaysis", TRUE)
    }
  })
  
  observeEvent(input$`All_Anaysis`,{
    output$`Tool_Info` <- renderUI({
      if(input$`All_Anaysis` == TRUE){
        "All analysis will be run. For more information see 'Wiki' page"
      } else {
        "Please select what any option!"
      }
    })
    
    if(input$`All_Anaysis` == TRUE){
      updateMaterialSwitch(session, "KMT_number", FALSE)
      updateMaterialSwitch(session, "IKD", FALSE)
      updateMaterialSwitch(session, "Curvature", FALSE)
      updateMaterialSwitch(session, "End_Morphology", FALSE)
      updateMaterialSwitch(session, "Fiber_Area", FALSE)
      updateMaterialSwitch
      
    }else if (input$`All_Anaysis` == FALSE){
      updateMaterialSwitch(session, "KMT_number", TRUE)
      updateMaterialSwitch(session, "IKD", TRUE)
      updateMaterialSwitch(session, "Curvature", TRUE)
      updateMaterialSwitch(session, "End_Morphology", TRUE)
      updateMaterialSwitch(session, "Fiber_Area", TRUE)
      updateMaterialSwitch(session, "KMT_Minus_End_Seeds", TRUE)
    }
  })
}