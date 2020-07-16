################################################################################
# Module Setting_Buttons UI/Server
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-19
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
           inputId = ns("KMT_Torque"),
           label = "KMT Torque (Beta)",
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
           inputId = ns("Fiber_Curv_Length"),
           label = "Fiber length & curvature",
           value = FALSE, 
           right = TRUE,
           status = "info"
         ),
         materialSwitch(
           inputId = ns("k_core_area"),
           label = "Area and position of the kinetochore",
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
  
  # Reactivity for KMT number ---------------------------------------------------
  observeEvent(input$`KMT_number`,{
    if(input$`KMT_number` == TRUE){
      updateMaterialSwitch(session, "All_Anaysis", FALSE)
    }
    All_Closed()
    
    output$`Tool_Info` <- renderUI({
      if(input$`KMT_number` == TRUE){
        "This tool will analyze how many KMTs can be found on each kinetochore. 
        And how many of KMTs has their (-) ends in a distance of 1 um from the centrosome.
        For more information see 'Wiki' page"
      }
    })
  })
  
  # Reactivity for IKD ----------------------------------------------------------
  observeEvent(input$`IKD`,{
    if(input$`IKD` == TRUE){
      updateMaterialSwitch(session, "All_Anaysis", FALSE)
    }
    All_Closed()
    
    output$`Tool_Info` <- renderUI({
      if(input$`IKD` == TRUE){
        "This tool will analyze a distance between each pair of sister-kinetochore. 
        For more information see 'Wiki' page"
      }
    })
    
    if(input$`IKD` == TRUE){
      confirmSweetAlert(
        session,
        inputId = "IKD_confirmation",
        title = "Want to confirm ?",
        text = "The Inter-Kinetochore distance will be calculated. This analysis relies on corresponding k-fiber labels.
      e.g. sister-kinetochore for Pole1_00 is Pole2_00.",
        type = "question",
        btn_labels = c("Cancel", "Confirm"),
        btn_colors = NULL
      )
    }
  })
  
  # Reactivity for Curvature ----------------------------------------------------
  observeEvent(input$`Curvature`,{
    if(input$`Curvature` == TRUE){
      updateMaterialSwitch(session, "All_Anaysis", FALSE)
    }
    All_Closed()
    
    output$`Tool_Info` <- renderUI({
      if(input$`Curvature` == TRUE){
        "This tool will analyze the total and the local curvature for each KMT. 
        For more information see 'Wiki' page"
      }
    })
  })
  
  # Reactivity for End_morphology -------------------------------------------------
  observeEvent(input$`End_Morphology`,{
    if(input$`End_Morphology` == TRUE){
      updateMaterialSwitch(session, "All_Anaysis", FALSE)
    }
    All_Closed()
    
    output$`Tool_Info` <- renderUI({
      if(input$`End_Morphology` == TRUE){
        "This tool will analyze end types distribution.
        For more information see 'Wiki' page"
      }
    })
  })
  
  # Reactivity for End_morphology -------------------------------------------------
  observeEvent(input$`KMT_Torque`,{
    if(input$`KMT_Torque` == TRUE){
      updateMaterialSwitch(session, "All_Anaysis", FALSE)
    }
    All_Closed()
    
    output$`Tool_Info` <- renderUI({
      if(input$`KMT_Torque` == TRUE){
        "This tool will analyze a torque of a KMTs in a fiber.
        For more information see 'Wiki' page"
      }
    })
  })
  
  # Reactivity for fiber area ---------------------------------------------------
  observeEvent(input$`Fiber_Area`,{
    if(input$`Fiber_Area` == TRUE){
      updateMaterialSwitch(session, "All_Anaysis", FALSE)
    }
    All_Closed()
    
    output$`Tool_Info` <- renderUI({
      if(input$`Fiber_Area` == TRUE){
        "This tool will analyze the area of each fiber based on the polygon approach, and
        the neighborhood density along Kinetochore-Pole axis.
        For more information see 'Wiki' page"
      }
    })
  })
  
  # Reactivity for minu end seeds -----------------------------------------------
  observeEvent(input$`KMT_Minus_End_Seeds`,{
    if(input$`KMT_Minus_End_Seeds` == TRUE){
      updateMaterialSwitch(session, "All_Anaysis", FALSE)
    }
    All_Closed()
    
    output$`Tool_Info` <- renderUI({
      if(input$`KMT_Minus_End_Seeds` == TRUE){
        "This tool will analyze the position of all (-) ends with respect to the KMT, and 
        determining the position of MT which nucleated from KMT.
        For more information see 'Wiki' page"
      }
    })
  })

  # Reactivity for Fiber curve and length button --------------------------------
  observeEvent(input$`Fiber_Curv_Length`,{
    if(input$`Fiber_Curv_Length` == TRUE){
      updateMaterialSwitch(session, "All_Anaysis", FALSE)
    }
    All_Closed()
    
    output$`Tool_Info` <- renderUI({
      if(input$`Fiber_Curv_Length` == TRUE){
        "This tool will analyze the length of a k-fiber based on the assumption that a fiber
        is a fiber till it has at least 3 KMTs, the fiber total and local curvature is calculated 
        using the fiber generated based on the above assumption.
        For more information see 'Wiki' page"
      }
    })
  })
  
  # Reactivity for Fiber curve and length button --------------------------------
  observeEvent(input$`k_core_area`,{
    if(input$`k_core_area` == TRUE){
      updateMaterialSwitch(session, "All_Anaysis", FALSE)
    }
    All_Closed()
    
    output$`Tool_Info` <- renderUI({
      if(input$`k_core_area` == TRUE){
        "This tool will analyze the area of the kinetochore and combine it with kinetochore position in the spindle
        and number of the KMTs at the kinetochore.
        For more information see 'Wiki' page"
      }
    })
  })
  
  # Reactivity for all button --------------------------------------------------
  
  observeEvent(input$`All_Anaysis`,{
    output$`Tool_Info` <- renderUI({
      if(input$`All_Anaysis` == TRUE){
        "All analysis will be run. For more information see 'Wiki' page"
      }
    })
    
    if(input$`All_Anaysis` == TRUE){
      updateMaterialSwitch(session, "KMT_number", FALSE)
      updateMaterialSwitch(session, "IKD", FALSE)
      updateMaterialSwitch(session, "Curvature", FALSE)
      updateMaterialSwitch(session, "End_Morphology", FALSE)
      updateMaterialSwitch(session, "KMT_Torque", FALSE)
      updateMaterialSwitch(session, "Fiber_Area", FALSE)
      updateMaterialSwitch(session, "KMT_Minus_End_Seeds", FALSE)
      updateMaterialSwitch(session, "Fiber_Curv_Length", FALSE)
      updateMaterialSwitch(session, "k_core_area", FALSE)
    }
  })
  
  All_Closed <- function(){
    if (input$`All_Anaysis` == FALSE &&
        input$`KMT_number` == FALSE &&
        input$`IKD` == FALSE &&
        input$`Curvature` == FALSE &&
        input$`End_Morphology` == FALSE &&
        input$`KMT_Torque` == FALSE &&
        input$`Fiber_Area` == FALSE &&
        input$`KMT_Minus_End_Seeds` == FALSE &&
        input$`Fiber_Curv_Length` == FALSE &&
        input$`k_core_area` == FALSE){
      updateMaterialSwitch(session, "All_Anaysis", TRUE)
    }
  }
}