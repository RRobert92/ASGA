################################################################################
# Module Setting_Buttons UI/Server
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-19
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
################################################################################

# Setting_BUttons_UI  ----------------------------------------------------------
Setting_Buttons_UI <- function(id) {
  ns <- NS(id)
  column(
    4,
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
Setting_Buttons_Server <- function(input, output, session) {

  # Reactivity for KMT number ---------------------------------------------------
  observeEvent(input$`KMT_number`, {
    All_Closed()
    Any_One()
    Sys.sleep(0.1)

    output$`Tool_Info_1` <- renderUI({
      if (input$`KMT_number` == TRUE) {
        "This tool will analyze how many KMTs can be found on each kinetochore. 
        And how many of KMTs has their (-) ends in a distance of 1 um from the centrosome.
        For more information see 'Wiki' page"
      }
    })

    if (input$`KMT_number` == TRUE) {
      inputSweetAlert(
        session = session, 
        type = "info",
        inputId = "KMT_no_config", input = "text",
        title = "Set-up analysis parameter",
        text = "Threshold for the microtubule minus-end interaction with the spindle pole. Unit [um]"
      )
    }

    observeEvent(input[["KMT_no_config"]], {
      assign("Minus_Threshold",
        as.numeric(input[["KMT_no_config"]]),
        envir = .GlobalEnv
      )
    })
  })

  # Reactivity for IKD ----------------------------------------------------------
  observeEvent(input$`IKD`, {
    All_Closed()
    Any_One()
    Sys.sleep(0.1)

    output$`Tool_Info_1` <- renderUI({
      if (input$`IKD` == TRUE) {
        "This tool will analyze a distance between each pair of sister-kinetochore. 
        For more information see 'Wiki' page"
      }
    })

    if (input$`IKD` == TRUE) {
      confirmSweetAlert(
        session = session, 
        type = "question",
        inputId = "IKD_confirmation", input = "text",
        title = "Want to confirm ?",
        text = "The Inter-Kinetochore distance will be calculated. This analysis relies on corresponding k-fiber labels.
      e.g. sister-kinetochore for Pole1_00 is Pole2_00.",
        btn_labels = c("Cancel", "Confirm"),
        btn_colors = c("#C95050", "#a5dc86")
      )
    }
    
    observeEvent(input[["IKD_confirmation"]], {
      if(input[["IKD_confirmation"]] == FALSE){
        updateMaterialSwitch(session, "IKD", FALSE)
      }
    })
  })

  # Reactivity for Curvature ----------------------------------------------------
  observeEvent(input$`Curvature`, {
    All_Closed()
    Any_One()
    Sys.sleep(0.1)

    output$`Tool_Info_1` <- renderUI({
      if (input$`Curvature` == TRUE) {
        "This tool will analyze the total and the local curvature for each KMT. 
        For more information see 'Wiki' page"
      }
    })
  })

  # Reactivity for End_morphology -------------------------------------------------
  observeEvent(input$`End_Morphology`, {
    All_Closed()
    Any_One()
    Sys.sleep(0.1)

    output$`Tool_Info_1` <- renderUI({
      if (input$`End_Morphology` == TRUE) {
        "This tool will analyze end types distribution.
        For more information see 'Wiki' page"
      }
    })
  })

  # Reactivity for End_morphology -------------------------------------------------
  observeEvent(input$`KMT_Torque`, {
    All_Closed()
    Any_One()
    Sys.sleep(0.1)

    output$`Tool_Info_1` <- renderUI({
      if (input$`KMT_Torque` == TRUE) {
        "This tool will analyze a torque of a KMTs in a fiber.
        For more information see 'Wiki' page"
      }
    })
  })

  # Reactivity for fiber area ---------------------------------------------------
  observeEvent(input$`Fiber_Area`, {
    All_Closed()
    Any_One()
    Sys.sleep(0.1)

    output$`Tool_Info_1` <- renderUI({
      if (input$`Fiber_Area` == TRUE) {
        "This tool will analyze the area of each fiber based on the polygon approach, and
        the neighborhood density along Kinetochore-Pole axis.
        For more information see 'Wiki' page."
      }
    })

    if (input$`Fiber_Area` == TRUE) {
      inputSweetAlert(
        session = session, 
        type = "info",
        inputId = "Fiber_area_config", input = "text",
        title = "Set-up analysis parameter",
        text = "Bin size used to calculate fiber area every specified distance on the spindle pole axis. Unit [nm]"
      )
    }

    observeEvent(input[["Fiber_area_config"]], {
      assign("Fiber_area_config",
        round(as.numeric(input[["Fiber_area_config"]]) / 20, 0) - 1,
        envir = .GlobalEnv
      )
    })
  })

  # Reactivity for minu end seeds -----------------------------------------------
  observeEvent(input$`KMT_Minus_End_Seeds`, {
    All_Closed()
    Any_One()
    Sys.sleep(0.1)

    output$`Tool_Info_1` <- renderUI({
      if (input$`KMT_Minus_End_Seeds` == TRUE) {
        "This tool will analyze the position of all (-) ends with respect to the KMT, and 
        determining the position of MT which nucleated from KMT.
        For more information see 'Wiki' page"
      }
    })

    if (input$`KMT_Minus_End_Seeds` == TRUE) {
      inputSweetAlert(
        session = session, 
        type = "info",
        inputId = "Minus_end_config", input = "text",
        title = "Set-up analysis parameter",
        text = "Interaction distance between kinetochore microtubule and microtubule minus-end. Unit [um]"
      )
    }

    observeEvent(input[["Minus_end_config"]], {
      assign("Minus_Distance",
        as.numeric(input[["Minus_end_config"]]),
        envir = .GlobalEnv
      )
    })
  })

  # Reactivity for Fiber curve and length button --------------------------------
  observeEvent(input$`Fiber_Curv_Length`, {
    All_Closed()
    Any_One()
    Sys.sleep(0.1)

    output$`Tool_Info_1` <- renderUI({
      if (input$`Fiber_Curv_Length` == TRUE) {
        "This tool will analyze the length of a k-fiber based on the assumption that a fiber
        is a fiber till it has at least 3 KMTs, the fiber total and local curvature is calculated 
        using the fiber generated based on the above assumption.
        For more information see 'Wiki' page"
      }
    })
  })

  # Reactivity for Fiber curve and length button --------------------------------
  observeEvent(input$`k_core_area`, {
    All_Closed()
    Any_One()
    Sys.sleep(0.1)

    output$`Tool_Info_1` <- renderUI({
      if (input$`k_core_area` == TRUE) {
        "This tool will analyze the area of the kinetochore and combine it with kinetochore position in the spindle
        and number of the KMTs at the kinetochore.
        For more information see 'Wiki' page"
      }
    })
  })

  # Reactivity for all button --------------------------------------------------

  observeEvent(input$`All_Anaysis`, {
    if (input$`All_Anaysis` == TRUE) {
      updateMaterialSwitch(session, "KMT_number", FALSE)
      updateMaterialSwitch(session, "IKD", FALSE)
      updateMaterialSwitch(session, "Curvature", FALSE)
      updateMaterialSwitch(session, "End_Morphology", FALSE)
      updateMaterialSwitch(session, "KMT_Torque", FALSE)
      updateMaterialSwitch(session, "Fiber_Area", FALSE)
      updateMaterialSwitch(session, "KMT_Minus_End_Seeds", FALSE)
      updateMaterialSwitch(session, "Fiber_Curv_Length", FALSE)
      updateMaterialSwitch(session, "k_core_area", FALSE)

      output$`Tool_Info_1` <- renderUI({
        "All analysis will be run with the stamdard settings. 
        For more information see 'Wiki' page"
      })
    }
  })

  All_Closed <- function() {
    if (input$`KMT_number` == FALSE &&
      input$`IKD` == FALSE &&
      input$`Curvature` == FALSE &&
      input$`End_Morphology` == FALSE &&
      input$`KMT_Torque` == FALSE &&
      input$`Fiber_Area` == FALSE &&
      input$`KMT_Minus_End_Seeds` == FALSE &&
      input$`Fiber_Curv_Length` == FALSE &&
      input$`k_core_area` == FALSE) {
      updateMaterialSwitch(session, "All_Anaysis", TRUE)
    }
  }

  Any_One <- function() {
    if (input$`KMT_number` == TRUE ||
      input$`IKD` == TRUE ||
      input$`Curvature` == TRUE ||
      input$`End_Morphology` == TRUE ||
      input$`KMT_Torque` == TRUE ||
      input$`Fiber_Area` == TRUE ||
      input$`KMT_Minus_End_Seeds` == TRUE ||
      input$`Fiber_Curv_Length` == TRUE ||
      input$`k_core_area` == TRUE) {
      updateMaterialSwitch(session, "All_Anaysis", FALSE)
    }
  }
}
