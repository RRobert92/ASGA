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
    tags$div(
      class = "Setting_button",
      materialSwitch(
        inputId = ns("All_Anaysis"),
        label = "All",
        value = TRUE,
        right = TRUE,
        status = "info"
      )
    ),
    tags$div(
      class = "Setting_button",
      materialSwitch(
        inputId = ns("KMT_number"),
        label = "No of KMTs at a Pole and a K-core",
        value = FALSE,
        right = TRUE,
        status = "info"
      )
    ),
    tags$div(
      class = "Setting_button",
      materialSwitch(
        inputId = ns("IKD"),
        label = "Inter-kinetochore Distance",
        value = FALSE,
        right = TRUE,
        status = "info"
      )
    ),
    tags$div(
      class = "Setting_button",
      materialSwitch(
        inputId = ns("Curvature"),
        label = "KMT curvature",
        value = FALSE,
        right = TRUE,
        status = "info"
      )
    ),
    tags$div(
      class = "Setting_button",
      materialSwitch(
        inputId = ns("End_Morphology"),
        label = "End Morphology",
        value = FALSE,
        right = TRUE,
        status = "info"
      )
    ),
    tags$div(
      class = "Setting_button",
      materialSwitch(
        inputId = ns("KMT_Torque"),
        label = "KMT Torque",
        value = FALSE,
        right = TRUE,
        status = "info"
      )
    ),
    tags$div(
      class = "Setting_button",
      materialSwitch(
        inputId = ns("Fiber_Area"),
        label = "Fiber Area & Neighorhood Densit",
        value = FALSE,
        right = TRUE,
        status = "info"
      )
    ),
    tags$div(
      class = "Setting_button",
      materialSwitch(
        inputId = ns("Fiber_Curv_Length"),
        label = "Fiber length & curvature",
        value = FALSE,
        right = TRUE,
        status = "info"
      )
    ),
    tags$div(
      class = "Setting_button",
      materialSwitch(
        inputId = ns("k_core_area"),
        label = "Area and position of the kinetochore",
        value = FALSE,
        right = TRUE,
        status = "info"
      )
    ),
    tags$div(
      class = "Setting_button",
      materialSwitch(
        inputId = ns("KMT_Minus_End_Seeds"),
        label = "KMTs Nucleation of MT",
        value = FALSE,
        right = TRUE,
        status = "info"
      )
    ),
    tags$div(
      class = "Setting_button",
      materialSwitch(
        inputId = ns("MT_Interaction"),
        label = "MT Interaction",
        value = FALSE,
        right = TRUE,
        status = "info"
      )
    )
  )
}

# Setting_BUttons_Server  ------------------------------------------------------
Setting_Buttons_Server <- function(input, output, session) {

  # Reactivity for all analysis ------------------------------------------------
  observeEvent(input$`All_Anaysis`, {
    All_Closed()
    Any_One()

    Sys.sleep(0.1)

    output$`Tool_Info_1` <- renderUI({
      if (input$`All_Anaysis` == TRUE) {
        "This setting will compute all analysis for your uploaded files, some
        analyses are time-consuming.
        Take a coffee break."
      }
    })
  })

  # Reactivity for KMT number ------------------------------------------------
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
        inputId = "KMT_no_config",
        input = "text",
        title = "Set-up analysis parameter",
        text = "Threshold for the microtubule minus-end interaction with the spindle pole. Unit [um]"
      )
    }

    observeEvent(input[["KMT_no_config"]], {
      assign("MINUS_THRESHOLD",
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
        inputId = "IKD_confirmation",
        input = "text",
        title = "Want to confirm ?",
        text = "The Inter-Kinetochore distance will be calculated. This analysis relies on corresponding k-fiber labels.
      e.g. sister-kinetochore for Pole1_00 is Pole2_00.",
        btn_labels = c("Cancel", "Confirm"),
        btn_colors = c("#C95050", "#a5dc86")
      )
    }

    observeEvent(input[["IKD_confirmation"]], {
      if (input[["IKD_confirmation"]] == FALSE) {
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

    if (input$`Curvature` == TRUE) {
      inputSweetAlert(
        session = session,
        type = "info",
        inputId = "Curvature_config",
        input = "text",
        title = "Set-up analysis parameter",
        text = "Bin size used to calculate local curvature every specified distance on the spindle pole axis. Unit [nm]"
      )
    }

    observeEvent(input[["Curvature_config"]], {
      assign("CURVATURE_CONFIG",
        round(as.numeric(input[["Curvature_config"]]) / 20, 0) - 1,
        envir = .GlobalEnv
      )
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

    if (input$`End_Morphology` == TRUE) {
      confirmSweetAlert(
        session = session,
        type = "info",
        inputId = "Relative_nonKMT",
        title = "Relative Position for non-KMTs",
        text = "Is the relative position should be calculated between pole1 and pole2?",
        btn_labels = c("No", "Yes"),
        btn_colors = c("#C95050", "#a5dc86")
      )
    }

    observeEvent(input[["Relative_nonKMT"]], {
      assign("RP_Pole",
             input[["Relative_nonKMT"]],
             envir = .GlobalEnv
      )
    })
  })

  # Reactivity for KMT torque -------------------------------------------------
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
        inputId = "Fiber_area_config",
        input = "text",
        title = "Set-up analysis parameter",
        text = "Bin size used to calculate fiber area every specified distance on the spindle pole axis. Unit [nm]"
      )
    }

    observeEvent(input[["Fiber_area_config"]], {
      assign("FIBER_AREA_CONFIG",
        round(as.numeric(input[["Fiber_area_config"]]) / 20, 0) - 1,
        envir = .GlobalEnv
      )
    })
  })

  # Reactivity for minus end seeds -----------------------------------------------
  observeEvent(input$`KMT_Minus_End_Seeds`, {
    All_Closed()
    Any_One()

    Sys.sleep(0.1)

    output$`Tool_Info_1` <- renderUI({
      if (input$`KMT_Minus_End_Seeds` == TRUE) {
        "This tool will analyze the position of all (-) ends with respect to the KMT,
              and determining the position of MT which nucleated from KMT.
              In addition, this tool also analyze KMTs (-) ends position in proximity to MT lattice.
              For more information see 'Wiki' page"
      }
    })

    if (input$`KMT_Minus_End_Seeds` == TRUE) {
      confirmSweetAlert(
        session = session,
        type = "question",
        inputId = "Minus_End_Seeds_confirmation",
        input = "text",
        title = "Want to confirm ?",
        text = "These tools will calculate the interaction of KMT lattice and minus ends with other MT. It required long computation time,
      and therefore by standard, this analysis is switched off at shinyapp.io server!
      This tool can and is strongly suggested to be used with a computer cluster.",
        btn_labels = c("Cancel", "Confirm"),
        btn_colors = c("#C95050", "#a5dc86")
      )
    }

    observeEvent(input[["Minus_End_Seeds_confirmation"]], {
      if (input[["Minus_End_Seeds_confirmation"]] == FALSE) {
        updateMaterialSwitch(session, "KMT_Minus_End_Seeds", FALSE)
        SHINY_IO <<- TRUE
      } else if (input[["Minus_End_Seeds_confirmation"]] == TRUE &&
        input$`KMT_Minus_End_Seeds` == TRUE) {
        SHINY_IO <<- FALSE
        confirmSweetAlert(
          session = session,
          type = "question",
          inputId = "Run_as_Function_Seed",
          input = "text",
          title = "Want to confirm ?",
          text = "Would you like to run KMT interaction analysis as a function of interaction distance?
        If yes the interaction distance will be vary between 25, 35, 50, 75 and 100 nm.
        Keep in mind it will also take substancial longer amount of time. Preferable run over night :)",
          btn_labels = c("Cancel", "Confirm"),
          btn_colors = c("#C95050", "#a5dc86")
        )
      }
    })

    observeEvent(input[["Run_as_Function_Seed"]], {
      if (input[["Run_as_Function_Seed"]] == TRUE) {
        assign("SEED_INT_AS_FUNCTION",
          TRUE,
          envir = .GlobalEnv
        )
      } else {
        assign("SEED_INT_AS_FUNCTION",
          FALSE,
          envir = .GlobalEnv
        )

        inputSweetAlert(
          session = session,
          type = "info",
          inputId = "Seed_config",
          input = "text",
          title = "Set-up analysis parameter",
          text = "Interaction distance between microtubule and other microtubule. Unit [um]"
        )
      }
    })

    observeEvent(input[["Seed_config"]], {
      assign("MINUS_DISTANCE",
        as.numeric(input[["Seed_config"]]),
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

  # Reactivity for MT interaction button --------------------------------
  observeEvent(input$`MT_Interaction`, {
    All_Closed()
    Any_One()
    Sys.sleep(0.1)

    output$`Tool_Info_1` <- renderUI({
      if (input$`MT_Interaction` == TRUE) {
        "This tool allowas to calculate interaction between every MT in the data set.
                For more information see 'Wiki' page"
      }
    })

    if (input$`MT_Interaction` == TRUE) {
      confirmSweetAlert(
        session = session,
        type = "question",
        inputId = "Interaction_confirmation",
        input = "text",
        title = "Want to confirm ?",
        text = "These tools will calculate the interaction of every MT. It required long computation time,
        and therefore by standard, this analysis is switched off at shinyapp.io server!
        This tool can and is strongly suggested to be used with a computer cluster.",
        btn_labels = c("Cancel", "Confirm"),
        btn_colors = c("#C95050", "#a5dc86")
      )
    }

    observeEvent(input[["Interaction_confirmation"]], {
      if (input[["Interaction_confirmation"]] == FALSE) {
        updateMaterialSwitch(session, "MT_Interaction", FALSE)
        SHINY_IO <<- TRUE
      } else if (input[["Interaction_confirmation"]] == TRUE &&
        input$`MT_Interaction` == TRUE) {
        SHINY_IO <<- FALSE
        confirmSweetAlert(
          session = session,
          type = "question",
          inputId = "Run_as_Function",
          input = "text",
          title = "Want to confirm ?",
          text = "Would you like to run MT interaction analysis as a function of interaction distance?
          If yes the interaction distance will be vary between 25, 35, and 50.
          Keep in mind it will also take substancial longer amount of time. Preferable run over night :)",
          btn_labels = c("Cancel", "Confirm"),
          btn_colors = c("#C95050", "#a5dc86")
        )
      }
    })

    observeEvent(input[["Run_as_Function"]], {
      if (input[["Run_as_Function"]] == TRUE) {
        assign("MT_INT_AS_FUNCTION",
          TRUE,
          envir = .GlobalEnv
        )
      } else {
        assign("MT_INT_AS_FUNCTION",
          FALSE,
          envir = .GlobalEnv
        )

        inputSweetAlert(
          session = session,
          type = "info",
          inputId = "MT_point_config",
          input = "text",
          title = "Set-up analysis parameter",
          text = "Interaction distance between microtubule and other microtubule. Unit [um]"
        )
      }
    })

    observeEvent(input[["MT_point_config"]], {
      assign("MT_POINT_CONFIG",
        as.numeric(input[["MT_point_config"]]),
        envir = .GlobalEnv
      )
    })
  })

  # Reactivity for all button --------------------------------------------------

  observeEvent(input$`All_Anaysis`, {
    All_Closed()
    Any_One()
    Sys.sleep(0.1)

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
      updateMaterialSwitch(session, "MT_Interaction", FALSE)

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
      input$`k_core_area` == FALSE &&
      input$`MT_Interaction` == FALSE) {
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
      input$`k_core_area` == TRUE ||
      input$`MT_Interaction` == TRUE) {
      updateMaterialSwitch(session, "All_Anaysis", FALSE)
    }
  }
}
