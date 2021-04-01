################################################################################
# Shiny Server
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-21
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
################################################################################

# Shiny Server  ----------------------------------------------------------------
function(input, output, session) {
  assign("Data_Points_1",
    readRDS("tests/Data_Points_1"),
    envir = .GlobalEnv
  )
  assign("Data_Segments_1",
    readRDS("tests/Data_Segments_1"),
    envir = .GlobalEnv
  )

  # Hide pages  ----------------------------------------------------------------
  hideTab(inputId = "innavbar", target = "GetStarted")
  hideTab(inputId = "innavbar", target = "3D_Viewer")
  hideTab(inputId = "innavbar-GS", target = "Settings")
  hideTab(inputId = "innavbar-GS", target = "Report")

  # Get_Started button  --------------------------------------------------------
  observeEvent(input$GetStarted, {
    if (NUM_FILES == 0) {
      updateTabsetPanel(session, "innavbar", selected = "GetStarted")
      showTab(inputId = "innavbar", target = "GetStarted")
      updateTabsetPanel(session, "innavbar-GS", selected = "UploadData")
    } else {
      updateTabsetPanel(session, "innavbar", selected = "GetStarted")
      showTab(inputId = "innavbar", target = "GetStarted")
      updateTabsetPanel(session, "innavbar-GS", selected = "Settings")
    }
  })

  # Wiki & about button  -------------------------------------------------------
  observeEvent(input$Wiki, {
    js$browseURL("https://rrobert92.github.io/ASGA/")
  })

  observeEvent(input$innavbar, {
    if (input$innavbar == "Wiki") {
      js$browseURL("https://rrobert92.github.io/ASGA/")
      updateTabsetPanel(session, "innavbar", selected = "Home")
    }
    if (input$innavbar == "About") {
      js$browseURL("https://rrobert92.github.io/ASGA/Cit/")
      updateTabsetPanel(session, "innavbar", selected = "Home")
    }
  })

  observeEvent(input$"innavbar-GS", {
    if (input$"innavbar-GS" == "Wiki") {
      js$browseURL("https://rrobert92.github.io/ASGA/")
    }
    if (input$"innavbar-GS" == "About") {
      js$browseURL("https://rrobert92.github.io/ASGA/Cit/")
    }
  })

  observeEvent(input$"innavbar-3D", {
    if (input$"innavbar-3D" == "Wiki") {
      js$browseURL("https://rrobert92.github.io/ASGA/")
    }
    if (input$"innavbar-3D" == "About") {
      js$browseURL("https://rrobert92.github.io/ASGA/Cit/")
    }
  })

  # 3D_Viewer page  ------------------------------------------------------------
  observeEvent(input$DataViewer, {
    hideTab(inputId = "innavbar", target = "GetStarted")
    hideTab(inputId = "innavbar-GS", target = "Settings")
    hideTab(inputId = "innavbar-GS", target = "Report")
    hideTab(inputId = "innavbar-3D", target = "3D_Viewer")
    showTab(inputId = "innavbar", target = "3D_Viewer")

    updateTabsetPanel(session, "innavbar", selected = "3D_Viewer")
    updateTabsetPanel(session, "innavbar-3D", selected = "3D_Data_Select")
  })

  # 3D_Viewer open model logic  ------------------------------------------------
  observeEvent(input$`3D_Viewer_Pub_1`, {
    showTab(inputId = "innavbar-3D", target = "3D_Viewer")
    updateTabsetPanel(session, "innavbar-3D", selected = "3D_Viewer")
  })

  observeEvent(input$`Home-MT_NO`, {
    assign("MT_NO_IMPUT",
      as.numeric(input[["Home-MT_NO"]]),
      envir = .GlobalEnv
    )

    output$wdg <- renderRglwidget({
      open3d()
      rgl.bg(color = "black", fogtype = "none")
      rgl.light(
        diffuse = "gray75",
        specular = "gray75", viewpoint.rel = FALSE
      )

      for (i in 1:MT_NO_IMPUT) {
        MT <- as.numeric(unlist(strsplit(Data_Segments_1[i, "Point IDs"], split = ",")))
        MT <- Data_Points_1[as.numeric(MT[which.min(MT)] + 1):as.numeric(MT[which.max(MT)] + 1), 2:4]
        # MT <- cylinder3d(MT/10000, radius=0.01)
        if (length(Data_Segments_1[i, 2:94][Data_Segments_1[i, 2:94] == TRUE]) == 1) {
          # shade3d(MT, col="red")
          lines3d(MT, col = "red")
        } else {
          # shade3d(MT, col="white")
          lines3d(MT, col = "white", alpha = 1)
        }
      }
      scene <- scene3d()
      rgl.close()

      rglwidget(scene, reuse = TRUE)
    })
  })

  # Get file and Load data  ----------------------------------------------------
  callModule(Getfiles_Server, "Home")

  # Upload data UI  ------------------------------------------------------------
  output$Upload <- renderUI({
    UploadData_UI("GetStarted")
  })

  # Load standard data ---------------------------------------------------------
  observeEvent(input$`Test_unit`, {
    showTab(inputId = "innavbar-GS", target = "Settings")
    updateTabsetPanel(session, "innavbar-GS", selected = "Settings")

    NUM_FILES <<- 1
    DATA_TEST <<- 1
    TEST <<- TRUE
    AMIRA <<- FALSE
  })

  # Download zip files ---------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      "ASGA_Data.zip"
    },
    content = function(fname) {
      setwd("Data/")
      on.exit(setwd("../"))

      Zip_Files_xlsl <- list.files(path = getwd(), pattern = ".xlsx$")
      Zip_Files_am <- list.files(path = getwd(), pattern = ".am$")
      Zip_Files <- c(Zip_Files_am, Zip_Files_xlsl)
      zipr(zipfile = fname, files = Zip_Files)

      file.remove(Zip_Files)
    }
  )

  # Page responsiveness after loading data  ------------------------------------
  observeEvent(input$`Home-file`, {
    TEST <<- FALSE

    showTab(inputId = "innavbar-GS", target = "Settings")
    if (DATA_TEST == 1) {
      updateTabsetPanel(session, "innavbar-GS", selected = "Settings")
    } else {
      updateTabsetPanel(session, "innavbar", selected = "Home")
      hideTab(inputId = "innavbar-GS", target = "Settings")
      hideTab(inputId = "innavbar", target = "GetStarted")
      NUM_FILES <<- 0
    }
  })

  # Page responsiveness after loading data  ------------------------------------
  observeEvent(input$`Home-file1`, {
    TEST <<- FALSE

    if (AnalysisTest == 1) {
      showTab(inputId = "innavbar-GS", target = "Report")
      updateTabsetPanel(session, "innavbar-GS", selected = "Report")

      File_name <<- as.data.frame(File_name)
      NUM_FILES <<- readr::parse_number(File_name[nrow(File_name), 1])

      df <- data.frame()

      for (i in 1:nrow(File_name)) {
        name <- as.data.frame(str_split(File_name[i, 1], "_"))
        df[i, 1] <- as.numeric(name[2, 1])
        name <- as.data.frame(str_split(
          File_name[i, 1],
          paste("Data_", df[i, 1], "_", sep = "")
        ))
        df[i, 2] <- as.character(name[2, 1])
      }

      File_name <<- df

      rm(df, name)

      # Collect information to start a plot after analysis ---------------------
      lapply(1:NUM_FILES, function(i) {
        observeEvent(input[[paste("Data_label", i, sep = "_")]], {
          assign(paste("Data_label", i, sep = "_"),
            input[[paste("Data_label", i, sep = "_")]],
            envir = .GlobalEnv
          )
        })

        observeEvent(input[[paste("Data_color", i, sep = "_")]], {
          assign(paste("Data_color", i, sep = "_"),
            input[[paste("Data_color", i, sep = "_")]],
            envir = .GlobalEnv
          )
        })

        observeEvent(input[[paste("Data_bin", i, sep = "_")]], {
          assign(paste("Data_bin", i, sep = "_"),
            input[[paste("Data_bin", i, sep = "_")]],
            envir = .GlobalEnv
          )
        })
      })

      callModule(Report_Plot_Settings, "Home")
      callModule(Report_Plot, "Home")
    } else {
      updateTabsetPanel(session, "innavbar", selected = "Home")
      callModule(Error_Handler, "Home")
    }
  })

  # Relativity for the Home and GS page  ----------------------------------------
  observe({
    if (req(input$`innavbar-GS`) == "Home") {
      updateTabsetPanel(session, "innavbar", selected = "Home")
      hideTab(inputId = "innavbar", target = "GetStarted")
    } else if (req(input$`innavbar-GS`) == "UploadData") {
      updateTabsetPanel(session, "innavbar-GS", selected = "UploadData")
    } else if (req(input$`innavbar-GS`) == "Settings") {
      updateTabsetPanel(session, "innavbar-GS", selected = "Settings")
    }
  })

  # Relativity for the Home and 3D Page  ---------------------------------------
  observe({
    if (req(input$`innavbar-3D`) == "Home") {
      updateTabsetPanel(session, "innavbar", selected = "Home")
      hideTab(inputId = "innavbar", target = "3D_Viewer")
    }
  })
  # Relativity for the Settings button  ----------------------------------------
  callModule(Setting_Buttons_Server, "Home")

  # Relativity for Pre-Analysis  -----------------------------------------------
  observeEvent(input$`Submit`, {
    if (TEST == FALSE) {
      withProgress(message = "Analyzing:", value = 1, {
        for (y in 1:NUM_FILES) {
          current_data <<- y
          incProgress(1 / NUM_FILES,
            detail = paste("Data set no.", y, sep = " ")
          )
          Sys.sleep(0.1)

          callModule(Load_Data, "Home")
          callModule(Pre_Analysis, "Home")

          if (input$`Home-All_Anaysis` == TRUE) {
            callModule(A_KMT_number, "Home")
            callModule(A_IKD, "Home")
            callModule(A_Curvature, "Home")
            callModule(A_End_Morphology, "Home")
            callModule(A_KMT_Torque, "Home")
            callModule(A_Fiber_Area, "Home")
            callModule(A_Fiber_Length_Curv, "Home")
            callModule(A_K_Core_Area, "Home")
            if (SHINY_IO == FALSE) {
              callModule(A_KMT_Minus_End_Seeds, "Home")
              callModule(A_MT_Bridging, "Home")
            }
          }

          if (input$`Home-KMT_number` == TRUE) {
            callModule(A_KMT_number, "Home")
          }

          if (input$`Home-IKD` == TRUE) {
            callModule(A_IKD, "Home")
          }

          if (input$`Home-Curvature` == TRUE) {
            callModule(A_Curvature, "Home")
          }

          if (input$`Home-End_Morphology` == TRUE) {
            callModule(A_End_Morphology, "Home")
          }

          if (input$`Home-Fiber_Area` == TRUE) {
            callModule(A_Fiber_Area, "Home")
          }

          if (input$`Home-Fiber_Curv_Length` == TRUE) {
            callModule(A_Fiber_Length_Curv, "Home")
          }

          if (input$`Home-KMT_Minus_End_Seeds` == TRUE) {
            callModule(A_KMT_Minus_End_Seeds, "Home")
          }

          if (input$`Home-k_core_area` == TRUE) {
            callModule(A_K_Core_Area, "Home")
          }

          if (input$`Home-KMT_Torque` == TRUE) {
            callModule(A_KMT_Torque, "Home")
          }
          if (input$`Home-MT_Interaction` == TRUE) {
            callModule(A_MT_Bridging, "Home")
          }

          callModule(Save_Data, "Home")
        }

        showTab(inputId = "innavbar-GS", target = "Report")
        updateTabsetPanel(session, "innavbar", selected = "Report")

        File_name <<- as.data.frame(ls(pattern = "Data_", envir = .GlobalEnv))
        NUM_FILES <<- readr::parse_number(File_name[nrow(File_name), 1])

        df <- data.frame()

        for (i in 1:nrow(File_name)) {
          name <- as.data.frame(str_split(File_name[i, 1], "_"))
          df[i, 1] <- as.numeric(name[2, 1])
          name <- as.data.frame(str_split(
            File_name[i, 1],
            paste("Data_", df[i, 1], "_", sep = "")
          ))
          df[i, 2] <- as.character(name[2, 1])
        }

        File_name <<- na.omit(df)

        rm(df, name)
      })

      # Download data-set ------------------------------------------------------
      output$`Home-Download_Button` <- renderUI({
        downloadBttn("downloadData",
          label = "Download",
          style = "material-flat", color = "success"
        )
      })

      # Collect information to start a plot after analysis ---------------------
      lapply(1:NUM_FILES, function(i) {
        observeEvent(input[[paste("Data_label", i, sep = "_")]], {
          assign(paste("Data_label", i, sep = "_"),
            input[[paste("Data_label", i, sep = "_")]],
            envir = .GlobalEnv
          )
        })

        observeEvent(input[[paste("Data_color", i, sep = "_")]], {
          assign(paste("Data_color", i, sep = "_"),
            input[[paste("Data_color", i, sep = "_")]],
            envir = .GlobalEnv
          )
        })

        observeEvent(input[[paste("Data_bin", i, sep = "_")]], {
          assign(paste("Data_bin", i, sep = "_"),
            input[[paste("Data_bin", i, sep = "_")]],
            envir = .GlobalEnv
          )
        })
      })

      callModule(Report_Plot, "Home")
    } else {
      tryCatch(
        {
          Amira_df <<- as_tibble(readLines("tests/ASGA_Test_Data_Set.am"))
        },
        error = function(e) {}
      )
      tryCatch(
        {
          Amira_df <<- as_tibble(readLines("R/tests/ASGA_Test_Data_Set.am"))
        },
        warning = function(w) {},
        error = function(e) {}
      )
      names(Amira_df)[1] <<- "X1"

      Nodes <<- read_excel(
        "tests/ASGA_Test_Data_Set.xlsx",
        sheet = "Nodes"
      )
      Nodes <<- Load_Amira_Nodes()

      Segments <<- read_excel(
        "tests/ASGA_Test_Data_Set.xlsx",
        sheet = "Segments"
      )
      Segments <<- Load_Amira_Segments()

      Points <<- read_excel(
        "tests/ASGA_Test_Data_Set.xlsx",
        sheet = "Points"
      )
      Points <<- Load_Amira_Points()

      callModule(Load_Data, "Home")
      callModule(Test_Functions, "Home")
      callModule(Test_Output, "Home")

      updateTabsetPanel(session, "innavbar-GS", selected = "UploadData")
      callModule(Test_Result, "Home")

      setwd("Data/")
      Files <<- list.files(path = getwd(), pattern = ".xlsx$")
      file.remove(Files)
      setwd("../")

      if (Test_df == TRUE) {
        rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
      }

      TEST <<- FALSE
      source("global.r")
    }
  })

  # Report page output ---------------------------------------------------------
  output$`Home-Plot_Settings` <- renderUI({
    Report_Plot_Settings("Report")
  })

  output$`Home-Report_Page` <- renderUI({
    tagList(
      if (length(File_name[File_name$V2 == "KMT_No", 2]) >= 1) {
        tagList(
          tags$p(class = "splash-subhead-Report", "KMTs number per kinetochore"),
          Report_Plot_KMT_No("Report")
        )
      },
      if (length(File_name[File_name$V2 == "LD", 2]) >= 1) {
        tagList(
          tags$p(class = "splash-subhead-Report", "KMT length distribution"),
          Report_Plot_LD("Report"),
          Report_Plot_LD2("Report")
        )
      },
      if (length(File_name[File_name$V2 == "IKD", 2]) >= 1) {
        tagList(
          tags$p(class = "splash-subhead-Report", "Inter-kinetochore distance"),
          Report_Plot_IKD("Report")
        )
      }
    )
  })

  # Refresh for the Report page ------------------------------------------------
  observeEvent(input$Refresh, {
    callModule(Report_Plot, "Home")
  })
}
