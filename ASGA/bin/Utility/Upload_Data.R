################################################################################
# Module Upload_Data
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-17
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
################################################################################

# Upload Data  -----------------------------------------------------------------
Getfiles_Server <- function(input, output, session) {
  observeEvent(input$file, {
    infile <- input$file
    if (is.null(infile)) {
      return(NULL)
    } else {
      numfiles <<- nrow(infile)
    }

    # Load Data  ---------------------------------------------------------------
    progressSweetAlert(
      session = session, id = "LoadData",
      title = "Loading your data",
      display_pct = TRUE,
      value = 0
    )

    for (i in 1:numfiles) {
      if (str_detect(input$file$datapath[i], ".xlsx")) {
        tryCatch(
          {
            Data <- read_excel(input$file$datapath[i], sheet = "Nodes")
            assign(paste("Data", "Nodes", i, sep = "_"),
              Data,
              envir = .GlobalEnv
            )
          },
          error = function(e) {}
        )

        tryCatch(
          {
            Data <- read_excel(input$file$datapath[i], sheet = "Points")
            assign(paste("Data", "Points", i, sep = "_"),
              Data,
              envir = .GlobalEnv
            )
          },
          error = function(e) {}
        )

        tryCatch(
          {
            Data <- read_excel(input$file$datapath[i], sheet = "Segments")
            assign(paste("Data", "Segments", i, sep = "_"),
              Data,
              envir = .GlobalEnv
            )
          },
          error = function(e) {}
        )
        Amira <<- FALSE
        
      } else if (str_detect(input$file$datapath[i], ".am")) {
        Amira <<- readLines(input$file$datapath[i])
        updateProgressBar(
          session = session,
          id = "LoadData",
          value = i * 100 / numfiles,
          title = paste("Loading your data:", " Amira file no.", i, " loaded", sep = "")
        )

        Sys.sleep(0.1)
        Amira <<- as_tibble(Amira)
        names(Amira)[1] <<- "X1"

        tryCatch(
          {
            assign(paste("Data", "Nodes", i, sep = "_"),
              Load_Amira_Nodes(),
              envir = .GlobalEnv
            )
            updateProgressBar(
              session = session,
              id = "LoadData",
              value = i * 100 / numfiles,
              title = paste("Loading your data:", " Node file no.", i, " loaded", sep = "")
            )

            Sys.sleep(0.1)

            assign(paste("Data", "Points", i, sep = "_"),
              Load_Amira_Points(),
              envir = .GlobalEnv
            )
            updateProgressBar(
              session = session,
              id = "LoadData",
              value = i * 100 / numfiles,
              title = paste("Loading your data:", " Point file no.", i, " loaded", sep = "")
            )

            Sys.sleep(0.1)

            assign(paste("Data", "Segments", i, sep = "_"),
              Load_Amira_Segments(),
              envir = .GlobalEnv
            )
            updateProgressBar(
              session = session,
              id = "LoadData",
              value = i * 100 / numfiles,
              title = paste("Loading your data:", " Segment file no.", i, " loaded", sep = "")
            )

            Sys.sleep(0.1)

            assign(paste("Amira", "Dataset", i, sep = "_"),
              Amira,
              envir = .GlobalEnv
            )

            Amira <<- TRUE
          },
          error = function(e) {}
        )
      }
      # Check Data  -------------------------------------------------------------
      Check_Data(i)
      updateProgressBar(
        session = session,
        id = "LoadData",
        value = i * 100 / numfiles
      )

      Sys.sleep(0.1)
    }

    closeSweetAlert(session = session)

    if (numfiles == 1) {
      text_dataset <<- "dataset was"
    } else {
      text_dataset <<- "dataset's were"
    }

    if (Amira == TRUE) {
      datatype <<- "Amira ASCII"
    } else {
      datatype <<- "Excel ASCII"
    }

    # Pop-UP windows with Completion/Errors  ----------------------------------------
    callModule(Error_Handler, "Home")

    showTab(inputId = "innavbar-GS", target = "Settings")
  })

  ## Upload of analyzed data sets

  observeEvent(input$file1, {
    infile <- input$file1
    if (is.null(infile)) {
      return(NULL)
    } else {
      numfiles <<- nrow(infile)
    }

    progressSweetAlert(
      session = session,
      id = "LoadData",
      title = "Loading your data",
      display_pct = TRUE, value = 0
    )


    for (i in 1:numfiles) {
      tryCatch(
        {
          File_name <<- stringi::stri_extract_first(str = infile$name, regex = ".*(?=\\.)")
          assign(File_name[i],
            read_excel(input$file1$datapath[i]),
            envir = .GlobalEnv
          )

          assign(File_name[i],
            get(File_name[i])[2:ncol(get(File_name[i]))],
            envir = .GlobalEnv
          )
        },
        error = function(e) {}
      )
      
      Check_Analysis(File_name)

      updateProgressBar(
        session = session,
        id = "LoadData",
        value = i * 100 / numfiles
      )
      Sys.sleep(0.1)
      
      if(AnalysisTest != 1) break
    }
    closeSweetAlert(session = session)
    hideTab(inputId = "innavbar-GS", target = "Settings")
  })
}
