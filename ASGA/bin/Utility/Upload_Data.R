################################################################################
# Module Upload_Data
#
# Author: Robert Kiewisz
# Created: 2020-05-17
################################################################################


# Upload Data  -----------------------------------------------------------------
Getfiles_Server <- function (input, output, session){
  
  observeEvent(input$file,{
    infile <- input$file
    if (is.null(infile)){
      return(NULL)
    }else {
      numfiles <<- nrow(infile)
    }
    
    # Load Data  ---------------------------------------------------------------
    progressSweetAlert(
      session = session, id = "LoadData",
      title = "Loading your data",
      display_pct = TRUE, value = 0
    )
    
    for(i in 1:numfiles){
      tryCatch({
        Data <-  read_excel(input$file$datapath[i], sheet = "Nodes")
        assign(paste("Data", "Nodes", i ,sep = "_"), 
               Data,
               envir=.GlobalEnv) 
      }, error = function(e){})
      tryCatch({
        Data <-  read_excel(input$file$datapath[i], sheet = "Points")
        assign(paste("Data", "Points", i ,sep = "_"), 
               Data,
               envir=.GlobalEnv)
      }, error = function(e){})
      tryCatch({
        Data <-  read_excel(input$file$datapath[i], sheet = "Segments")
        assign(paste("Data", "Segments", i ,sep = "_"), 
               Data,
               envir=.GlobalEnv)       
      }, error = function(e){})
      
      # Check Data  -------------------------------------------------------------
      CheckData(i)
      updateProgressBar(
        session = session,
        id = "LoadData",
        value = i*100/numfiles
      )
      
      Sys.sleep(0.1)
    }
    closeSweetAlert(session = session)
    
    
    if(numfiles == 1){
      text <- "dataset"
    } else {
      text <- "dataset's"
    }
    
# Pop-UP windows with Completion/Errors  ----------------------------------------
    if(DataTest == 1){
      showTab(inputId = "innavbar-GS", target = "Settings")
      sendSweetAlert(
        session = session,
        title = "The data structure looks great!",
        text = paste("A", numfiles, text, "were successfuly upload!", "
                     Press Ok to analyze your awesome data!",
                     sep = " "),
        type = "success",
        btn_labels = "OK",
        btn_colors = "#39B855",
        closeOnClickOutside = TRUE
      )
      
    }else if (DataTest == 2){
      sendSweetAlert(
        session = session,
        title ="Looks like you there is a proble with your data",
        text = "The labeling in the 'segments' excel sheet should start with Pole1_00, not Pole2_00.
                Please check it with the guidelines and try again.",
        type = "error",
        btn_labels = "OK",
        btn_colors = "#C95050",
        closeOnClickOutside = TRUE
      )
    }else if (DataTest == 3){
      sendSweetAlert(
        session = session,
        title ="Looks like you there is a proble with your data",
        text ="The 'segments' data structure looks strange! 'Segment ID', 'Point IDs', or 'length' are missing or are in the wrong order...
               Please check it with the guidelines and try again.",
        type = "error",
        btn_colors = "#C95050",
        btn_labels = "OK",
        closeOnClickOutside = TRUE
      )
    } else if (DataTest == 4){
      sendSweetAlert(
        session = session,
        title ="Looks like you there is a proble with your data",
        text ="The labeling in the 'Nodes' excel sheet missing information about Pole1...
                Please check it with the guidelines and try again.",
        type = "error",
        btn_colors = "#C95050",
        btn_labels = "OK",
        closeOnClickOutside = TRUE
      )
    } else if (DataTest == 5){
      sendSweetAlert(
        session = session,
        title ="Looks like you there is a proble with your data",
        text ="The labeling in the 'Nodes' excel sheet missing information about Pole2...
                Please check it with the guidelines and try again.",
        type = "error",
        btn_colors = "#C95050",
        btn_labels = "OK",
        closeOnClickOutside = TRUE
      )
    } else if (DataTest == 6){
      sendSweetAlert(
        session = session,
        title ="Looks like you there is a proble with your data",
        text ="Could not find any 'Poles' coordinates in the Nodes excel sheet! Please check it with the guidelines and try again.",
        type = "error",
        btn_colors = "#C95050",
        btn_labels = "OK",
        closeOnClickOutside = TRUE
      )
    } else if (DataTest == 7){
      sendSweetAlert(
        session = session,
        title ="Looks like you there is a proble with your data",
        text ="The data structure is not compatible at all. Did you try to load a wrong file?
        Please check it with the guidelines and try again.",
        type = "error",
        btn_colors = "#C95050",
        btn_labels = "OK",
        closeOnClickOutside = TRUE
      )
    } else if (DataTest == 0){
      sendSweetAlert(
        session = session,
        title ="Looks like you try to upload a wrong file",
        text ="Please check it with the guidelines and try again.",
        type = "error",
        btn_colors = "#C95050",
        btn_labels = "OK",
        closeOnClickOutside = TRUE
      )
    }
  })
  
}