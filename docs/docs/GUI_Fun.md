---
title: GUI
authors: Robert Kiewisz
date: 2021-02-15
---

  <style>
  th {
    display: none;
  }
</style>

#**ASGA GUI**

  ***Version***&nbsp;&nbsp;                      {{ version }} <br/>
  ***Data***&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; {{ Data }} <br/>
  ***Author***&nbsp;&nbsp;&nbsp;                 {{ Author }} <br/>
  ***Depends***                                  `shiny` `shinyjs` `shinycssloaders` `shinyWidgets` `shinyBS` `shinyalert` `colourpicker` `ggplot2` `ggbeeswarm` `Hmisc` `egg`<br/>
  ***License***&nbsp;&nbsp;&nbsp;                {{ License }}<br/><br/>
  
  ***Description*** Collection of GUI modules responsible for button and messaging handling
  
<br/>
<a name="Error_Handler"></a>
<hr /><center>
###[`Error_Handler`](https://github.com/RRobert92/ASGA/blob/main/ASGA/bin/Utility/Error_Messages.R)
<hr /></center>

####***Description***<br/>

ASGA GUI module utilizing `sendSweetAlert` library for sending error messages. <br/>

####***Usage***<br/>
```
callModule(Error_Handler, "Home")
```

####***Details***<br/>

The `Error_Handler` module read out the `DATA_TEST` test output value from `Check_Data` module.

####***Value***<br/>

|               |                                                              |
|---------------|--------------------------------------------------------------|
`DATA_TEST` == 1| Data set are valid and comply with ASGA standard
`DATA_TEST` > 1 | Data set does not comply with ASGA standard. The number indicates error type.

<br/><br/>
<a name="Setting_Buttons"></a>
<hr /><center>
###[`Setting_Buttons`](https://github.com/RRobert92/ASGA/blob/main/ASGA/bin/Utility/Setting_Buttons.R)
<hr /></center>

####***Description*** <br/>
The ASGA GUI module containing a set of buttons used for selection by a user which analysis should be run.

####***Usage***<br/>
```
# Setting_Buttons_UI()
<ASGA/www/Get_Started/index.R>

          fluidRow(
            Setting_Buttons_UI("Home"),
            column(8, uiOutput(ns("Tool_Info_1")))
          )

# Setting_Buttons_Server()
<server.R>

callModule(Setting_Buttons_Server, "Home")
```
<br/>

####***Example***<br/>

```
# Setting_Buttons_UI()

materialSwitch(
      inputId = ns("Curvature"),      # Button ID
      label = "KMT curvature",        # Button text visible in GUI
      value = FALSE,                  # Is active from start
      right = TRUE,                   # Button label on the right side
      status = "info"                 # Button color "info" - Blue
    )
```

```
# Setting_Buttons_Server()

  observeEvent(input$`Curvature`, {    # Button action
    All_Closed()                        # Button behavior when all buttons are off
    Any_One()                           # Button behavior at least one is on

    Sys.sleep(0.1)                      # Wait 0.1s for more smooth action

    output$`Tool_Info_1` <- renderUI({ # Button description
      if (input$`Curvature` == TRUE) {
        "Tool description"              # Desctiption of a tool shown when button is activated
      }
    })
    
    if (input$`Curvature` == TRUE) {    # Messege shown when the button is press (not obligatory)
      inputSweetAlert(
        session = session,
        type = "info",
        inputId = "Curvature_config",
        input = "text",
        title = "Set-up analysis parameter",
        text = "Bin size used to calculate local curvature every specified distance on the spindle pole axis. Unit [nm]"
      )
    }
    
    observeEvent(input[["Curvature_config"]], { # Collect user input if new constant was specified
      assign("CURVATURE_CONFIG",                # Constant name
             round(as.numeric(input[["Curvature_config"]]) / 20, 0) - 1, # The value to be saves
             envir = .GlobalEnv
      )
    })
  })
```

<br/><br/>
<a name="Report"></a>
<hr /><center>
###[`Report`](https://github.com/RRobert92/ASGA/blob/main/ASGA/bin/Utility/Report.R)
<hr /></center>

####***Description*** <br/>
The ASGA GUI module to display after analysis set of standard plots for data validation.
