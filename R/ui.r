################################################################################
# Shiny UI-Home
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-19
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
################################################################################

# Shiny UI-Home  ---------------------------------------------------------------
fluidPage(
  includeCSS("www/css/style.css"),

  useShinyalert(),
  useShinyjs(),
  extendShinyjs(text = JS_CODE, functions = 'browseURL'),

  navbarPage(
    title = APP_TITLE,
    collapsible = TRUE,
    inverse = TRUE,
    position = "fixed-top",
    id = "innavbar",
    selected = "Home",
    # footer = footnoteUI("footnote"),
    tabPanel(
      "GetStarted",
      GetStarted_UI("Home")
    ),

    tabPanel(
      "3D Viewer",
      Viewer_UI("Home")
    ),

    tabPanel(
      "Home",
      fluidRow(
        homeUI("Home")
      )
    ),

    tabPanel(
      "Wiki"
    ),

    tabPanel(
      "About"
    )
  )
)
