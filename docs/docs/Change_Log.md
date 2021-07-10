---
title: ChangLog Page
authors: Robert Kiewisz
date: 2021-01-14
---
<a name="Change_Log"></a>
<center>
#Change Log
</center>

---
## v0.36 - Published 10/07/2021
---

`Major changes`
- Maintenance update to calculate non-KMT ends type


---
## v0.34.1 - Published 10/03/2021
---

`Major changes`
- Maintenance update
- Introduced desktop app for Win x64, Linux and mac with electron-packager

---
## v0.34 - Published 01/03/2021
---

`Major changes`

- Restructure MT interaction functions
    - Improved performance of the calculation
    - Added option to run the calculation as a function of distance
- Added export of curvature data into Amira file
- Added function to calculate SMTs minus-end distance
- Restructured Analysis folder and split function for KMTs/SMTs and Stat
- New tool to calculate position and interaction of KMT minus-ends with other MTs

`Minor changes`

- Added Menger curvature
- Error handler for analyzed data when uploaded
- Added selection for local curvature by user
- Added unified test protocol for all tools

`Bug Fix`

- Bugfixes for Amira output

`Other`

- Update library
- small responsiveness improvement of the main page when user upload 
  the wrong file
- Added ASGA documentation 
- Added a script generating plots for eLife paper
- Finished ASGA documentation

---
## v0.33 - Published 28/12/2020
---

`Major changes`

- New tool for MT interaction
- Introduced new file format (.am) for readout and save analysis to read in
  [Amira ZIB](https://amira.zib.de/)
- Update `R` to 4.0.3 and packages

`Minor changes`

- Change output in the `End_morphology` to show the name of the fiber not column no.

`Bug Fix`

- Bugfix in the button reactivity, replaced  all `shinyAlert` to `sweetAlert`
- Bugfix in `check_Data` utility when displaying error type 
- Bugfix in `fiber_area` when using amira file
- Bugfix for `upload_data` when using amira file
- Bugfix for `saving_amira` and loading amira files
- Bugfix with missing Amira variable shown in `Test_Unit`

`Other`

- Added parallel processing library (`parallel`)
- Created R Shiny template v1.0
- Maintenance update and cleaned code
- Remove deprecated `as.tibble()` from loading amira files

---
## v0.32 - Published 30/08/2020 
---

`Major changes`

- Code reviewed and standardized according to the tidyverse guidelines
- Added `renv` library management 
- Added custom test unit

`Minor changes`

- Added node_id as an output of `end_morpology`

`Bug Fix`

- Review entire code (v0.31.1)

`Other`

- added ASGA logo

---
## v0.31.1 - Published 17/07/2020
---

`Major changes`

- Added Torque and helicity tools


`Bug Fix`

- Fixed issue with end morphology
- Hot Fixes in a pre analysis 

---
## v0.31 - Published 09/07/2020
---

`Mejor changes`

- Added upload for the analyzed data for plotting
- Added analysis report page and its responsiveness 
- Allows to change names and color of a graphs
- Added tool for length and curvature of a fiber
- Added tool for calculation of a kinetochore area and it's position

`Minor changes`

- Changed style of the Download button

`Bug Fix`

- Adjustment in a CSS style for different screen resolutions

`Other`

- Created ASGA_dev for console based ASGA used for testing new tools
- Change threshold for the ellipse model
- Added plotting for the Ellipse, plus end and unified model

---
##  v0.30.1 - Published 24/05/2020
---

`Major changes`

- Moved loading of a data from Upload to separate function
- Upload and analysis of multiple data is now supported
- Added module for saving data
- Added module for exporting and downloading data

`Minor changes`

- Title of an app is now a variable
- Added small progress bar if more then one data set was upload to indicate which 
  data set is currently analyzed

`Bug Fix`

- Fixed analysis of the end morphology 

`Other`

- Started documentation of changes (a.k.a. Change Log)

---
## v0.30 - Published 18/05/2020
---

`Major changes`

- Added all tools from the local module
- Cleaned CSS
- Added fully functional button to the set-up settings
