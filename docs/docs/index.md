---
title: Main Page
authors: Robert Kiewisz
date: 2021-01-14
---

<img align="center" src="https://github.com/RRobert92/ASGA/blob/main/docs/img/asga%20baner.png?raw=true">


# Open-Source Spindle analysis tool
[![R](https://github.com/RRobert92/ASGA/actions/workflows/r.yml/badge.svg?branch=main)](https://github.com/RRobert92/ASGA/actions/workflows/r.yml)
![GitHub release (latest by date)](https://img.shields.io/github/v/release/RRobert92/MT_Analysis)
![GitHub](https://img.shields.io/github/license/RRobert92/MT_Analysis)
![GitHub Release Date](https://img.shields.io/github/release-date/RRobert92/MT_Analysis)
![GitHub contributors](https://img.shields.io/github/contributors/RRobert92/MT_Analysis)
![Status](https://img.shields.io/badge/lifecycle-maturating-blue.svg)
[![CodeFactor](https://www.codefactor.io/repository/github/rrobert92/asga/badge)](https://www.codefactor.io/repository/github/rrobert92/asga)

**Spindle analysis tool** is an open-source cluster of tools with its ongoing development for meiotic and mitotic spindle analysis. Key futures:
  
1. Online access

2. Simple and intuitive to use with implemented UI. Only require installation of the R software environment and optional Rstudio.

3. It is designed from the ground up to incrementally scale up this tool based on evolving use cases. 

4. Allows working on projects reproducibly and compare data.

**Spindle analysis tool** aims to replace huge and complicated to work with a spreadsheet that is frequently used for Spatial Graph analysis, but also it allows to uniform and completely automate analysis.

# Table of Contents

* [Quick start](#Quick_start)
  * [Data Preparation](#Quick_start_DP)
  * [Dependency](#Dependency)
* [Contributing](#Contributing)
* [Copyright](#Copyright)

<a name="Quick_start"></a>
# Quick start
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.
You can run the software in an online-version under the [Shinyapp.io](https://kiewisz.shinyapps.io/ASGA/) or localy.

<a name="Quick_start_DP"></a>
### Data preparation befor you start

* Clean spatial graph and manual check all microtubules for its correct segmentation

* Create a node for a center of centrioles and label them Pole1 and Pole2

* Rotate spatial graph to align spindle in X and Y

* Reorient spatial graph in a way that Pole1 correspond to the bottom of a spindle and Pole 2 to the top

* Label selected classes of MT. For example: Pole1_00, Pole1_01, etc.

* Using Amira calculate MT length

* Export Amira file as Amira ASCII (.am format) or XML (.xml format depreceated)

* ! if XML format is used .xml file have to be open in excel and saved as .xlsx


<a name="Dependency"></a>
### Dependency for local use
```
R v3.5.3 or newer
Rstudio IDE v1.2 or newer
Java SE 11 (LTS)
```
### Dependency R library
```
- shiny
- shinycssloaders
- shinyWidgets
- shinyBS
- shinyalert
- colourpicker
- readxl
- plyr
- tidyverse
- ggplot2
- egg
- base
- alphashape3d
- xlsx
- zip
```

<a name="Contributing"></a>
# Contributing
Contributions, collaborators and/or constructive criticism are welcome! Please see our Contributing Guide "Soon available" for more details.

<a href="https://sourcerer.io/rrobert92"><img src="https://avatars0.githubusercontent.com/u/56911280?v=4" height="50px" width="50px" alt=""/></a>
<a href="https://github.com/gunar-f"><img src="https://avatars0.githubusercontent.com/u/70518136?s=400&v=4" height="50px" width="50px" alt=""/></a>
<a name="Copyright"></a>

## Copyright
This project is distributed under the General Public License (GPL) version 3.0 - see the [license.md](license.md) file for details.
