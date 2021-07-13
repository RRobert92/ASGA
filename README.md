<img src="https://github.com/RRobert92/ASGA/blob/main/docs/img/asga%20baner.png?raw=true">

# Open-Source Spindle analysis tool
[![R](https://github.com/RRobert92/ASGA/actions/workflows/r.yml/badge.svg?branch=main)](https://github.com/RRobert92/ASGA/actions/workflows/r.yml)
![GitHub release (latest by date)](https://img.shields.io/github/v/release/RRobert92/MT_Analysis)
![GitHub top language](https://img.shields.io/github/languages/top/RRobert92/MT_Analysis)
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
    * [Installation](#Quick_start_IN)
* [Contributing](#Contributing)
* [Copyright](#Copyright)

<a name="Quick_start"></a>
# Quick startS
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.
You can run the software in an online-version under the [Shinyapp.io](https://kiewisz.shinyapps.io/ASGA/) or localy.

<a name="Quick_start_DP"></a>
### Data preparation befor you start
```
* Clean spatial graph and manual check all microtubules for its correct segmentation
* Create a point for a center of centrioles and label them Pole1 and Pole2
* Reorient spatial graph in a way that Pole1 correspond to the bottom of a spindle and Pole 2 to the top
* Rotate spatial graph to align labels Pole1 and Pole2 their position in X and Y
* Label selected classes of MT. For example: Pole1_00, Pole1_01, etc.
* Using Amira calculate MT length
```

<a name="Dependency"></a>
### Dependency for local use
```
R v3.5.3 or newer
Rstudio v1.2 or newer
Java SE 11 (LTS)

R library
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

<a name="Quick_start_IN"></a>
### Installation
```
install.packages("remotes")
remotes::install_github("RRobert92/ASGA")

library(shiny)
runApp("ASGA")
```
For portable ASGA application:
```
git clone https://github.com/RRobert92/ASGA
cd ASGA

run Distribution_Creat.ps1
```

<a name="Contributing"></a>
# Contributing
Contributions, collaborators and/or constructive criticism are welcome! Please see our Contributing Guide "Soon available" for more details.

<a href="https://sourcerer.io/rrobert92"><img src="https://avatars0.githubusercontent.com/u/56911280?v=4" height="50px" width="50px" alt=""/></a>
<a name="Copyright"></a>
## Copyright
This project is distributed under the General Public License (GPL) version 3.0 - see the [LICENSE.md](LICENSE.md) file for details.

## Citation
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3732108.svg)](https://doi.org/10.5281/zenodo.3732108)

Kiewisz R. *Spindle analysis tool* (2020) [DOI: 10.5281/zenodo.3732108](https://doi.org/10.5281/zenodo.3732108)
