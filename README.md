# Open-Source Spindle analysis tool
![GitHub release (latest by date)](https://img.shields.io/github/v/release/RRobert92/MT_Analysis)
![GitHub top language](https://img.shields.io/github/languages/top/RRobert92/MT_Analysis)
![GitHub](https://img.shields.io/github/license/RRobert92/MT_Analysis)
![GitHub Release Date](https://img.shields.io/github/release-date/RRobert92/MT_Analysis)
![GitHub contributors](https://img.shields.io/github/contributors/RRobert92/MT_Analysis)
![DOI](https://img.shields.io/badge/DOI-Avaiable-green)

**Spindle analysis tool** is an open-source cluster of tools with its ongoing development for a meiotic and mitotic spindle analysis. Key futures:
  
1. Simple and intuitive to use with implemented UI. Only require installation of the R software environment and optional Rstudio.

2. It is designed from the ground up to incrementally scale up this tool based on evolving use cases. 

3. Allows working on projects reproducibly and compare data.

**Spindle analysis tool** aims to replace huge and complicated to work with a spreadsheet that is frequently used for Spatial Graph analysis, but also it allows to uniform and completely automate analysis.

# Table of Contents

* [Quick start](#Quick_start)
  * [Dependency](#Dependency)
  * [Data Preparation](#Quick_start_DP)
  * [Installation](#Quick_start_IN)
* [Contributing](#Contributing)
* [Copyright](#Copyright)
  
<a name="Quick_start"></a>
# Quick start
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

<a name="Dependency"></a>
### Dependency
```
R 3.5.3 or newer
Rstudio 1.3.929 or newer
Java SE 11 (LTS) or newer
```
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
<a name="Quick_start_IN"></a>
### Installation

<a name="Contributing"></a>
# Contributing
Contributions, collaborators and/or constructive criticism are welcome! Please see our Contributing Guide "Soon available" for more details.

<a href="https://sourcerer.io/rrobert92"><img src="https://avatars0.githubusercontent.com/u/56911280?v=4" height="50px" width="50px" alt=""/></a>

<a name="Copyright"></a>
## Copyright
This project is distributed under the General Public License (GPL) version 3.0 - see the [LICENSE.md](LICENSE.md) file for details.

## Citation
[![DOI](https://zenodo.org/badge/216998366.svg)](https://zenodo.org/badge/latestdoi/216998366)

Kiewisz R. *Spindle analysis tool V1.1* (2020) [DOI: 10.5281/zenodo.3756981](https://doi.org/10.5281/zenodo.3756981)
