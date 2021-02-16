---
title: Installation page
authors: Robert Kiewisz
date: 2021-02-15
---

# **Installation**
## Online usage
The limited functionality of the ASGA is available on online platform [`shinyapps.io`](https://kiewisz.shinyapps.io/ASGA/).

The online version is limited to work on the files of approximate size ~10 mb. To analyze spatial graph of bigger size please use offline version.

## Offline usage 
### Installation with Github
```
install.packages("remotes")
remotes::install_github("RRobert92/ASGA")
```
The ASGA is supplied with library/dependency management `renv`. For ensuring smooth start rung `renv::restore()` to install all needed dependency.
