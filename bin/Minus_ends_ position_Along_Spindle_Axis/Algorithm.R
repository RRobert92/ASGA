##  |(y2 - y1)x0 - (x2 - x1)y0 + x2y1 - y2x1|
## _________________________________________
##     sqrt((y2 - y1)^2 + (x2 - x1_^2))

## Load data
library(readxl)
library(tidyverse)
Segments <- read_excel("Pulpit/Metaphase_1_KMTs.xlsx", 
                       sheet = "Segments")
Nodes <- read_excel("Pulpit/Metaphase_1_KMTs.xlsx", 
                     sheet = "Nodes")

## Select all KMTs
##define Pole1 and Pole2 position in um
Pole1 <- data.frame(X = c(3.63459), Y = c(9.58781), Z = c(2.99921))
Pole2 <- data.frame(X = c(5.03459), Y = c(2.58781), Z = c(2.79921))

## count n. of columns and selected only needed one + ! end polaritis
ncol_seg <- ncol(Segments_1)
nrow_1 <- seq(from = 1, to = nrow(Segments_1),by = 1)
nrow_2 <- seq(from = 1, to = nrow(Segments_1),by = 1)
Segments_1 <- Segments_1 %>% select(1, 
                                    ncol_seg - 2, 
                                    ncol_seg - 1)
Segments_2 <- Segments_2 %>% select(1, 
                                    ncol_seg - 2, 
                                    ncol_seg - 1)

## extract xyz coord for each nodes in um
ncol_nodes <- ncol(Nodes)
Nodes <- Nodes %>% select(1,
                          ncol_nodes - 3,
                          ncol_nodes - 2,
                          ncol_nodes - 1)

## Select only minus ends

## calculate distance of the minus end to the PoleToPole_axis

## with pitagoras calculate position on a POleToPOle_axis 

## Normalized all data between 0 and 1 
