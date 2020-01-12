## load data
library(readxl)
library(tidyverse)
Segments <- read_excel("Pulpit/Metaphase_1_KMTs.resampled.rotated.2_75.0.-9_55.am.xlsx",
                       sheet = "Segments")
Nodes <- read_excel("Pulpit/Metaphase_1_KMTs.resampled.rotated.2_75.0.-9_55.am.xlsx",
                    sheet = "Nodes")

## select KTMs for one pole
Segments_1 <- Segments %>% filter_at(vars(starts_with("Pole1")), 
                                     any_vars(.>= 1))
Segments_2 <- Segments %>% filter_at(vars(starts_with("Pole2")), 
                                     any_vars(.>= 1))
Nodes <- Nodes %>% select(1,
                          ncol(Nodes)-3, 
                          ncol(Nodes)-2, 
                          ncol(Nodes)-1)
Nodes[2:4] <- Nodes[2:4]/10000
## count n. of columns and selected only needed one + ! end polaritis
ncol<- ncol(Segments_1)
nrow_1 <- seq(from = 1, to = nrow(Segments_1),by = 1)
Segments_1 <- Segments_1 %>% select(1,
                                    ncol-3, 
                                    ncol-2, 
                                    ncol-1,
                                    ncol)
Segments_2 <- Segments_2 %>% select(1,
                                    ncol-3, 
                                    ncol-2, 
                                    ncol-1,
                                    ncol)
nrow_2 <- seq(from = 1, to = nrow(Segments_2),by = 1)

## creat for each fiber list of points
Select_MTs <- function(x, y){
  selected_MTs <- data.frame(Length = c(y[x,2]/10000),
                             Node_1 = c(y[x,3]),
                             Node_2 = c(y[x,4]))
  
}

for (i in nrow_1) {
  name <- paste("Pole1_", i, sep = "")
  assign(name, Select_MTs(i, Segments_1))
}
for (i in nrow_2) {
  name <- paste("Pole2_", i, sep = "")
  assign(name, Select_MTs(i, Segments_2))
}

##MTs curvature
MTs_Curvature <- function(x){
  ##d(p1,p2)=sqrt((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2)
  node_distance <- sqrt((Nodes[x[1,2]+1,2] - Nodes[x[1,3]+1,2])^2 + (Nodes[x[1,2]+1,3] - Nodes[x[1,3]+1,3])^2 + (Nodes[x[1,2]+1,4] - Nodes[x[1,3]+1,4])^2)
  data.frame(Ratio = c(x[1,1] / node_distance),
                            Full_Length = c(x[1,1])) 
}

for (i in nrow_1) {
  name <- paste("Pole1_", i, sep = "")
  assign(name, MTs_Curvature(get(name)))
  assign(name, rename(get(name), c("X.Coord" = "Ratio")))
}
for (i in nrow_2) {
  name <- paste("Pole2_", i, sep = "")
  assign(name, MTs_Curvature(get(name)))
  assign(name, rename(get(name), c("X.Coord" = "Ratio")))
}

##Bined data in one table
Pole1_full <- data.frame()
Pole2_full <- data.frame()
for(i in nrow_1){
  name <- paste("Pole1_", i, sep = "")
  Pole1_full <- rbind.data.frame(Pole1_full, get(name))
}
for(i in nrow_2){
  name <- paste("Pole2_", i, sep = "")
  Pole2_full <- rbind.data.frame(Pole2_full, get(name))
}

##save data as xlsx
library(xlsx) 
write.xlsx(Pole1_full, "Data#1_Pole1_mts_curv.xlsx", row.names = FALSE)
write.xlsx(Pole2_full, "Data#1_Pole2_mts_curv.xlsx", row.names = FALSE)
