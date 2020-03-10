# The core tool to sort MT points based on their (+) and (-) end #
##################################################################
## y variablein in the function is a DF name with points assinged to the MT (e.g. Pole1_01_1)

## Function to work with Pole_1
## Relative position of point between kinetochore and pole ( 1 - 0 ) for Pole_1
##  (n - min)  part_1  where n is XYZ position of the point, where min is XYZ position of the Pole1
## __________
## (max - min) part_2  where max is XYZ position of the Kinetochore, where min is XYZ position of the Pole1

##X - longest KMTs in fiber 
##Y - is POleX_YY_ZZ
relativ_pos_1 <- function(x, y){
  
  relativ_pos_part1 <- lapply(y[3], 
                              function(z){y[3] - Pole1[1,2]})
  relativ_pos_part1 <- data.frame(relativ_pos_part1[["Y Coord"]][["Y Coord"]])
  
  relativ_pos_part2 <- x[1,3] - Pole1[1,2]
  relativ_positon <- lapply(relativ_pos_part1, 
                            function(z){round(relativ_pos_part1[1] / relativ_pos_part2, 2)})
  
  relat_pos <- data.frame(relativ_positon[["relativ_pos_part1...Y.Coord......Y.Coord..."]][["relativ_pos_part1...Y.Coord......Y.Coord..."]])
  names(relat_pos)[1] <- "Relative_Position"
  cbind(y, relat_pos)
}

## Relative position of point between kinetochore and pole ( 1 - 0 ) for Pole_2
##  (n - min)  part_1  where n is XYZ position of the point, where min is XYZ position of the Pole2
## __________
## (max - min) part_2  where max is XYZ position of the Kinetochore, where min is XYZ position of the Pole2

##X - longest KMTs in fiber 
##Y - is POleX_YY_ZZ
relativ_pos_2 <- function(x, y){
  relativ_pos_part1 <- lapply(y[3], 
                              function(z){y[3] - Pole2[1,2]})
  relativ_pos_part1 <- data.frame(relativ_pos_part1[["Y Coord"]][["Y Coord"]])
  
  relativ_pos_part2 <- x[1,3] - Pole2[1,2]
  relativ_positon <- lapply(relativ_pos_part1, 
                            function(z){round(relativ_pos_part1[1] / relativ_pos_part2, 2)})
  
  relat_pos <- data.frame(relativ_positon[["relativ_pos_part1...Y.Coord......Y.Coord..."]][["relativ_pos_part1...Y.Coord......Y.Coord..."]])
  names(relat_pos)[1] <- "Relative_Position"
  cbind(y, relat_pos)
}