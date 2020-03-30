##################################################################
# The core tool to sort MT points based on their (+) and (-) end #
##################################################################
## y variablein in the function is a DF name with points assinged to the MT (e.g. Pole1_01_1)

## Function to work with Pole_1

Sort_by_distance_to_pole1 <- function(y){
  position <- data.frame(x = c(Pole1[1,1], Pole1[1,1]),
                         y = c(Pole1[1,2], Pole1[1,2]),
                         z = c(Pole1[1,3], Pole1[1,3]),
                         x1 = c(y[1,2], y[nrow(y),2]),
                         y1 = c(y[1,3], y[nrow(y),3]),
                         z1 = c(y[1,4], y[nrow(y),4]))
  
  position$distance <- apply(position, 1, function(z) dist(matrix(z, nrow = 2, byrow = TRUE)))
  
  if(position[1,7] < position[2,7]){
    y %>% arrange(desc(Point_ID))
  } else {
    y
  }
}

## Function to work with Pole_2

Sort_by_distance_to_pole2 <- function(y){
  position <- data.frame(x = c(Pole2[1,1], Pole2[1,1]),
                         y = c(Pole2[1,2], Pole2[1,2]),
                         z = c(Pole2[1,3], Pole2[1,3]),
                         x1 = c(y[1,2], y[nrow(y),2]),
                         y1 = c(y[1,3], y[nrow(y),3]),
                         z1 = c(y[1,4], y[nrow(y),4]))
  
  position$distance <- apply(position, 1, function(z) dist(matrix(z, nrow = 2, byrow = TRUE)))
  
  if(position[1,7] < position[2,7]){
    y %>% arrange(desc(Point_ID))
  } else {
    y
  }
}