#########################################################
# The analysis tool to count inter kinetochore distance #
#########################################################
## x variable in the function is a number of a KMTs in the fiber (e.g. 1, 2, 3... etc.)

## Get a possition of the all kinetochore
Kinetochore_XYZ <- function(x) {
  Plus_end <- data.frame()
  for (i in 1:nrow(get(colnames(Segments)[x]))){
    Plus_end[i,1:3] <- get(paste(colnames(Segments)[x], 
                                 i, 
                                 sep = "_"))[1,2:4]
  }
  Plus_end <- data.frame(X_Median = c(median(as.matrix(Plus_end[1]))),
                         Y_Median = c(median(as.matrix(Plus_end[2]))),
                         Z_Median = c(median(as.matrix(Plus_end[3]))))
}

## Used output from Kinetochore_XYZ function (Kinetochore_Pole1, Kineetochore_Pole2)
## x variable is fixed Kinetochore_Pole1
Inter_Kinetochore_Dist <- function(){
  
  total <- nrow(Kinetochore_Pole1)
  pb <- winProgressBar(min = 0,
                       max =  total,
                       width = 420)
  
  Inter_Kinetochore_Distance <- data.frame()
  
  ## Get distance to each kinetochore on the oposit pole
  for(i in 1:nrow(Kinetochore_Pole1)){
    tryCatch({
       DF <- data.frame()
    
     ## Get oposit pole
      DF[1:nrow(Kinetochore_Pole2),1:3] <- Kinetochore_Pole1[i,1:3]
      positions <- cbind(DF, Kinetochore_Pole2)
    
    ## Calculate distance to each oposit kinetochore and save smales distance
    positions$distance <- apply(positions, 1, function(z) dist(matrix(z, nrow = 2, byrow = TRUE)))
    Inter_Kinetochore_Distance[i,1] <- min(positions$distance)
    
    ## Remove kinetochore wich was found on the oposit pole
    Kinetochore_Pole2 [which.min(positions$distance), 1:3] <- NA
    Kinetochore_Pole2 <- na.omit(Kinetochore_Pole2)
    
    Sys.sleep(0.1)
    setWinProgressBar(pb, i, 
                      title = paste("Finding kinetochore pair and getting distance...",
                                    round((i - 1) / total * 100, 
                                          0), 
                                    "% Done"))
    },
    error = function(e){}
    )
  }
  
  close(pb)
  Inter_Kinetochore_Distance
}