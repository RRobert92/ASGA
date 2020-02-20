## Used output from Kinetochore_XYZ function (Kinetochore_Pole1, Kineetochore_Pole2)
## x variable is fixed Kinetochore_Pole1
Inter_Kinetochore_Dist <- function(){
  
  total <- length(which(colnames(Segments) == "Pole1_00") : which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole1")))[length(colnames(Segments %>% select(starts_with("Pole1"))))]))
  pb <- winProgressBar(min = 2,
                       max =  total,
                       width = 420)
  
  Inter_Kinetochore_Distance <- data.frame()
  
  ## Get distance to each kinetochore on the oposit pole
  for(i in which(colnames(Segments) == "Pole1_00") : which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole1")))[length(colnames(Segments %>% select(starts_with("Pole1"))))])){
    tryCatch({
       DF_Pole1 <- colnames(Segments)[i]
       DF <- data.frame(str_split(gsub("[^[:digit:]]", "Pole_1", DF_Pole1), pattern = "Pole_1"))
       DF_Pole2 <- paste("Pole2", DF[6,1], sep = "_")
       
       Plus_end_1 <- data.frame()
     for (j in 1:nrow(get(DF_Pole1))) {
       
       Plus_end_1[j,1:3] <- get(paste(DF_Pole1, 
                                    j, 
                                    sep = "_"))[1,2:4]
    }
    Plus_end_1 <- data.frame(X_Median = c(median(as.matrix(Plus_end_1[1]))),
                             Y_Median = c(median(as.matrix(Plus_end_1[2]))),
                             Z_Median = c(median(as.matrix(Plus_end_1[3]))))
       
    Plus_end_2 <- data.frame()
    for (j in 1:nrow(get(DF_Pole2))) {
      
      Plus_end_2[j,1:3] <- get(paste(DF_Pole2, 
                                     j, 
                                     sep = "_"))[1,2:4]
    }
    Plus_end_2 <- data.frame(X_Median = c(median(as.matrix(Plus_end_2[1]))),
                             Y_Median = c(median(as.matrix(Plus_end_2[2]))),
                             Z_Median = c(median(as.matrix(Plus_end_2[3]))))
    
      
    
    ## Calculate distance to each oposit kinetochore and save smales distance
    positions <- sqrt((Plus_end_1[1,1] - Plus_end_2[1,1])^2 + (Plus_end_1[1,2] - Plus_end_2[1,2])^2 + (Plus_end_1[1,3] - Plus_end_2[1,3])^2)
    Inter_Kinetochore_Distance[i,1] <- round(positions, 5)
 
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
  DF <- Inter_Kinetochore_Distance
  na.omit(DF)
}
