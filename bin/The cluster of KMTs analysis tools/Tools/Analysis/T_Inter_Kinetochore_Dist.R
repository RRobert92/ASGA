######################################################################################################
# The analysis tool extract distance between sister kinetochore and number KMTs on this kinetochores #
######################################################################################################

## Count the inter-kinetochore distance
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
  names(DF)[1] <- "Inter-kinetochore distance"
  na.omit(DF)
}

## Count no of KMTs between sister kinetochores
Compare_KMTs_no_for_sister <- function(){
  total <- length(which(colnames(Segments) == "Pole1_00") : which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole1")))[length(colnames(Segments %>% select(starts_with("Pole1"))))]))
  pb <- winProgressBar(min = 2,
                       max =  total,
                       width = 420)
  
  KMTs_at_Pole1 <- data.frame()
  KMTs_at_Pole2 <- data.frame()
  
  for(i in which(colnames(Segments) == "Pole1_00") : which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole1")))[length(colnames(Segments %>% select(starts_with("Pole1"))))])){
    tryCatch({
      DF_Pole1 <- colnames(Segments)[i]
      DF <- data.frame(str_split(gsub("[^[:digit:]]", "Pole_1", DF_Pole1), pattern = "Pole_1"))
      DF_Pole2 <- paste("Pole2", DF[6,1], sep = "_")
      
      KMT_end_1 <- nrow(get(DF_Pole1))
      KMT_end_2 <- nrow(get(DF_Pole2))
      
      if(KMT_end_1 == 0 | KMT_end_2 == 0){
        KMT_end_1 <- NA
        KMT_end_2 <- NA
      } else {}
      
      ## No. of KMTs at each kinetochore
      KMTs_at_Pole1[i,1] <- KMT_end_1
      KMTs_at_Pole2[i,1] <- KMT_end_2
      Sys.sleep(0.1)
      setWinProgressBar(pb, i, 
                        title = paste("Combine Inter-Kinetochore distance and no. of KMTs...",
                                      round((i - 1) / total * 100, 
                                            0), 
                                      "% Done"))
    },
    error = function(e){}
    )
  }
  
  KMTs_at_Pole1 <- na.omit(KMTs_at_Pole1)
  KMTs_at_Pole2 <- na.omit(KMTs_at_Pole2)
  
  ## Inter-Kinetochore distance vs. no. of KTMs
  Dist <- rbind(Inter_Kinetochore_Distance, Inter_Kinetochore_Distance)
  KMT <- rbind(KMTs_at_Pole1, KMTs_at_Pole2)
  DF <- cbind(Dist, KMT)
  names(DF)[1] <- "Inter-kinetochore distance"
  names(DF)[2] <- "KMTs no."

   close(pb)
   DF
}

## Count delta of KMTs between sister kinetochores
Compare_KMTs_delta_for_sister <- function(){
  total <- length(which(colnames(Segments) == "Pole1_00") : which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole1")))[length(colnames(Segments %>% select(starts_with("Pole1"))))]))
  pb <- winProgressBar(min = 2,
                       max =  total,
                       width = 420)
  
  KMTs_at_Pole1 <- data.frame()
  KMTs_at_Pole2 <- data.frame()
  
  for(i in which(colnames(Segments) == "Pole1_00") : which(colnames(Segments) == colnames(Segments %>% select(starts_with("Pole1")))[length(colnames(Segments %>% select(starts_with("Pole1"))))])){
    tryCatch({
      DF_Pole1 <- colnames(Segments)[i]
      DF <- data.frame(str_split(gsub("[^[:digit:]]", "Pole_1", DF_Pole1), pattern = "Pole_1"))
      DF_Pole2 <- paste("Pole2", DF[6,1], sep = "_")
      
      KMT_end_1 <- nrow(get(DF_Pole1))
      KMT_end_2 <- nrow(get(DF_Pole2))
      
      if(KMT_end_1 == 0 | KMT_end_2 == 0){
        KMT_end_1 <- NA
        KMT_end_2 <- NA
      } else {}
      
      ## No. of KMTs at each kinetochore
      KMTs_at_Pole1[i,1] <- KMT_end_1
      KMTs_at_Pole2[i,1] <- KMT_end_2
      Sys.sleep(0.1)
      setWinProgressBar(pb, i, 
                        title = paste("Combine Inter-Kinetochore distance and KMTs delta...",
                                      round((i - 1) / total * 100, 
                                            0), 
                                      "% Done"))
    },
    error = function(e){}
    )
  }
  
  KMTs_at_Pole1 <- na.omit(KMTs_at_Pole1)
  KMTs_at_Pole2 <- na.omit(KMTs_at_Pole2)
  
## Inter-Kinetochore distance vs. delta KTMs
  Delta <- abs(KMTs_at_Pole1 - KMTs_at_Pole2)
  DF <- cbind(Inter_Kinetochore_Distance, Delta)
  names(DF)[1] <- "Inter-kinetochore distance"
  names(DF)[2] <- "Delta of KMTs"
  
  close(pb)
  DF
}
