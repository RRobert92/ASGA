####################################################################
# The analysis tool to calcualte total and local curvature of KMTs #
####################################################################

## Count total curvature ration for each KMT. 
## Ration is determined by dividing total KMT length and length of KMT between (+) and (-) end
total_curvature <- function(x){
  curvarture <- data.frame()
  for (i in 1:nrow(get(paste(colnames(Segments)[x])))){
    KMT <- get(paste(colnames(Segments)[x], i, sep = "_"))
    curv <- sqrt((KMT[1,2] - KMT[nrow(KMT),2])^2 + (KMT[1,3] - KMT[nrow(KMT),3])^2 + (KMT[1,4] - KMT[nrow(KMT),4])^2)
    total_length <- Pole1_00[i,2]
    curvarture[i,1] <- total_length / curv
    curvarture[i,2] <- x
    names(curvarture)[1] <- "Curvature"
    names(curvarture)[2] <- "k-fiber no."
  }
  curvarture
}

## Count a distance between first point and first point + 5 (1+5) for data with 20nm step, curvature is count every 100 nm
## Count the curvature ratio for each step
local_curvature <- function(2){
 
  for (i in 1:nrow(get(paste(colnames(Segments)[2])))){
    KMT <- get(paste(colnames(Segments)[2], i, sep = "_"))
    j = 1
    output_curve <- data.frame(Curve = as.numeric())
    
    while (j < nrow(KMT)) {
      output_curve[j,1] <- sqrt((KMT[j,2] - KMT[j+5,2])^2 + (KMT[j,3] - KMT[j+5,3])^2 + (KMT[j,4] - KMT[j+5,4])^2) 
      j = j + 5
    }
    output_curve <- na.omit(output_curve)
    
    output_full <- data.frame(Full_L = as.numeric())
    j = 1
    
    while (j < nrow(KMT)){
    full1 <- sqrt((KMT[j,2] - KMT[j+1,2])^2 + (KMT[j,3] - KMT[j+1,3])^2 + (KMT[j,4] - KMT[j+1,4])^2)
    full2 <- sqrt((KMT[j+1,2] - KMT[j+2,2])^2 + (KMT[j+1,3] - KMT[j+2,3])^2 + (KMT[j+1,4] - KMT[j+2,4])^2)
    full3 <- sqrt((KMT[j+2,2] - KMT[j+3,2])^2 + (KMT[j+2,3] - KMT[j+3,3])^2 + (KMT[j+2,4] - KMT[j+3,4])^2)
    full4 <- sqrt((KMT[j+3,2] - KMT[j+4,2])^2 + (KMT[j+3,3] - KMT[j+4,3])^2 + (KMT[j+3,4] - KMT[j+4,4])^2)
    full5 <- sqrt((KMT[j+4,2] - KMT[j+5,2])^2 + (KMT[j+4,3] - KMT[j+5,3])^2 + (KMT[j+4,4] - KMT[j+5,4])^2)
    output_full[j,] <- sum(full1,full2, full3, full4, full5)
    j = j + 5
    }
    output_full <- na.omit(output_full)
    
    
    ## get mean for the x and y, and calculate relative position for the pole which is recognise by the name of the label
    output_mean <- data.frame(Mean_Position = as.numeric())
    j = 1  
    
    while(j < nrow(KMT)){
      output_mean[j,] <- (KMT[j,2] + KMT[j+5,2])/2
      j = j + 5
    }
    output_mean <- na.omit(output_mean)
    
  }

  
  full_data <- cbind.data.frame(Curvature = output_full$Full_L/output_curve$Curve,
                                Relative_Position = output_mean$Mean_Position)
  full_data[complete.cases(full_data),]
}
