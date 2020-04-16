################################################
# The analysis tools to defined end morphology # 
################################################

## Count the error rate between sample
End_Type_Error <- function(){
  if(ncol(Nodes %>% select(starts_with("EndType"))) == 2){
    Correct <- count(Nodes[,7])
    End_type_error <- data.frame(Correct = c(round((Correct[2,2]/sum(Correct[2]))*100,
                                                   2)),
                                 Wrong = c(round((Correct[1,2]/sum(Correct[2]))*100,
                                                 2)))
  } else {
    break
  }
  rm(Correct)
  
  End_type_error
}

## Analyse the end distribution according to the relative position of the KMT end
## X is here no. of column in the "Segment"
## Y is a no. of a Pole 1 or 2
End_distribution_Plus <- function(x, y){
  ## Function setting
  
  if(y ==1) {
    y <- Pole1
  } else {
    y <- Pole2
  }
  
  Plus <- data.frame()
  
  ##for x find Node ID that belong to Segment ID
  for(i in 1:nrow(get(colnames(Segments_KMT)[x]))){
    S_ID <- get(colnames(Segments_KMT)[x])[i,1]
    
    N_ID_1 <- as.numeric(Segments_KMT[S_ID+1, "Node ID #1"])
    Node_1 <- Nodes[N_ID_1 + 1,]
    
    N_ID_2 <- as.numeric(Segments_KMT[S_ID+1, "Node ID #2"])
    Node_2 <- Nodes[N_ID_2 + 1,]
    
    if(abs(Node_1["Y Coord"] - y["Y.Coord"]) > abs(Node_2["Y Coord"] - y["Y.Coord"])){
      Plus[i,1:8] <- cbind(Node_1,
                           get(colnames(Segments_KMT)[x])[i,
                                                      "Relative_plus_position"])

      Plus[i,1] <- x
      
    } else {
      Plus[i,1:8] <- cbind(Node_2,
                           get(colnames(Segments_KMT)[x])[i,
                                                      "Relative_plus_position"])
      
      Plus[i,1] <- x
      }
    
  }
  names(Plus)[1] <- "Fiber"
  names(Plus)[8] <- "Relative_plus_position"
  
  rm(S_ID, N_ID_1, N_ID_2, Node_1, Node_2, y)
  
  Plus
} 

## Analyze the end distribution according to the relative position of the KMT end
## X is here no. of column in the "Segment"
## Y is a no. of a Pole 1 or 2
End_distribution_Minus <- function(x, y){
  ## Function setting
  
  if(y ==1) {
    y <- Pole1
  } else {
    y <- Pole2
  }
  
  Minus <- data.frame()
  
  ##for x find Node ID that belong to Segment ID
  for(i in 1:nrow(get(colnames(Segments_KMT)[x]))){
    S_ID <- get(colnames(Segments_KMT)[x])[i,
                                           1]
    N_ID_1 <- as.numeric(Segments_KMT[S_ID+1, 
                                      "Node ID #1"])
    Node_1 <- Nodes[N_ID_1 + 1,]
    
    N_ID_2 <- as.numeric(Segments_KMT[S_ID+1, 
                                      "Node ID #2"])
    Node_2 <- Nodes[N_ID_2 + 1,]
    
    if(abs(Node_1["Y Coord"] - y["Y.Coord"]) > abs(Node_2["Y Coord"] - y["Y.Coord"])){
      Minus[i,1:8] <- cbind(Node_2,
                            get(colnames(Segments_KMT)[x])[i,
                                                           "Relative_minus_position"])
      
      Minus[i,1] <- x
      
    } else {
      Minus[i,1:8] <- cbind(Node_1,
                            get(colnames(Segments_KMT)[x])[i,
                                                           "Relative_minus_position"])
      
      Minus[i,1] <- x
    }
    
  }
  
  names(Minus)[1] <- "Fiber"
  names(Minus)[8] <- "Relative_minus_position"
  
  rm(S_ID, N_ID_1, N_ID_2, Node_1, Node_2, y)
  
  Minus
} 
