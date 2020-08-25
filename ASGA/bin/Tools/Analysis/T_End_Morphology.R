#####################################################################################
# Tool End_Morphology
#
# The analysis tools to defined end morphology
#
# Count the error rate between sample
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-04-21
#####################################################################################


# Tool End Morphology ---------------------------------------------------------------
End_Type_Error <- function(){
  if(ncol(Nodes %>% select(starts_with("EndType"))) == 2){
    End_type_error <- data.frame(Correct = c(round(table(Nodes[,7])["TRUE"]*100/nrow(Nodes), 2)),
                                 Wrong = c(round(table(Nodes[,7])["FALSE"]*100/nrow(Nodes),
                                                 2)))
  } else {
    break
  }
  End_type_error
}


# Analyze the end distribution according to the relative position of the KMT end ----
# X is here no. of column in the "Segment" ------------------------------------------
# Y is a no. of a Pole 1 or 2 -------------------------------------------------------
End_distribution_Plus <- function(x, y){
  
  # Function setting ------------------------------------------------------------------
  if(y ==1) {
    y <- Pole1
  } else {
    y <- Pole2
  }
  
  Plus <- data.frame()
  
  # For x find Node ID that belong to Segment ID ---------------------------------------
  if(nrow(get(colnames(Segments)[x])) >= 1){
    for(i in 1:nrow(get(colnames(Segments)[x]))){
      S_ID <- get(colnames(Segments)[x])[i,1]
      
      N_ID_1 <- as.numeric(Segments[as.numeric(S_ID+1), "Node ID #1"])
      Node_1 <- Nodes[as.numeric(N_ID_1+1),]
      
      N_ID_2 <- as.numeric(Segments[as.numeric(S_ID+1), "Node ID #2"])
      Node_2 <- Nodes[as.numeric(N_ID_2+1),]
      
      if(abs(Node_1["Y Coord"] - y["Y.Coord"]) > abs(Node_2["Y Coord"] - y["Y.Coord"])){
        Plus[i,1:8] <- cbind(Node_1,
                             get(colnames(Segments)[x])[i,
                                                        "Relative_plus_position"])
        
        Plus[i,1] <- x
        Plus[i,9] <- as.numeric(Segments[as.numeric(S_ID+1), "Node ID #1"])
        
      } else {
        Plus[i,1:8] <- cbind(Node_2,
                             get(colnames(Segments)[x])[i,
                                                        "Relative_plus_position"])
        
        Plus[i,1] <- x
        Plus[i,9] <- as.numeric(Segments[as.numeric(S_ID+1), "Node ID #2"])
      }
    }
    names(Plus)[1] <- "Fiber"
    names(Plus)[8] <- "Relative_plus_position"
    names(Plus)[9] <- "Node_ID"
    
    rm(S_ID, N_ID_1, N_ID_2, Node_1, Node_2, y)
    
    Plus
  } 
}

# Analyze the end distribution according to the relative position of the KMT end -----
# X is here no. of column in the "Segment" -------------------------------------------
# Y is a no. of a Pole 1 or 2 --------------------------------------------------------
End_distribution_Minus <- function(x, y){
  
  # Function setting -------------------------------------------------------------------
  if(y ==1) {
    y <- Pole1
  } else {
    y <- Pole2
  }
  
  Minus <- data.frame()
  
  # For x find Node ID that belong to Segment ID ---------------------------------------
  if(nrow(get(colnames(Segments)[x])) >= 1){
    for(i in 1:nrow(get(colnames(Segments)[x]))){
      S_ID <- get(colnames(Segments)[x])[i,1]
      
      N_ID_1 <- as.numeric(Segments[as.numeric(S_ID+1), 
                                    "Node ID #1"])
      Node_1 <- Nodes[as.numeric(N_ID_1+1),]
      
      N_ID_2 <- as.numeric(Segments[as.numeric(S_ID+1), 
                                    "Node ID #2"])
      Node_2 <- Nodes[as.numeric(N_ID_2+1),]
      
      if(abs(Node_1["Y Coord"] - y["Y.Coord"]) > abs(Node_2["Y Coord"] - y["Y.Coord"])){
        Minus[i,1:8] <- cbind(Node_2,
                              get(colnames(Segments)[x])[i,
                                                         "Relative_minus_position"])
        
        Minus[i,1] <- x
        Minus[i,9] <- as.numeric(Segments[as.numeric(S_ID+1), "Node ID #1"])
        
      } else {
        Minus[i,1:8] <- cbind(Node_1,
                              get(colnames(Segments)[x])[i,
                                                         "Relative_minus_position"])
        
        Minus[i,1] <- x
        Minus[i,9] <- as.numeric(Segments[as.numeric(S_ID+1), "Node ID #2"])
      }
    }
    names(Minus)[1] <- "Fiber"
    names(Minus)[8] <- "Relative_minus_position"
    names(Minus)[9] <- "Node_ID"
    
    rm(S_ID, N_ID_1, N_ID_2, Node_1, Node_2, y)
    
    Minus
  }
}
