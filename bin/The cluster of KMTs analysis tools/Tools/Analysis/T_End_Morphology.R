################################################
# The analysis tools to defined end morphology # 
################################################

## Count the error rate between sample
End_Tyep_Error <- function(){
  if(ncol(Nodes %>% select(starts_with("EndType"))) == 2){
    Correct <- count(Nodes[,7])
  End_type_error <- data.frame(Correct = c(round((Correct[2,2]/sum(Correct[2]))*100,2)),
                               Wrong = c(round((Correct[1,2]/sum(Correct[2]))*100,2)))
  } else {
    break
  }
  
  rm(Correct)
}

## Count overall number of the end type
End_Type_overall <- function(x){
  
}

## Analyse the end distribution according to the relative position of the KMT end
End_distribution <- function(){
  
}