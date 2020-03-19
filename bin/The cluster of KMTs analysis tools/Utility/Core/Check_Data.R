Test_Segments <- colnames(Segments)[1] == "Segment ID" && colnames(Segments)[ncol(Segments)] == "Point IDs" && colnames(Segments)[ncol(Segments) - 3] == "length"

Test_Pole1 <- colnames(Nodes %>% select(Pole1)) == "Pole1"
if (!exists("Test_Pole1")){
  Test_Pole1 <- FALSE
} else {}

Test_Pole2 <- colnames(Nodes %>% select(Pole2)) == "Pole2"
if (!exists("Test_Pole2")){
  Test_Pole2 <- FALSE
} else {}

if (Test_Segments == TRUE && Test_Pole1 == TRUE && Test_Pole2 == TRUE) {
  msg_box("Looks like you read the instruction!
           The data structure looks great! 
           Press Ok to analyze your awesome data!")
  
} else if (Test_Segments == TRUE && which(colnames(Segments) == "Pole1_00") > which(colnames(Segments) == "Pole2_00")){
  quit(msg_box("Looks like you didn't read the instrution :(
                The labeling in the Segmeent excel sheet shoult start with Pole1_00 not Pole2_00.
                Please check it with the guidelines and try again."))
  
} else if (Test_Segments == FALSE && Test_Pole1 == TRUE && Test_Pole2 == TRUE) {
  quit(msg_box("Looks like you didn't read the instrution :(
               The Segments data structure looks strange! Segment ID, Point IDs or length are missing or are in the wrong order...
               Please check it with the guidelines and try again."))
  
} else if (Test_Segments == TRUE && Test_Pole1 == FALSE) {
  quit(msg_box("Looks like you didn't read the instrution :(
                The labeling in the Node excel sheet missing information about Pole1...
                Please check it with the guidelines and try again."))
  
} else if (Test_Segments == TRUE && Test_Pole2 == FALSE) {
  quit(msg_box("Looks like you didn't read the instrution :(
                The labeling in the Node excel sheet missing information about Pole2...
                Please check it with the guidelines and try again."))
  
} else if (Test_Segments == TRUE && Test_Pole1 == TRUE && Test_Pole2 == TRUE) {
  msg_box("Could not find any 'Poles' coordinates in the Nodes excel sheet! Please check it with the guidelines and try again.")
  dlg_message("Are the 'Pole's' coordinates embedded in the raw data in the Node sheet?",
              "yesno")$res
  
  if (res == "Yes") {
    Pole1 <- dlg_input("Is this correct label name for the 'Pole_1'?", 
                       "Pole1")$res
    Pole2 <- dlg_input("Is this correct label name for the 'Pole_2'?", 
                       "Pole2")$res
    
  } else if (res == "No") {
    quit(msg_box("Please add 'Pole_1' and 'Pole_2' coordinates in the Nodes sheet and try again."))
    
  }
}

rm(Test_Segments, 
   Test_Pole1,
   Test_Pole2)
