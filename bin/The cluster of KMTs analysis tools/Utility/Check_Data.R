Test_Segments <- colnames(Segments)[1] == "Segment ID" && colnames(Segments)[ncol(Segments)] == "Point IDs" && colnames(Segments)[ncol(Segments) - 3] == "length"
Test_Poles <- colnames(Nodes %>% select(Pole1)) == "Pole1" && colnames(Nodes %>% select(Pole2)) == "Pole2"

if (Test_Poles && Test_Segments == TRUE) {
  msg_box("The data structure looks great! Press Ok to analyze your awesome data!")
  
} else if (Test_Segments == FALSE && Test_Poles == TRUE) {
  quit(msg_box("The Segments data structure looks strange! Please check it with the guidelines and try again."))
  
} else if (Test_Poles == FALSE && Test_Segments == TRUE) {
  msg_box("Could not find 'Poles' coordinates in the Nodes excel sheet! Please check it with the guidelines and try again.")
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
   Test_Poles, 
   Poles)
