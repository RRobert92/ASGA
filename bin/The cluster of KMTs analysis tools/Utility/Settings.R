Data <- dlg_open(title = "Select data file", 
                 filters = dlg_filters[c("xlsx", "All")])$res

Segments <- read_excel(Data,
                       sheet = "Segments")
Nodes <- read_excel(Data,
                    sheet = "Nodes")
Points <- read_excel(Data,
                     sheet = "Points")

Poles <- dlg_message("Are the Poles labeled in the Node sheet as 'Pole1' and 'Pole2'?", 
                     "yesno")$res
if (Poles == "yes") {
  Pole1 <- "Pole1" ## Name of the label for the Pole1 in the Node section
  Pole2 <- "Pole2" ## Name of the label for the Pole2 in the Node section
} else {
  Pole1 <- dlg_input("What is a label for the Pole_1?", 
                     "Pole1")$res
  Pole2 <- dlg_input("What is a label for the Pole_2?", 
                     "Pole2")$res
}

Data_label <- dlg_input("What is a label for the data? Please use following theme.
                        This label will be used to save your data.", 
                        "WT_1")$res

Output <- "Output/"

Minus_Threshold <- 1

all <- dlg_message("Do you want to run full analysis?",
                   "yesno")$res
if(all == "yes"){
  analysis <- 0
} else {
  analysis <- dlg_input("What analysis to run?
  Pick one from list: 
                        1 - LD and base KMTs numbers
                        2 - Inter-kinetochore distance
                        3 - Curvature of KMTs
                        4 - k-fiber area (polygon)
                        5 - Length of a k-fiber
                        6 - Curvature of k-fiber")$res
}

if(analysis == 0 || analysis == 2){
  IKD <- dlg_message("*Impotent* !! You want to analyze Inter-Kinetochore distance...

                      This tool is taking into account labels form the Segment sheet.
                      Please make sure your data are properly labeled.
                   
                        Example:
                        Fibers with a pair form the opposite side should be labeled:
                          - Pole1_01 and correspond fiber as  Pole2_01
                          
                          Fibers without pair form the opposite side should be labeled:
                          - Pole0_01... for the Pole1
                          - Pole3_01... for the Pole2
                   
                      IS THIS TRUE FOR YOUR DATA?",
                     "yesno")$res
}

if(IKD == "yes"){
  
} else if(IKD == "no"){
  if(all == "yes"){
    dlg_message("I am sorry to hear that :( Please fixed that if you can! We will move now to analyse rest of your awsame data!")
    
  } else if( all == "no"){
    dlg_message("I am sorry to hear that :( Please fixed that and try again")
  }
}
rm(Data)