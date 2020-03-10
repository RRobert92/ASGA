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

Data_label <- dlg_input("What is a label for the data?
                        This label will be used to save your data.", 
                        "WT")$res

Output <- "Output/"

Minus_Threshold <- 1
