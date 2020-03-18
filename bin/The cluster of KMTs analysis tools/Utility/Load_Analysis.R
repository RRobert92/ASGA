## Load Length Distribution
for (i in 1:No_of_Data) {
  tryCatch({
    assign(paste(Data_label, "_", i, "_LD", sep = ""),
           read_excel(paste("Output/", Data_label, "_", i, "_LD.xlsx", sep = "")))
  },
  error = function(e){})
}

## Load Inter-kinetochore distance
for (i in 1:No_of_Data) {
  tryCatch({
    assign(paste(Data_label, "_", i, "_IKD", sep = ""),
           read_excel(paste("Output/", Data_label, "_", i, "_Inter_Kinetochore_Distance.xlsx", sep = "")))
  },
  error = function(e){})
}

## Load no of a KMTs at the kinetochore
for (i in 1:No_of_Data) {
  tryCatch({
    assign(paste(Data_label, "_", i, "_KMTs_at_K", sep = ""),
           read_excel(paste("Output/", Data_label, "_", i, "_KMTs_no.xlsx", sep = "")))
  },
  error = function(e){})
}

## Load no of a KMTs at the pole 1/2
for (i in 1:No_of_Data) {
  tryCatch({
    assign(paste(Data_label, "_", i, "_KMTs_at_P1", sep = ""),
           read_excel(paste("Output/", Data_label, "_", i, "_KMTs_at_the_Pole1.xlsx", sep = "")))
  },
  error = function(e){})
  tryCatch({
    assign(paste(Data_label, "_", i, "_KMTs_at_P2", sep = ""),
           read_excel(paste("Output/", Data_label, "_", i, "_KMTs_at_the_Pole2.xlsx", sep = "")))
  },
  error = function(e){})
}

## Load minus end position
for (i in 1:No_of_Data) {
  tryCatch({
    assign(paste(Data_label, "_", i, "_Minus_end_P1", sep = ""),
           read_excel(paste("Output/", Data_label, "_", i, "_Minus_end_position_P1.xlsx", sep = "")))
  },
  error = function(e){})
  tryCatch({
    assign(paste(Data_label, "_", i, "_Minus_end_P2", sep = ""),
           read_excel(paste("Output/", Data_label, "_", i, "_Minus_end_position_P2.xlsx", sep = "")))
  },
  error = function(e){})
}

## Load total and local curvature
for (i in 1:No_of_Data) {
  tryCatch({
    assign(paste(Data_label, "_", i, "_total_curvature", sep = ""),
           read_excel(paste("Output/", Data_label, "_", i, "_KMTs_total_Curvature.xlsx", sep = "")))
  },
  error = function(e){})
  tryCatch({
    assign(paste(Data_label, "_", i, "_local_curvature", sep = ""),
           read_excel(paste("Output/", Data_label, "_", i, "_KMTs_local_Curvature.xlsx", sep = "")))
  },
  error = function(e){})
}

## Load end morphology
for (i in 1:No_of_Data) {
  tryCatch({
    assign(paste(Data_label, "_", i, "_End_type_error", sep = ""),
           read_excel(paste("Output/", Data_label, "_", i, "_End_type_error.xlsx", sep = "")))
  },
  error = function(e){})
  tryCatch({
    assign(paste(Data_label, "_", i, "_(+)_end_morphology_P1", sep = ""),
           read_excel(paste("Output/", Data_label, "_", i, "_(+)_morphology_P1.xlsx", sep = "")))
  },
  error = function(e){})
  tryCatch({
    assign(paste(Data_label, "_", i, "_(+)_end_morphology_P2", sep = ""),
           read_excel(paste("Output/", Data_label, "_", i, "_(+)_morphology_P2.xlsx", sep = "")))
  },
  error = function(e){})
  tryCatch({
    assign(paste(Data_label, "_", i, "_(-)_end_morphology_P1", sep = ""),
           read_excel(paste("Output/", Data_label, "_", i, "_(-)_morphology_P1.xlsx", sep = "")))
  },
  error = function(e){})
  tryCatch({
    assign(paste(Data_label, "_", i, "_(-)_end_morphology_P2", sep = ""),
           read_excel(paste("Output/", Data_label, "_", i, "_(-)_morphology_P2.xlsx", sep = "")))
  },
  error = function(e){})
}