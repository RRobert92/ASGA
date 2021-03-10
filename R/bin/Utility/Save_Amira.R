################################################################################
# Module Save_Amira
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Collect data.frame as variable x, and bind it to the Amira file
# as variable y is a column name to be written in th Amira  file
#
# Author: Robert Kiewisz
# Created: 2020-09-23
# Reviewed:
################################################################################

# Save segment, point or nodes  file as int variable ---------------------------
Save_amira <- function(x, y, Elements, Data_type) {

  # Check if needed data x[y] is compatible with Amira file and select Elements
  if (Elements == "Nodes") {
    Rows_need <- as.numeric(gsub("[^[:digit:]]", "", get(paste("Amira", "Dataset", current_data, sep = "_")) %>%
      filter(str_detect(X1, "VERTEX")) %>%
      filter(str_detect(X1, "define"))))

    Type_data <- "VERTEX"
  } else if (Elements == "Points") {
    Rows_need <- as.numeric(gsub("[^[:digit:]]", "", get(paste("Amira", "Dataset", current_data, sep = "_")) %>%
      filter(str_detect(X1, "POINT")) %>%
      filter(str_detect(X1, "define"))))

    Type_data <- "POINT"
  } else if (Elements == "Segments") {
    Rows_need <- as.numeric(gsub("[^[:digit:]]", "", get(paste("Amira", "Dataset", current_data, sep = "_")) %>%
      filter(str_detect(X1, "EDGE")) %>%
      filter(str_detect(X1, "define"))))

    Type_data <- "EDGE"
  }

  if (Rows_need == nrow(x)) {
    if(length(is.na(x[y])[x[y] == TRUE]) > 0){
      x[y][is.na(x[y])] <- "nan" 
    }

    No_column <- get(paste("Amira", "Dataset", current_data, sep = "_")) %>%
      filter(str_detect(X1, "@"))
    No_column <- No_column %>%
      separate(X1, c("V1", "V2", "V3", "V4", "V5", "V6"), sep = " ", fill = "left")

    No_column_data <- No_column[6] %>%
      separate(V6, c("V1", "V2"), sep = "@")

    No_column_name <- paste("@", max(transform(No_column_data, V2 = as.numeric(V2))[2]) + 1, sep = "")
    No_column_name <- paste(Type_data, " { ", Data_type, " ", colnames(x)[y], " } ", No_column_name, sep = "")

    No_column_data <- paste("@", max(transform(No_column_data, V2 = as.numeric(V2))[2]), sep = "")

    Pattern <- as.vector(paste(get(paste("Amira", "Dataset", current_data, sep = "_")) %>%
      filter(str_detect(X1, No_column_data)) %>%
      filter(str_detect(X1, "POINT"))))

    if(Pattern == "character(0)"){
      Pattern <- as.vector(paste(get(paste("Amira", "Dataset", current_data, sep = "_")) %>%
                                   filter(str_detect(X1, No_column_data)) %>%
                                   filter(str_detect(X1, "VERTEX"))))
    }
    if(Pattern == "character(0)"){
      Pattern <- as.vector(paste(get(paste("Amira", "Dataset", current_data, sep = "_")) %>%
                                   filter(str_detect(X1, No_column_data)) %>%
                                   filter(str_detect(X1, "EDGE"))))
    }

    No_column_data <- which(get(paste("Amira", "Dataset", current_data, sep = "_")) ==
      Pattern)

    df <- get(paste("Amira", "Dataset", current_data, sep = "_"))[1:No_column_data, 1]
    df[No_column_data + 1, 1] <- No_column_name

    df_1 <- get(paste("Amira", "Dataset", current_data, sep = "_"))[No_column_data + 1:nrow(get(paste("Amira", "Dataset", current_data, sep = "_"))), 1]

    df_1 <- drop_na(df_1)

    # Save data x[y] with the right header e.g. @105
    No_column_data <- No_column[6] %>%
      separate(V6, c("V1", "V2"), sep = "@")

    No_column_name <- paste("@", max(transform(No_column_data, V2 = as.numeric(V2))[2]) + 1, sep = "")

    df_2 <- rbind(No_column_name, x[y])
    names(df_2)[1] <- "X1"

    df_amira <- rbind(
      df,
      df_1,
      df_2
    )

    rm(
      No_column, No_column_data, No_column_name,
      df, df_1, df_2,
      Pattern,
      Rows_need, Type_data
    )
  }

  df_amira
}
