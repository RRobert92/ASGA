---
title: Global
authors: Robert Kiewisz
date: 2021-02-15
---
<style>
    th {
        display: none;
    }
</style>

#**ASGA Global**

  ***Version***&nbsp;&nbsp;                      {{ version }} <br/>
  ***Data***&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; {{ Data }} <br/>
  ***Author***&nbsp;&nbsp;&nbsp;                 {{ Author }} <br/>
  ***Depends***                                  `Shiny` `tidyverse` `base` `readxl` `xlsx` `zip` <br/>
  ***License***&nbsp;&nbsp;&nbsp;                {{ License }}<br/><br/>
  
  ***Description*** Global modules and functions used in ASGA

<br/>
<a name="Check_Data"></a>
<hr /><center>
###[`Check_Data`](https://github.com/RRobert92/ASGA/blob/main/ASGA/bin/Utility/Check_Data.R)
<hr /></center>

####***Description*** <br/>
The ASGA test module to check each uploaded data set before analysis. The test module validates if the uploaded data fitting the expected standardized structure.

####***Usage***<br/>
```
<Upload_Data.R>

Check_Data(i) # Check data routine for raw data 
Check_Analysis(File_name) # Check data routine for analyzed data

```

####***Arguments***<br/>
 
|           |                                                                  |
|-----------|------------------------------------------------------------------|
`i`         | single-digit number of the uploaded file
`File_name` | name of the uploaded file

<br/>

####***Value***<br/>
Check_Data object create the following components:

|Data_frame    |                                                              |
|--------------|--------------------------------------------------------------|
`DATA_TEST`    | Test output with numeric value interpreted in `Error_Handler` GUI ASGA module
`AnalysisTest` | Test output with numeric value interpreted in `Upload_Data` Global ASGA modules


<br/><br/>
<a name="Upload_Data"></a>
<hr /><center>
###[`Upload_Data`](https://github.com/RRobert92/ASGA/blob/main/ASGA/bin/Utility/Upload_Data.R)
<hr /></center>

####***Description*** <br/>
The ASGA module to handle uploaded files. There are two routines, one for handling raw data set in `.xlsx`  or `.am` file format. The second one to handle `.xlsx` files download after analysis by ASGA.


####***Usage***<br/>
```
<server.R>

callModule(Getfiles_Server, "Home")
```
<br/>

####***Value***<br/>
Upload_Data object creates the following components:

|Data_frame    |                                                              |
|--------------|--------------------------------------------------------------|
`NUM_FILES`    | Constant storing the value of number uploaded data sets
`Amira_df`     | Temp file of raw `.am` files 
`AMIRA`        | Constant value if = TURE the Amira filed are used
`text_dataset` | Variable used in `Error_Handler` GUI ASGA module for displaying messages
`datatype`     | Variable used in `Error_Handler` GUI ASGA module for displaying messages
`AnalysisTest` | Test result variable from `Check_Data` Global ASGA modules used to break upload if data are not correct

<br/>

####***Details***<br/>
This module works in tandem with `Check_Data`, `Load_Amira` Global ASGA modules and `Error_Handler` GUI ASGA module

 
<br/><br/>
<a name="Load_Amira"></a>
<hr /><center>
###[`Load_Amira`](https://github.com/RRobert92/ASGA/blob/main/ASGA/bin/Utility/Load_Amira.R)
<hr /></center>

####***Description*** <br/>
The ASGA module called when the `.am` file is uploaded by the user. The module is calling three functions (`Load_Amira_Nodes`, `Load_Amira_Points`, `Load_Amira_Segments`) to extract from .am files nodes, points and segments information.

####***Usage***<br/>
```
Load_Amira_Nodes()
Load_Amira_Points()
Load_Amira_Segments()

```
<br/>

####***Example***<br/>

```
<Upload_Data.R>

assign(paste("Data", "Node", i, sep = "_"),
              Load_Amira_Nodes(),
              envir = .GlobalEnv
            )
            
assign(paste("Data", "Points", i, sep = "_"),
              Load_Amira_Points(),
              envir = .GlobalEnv
            )
            
assign(paste("Data", "Segments", i, sep = "_"),
              Load_Amira_Segments(),
              envir = .GlobalEnv
            )
```

<br/><br/>
<a name="Load_Data"></a>
<hr /><center>
###[`Load_Data`](https://github.com/RRobert92/ASGA/blob/main/ASGA/bin/Utility/Load_Data.R)
<hr /></center>

####***Description*** <br/>
The ASGA module called for each data set before analysis to the standardized spatial graph for analysis.

####***Usage***<br/>
```
<server.R>

callModule(Load_Data, "Home")
```
<br/>

####***Value***<br/>
Load_Data object create the following components:

|Data_frame      |                                                              |
|----------------|--------------------------------------------------------------|
`Node`           | Data frame containing columns (`Node ID`, `X Coord`, `Y Coord`, `Z Coord`) for each node/MT end
`Points`         | Data frame containing columns (`Point ID`, `X Coord`, `Y Coord`, `Z Coord`) for each point in the point cloud
`Segments`       | Data frame containing columns (`Segment ID`,`Pole1_00`,... ,`length`, `Node ID #1`, `Node ID #2`, `Point IDs`)
`Segments_1_KMT` | Data frame containing columns (`Segment ID`, `length`, `Node ID #1`, `Node ID #2`, `Point IDs`) for k-fiber in Pole_1
`Segments_2_KMT` | Data frame containing columns (`Segment ID`, `length`, `Node ID #1`, `Node ID #2`, `Point IDs`) for k-fiber in Pole_2
`Segments_KMT`   | Data frame containing columns (`Segment ID`, `length`, `Node ID #1`, `Node ID #2`, `Point IDs`) for k-fiber in both poles
`Segments_SMT`   | Data frame containing columns (`Segment ID`, `length`, `Node ID #1`, `Node ID #2`, `Point IDs`) for SMTs in Pole_1
`Pole1`          | Data frame containing columns (`X`, `Y`, `Z`) for Pole1 coordinates
`Pole2`          | Data frame containing columns (`X`, `Y`, `Z`) for Pole2 coordinates


<br/><br/>
<a name="Save_Data"></a>
<hr /><center>
###[`Save_Data`](https://github.com/RRobert92/ASGA/blob/main/ASGA/bin/Utility/Save_Data.R)
<hr /></center>

####***Description*** <br/>
The ASGA module called at the end of the analysis to save all indicated data sets.
<br/>

####***Example***<br/>
```
<Save_Data.R>
  
  # Save Data for SMT ends -----------------------------------------------------
  tryCatch(
    {
      assign(paste("Data", current_data, "Name", sep = "_"),
             Name_df,
             envir = .GlobalEnv
      )
      write.xlsx(
        get(paste("Data", current_data, "Name", sep = "_")),
        paste("Data/", "Data_", current_data, "_Name.xlsx", sep = "")
      )
    },
    error = function(e) {}
  )

```
<br/>

####***Arguments***<br/>

|           |                                                                  |
|-----------|------------------------------------------------------------------|
`Name`      | Name of the file in `.GlobalEnv` to be saved
`Name_df`   | Name of the data_frame in `.GlobalEnv`

<br/>

####***Details***<br/>
Only `.xlsx` are saved if Amira files were used for analysis then also an overwrite by Save_Amira `.am` files are saved. 

<br/><br/>
<a name="Save_Amira"></a>
<hr /><center>
###[`Save_Amira`](https://github.com/RRobert92/ASGA/blob/main/ASGA/bin/Utility/Save_Amira.R)
<hr /></center>

####***Description*** <br/>
The function overwrites  the uploaded by user Amira files by supplying additional analysis for visualization in ZiB Amira software.
<br/>

####***Usage***<br/>
```
Save_amira(x, y, Elements, Data_type)

```
<br/>

####***Arguments***<br/>

|           |                                                                  |
|-----------|------------------------------------------------------------------|
`x`         | Data_frame name which should be saved in Amira file
`y`         | Data_frame column number which should be saved in Amira file
`Elements`  | Type of saved data e.g ("Nodes", "Points", "Segments")
`Data_type` | Type of input e.g. ("int", "float", "float[3]")

<br/>
 
####***Details***<br/>
 Data set that are indicated by the user in __x[y]__ need to be sorted accordingly to the segment number. 
 
 Before the `.am` file is overwrite the __x[y]__ object is checked if the defined element number is the same as in the Amira file.
 
####***Example***<br/>
```
<Save_Data.R>

tryCatch({
    assign(paste("Amira", "Dataset", current_data, sep = "_"),
        Save_amira(DF, 2, "Segments", "float"),
        envir = .GlobalEnv)
}, error = function(e) {}
)

```

<br/><br/>
<a name="Export_Data"></a>
<hr /><center>
###[`Export_Data`](https://github.com/RRobert92/ASGA/blob/main/ASGA/bin/Utility/Export_Data.R)
<hr /></center>

####***Description*** <br/>
Small unit designed to collect a list of all saved data, zip them and send to download by the user. After the user download data, all files are deleted.

####***Usage***<br/>
```
<server.R>

# to intoduce new formar for downloadHandler
Zip_Files_Format <- list.files(path = getwd(), pattern = Pattern)

```
<br/>

####***Arguments***<br/>

|         |                                                                    |
|---------|--------------------------------------------------------------------|
`Pattern` | Example: ".am$"

<br/>

####***Details***<br/>
Only `.xlsx` and `.am` files are collected