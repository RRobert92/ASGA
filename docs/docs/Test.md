---
title: Test_Unit page
authors: Robert Kiewisz
date: 2021-02-15
---
<style>
    th {
        display: none;
    }
</style>

# **Test_Unit**
ASGA module for testing each function/tools in the ASGA software

  ***Version***&nbsp;&nbsp;                      {{ version }} <br/>
  ***Data***&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; {{ Data }} <br/>
  ***Author***&nbsp;&nbsp;&nbsp;                 {{ Author }} <br/>
  ***Depends***                                  `Shiny` `shinyWidgets` `tidyverse`<br/>
  ***License***&nbsp;&nbsp;&nbsp;                {{ License }}<br/><br/>
  
  ***Description*** The test unit is composed of two-part ([`Test_Functions`](#Test_Functions), 
  [`Test_Output`](#Test_Output)) and GUI part for display error messages [`Test_Result`](#Test_Result)<br/><br/>
  
<a name="Test_Functions"></a>

<hr /><center>
###[`Test_Functions`](https://github.com/RRobert92/ASGA/blob/main/ASGA/tests/Test_Output.R)
<hr /></center>

####***Description*** <br/>
The part of the test unit is designed for quick and straightforward testing if the tool is passing the test data set

####***Usage***<br/>
```
 tryCatch(
      {
        callModule(`Name`, "Home")
        Test_value[1, `ID`] <<- TRUE
      },
      error = function(e) {
        Test_value[1, `ID`] <<- FALSE
      }
    )
    names(Test_value)[`ID`] <<- `Name`
    
<server.R>
      callModule(Load_Data, "Home")
      callModule(Test_Functions, "Home")
      callModule(Test_Output, "Home")
```
####***Arguments***<br/>

|         |                                                                    |
|---------|--------------------------------------------------------------------|
`Name`    | Testing function class name
`ID`      | Continue number in data_frame
 
 <br/>
 
####***Example***<br/>
```
    tryCatch(
      {
        callModule(Pre_Analysis, "Home")
        Test_value[1, 1] <<- TRUE
      },
      error = function(e) {
        Test_value[1, 1] <<- FALSE
      }
    )
    names(Test_value)[1] <<- "Pre_Analsis"
    
<server.R>
      callModule(Load_Data, "Home")
      callModule(Test_Functions, "Home")
      callModule(Test_Output, "Home")
```

<br/><br/>
<a name="Test_Output"></a>
<hr /><center>
###[`Test_Output`](https://github.com/RRobert92/ASGA/blob/main/ASGA/tests/Tests.R)
<hr /></center>

####***Description*** <br/>
The part of the test unit is designed for testing of the core functionality of the ASGA as well as testing tools output

####***Usage***<br/>
```
Test_df[1, `ID`] <<- Test_anaysis(`Name`, 
                                  P1_P2 = TRUE, 
                                  EXP_ncol = 6, 
                                  EXP_ncol = c(4,2,2))
```

####***Arguments***<br/>

|         |                                                                    |
|---------|--------------------------------------------------------------------|
`Name`    | Testing function class name
`ID`      | Continue number in data_frame
`P1_P2`   | `TRUE` `FALSE` value if P1 and P2 tools spited data for pole1 and pole2
`EXP_ncol`| Expected number of columns from standard test data set
`EXP_ncol`| Expected number of rows from standard test data set

<br/>

####***Example***<br/>
```
Test_df[1, 10] <<- Test_anaysis("Data_1_LD", 
                                TRUE, 
                                6, 
                                c(4,2,2))
                                
<server.R>
      callModule(Load_Data, "Home")
      callModule(Test_Functions, "Home")
      callModule(Test_Output, "Home")
```
<br/><br/>
<a name="Test_Result"></a>
<hr /><center>
###[`Test_Result`](https://github.com/RRobert92/ASGA/blob/main/ASGA/tests/Test_Output.R)
<hr /></center>

####***Description*** <br/>
The part of the test unit that gave two types of GUI message to the user:



|     |     |
|-----|-----|
__Pass all test correctly__ | Green
__Error occurred__ |  Yellow and error number indicating `ID` corresponding to testing function class name