---
title: Template
authors: Robert Kiewisz
date: 2021-02-15
---
<style>
    th {
        display: none;
    }
</style>

# **Template for documentation**
Test set-up for testing each function/tools in the ASGA software

  ***Version***&nbsp;&nbsp;                      {{ version }} <br/>
  ***Data***&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; {{ Data }} <br/>
  ***Author***&nbsp;&nbsp;&nbsp;                 {{ Author }} <br/>
  ***Depends***                                  <br/>
  ***License***&nbsp;&nbsp;&nbsp;                {{ License }}<br/><br/>
  
  ***Description*** The test unit is composed of two-part ([`Test_Functions`](#Test_Functions), 
  [`Test_Output`](#Test_Output)) and GUI part for display error messages [`Test_Result`](#Test_Result)<br/><br/>
  
<a name="Test_Functions"></a>
<hr /><center>
###[`Test_Functions`](https://github.com/RRobert92/ASGA/blob/main/ASGA/tests/Test_Output.R)
<hr /></center>

####***Description*** <br/>
The part of the `Test_Unit` is designed for quick and straightforward testing if the tool is passing the test data set

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
```
####***Arguments***<br/>

|         |                                                                    |
|---------|--------------------------------------------------------------------|
`Name`    | Testing function class name
`ID`      | Continue number in data_frame
 
 <br/> 
 
####***Details***<br/> 
If needed more comments about arguments

####***Value***<br/>
Stored values objects

 - `Name`&nbsp;&nbsp;&nbsp;&nbsp; Testing function class name
 - `ID`&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Continue number in data_frame
 
####***References***<br/>

 
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
```