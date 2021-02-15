---
title: Test_Unit page
authors: Robert Kiewisz
date: 2021-02-15
---

# **Test_Unit**
Test set-up for testing each function/tools in the ASGA software

The **Test_Unit** is composed of two-part ([`Test_Functions`](#Test_Functions), [`Test_Output`](#Test_Output)) and GUI part for display error messages [`Test_Result`](#Test_Result)

<a name="Test_Functions"></a>
###[`Test_Functions`](https://github.com/RRobert92/ASGA/blob/main/ASGA/tests/Test_Output.R)
The part of the `Test_Unit` is designed for quick and straightforward testing if the tool is passing the test data set

Usage of `Test_Functions`:
```
 tryCatch(
      {
        callModule(`Testing function class name`, "Home")
        Test_value[1, `Continue number in data_frame`] <<- TRUE
      },
      error = function(e) {
        Test_value[1, `Continue number in data_frame`] <<- FALSE
      }
    )
    names(Test_value)[`Continue number in data_frame`] <<- `Name of the tested tool`
    
#Example:
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
<a name="Test_Output"></a>
###[`Test_Output`](https://github.com/RRobert92/ASGA/blob/main/ASGA/tests/Tests.R)
The part of the `Test_Unit` is designed for testing of the core functionality of the ASGA as well as testing tools output

Usage of `Test_Output`:
```
Test_df[1, 'Continue number in data_frame'] <<- Test_anaysis('Data frame tool output names', 
                                                             'TRUE/FASLE value if P1 and P2 data exist', 
                                                             'Expected number of columns', 
                                                             'List of expected number of row')
#Example:
Test_df[1, 10] <<- Test_anaysis("Data_1_LD", 
                                TRUE, 
                                6, 
                                c(4,2,2))

```
<a name="Test_Result"></a>
###[`Test_Result`](https://github.com/RRobert92/ASGA/blob/main/ASGA/tests/Test_Output.R)
The part of the `Test_Unit` that gave two types of GUI message to the user:
  
  - Pass all test correctly <- Green
  - Error occurred <- Yellow and error number indicating `Continue number in data_frame`
