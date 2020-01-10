# MT Curvature Along Spindle Axis
Analyzing the changes in the KMTs curvature along the spindle pole axis 

## Getting Started
This program calculates how the curvature of given MTS (KMTs) is changing along the spindle axis .<br/>
For this You need exported data from ZIB Amira, and formatted into .xlsx file using excel

## Data preparation
Data also should be prepared prior to the analysis<br/>
      - Preferably data should be resampled for 1000A ~ 0.1um and cleaned out for any MTs/KMTs which are not fully in the volume<br/>
      - Fiber which does not have all KMTs should be deleted for the best accuracy of the program.<br/>
      - Pole coordinate should be marked as a Node and named Pole1 and Pole2<br/>
      - Also, spindle poles should be aligned in a way that Pole1 correspond to the bottom of the spindle and Pole2 to the top (*this will be fixed later)<br/>
      - For proper calculation of the relative position, the spindle pole-to-pole axis should be aligned with the spindle Y-axis <br/>
        - This can be done with "Transfor Editor" in Zib Amira<br/>
      - The last requirement is to label all fiber with names PoleX_YY<br/>
      
        X -> Pole 1 or 2
        YY -> number of the fiber
        
        Example
        Pole1_00, Pole1_01 etc.
        
## Using the code
Up to now, the program is analog and require user interaction upon uploading your data.

      lines 4-8 -> dir of the file
      lines 339-340 -> names and dir for the output files
