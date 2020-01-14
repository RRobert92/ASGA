# Analyzing the KMTs curvature

To retrieve a curvature data of the single KMTs. This tool determined which KMTs belong to which pole based on the labels created with Zib Amira. Nextly, the program is selecting information about the IDs of the points associated with each KMTs. These ID points are containing XYZ coordinates of the KMTsposition in the 3D space. The relative position of each point on the Pole-to-Pole axis is calculated in a way, that kinetochore position starts as (1) and pole position finished at (0) (Figure 1).<br/>

![Relative_position](/img/MT_Curvature_Along_Spindle_Axis/Relative_Position.jpg)<br/>

**Figure 1:** Illustration presenting calculation of the relative position for each KMTs at the spindle pole to kinetochore axis.<br/><br/>
As a final step, the tool is selecting points on the each KMT (i + 5, each 500 nm), calculating the distance between selected points (KMTs Length_y), and the full length between selected points (KMTs Length_x). The curvature is calculated as a ratio between KMTs Length_x and KMTs Length_y (Figure 2).
As a final output, the user retrieving separate .xlsx file with a curvature ratio and their relative position at the spindle pole-to-kinetochore axis for each KMTs that belong other to Pole_1 or Pole_2.<br/>

![KMTs_Curvature](/img/MT_Curvature_Along_Spindle_Axis/MT_Curvature_Along_Spindle_Axis.jpg)<br/>
**Figure 2:** Illustration presenting KMTs curature distribution analysis along spindle pole to kinetochore. <br/><br/>

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
        
## Getting Started
The tool is fully automatic and required for the user to only feed information about file directory which has to be analyzed and the name and directory of the output file.

      - lines 4, 6 & 8 -> dir of the file
      - lines 338-339 -> names and dir for the output files
