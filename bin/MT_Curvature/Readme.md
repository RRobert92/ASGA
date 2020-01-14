# Analyzing the KMTs curvature

To retrieve a curvature data of the single KMTs. This tool determined which KMTs belong to which pole based on the labels created with Zib Amira. Nextly, the program is selecting information about MT length (calculated with Amira, and saved in the output file), and IDs of the Node #1 and Node #2, which are containing XYZ coordinates for the KMTs (+) and (-) ends. Finally, the program is calculating distance in 3D between these two Nodes (KMTs Length_y). The curvature is calculated as a ratio of total KMTs length (KMTs Length_x) and KMTs Length_y (see figure below).
As a final output, the user retrieving separate .xlsx file with a curvature ratio for each KMTs that belong to Pole_1 or Pole_2.

![KMTs_Curvature](/img/MT_Curvature/KMTs_Curvature.jpg)

## Data preparation
Data also should be prepared prior to the analysis<br/>
      - Preferably data should be cleaned out for any MTs/KMTs which are not fully in the volume<br/>
      - Fiber which does not have all KMTs should be deleted for the best accuracy of the program.<br/>
      - Pole coordinate should be marked as a Node and named Pole1 and Pole2<br/>
      - The last requirement is to label all fiber with names PoleX_YY<br/>
      
        X -> Pole 1 or 2
        YY -> number of the fiber
        
        Example
        Pole1_00, Pole1_01 etc.
        
## Getting Started
The tool is fully automatic and required for the user to only feed information about file directory which has to be analyzed and the name and directory of the output file.

      - lines 4 & 6 -> dir of the file
      - lines 84-85 -> names and dir for the output files
