# Analyzing the k-fiber curvature & length

To retrieve a length and curvature data of the whole fiber. This tool determined which KMTs belong to the fiber-based on the labels created with Zib Amira. Nextly from all KMTs in the fiber program is selecting leading KMTs based on its length and position of the minus ends to the pole. The leading KMTs are used to create cross-section planes of the fiber. In order to do that, the tool is selecting points on the leading KMTs (i   5) and finding the closest point to it on each KMTs in the fiber. This ensures that selected points are in the right orientation plane of the fiber.
The selected points are used to create a median position of the fiber and calculate the area of the cross-section plane.
This created median MT is then used to calculate the full length of the fiber and the length between plus and minus end of the fiber.
As a final output, the user retrieving .xlsx file with curvature ratio and the full length of the fiber.

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
Tool is fully automatic, and required for the user to only feed information about file directory which has to be analysed and the name and directory of the output file.

      - lines 5, 7 & 12 -> dir of the file
      - lines 365 -> names and dir for the output files
      
