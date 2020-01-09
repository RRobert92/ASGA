# Analyzing the k-fiber curvature

To retrive curvature data of the whole fiber. This tool firstly cread for each fiber median position of all KMTs in the fiber.
This created median MT is then used to calculate full lenght of the fiber and the length between plus and minus end of the fiber.
As a final output,  user retrivind .xlsx file with curvature ratio and full length of th fiber

## Data preparation
Data also should be prepared prior to the analysis

      - Prefarable data should be resampled for 1000A ~ 0.1um, and cleaned out for any MTs/KMTs which are not fully in the volume
      - Fiber which does not have all KMTs, should be deleted for the best accuracy of the program.
      - Pole coordinate should be marked as a Node and named Pole1 and Pole2
      - Also spindle poles should be aligned in a way that Pole1 corespond to the bottom of the spindle and Pole2 to the top (*this will be fixed later)
      - For proper calculation of the relative position, spindle pole-to-pole axis, should be aligned with the spindle Y-axis 
        - This can be done with "Transfor Editior" in Zib Amira
      - The last requirement is to label all fiber with names PoleX_YY
        X -> Pole 1 or 2
        YY -> number of the fiber
        
        Example
        Pole1_00, Pole1_01 etc.
        
## Getting Started
Tool is fully automatic, and required for the user to only feed information about file directory which has to be analysed and the name and directory of the output file.

      - lines 5, 7 & 12 -> dir of the file
      - lines 365 -> names and dir for the output files
      
