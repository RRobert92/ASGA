# Calculate k-fiber cross-section with polygon and circular approach 
To calculate the fiber area, this tool determined which KMTs belong to the fiber-based on the labels created with Zib Amira. Nextly from all KMTs in the fiber program is selecting leading KMTs based on its length and position of the minus ends to the pole. The leading KMTs are used to create cross-section planes of the fiber. In order to do that, the tool is selecting points on the leading KMTs (i + 5) and finding the closest point to it on each KMTs in the fiber. This ensures that selected points are in the right orientation plane of the fiber.
The selected points are used to create a median position of the fiber and calculate the area of the cross-section plane.
The tool is calculating both circular and polygon areas.<br/><br/>
      **Circular area**: is calculated by measuring a distance from a median point to each KMTs point in a given plane. The largest distance is used to the determined circular area (P=pi*r^2) where r = minimum distance that encloses all KMTs in the cross-section.<br/>
      **Polygon area**: is calculated by creating a polygon using a [3D alpha sphear method](https://graphics.stanford.edu/courses/cs268-11-spring/handouts/AlphaShapes/as_fisher.pdf). This creat a triangle .obj file of a polygon shape k-fiber cross-section. In order to calculate the area of any k-fiber cross-section, the tool is multiplying points and shifting them in XYZ direction by a fixed distance. This allows creating a prism with an n-side polygonal base.
      Used "alphashape3d" library, which allows calculating the volume of a prism. 
      Area of the k-fiber cross-section is measured by Area_prism = Volume_prism/ High_prism (which is a fixed distance of a shift)
      
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

      - lines 5, 7 & 12 -> dir of the file
      - lines 761-762 -> names and dir for the output files
