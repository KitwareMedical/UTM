## What Is This?

This is a display of different types of voxelwise correlation analysis. The
pixelwise correlation results from analysis of a population of n subjects /
images with associated variables.  The correlation at each voxel indicates
whether a change in that voxel is correlated with the varibale of interest. 


## What Do I See?

The background image, in grayscale, show the average tissue amount ay each
voxel of the n subjects.  Overlayed on the background are the strength of the
correlation at voxels below the p-Value threshold, which can be adjusted. 


The interpretation of the correlation depends on the type of Analysis selected.

### Analysis Types



There are three different analysis results:

1. Intensity: Classical voxel based morphometry.
2. Allocation: Indicates tissue loss or gain using optimal transport mophometric analysis.
3. Transport: Indicates movement of tissue using  optimal transport mophometric analysis.

#### Intensity Interpretation

This analysis directly correlates the intensity at a particulr voxel with the
variable. Thus a postive correlation indicates that an increase in intensity is
correlated with the variable. E.g. if the inout images are white matter masks a
positive correlations that at that voxek an increase in white matter is
correlated with the variable of interest. Similarly for CSF this would indicate
a less clear fluid.


#### Allocation Interpretation

The interpretation of allocation is similar to intensity. Instead of directly 
correlating intensity at a voxel an intermediate step computes a value at each 
voxel for each subject that corresponds to whether, at that voxel, tissue was 
added or removed with respect to average. Without further constraints this would 
be equivivalent to the intensity interpretation. However, mass is only added if 
the subject has overall more mass with respect to teh average and vice versa only
removed if the subject has less mass overall. 

The mass for each subject is added or removed such that it is in a strict mathematical 
sense optimal. Namely such that if the subject and the average with mass added or 
removed can be matched with minimla effort, i.e. minimal transport of tissue.

#### Transport Interpretation

The transport interpretation stems from the above mass allocation deifntion of matching
a subject to the the mass equalized average. The transport image shows were mass was moved 
from (positive voxel sign) and where mass was moved to (negative voxel sign). Thus, a 
positive correlation indicates a large movement of mass from that location and a negative 
correlation a large movement of mass to that location.

## Interactions


### Sliders

-  Slices: Select slice to view
-  P-Value Threshold: Only correlations with p-Value below the threshold are overlaid on the background image
-  Correlation Window Level: Adjust minimum and maximum of the colorscale for the corrleation display
-  Transparency: Transparency of the correlation display overlay


### Mouse Interactions

Clicking on a location in any slice will move the slice selection to that location and display the corresponding anatomical label from the Talairach atlas. Selecting a label from the anaomtical hierarchy will highlight that region. 
