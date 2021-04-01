# VolPA - Volumetric Population Analysis
![UTM Example](Figures/UTM-3D-example.png )
![UTM Example](Figures/UTM-3D-example-2.png )

This is a repository that contains scripts to run volmetric population analysis
on brain image data sets.  For a general introduction see 
[Brain morphometry](https://en.wikipedia.org/wiki/Brain_morphometry).

The scripts perfrom preprocessing, feature extraction and different statistical
analysis.

The scripts perfrom feature extraction using:
- Standard and modified VBM approaches 
  ([VBM](https://en.wikipedia.org/wiki/Voxel-based_morphometry))
- A novel approach based on optimal tansport 
  ([UTM](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6547365/)) that aims to mitgiate some concerns with VBM.

For both approaches different statistical analysises are available:

- **Voxel based correlation analysis**: Correlation of individual voxels<br/>
  [Example result visualization](https://sg-kitware.shinyapps.io/OASIS-1-GM/)
- **Component based analysis**<br/>
  [Example result visualization](https://sg-kitware.shinyapps.io/OASIS-1-GM-Components/)
  - PCA: Principal compnent based statistical models
  - SpatCA: Spatially regularized compnent based statsitical models 
    ([SpatCA](https://link.springer.com/chapter/10.1007/978-3-030-59728-3_65))
- **Parcelation based analysis**<br/>
  [Example result visualization](https://sg-kitware.shinyapps.io/OASIS-1-GM-Parcels/)

Depending on the feature extraction (VBM or UTM) the results have slightly different interpretations.

## References 
The analysis approach is based on the work in:
> Gerber S, Niethammer M, Styner M, Aylward S. 
> Exploratory Population Analysis with Unbalanced Optimal Transport. 
> Med Image Comput Comput Assist Interv. 2018  
> [Pubmed Link](https://pubmed.ncbi.nlm.nih.gov/31172134/)

A journal article with improvements and additions to the method is in progress.

The spatial component analysis approach is described in:
> Gerber, Samuel, and Marc Niethammer.  
> Spatial Component Analysis to Mitigate Multiple Testing in Voxel-Based Analysis.  
> International Conference on Medical Image Computing and Computer-Assisted Intervention 2020  
> [Link](https://link.springer.com/chapter/10.1007/978-3-030-59728-3_65)



## Running the UTM analysis:

There are three stages to running the scripts:
1. Preprocessing
   - See python folder
2. UTM analysis on extracted data
   - See Scripts folder
3. Visualization of results
   - See Scripts/Shiny

For a more detailed description see the documentatio in [doc folder](doc).


### Examples

To run the examples install the [requirements](#requirements-to-run-scripts) first 

For a self contained example see  
[run-example.py](python/run-example.py)  
The example preprocesses a set of images and passes the preprocessed images to the analysis script.

For toy example see [Example/Annulus](Example/Annulus)

For an example on the [OASIS-1 data set](https://www.oasis-brains.org/)  
[run-oasis-1.py](python/run-oasis-1.py)

## Visualization of the Results
The results are visualzed with [Shiny](https://shiny.rstudio.com/) applications in the folders
 - [Shiny](./Scripts/Shiny/app.R)
 - [ShinyParcels](./Scripts/ShinyParcels/app.R)
 - [ShinyComponents](./Scripts/ShinyComponents/app.R)
Each folder contains a *upload.to.shinyapps.R* for bundling of relevant files from the output of the main script.

## Requirements to Run Scripts

### Preprocessing
- python 3
- packages
  - antspy (make sure to install from source, the wheel is broken)
  - pandas
  - argparse
  - numpy
  - nibabel
  - dipy

### Analysis Steps
- R >= 3.6
- R packages (Packages folder contains a script to install all):
  - gmra
  - mop, gmra (contained in this repository)
  - data.table
  - lmvar
  - mmand
  - Rtsne
  - stringr
  - foreach
  - doParallel
  - RColorBrewer
  - optparse
  - pracma
  - yaml
  - ANTsR (requires devtools which depends on curl and git devel libraries)
- R packages dependencies: x11, gl/glu, libpng, curl, git dev libraries

In the Packages subfolder in Scripts are scripts to install all these packages
(in particular install-all.sh).  If that script fails due to not able to write
into the R library directory install a package manually from R in order to
create a local lib directory ( start R, run install.packages("optparse") )

### Visualization
- R
- R packages:
  - optparse
  - shiny
  - shinyBS
  - shinythemes
  - stringr
  - markdown
  - broom
  - corrplot
  - vtkwidgets
  - shinyWidgets


## Older Version

An older version that was used for the MICCAI 2018
[Unbalanced Optimal Transport for Exploratory Population Analysis](https://github.com/KitwareMedicalPublications/2018-MICCAI-UTM)
can be found in the Apps-Old folder.

The processing pipeline has been signifcantly changed. To run the older
examples from the paper you will ahve to use the correct paths to Scripts/Old
to amke this work again.

# Older Analysis Results
Analysis result with older prototype application.

## OASIS-1 data set
Data Set: https://www.oasis-brains.org/#oasis1

VBM and UTM (different levels of local and global) analysis of 
mini mental score, clinical dementia rating, age:
- White matter: https://sg-kitware.shinyapps.io/OASIS1-WhiteMatter
- Gray Matter: https://sg-kitware.shinyapps.io/OASIS1-GrayMatter
- CSF: https://sg-kitware.shinyapps.io/OASIS1-CSF

## IBIS data set
Data set: IBIS for study of Autsim development

VBM and UTM analysis of different groups 
(HRASD - High risk autisum, LRpos - Low risk positive, LRneg - Low risk negative)
- White Matter: https://sg-kitware.shinyapps.io/IBIS-Normalized-WhiteMatter
- Gray Matter: https://sg-kitware.shinyapps.io/IBIS-Normalized-GrayMatter
- CSF: https://sg-kitware.shinyapps.io/IBIS-Normalized-CSF

## OASIS-3 - data set
Data Set: https://www.oasis-brains.org/#oasis1

VBM and UTM:
- White Matter VBM only: https://sg-kitware.shinyapps.io/OASIS-3-WM
- White Matter: https://sg-kitware.shinyapps.io/OASIS3-WhiteMatter

[Spatial component based analysis](https://link.springer.com/chapter/10.1007/978-3-030-59728-3_65)
- https://sg-kitware.shinyapps.io/OASIS-3-WM-Components




