
if [ "$#" -lt 10 ]; then
    echo "Usage $0 Feature-File-Pattern Variables-File Analysis-Path AnalysisCor-File Analysis-Table nPermutations p-Value-Threshold Barycenter-Euclidean-File Barycenter-OT-File analysisFlag slice-axial slice-coronal slice-sagital"
    return 1
fi
_FEATURE_FILE_PATTERN=$1
_VAR_FILE=$2
_ANALYSIS_PATH=$3
_ANALYSIS_COR_FILE=$4
_ANALYSIS_TABLE=$5
_NPERMUTATIONS=$6
_THRESHOLD=$7
_BARYCENTER_EUCLIDEAN_FILE=$8
_BARYCENTER_OT_FILE=$9
_ANALYSIS_FLAG=${10}
if [ "$#" -gt 9 ]; then
_AXIAL_SLICE=${11}
_CORONAL_SLICE=${12}
_SAGITAL_SLICE=${13}
else
_AXIAL_SLICE=0
_CORONAL_SLICE=0
_SAGITAL_SLICE=0
fi


echo ""
echo "---Running Processing Barycenter---"

SCRIPTFOLDER=$(dirname $BASH_SOURCE)
echo $SCRIPTFOLDER 

#Create  Folders
if [ ! -d "$_ANALYSIS_PATH" ]; then
  mkdir $_ANALYSIS_PATH
fi


N=$(find $IMAGES_PATH -maxdepth 1 -type f -print | wc -l)
echo $N

if [ $_ANALYSIS_FLAG -ge 1 ]; then
echo "----Perfroming Correlation Analysis----"
#Perform correlation analysis
Rscript $SCRIPTFOLDER/Analysis/analyse.images.cor.R $_FEATURE_FILE_PATTERN $_VAR_FILE $_ANALYSIS_PATH $_ANALYSIS_COR_FILE $_NPERMUTATIONS
fi

echo "----Extracting 2D Slices from Correlation Analysis----"
#Save images from correlation analysis
Rscript $SCRIPTFOLDER/Analysis/extract.slice.cor.R $_ANALYSIS_COR_FILE $_VAR_FILE $_ANALYSIS_PATH $_THRESHOLD $_BARYCENTER_EUCLIDEAN_FILE $_BARYCENTER_OT_FILE "$_AXIAL_SLICE" "$_CORONAL_SLICE" "$_SAGITAL_SLICE" 


echo "----Creating Correlation Analsysis PDF----"
Rscript $SCRIPTFOLDER/Analysis/cor.table.R $_VAR_FILE $_ANALYSIS_PATH $_ANALYSIS_TABLE 
cd $_ANALYSIS_PATH
pdflatex $_ANALYSIS_TABLE 
cd ..

#Perform tsne analysis
#Rscript $SCRIPTFOLDER/Analysis/analyse.tsne.R $TRANSPORT_GRIDS_PATH $VAR_PATH $ANALYSIS_PATH $ANALYSIS_TSNE_FILE

