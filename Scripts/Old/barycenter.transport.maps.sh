
if [ "$#" -ne 7 ]; then
    echo "Usage $0 GMRA-File Barycenter-File TransportMaps-File Feature-File-Pattern Variables-File  massCost transportType"
    return 1
fi
_GMRA_FILE=$1
_BARYCENTER_FILE=$2
_TRANSPORT_MAPS_FILE=$3
_FEATURE_FILE_PATTERN=$4
_VAR_FILE=$5
_MASS_COST=$6
_TRANSPORT_TYPE=$7


#Storage for analysis results and intermediate steps


N=$(find $IMAGES_PATH -maxdepth 1 -type f -print | wc -l)
echo $N


echo "----Computing Optimal Transport Maps to Barycenter----"
#Compute transport maps to mean
parallel Rscript $SCRIPTFOLDER/Processing/mean.transport.unscaled.R $_MASS_COST $_TRANSPORT_TYPE $_GMRA_FILE $_BARYCENTER_FILE $_TRANSPORT_MAPS_FILE ::: $(seq 1 1 $N)


echo "----Computing Allocation and Transport Cost Images----"
#Compute transport mass allocation and transport cost images
Rscript $SCRIPTFOLDER/Processing/create.images.R $_TRANSPORT_MAPS_FILE $_GMRA_FILE $_FEATURE_FILE_PATTERN $_VAR_FILE


