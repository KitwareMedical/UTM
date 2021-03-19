
if [ "$#" -ne 8 ]; then
    echo "Usage $0 GMRA-Pattern Barycenter-File Barycenter-Discrete-File Barycenter-Euclidean-File Barycenter-TransportMaps-File Barycenter-Solution-File Transport-Type Mass-Cost"
    return 1
fi
_GMRA_FILE_PATTERN=$1
_BARYCENTER_OT_FILE=$2
_BARYCENTER_OT_DISCRETE_FILE=$3
_BARYCENTER_EUCLIDEAN_FILE=$4
_TRANSPORT_MAPS_FILE=$5
_BARYCENTER_OT_SOLUTION_FILE=$6
_TRANSPORT_TYPE=$7
_MASS_COST=$8

SCRIPTFOLDER=$(dirname $BASH_SOURCE)
echo $SCRIPTFOLDER 


echo ""
echo "----Computing Optimal Transport Barycenter----"
Rscript $SCRIPTFOLDER/Processing/barycenter.transport.R $_GMRA_FILE_PATTERN $_BARYCENTER_OT_DISCRETE_FILE $_BARYCENTER_OT_FILE $_TRANSPORT_MAPS_FILE $_BARYCENTER_EUCLIDEAN_FILE $_BARYCENTER_OT_SOLUTION_FILE $_TRANSPORT_TYPE $_MASS_COST


