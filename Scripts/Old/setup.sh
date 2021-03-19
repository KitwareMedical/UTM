
if [ "$#" -ne 6 ]; then
    echo "Usage $0 Images-Folder Variables-Table Variables-File Points-Pattern GMRA-Pattern Barycenter-Euclidean-File "
    return 1
fi
_IMAGES_PATH=$1
_VAR_TABLE=$2
_VAR_FILE=$3
_POINTS_FILE=$4
_GMRA_FILE=$5
_BARYCENTER_EUCLIDEAN_FILE=$6

SCRIPTFOLDER=$(dirname $BASH_SOURCE)
echo $SCRIPTFOLDER 

N=$(find $IMAGES_PATH -maxdepth 1 -type f -print | wc -l)
echo $N

echo "----Running Setup----"
Rscript $SCRIPTFOLDER/Processing/setup.R $_IMAGES_PATH $_VAR_TABLE $_VAR_FILE $_POINTS_FILE $_BARYCENTER_EUCLIDEAN_FILE     


echo "----Creating GMRA Trees----"
parallel Rscript $SCRIPTFOLDER/Processing/create.gmra.R $_GMRA_FILE $_POINTS_FILE ::: $(seq 1 1 $N)

