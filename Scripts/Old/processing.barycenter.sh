#!/bin/bash

if [ "$#" -lt 8 ]; then
    echo "Usage $0  
                             Images-Folder 
                             Variables-Table 
                             Number-of-Permutations 
                             p-Value-Threshold 
                             Mass-Cost 
                             Transport-Type 
                             Analysis-Path 
                             Analysis-Flag
                             [Slice-Axial] 
                             [Slice-Coronal] 
                             [Slice-Sagital] "
    exit 1
fi
IMAGES_PATH=$1
VAR_TABLE=$2
NPERMUTATIONS=$3
THRESHOLD=$4
MASS_COST=$5
TRANSPORT_TYPE=$6
ANALYSIS_PATH=$7
ANALYSIS_FLAG=$8
if [ "$#" -gt 8 ]; then
AXIAL_SLICE=$9
CORONAL_SLICE=${10}
SAGITAL_SLICE=${11}
else
AXIAL_SLICE=0
CORONAL_SLICE=0
SAGITAL_SLICE=0
fi


echo ""
echo "---Running Processing Barycenter---"

GMRA_PATH="./GMRA"
GMRA_FILE="$GMRA_PATH/gmra%04d.gmra"
GMRA_FILE_PATTERN="$GMRA_PATH/gmra"

POINTS_PATH="./Points"
POINTS_FILE="$POINTS_PATH/points%04d.Rdata"

FEATURES_PATH="./Features"
FEATURES_FILE="$FEATURES_PATH/features%04d.Rdata"

BARYCENTER_FOLDER="./Barycenters"
if [ ! -d "$BARYCENTER_FOLDER" ]; then
  mkdir $BARYCENTER_FOLDER
fi

BARYCENTER_EUCLIDEAN_FILE="$BARYCENTER_FOLDER/barycenter-euclidean.Rdata"
BARYCENTER_OT_DISCRETE_FILE="$BARYCENTER_FOLDER/barycenter-ot-discrete.Rdata"
BARYCENTER_OT_FILE="$BARYCENTER_FOLDER/barycenter-ot.Rdata"
BARYCENTER_OT_SOLUTION_FILE="$BARYCENTER_FOLDER/barycenter-ot-solution.Rdata"

VAR_FILE="./variables.Rdata"

TRANSPORT_MAPS_BARYCENTER_PATH="$ANALYSIS_PATH/BarycenterTransportMaps"
TRANSPORT_MAPS_BARYCENTER_FILE="$TRANSPORT_MAPS_BARYCENTER_PATH/trp-%04d.Rdata"


#Create  Folders
if [ ! -d "$ANALYSIS_PATH" ]; then
  mkdir $ANALYSIS_PATH
fi

if [ ! -d "$TRANSPORT_MAPS_BARYCENTER_PATH" ]; then
  mkdir $TRANSPORT_MAPS_BARYCENTER_PATH
fi

if [ ! -d "$FEATURES_PATH" ]; then
  mkdir $FEATURES_PATH
fi

if [ ! -d "$GMRA_PATH" ]; then
  mkdir $GMRA_PATH
fi

if [ ! -d "$POINTS_PATH" ]; then
  mkdir $POINTS_PATH
fi




SCRIPTFOLDER=$(dirname $BASH_SOURCE)
echo $SCRIPTFOLDER 


if [ $ANALYSIS_FLAG -ge 4 ]; then
#Create point clouds and grma trees
source $SCRIPTFOLDER/setup.sh $IMAGES_PATH $VAR_TABLE $VAR_FILE $POINTS_FILE $GMRA_FILE $BARYCENTER_EUCLIDEAN_FILE
fi

if [ $ANALYSIS_FLAG -ge 3 ]; then
#Compute ot barycenter
source $SCRIPTFOLDER/barycenter.compute.sh $GMRA_FILE_PATTERN $BARYCENTER_OT_FILE $BARYCENTER_OT_DISCRETE_FILE $BARYCENTER_EUCLIDEAN_FILE $TRANSPORT_MAPS_BARYCENTER_FILE $BARYCENTER_OT_SOLUTION_FILE $TRANSPORT_TYPE $MASS_COST
fi



#Analysis with transport type source unbalanced
ANALYSIS_COR_FILE="$ANALYSIS_PATH/cor-utm-vbm.Rdata"
ANALYSIS_TABLE="cor-tables.tex"

TRANSPORT_MAPS_PATH="$ANALYSIS_PATH/TransportMaps"
TRANSPORT_MAPS_FILE="$TRANSPORT_MAPS_PATH/trp-%04d.Rdata"

if [ ! -d "$TRANSPORT_MAPS_PATH" ]; then
  mkdir $TRANSPORT_MAPS_PATH
fi

if [ $ANALYSIS_FLAG -ge 2 ]; then
#compute transport map to barycenter
source $SCRIPTFOLDER/barycenter.transport.maps.sh $GMRA_FILE $BARYCENTER_OT_DISCRETE_FILE $TRANSPORT_MAPS_FILE $FEATURES_FILE $VAR_FILE $MASS_COST $TRANSPORT_TYPE
fi

source $SCRIPTFOLDER/analysis.sh $FEATURES_FILE $VAR_FILE $ANALYSIS_PATH $ANALYSIS_COR_FILE $ANALYSIS_TABLE $NPERMUTATIONS $THRESHOLD $BARYCENTER_EUCLIDEAN_FILE $BARYCENTER_OT_DISCRETE_FILE $ANALYSIS_FLAG $AXIAL_SLICE $CORONAL_SLICE $SAGITAL_SLICE





