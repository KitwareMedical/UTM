#!/bin/sh


IMAGES=$1
INITIAL_TEMPLATE=$2
DIMENSION=$3
FOLDER=$4
SCRIPTFOLDER=$5

echo $0 $1 $2 $3


mkdir "../TBM"
mkdir "../TBM/Atlas"

#buildtemplateparallel.sh -d $DIMENSION -o Atlas -j 5 -c 2 *.png
#mv *.nii.gz ../TBM/Atlas/
#ConvertImage $DIMENSION "../TBM/Atlas/Atlastemplate.nii.gz" "../TBM/Atlas/template.nrrd" 0


#mkdir ../TBM/Jacobians
#for  filename in ../TBM/Atlas/*Warp.nii.gz; do
#  BASE=$(basename "$filename" .nii.gz)
#  echo $BASE
#  CreateJacobianDeterminantImage $DIMENSION  $filename "../TBM/Jacobians/${BASE}-det.nii.gz"
#  ConvertImage $DIMENSION "../TBM/Jacobians/${BASE}-det.nii.gz" "../TBM/Jacobians/${BASE}-det.nrrd" 0
#done  
#rm ../TBM/Jacobians/*template*
#rm ../TBM/Jacobians/*InverseWarp*


mkdir ../TBM/Features
mkdir ../TBM/Features/ANTS-TBM
RScript "$SCRIPTFOLDER/ANTS-TBM/create.images.R" ../TBM/Jacobians  "../TBM/barycenter.Rdata" "../TBM/Features/ANTS-TBM/features%04d.Rdata" "../TBM/Atlas/template.nrrd" "../variables.csv" "../TBM/Atlas/variables.Rdata" 1


mkdir ../TBM/Correlations
RScript "$SCRIPTFOLDER/Analysis/analyse.images.cor.R" "../TBM/Features/"  "../TBM/Atlas/variables.Rdata"  "../TBM/Correlations" "../TBM/Correlations/cor-tbm.Rdata" 10000 7 

RScript "$SCRIPTFOLDER//Analysis/extract.slice.cor.R" "../TBM/Correlations/cor-tbm.Rdata" "../TBM/Atlas/variables.Rdata"  "../TBM/Correlations" 0.95 "../TBM/barycenter.Rdata" "../TBM/barycenter.Rdata"
