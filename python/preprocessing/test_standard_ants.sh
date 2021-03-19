#/usr/bin/sh

#image data from https://www.oasis-brains.org/#data (OASIS-1 disc 1)
#  includes csv with clinical data ( https://www.oasis-brains.org/files/oasis_cross-sectional.csv)
#
#atlas data from https://www.nitrc.org/frs/download.php/4841/sri24_spm8.zip
#
#NOTE: Image data from OASIS-1 is not loaded with correct orientation
#image data in example/input-data fixes oriantation thus using that data for the example

python standard_ants.py \
 --image ../example/input-data/OAS1_0001_MR1/RAW/OAS1_0001_MR1_mpr-1_anon.nii \
 --atlas ../example/input-data//Atlas/sri24/templates/T1.nii \
 --atlas_csf ../example/input-data//Atlas/sri24/tpm/csf.nii \
 --atlas_grey ../example/input-data/Atlas/sri24/tpm/grey.nii \
 --atlas_white ../example/input-data//Atlas/sri24/tpm/white.nii \
 --out_image out.nii \
 --out_jac_image out_jac.nii \
 --out_segmentation out_seg.nii \


python create_feature_input.py \
 --segmentation_image out_seg.nii \
 --segmentation_id 1 \
 --intensity_image out_jac.nii \
 --out_image out_feature_input.nii \

