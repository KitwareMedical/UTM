import concurrent.futures
import csv
import os
import subprocess
import argparse
import preprocessing.standard_ants as preprocessing
import preprocessing.create_feature_input as feature_input
import ants
import numpy as np
import pandas
import ants
from shutil import copyfile

def process_one(row):
  #Register to atlas and segment image
  row=row[1]
  patient_id = row["name"]

  input_image = "{0}/{1}/PROCESSED/MPRAGE/T88_111/{1}_mpr_n4_anon_111_t88_gfc.hdr".format(input_folder, patient_id)
  if not os.path.exists(input_image):
    print( "Image {0} not found".format(input_image) )
    return {}

  segmentation_image_file = "{0}/{1}/FSL_SEG/{1}_mpr_n4_anon_111_t88_masked_gfc_fseg.hdr".format(input_folder, patient_id)

  #Fix orientation (input are in analyze format,
  #  ants.image_read does not have orientation correct/available)
  oriented_image = "{0}/{1}.nii".format(intermediate_folders["oriented"], patient_id)
  im = ants.image_read(input_image)
  direction = im.direction
  direction[1, 1] = -1
  im.set_direction(direction)
  ants.image_write(im, oriented_image)

  #Registration to sri24
  args = argparse.Namespace
  args.image = oriented_image
  args.atlas = "../Atlas/sri24/templates/T1.nii"
  args.atlas_csf = "../Atlas/sri24/tpm/csf.nii"
  args.atlas_grey = "../Atlas/sri24/tpm/grey.nii"
  args.atlas_white = "../Atlas/sri24/tpm/white.nii"
  args.out_image = "./out_tmp.nii"
  args.out_jac_image = "{0}/{1}.nii".format(
          intermediate_folders["jacobians"], patient_id)
  args.out_tx = "{0}/{1}.nii".format(
          intermediate_folders["transforms"], patient_id)
  args.out_segmentation = "{0}/{1}.nii".format(
          intermediate_folders["segmentations"], patient_id)

  reg = preprocessing.standard_ants_preprocessing(args)


  #OASIS segmentaion with an affine transform only applied
  im_seg = ants.image_read(segmentation_image_file)
  im_seg.set_direction(direction)
  im_atlas = ants.image_read(args.atlas)
  tx = reg['fwdtransforms']
  print(tx)
  im_seg_affine = ants.apply_transforms( fixed=im_atlas, moving=im_seg,
                                         transformlist=tx[1])
  affine_seg_fname = "{0}/{1}.nii".format(
          intermediate_folders["segmentations_affine"], patient_id)
  ants.image_write( im_seg_affine, affine_seg_fname )

  #OASIS segmentation with full warp applied
  im_seg_warp = ants.apply_transforms( fixed=im_atlas, moving=im_seg,
                                         transformlist=tx )
  ants.image_write( im_seg_warp, args.out_segmentation )


  ## Extract and modulate by jacobian for warped segmentation
  args2 = argparse.Namespace
  args2.intensity_image = args.out_jac_image
  args2.segmentation_image = args.out_segmentation
  args2.downsample = 2

  #csf matter
  args2.segmentation_id = "1"
  args2.out_image = "{0}/{1}.nii".format(
          preprocess_folders["csf"], patient_id )
  feature_input.create_feature_input( args2 )

  #grey matter
  args2.segmentation_id = "2"
  args2.out_image = "{0}/{1}.nii".format(
          preprocess_folders["gray"], patient_id )
  feature_input.create_feature_input( args2 )

  #white matter
  args2.segmentation_id = "3"
  args2.out_image = "{0}/{1}.nii".format(
          preprocess_folders["white"], patient_id )
  feature_input.create_feature_input( args2 )


  ## Extract for affine only
  args2.intensity_image = None
  args2.segmentation_image = affine_seg_fname

  #csf matter
  args2.segmentation_id = "1"
  args2.out_image = "{0}/{1}.nii".format(
          preprocess_folders["csf_affine"], patient_id )
  feature_input.create_feature_input( args2 )

  #grey matter
  args2.segmentation_id = "2"
  args2.out_image = "{0}/{1}.nii".format(
          preprocess_folders["gray_affine"], patient_id )
  feature_input.create_feature_input( args2 )

  #white matter
  args2.segmentation_id = "3"
  args2.out_image = "{0}/{1}.nii".format(
          preprocess_folders["white_affine"], patient_id )
  feature_input.create_feature_input( args2 )


  ## Extract for oasis segmentation / no registration
  args2.intensity_image = None
  args2.segmentation_image = segmentation_image_file

  #csf matter
  args2.segmentation_id = "1"
  args2.out_image = "{0}/{1}.nii".format(
          preprocess_folders["csf_oasis"], patient_id )
  feature_input.create_feature_input( args2 )

  #grey matter
  args2.segmentation_id = "2"
  args2.out_image = "{0}/{1}.nii".format(
          preprocess_folders["gray_oasis"], patient_id )
  feature_input.create_feature_input( args2 )

  #white matter
  args2.segmentation_id = "3"
  args2.out_image = "{0}/{1}.nii".format(
          preprocess_folders["white_oasis"], patient_id )
  feature_input.create_feature_input( args2 )

  row2 = row
  row2["name"] = "{0}.nii".format(patient_id)
  return row2.to_dict()

def main():
  if not args.nopreprocess:
    #Do preprocessing
    data_out = pandas.DataFrame(columns=data.columns)
    with concurrent.futures.ProcessPoolExecutor(max_workers=10) as executor:
      for new_row in executor.map(process_one, data.iterrows(), chunksize=2):
        if new_row:
          data_out = data_out.append(new_row, ignore_index=True)

    data_out.to_csv(csv_out_file, index=False)

  #Call UTM script
  try:
    os.mkdir( "{0}/results".format(output_folder) )
  except FileExistsError:
    pass
  for key, value in preprocess_folders.items():
      #os.mkdir( "{0}/results/{1}".format(output_folder, key) )
      #copyfile("../Atlas/sri24/labels/atlas.Rdata", "{0}/results/{1}/atlas.Rdata".format(output_folder, key) )
      os.system(("Rscript ../Scripts/run.utm.barycenter.R {0} {1}"
                 " --working.folder {2}/results/{3}"
                 " --config oasis-1.yaml" ).format(value, csv_out_file, output_folder, key ) )

##Run script
parser = argparse.ArgumentParser(description=('Perform On OASIS-1 data.'
        ' See download-oasis-1.py for obtaining data.'))
parser.add_argument('--input_folder', required=True,
        help='path to folder containg oasis-1 data')
parser.add_argument('--input_csv', required=True,
        help='csv input file (oasis_cross-sectional.csv from download)',
        default="")
parser.add_argument('--output_folder', required=True,
        help='output path to store files to')
parser.add_argument('--nopreprocess', required=False,
        action="store_true", default=False)
args = parser.parse_args()

input_folder = args.input_folder
input_csv = args.input_csv
output_folder = args.output_folder


#Folders for different preprocessing options and intermediate images
preprocess_folders = {
 "white" : "{0}/white".format(output_folder),
 "gray"  : "{0}/gray".format(output_folder),
 "csf"   : "{0}/csf".format(output_folder),
 "white_affine" : "{0}/white_affine".format(output_folder),
 "gray_affine"  : "{0}/gray_affine".format(output_folder),
 "csf_affine"   : "{0}/csf_affine".format(output_folder),
 "white_oasis" : "{0}/white_oasis".format(output_folder),
 "gray_oasis"  : "{0}/gray_oasis".format(output_folder),
 "csf_oasis"   : "{0}/csf_oasis".format(output_folder)
}

intermediate_folders = {
 "transforms" : "{0}/transforms".format(output_folder),
 "segmentations" : "{0}/segmentations".format(output_folder),
 "segmentations_affine" : "{0}/segmentations_affine".format(output_folder),
 "jacobians" : "{0}/jacobians".format(output_folder),
 "oriented" : "{0}/oriented".format(output_folder)
}

for f in preprocess_folders.values():
  try:
    os.mkdir(f)
  except FileExistsError:
    pass

for f in intermediate_folders.values():
  try:
    os.mkdir(f)
  except FileExistsError:
    pass

#Modified input csv file to match R script expectations
csv_out_file = "{0}/data.csv".format(output_folder)
data = pandas.read_csv(input_csv, header=0)
data = data.rename(columns={"ID":"name"})

if __name__ == '__main__':
  main()
