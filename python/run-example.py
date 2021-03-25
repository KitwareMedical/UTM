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


def process_one(row):
  #Register to atlas and segment image
  row=row[1]
  args = argparse.Namespace
  args.image = "./example/input-data/{0}/RAW/{0}_mpr-1_anon.nii".format(row["ID"])
  args.atlas = "../Atlas/sri24/templates/T1.nii"
  args.atlas_csf = "../Atlas/sri24/tpm/csf.nii"
  args.atlas_grey = "../Atlas/sri24/tpm/grey.nii"
  args.atlas_white = "../Atlas/sri24/tpm/white.nii"
  args.out_image = "./example/warped/{0}.nii".format(row["ID"])
  args.out_tx = "./example/transforms/{0}".format(row["ID"])
  args.out_jac_image = "./example/jacobians/{0}.nii".format(row["ID"])
  args.out_segmentation = "./example/segmentations/{0}.nii".format(row["ID"])

  reg = preprocessing.standard_ants_preprocessing(args)

  print( reg['fwdtransforms'] )
  print( reg['invtransforms'] )

  #Extract white matter and modulate by jacobian
  args2 = argparse.Namespace
  args2.out_image = "{0}/{1}.nii".format(preprocess_folder, row["ID"])
  args2.segmentation_id = "3"
  args2.downsample = 2
  args2.intensity_image = args.out_jac_image
  args2.segmentation_image =  args.out_segmentation

  feature_input.create_feature_input(args2)

  row2 = row
  row2["ID"] = "{0}.nii".format(row["ID"])
  return row2

def main():
  #Do preprocessing
  if not args.nopreprocess:
    data_out = pandas.DataFrame(columns=data.columns)
    with concurrent.futures.ProcessPoolExecutor() as executor:
      for new_row in executor.map(process_one, data.iterrows(), chunksize=2):
        data_out = data_out.append(new_row)

    #R UTM scripts expects a name column that identifes images in teh preprocess_folder
    data_out = data_out.rename(columns={"ID" : "name"})
    data_out.to_csv(csv_out_file, index=False)

  #Call UTM script
  try:
    os.mkdir("./example/results")
  except FileExistsError:
    pass
  os.system( "Rscript ../Scripts/run.utm.barycenter.R {0} {1}"
              " --working.folder ./example/results"
              " --config example.yaml".format(preprocess_folder, csv_out_file ) )

  #Run shiny voxel based visualization
  os.system( "cp ../Scripts/Shiny/* ./example/results" )
  os.system( "cp ../Scripts/ShinyVtkScripts/render.js ./example/results/render.js" )
  os.system( "Rscript ./example/results/app.R"
             " --workingfolder ./example/results/" )

  #Run shiny component based visualization
  #Fails because of to few observation to fit model
  #os.system( "cp ../Scripts/ShinyComponents/* ./example/results" )
  #os.system( "Rscript ./example/results/app.R --workingfolder ./example/results/" )


#Create folders for storing preprocessed images and intermediate images
preprocess_folder = "./example/preprocessed-input-data"
try:
  os.mkdir(preprocess_folder)
except FileExistsError:
  pass
try:
  os.mkdir("./example/transforms")
except FileExistsError:
  pass
try:
  os.mkdir("./example/jacobians")
except FileExistsError:
  pass
try:
  os.mkdir("./example/segmentations")
except FileExistsError:
  pass
try:
  os.mkdir("./example/warped")
except FileExistsError:
  pass

#Note input file is a subsect of the oasis_cross_section.csv
csv_in_file = './example/input-data/oasis_cross-sectional.csv'
data = pandas.read_csv(csv_in_file, header=0)
#Create the data csv file that the R UTM processing script expects
csv_out_file = "./example/data.csv"

parser = argparse.ArgumentParser(description='Perform Example UTM Analysis on small set of images')
parser.add_argument('--nopreprocess', required=False, action="store_true", default=False)
args = parser.parse_args()

if __name__ == '__main__':
  main()
