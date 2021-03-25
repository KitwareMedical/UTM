import os
import argparse


##Run script
parser = argparse.ArgumentParser(description=('Perform On OASIS-1 data.'
        ' See download-oasis-1.py for obtaining data.'))
parser.add_argument('--output_folder', required=True,
        help='output path to store files to')
args = parser.parse_args()

output_folder = args.output_folder


#Folders for different preprocessing options and intermediate images
preprocess_folder = "{0}/gray".format(output_folder)

scales = {
 "gray_scale_1" : "oasis-1-scale-1.yaml",
 "gray_scale_2" : "oasis-1-scale-2.yaml",
 "gray_scale_3" : "oasis-1-scale-3.yaml"
}

for f in scales.keys():
  try:
    os.mkdir("{0}/{2}".format(output_folder, f))
  except FileExistsError:
    pass

#Modified input csv file to match R script expectations
csv_out_file = "{0}/data.csv".format(output_folder)

for key, value in scales.items():
  os.system(("Rscript ../Scripts/run.utm.barycenter.R {3}/gray {1}"
             " --working.folder {2}/results/{3}"
             " --config {0}" ).format(value, csv_out_file, output_folder, key ) )

if __name__ == '__main__':
  main()
