#!/usr/bin/env Rscript
library("optparse", quietly=TRUE)
library("stringr", quietly=TRUE)

option_list = list(
  #make_option(c("-i", "--images"), type="character", default=NULL, 
  #            help="Folder containing images to analyze", metavar="character"),
	#make_option(c("-v", "--variables"), type="character", default=NULL, 
  #            help="Variables to analyze.\n 
  #                  CSV file with header row. \n
  #                 One column with name containing the image names. \n
  #                 Remaining columns variables with a numeric entry for each image.", metavar="character"),
  make_option(c("-p", "--permutations"), type="integer", default=10000, 
              help="Number of permutations for p-Value testing. [default 10000]", metavar="integer"),
  make_option(c("-t", "--threshold"), type="double", default=0.05, 
              help="Threshold for p-Value signficance. default=[0.05]", metavar="double"),
  make_option(c("-a", "--analysisfolder"), type="character", default="Analysis", 
              help="Folder to store results in. [default=Analysis]", metavar="character"),
  make_option(c("-d", "--dimension"), type="integer", help="Image dimension", metavar="integer"),
  make_option(c("-f", "--analysisflag"), type="integer", default=4, 
              help="Analysis steps to perform [default=4]\n
                    >3 - All \n
                    3 - Do not recmpute setup step \n
                    2 - Do not recompute barycneter \n
                    1 - Do not recompute transport maps \n
                    0 - Do not recompute correlation images", metavar="integer"),
  make_option(c("-s", "--sigma"), type="double", default=0.0, 
              help="Smoothing of feature images [default=0.0]", metavar="double"),
  make_option("--workingfolder", type="character", default=".", 
              help="Working directory folder [default=.]", metavar="character"),
  make_option("--gmrafolder", type="character", default="GMRA", 
              help="Folder for storing GMRA files [default=GMRA]", metavar="character"),
  make_option("--atlasfolder", type="character", default="Atlas", 
              help="Folder for storing Atlas data [default=Atlas]", metavar="character"),
  make_option("--featuresfolder", type="character", default="Features", 
              help="Subfolder in Analysis for storing feature images [default=Features]", metavar="character"),
  make_option("--barycenterfolder", type="character", default="Barycenter", 
              help="Subfolder in Analysis for storing barycenters [default=Barycenter]", metavar="character"),
  make_option("--jacobianfolder", type="character", default="Jacobians", 
              help="Subfolder in Analysis for storing Jacobians [default=Jacobians]", metavar="character"),
  make_option( "--axial", type="double", default=0.4, 
              help="Axial slice percentage for report [default=0.4]",metavar="double" ),
  make_option( "--coronal", type="double", default=0.4, 
              help="Coronal slice percentage for report [default=0.6]",metavar="double" ),
  make_option( "--sagittal", type="double", default=0.4, 
              help="Sagittal slice percentage for report [default=0.6]",metavar="double" )
) 


pos.args.help = "  
  Image-Folder 
      Folder containing images to analyze


  Variables-CSV-File 
      Variables to analyze.
      CSV file with header row.
      One column with name containing the image names.
      Remaining columns variables with a numeric entry for each image"

      
opt_parser = OptionParser( option_list=option_list, 
                           usage = "usage: %prog Image-Folder Variables-CSV-File [options]", 
                           description=pos.args.help)
opts <- try( parse_args(opt_parser, positional_arguments=2), silent=FALSE )

if( inherits(opts, "try-error") ){
  print_help( opt_parser )
  quit( "no", 1)
}


#Script folder
script.file <- sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1])
if (.Platform$OS.type == "windows") {
  script.file <- gsub("\\\\", "\\\\\\\\", prog)
}
script.file = normalizePath(script.file)
script.folder <- dirname(script.file)

config = opts$options
config$imagefolder = opts$args[1]
config$variables = opts$args[2]

#steup intermediate files and folders
config$gmrafilepattern = sprintf("./%s/gmra%s.gmra", config$gmrafolder, "%04d" )
config$gmrafilebase = sprintf("./%s/gmra", config$gmrafolder )

config$pointsfilepattern = sprintf("./%s/points%s.Rdata", config$pointsfolder, "%04d" )

#config$barycenterfolder = sprintf("./%s/%s/", config$analysisfolder, config$barycenterfolder)

config$barycenters = list()
config$barycenters$euclidean = sprintf("./%s/barycenter-euclidean.Rdata", config$barycenterfolder)
config$barycenters$otdiscrete = sprintf("./%s/barycenter-ot-discrete.Rdata", config$barycenterfolder)
config$barycenters$ot = sprintf("./%s/barycenter-ot.Rdata", config$barycenterfolder)
config$barycenters$otsolution = sprintf("./%s/barycenter-ot-solution.Rdata", config$barycenterfolder)

config$transportfolder = sprintf("./%s/%s/", config$analysisfolder, config$transportfolder)
config$transportfilepattern = sprintf("./%s/trp-%s.Rdata", config$transportfolder, "%04d" )

config$featuresfolder = sprintf("./%s/%s/", config$analysisfolder, config$featuresfolder)
config$featuresfilepattern = sprintf("./%s/features%s.Rdata", config$featuresfolder, "%04d" )


config$correlationfile = sprintf("./%s/cor-vbm-utm.Rdata", config$analysisfolder) 
config$variablesfile = "./variables.Rdata"
config$pdffile = "cor-tables.tex"

config$nimages = length(list.files(config$imagefolder, "*.png"))  +
                 length(list.files(config$imagefolder, "*.nrrd")) +
                 length(list.files(config$imagefolder, "*.hdr"))  +
                 length(list.files(config$imagefolder, "*.nii"))


config$resultsfolder = sprintf("./%s/Images/", config$analysisfolder)
config$slicesfolder = sprintf("./%s/Slices/", config$analysisfolder)

config$imagefolder = normalizePath( config$imagefolder) 
config$variables = normalizePath( config$variables)

print("Working Folder: " )


#Create directories
dir.create( config$workingfolder, showWarnings=FALSE  )
setwd( config$workingfolder )
dir.create( config$analysisfolder, showWarnings=FALSE )
dir.create( config$featuresfolder, showWarnings=FALSE  )
dir.create( config$atlasfolder, showWarnings=FALSE  )
dir.create( config$jacobianfolder, showWarnings=FALSE  )
dir.create( config$resultsfolder, showWarnings=FALSE  )
dir.create( config$slicesfolder, showWarnings=FALSE  )

config$atlasfolder = normalizePath(config$atlasfolder)
config$jacobianfolder = normalizePath(config$jacobianfolder)


#Compute wasserstein barycenter
if( config$analysisflag >= 3){
  
  print("-- Computing Template--") 
  buildtemplateparallel.sh -d $DIMENSION -o ../Atlas/ -j 5 -c 0 -i $INITIAL_TEMPLATE $IMAGES
  system2("buildtemplateparallel.sh", c( "-d", config$dimension, 
                                         "-o", config$atlasfolder,
                                         "-j", 6,
                                         "-c", 0,
                                         "-i" config$template,
                                         sprintf("%S/*", config$imagefolder) )
  )
}



if( str_to_lower( config$barycentertype ) == "wasserstein" ){
  barycenter.file = config$barycenters$otdiscrete
}else{
  barycenter.file = config$barycenters$euclidean
}

#compute transport maps to barycenter create allocation ans transport feature images
if( config$analysisflag >= 2){

  print("-- Computing transport maps to barycenter --") 
  system2("parallel", paste( c( "Rscript", sprintf("%s/Processing/mean.transport.unscaled.R", script.folder),
                          config$cost,
                          config$massbalancing,
                          config$gmrafilepattern,
                          barycenter.file,
                          config$transportfilepattern,
                          ":::",
                          sprintf("%d", 1:config$nimages) ), collapse=" " )  )

  print("-- Extracting mass allocation and transport cost images from transport maps --") 
  system2("Rscript", c( sprintf("%s/Processing/create.images.R", script.folder),
                          config$transportfilepattern,
                          config$gmrafilepattern,
                          config$featuresfilepattern,
                          config$variablesfile, 
                          config$sigma ) )

}


#Compute correlations
if( config$analysisflag >= 1 ){
  print("-- Computing correlation images --") 
  system2("Rscript", c( sprintf("%s/Analysis/analyse.images.cor.R", script.folder),
                          config$featuresfilepattern,
                          config$variablesfile,
                          config$resultsfolder,
                          config$correlationfile,
                          config$permutations) )

}


threshold = 1 - config$threshold
if(threshold > 1){
  threshold = 1
}

#Extract 2D images if necessary or store 2D in png format for creating a repeor
print("-- Extracting slices for report --") 
system2("Rscript", c( sprintf("%s/Analysis/extract.slice.cor.R", script.folder),
                          config$correlationfile,
                          config$variablesfile,
                          config$slicesfolder,
                          threshold,
                          config$barycenters$euclidean,
                          barycenter.file,
                          config$axial,
                          config$coronal,
                          config$sagittal) )

#Create pd report
print("-- Generating report --") 
system2("Rscript", c( sprintf("%s/Analysis/cor.table.R", script.folder),
                          config$variablesfile,
                          config$slicesfolder,
                          config$pdffile) )

setwd(config$slicesfolder)
system2("pdflatex", c("--interaction=batchmode", config$pdffile, " > /dev/null")  )
system2("mv", c("*.pdf", "../" ) )
setwd("..")

