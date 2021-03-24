#!/usr/bin/env Rscript
suppressWarnings({
library(optparse, quietly=TRUE, warn.conflicts=F, verbose=FALSE)
library(stringr, quietly=TRUE, warn.conflicts=F, verbose=FALSE)
library(doParallel, quietly=TRUE, warn.conflicts=F, verbose=FALSE)
})

option_list = list(
  make_option("--working.folder", type="character", default=".",
              help="Working directory folder [default=.]", metavar="character"),
  make_option( c("--config"), type="character", default="",
               help="yaml configuration file. If not provided loading default_congiguration.yaml." )
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
                           description=pos.args.help )
opts <- try( parse_args(opt_parser, positional_arguments=2), silent=FALSE )
if( inherits(opts, "try-error") ){
  print_help( opt_parser )
  quit( "no", 1)
}


### Setup configuration

#Script folder
script.file <- sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1])
if (.Platform$OS.type == "windows") {
  script.file <- gsub("\\\\", "\\\\\\\\", prog)
}
script.folder = "./"
script.file = normalizePath(script.file)
script.folder <- dirname(script.file)

source(sprintf("%s/get.configuration.R", script.folder))
configfile = opts$options$config
if(configfile == ""){
  configfile = sprintf("%s/default_configuration.yaml", script.folder)
}
config = get.configuration(
                configfile,
                opts$args[1],
                opts$args[2],
                script.folder)

dir.create( opts$options$working.folder, showWarnings=FALSE  )
setwd( opts$options$working.folder )
create.directories(config)

#save configuration as r data
r.config = "./configuration.Rdata"
save(config, file=r.config)
if(config$atlas$use){
  #store atlas for use with shiny apps in working dir
  try({
    load(config$atlas$file)
    save(atlas, file="./atlas.Rdata")
  })
}


#Redirect progress updates to pipe if available
update.progress <- function(message, total, current, pipe){
  if( pipe != "" ){
    write( sprintf("{\n 'message' : %s, \n 'current' : %d, \n 'total' : %d \n}",
                  message, current, total),
           pipe, append=TRUE )
  }else{
    print( message )
  }
}


run.script <- function(script.file, message, current.start, current.end){
  tryCatch({
    update.progress( message, 100, current.start, config$progresspipe )
    script = sprintf("%s/%s --config %s", script.folder, script.file, r.config)
    ret.value = system2("Rscript", script)
    if( ret.value > 0 ){
      stop( ret.value )
    }
    update.progress( sprintf("%s - Done", message), 100, current.end, config$progresspipe )
    },
    error = function(e){
      print(e)
      update.progress( sprintf("%s - Failed", message), 100, current.end, config$progresspipe )
      update.progress( e$message, 100, current.end, config$progresspipe)
    }
  )
}

### Run thorugh pipeline

#Call setup scripts
if( config$setup$use ){
  run.script("Processing/setup.R", "Setup", 0, 5)
}
load(config$variablesfile)

#Compute multiscale representations if transport is used
if( config$transport$use ){
  tryCatch({
    update.progress( "Computing Multiscale", 100, 5, config$progresspipe )
    system2("parallel",
          paste(c(
          "--jobs", config$nparallel,
              "Rscript", sprintf("%s/Processing/create.gmra.R", script.folder),
                  config$transport$recompute,
                  config$transport$pointsfolder,
                  config$transport$gmrafolder,
                 ":::", file.names
                ), collapse=" " )
    )
    update.progress( "Computing Multiscale - Done", 100, 10, config$progresspipe )
    },
    error = function(e){
      update.progress( "Computing Mutliscale - Failed", 100, 10, config$progresspipe )
      update.progress( e$message, 100, 10, config$progresspipe)
    }
  )
}

#Compute wasserstein barycenter
if( config$barycenters$use & str_to_lower( config$barycenters$type ) == "wasserstein" ){
  run.script("Processing/barycenter.transport.R", "Computing OT Barycenter", 10, 40 )
}

#compute transport maps to barycenter create allocation and transport feature images
if( config$transport$use){
  tryCatch({
    update.progress( "Computing transport maps", 100, 40, config$progresspipe )
    system2( "parallel", paste( c( "--jobs", config$nparallel,
              "Rscript", sprintf("%s/Processing/mean.transport.unscaled.R", script.folder),
                  config$transport$cost,
                  config$transport$massbalancing,
                  config$transport$gmrafolder,
                  config$barycenters$file,
                  config$transport$transportfolder,
                  config$transport$degree,
                  config$transport$pointsfolder,
                  config$transport$recompute,
            ":::", file.names ), collapse=" " )
    )
    update.progress( "Computing transport maps - Done", 100, 70, config$progresspipe )
    },
    error = function(e){
      update.progress( "Computing transport maps - Failed", 100, 70, config$progresspipe )
      update.progress( e$message, 100, 70, config$progresspipe)
    }
  )
}

if( config$features$vbm$use ){
  dir.create( config$features$vbm$folder, showWarnings=FALSE  )
  run.script("Processing/vbm.transport.R", "VBM Features", 70, 71 )
}

if( config$features$conv$use ){
  dir.create( config$features$conv$folder, showWarnings=FALSE  )
  run.script("Processing/conv.transport.R", "Convolutional Features", 71, 72)
}

if( config$features$utm$use){
  dir.create( config$features$utm$folder, showWarnings=FALSE  )
  run.script("Processing/create.utm.features.R", "UTM Features", 72, 73)
}

#Compute correlations
if( config$analysis$correlation$use ){
  run.script("Analysis/analyse.images.cor.R", "Correlation Analysis",  73, 80)
}

#Compute components
if( config$analysis$components$use ){
  run.script("Analysis/analyse.images.components.R", "Component Analysis", 80, 89)
}

#Compute parcelation projections
if( config$analysis$parcels$use){
  run.script("Analysis/analyse.images.parcels.R", "Parcel Analysis", 89, 90)
}

#Compute morse smale parcelations
if( config$analysis$ms.parcels$use ){
  run.script("Analysis/analyse.images.ms.parcels.R", "Morse-Smale Analysis", 90, 95)
}


#### Legacy code - might not run correctly anymore
#Save correlation and p-value images and create pdf summary of images
if( config$report$use ){
  threshold = 1 - config$report$threshold
  if(threshold > 1){
    threshold = 1
  }

  update.progress( "Saving correlation images", 100, 95, config$progresspipe )
  system2("Rscript", c( sprintf("%s/Analysis/save.cor.images.R", script.folder),
             config$results$folder,
             config$analysis$correlations$file,
             threshold,
             config$barycenters$euclidean,
             config$barycenters$file )
  )
  update.progress( "Saving correlation images - Done", 100, 96, config$progresspipe )

  #Extract 2D images if necessary or store 2D in png format for creating a repeot
  update.progress( "Extracting slices for report", 100, 96, config$progresspipe )
  system2("Rscript", c( sprintf("%s/Analysis/extract.slice.cor.R", script.folder),
              config$correlations$file,
              config$variablesfile,
              config$reports$slices$folder,
              threshold,
              config$barycenters$euclidean,
              config$barycenters$file,
              config$report$axial,
              config$report$coronal,
              config$report$sagittal)
  )
  update.progress( "Extracting slices for report - Done", 100, 98, config$progresspipe )

  #Create pd report
  update.progress( "Generating report", 100, 95, config$progresspipe )
  system2("Rscript", c( sprintf("%s/Analysis/cor.table.R", script.folder),
              config$variablesfile,
              config$report$slices$folder,
              config$report$pdffile)
  )
  setwd(config$slicesfolder)
  system2("pdflatex", c("--interaction=batchmode", config$report$pdffile, " > /dev/null")  )
  system2("mv", c("*.pdf", "../" ) )
  setwd("..")
  update.progress( "Generating report - Done", 100, 100, config$progresspipe )
}
