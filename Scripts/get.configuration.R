library("yaml", quietly=TRUE, verbose=FALSE)
library("stringr", quietly=TRUE, verbose=FALSE)
library("doParallel", quietly=TRUE, verbose=FALSE)

get.configuration <- function(file, imagefolder, variables, script.folder){

config = read_yaml(file)
config$variablesfile = "./variables.Rdata"
config$imagefolder = imagefolder
config$variables = variables
config$script.folder = script.folder

config$nimages = length( list.files(config$imagefolder, "*.png") )  +
                 length( list.files(config$imagefolder, "*.nrrd") ) +
                 length( list.files(config$imagefolder, "*.hdr") )  +
                 length( list.files(config$imagefolder, "*.nii") )


if(config$nparallel < 1){
  config$nparallel = detectCores()
} else{
  config$nparallel = min( config$nparallel, detectCores() )
}

if(config$atlas$file == ""){
  config$atlas$file = sprintf("%s/../Atlas/sri24/labels/atlas.Rdata", script.folder)
}

config$barycenters$euclidean =
  sprintf("./%s/barycenter-euclidean.Rdata", config$barycenter$folder)
config$barycenters$otdiscrete =
  sprintf("./%s/barycenter-ot-discrete.Rdata", config$barycenter$folder)
config$barycenters$ot =
  sprintf("./%s/barycenter-ot.Rdata", config$barycenter$folder)
config$barycenters$otsolution =
  sprintf("./%s/barycenter-ot-solution.Rdata", config$barycenter$folder)
if( str_to_lower( config$barycenters$type ) == "wasserstein" ){
  config$barycenters$file = config$barycenters$otdiscrete
}else{
  config$barycenters$file = config$barycenters$euclidean
}

config$features$folder =
  sprintf("./%s/%s/", config$analysis$folder, config$features$folder)
config$features$vbm$folder =
  sprintf("%s/%s/", config$features$folder, config$features$vbm$folder)
config$features$utm$folder =
  sprintf("%s/%s/", config$features$folder, config$features$utm$folder)
config$features$conv$folder =
  sprintf("%s/%s/", config$features$folder, config$features$conv$folder)

config$analysis$correlation$file =
  sprintf("./%s/cor-vbm-utm.Rdata", config$analysis$folder)
config$report$pdffile = "cor-tables.tex"

config$results$folder =
  sprintf( "./%s/Images/", config$analysis$folder )
config$report$slices$folder  =
  sprintf( "./%s/Slices/", config$analysis$folder )

config$imagefolder = normalizePath( config$imagefolder )
config$variables   = normalizePath( config$variables )


#Make sure only the requested steps are performed to update the pipeline
if( !config$analysis$correlation$use & !config$analysis$components$use){
  config$features$utm$use = FALSE
  config$features$vbm$use = FALSE
  config$features$conv$use = FALSE
}
if( !config$features$utm$use ){
  config$transport$use = FALSE
}
if( !config$transport$use ){
  config$barycenter$use = FALSE
}
config
}

#
create.directories <- function(config){
  dir.create( config$analysis$folder, showWarnings=FALSE )
  dir.create( config$transport$gmrafolder, showWarnings=FALSE  )
  dir.create( config$transport$pointsfolder, showWarnings=FALSE  )
  dir.create( config$features$folder, showWarnings=FALSE  )
  dir.create( config$barycenter$folder, showWarnings=FALSE  )
  dir.create( config$transport$transportfolder, showWarnings=FALSE  )
  dir.create( config$results$folder, showWarnings=FALSE  )
  dir.create( config$report$slices$folder, showWarnings=FALSE  )
}
