#required librarys
suppressMessages(
suppressWarnings({
library(ANTsR, quietly=TRUE, warn.conflicts=F, verbose=FALSE)
})
)

analysis.parcels <- function(config){

features.folder <- config$features$folder
variables.path <- config$variablesfile
save.path <- config$results$folder
load( config$atlas$file )

load( variables.path )
feature.folders <- list.dirs(features.folder, full.names=TRUE, recursive=FALSE)
files <- sprintf("%s/%s.Rdata", feature.folders[[1]], file.names)
load( files[[1]] )

shape = dim(features[[1]])
if(!all(dim(atlas$label.image) == shape)){
 atlas$label.image = as.array( resampleImage( as.antsImage(atlas$label.image), shape, TRUE) )
}

labels = as.vector(atlas$label.image)
max.label = max(labels)
ulabels = sort(unique(labels))
if(ulabels[1] == 1){
  ulabels = ulabels[-1]
}
ilist = vector( mode="list", length=length(ulabels) )
for(i in 1:length(ulabels)){
 ilist[[i]] = list(label = ulabels[i], index = which( labels == ulabels[i] ) )
}

for( k in 1:length(feature.folders) ){
  print(basename(feature.folders[[k]]) )
  files <- sprintf("%s/%s.Rdata", feature.folders[[k]], file.names)
  n.points = length(files)
  parcel.projection <- list()
  dims <- list()
  load( files[[1]] )
  n.features = length(features)
  for(i in 1:n.features){
   parcel.projection[[i]] = matrix(NA, ncol=length(ulabels), nrow = n.points)
   colnames(parcel.projection[[i]]) <- as.character(atlas$labels[ulabels,1])
   dims[[i]] <- dim(features[[i]])
  }
  for( i in 1:n.points ){
    load( files[[i]] )
    for( f in 1:n.features ){
      x = as.vector( features[[f]] )
      for( l in 1:length(ilist) ){
        parcel.projection[[f]][i,l] = sum(x[ilist[[l]]$index])
      }
    }
  }
  parcels = list(projections = parcel.projection, dimension = dims, names=names(features) )
  label.index = ulabels
  save( parcels, label.index, file = sprintf("%s/%s-parcels.Rdata", save.path, basename(feature.folders[[k]]) )  )
}

}

suppressWarnings(
  library(optparse, quietly=TRUE, warn.conflicts=F, verbose=FALSE)
)
option_list = list(
  make_option( c("--config"), type="character", default="",
               help="configuration Rdata file with a config structure" )
)
opt_parser = OptionParser( option_list=option_list,
                           usage = "usage: %prog --config config.Rdata" )
opts <- try( parse_args(opt_parser), silent=FALSE )
load(opts$config)
analysis.parcels(config)
