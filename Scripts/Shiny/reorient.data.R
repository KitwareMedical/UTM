library(optparse, quietly=TRUE)

option_list = list(  
  make_option("--barycenterfolder", type="character", default="Barycenter", 
              help="Subfolder in Analysis for storing barycenters [default=Barycenter]", metavar="character"),
  make_option(c("-a", "--analysisfolder"), type="character", default="Analysis", 
              help="Folder to store results in. [default=Analysis]", metavar="character"),
  make_option(c("-b", "--barycentertype"), type="character", default="Euclidean", 
              help="Barycenter to display [default = Euclidean]", metavar="character"),
  make_option("--workingfolder", type="character", default=".", 
              help="Working directory folder [default=.]", metavar="character"),
  make_option("--pointsfolder", type="character", default="Points", 
              help="Folder for storing Point cloud data [default=Points]", metavar="character"),
  make_option( "--orientation", type="character", default="xyz", 
               help="Permutation of orientation of images", metavar="character"), 
  make_option( "--atlas", type="character", default="atlas.Rdata", 
               help="Atlas for displaying anatomical labels and information", metavar="character"),
  make_option( "--flipX", help="Flip x axis", action="store_true", default=FALSE ),
  make_option( "--flipY", help="Flip y axis", action="store_true", default=FALSE ),
  make_option( "--flipZ", help="Flip z axis", action="store_true", default=FALSE )
) 


pos.args.help = " "

      
opt_parser = OptionParser( option_list=option_list, 
                           usage = "usage: %prog [options]", 
                           description=pos.args.help)
opts <- try( parse_args(opt_parser, positional_arguments=0), silent=FALSE )

if( inherits(opts, "try-error") ){
  print_help( opt_parser )
  quit( "no", 1)
}

config = opts$options

config$barycenters = list()
config$barycenters$euclidean = sprintf("./%s/barycenter-euclidean.Rdata", config$barycenterfolder)
config$barycenters$otdiscrete = sprintf("./%s/barycenter-ot-discrete.Rdata", config$barycenterfolder)

config$correlationfile = sprintf("./%s/cor-vbm-utm.Rdata", config$analysisfolder) 

config$variablesfile = "./variables.Rdata"

config$pointsfilepattern = sprintf("./%s/points%s.Rdata", config$pointsfolder, "%04d" )

olist = strsplit(config$orientation, "")[[1]]
permutation = c( which( olist == 'x' ),
                 which( olist == 'y' ),
                 which( olist == 'z') )

try( setwd( config$workingfolder ), silent=TRUE )

print(config)
print( permutation )

#load( config$variablesfile  )


reorient <- function(im, fX, fY, fZ, p){
  d = dim(im)
  
  x = 1:d[1]
  if(fX){
    x = rev(x)
  }

  y = 1:d[2]
  if(fY){
    y = rev(y)
  } 

  z = 1:d[3]
  if(fZ){
    z = rev(z)
  } 
  im = im[x,y,z]
  im = aperm(im, p)
}

load( config$barycenters$euclidean )
barycenter$image = reorient(barycenter$image, config$flipX, config$flipY, config$flipZ, permutation)
barycenter$orig.image = reorient(barycenter$orig.image, config$flipX, config$flipY, config$flipZ, permutation)
save(barycenter, file=config$barycenters$euclidean)

if(T){
barycenter = NULL
try(suppressWarnings( load( config$barycenters$otdiscrete ) ), silent = TRUE )
if( !is.null(barycenter) ){
  barycenter$image = reorient(barycenter$image, config$flipX, config$flipY, config$flipZ, permutation)
  barycenter$orig.image = reorient(barycenter$orig.image, config$flipX, config$flipY, config$flipZ, permutation)
  save(barycenter, file=config$barycenters$otdiscrete)
}

atlas <- NULL
try( load(config$atlas), silent=TRUE)
if( !is.null(atlas) ){
  atlas = reorient(atlas, config$flipX, config$flipY, config$flipZ, permutation)
  save(atlas, labels, file=config$atlas)
}

load( config$correlationfile )
for(i in 1:length(cor.res) ){
  for(j in 1:(length(cor.res[[i]])-1) ){
    for(k in 1:length(cor.res[[i]][[j]]) ){
      cor.res[[i]][[j]][[k]] = reorient(cor.res[[i]][[j]][[k]], config$flipX, config$flipY, config$flipZ, permutation)
    }
  }
}
save( cor.res, file=config$correlationfile )
}
