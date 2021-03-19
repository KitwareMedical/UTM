
args <- commandArgs( trailingOnly=TRUE)

save.path <- args[1]
cor.file <- args[2]
threshold <- as.double( args[3] )
barycenter.euclidean.file <- args[4]
barycenter.ot.file <- args[5]

load(cor.file)

save.image <- function(im, filename){
  if( length(dim(im)) == 2 ){
    dim(im) = c(dim(im), 1)
  }
  suppressPackageStartupMessages( library(nat, quietly=TRUE, warn.conflicts=F) )
  write.im3d(im, sprintf("%s.nrrd", filename) , "nrrd")
}

load( barycenter.euclidean.file )
save.image( barycenter$image, sprintf("%s/barycenter-euclidean", save.path) )

load( barycenter.ot.file )
save.image( barycenter$image, sprintf("%s/barycenter-ot", save.path) )


for( i in 1:length(cor.res) ){
   x <- cor.res[[i]]
   fbase <- sprintf("%s/%s", save.path, x$name)
   dir.create(fbase, showWarnings=FALSE)
   for( j in 1:length(x$im)){
     save.image( x$im[[j]], sprintf("%s/%s-cor", fbase, names(x$im)[j] ) )
   }
   for( j in 1:length(x$pim)){
     save.image( x$pim[[j]], sprintf("%s/%s-pvalue", fbase, names(x$pim)[j] ) )
     tim <- x$im[[j]]
     tim[ which(x$pim[[j]] <= threshold) ] = 0
     save.image( tim, sprintf("%s/%s-tcor", fbase, names(x$pim)[j] ) )
   }
}


