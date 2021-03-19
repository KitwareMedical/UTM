
## Script paramaters
args <- commandArgs( trailingOnly=TRUE)

cor.file <- args[1]
variables.file <- args[2]
save.path <- args[3]
threshold <- as.double(args[4])
barycenter.euclidean.file <- args[5] 
barycenter.ot.discrete.file <- args[6] 

if(length(args) == 7){
  c.max.fixed <- as.double(args[7])
} else{
  c.max.fixed <- NA
}
  

load( variables.file )
load( cor.file )
dims <- dim(cor.res[[1]]$im[[1]])

if(length(dims) == 3){
  slice.axial <- as.double(args[7])
  slice.coronal <- as.double(args[8])
  slice.sagittal <- as.double(args[9])
}

#required librarys
library( RColorBrewer, quietly=TRUE, warn.conflicts=F )
suppressPackageStartupMessages( library( nat, quietly=TRUE, warn.conflicts=F ) )
library( mmand, quietly=TRUE, warn.conflicts=F )

## Helper functions
save.images.3d <- function(im, pim, name, bg.euclidean, bg.ot.discrete, save.path, 
                           slice.axial, slice.coronal, slice.sagittal, 
                           threshold=0.95, c.max=1 ){
  tim <- im
  #t.cols = brewer.pal("PiYG", n=11)
  #t.cols[6] = "#FFFFBF"
  #t.cols = colorRamp( t.cols)
  #t.cols = rgb( t.cols( seq(0, 1, length.out=99))/256 )
  #t.cols[50] = "#00000000"
  t.cols = hcl.colors( palette="PuOr", n = 255, alpha=1)
  g.cols = gray.colors(n=255, start=0, end=1)
  d <- dim(im[[1]])

  for(i in 1:length(im)){
    t.index <- which( pim[[i]] <= threshold )
    tim[[i]][ t.index ] = NA #0 
  
    #TODO store background in features file
    if( strtrim( names(im)[[i]], 3)  == "vbm"){
      bg=bg.euclidean
    }
    else{
      bg=bg.ot.discrete
    }
    db <- dim(bg)


    png( file = sprintf("%s/cor-axial-%s-%s.png", save.path, name, names(im)[[i]] ) )
    par( mar=c(0,0,0,0))
    image( bg[,,db[3]*slice.axial], col=g.cols, axes=FALSE, asp=1, zlim=range(bg) )
    image( im[[i]][,,d[3] * slice.axial], col=t.cols, add=TRUE, zlim=c(-c.max, c.max), 
           axes=FALSE, asp =1, useRaster=TRUE)
    abline( v = slice.sagittal, col="#d4b7ff" )
    abline( h = slice.coronal, col="#d4b7ff" )
    dev.off()
  
    png( file = sprintf("%s/cor-axial-%s-t-%s.png", save.path, name, names(im)[[i]] ) )
    par( mar=c(0,0,0,0))
    image( bg[,,db[3] * slice.axial], col=g.cols, axes=FALSE, asp=1, zlim=range(bg) )
    image( tim[[i]][,,d[3] * slice.axial], col=t.cols, add=TRUE, zlim=c(-c.max, c.max), 
           axes=FALSE, asp =1, useRaster=TRUE)
    abline( v = slice.sagittal, col="#d4b7ff" )
    abline( h = slice.coronal, col="#d4b7ff" )
    dev.off() 
  
    png( file = sprintf("%s/cor-axial-%s-p-%s.png", save.path, name, names(im)[[i]] ) )
    par( mar=c(0,0,0,0))
    image( bg[,,db[3] * slice.axial], col=g.cols, axes=FALSE, asp=1, zlim=range(bg) )
    image( pim[[i]][,,d[3] * slice.axial], col=heat.colors(100), add=TRUE, zlim=c(0, 1), 
           axes=FALSE, asp =1, useRaster=TRUE)
    abline( v = slice.sagittal, col="#d4b7ff" )
    abline( h = slice.coronal, col="#d4b7ff" )
    dev.off() 

    png( file = sprintf("%s/cor-coronal-%s-%s.png", save.path, name, names(im)[[i]] ) )
    par( mar=c(0,0,0,0))
    image( bg[,db[2] * slice.coronal, ], col=g.cols, axes=FALSE, asp=1, zlim=range(bg) )
    image( im[[i]][,d[2] * slice.coronal,], col=t.cols, add=TRUE, zlim=c(-c.max, c.max), 
           axes=FALSE, asp =1, useRaster=TRUE)
    abline( v = slice.sagittal, col="#d4b7ff" )
    abline( h = slice.axial, col="#d4b7ff" )
    dev.off()
  
    png( file = sprintf("%s/cor-coronal-%s-t-%s.png", save.path, name, names(im)[[i]] ) )
    par( mar=c(0,0,0,0))
    image( bg[,db[2] * slice.coronal,], col=g.cols, axes=FALSE, 
           asp=1, useRaster=TRUE, zlim=range(bg) )
    image( tim[[i]][,d[2] * slice.coronal,], col=t.cols, add=TRUE, zlim=c(-c.max, c.max), 
           axes=FALSE, asp =1, useRaster=TRUE)
    abline( v = slice.sagittal, col="#d4b7ff" )
    abline( h = slice.axial, col="#d4b7ff" )
    dev.off() 
  
    png( file = sprintf("%s/cor-coronal-%s-p-%s.png", save.path, name, names(im)[[i]] ) )
    par( mar=c(0,0,0,0))
    image( bg[,db[2] * slice.coronal,], col=g.cols, axes=FALSE, 
           asp=1, useRaster=TRUE, zlim=range(bg) )
    image( pim[[i]][,d[2] * slice.coronal,], col=heat.colors(100), add=TRUE, zlim=c(0, 1), 
           axes=FALSE, asp =1, useRaster=TRUE)
    abline( v = slice.sagittal, col="#d4b7ff" )
    abline( h = slice.axial, col="#d4b7ff" )
    dev.off() 


    png( file = sprintf("%s/cor-sagittal-%s-%s.png", save.path, name, names(im)[[i]] ) )
    par( mar=c(0,0,0,0))
    image( bg[db[1]*slice.sagittal,,], col=g.cols, axes=FALSE, 
           asp=1, useRaster=TRUE, zlim=range(bg) )
    image( im[[i]][d[1]*slice.sagittal,,], col=t.cols, add=TRUE, zlim=c(-c.max, c.max),
           axes=FALSE, asp =1, useRaster=TRUE)
    abline( h = slice.axial, col="#d4b7ff" )
    abline( v = slice.coronal, col="#d4b7ff" )
    dev.off()
  
    png( file = sprintf("%s/cor-sagittal-%s-t-%s.png", save.path, name, names(im)[[i]] ))
    par( mar=c(0,0,0,0))
    image( bg[db[1]*slice.sagittal,,], col=g.cols, axes=FALSE, 
           asp=1, useRaster=TRUE, zlim=range(bg) )
    image( tim[[i]][d[1]*slice.sagittal,,], col=t.cols, add=TRUE, zlim=c(-c.max, c.max), 
           axes=FALSE, asp =1, useRaster=TRUE)
    abline( h = slice.axial, col="#d4b7ff" )
    abline( v = slice.coronal, col="#d4b7ff" )
    dev.off() 

    png( file = sprintf("%s/cor-sagittal-%s-p-%s.png", save.path, name, names(im)[[i]] ))
    par( mar=c(0,0,0,0))
    image( bg[db[1]*slice.sagittal,,], col=g.cols, axes=FALSE, asp=1, 
           useRaster=TRUE, zlim=range(bg) )
    image( pim[[i]][d[1]*slice.sagittal,,], col=heat.colors(100), add=TRUE, zlim=c(0, 1), 
           axes=FALSE, asp =1, useRaster=TRUE)
    abline( h = slice.axial, col="#d4b7ff" )
    abline( v = slice.coronal, col="#d4b7ff" )
    dev.off()


    write.nrrd( im[[i]] ,  file = sprintf("%s/cor-%s-%s.nrrd", save.path, name, names(im)[[i]] ) )
    write.nrrd( tim[[i]],  file = sprintf("%s/cor-t-%s-%s.nrrd", save.path, name, names(im)[[i]] ) )
    write.nrrd( pim[[i]],  file = sprintf("%s/cor-p-%s-%s.nrrd", save.path, name, names(im)[[i]] ) )
  }
}


save.images.2d <- function( im, pim, name, bg.euclidean, bg.ot.discrete, 
                            save.path, threshold=0.95, c.max=1){
  tim <- im
  
  #t.cols = brewer.pal("PiYG", n=11)
  #t.cols[6] = "#FFFFBF"
  #t.cols = colorRamp( t.cols)
  #t.cols = rgb( t.cols( seq(0, 1, length.out=99))/256 )
  #t.cols[50] = "#00000000"
  t.cols = hcl.colors( palette="PuOr", n = 255, alpha=1)
  g.cols = gray.colors(n=255, start=0, end=1)
  d  <- dim( im[[1]] )
  for(i in 1:length(im)){
    c.max = max(abs(im[[i]]))
    print( sprintf("%s max cor: %f", names(im)[[i]], c.max) )
    t.index <- which( pim[[i]] <= threshold )
    tim[[i]][ t.index ] = 0 

    if( strtrim( names(im)[[i]], 3 ) == "vbm"){
      bg= bg.euclidean
     # bg= max(bg.euclidean) - bg.euclidean
    }
    else{
      bg= bg.ot.discrete
     # bg= max(bg.ot.discrete) - bg.ot.discrete
    }
  
    png( file = sprintf("%s/cor-%s-%s.png", save.path, name, names(im)[[i]] ), width=d[1], height=d[2] )
    par( mar=c(0,0,0,0))
    #image( bg, col=g.cols, axes=FALSE,  zlim=range(bg)*0.99  )
    image( im[[i]], col=t.cols, zlim=c(-c.max, c.max), 
           axes=FALSE,  useRaster=TRUE )
    dev.off()
  
    png( file = sprintf("%s/cor-%s-t-%s.png", save.path, name, names(im)[[i]] ), width=d[1], height=d[2] )
    par( mar=c(0,0,0,0))
    #image( bg, col=g.cols, axes=FALSE, zlim=range(bg)*0.99  )
    image( tim[[i]], col=t.cols,  zlim=c(-c.max, c.max), 
           axes=FALSE,  useRaster=TRUE )
    dev.off() 
    
    png( file = sprintf("%s/cor-%s-p-%s.png", save.path, name, names(im)[[i]] ), width=d[1], height=d[2] )
    par( mar=c(0,0,0,0))
    #image( bg, col=g.cols, axes=FALSE,  zlim=range(bg)*0.99 )
    image( pim[[i]], col=heat.colors(100),  zlim=c(0, 1), 
           axes=FALSE, useRaster=TRUE )
    dev.off() 
  }
}




load(barycenter.euclidean.file)
bg.euclidean = barycenter$image
load(barycenter.ot.discrete.file)
bg.ot.discrete = barycenter$image

##Variables
for(i in 1:length(cor.res)  ){

  if( is.na(c.max.fixed) ){
  c.max = 0
    for(im in cor.res[[i]]$im){
      c.max = max( c(c.max, abs(im)) )
    }
  }else{
    c.max <- c.max.fixed
  }
  print( sprintf("Max cor %s: %f", cor.res[[i]]$name, c.max) )
  c.max = 1
  if( length(dims) == 3 ){
    cor.res[[i]]$im$allocation <- rescale(cor.res[[i]]$im$allocation, 2, "triangle")
    cor.res[[i]]$im$transport <- rescale(cor.res[[i]]$im$transport, 2, "triangle")
    cor.res[[i]]$im$intensity <- rescale(cor.res[[i]]$im$intensity, 2, "triangle")
    
    cor.res[[i]]$pim$allocation <- gaussianSmooth( rescale(cor.res[[i]]$pim$allocation, 2, "triangle"), rep(0.5, 3) )
    cor.res[[i]]$pim$transport <- gaussianSmooth( rescale(cor.res[[i]]$pim$transport, 2, "triangle"), rep(0.5, 3) )
    cor.res[[i]]$pim$intensity <- gaussianSmooth( rescale(cor.res[[i]]$pim$intensity, 2, "triangle"), rep(0.5, 3) )


    save.images.3d( cor.res[[i]]$im, cor.res[[i]]$pim, cor.res[[i]]$name, 
                    bg.euclidean, bg.ot.discrete, save.path, slice.axial, slice.coronal, 
                    slice.sagittal, threshold, c.max)
  }
  else{
    save.images.2d( cor.res[[i]]$im, cor.res[[i]]$pim, cor.res[[i]]$name, 
                    bg.euclidean, bg.ot.discrete, save.path, threshold, c.max)

  }
}










