
## Script paramaters
args <- commandArgs( trailingOnly=TRUE)

average.image.path <- args[1]
variables.path <- args[2]
save.path <- args[3]
save.file <- args[4]

#required librarys
library(Rtsne)
library(mmand)
library(RColorBrewer)
library(lmvar)





##Data matrix / resmapling of image grids
load( variables.path)
load(average.image.path)
for(i in 1:length(avg.images$allocation)){
  if( length(avg.images$allocation[[i]]) > 1){
    avg.images$allocation[[i]] =  rescale( avg.images$allocation[[i]], 1.5, "triangle")
    avg.images$transport[[i]] =  rescale( avg.images$transport[[i]], 1.5, "triangle")
    avg.images$intensity[[i]] =  rescale( avg.images$intensity[[i]], 1.5, "triangle")

  }
}

dims <- dim(avg.images$allocation[[1]])
allocation <- matrix(NA, nrow=length(avg.images$allocation), ncol = prod(dims) )
transport  <- matrix(NA, nrow=length(avg.images$allocation), ncol = prod(dims) )
intensity  <- matrix(NA, nrow=length(avg.images$allocation), ncol = prod(dims) )
for(i in 1:length(avg.images$allocation) ){
  allocation[i, ] = as.vector(avg.images$allocation[[i]]) 
  transport[i, ]  = as.vector(avg.images$transport[[i]] ) 
  intensity[i, ]  = as.vector(avg.images$intensity[[i]] ) 
}
rm( avg.images )
gc()

#hc.allocation <- hclust( pdist(allocation) )
#hc.transport  <- hclust( pdist(transport) )
#hc.intensity  <- hclust( pdist(intensity) )

tsnes <- list()
tsnes$allocation <- Rtsne(allocation, dims=5, check_duplicates=FALSE)
tsnes$transport <- Rtsne(transport, dims=5, check_duplicates=FALSE)
tsnes$a.t <- Rtsne( cbind(allocation / sqrt(sum(allocation^2)), transport / sqrt(sum(transport^2) ) ), dims=5, check_duplicates=FALSE )
tsnes$intensity <- Rtsne(intensity, dims=5, check_duplicates=FALSE)

pcas <- list()
pcas$allocation <- prcomp( allocation )$x
pcas$transport <- prcomp( transport )$x
pcas$a.t <- prcomp( cbind(allocation / sqrt(sum(allocation^2)), transport / sqrt(sum(transport^2) ) ) )$x
pcas$intensity <- prcomp( intensity )$x

##Variables
tsne.lms <- list()
pca.lms <- list()
v.names <- c()
for(i in 1:length(variables)  ){
  v = variables[[i]]
  print( v$name )
  v.names <- c( v.names, v$name )
  if( is.integer( v$values[v$index] ) ){
    pal <- brewer.pal("Dark2", n=8)
    cols <- pal[v$values[v$index]]
  }
  else{
    pal <- brewer.pal("YlOrRd", n=9)
    ramp <- colorRamp(pal)
    v.min <- min( v$values[v$index] )
    v.max <-  max( v$values[v$index ] )
    if( v.min * v.max < 0 ){
      m <- max( abs( c(v.min, v.max) ) )
      vals <- ( v$values[v$index] + m)/(2*m)
    }
    else{
      vals <- ( v$values[v$index] - v.min)/(v.max-v.min)
    }
    cols <- rgb( ramp( vals )/255 )
  }

  png( sprintf("%s/tsne-%s-allocation.png", save.path, v$name) )
par(mar=c(5,6,1,1) )
  plot( tsnes$allocation$Y[v$index, 1:2], col=cols, pch=19, cex.axis=1.5, cex.lab=2, cex=2, bty="n", xlab="x", ylab="y", asp=1)
  dev.off()

  png( sprintf("%s/tsne-%s-transport.png", save.path, v$name) )
par(mar=c(5,6,1,1) )
  plot( tsnes$transport$Y[v$index, 1:2], col=cols, pch=19,  cex.axis=1.5, cex.lab=2, cex=2, bty="n", xlab="x", ylab="y", asp=1 )
  dev.off()
  
  png( sprintf("%s/tsne-%s-intensity.png", save.path, v$name) )
par(mar=c(5,6,1,1) )
  plot( tsnes$intensity$Y[v$index, 1:2], col=cols, pch=19, cex.axis=1.5, cex.lab=2, cex=2, bty="n", xlab="x", ylab="y", asp=1 )
  dev.off()
  
  png( sprintf("%s/tsne-%s-allocation_transport.png", save.path, v$name) )
par(mar=c(5,6,1,1) )
  plot( tsnes$a.t$Y[v$index, 1:2], col=cols, pch=19, cex.axis=1.5, cex.lab=2, cex=2,  bty="n", xlab="x", ylab="y", asp=1 )
  dev.off()



  png( sprintf("%s/pca-%s-allocation.png", save.path, v$name) )
par(mar=c(5,6,1,1) )
  plot( pcas$allocation[v$index, 1:2], col=cols, cex.axis=1.5, cex.lab=2, cex=2, pch=19, bty="n", asp=1)
  dev.off()

  png( sprintf("%s/pca-%s-transport.png", save.path, v$name) )
par(mar=c(5,6,1,1) )
  plot( pcas$transport[v$index, 1:2], col=cols, cex.axis=1.5, cex.lab=2, cex=2, pch=19, bty="n", asp=1 )
  dev.off()
  
  png( sprintf("%s/pca-%s-intensity.png", save.path, v$name) )
par(mar=c(5,6,1,1) )
  plot( pcas$intensity[v$index, 1:2], col=cols, cex.axis=1.5, cex.lab=2, cex=2,  pch=19,  bty="n", asp=1 )
  dev.off()
  
  png( sprintf("%s/pca-%s-allocation_transport.png", save.path, v$name) )
par(mar=c(5,6,1,1) )
  plot( pcas$a.t[v$index, 1:2], col=cols, pch=19, cex.axis=1.5, cex.lab=2, cex=2, bty="n", asp=1 )
  dev.off()




  tsne.lm <- list()
  for( k in 1:length(tsnes) ){
    print(k)
    tsne.lm[[k]] <- list()
    for( j in 2:5){
      tmp <- cv.lm( lm( v$values[v$index] ~ tsnes[[k]]$Y[v$index, 1:j], x=TRUE, y=TRUE ) )
      tsne.lm[[k]]$MAE <- rbind( tsne.lm[[k]]$MAE, unlist( tmp$MAE ) )
      tsne.lm[[k]]$MSE <- rbind( tsne.lm[[k]]$MSE, unlist( tmp$MSE ) )
      tsne.lm[[k]]$MSE_sqrt <- rbind( tsne.lm[[k]]$MSE_sqrt, unlist( tmp$MSE_sqrt ) )

    }
  }
  names(tsne.lm) <- names(tsnes)
  tsne.lms[[i]] <- tsne.lm


  
  pca.lm <- list()
  for( k in 1:length(pcas) ){
    print(k)
    pca.lm[[k]] <- list()
    for( j in 2:5){
      tmp <- cv.lm( lm( v$values[v$index] ~ pcas[[k]][v$index, 1:j], x=TRUE, y=TRUE ) )
      pca.lm[[k]]$MAE <- rbind( pca.lm[[k]]$MAE, unlist( tmp$MAE ) )
      pca.lm[[k]]$MSE <- rbind( pca.lm[[k]]$MSE, unlist( tmp$MSE ) )
      pca.lm[[k]]$MSE_sqrt <- rbind( pca.lm[[k]]$MSE_sqrt, unlist( tmp$MSE_sqrt ) )

    }
  }
  names(pca.lm) <- names(pcas)
  pca.lms[[i]] <- pca.lm

}


names(tsne.lms) <- v.names
names(pca.lms) <- v.names
save(tsnes, pcas, tsne.lms, pca.lms, file = save.file )










