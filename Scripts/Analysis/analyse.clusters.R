
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

tsne.allocation <- Rtsne(allocation)
tsne.transport <- Rtsne(transport)
tsne.a.t <- Rtsne( cbind(allocation / sqrt(sum(allocation^2)), transport / sqrt(sum(transport^2) ) ) )
tsne.intensity <- Rtsne(intensity)

##Variables
for(i in 1:length(variables)  ){
  v = variables[[i]]
  print( v$name )
  
  plot( tsne.allocation[v$index, ], col=v$values[v$index] )
  dev.copy2pdf( sprintf("%s/tsne-%s-allocation.pdf", save.path, v$name) )
  
  plot( tsne.transport[v$index, ], col=v$values[v$index] )
  dev.copy2pdf( sprintf("%s/tsne-%s-transport.pdf", save.path, v$name) )
  
  plot( tsne.intensity[v$index, ], col=v$values[v$index] )
  dev.copy2pdf( sprintf("%s/tsne-%s-intensity.pdf", save.path, v$name) )
  
  plot( tsne.a.t[v$index, ], col=v$values[v$index] )
  dev.copy2pdf( sprintf("%s/tsne-%s-allocation_transport.pdf", save.path, v$name) )
}

save(tsne.allocation, tsne.transport, tsne.intensity, tsne.a.t, file = save.file )










