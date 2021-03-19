#required librarys
library(mmand, quietly=TRUE, warn.conflicts=F, verbose=FALSE)

library(foreach, quietly=TRUE, warn.conflicts=F, verbose=FALSE)
library(doParallel, quietly=TRUE, warn.conflicts=F, verbose=FALSE)
library(stream, quietly=TRUE, warn.conflicts=F, verbose=FALSE)

## Script paramaters
#args <- commandArgs( trailingOnly=TRUE)

#features.folder <- args[1]
#variables.path <- args[2]
#save.path <- args[3]
#save.file <- args[4]
#n.permutations <- as.integer(args[5])
#n.parallel <- as.integer( args[6] )


n.parallel = 1
features.folder = "Run1/Analysis/Features/"
variables.path = "Run1/variables.Rdata"

if(n.parallel < 1){
  n.parallel <- detectCores()
} else {
  n.parallel <- min( n.parallel, detectCores() )
}
cl <- makeCluster( n.parallel )
registerDoParallel( cl )



DSD_MorphometryStream <- function( index, files, chunk.size ){
  load( files[[1]] )
  n <- length(features[[1]])
  chunk.size = min(100000, n)
  chunk <- setRefClass("chunk", fields = list(index = "numeric",
                                   current = "numeric",
                                   data = "matrix" ) )
  structure(
    list( description = "Morphometry Data Stream",
          index = index,
          files = files,
          n = n,
          d = length(files),
          chunk.size = chunk.size,
          n.chunks = ceiling( n / chunk.size ),
          chunk = chunk(index=1, current=0, data=matrix(nrow=0,ncol=0))
    ),
    class = c("DSD_MorphometryStream", "DSD_R", "DSD")
  )
}



get_points.DSD_MorphometryStream <- function(x, n = 1, assignment = FALSE, ...) {

  load.chunk <- function(x){
    x$chunk$index <- 1
    if( x$chunk$current >= x$n.chunks){
      x$chunk$current <- 0
    }
    start <- x$chunk$current * x$chunk.size + 1
    end <- start + x$chunk.size-1
    if( end > x$n ){
        end = x$n
    }
    x$chunk$data <- matrix(0, ncol=x$d, nrow=end-start+1)
    for(j in 1:x$d){
      load( files[[j]] )
      x$chunk$data[,j]  = as.vector(features[[x$index]])[start:end]
    }
    x$chunk$current <- x$chunk$current+1
  }


  data <- c()
  print(n)
  while(n > 0 ){
    if( x$chunk$index > nrow(x$chunk$data) ){
      load.chunk(x)
    }
    data.index <- min(n, nrow(x$chunk$data) - x$chunk$index + 1)
    data <- rbind(data, x$chunk$data[x$chunk$index - 1 + 1:data.index, ])
    x$chunk$index <- x$chunk$index + data.index
    n <- n - data.index

  }

  #standardize rows
  #such that data[i, ] * data[j,] = cor(i, j)
  data <- t( scale( t(data) )  )
  data[is.na(data[,1]), ] = 0
  data <- data * sqrt( 1 / (ncol(data)-1) )
  data <- as.matrix(data)
  if(assignment){
    attr(data, "assignment") <- rep(NA_integer_, n)
  }
  data
}



block.size <- 1000
feature.folders <- list.dirs(features.folder, full.names=TRUE, recursive=FALSE)
feature.names <- c()
for( f in feature.folders ){
  files <- list.files(f, full.names=TRUE)
  feature.stream <- DSD_MorphometryStream(2, files, chunk.size=block.size)
  dbstream <- DSC_DBSTREAM(r = 0.1, lambda=0, minweight=0, Cm=0)

  cl <- c()
  for(i in 1:ceiling( feature.stream$n / block.size ) ){
    npoints <- min(feature.stream$n-length(cl), block.size)
    update(dbstream, get_points(feature.stream, npoints), assignments = TRUE)
    cl <- c(cl, get_cluster_assignments(dbstream) )
  }

  km <- DSC_Kmeans(k=10) #DSC_DBSCAN(eps=0.1)
  recluster(km, dbstream)
  cl <- c()
  feature.stream <- DSD_MorphometryStream(2, files, chunk.size=block.size)
  for(i in 1:ceiling( feature.stream$n / block.size ) ){
    npoints <- min(feature.stream$n-length(cl), block.size)
    cl <- c( cl, get_assignment(km, get_points(feature.stream, npoints) ) )
  }


}




