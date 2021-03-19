#required librarys
suppressWarnings({
library(msr, quietly=TRUE, warn.conflicts=F, verbose=FALSE)
})

analysis.ms.parcels <- function(config){


features.folder <- config$features$folder
save.path <- config$results$folder
n.components <- config$analysis$ms.parcels$n.components
pLevelP <- config$analysis$ms.parcels$pLevelP

compute.variance.images <- function(files){
  load( files[[1]] )
  n.features = length(features)
  n.points = length(files)

  #Compute mean
  X.mean = list()
  for(k in 1:n.features ){
    X.mean[[k]] = 0
  }

  for( i in 1:n.points ){
    load( files[i] )
    for(k in 1:n.features ){
       x =  features[[k]]
       X.mean[[k]] =  X.mean[[k]] +  x
    }
  }
  variance = list()
  for( k in 1:n.features){
    X.mean[[k]] =  X.mean[[k]] / n.points
    variance[[k]] = 0
  }
  for( i in 1:n.points ){
    load( files[[i]] )
    for(k in 1:n.features ){
      x = features[[k]] - X.mean[[k]]
      variance[[k]] = variance[[k]] + x^2
    }
  }
  for( k in 1:n.features){
    variance[[k]] = variance[[k]] / (n.points-1)
  }
  list(variance = variance, avg=X.mean)
}


morse.smale.components = function(files, n.components){
  res <- compute.variance.images(files)
  variances = res$variance
  avg = res$avg

  partitions = list()
  n.features = length(variances)
  for(i in 1:n.features ){
    pim <- variances[[i]]
    ind = which(pim>0, arr.ind=TRUE)
    ms <- msc.nn(x=ind, y=pim[ind], knn=26, pLevelP=pLevelP)#nLevels=n.components)
    msl = ms$level[[length(ms$level)]]
    u = sort(unique(msl$ascending))
    for(k in 1:length(u)){
      msl$ascending[ msl$ascending == u[k] ] = k
    }
    pim[] = 0
    #pim[ms$x] = msl$partition
    pim[ms$x] = msl$ascending
    partitions[[i]] = pim
  }

  n.points = length(files)
  projections = list()
  projections2 = list()
  for( k in 1:n.features ){
    n.part = max(partitions[[k]])
    projections[[k]] = matrix(0, ncol=n.part, nrow=n.points)
    projections2[[k]] = matrix(0, ncol=n.part, nrow=n.points)
  }
  for( i in 1:n.points ){
    load( files[[i]] )
    for( k in 1:n.features ){
      x = features[[k]] - avg[[k]]
      n.part = max(partitions[[k]])
      for( j in 1:n.part){
        ind = which(partitions[[k]] == j)
        l = sqrt(sum(variances[[k]][ind]))
        projections2[[k]][i, j] = sum(x[ind] * sqrt(variances[[k]][ind])) / l
        l = sqrt(length(ind))
        projections[[k]][i, j] = sum(x[ind])/l
      }
    }
  }
  list(variances=variances, projections=projections,
       projections2=projections2, partitions=partitions,
       dimensions=lapply(variances, dim), names=names(features))
}

feature.folders <- list.dirs(features.folder, full.names=TRUE, recursive=FALSE)
for( k in 1:length(feature.folders) ){
  files <- list.files(feature.folders[[k]], full.names=TRUE)
  ms.comps <- morse.smale.components(files, n.components )
  ms.comps.file = sprintf( "%s/%s-ms.Rdata",
                      save.path, basename(feature.folders[[k]]) )
  save(ms.comps,file=ms.comps.file)
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
analysis.ms.parcels(config)

