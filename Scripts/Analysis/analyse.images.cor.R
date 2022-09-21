#required librarys
suppressWarnings({
library(foreach, quietly=TRUE, warn.conflicts=F, verbose=FALSE )
library(doParallel, quietly=TRUE, warn.conflicts=F, verbose=FALSE )
})

analysis.correlations <- function(config){
features.folder <- config$features$folder
variables.path <- config$variablesfile
save.path <- config$results$folder
save.file <- config$analysis$correlation$file
n.permutations <- config$analysis$correlation$permutations
n.parallel <- config$nparallel
n.chunks <- config$n.chunks

if(n.parallel < 1){
  n.parallel <- detectCores()
} else {
  n.parallel <- min( n.parallel, detectCores() )
}

cor.permutation.test <- function(y, x, n){
  c.y <- suppressWarnings( cor(y, x) )
  k <- which( !is.na(c.y) )
  j <- which( is.na(c.y) )
  c.y[j] = 0

  p.y <- rep( 0, ncol(x) )

  cl <- makeCluster( n.parallel )
  registerDoParallel( cl )
  chunk.size.cor =min(200, n)
  l <- ceiling( n / chunk.size.cor )
  p.y[k] <- foreach(i=1:l, .combine="+") %dopar% {
    if( i * chunk.size.cor > n){
      size <- chunk.size.cor - chunk.size.cor * i + n
    }
    else{
      size <- chunk.size.cor
    }
    y.random <- replicate(size, sample(y, length(y) ) )
    q <- suppressWarnings( cor(y.random, x[, k] ) )
    rowSums( t(abs( q )) <= abs( c.y[k] ) )
  }

  stopCluster(cl)
  list(cor=c.y, p=p.y/n, n=n)
}

load( variables.path )
if (!exists('file.names') || !exists('dims') || !exists('variables')) {
  stop(sprintf("The file %s should have contained objects 'file.names', 'dims', and 'variables'",variables.path))
}
if (length(variables)<1) {
  stop("The variables list should have at least one item to do correlation analysis. Make sure there is at least one feature column in the input data table.")
}
if (!( ('name' %in% names(variables[[1]])) && ('values' %in% names(variables[[1]])) && ('index' %in% names(variables[[1]])) )) {
  stop("The variables list is not correctly formatted for doing correlation analysis.")
}

feature.folders <- list.dirs(features.folder, full.names=TRUE, recursive=FALSE)
feature.names <- c()
for( f in feature.folders ){
  #print(f)
  files <- list.files(f, full.names=TRUE)
  load( files[[1]] )
  feature.names <- c(feature.names, names(features) )
}
#print(feature.names)

d.im = length( features[[1]] )
cor.res <- list()
for(j in 1:length(variables)  ){
  cor.res[[j]] = list( im=list(), pim=list(), name = variables[[j]]$name )
  for(i in 1:length(feature.names) ){
     cor.res[[j]]$im[[i]]  = rep(0, d.im)
     cor.res[[j]]$pim[[i]] = rep(0, d.im)
  }
  names(cor.res[[j]]$im)  = feature.names
  names(cor.res[[j]]$pim) = feature.names
}


chunk.size = min(n.chunks, d.im)
l <- ceiling( d.im / chunk.size )
#print( sprintf(" Chunk size: %d", chunk.size) )

for(i in 1:l){
  start <- (i-1) * chunk.size + 1
  end <- start+chunk.size-1
  if( end > d.im ){
    end = d.im
  }
  all <- list()
  for(j in 1:length( feature.names )  ){
    all[[j]] = matrix(0, nrow=n, ncol=end-start+1)
  }
  names(all) = feature.names
  koff <- 0
  for( f in feature.folders ){
    for(j in 1:length(file.names)){
      load( sprintf("%s/%s.Rdata", f, file.names[j] ) )
      for(k in 1:length(features) ){
        all[[koff+k]][j, ]  = as.vector(features[[k]])[start:end]
      }
    }
    koff = koff + length( features )
  }
  for(j in 1:length(variables)  ){
    v = variables[[j]]
    #print( sprintf("Permutation test for %s - chunk %d of %d", v$name, i, l) )
    res <- list(j=j, ycors=list() )
    for(k in 1:length(all) ){
      ycors  <- cor.permutation.test( v$values[v$index], all[[k]][v$index, ],
                                      n.permutations )
      cor.res[[j]]$im[[k]][start:end]  <- ycors$cor
      cor.res[[j]]$pim[[k]][start:end] <- ycors$p
    }
  }
  #res.cors <- foreach(j = 1:length(variables)  ) %do% {
  #  v = variables[[j]]
  #  print( sprintf("Permutation test for %s - chunk %d of %d", v$name, i, l) )
  #  res <- list(j=j, ycors=list() )
  #  for(k in 1:length(features) ){
  #    res$ycors[[k]]  <- cor.permutation.test( v$values[v$index], all[[k]][v$index, ], n.permutations )
  #  }
  #  res
  #}
  #for(res in res.cors){
  #  ycors = res$ycors
  #  for(k in 1:length(ycors) ){
  #    cor.res[[res$j]]$im[[k]][start:end] = ycors[[k]]$cor
  #    cor.res[[res$j]]$pim[[k]][start:end] = ycors[[k]]$p
  #  }
  #}

}

for(j in 1:length(variables)  ){
  for(i in 1:length(cor.res[[j]]$im ) ){
    cor.res[[j]]$im[[i]]  = array( cor.res[[j]]$im[[i]],  dim=dims )
    cor.res[[j]]$pim[[i]] = array( cor.res[[j]]$pim[[i]], dim=dims )
  }
}

save(cor.res, file = save.file )

# Write out images as nifti files
library("RNifti")
for (j in 1:length(cor.res)) {
  res <- cor.res[[j]]

  # Allocation
  writeNifti(res$im$allocation, sprintf("%s/%s_allocation.nii.gz", save.path, res$name))
  writeNifti(res$pim$allocation, sprintf("%s/%s_allocation_pvalue.nii.gz", save.path, res$name))

  # Transport
  writeNifti(res$im$transport, sprintf("%s/%s_transport.nii.gz", save.path, res$name))
  writeNifti(res$pim$transport, sprintf("%s/%s_transport_pvalue.nii.gz", save.path, res$name))

  # VBM
  writeNifti(res$im$vbm, sprintf("%s/%s_vbm.nii.gz", save.path, res$name))
  writeNifti(res$pim$vbm, sprintf("%s/%s_vbm_pvalue.nii.gz", save.path, res$name))
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
analysis.correlations(config)
