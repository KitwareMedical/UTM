

split.principal.components <- function( pcs, n.comps, files,                                                                          kf = 1:length(pcs$components)){
  regularized <- pcs$components
  n.features = length( regularized )
  n.points = length( files )
  n.components = nrow( regularized[[1]] )

  #spatial smooothnes regularization: gaussian smooth
  for( k in kf ){
     split.pos <- regularized[[k]][1:n.comps, ]
     split.neg <- regularized[[k]][1:n.comps, ]
     split.pos[split.pos < 0 ] = 0
     split.neg[split.neg > 0 ] = 0
     split.neg = -split.neg
     
     
     split.all = rbind(split.pos, split.neg)
     norms = sqrt( rowSums( split.all^2) )
     split.all = split.all / norms

     regularized[[k]] = split.all
  }
  
  PU <- list()
  dims = list()
  for( k in 1:n.features){
    PU[[k]] = matrix(0, nrow=nrow(regularized[[k]]), ncol=n.points)
  }
  for( i in 1:n.points ){
    load( files[[i]] )
    for( k in 1:n.features){
      x = as.vector(features[[k]]) - pcs$mean[[k]]
      PU[[k]][, i] = regularized[[k]] %*% x 
    }
  }
  list(components = regularized, projections=PU, dimension=pcs$dimension) 
}



split.principal.components.max <- function( pcs, n.comps, files, sigma,
                                            kf = 1:length(pcs$components)){
  library( ANTsR )
  regularized <- pcs$components
  n.features = length( regularized )
  n.points = length( files )
  n.components = nrow( regularized[[1]] )

  #spatial smooothnes regularization: gaussian smooth
  for( k in kf ){
     split.pos <- regularized[[k]][1:n.comps, ]
     split.neg <- regularized[[k]][1:n.comps, ]
     split.pos[split.pos < 0 ] = 0
     split.neg[split.neg > 0 ] = 0
     split.neg = -split.neg
     
     
     split.all = rbind(split.pos, split.neg)
     #norms = sqrt( rowSums( split.all^2) )
     #split.all = split.all / norms
     for( i in 1:nrow(split.all) ){
        split.all[i,] <- as.vector( as.array( 
                         smoothImage( 
                           as.antsImage( 
                             array(split.all[i,], dim=pcs$dimension[[k]] )
                           ),  
                           sigma  
                         )
                       ))
     }
     mc <- max.col(t(split.all))
     for( i in 1:length(mc)){
       split.all[-mc[i], i] = 0
     }
     
     norms = sqrt( rowSums( split.all^2) )
     split.all = split.all / norms
     split.all = split.all[which(norms>0), ]
     regularized[[k]] = split.all
  }
  
  PU <- list()
  dims = list()
  for( k in 1:n.features){
    PU[[k]] = matrix(0, nrow=nrow(regularized[[k]]), ncol=n.points)
  }
  for( i in 1:n.points ){
    load( files[[i]] )
    for( k in 1:n.features){
      x = as.vector(features[[k]]) - pcs$mean[[k]]
      PU[[k]][, i] = regularized[[k]] %*% x 
    }
  }
  list(components = regularized, projections=PU, dimension=pcs$dimension)
}


