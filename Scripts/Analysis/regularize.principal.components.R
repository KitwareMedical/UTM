

regularize.principal.components <- function( pcs, lambda.factor.pos, lambda.factor.neg, sigma,
                                             step, n.iter, n.comps, files, init.pca = TRUE,
                                             kf = 1:length(pcs$components)){
  suppressWarnings({
  library( ANTsR, quietly=TRUE, warn.conflicts=F, verbose=FALSE)
  library( pracma, quietly=TRUE, warn.conflicts=F, verbose=FALSE )
  })
  #library( tvR )

  regularized <- pcs$components
  n.features = length( regularized )
  n.points = length( files )
  n.components = nrow( pcs$components[[1]] )

  objective = list()
  kernel <- list()
  #spatial smooothnes regularization: gaussian smooth
  for( k in kf ){
    #scalings = pcs$Sigma[[k]][1:nrow(regularized[[k]])]
    #scalings = scalings / mean(scalings)

    orig.comps = regularized[[k]] #* scalings
    nnz = colSums(abs(orig.comps)) > 0
    if(init.pca){
      n.comps.tmp <- ceiling(n.comps/2)
      n.comps.tmp2 = n.comps - n.comps.tmp
      updated <- t(orig.comps)[, 1:n.comps.tmp]
      updated2 <- -t(orig.comps)[, 1:n.comps.tmp2]
      updated[updated < 0]  = 0
      updated2[updated2 < 0]  = 0
      updated = cbind(updated, updated2)
      updated = updated[ , order( c(1:n.comps.tmp, 1:n.comps.tmp2) ) ]
      comps  = svd( updated[nnz, ] )
      updated[nnz, ] = comps$u %*% t(comps$v)
    }
    else{
      d = dim(regularized[[k]])
      updated <- matrix(runif(n.comps*d[2]), nrow=d[2])
      updated[!nnz, ]  = 0
      comps  = svd( updated[nnz, ] )
      updated[nnz, ] = comps$u %*% t(comps$v)
    }
    #updated = t( t(updated) / sqrt( colSums(updated^2) ) )

    objective[[k]] = 1
    kernel[[k]] = 1
    #lambda <- lambda.factor * apply( abs(updated), 2, max)
    lambda.pos <- lambda.factor.pos *  mean( abs(updated[nnz,]) )
    lambda.neg <- lambda.factor.neg *  mean( abs(updated[nnz,]) )
    #print( lambda.pos )
    #print( lambda.neg )
    prev = 0
    prev.objective = .Machine$double.xmax
    current.objective = .Machine$double.xmax/2
    iteration.counter = 0
    progress.images <- list()
    while( prev.objective > current.objective & iteration.counter < n.iter &
           abs(prev.objective-current.objective)/abs(prev.objective) > 0.001 ){
    #for(n in 1:n.iter){

      iteration.counter = iteration.counter + 1
      pk = ( orig.comps %*% updated)
      nk = min(ncol(pk), nrow(pk))
      current = sum(pk^2) / nk
      #print( sprintf("Iteration: %i", iteration.counter )  )
      #print( sprintf("Norms: %s", colSums( updated^2)  ) )
      #print( sprintf("Difference: %s", abs(prev-current) ) )
      #print( sprintf("Projection Kernel: %s", current) )
      prev = current

      #Compute objective value:
      gnorm = 0;
      for( i in 1:ncol(updated) ){
        grad = iMath( as.antsImage(
            array(updated[,i], dim=pcs$dimension[[k]] )
          ), "Grad")
        gnorm = gnorm + sum(grad^2)
        #gnorm = gnorm + sum(abs(grad) )
      }
      gnorm = gnorm / ncol(updated)

      l1norm.neg = -sum( updated[updated < 0] )
      l1norm.pos = sum( updated[updated > 0] )
      #print( sprintf("Grad: %s", gnorm) )
      #print( sprintf("L1n: %s", l1norm.neg) )
      #print( sprintf("L1p: %s", l1norm.pos) )
      #print( sprintf("Objective l1 + g: %s", sigma * gnorm
      #       + lambda.neg * l1norm.neg + lambda.pos * l1norm.pos ) )

      prev.objective = current.objective
      current.objective = sigma * gnorm + lambda.neg * l1norm.neg + lambda.pos * l1norm.pos  - current / 2
      #print( sprintf("Objective: %s",  current.objective) )
      #print( sprintf("Relative Delta: %f",
      #               abs(prev.objective-current.objective) / abs(prev.objective) ) )

      objective[[k]] = c(objective[[k]], current.objective)
      kernel[[k]] = c(kernel[[k]], current)

      #Gradient step towards pca subspace
      #updated = updated - step * ( (rowSums(updated) - csr) / ncol(updated) )
      gk = t( orig.comps ) %*% pk / nk
      #print( "PK gradient norms: ")
      #print( sqrt(colSums(gk^2) ) )
      #print( max(gk ) )
      updated = updated +  step * gk

      for( i in 1:ncol(updated) ){
        updated[,i] <- as.vector( as.array(
                         smoothImage(
                          as.antsImage(
                             array(updated[,i], dim=pcs$dimension[[k]] )
                           ),
                           step*sigma
                         )
                       ))
      #  updated[,i] = as.vector( denoise2( array(updated[,i], dim=pcs$dimension[[k]]),
      #                           step*sigma, niter=2, method="TVL2.FiniteDifference" ) )

        updated[!nnz, i] = 0
      }



      #sparsity regularization: l1 proximal operator
      gt <- updated >  step*lambda.pos
      updated[ gt ] <- updated[gt] - step * lambda.pos
      #updated[ !gt ] = 0
      lt <- updated < -step*lambda.neg
      updated[ lt  ] <- updated[lt] + step*lambda.neg
      updated[ !gt & !lt ] = 0


      #Project to grassmannian
      comps  = svd( updated[nnz, ] )
      updated[nnz, ] = comps$u %*% t(comps$v)
      #comps  = svd( updated )
      #updated = comps$u %*% t(comps$v)

    }
    regularized[[k]] = t( updated )
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
  list( components = regularized, projections=PU, dimension=pcs$dimension,
        objective=objective, kernel=kernel, progress.images=progress.images)
}


