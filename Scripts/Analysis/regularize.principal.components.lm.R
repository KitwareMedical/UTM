

regularize.principal.components.lm <- function( pcs, lambda.factor.pos, lambda.factor.neg,
                                                sigma, step, n.iter,
                                                n.comps, kappa, cor.index, files,
                                                init.pca = TRUE){
  suppressWarnings({
  library( ANTsR )
  library( pracma )
  #library( tvR )
  })

  regularized <- pcs$components
  n.features = length( regularized )
  n.points = length( files )
  n.components = nrow( pcs$components[[1]] )

  objective = list()
  #spatial smooothnes regularization: gaussian smooth
  for( k in 1:n.features ){
    correlations = pcs$correlations[[k]][[cor.index]]
    correlations[is.na(correlations)] = 0
    nnz = (colSums(abs(regularized[[k]])) > 0)
    if(init.pca){
      updated <- t(regularized[[k]])[, 1:n.comps]
      #for(i in 1:ncol(updated) ){
      #  if( sum(updated[,i]) < 0 ){
      #    updated[,i]=-updated[,i]
      #  }
      #}
      #updated[updated < 0]  = 0
      #updated = abs(updated)
      #comps  = svd( updated[nnz, ] )
      #updated[nnz, ] = comps$u %*% t(comps$v)
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
    #lambda <- lambda.factor * apply( abs(updated), 2, max)
    lambda.pos <- lambda.factor.pos *  max( abs(updated) )
    lambda.neg <- lambda.factor.neg *  max( abs(updated) )
    print( lambda.pos )
    print( lambda.neg )
    prev = 0
    prev.objective = .Machine$double.xmax
    current.objective = .Machine$double.xmax/2
    iteration.counter = 0
    while( prev.objective > current.objective &
           iteration.counter < n.iter &
           abs(prev.objective-current.objective)/abs(prev.objective) > 0.001 ){

      iteration.counter = iteration.counter + 1
      pk = ( regularized[[k]] %*% updated)
      current = sum(pk^2) / ncol(updated)
      print( sprintf("Iteration: %i", iteration.counter )  )
      #print( sprintf("Norms: %s", mean(colSums( updated^2) ) ) )
      print( sprintf("Difference: %s", abs(prev-current) ) )
      print( sprintf("Projection Kernel: %s", current) )
      prev = current
      ck = t(updated) %*% correlations
      print( dim(ck) )
      print( sprintf("Cor: %.4f", sum(ck^2) ) )

      #Compute objective value:
      gnorm = 0
      for( i in 1:ncol(updated) ){
        grad = iMath( as.antsImage(
            array(updated[,i], dim=pcs$dimension[[k]] )
          ), "Grad")
        gnorm = gnorm + sum(grad^2)
      }
      gnorm = gnorm / ncol(updated)

      l1norm.neg = -sum( updated[updated < 0] )
      l1norm.pos = sum( updated[updated > 0] )
      print( sprintf("Grad: %s", gnorm) )
      print( sprintf("L1n: %s", l1norm.neg) )
      print( sprintf("L1p: %s", l1norm.pos) )
      print( sprintf("Objective l1 + g: %s", sigma * gnorm + lambda.neg * l1norm.neg + lambda.pos * l1norm.pos ) )

      prev.objective = current.objective
      current.objective = sigma * gnorm + lambda.neg * l1norm.neg + lambda.pos * l1norm.pos  - current / 2 - kappa * sum(ck^2)
      print( sprintf("Objective: %s",  current.objective) )
      print( sprintf("Relative Delta: %f", abs(prev.objective-current.objective) / abs(prev.objective) ) )

      objective[[k]] = c(objective[[k]], current.objective)

      #Gradient step towards pca subspace
      #updated = updated - step * ( (rowSums(updated) - csr) / ncol(updated) )
      updated = updated +  ( step / ncol(updated) )  * t( regularized[[k]] ) %*% pk
      #updated = updated + step * t( pk %*% regularized[[k]])

      for( i in 1:ncol(updated) ){
        updated[,i] <- as.vector( as.array(
                         smoothImage(
                           as.antsImage(
                             array(updated[,i], dim=pcs$dimension[[k]] )
                           ),
                           step*sigma
                         )
                       ))
        #updated[,i] = as.vector( denoise2( array(updated[,i], dim=pcs$dimension[[k]]),
        #                         sigma, niter=2, method="TVL2.FiniteDifference" ) )

        updated[!nnz, i] = 0
      }

      #maximize correlations to variables
      gradient.cor = outer( correlations, as.vector(ck) )
      print( dim(gradient.cor) )
      updated = updated + kappa * gradient.cor

      #sparsity regularization: l1 proximal operator
      gt <- updated >  step*lambda.pos
      updated[ gt ] <- updated[gt] - step * lambda.pos
      #updated[ !gt ] = 0
      lt <- updated < -step*lambda.neg
      updated[ lt  ] <- updated[lt] + step * lambda.neg
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
    PU[[k]] = matrix(0, nrow=n.comps, ncol=n.points)
  }
  for( i in 1:n.points ){
    load( files[[i]] )
    for( k in 1:n.features){
      x = as.vector(features[[k]]) - pcs$mean[[k]]
      PU[[k]][, i] = regularized[[k]] %*% x
    }
  }
  list(components = regularized, projections=PU, dimension=pcs$dimension, objective=objective)
}


