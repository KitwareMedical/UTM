analysis.components <- function(config){

features.folder <- config$features$folder
variables.path <- config$variablesfile
save.path <- config$results$folder
n.components <- config$analysis$components$ncomponents
n.components.reg <- config$analysis$components$ncomponents.reg
lambda.pos <- config$analysis$components$lambda.pos
lambda.neg <- config$analysis$components$lambda.neg
sigma <- config$analysis$components$sigma
step <- config$analysis$components$step
n.iters <- config$analysis$components$n.iters
kappa <- config$analysis$components$kappa
cor.index <- 1

source(sprintf("%s/Analysis/regularize.principal.components.R", config$script.folder) )
source(sprintf("%s/Analysis/split.principal.components.R", config$script.folder) )


#compute principal components from m>>n, byt computing svd of X^t X, with X mean cenetered
#Then the principal compnents are X V S^-1 with X^tX = V S V^t, X = U S V^t
principal.components.randomized <- function(files, n.components, variables,
                                           n.power.iterations=2, n.over=4){
  suppressMessages(
  suppressWarnings(
  library(ANTsR, quietly=TRUE, verbose=F, warn.conflicts=)
  ))

  load( files[[1]] )
  n.features = length(features)
  n.points = length(files)

  #correlations <- list(n.features)
  sumx2 <- list(n.features)
  #Compute mean
  X.mean = list()
  for(k in 1:n.features ){
    X.mean[[k]] = 0
    #correlations[[k]] = list()
    #sumx2[[k]] = list()
    #for( l in 1:length(variables) ){
    #  correlations[[k]][[l]] = 0
    #  sumx2[[k]][[l]]= 0
    #}
  }

  for( i in 1:n.points ){
    load( files[i] )
    for(k in 1:n.features ){
       x =  as.vector(features[[k]])
       X.mean[[k]] =  X.mean[[k]] +  x

#       for( l in 1:length(variables) ){
#         if( is.element(i, variables[[l]]$index) ){
#           sumx2[[k]][[l]] = sumx2[[k]][[l]] + x^2
#           correlations[[k]][[l]] = correlations[[k]][[l]] + x * variables[[l]]$values[i]
#         }
#       }
    }
  }
  for(k in 1:n.features ){
    X.mean[[k]] =  X.mean[[k]] / n.points

#    for( l in 1:length(variables) ){
#      correlations[[k]][[l]] = correlations[[k]][[l]] / ( sqrt(sum( variables[[l]]$values^2)) * sqrt( sumx2[[k]][[l]] )  )
#    }
  }
  n.dim = length( X.mean[[k]] )


  #Approximate range
  Omega = matrix( rnorm(n.points*(n.components+n.over)), nrow=n.points )

  Y = list()
  for(k in 1:n.features){
    Y[[k]] = 0
  }

  variance = list()
  for( k in 1:n.features){
    variance[[k]] = 0
  }
  for( i in 1:n.points ){
    load( files[[i]] )
    for(k in 1:n.features ){
      x1 = as.vector(features[[k]]) - X.mean[[k]]
      variance[[k]] = variance[[k]] + x1^2
      Y[[k]] = Y[[k]] + outer(x1, Omega[i, ])
    }
  }
  for( k in 1:n.features){
    variance[[k]] = variance[[k]] / (n.points-1)
  }


  #Power iterations
  Yp <- list()
  for(k in 1:n.features){
    Yp[[k]] = matrix(0, nrow=ncol(Y[[k]]), ncol=n.points)
  }

  for( p in 1:n.power.iterations){
    for( i in 1:n.points ){
      load( files[[i]] )
      for(k in 1:n.features ){
        x1 = as.vector(features[[k]]) - X.mean[[k]]
        Yp[[k]][,i] = t(Y[[k]]) %*% x1
      }
    }
    for(k in 1:n.features){
      Y[[k]] = 0
    }
    for( i in 1:n.points ){
      load( files[[i]] )
      for(k in 1:n.features ){
        x1 = as.vector(features[[k]]) - X.mean[[k]]
        Y[[k]] = Y[[k]] + outer(x1, Yp[[k]][, i])
      }
    }
  }


  #Form Q
  Q <- list()
  for(k in 1:n.features){
    Q[[k]] = qr.Q( qr(Y[[k]]) )
  }

  #Form Q
  B <- list()
  for(k in 1:n.features){
    B[[k]] = matrix(0, nrow=ncol(Q[[k]]), ncol=n.points)
  }

  for( i in 1:n.points ){
    load( files[[i]] )
    for(k in 1:n.features ){
      x1 = as.vector(features[[k]]) - X.mean[[k]]
      B[[k]][,i] = t(Q[[k]]) %*% x1
    }
  }

  U <- list()
  Sigma <- list()
  for( k in 1:n.features){
    decomposition = svd(B[[k]], nu=n.components, nv=0)
    Sigma[[k]] = decomposition$d
    U[[k]] =t( Q[[k]] %*% decomposition$u )
  }

  PU <- list()
  PUS <- list()
  dims = list()
  smoothed = list()
  for( k in 1:n.features ){
    U[[k]] = U[[k]] / sqrt( rowSums( U[[k]]^2) )
    PU[[k]] = matrix(0, nrow=n.components, ncol=n.points)
    PUS[[k]] = matrix(0, nrow=n.components, ncol=n.points)
    dims[[k]] = dim( features[[k]] )

    smoothed[[k]] = U[[k]]
    for( i in 1:nrow(smoothed[[k]]) ){
      smoothed[[k]][i, ] <- as.vector( as.array(
                        smoothImage(
                          as.antsImage(
                            array(smoothed[[k]][i,], dim=dims[[k]] )
                          ),  2
                        )
                      ))
     }
     smoothed[[k]] = smoothed[[k]] / sqrt( rowSums( smoothed[[k]]^2) )
  }
  for( i in 1:n.points ){
    load( files[[i]] )
    for( k in 1:n.features ){
      x = as.vector( features[[k]] ) - X.mean[[k]]
      PU[[k]][, i] = U[[k]] %*% x
      PUS[[k]][, i] = smoothed[[k]] %*% x
    }
  }

  list( components.smoothed = smoothed, components = U,
        projections = PU, Sigma=Sigma,
        #correlations=correlations,
        projections.smoothed=PUS, mean=X.mean,
        dimension=dims, names=names(features),
        variances=variance ) #, X=X )
}




#compute principal components from m>>n, byt computing svd of X^t X, with X mean cenetered
#Then the principal compnents are X V S^-1 with X^tX = V S V^t, X = U S V^t
principal.components <- function(files, n.components){
  suppressMessages(
  suppressWarnings(
  library(ANTsR)
  ))

  load( files[[1]] )
  n.features = length(features)
  n.points = length(files)
  #X = list()
  #non.centered = T
  #if(non.centered){
  #print( "Compute Mean" )
  X.mean = list()
  for(k in 1:n.features ){
    X.mean[[k]] = 0
    #X[[k]] = matrix(NA, nrow=length(as.vector(features[[k]])), ncol=n.points)
  }

  for( file in files ){
    load( file )
    for(k in 1:n.features ){
       X.mean[[k]] =  X.mean[[k]] +  as.vector(features[[k]])
    }
  }
  for(k in 1:n.features ){
    X.mean[[k]] =  X.mean[[k]] / n.points
  }
  #}

  #print( "Compute Cov" )
  X.cov = list()
  for(k in 1:n.features ){
    X.cov[[k]] = matrix(0, n.points, n.points )
  }

  variance = list()
  for( k in 1:n.features){
    variance[[k]] = 0
  }
  for( i in 1:n.points ){
    load( files[[i]] )
    x1 = list()
    for(k in 1:n.features ){

      x1[[k]] = as.vector(features[[k]]) - X.mean[[k]]
      variance[[k]] = variance[[k]] + x1^2
      #X[[k]][,i] = x1[[k]]
      X.cov[[k]][i,i] = sum( x1[[k]] * x1[[k]] )
    }
    if( i < n.points ){
      for( j in (i+1):n.points ){
        load( files[[j]] )
        for(k in 1:n.features ){
          x2 =  as.vector(features[[k]]) - X.mean[[k]]
          X.cov[[k]][i,j] = sum( x1[[k]] * x2 )
          X.cov[[k]][j,i] = X.cov[[k]][i,j]
        }
      }
    }
  }
  for( k in 1:n.features){
    variance[[k]] = variance[[k]] / (n.points-1)
  }


 # print( "Compute SVD" )
  VSinv = list()
  for( k in 1:n.features ){
    decomposition = svd(X.cov[[k]], nv=n.components, nu=0)
    VSinv[[k]] = decomposition$v
    for( i in 1:ncol(VSinv[[k]]) ){
      VSinv[[k]][,i] = VSinv[[k]][, i] / sqrt( decomposition$d[i] )
    }
  }

  #print( "Compute U" )
  U <- list()
  for( k in 1:n.features){
    U[[k]] = matrix(0, ncol=length(features[[k]]), nrow=n.components)
  }
  for( i in 1:n.points ){
    load( files[[i]] )
    for( k in 1:n.features ){
      U[[k]] = U[[k]] + outer( VSinv[[k]][i, ], as.vector(features[[k]]) -  X.mean[[k]] )
    }
  }


  #print( "Project Data" )
  PU <- list()
  PUS <- list()
  dims = list()
  smoothed = list()
  for( k in 1:n.features ){
    U[[k]] = U[[k]] / sqrt( rowSums( U[[k]]^2) )
    PU[[k]] = matrix(0, nrow=n.components, ncol=n.points)
    PUS[[k]] = matrix(0, nrow=n.components, ncol=n.points)
    dims[[k]] = dim( features[[k]] )

    smoothed[[k]] = U[[k]]
    for( i in 1:nrow(smoothed[[k]]) ){
      smoothed[[k]][i, ] <- as.vector( as.array(
                        smoothImage(
                          as.antsImage(
                            array(smoothed[[k]][i,], dim=dims[[k]] )
                          ),  2
                        )
                      ))
     }
     smoothed[[k]] = smoothed[[k]] / sqrt( rowSums( smoothed[[k]]^2) )
  }
  for( i in 1:n.points ){
    load( files[[i]] )
    for( k in 1:n.features ){
      x = as.vector( features[[k]] ) - X.mean[[k]]
      PU[[k]][, i] = U[[k]] %*% x
      PUS[[k]][, i] = smoothed[[k]] %*% x
    }
  }

  list( components.smoothed = smoothed, components = U, projections = PU,
        projections.smoothed=PUS, mean=X.mean, dimension=dims,
        names=names(features), variances=variance ) #, X=X )
}

load( variables.path )

feature.folders <- list.dirs(features.folder, full.names=TRUE, recursive=FALSE)
for( k in 1:length(feature.folders) ){
  #print(basename(feature.folders[[k]]) )
  files <- sprintf("%s/%s.Rdata", feature.folders[[k]], file.names)
  pcs.file = sprintf( "%s/%s-pcs.Rdata",
                      save.path, basename(feature.folders[[k]]) )
  pcs.split.file = sprintf( "%s/%s-pcs-split.Rdata",
                            save.path, basename(feature.folders[[k]]) )
  pcs.split.max.file = sprintf( "%s/%s-pcs-split-max.Rdata",
                                save.path, basename(feature.folders[[k]]) )

  n.comps.exist = -1
  if( file.exists(pcs.file) ){
    load(pcs.file)
    n.comps.exist = nrow(pcs$components[[1]])
  }
  if( n.comps.exist != n.components){
    #pcs <- principal.components( files, n.components )
    pcs <- principal.components.randomized( files, n.components, variables )
    save( pcs,  file= pcs.file)
  }

  n.comps.exists = -1
  if( file.exists(pcs.split.file) ){
    load(pcs.split.file)
    n.comps.exist = nrow(reg.pcs$components[[1]])
  }
  if( n.comps.exist != n.components){
    #pcs <- principal.components( files, n.components )
    #reg.pcs <- split.principal.components( files, n.components, files )
    #save( reg.pcs,  file= pcs.split.file)
  }

  n.comps.exists = -1
  if( file.exists(pcs.split.max.file) ){
    load(pcs.split.max.file)
    n.comps.exist = nrow(reg.pcs$components[[1]])
  }
  if( n.comps.exist != n.components){
    #pcs <- principal.components( files, n.components )
    #reg.pcs <- split.principal.components.max( files, n.components, files, sigma )
    #save( reg.pcs,  file= pcs.split.max.file)
  }


  #print( "  Computing regularized principal components" )
  reg.pcs <- regularize.principal.components( pcs, lambda.pos, lambda.neg, sigma,
                                              step, n.iters, n.components.reg,
                                              files )
  save( reg.pcs, lambda.pos, lambda.neg, sigma, step, n.iters,
        n.components, n.components.reg,
        file = sprintf("%s/%s-pcs-reg.Rdata", save.path, basename(feature.folders[[k]]) )  )
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
analysis.components(config)

