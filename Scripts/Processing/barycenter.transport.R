suppressMessages(
suppressWarnings({
library(tools, quietly=TRUE, warn.conflicts=F, verbose=FALSE )
library(mop, quietly=TRUE)
library(gmra, quietly=TRUE)
library(mmand, quietly=TRUE)
})
)

compute.transport.barycenter <- function(config){

gmra.file.pattern = config$transport$gmrafilebase
barycenter.ot.discrete.file = config$barycenters$otdiscrete
barycenter.ot.file = config$barycenters$ot
trp.file.path = config$transport$filepattern
barycenter.euclidean.file = config$barycenters$euclidean
barycenter.solution.file = config$barycenters$otsolution
transport.type = config$transport$massbalancing
massCost = config$transport$cost
p.degree = config$transport$degree
points.file.pattern <- config$transport$pointsfilepattern
n.parallel <- config$nparallel

barycenter.ot.gmra.file <-
  sprintf("%s.gmra", file_path_sans_ext( barycenter.ot.file ) )
barycenter.ot.discrete.gmra.file <-
  sprintf("%s.gmra", file_path_sans_ext( barycenter.ot.discrete.file ) )
gmra.files <- list.files(dirname(gmra.file.pattern),
                         basename(gmra.file.pattern), full.names=TRUE )
trp.files <- sprintf( trp.file.path, 1:length(gmra.files) )

load( barycenter.euclidean.file )
dims = dim(barycenter$image)
mean.image = barycenter$image
X.mean <- which( mean.image > 0, arr.ind = TRUE )
#mean.image = gaussianSmooth(barycenter$image, sigma = rep(1,length(dims)) )
#X.mean <- which( mean.image > mean( mean.image[mean.image > 0] ) * 0.9 , arr.ind = TRUE)
mean.weights = mean.image[X.mean]
mean.weights = mean.weights * sum(barycenter$image) / sum(mean.weights)

#gmra.tmp <- gmra.load.tree(gmra.files[[1]])
#X.mean <- gmra.centers(gmra.tmp, 100)
#mean.weights = rep(1, nrow(X.mean) )
#mean.weights = mean.weights / sum(mean.weights) * sum( barycenter$image )

weights <- list()
for(i in 1:length(gmra.files) ){
  load( sprintf(points.file.pattern, i) )
  weights[[i]] = x.weights
}

#X.mean = gmra.centers( gmra.load.tree( gmra.files[[1]] ) , 1000)

#trp.lp <- multiscale.transport.create.lp( oType = 30 )
#icprop <- multiscale.transport.create.iterated.capacity.propagation.strategy( 3, 0 )
#multiscale.transport.set.propagation.strategy.1( trp.lp, icprop )

#this one fails -> need to investigate
#multiscale.transport.add.potential.neighborhood.strategy(trp.lp, 0.01, 0.0001)
#multiscale.transport.add.expand.neighborhood.strategy(trp.lp, 4)

barycenter.solution = multiscale.transport.barycenter.parallel(
                          X.mean, barycenter.ot.gmra.file, mean.weights,
                          gmra.files, weights, trp.files, scaleMass=transport.type==0,
                          transport.type = transport.type, massCost=massCost,
                          n.iterations=20, step=0.5, p=p.degree, n.parallel=n.parallel )

barycenter = list( image = NA,
                   gmra.weights = mean.weights,
                   gmra.file = barycenter.ot.gmra.file )
save(barycenter, file = barycenter.ot.file)

barycenter.ot.discrete <- array(0, dim=dims)
ind <- round(as.matrix(barycenter.solution$X.mean))
storage.mode(ind) = "integer"
for( i in 1:nrow(ind) ){
  barycenter.ot.discrete[ t(ind[i,]) ] = barycenter.ot.discrete[ t(ind[i,]) ]  + mean.weights[i]
}
barycenter.ot.discrete = gaussianSmooth(barycenter.ot.discrete,
                                        sigma = rep(1, length(dim(barycenter.ot.discrete) ) ) )
sumw = sum(barycenter.ot.discrete)
barycenter.ot.discrete[ barycenter.ot.discrete < 0.1 *
                       mean(barycenter.ot.discrete[barycenter.ot.discrete>0]) ] = 0

X <- which(barycenter.ot.discrete > 0, arr.ind=TRUE)
barycenter.ot.discrete.weights = barycenter.ot.discrete[X] #rep(1, nrow(X))
barycenter.ot.discrete.weights = barycenter.ot.discrete.weights *
  sumw / sum(barycenter.ot.discrete)

save( barycenter.solution,
      file = barycenter.solution.file )

m.gmra <- gmra.create.ikm(X, eps=0, nKids=2^(length(dims) ), stop=3 )
gmra.save.tree( m.gmra, barycenter.ot.discrete.gmra.file )

barycenter = list( image = barycenter.ot.discrete,
                   gmra.weights = barycenter.ot.discrete.weights,
                   gmra.file = barycenter.ot.discrete.gmra.file )
save(barycenter, file=barycenter.ot.discrete.file)
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
compute.transport.barycenter(config)
