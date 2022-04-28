suppressWarnings({
library(mop, quietly=TRUE, warn.conflicts=F, verbose=FALSE )
library(gmra, quietly=TRUE, warn.conflicts=F, verbose=FALSE )
})

args <- commandArgs(trailingOnly = TRUE)

massCost = as.integer( args[1] )
transport.type = as.integer( args[2] )
gmra.folder = args[3]
barycenter.file = args[4]
transport.folder = args[5]
p.degree = as.integer( args[6] )
points.folder = args[7]
recompute = as.logical(args[8])
file.name = args[9]

gmra.file <- sprintf("%s/%s.gmra", gmra.folder, file.name)
points.file <- sprintf("%s/%s.Rdata", points.folder, file.name)
trp.file <- sprintf("%s/%s.Rdata", transport.folder, file.name)
if( !recompute & file.exists(trp.file) ){
  q()
}

load( barycenter.file )
# We should now have a list, barycenter, that contains data for the template image that was built out of the population:
# - barycenter$orig.image is the euclidean mean image
# - barycenter$image is the sparse euclidean mean image
# - barycenter$gmra.weights is a vector of voxel values of the sparse euclidean mean image, at the locations where that value is not zero
# - barycenter$gmra.file is the location of the GMRA (geometric multiresolution analysis) file that contains the IKmeans tree
#   (see https://github.com/samuelgerber/gmra/blob/d7e27ef6e80572f1f2d7694a07d518b8f104972b/src/gmra.cc#L181)
if (!exists('barycenter')) {
  stop(sprintf("The file %s should have contained a list 'barycenter' describing the constructed template image", barycenter.file))
}
if (!(("gmra.weights" %in% names(barycenter)) && ("gmra.file" %in% names(barycenter)))) {
  stop(sprintf("The list 'barycenter' in %s should contain named items 'gmra.weights' and 'gmra.file'", barycenter.file))
}

#needed for point weights (x.weights)
load( points.file )
# We should now have loaded variables x, x.weights:
# - x lists the locations of voxels with nonzero value from the original image file
# - x.weights lists the corresponding values at those voxels
if (!exists('x.weights')) {
  stop(sprintf("The file %s should have contained a list of values x.weights", points.file))
}

trp.lp <- multiscale.transport.create.lp( oType = 31, transport.type=transport.type,
                                          massCost=massCost )
icprop <- multiscale.transport.create.iterated.capacity.propagation.strategy( 3, 0 )
multiscale.transport.set.propagation.strategy.1( trp.lp, icprop )

gmra1 <- gmra.load.tree( barycenter$gmra.file )
gmra2 <- gmra.load.tree( gmra.file )
trp <- multiscale.transport.solve( trp.lp, gmra1, gmra2, p = p.degree,
                                  nType=1, dType=1, scaleMass = transport.type==0,
                                  w1=barycenter$gmra.weights, w2 = x.weights )

k <- length(trp$cost)
fromMass = trp$fromMass[[k]]
toMass = trp$toMass[[k]]
from = trp$from[[k]]
to = trp$to[[k]]
cost = trp$cost
map = trp$map[[k]]
save( fromMass, from, toMass, to, cost, map, file = trp.file )

