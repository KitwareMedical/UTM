library(gmra, quietly=TRUE, warn.conflicts=F, verbose=FALSE )

args <- commandArgs(trailingOnly = TRUE)
recompute <- as.logical(args[1])
points.folder   <- args[2]
gmra.folder   <- args[3]
file.name <- args[4]

point.file = sprintf("%s/%s.Rdata", points.folder, file.name)
gmra.file <-sprintf("%s/%s.gmra", gmra.folder, file.name)
if( !recompute & file.exists(gmra.file) ){
  q()
}

load( point.file )
# We should now have loaded variables x, x.weights:
# - x lists the locations of voxels with nonzero value from the original image file
# - x.weights lists the corresponding values at those voxels
if (!exists('x')) {
  stop(sprintf("The file %s should have contained a list of points x",point.file))
}

gmra <- gmra.create.ikm(X = x, eps = 0, nKids=8, stop=3)
gmra.save.tree( gmra, gmra.file )
