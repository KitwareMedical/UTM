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
gmra <- gmra.create.ikm(X = x, eps = 0, nKids=8, stop=3)
gmra.save.tree( gmra, gmra.file )
