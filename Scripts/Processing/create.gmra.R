library(gmra, quietly=TRUE, warn.conflicts=F, verbose=FALSE )

args <- commandArgs(trailingOnly = TRUE)
gmra.file   <- args[1]
point.file <-  args[2]
i <- as.integer( args[3] )
load( sprintf(point.file, i) )
gmra <- gmra.create.ikm(X = x, eps = 0, nKids=8, stop=3)
gmra.save.tree( gmra, sprintf(gmra.file, i) )
