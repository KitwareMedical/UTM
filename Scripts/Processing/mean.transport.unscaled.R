suppressWarnings({
library(mop, quietly=TRUE, warn.conflicts=F, verbose=FALSE )
library(gmra, quietly=TRUE, warn.conflicts=F, verbose=FALSE )
})

args <- commandArgs(trailingOnly = TRUE)

massCost = as.integer( args[1] )
transport.type = as.integer( args[2] )
gmra.file.pattern = args[3]
barycenter.file = args[4]
transport.map.file.pattern = args[5]
p.degree = as.integer( args[6] )
points.file.pattern = args[7]
index = as.integer( args[8] )

gmra.file <- sprintf(gmra.file.pattern, index)
load( barycenter.file )
load( sprintf(points.file.pattern, index ) )

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
save( fromMass, from, toMass, to, cost, map,
      file = sprintf(transport.map.file.pattern, index) )

