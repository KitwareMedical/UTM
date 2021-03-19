args <- commandArgs(trailingOnly = TRUE)
print(args)

index1 = as.integer( args[3] )
index2 = as.integer( args[4] )
if(index1 >= index2){
  q()
}

load.path = args[1]
save.path = args[2]

file1 <- sprintf("%s/gmra%.4d.gmra", load.path, index1)
file2 <- sprintf("%s/gmra%.4d.gmra", load.path, index2)

print(file1)

library(mop)
library(gmra)


trp.lp <- multiscale.transport.create.lp( oType = 26 )
icprop <- multiscale.transport.create.iterated.capacity.propagation.strategy( 3, 0 )
multiscale.transport.set.propagation.strategy.1( trp.lp, icprop )

#this one fails -> need to investigate
#multiscale.transport.add.potential.neighborhood.strategy(trp.lp, 0.01, 0.0001)

#multiscale.transport.add.expand.neighborhood.strategy(trp.lp, 4)

gmra1 <- gmra.load.tree( file1 )
gmra2 <- gmra.load.tree( file2 )
trp <- multiscale.transport.solve( trp.lp, gmra1, gmra2, p = 2, nType=0, dType=1, scaleMass=FALSE)

k <- length(trp$cost)
fromMass = trp$fromMass[[k]]
toMass = trp$toMass[[k]]
from = trp$from[[k]]
to = trp$to[[k]]
cost = trp$cost
map = trp$map[[k]]
save( fromMass, from, toMass, to, cost, map, 
      file = sprintf("%s/transport-%.4d-%.4d.Rdata", save.path, index1, index2) )

# Shell command:
#parallel Rscript pairwise.transport.unscaled.R loadpath savepath ::: {1..457} ::: {1..457}
