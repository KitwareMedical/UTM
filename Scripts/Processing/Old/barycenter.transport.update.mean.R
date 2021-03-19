args <- commandArgs(trailingOnly = TRUE)
print(args)

gmra.load.path = args[1]
gmra.file.pattern = args[2] 
mean.file.prefix = args[3]

files <- list.files( gmra.load.path, gmra.file.pattern, full.names=TRUE )

mean.gmra <- sprintf("%s.gmra", mean.file.prefix)
mean.rdata <- sprintf("%s.Rdata", mean.file.prefix)


library(mop)
library(gmra)



X.mean = load(mean.rdata)

X.update <- as.data.table( matrix(0, nrow = nrow(X.mean), ncol = ncol(X.mean) ) )
mass.update <- rep(0, nrow(X.mean) )
    
cost <- 0

for( i in 1:length(files) ){
  print(i)
  trp <- load( files[[i]] )

  n = length(trp$cost)
  cost <- cost + trp$cost[[n]]
  print(trp$cost[[n]]) 
  if(trp$cost[[n]] > 0 ){
    
    map = trp$map[[n]]
    from.index = trp$fromIndex[[n]]
    from.size = trp$fromSize[[n]] 
       
    delta = as.data.table( trp$to[[n]][map[,2], ] - trp$from[[n]][map[,1], ] )
    delta = delta * map[,3]
    delta$from.id = map[,1]
    delta = delta[, lapply(.SD, sum, na.rm=TRUE), by = from.id]
    delta = delta[order(from.id)]

    delta.mass = data.table( from.id = map[,1], mass=map[,3] )
    delta.mass = delta.mass[, lapply(.SD, sum, na.rm=TRUE), by = from.id]
    delta.masss = delta.mass[order(from.id)]   

    delta = delta[,!"from.id"]

    delta = delta[ rep(1:length(from.size), from.size), ]
    delta.mass = delta.mass[ rep(1:length(from.size), from.size), ]

    X.update[ from.index, ]  = X.update[ from.index, ]  + delta
    mass.update[ from.index ] = mass.update[ from.index ] + delta.mass$mass

  }
}
    

if( prev.cost < cost ){
  step = 0.9 * step
}
X.update = X.update / mass.update
X.update = step * X.update / length(gmra.marginals)
avg.step = mean( sqrt(rowSums(X.update^2) ) )
X.mean = X.mean + X.update
    
gmra.mean = gmra.create.ikm(X.mean, eps=mean.radius, nKids = 4, stop=3)
   
save(X.mean, file=mean.rdata)
gmra.save.tree( gmra.mean, file=mean.gmra )
 
invisible( cost, avg.step, step)
