library(data.table)
library(gmra)

create.image.from.gmra.2d <- function(filename, dim.x, dim.y){
        print(filename)
  gmra <- gmra.load.tree( filename )
  pts <- gmra.centers(gmra, 1000)
  print(dim(pts))
  im <- matrix(0, nrow=dim.x, ncol=dim.y)

  im[pts] = 1

  im
}

create.image.from.gmra.3d <- function(filename, dim.x, dim.y, dim.z){
  gmra <- gmra.load.tree( filename )
  pts <- gmra.centers(gmra, 1000)
  im <- array(0, dim=c(dim.x, dim.y, dim.z) )
  im[pts] = 1

  im
}


analyse.mass.3d <- function(toCoords, fromCoords, toMass, fromMass, map, dim.x, dim.y, dim.z){
   
  from <- as.vector(fromMass)
  to   <- as.vector(toMass)
  from.total <- sum( from )
  to.total   <- sum( to )

  map <- as.data.table(map)
  map.from <- map[,.( mass=sum(mass), cost = sum(cost*mass)/sum(mass) ), by=from]
  map.to <- map[,.( mass=sum(mass), cost = sum(cost*mass)/sum(mass) ), by=to]
  to[map.to[,to]] = to[map.to[,to]] - map.to[, mass] 
  from[map.from[,from]] = from[map.from[,from]] - map.from[, mass] 


  difference.from = array( 0, dim = c(dim.x, dim.y, dim.z) )
  transport.from = array( 0, dim = c(dim.x, dim.y, dim.z) )
  from <- data.table( x=fromCoords[,1], y=fromCoords[,2], z=fromCoords[,3] , from = from )
  from <- from[, .(from = sum(from)), by=.(x,y,z)]
  difference.from[ as.matrix(from[, .(x, y, z)]) ] = from[, from]

  from.cost <- numeric( length(fromMass) )
  from.cost[ map.from[,from]] <- map.from[,cost]
  from.cost <- data.table( x=fromCoords[,1], y=fromCoords[,2], z=fromCoords[,3], cost=from.cost)
  from.cost <- from.cost[, .(cost=sum(cost)), by=.(x,y,z)]
  transport.from[ as.matrix( from.cost[, .(x, y, z)]) ] = from.cost[, cost]


  difference.to = array( 0, dim = c(dim.x, dim.y, dim.z) )
  transport.to = array( 0, dim = c(dim.x, dim.y, dim.z) )
  
  to <- data.table( x=toCoords[,1], y=toCoords[,2], z=toCoords[,3], to = to )
  to <- to[, .(to=sum(to)), by=.(x,y,z)]
  difference.to[ as.matrix( to[, .(x, y, z)] ) ] = to[, to]

  to.cost <- numeric( length(toMass) ) 
  to.cost[map.to[,to]] <- map.to[,cost]
  to.cost <- data.table( x=toCoords[,1], y=toCoords[,2], z=toCoords[,3], cost=to.cost)
  to.cost <- to.cost[, .( cost = sum(cost)), by=.(x,y,z)]
  transport.to[ as.matrix(to.cost[, .(x, y, z)]) ] = to.cost[, cost]


  #list( from.total=from.total, to.total = to.total, to=to, from=from, 
  list( minimum = min(from.total, to.total), difference.to=difference.to, difference.from=difference.from, 
        transport.from=transport.from, transport.to=transport.to)
}


analyse.mass.2d <- function(toCoords, fromCoords, toMass, fromMass, map, dim.x, dim.y){
   
  from <- as.vector(fromMass)
  to   <- as.vector(toMass)
  from.total <- sum( from )
  to.total   <- sum( to )

  map <- as.data.table(map)
  map.from <- map[,.( mass=sum(mass), cost = sum(cost*mass)/sum(mass) ), by=from]
  map.to <- map[,.( mass=sum(mass), cost = sum(cost*mass)/sum(mass) ), by=to]
  to[map.to[,to]] = to[map.to[,to]] - map.to[, mass] 
  from[map.from[,from]] = from[map.from[,from]] - map.from[, mass] 


  difference.from = array( 0, dim = c(dim.x, dim.y) )
  transport.from = array( 0, dim = c(dim.x, dim.y) )
  from <- data.table( x=fromCoords[,1], y=fromCoords[,2], from = from )
  from <- from[, .(from = sum(from)), by=.(x,y)]
  difference.from[ as.matrix(from[, .(x, y)]) ] = from[, from]

  from.cost <- numeric( length(fromMass) )
  from.cost[ map.from[,from]] <- map.from[,cost]
  from.cost <- data.table( x=fromCoords[,1], y=fromCoords[,2], cost=from.cost)
  from.cost <- from.cost[, .(cost=sum(cost)), by=.(x,y)]
  transport.from[ as.matrix( from.cost[, .(x, y)]) ] = from.cost[, cost]


  difference.to = array( 0, dim = c(dim.x, dim.y) )
  transport.to = array( 0, dim = c(dim.x, dim.y) )
  
  to <- data.table( x=toCoords[,1], y=toCoords[,2], to = to )
  to <- to[, .(to=sum(to)), by=.(x,y)]
  difference.to[ as.matrix( to[, .(x, y)] ) ] = to[, to]

  to.cost <- numeric( length(toMass) ) 
  to.cost[map.to[,to]] <- map.to[,cost]
  to.cost <- data.table( x=toCoords[,1], y=toCoords[,2], cost=to.cost)
  to.cost <- to.cost[, .( cost = sum(cost)), by=.(x,y)]
  transport.to[ as.matrix(to.cost[, .(x, y)]) ] = to.cost[, cost]


  #list( from.total=from.total, to.total = to.total, to=to, from=from, 
  list( minimum = min(from.total, to.total), difference.to=difference.to, difference.from=difference.from, 
        transport.from=transport.from, transport.to=transport.to)
}





create.average.images.3d <- function( load.path, gmra.path, n, dim.x, dim.y, dim.z){  
  res <- list()
  res$allocation <- as.list( rep(0, length = n)  )
  res$transport  <- as.list( rep(0, length = n)  )
  res$intensity  <- as.list( rep(0, length = n)  )
  for( i in 1:n ){
  print( i )
  filename = sprintf("%s/gmra%.4d.gmra", gmra.path, i)
  res$intensity[[i]] = create.image.from.gmra.3d( filename, dim.x, dim.y, dim.z )
 
  for( j in 1:n ){
     filename = sprintf("%s/transport-%.4d-%.4d.Rdata", load.path, i, j)
     if( file.exists( filename ) ){
       #gray matter
       load(filename)
       mass <- analyse.mass.3d( to, from, toMass, fromMass, map, 
                             dim.x, dim.y, dim.z)

       res$allocation[[i]] = res$allocation[[i]] + mass$difference.from - mass$difference.to
       res$transport[[i]] = res$transport[[i]] + mass$transport.from
       res$allocation[[j]] = res$allocation[[j]] + mass$difference.to - mass$difference.from
       res$transport[[j]] = res$transport[[j]] + mass$transport.to
     }
  }
  }
  res
}
create.average.images.2d <- function( load.path, gmra.path, n, dim.x, dim.y){  
  res <- list()
  res$allocation <- as.list( rep(0, length = n)  )
  res$transport  <- as.list( rep(0, length = n)  )
  res$intensity  <- as.list( rep(0, length = n)  )
  for( i in 1:n ){
    print( i )
    filename = sprintf("%s/gmra%.4d.gmra", gmra.path, i)
    res$intensity[[i]] = create.image.from.gmra.2d( filename, dim.x, dim.y )
    
    for( j in 1:n ){
      filename = sprintf("%s/transport-%.4d-%.4d.Rdata", load.path, i, j)
      if( file.exists( filename ) ){
        #gray matter
        load(filename)
        mass <- analyse.mass.2d( to, from, toMass, fromMass, map, 
                             dim.x, dim.y)

        res$allocation[[i]] = res$allocation[[i]] + mass$difference.from - mass$difference.to
        res$transport[[i]] = res$transport[[i]] + mass$transport.from
        res$allocation[[j]] = res$allocation[[j]] + mass$difference.to - mass$difference.from
        res$transport[[j]] = res$transport[[j]] + mass$transport.to
      }
    }
  }
  res
}


args <- commandArgs(trailingOnly = TRUE)
transport.maps.path = args[1]
gmra.path = args[2]
save.path = args[3]
var.path = args[4]

load(var.path)

if( length(dims) == 3 ){
  avg.images <- create.average.images.3d(transport.maps.path, gmra.path, n, dims[1], dims[2], dims[3])
}else{
  avg.images <- create.average.images.2d(transport.maps.path, gmra.path, n, dims[1], dims[2])
}

save(avg.images, file=save.path )
