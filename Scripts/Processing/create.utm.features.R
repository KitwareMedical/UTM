suppressWarnings({
library(data.table, quietly=TRUE, verbose=FALSE, warn.conflicts=F)
library(mmand, quietly=TRUE, verbose=FALSE, warn.conflicts=F)
library(foreach, quietly=TRUE, verbose=FALSE, warn.conflicts=F)
library(doParallel, quietly=TRUE, verbose=FALSE, warn.conflicts=F)
})

utm.features <- function(config){

transport.folder = config$transport$transportfolder
points.folder = config$transport$pointsfolder
var.file = config$variablesfile
sigma = config$features$utm$sigma
n.parallel <- config$nparallel

load(var.file)

if(n.parallel < 1){
  n.parallel <- detectCores()
} else{
  n.parallel <- min( n.parallel, detectCores() )
}
cl <- makeCluster( n.parallel )
registerDoParallel( cl )

create.image.from.points.2d <- function(filename, dim.x, dim.y){
  load( filename )
  im <- matrix(0, nrow=dim.x, ncol=dim.y)

  im[x] = x.weights

  im
}

create.image.from.points.3d <- function(filename, dim.x, dim.y, dim.z){
  load( filename )
  im <- array(0, dim=c(dim.x, dim.y, dim.z) )
  im[x] = x.weights

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


  difference.to[ abs(difference.to) < 0.001 * min(c(toMass, fromMass)) ] = 0
  difference.from[ abs(difference.from) < 0.001 * min( c(toMass,fromMass) ) ] = 0
  #difference.to[ abs(difference.to) < 10^-10 ] = 0
  #difference.from[ abs(difference.from) < 10^-10 ] = 0

  #list( from.total=from.total, to.total = to.total, to=to, from=from,
  list( minimum = min(from.total, to.total), difference.to=difference.to,
        difference.from=difference.from,
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

  difference.to[ abs(difference.to) < 0.001 * min(c(toMass, fromMass)) ] = 0
  difference.from[ abs(difference.from) < 0.001 * min( c(toMass,fromMass) ) ] = 0

  #list( from.total=from.total, to.total = to.total, to=to, from=from,
  list( minimum = min(from.total, to.total), difference.to=difference.to,
        difference.from=difference.from,
        transport.from=transport.from, transport.to=transport.to)
}





create.average.images.3d <- function(feature.folder, points.folder, transport.folder,
                                     dim.x, dim.y, dim.z, sigma){
  foreach( name = file.names, .export = c('create.image.from.points.3d', 'analyse.mass.3d'),
                    .packages = c("gmra", "data.table", "mmand") ) %dopar% {
  #for( name = file.names){
    points.file <- sprintf("%s/%s.Rdata", points.folder, name)
    trp.file <- sprintf("%s/%s.Rdata", transport.folder, name)
    feature.file = sprintf("%s/%s.Rdata", feature.folder, name)

    intensity = create.image.from.points.3d( points.file, dim.x, dim.y, dim.z )
    load(trp.file)
    mass <- analyse.mass.3d( to, from, toMass, fromMass, map,
                             dim.x, dim.y, dim.z)


    allocation = mass$difference.to - mass$difference.from
    transport  = mass$transport.to - mass$transport.from
    #transport = mass$transport.from
    if(sigma > 0 ){
      val = as.integer(3*sigma)
      val = val + !(val%%2)

      mask = erode( abs( transport ) < 10^-10, array(1, dim=rep(val,3) ) )
      transport  = gaussianSmooth( transport,  rep(sigma,3) )
      transport[mask > 0] = 0

      mask = erode( abs( allocation ) < 10^-10, array(1, dim=rep(val,3) ) )
      allocation = gaussianSmooth( allocation, rep(sigma,3) )
      allocation[mask > 0] = 0

    }
    features = list( allocation=allocation, transport=transport )

    save( features, file=feature.file, compress=FALSE )
  }
  invisible()
}



create.average.images.2d <- function( feature.folder, points.folder, transport.folder,
                                      dim.x, dim.y, sigma,
                                      feature.file.pattern){
  foreach( name = file.names, .export = c('create.image.from.points.2d', 'analyse.mass.2d'),
                    .packages = c("gmra", "data.table", "mmand") ) %dopar% {

    points.file <- sprintf("%s/%s.Rdata", points.folder, name)
    trp.file <- sprintf("%s/%s.Rdata", transport.folder, name)
    feature.file = sprintf("%s/%s.Rdata", feature.folder, name)

    intensity = create.image.from.points.2d( points.file, dim.x, dim.y )

    load(trp.file)
    mass <- analyse.mass.2d( to, from, toMass, fromMass,
                             map, dim.x, dim.y)

#    image(mass$difference.to)
#    image(mass$difference.from)

    allocation <- mass$difference.to - mass$difference.from
    transport  <- mass$transport.to - mass$transport.from
    #transport.from  <- mass$transport.from
    #transport.to <- mass$transport.to
    if(sigma > 0 ){
      val = as.integer(3*sigma)
      val = val + !(val%%2)

      mask = erode( abs( transport ) < 10^-10, array(1, dim=rep(val,2) ) )
      transport  = gaussianSmooth( transport,  rep(sigma,2) )
      transport[mask > 0] = 0

      mask = erode( abs( allocation ) < 10^-10, array(1, dim=rep(val,2) ) )
      allocation = gaussianSmooth( allocation, rep(sigma,2) )
      allocation[mask > 0] = 0

    }
    features = list( allocation=allocation, transport=transport  )
 #, transportto=transport.to, transportfrom = transport.from)
    save( features, file=feature.file, compress=FALSE )
  }
  invisible()
}



feature.folder = config$features$utm$folder
if( length(dims) == 3 ){
  create.average.images.3d(feature.folder, points.folder, transport.folder,
                           dims[1], dims[2], dims[3], sigma)
}else{
  create.average.images.2d(feature.folder, points.folder, transport.folder,
                           dims[1], dims[2], sigma)
}

stopCluster(cl)

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
utm.features(config)
