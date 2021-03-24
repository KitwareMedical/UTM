suppressMessages(
suppressWarnings({
library(tools, quietly=TRUE, warn.conflicts=F, verbose=FALSE)
library(mmand, quietly=TRUE, warn.conflicts=F, verbose=FALSE)
library(foreach, quietly=TRUE, warn.conflicts=F, verbose=FALSE)
library(doParallel, quietly=TRUE, warn.conflicts=F, verbose=FALSE)
library(ANTsR, quietly=TRUE, warn.conflicts=F, verbose=FALSE)
})
)

convolutional.transport.features <- function(config){

barycenter.file = config$barycenters$euclidean
images.path = config$imagefolder
var.table = config$variables
conv.sigma = config$features$conv$sigma
conv.lambda1 = config$features$conv$lambda.mean
conv.lambda2 = config$features$conv$lambda.target
conv.iterations = config$features$conv$iterations
n.parallel = config$nparallel

if(n.parallel < 1){
  n.parallel <- detectCores()
} else{
  n.parallel <- min( n.parallel, detectCores() )
}
cl <- makeCluster( n.parallel )
registerDoParallel( cl )



load( barycenter.file )
vars <- read.table(var.table, sep=",", header=TRUE)

foreach(i=1:length(vars$name), .combine="+",
        .packages = c("tools", "ANTsR") ) %dopar% {
#for( i in 1:length(vars$name) ){

  load.image <- function(image.file){
    file.type = tolower( file_ext( image.file ) )
    if(      file.type == "png"   ){
      library(png, quietly=TRUE, warn.conflicts=F)
      im <- readPNG( image.file )
      if(length(dim(im)) == 3){
        im = as.matrix( im[,,1] )
      }
    }
    else{
      suppressMessages(
      suppressWarnings(
        library(ANTsR, quietly=TRUE, warn.conflicts=F, verbose=F)
      ))
      im <- as.array(antsImageRead( image.file ))
    }
    im
  }


  convolutionalWasserstein <- function(im1, im2, sigma, lambda1, lambda2, iters){

    dims <- dim(im1)
    d <- length(dim)
    a <- 1/prod(dims)
    v <- array(1, dim=dims)
    w <- array(1, dim=dims)

    #im1 <- im1
    #im2 <- im2


    eps =  0 #10^-10
    for(i in 1:iters){
      ws <- as.array( smoothImage( as.antsImage( w ), sigma ) )
      v  <- ( ws^( sigma^2 / (lambda1 + sigma^2) ) * ( im1 )^( lambda1 / (lambda1 + sigma^2) ) )
      #v <- pmin( ws * exp( lambda1 / sigma^2 ), pmax(ws * exp( -lambda1 / sigma^2 ), im1) )
      ind <- which(ws > 0)
      v[ind] = v[ind] / (ws[ind] + eps)

      vs <- as.array( smoothImage( as.antsImage( v ) , sigma ) )
      w  <- ( vs^( sigma^2 / (lambda2 + sigma^2) ) * ( im2 )^( lambda2 / (lambda2 + sigma^2) ) )
      #w <- pmin( vs * exp( lambda2 / sigma^2 ), pmax(vs * exp( -lambda2 / sigma^2 ), im2) )
      ind <- which(vs > 0 )
      w[ind] = w[ind] / (vs[ind] + eps)
    }
    #w <- w / sum(w) * sum(im1)
    #v <- v / sum(v) * sum(im2)
    a1 <- as.array( smoothImage( as.antsImage( v ), sigma ) ) * w
    a2 <- as.array( smoothImage( as.antsImage( w ), sigma ) ) * v
    #print( max(im1) )
    #print( max(a2) )
    #delta <- im1 - v
    #delta[ abs(delta) < max(im1)*a*eps ] <- 0
    delta  = a2 - im1
    delta[ abs(delta) < pmax( abs(im1), abs(a2)) * 0.1 ] = 0
    delta
  }




  image.file <- sprintf("%s/%s", images.path, vars$name[i])
  if( !file.exists(image.file)){
    return(NULL)
  }
  image <- load.image(image.file)
  mean <- barycenter$image #orig.image

  convw <- convolutionalWasserstein( mean, image, conv.sigma,
                                     conv.lambda1, conv.lambda2,
                                     conv.iterations)

  features = list(convolutional = convw)
  file.name = basename(file_path_sans_ext(image.file))
  conv.file = sprintf("%s/%s.Rdata", config$features$conv$folder, file.name)
  save( features, file = conv.file, compress=FALSE )

  NULL

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
convolutional.transport.features(config)
