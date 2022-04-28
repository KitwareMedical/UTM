suppressWarnings({
library(tools, quietly=TRUE, warn.conflicts=F, verbose=F)
library(mmand, quietly=TRUE, warn.conflicts=F, verbose=F)
library(foreach, quietly=TRUE, warn.conflicts=F, verbose=F)
library(doParallel, quietly=TRUE, warn.conflicts=F, verbose=F)
library(ANTsRCore, quietly=TRUE, warn.conflicts=F, verbose=F)
library(ANTsR, quietly=TRUE, warn.conflicts=F, verbose=F)
})

vbm.features <- function(config){

barycenter.file = config$barycenters$euclidean
images.path = config$imagefolder
var.table <- config$variables
sigma = config$features$vbm$sigma
weight.sigma = 0
transport.sigma = 0
n.parallel <- config$nparallel
recompute = config$features$recompute

if(n.parallel < 1){
  n.parallel <- detectCores()
} else{
  n.parallel <- min( n.parallel, detectCores() )
}
cl <- makeCluster( n.parallel )
registerDoParallel( cl )


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
if (!("image" %in% names(barycenter))) {
  stop(sprintf("The list 'barycenter' in %s should contain a named item 'image'", barycenter.file))
}

vars <- read.table(var.table, sep=",", header=TRUE)

foreach(i=1:length(vars$name), .combine="+",
        .packages = c("mmand", "tools", "ANTsR") ) %dopar% {
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

  smoothAndCrop <- function(im, sigma, domain){
    if( sigma > 0 ){
      im  <- as.array( smoothImage( as.antsImage(im), length( dim(im) ) ) )
    }
    if( sigma < 0){
      im  <- array( mean(im), dim=dims)
    }
    #w <- sum(im)
    #im[!domain] = 0
    #wd <- sum( im )
    #im <- im * w / wd
    im
  }


  image.file <- sprintf("%s/%s", images.path, vars$name[i])
  if(!file.exists(image.file)){
    next
  }
  file.name = basename(file_path_sans_ext(image.file))
  vbm.file = sprintf("%s/%s.Rdata", config$features$vbm$folder, file.name)

  if( !recompute & file.exists(vbm.file) ){
    return( NULL )
  }

  image <- load.image(image.file)
  mean <- barycenter$image #orig.image
  #vbm.allocation <- convolutionalWasserstein(mean, image, 1, 20)
  vbm <- image

  domain <- (mean > 0)  & (image > 0)
  dims <- dim(image)
  d <- length(dims)

  #w.mean <- smoothAndCrop(mean, weight.sigma, domain)
  #w.image <- smoothAndCrop(image, weight.sigma, domain)
  #w.delta <- w.mean -  w.image


 # t.mean <- smoothAndCrop(mean, transport.sigma, domain)
 # t.image <- smoothAndCrop(image, transport.sigma , domain)
 # t.delta   <- t.mean - t.image

 # delta   <- mean - image
 # eps = 10^-10
 # pos <-  (delta > eps)  * ( w.delta > eps )  * ( t.delta > eps )
 # neg <-  (delta < -eps) * (w.delta < -eps) * ( t.delta < -eps )


 # vbm.allocation <- array( 0, dim=dims )
 # pos <- which(pos > 0)
 # neg <- which(neg > 0)
 # if( length(pos) > 0 ){
 #    vbm.allocation[pos] = -pmin( w.delta[pos], t.delta[pos] )
 # }
 # if( length(neg) > 0 ){
 #    vbm.allocation[neg] = -pmax( w.delta[neg], t.delta[neg] )
 # }



  if(sigma > 0 ){
    val = as.integer(3*sigma)
    val = val + !(val%%2)

    mask = erode( abs( vbm ) < 10^-10, array(1, dim=rep(val,d) ) )
    vbm = gaussianSmooth( vbm,  rep(sigma,d) )
    vbm[mask > 0] = 0

   # mask = erode( abs( vbm.transport ) < 10^-10, array(1, dim=rep(val,d) ) )
   # vbm.transport  = gaussianSmooth( vbm.transport,  rep(sigma,d) )
   # vbm.transport[mask > 0] = 0

  #  mask = erode( abs( vbm.allocation ) < 10^-10, array(1, dim=rep(val,d) ) )
  #  vbm.allocation = gaussianSmooth( vbm.allocation, rep(sigma,d) )
  #  vbm.allocation[mask > 0] = 0
  }

  features = list(#vbmallocation = vbm.allocation,
                  vbm = vbm )
  save( features, file = vbm.file )

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
vbm.features(config)
