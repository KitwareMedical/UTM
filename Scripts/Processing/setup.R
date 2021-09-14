suppressMessages(
suppressWarnings({
library(tools, quietly=TRUE, warn.conflicts=F, verbose=F)
library(gmra, quietly=TRUE, warn.conflicts=F, verbose=F)
})
)

setup.utm <- function(config){

images.path         <- config$imagefolder
var.table           <- config$variables
var.file            <- config$variablesfile
points.folder    <- config$transport$pointsfolder
barycenter.euclidean.file <- config$barycenters$euclidean
barycenter.euclidean.gmra.file <- sprintf("%s.gmra",
                                          file_path_sans_ext( barycenter.euclidean.file ) )

mean.sparsity <- config$barycenters$sparsity

vars <- read.table(var.table, sep=",", header=TRUE)

im.mean = NULL
to.orig.index <- c()
index <- 1 # This seems to be not used anywhere. -e
im.counts = NULL
names <- c()
for( i in 1:length(vars$name) ){
  image.file <- sprintf("%s/%s", images.path, vars$name[i])
  if( !file.exists( image.file) ){
    next;
  }
  file.type = tolower( file_ext( image.file ) )
  if( file.type == "png" ){
    library(png, quietly=TRUE, warn.conflicts=F, verbose=F)
    im <- readPNG( image.file )
    if(length(dim(im)) == 3){
      im = as.matrix( im[,,1] )
    }
  }else{
    suppressMessages(
    suppressWarnings(
        library(ANTsR, quietly=TRUE, warn.conflicts=F, verbose=F)
    ))
    im <- as.array(antsImageRead( image.file ))
  }

  if( is.null(im.mean) ){
    im.mean = array(0, dim=dim(im) )
    im.counts = array(0, dim=dim(im) )
  }

  im.mean = im.mean + im
  x <- which(im > 0, arr.ind = TRUE )
  x.weights <- im[x]
  im.counts[x] = im.counts[x] + 1
  name = basename(file_path_sans_ext(image.file))
  names  <- append(names, name)
  save( x, x.weights, file = sprintf( "%s/%s.Rdata", points.folder, name) )
  to.orig.index <- c(to.orig.index, i)
  index <- index+1 # This seems to be not used anywhere. -e

}

n <- nrow(vars)
dims = dim(im.mean)


#Euclidean mean image and gmra
im.mean = im.mean / n
orig.im.mean = im.mean
sum.mean = sum(im.mean)
#im.counts = gaussianSmooth(im.counts, sigma = rep(1,length(dims)) )
index <- which(im.counts < mean.sparsity * n  )
#im.mean[ im.mean < mean( im.mean[im.mean > 0] ) * 0.1 ] = 0
im.mean[index] <- 0
im.mean <- im.mean / sum(im.mean) * sum.mean # Dividing then multiplying like this does nothing, right? Fix? -e
x <- which(im.mean > 0, arr.ind = TRUE )
m.gmra = gmra.create.ikm(X = x, eps = 0, nKids=4, stop=3)
gmra.save.tree( m.gmra, barycenter.euclidean.gmra.file)

barycenter.euclidean = im.mean
barycenter.euclidean.weights = im.mean[x]

#read variables and convert to list
variables <- list()
var.index = 1
for( i in 1:ncol(vars) ){
  name = colnames( vars )[[i]]
  if( colnames( vars )[[i]] != "name" ){
    values = as.numeric(vars[to.orig.index, i])
    index = which( !is.na( values ) )
    variables[[ var.index ]] = list( values=values, name=name, index=index )
    var.index = var.index+1
  }
}

file.names = names
save(variables, dims,n, to.orig.index, file.names, file = var.file)

barycenter = list( image = barycenter.euclidean, orig.image = orig.im.mean,
                   gmra.weights = barycenter.euclidean.weights,
                   gmra.file = barycenter.euclidean.gmra.file )
save(barycenter, file=barycenter.euclidean.file)

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
setup.utm(config)
