library(tools, quietly=TRUE, warn.conflicts=F)
library(mmand, quietly=TRUE)
library(foreach, quietly=TRUE, warn.conflicts=F)
library(doParallel, quietly=TRUE, warn.conflicts=F)
library(ANTsR, quietly=TRUE)

load.image <- function(image.file){
  file.type = tolower( file_ext( image.file ) )
  if(      file.type == "png"   ){
    library(png, quietly=TRUE, warn.conflicts=F)
    im <- readPNG( image.file )
    if(length(dim(im)) == 3){
      im = as.matrix( im[,,1] )
    }
  }
  else if( file.type == "hdr" | 
           file.type == "nii"   ) {
    suppressPackageStartupMessages( library(neuroim, quietly=TRUE, warn.conflicts=F) )
    im <- loadVolume( image.file )
  }
  else if( file.type == "nrrd"  ) {
    suppressPackageStartupMessages( library(nat, quietly=TRUE, warn.conflicts=F) )
    im <- read.nrrd( image.file, ReadByteAsRaw="none" )
    im = array( as.double(im), dim=dim(im) )
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
  #a1 - a
  print(max(a2))
  print( sum(im1) ) 
  print( sum(im2) ) 
  print( sum(a1) ) 
  print( sum(a2) ) 
  image(cbind(rbind(im1, im2), rbind(a1, a2) ))
  #print( max(im1) )
  #print( max(a2) )
  #delta <- im1 - v
  #delta[ abs(delta) < max(im1)*a*eps ] <- 0
 # delta 
  list(im1 = im1, im2 = im2, a1 = a1, a2 = a2)
}


