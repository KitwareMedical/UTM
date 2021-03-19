library(mmand, quietly=TRUE)
library(nat, quietly=TRUE)

create.average.images.2d <- function( file.folder, n, sigma, feature.file.pattern, barycenter){  
  res <- list()
  files <- list.files(file.folder, pattern="*.nrrd", full.names=T)

  for( i in 1:length(files)){
    jacobian =  read.nrrd(files[i])
    image(jacobian)
    if(sigma > 0 ){
      jacobian  = gaussianSmooth( jacobian,  rep(sigma,length(dim(jacobian))) )
    }
    jacobian[barycenter$image==0] = 0
    features = list(jacobian = jacobian)
    save( features, file=sprintf(feature.file.pattern, i) )
  }
  
}


args <- commandArgs(trailingOnly = TRUE)
folder <- args[1]
barycenter.file <- args[2]
feature.file.pattern = args[3]
template.file = args[4]
variables.table = args[5]
variables.file = args[6]
sigma = as.double(args[7])
print(sigma)

template <- read.nrrd(template.file)
barycenter=list()
barycenter$image = as.matrix(template)
save(barycenter, file=barycenter.file)

create.average.images.2d(folder, n, sigma, feature.file.pattern, barycenter)




#read variables and convert to list
vars <- read.table(variables.table, sep=",", header=TRUE)
variables <- list()
var.index = 1
for( i in 1:ncol(vars) ){
  name = colnames( vars )[[i]] 
  if( colnames( vars )[[i]] != "name" ){
    values = vars[, i]
    index = which( !is.na(values) )
    variables[[ var.index ]] = list( values=values, name=name, index = index )
    var.index = var.index+1
  }
}

image.files = list.files(folder, pattern="*.nrrd", full.names=T)

n = length(image.files)
dims = dim(template)

save( variables, 
      dims, 
      n, 
      file = variables.file )
