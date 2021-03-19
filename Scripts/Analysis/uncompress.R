#required librarys

## Script paramaters
args <- commandArgs( trailingOnly=TRUE)

features.folder <- args[1]
feature.folders <- list.dirs(features.folder, full.names=TRUE, recursive=FALSE)
for( k in 1:length(feature.folders) ){
  print(basename(feature.folders[[k]]) )
  files <- list.files(feature.folders[[k]], full.names=TRUE)
  for( file in files){
    load(file)
    save(features, file=file, compress=FALSE)
  }
}
