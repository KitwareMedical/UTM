suppressWarnings({
library(foreach, quietly=TRUE, warn.conflicts=F, verbose=F)
library(doParallel, quietly=TRUE, warn.conflicts=F, verbose=F)
})

run.unbalanced.transport.parallel <- function(config){

n.parallel <- config$nparallel
if(n.parallel < 1){
  n.parallel <- detectCores()
} else{
  n.parallel <- min( n.parallel, detectCores() )
}
cl <- makeCluster( n.parallel )
registerDoParallel( cl )

load(config$variablesfile)

foreach(name = file.names) %dopar% {
  tryCatch({
    system2( "Rscript", sprintf("%s/Processing/unabalanced.transport.R", config$script.folder),
                  config$transport$cost,
                  config$transport$massbalancing,
                  config$transport$gmrafolder,
                  config$barycenters$file,
                  config$transport$transportfolder,
                  config$transport$degree,
                  config$transport$pointsfolder,
                  config$transport$recompute,
                  name
            )
    },
    error = function(e){

    })
}

}
