suppressWarnings({
library(foreach, quietly=TRUE, warn.conflicts=F, verbose=F)
library(doParallel, quietly=TRUE, warn.conflicts=F, verbose=F)
})

run.create.gmra.parallel <- function(config){

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
    system2( "Rscript", sprintf("%s/Processing/create.gmra.R", config$script.folder),
                  config$transport$recompute,
                  config$transport$pointsfolder,
                  config$transport$gmrafolder,
                  name
            )
    },
    error = function(e){

    })
}

}
