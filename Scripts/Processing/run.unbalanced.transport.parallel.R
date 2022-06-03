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
if (!exists('file.names')) {
  stop(sprintf("The file %s should have contained a list file.names of input file basenames",config$variablesfile))
}

foreach(name = file.names) %dopar% {
  tryCatch({
    system2( "Rscript", c(sprintf("%s/Processing/unbalanced.transport.R", config$script.folder),
                  config$transport$cost, # Confusingly, this is allocation cost, not "transport cost" -e
                  config$transport$massbalancing,
                  config$transport$gmrafolder,
                  config$barycenters$file,
                  config$transport$transportfolder,
                  config$transport$degree,
                  config$transport$pointsfolder,
                  config$transport$recompute,
                  name)
            )
    },
    error = function(e){
      print(e)
    })
}
invisible(NULL)
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
run.unbalanced.transport.parallel(config)
