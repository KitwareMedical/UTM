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
if (!exists('file.names')) {
  stop(sprintf("The file %s should have contained a list file.names of input file basenames",config$variablesfile))
}

foreach(name = file.names) %dopar% {
  tryCatch({
    system2( "Rscript", c( sprintf("%s/Processing/create.gmra.R", config$script.folder),
                  config$transport$recompute,
                  config$transport$pointsfolder,
                  config$transport$gmrafolder,
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
run.create.gmra.parallel(config)
