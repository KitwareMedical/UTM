library(optparse, quietly=TRUE)
library(rsconnect)

option_list = list(
  make_option("--appname" , type="character", default="None",
              help="App name for shinyapps.io server[default=None]", metavar="character"),
  make_option("--directory", type="character", default=".",
              help="Directory to deploy [default=.]", metavar="character")
  )


pos.args.help = " "


opt_parser = OptionParser( option_list=option_list,
                           usage = "usage: %prog [options]",
                           description=pos.args.help)
opts <- try( parse_args(opt_parser, positional_arguments=0), silent=FALSE )

if( inherits(opts, "try-error") ){
  print_help( opt_parser )
  quit( "no", 1)
}

config = opts$options

script.file <- sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1])
if (.Platform$OS.type == "windows") {
  script.file <- gsub("\\\\", "\\\\\\\\", prog)
}
script.folder="."
try( {script.file = normalizePath(script.file);
      script.folder <- dirname(script.file)}, silent=TRUE)
if( is.na(script.folder) ){
  script.folder="."
}

#remove unessecary data
load(sprintf( "%s/Analysis/Images/VBM-pcs.Rdata", config$directory) )
pcs$projections.smoothed = NULL
pcs$correlations = NULL
save(pcs, file = sprintf( "%s/Analysis/Images/VBM-pcs.Rdata", config$directory) )

load(sprintf( "%s/Analysis/Images/VBM-pcs-reg.Rdata", config$directory) )
reg.pcs$projections.smoothed = NULL
reg.pcs$correlations = NULL
save(reg.pcs, file = sprintf( "%s/Analysis/Images/VBM-pcs-reg.Rdata", config$directory) )

#files to upload
appFiles = c( "Barycenter/barycenter-euclidean.Rdata",
             "Analysis/Images/VBM-pcs.Rdata",
             "Analysis/Images/VBM-pcs-reg.Rdata",
             "Analysis/Images/VBM-ms.Rdata",
             "variables.Rdata",
             "shiny-help.md",
             "render.js",
             "app.R" )
if( file.exists( sprintf("%s/Barycenter/barycenter-euclidean.Rdata", config$directory) ) ){
  appFiles = append(appFiles, "Barycenter/barycenter-euclidean.Rdata")
}
#copy default atlas
file.copy( sprintf("%s/../../Atlas/sri24/labels/atlas.Rdata", script.folder),
           sprintf("%s/atlas.Rdata", config$directory) )
if( file.exists( sprintf("%s/atlas.Rdata", config$directory) ) ){
  appFiles = append(appFiles, "atlas.Rdata")
}


#copy help file
file.copy( sprintf("%s/shiny-help.md", script.folder),
           sprintf("%s/shiny-help.md", config$directory) )

#copy vtkwidget render javascript
file.copy( sprintf("%s/../ShinyVtkScripts/render.js", script.folder),
           sprintf("%s/render.js", config$directory) )
#copy app
file.copy( sprintf("%s/app.R", script.folder),
           sprintf("%s/app.R", config$directory) )



deployApp( config$directory, appName=config$appname,  appFiles = appFiles )
configureApp( config$appname,  size="xxxlarge" )
