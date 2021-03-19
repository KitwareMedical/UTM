#!/usr/bin/env Rscript


# Load packages
library(shiny, quietly=TRUE)
library(shinyBS, quietly=TRUE)
library(shinythemes, quietly=TRUE)
library(RColorBrewer, quietly=TRUE)
library(markdown, quietly=TRUE)
library(optparse, quietly=TRUE)
library(stringr, quietly=TRUE)

option_list = list(  
  make_option("--barycenterfolder", type="character", default="Barycenter", 
              help="Subfolder in Analysis for storing barycenters [default=Barycenter]", metavar="character"),
  make_option(c("-a", "--analysisfolder"), type="character", default="Analysis", 
              help="Folder to store results in. [default=Analysis]", metavar="character"),
  make_option(c("-b", "--barycentertype"), type="character", default="Euclidean", 
              help="Barycenter to display [default = Euclidean]", metavar="character"),
  make_option("--workingfolder", type="character", default=".", 
              help="Working directory folder [default=.]", metavar="character"),
  make_option("--pointsfolder", type="character", default="Points", 
              help="Folder for storing Point cloud data [default=Points]", metavar="character")
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

config$barycenters = list()
config$barycenters$euclidean = sprintf("./%s/barycenter-euclidean.Rdata", config$barycenterfolder)
config$barycenters$otdiscrete = sprintf("./%s/barycenter-ot-discrete.Rdata", config$barycenterfolder)

config$correlationfile = sprintf("./%s/cor-vbm-utm.Rdata", config$analysisfolder) 

config$variablesfile = "./variables.Rdata"

config$pointsfilepattern = sprintf("./%s/points%s.Rdata", config$pointsfolder, "%04d" )


#Script folder
script.file <- sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1])
if (.Platform$OS.type == "windows") {
  script.file <- gsub("\\\\", "\\\\\\\\", prog)
}

script.folder="."
try( {script.file = normalizePath(script.file); 
      script.folder <- dirname(script.file)}, silent=TRUE)
if(is.na(script.folder) ){
  script.folder="."
}
help.file <- sprintf("%s/shiny-help.md", script.folder)

plot.file <- sprintf("%s/plot.slice.R", script.folder)
source(plot.file)


try( setwd( config$workingfolder ), silent=TRUE )



load( config$variablesfile  )

load( config$correlationfile )

load(config$barycenters$euclidean)
barycenter.euclidean = barycenter
try(suppressWarnings( load( config$barycenters$otdiscrete ) ), silent=TRUE )
barycenter.ot = barycenter
    
t.cols = brewer.pal("PiYG", n=11)
t.cols[6] = "#FFFFBF"
t.cols = colorRamp( t.cols )


c.max = 1

var.names <- c()
for(i in 1:length(cor.res) ){
  var.names <- c(var.names, cor.res[[i]]$name )
}
names(cor.res) = var.names
dims = dim(cor.res[[1]]$im[[1]])


background.names <- c( "Average", sprintf("Subject%4d", 1:n) )


# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
  title = "UTM Result Viewer",
  fluidRow(
      column(3, align="center",
          plotOutput( outputId = "axial", height = "300px",
                      click = "axial_click",
                      dblclick = "axial_dblclick",
                      hover = "axial_hover"
                     ),    
          sliderInput( inputId = "axialslice", label = "Axial Slice:",
                                   min = 1, max = dims[3], value = round(dims[3]/2), step = 1,
                                   animate = animationOptions(interval = dims[3]) 
                    )
          ),
      column(3, align="center",
         plotOutput(outputId = "coronal", height = "300px",
                      click = "coronal_click",
                      dblclick = "coronal_dblclick",
                      hover = "coronal_hover"),
         sliderInput( inputId = "coronalslice", label = "Coronal Slice:",
                                   min = 1, max = dims[2], value = round(dims[2]/2), step = 1,
                                   animate = animationOptions(interval = dims[2]) 
                    )
             ),
      column(3, align="center",
         plotOutput(outputId = "sagittal", height = "300px",
                      click = "sagittal_click",
                      dblclick = "sagittal_dblclick",
                      hover = "sagittal_hover"),
         sliderInput( inputId = "sagittalslice", label = "Sagittal Slice:",
                                   min = 1, max = dims[1], value = round(dims[1]/2), step = 1,
                                   animate = animationOptions(interval = dims[1]) 
                    )
         ),
      column(3, align="left",
             br(),
             textOutput(outputId="cor.limit.max"),
             plotOutput(outputId="colorbar", height = "200px", "30px"),
             textOutput(outputId="cor.limit.min"),
             hr(),
             htmlOutput( outputId = "textCor" )
      )
  ),
  hr(),
  tabsetPanel(
  tabPanel("Controls",            
  fluidRow(
      column(3,
         selectInput( inputId = "variable", label="Variable:", choices=names( cor.res ) ),
         bsPopover("variable", "Variable Selection", "Show correlations with respect to selection", "right", options = list(container = "body")),
          selectInput( inputId = "feature", label="Analysis:", choices=names( cor.res[[1]]$im ) ),
          bsPopover( "feature",  "Analysis Type",
            "<b>Intensity:</b> Classical voxel-based morphometry <br> <b>Allocation:</b> Shows correlatins with tissue loss / gain<br> <b>Transport:</b>  Shows correlation with tissue relocations<br>", "right", options = list(container = "body")  ),
          downloadButton('downloadSlices', 'Download Slice Images'),         
          downloadButton('downloadAllSlices', 'Download All Slice Images')
          #downloadButton('downloadData', 'Download Correlation Volumes')
      ),
      column(3,
         sliderInput( inputId = "threshold", label = "P-Value Threshold:",
                                   min = 0.0, max = 0.2, value = 0.05, step = 0.001,
                                   animate = animationOptions(interval = 50) 
                    )
      ),
      column(3,
         sliderInput( inputId = "maxcor", label = "Correlation Window Level:",
                                   min = 0.0, max = 1.0, value = 1.0, step = 0.01,
                                   animate = animationOptions(interval = 100) 
                    ),
         sliderInput( inputId = "transparency", label = "Transparency:",
                                   min = 0.0, max = 1.0, value = 1.0, step = 0.01,
                                   animate = animationOptions(interval = 100) ) 

      ),
      column(3,
         selectInput( inputId = "background", label = "Background Image:",
                     choices = background.names),
          bsPopover( "background",  "Background IOmage",
            "Background to display, either the average or a particular subject.", 
            "left", options = list(container = "body") ),
          hr(),
          htmlOutput( outputId = "variables" )

      )
    )
   ),
   tabPanel("Help",
      includeMarkdown( help.file )
   )
  ) 
 
)




get.background <- function( background.id, feature ){
  if(background.id == "Average" ){
    if(feature == "intensity" ){
      bg <- barycenter.euclidean$image
    }else{
      bg <- barycenter.ot$image
    }
  }else{
    subject.id = as.integer( substr( background.id, nchar(background.id)-3 , nchar(background.id) ) )
    load( sprintf(config$pointsfilepattern, subject.id) )
    bg <- array(0, dim=dims)
    bg[x] = x.weights
  }
  bg
}



print.background.info <- function( background.id ){
  res <- 
      "<table style=\"width:100%\">
  <tr>
    <th>Variable</th>
    <th>Value</th>
  </tr>"

  if(background.id == "Average" ){
    for(i in 1:length(variables) ){
      res <- sprintf("%s <tr><td>%s</td><td> %f</td></tr>", res, variables[[i]]$name, 
                     mean(variables[[i]]$value, na.rm=TRUE) )
    }    
  }else{
    subject.id = as.integer( substr( background.id, nchar(background.id)-3 , nchar(background.id) ) )
    for(i in 1:length(variables) ){
      res <- sprintf("%s <tr><td>%s</td><td> %f</td></tr>", res, variables[[i]]$name, variables[[i]]$value[subject.id] )
    }  
  }
  res <- sprintf("%s</table>", res)
  res
}
  



# Define server function
server <- function(input, output, session) {
  

  observe.variable <- observe({

    cmax = get.max.cor( input$variable )
    updateSliderInput(session, "maxcor", value=cmax) 
  })





  # Create scatterplot object the plotOutput function is expecting
  
  #axial slice
  output$axial <- renderPlot({
    plot.slice( input$variable, input$feature, input$maxcor, 
                input$transparency, input$threshold, input$background, 
                c(input$sagittalslice,  input$coronalslice, input$axialslice) ,3)
  
  })

  observeEvent(input$axial_click, {
     im  <- cor.res[[ input$variable ]]$im[[ input$feature ]]
     d <- dim(im)
     x = as.integer( input$axial_click$x * d[1] )
     y = as.integer( input$axial_click$y * d[2])
     updateSliderInput(session, "sagittalslice", value=x) 
     updateSliderInput(session, "coronalslice", value=y) 
  })



  #coronal slice
  output$coronal <- renderPlot({
    plot.slice( input$variable, input$feature, input$maxcor, 
                input$transparency, input$threshold, input$background, 
                c(input$sagittalslice,  input$coronalslice, input$axialslice), 2)
  })
  
  observeEvent(input$coronal_click, {
     im  <- cor.res[[ input$variable ]]$im[[ input$feature ]]
     d <- dim(im)
     x = as.integer( input$coronal_click$x * d[1] )
     y = as.integer( input$coronal_click$y * d[3])
     updateSliderInput(session, "sagittalslice", value=x) 
     updateSliderInput(session, "axialslice", value=y) 
  })


  #sagittal slice
  output$sagittal <- renderPlot({
    plot.slice( input$variable, input$feature, input$maxcor, 
                input$transparency, input$threshold, input$background, 
                c(input$sagittalslice,  input$coronalslice, input$axialslice), 1)
  })

  observeEvent(input$sagittal_click, {
     im  <- cor.res[[ input$variable ]]$im[[ input$feature ]]
     d <- dim(im)
     x = as.integer( input$sagittal_click$x * d[2] )
     y = as.integer( input$sagittal_click$y * d[3])
     updateSliderInput(session, "coronalslice", value=x) 
     updateSliderInput(session, "axialslice", value=y) 
  })



  #Info / Controls
  output$textCor <- renderUI({
    res <- sprintf( "Correlation    : %.02f", 
              cor.res[[ input$variable ]]$im[[ input$feature ]][
                        input$sagittalslice, input$coronalslice, input$axialslice]
           )   
    res 
  })
  
  output$cor.limit.min <- renderText({
    sprintf( "%.02f", -input$maxcor)
  })
  output$cor.limit.max <- renderText({
    sprintf( "%.02f", input$maxcor)
  })

  output$colorbar <- renderPlot({
    par( mar=c(0,0,0,0))
    t.cols.tmp = rgb( t.cols( seq(0, 1, length.out=99))/256)
    x <- matrix(1:200, nrow=200, ncol=30)
    image( t(x), col=t.cols.tmp, axes=FALSE )
  })


  output$variables <- renderText({
    print.background.info( input$background )
  })
  
  output$downloadSlices <- downloadHandler(

    filename = function() {
            sprintf("%s-%s.zip", input$variable, input$feature)
	  },

    # the argument 'file'.
    content = function(file) {
      folder <- sprintf("%s-%s", input$variable, input$feature) 
      dir.create( folder )
      file1 = sprintf("%s/cor-axial-%s-%s.png", folder, input$variable, input$feature )
      png( file = file1 )
      plot.slice( input$variable, input$feature, input$maxcor, 
                  input$transparency, input$threshold, input$background, 
                  c(input$sagittalslice,  input$coronalslice, input$axialslice), 3, input$anatomy)
      dev.off()

      file2 = sprintf("%s/cor-coronal-%s-%s.png", folder, input$variable, input$feature )
      png( file = file2 )
      plot.slice( input$variable, input$feature, input$maxcor, 
                  input$transparency, input$threshold, input$background, 
                  c(input$sagittalslice,  input$coronalslice, input$axialslice), 2, input$anatomy)
      dev.off()
      
      
      file3 = sprintf("%s/cor-sagittal-%s-%s.png", folder, input$variable, input$feature )
      png( file = file3 )
      plot.slice( input$variable, input$feature, input$maxcor, 
                  input$transparency, input$threshold, input$background, 
                  c(input$sagittalslice,  input$coronalslice, input$axialslice), 1, input$anatomy)
      dev.off()


      zip( file, c(file1, file2, file3) )

      unlink( folder )
    }
  )

  output$downloadAllSlices <- downloadHandler(

    filename = function() {
            sprintf("all-%d-%d-%d.zip", input$sagittalslice, input$coronalslice, input$axialslice)
	  },

    # the argument 'file'.
    content = function(file) {
      folder <- sprintf("all-%d-%d-%d", input$sagittalslice, 
                        input$coronalslice, input$axialslice)      
      dir.create( folder )

      files <- c()

      for(variable in var.names){
        for( feature in names( cor.res[[variable]]$im ) ){
          file1 = sprintf("%s/cor-axial-%s-%s.png", folder, variable, feature )
          png( file = file1 )
          plot.slice( variable, feature, input$maxcor, 
                  input$transparency, input$threshold, input$background, 
                  c(input$sagittalslice,  input$coronalslice, input$axialslice), 3, input$anatomy)
          dev.off()

          file2 = sprintf("%s/cor-coronal-%s-%s.png", folder, variable, feature )
          png( file = file2 )
          plot.slice( variable, feature, input$maxcor, 
                  input$transparency, input$threshold, input$background, 
                  c(input$sagittalslice,  input$coronalslice, input$axialslice), 2, input$anatomy)
          dev.off()
      
      
          file3 = sprintf("%s/cor-sagittal-%s-%s.png", folder, variable, feature )
          png( file = file3 )
          plot.slice( variable, feature, input$maxcor, 
                  input$transparency, input$threshold, input$background, 
                  c(input$sagittalslice,  input$coronalslice, input$axialslice), 1, input$anatomy)
          dev.off()

          files <- c(files, file1, file2, file3 )
        }
      }
      zip( file, files )

      unlink( folder )
    }
  )


}

# Create Shiny object
shinyApp(ui = ui, server = server)

