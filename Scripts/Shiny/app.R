#!/usr/bin/env Rscript


options(rgl.useNULL=TRUE)
options(rgl.inShiny = TRUE)

library(htmlwidgets)
library(rmarkdown)
library(optparse, quietly=TRUE)
library(stringr, quietly=TRUE)
library(shiny, quietly=TRUE)
library(shinyBS, quietly=TRUE)
library(shinyWidgets, quietly=TRUE)
library(shinythemes, quietly=TRUE)
library(DT, quietly=TRUE)
library(corrplot, quietly=TRUE)
library(vtkwidgets, quietly=TRUE)
library(raster, quietly=TRUE)

option_list = list(
make_option("--barycenterfolder", type="character", default="Barycenter",
  help="Subfolder in Analysis for storing barycenters [default=Barycenter]",
  metavar="character"),
make_option(c("-a", "--analysisfolder"), type="character", default="Analysis",
 help="Folder to store results in. [default=Analysis]", metavar="character"),
make_option(c("-b", "--barycentertype"), type="character", default="Euclidean",
  help="Barycenter to display [default = Euclidean]", metavar="character"),
make_option("--workingfolder", type="character", default=".",
  help="Working directory folder [default=.]", metavar="character"),
make_option( "--atlas", type="character", default="atlas.Rdata",
  help="Atlas for displaying anatomical labels and information", metavar="character")
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
#Script folder
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
help.file <- sprintf("%s/shiny-help.md", script.folder)

config = opts$options
config$barycenters = list()
config$barycenters$euclidean = sprintf("./%s/barycenter-euclidean.Rdata",
                                       config$barycenterfolder)
config$barycenters$otdiscrete = sprintf("./%s/barycenter-ot-discrete.Rdata",
                                        config$barycenterfolder)
config$correlationfile = sprintf("./%s/cor-vbm-utm.Rdata", config$analysisfolder)
config$variablesfile = "./variables.Rdata"

try( setwd( config$workingfolder ), silent=TRUE )

render.script = readLines("./render.js")
atlas <- NULL
try( load(config$atlas), silent=TRUE)
load( config$variablesfile  )
load( config$correlationfile )
load( config$barycenters$euclidean )
barycenter.euclidean = barycenter
try(suppressWarnings( load( config$barycenters$otdiscrete ) ), silent=TRUE )
barycenter.ot = barycenter
t.cols = hcl.colors( palette="PiYG", n = 255, alpha=1)
t.cols = colorRamp( t.cols )
var.names <- c()
for(i in 1:length(cor.res) ){
  var.names <- c(var.names, cor.res[[i]]$name )
}
names(cor.res) = var.names
dims = dim(cor.res[[1]]$im[[1]])
background.names <- c( "Average", "Atlas") #, sprintf("Subject%4d", 1:n) )

# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
 navbarPage("VolPA - Volumetric Population Analysis",
  tabPanel("Explore",
   fluidRow(
    column(3, align="left", offset = 0,
     wellPanel(
      selectInput( inputId = "variable", label="Variable:", choices=names( cor.res ) ),
      bsPopover("variable", "Variable Selection", "Show correlations with respect to selection",
                "right", options = list(container = "body")),
      selectInput( inputId = "feature", label="Analysis:", choices=names( cor.res[[1]]$im ) ),
      bsPopover("feature",  "Analysis Type",
                "<b>Intensity:</b> Classical voxel-based morphometry <br>
                 <b>Allocation:</b> Shows correlatins with tissue loss / gain<br>
                 <b>Transport:</b>  Shows correlation with tissue relocations<br>",
                "right", options = list(container = "body")  ),
      shinyWidgets::sliderTextInput( inputId = "threshold", label = "P-Value Threshold:",
                      choices=formatC(c(0,rev(c(rbind(5*10^-(1:8), 10^-(1:8))))), digits=1, format="e"),
                      selected = formatC(0.05,digits=1, format="e"), grid=TRUE),
      sliderInput(inputId = "maxcor", label = "Correlation Window Level:",
                  min = 0.0, max = 1.0, value = 1.0, step = 0.01, ticks=FALSE),
      sliderInput(inputId = "transparency", label = "Transparency:",
                  min = 0.0, max = 1.0, value = 1.0, step = 0.01 ),
      selectInput(inputId = "background", label = "Background Image:",
                  choices = background.names),
      bsPopover("background",  "Background Image",
                "Background to display, either the average or the atlas if available.",
                "left", options = list(container = "body") ),
      br(),
      bsCollapse(
       bsCollapsePanel("3D View Settings",
        checkboxInput("renderBackground", "Show 3D Background"),
        sliderInput("backgroundIsovalue", "Isovalue", min=0, max=1, value=0.5),
        sliderInput("backgroundTransparency", "Transparency", min=0, max=1, value=0.1),
        checkboxInput("backgroundSolid", "Solid"),
        checkboxInput("renderSliceZ", "Show Axial Slice"),
        checkboxInput("renderSliceY", "Show Coronal Slice"),
        checkboxInput("renderSliceX", "Show Saggital Slice")
       )
      ),
      hr(),
      downloadButton('downloadSlices', 'Download Slice Images'),
      downloadButton('downloadAllSlices', 'Download All Slice Images')
     )
    ),
    column(9, align="left", offset = 0,
     tabsetPanel(type="pills",
      tabPanel("Slice View",
       fluidRow(
        column(3, align="center", offset = 0, style='padding:2px;',
         plotOutput(outputId = "axial", width = "300px", click = "axial_click",
                    dblclick = "axial_dblclick", hover = "axial_hover" )
        ),
        column(3, align="center", offset = 0, style='padding:2px;',
         plotOutput(outputId = "coronal", width = "300px", click = "coronal_click",
                    dblclick = "coronal_dblclick", hover = "coronal_hover")
        ),
        column(3, align="center", offset = 0, style='padding:2px;',
         plotOutput(outputId = "sagittal", width = "300px",  click = clickOpts(id="sagittal_click"),
                    dblclick = "sagittal_dblclick", hover = "sagittal_hover")
        ),
        column(3, align="left",
         textOutput(outputId="cor.limit.max"),
         plotOutput(outputId="colorbar", height = "200px", "30px"),
         textOutput(outputId="cor.limit.min"),
         br(),
         htmlOutput( outputId = "textCor" ),
         hr(),
         radioButtons( "anatomy" , "Anatomy", c("All"), inline = FALSE)
        )
       )
      ),
      tabPanel("3d View",
       fluidRow(
        column(9, align="left", offset = 0,
         vtkWidgetOutput("volume1", height = "500px", width="auto")
        )
       )
      ),
      tabPanel("Variables",
       fluidRow(
        column(3, align="left",
         DTOutput( outputId = "variables" )
        ),
        column(5, align="center",
         plotOutput( outputId="variables.cor", height = "500px",)
        )
       )
      )
     ),
     fluidRow(
      column(3, align="center", offset = 0, style='padding:2px;',
       sliderInput(inputId = "axialslice", label = "Axial Slice:", min = 1, max = dims[3],
                   value = round(dims[3]/2), step = 1)
      ),
      column(3, align="center", offset = 0, style='padding:2px;',
       sliderInput(inputId = "coronalslice", label = "Coronal Slice:", min = 1, max = dims[2],
                   value = round(dims[2]/2), step = 1)
      ),
      column(3, align="center", offset = 0, style='padding:2px;',
       sliderInput(inputId = "sagittalslice", label = "Sagittal Slice:",min = 1,
                   max = dims[1], value = round(dims[1]/2), step = 1)
      )
     )
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
  }else if(background.id == "Atlas" ){
    if( !is.null(atlas) ){
      bg <-  atlas$bg.image
    }else{
      if(feature == "intensity" ){
        bg <- barycenter.euclidean$image
      }  else{
        bg <- barycenter.ot$image
      }
    }
  }else {
    #subject.id = as.integer( substr( background.id, nchar(background.id)-3 , nchar(background.id) ) )
    #load( sprintf(config$pointsfilepattern, subject.id) )
    #bg <- array(0, dim=dims)
    #bg[x] = x.weights
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

  if(background.id == "Average"  | background.id == "Atlas" ){
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


get.atlas.label <- function(slices ){
  index = NA
  if( !is.null(atlas) ){
    im  <- cor.res[[1]]$im[[1]]
    da <- dim(atlas$label.image)
    di <- dim(im)
    index <- atlas$label.image[ as.integer( slices[1] / di[1]*da[1] ),
                               as.integer( slices[2] / di[2]*da[2] ),
                               as.integer( slices[3] / di[3]*da[3] ) ]

  }
  index
}


get.max.cor <- function(variable){
  cmax = 0
  features <- cor.res[[ variable ]]$im
  mask <- barycenter.euclidean$orig.image > 0
  for(i in 1:length(features ) ){
    cmax = max( cmax, max(abs(features[[i]][mask]) ) )
  }
  cmax

}


plot.slice <- function(variable, feature, maxcor, transparency,
                       threshold, background, slice, axis, anatomy="All"){

  useAtlas <- anatomy != "All" & !is.null(atlas)

  im  <- cor.res[[ variable ]]$im[[ feature ]]
  pim <- cor.res[[ variable ]]$pim[[ feature  ]]

  bg <- get.background( background, feature )
  bg <- bg / max(bg)
  rbg <- range(bg)
  db <- dim(bg)
  d <- dim(im)
  if(useAtlas){
    bg.atlas <- atlas$label.image
    da <- dim(bg.atlas)
  }

  vline = 0
  hline = 0
  if(axis == 3){
    tim <- im[,,slice[3]]
    pim <- pim[,,slice[3]]
    bg <- bg[,, slice[3] / d[3] * db[3] ]
    mask <- barycenter.euclidean$orig.image[,,slice[3]] > 0
    if(useAtlas){
      bg.atlas <- bg.atlas[,, slice[3] / d[3] * da[3] ]
    }
    dm =max(d[c(1,2)])
    vline = (slice[1] - 0.5) / d[1]
    hline = (slice[2] - 0.5) / d[2]
  } else if(axis == 2){
    tim <- im[,slice[2],]
    pim <- pim[,slice[2],]
    bg <- bg[,slice[2] / d[2] * db[2],]
    mask <- barycenter.euclidean$orig.image[,slice[2],] > 0
    if(useAtlas){
      bg.atlas <- bg.atlas[,slice[2] / d[2] * da[2], ]
    }
    dm =max(d[c(1,3)])
    vline = (slice[1] - 0.5) / d[1]
    hline = (slice[3] - 0.5) / d[3]
  } else{
    tim <- im[slice[1],,]
    pim <- pim[slice[1],,]
    bg <- bg[slice[1]/ d[1] * db[1],,]
    mask <- barycenter.euclidean$orig.image[slice[1],,] > 0
    if(useAtlas){
      bg.atlas <- bg.atlas[slice[1] / d[1] * da[1],, ]
    }
    dm =max(d[c(2,3)])
    vline = (slice[2] - 0.5) / d[2]
    hline = (slice[3] - 0.5) / d[3]
  }
  tim = tim * mask

  par(mar=c(0,0,0,0))


  c.max <- maxcor
  tim[tim > c.max ] = c.max
  tim[tim < -c.max ] = -c.max
  t.cols.tmp = rgb( t.cols( seq(0, 1, length.out=99))/256, alpha=transparency )
  t.cols.tmp[50] = "#00000000"

  t.index <- which( pim <= (1-threshold) )
  tim[ t.index ] <- NA

  #grid.raster(bg, vp=viewport(angle=90, clip="on") )
  db = dim(bg)
  image( bg, col=gray.colors(100), axes=FALSE, zlim=rbg, asp=db[2]/db[1],
         useRaster=T)
  di = dim(tim)
  image( tim , col=t.cols.tmp, add=TRUE, zlim=c(-c.max, c.max),
         axes=FALSE, asp=di[2]/di[1], useRaster=T)

  if( useAtlas ){
    highlight = bg.atlas
    highlight[] = NA
    ls <- c()
    for( i in 1:ncol(atlas$labels) ){
      ls <- append(ls, which( atlas$labels[,i] == anatomy ))
    }
    index <- which( is.element(bg.atlas, ls) )
    highlight[ -index ] = 1
    dh = dim(highlight)
    image(  highlight , col="#000000AA", add=TRUE, zlim = c(0,1), asp=dh[2]/dh[1], useRaster=T)
  }

  abline(v=vline, col="#0085B8")
  abline(h=hline, col="#0085B8")
  invisible()
}

# Define server function
server <- function(input, output, session) {

  session$userData$background = ""
  session$userData$feature = ""
  session$userData$variable = ""
  session$userData$p.value = ""

  #labelIndex  <-reactiveVal( 1 )
  observe.atlas <- observe({
    im  <- cor.res[[ 1 ]]$im[[ 1]]
    da <- dim(atlas)
    di <- dim(im)
    index <- get.atlas.label( c(input$sagittalslice,
                                input$coronalslice,
                                input$axialslice) )
    if( !is.na(index) ){
      choices = as.vector( c("All", unlist( as.character(atlas$labels[index, ] )))  )
      updateRadioButtons( session, "anatomy", choices=choices, inline=FALSE)
    }
  })


  observe.variable <- observe({
    cmax = signif(get.max.cor( input$variable ),3)
    updateSliderInput(session, "maxcor", value=cmax, step=cmax/100)
  })


  observeEvent(input$button3d, {
                 create.3d(input$variable, input$feature, input$threshold)
  })


  # Create scatterplot object the plotOutput function is expecting
  #axial slice
  output$axial <- renderPlot({
    plot.slice( input$variable, input$feature, input$maxcor,
               input$transparency, input$threshold, input$background,
               c(input$sagittalslice,  input$coronalslice, input$axialslice) ,3, input$anatomy)

  })


  observeEvent(input$axial_click, {
                 im  <- cor.res[[ input$variable ]]$im[[ input$feature ]]
                 d <- dim(im)
                 x = as.integer( input$axial_click$x * d[1] )
                 y = as.integer( input$axial_click$y * d[2])
                 updateSliderInput(session, "sagittalslice", value=x)
                 updateSliderInput(session, "coronalslice", value=y)
  })


  #  observeEvent(input$axial_hover, {
  #    im  <- cor.res[[ input$variable ]]$im[[ input$feature ]]
  #    da <- dim(atlas)
  #    di <- dim(im)
  #    x = as.integer( input$axial_hover$x * da[1] )
  #    y = as.integer( input$axial_hover$y * da[2])
  #    labelIndex( atlas[x,y, as.integer( input$axialslice / di[3]*da[3] ) ] )
  # })


  #coronal slice
  output$coronal <- renderPlot({
    plot.slice( input$variable, input$feature, input$maxcor,
               input$transparency, input$threshold, input$background,
               c(input$sagittalslice,  input$coronalslice, input$axialslice), 2, input$anatomy)
  })


  observeEvent( input$coronal_click, {
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
               c(input$sagittalslice,  input$coronalslice, input$axialslice), 1, input$anatomy)
  })


  observeEvent( input$sagittal_click, {
                 im  <- cor.res[[ input$variable ]]$im[[ input$feature ]]
                 d <- dim(im)
                 dm <- max(d[2:3])
                 #print( sprintf("x: %.3f / y: %.3f", input$sagittal_click$x, input$sagittal_click$y ) )
                 x = as.integer( input$sagittal_click$x * d[2] )
                 y = as.integer( input$sagittal_click$y * d[3] )
                 #print( sprintf("x: %.3f / y: %.3f", x, y) )
                 updateSliderInput(session, "coronalslice", value=x)
                 updateSliderInput(session, "axialslice", value=y)
  })

  output$volume1 <- renderVtkWidget({
    withProgress(message = 'Loading Volume', value = 0, {
    incProgress(0.1, detail = "Prepare Volume")

    bg.image <- get.background(input$background, input$feature)
    dim.im = dim(cor.res[[ input$variable ]]$im[[ input$feature ]])
    dim.bg = dim(bg.image)
    spacing = dim.im / dim.bg
    im <- NULL
    if( session$userData$feature != input$feature ||
        session$userData$variable != input$variable ||
        session$userData$p.value != input$threshold
      ){
      session$userData$feature = input$feature
      session$userData$variable = input$variable
      session$userData$p.value = input$threshold
      im  <- cor.res[[ input$variable ]]$im[[ input$feature ]]
      #make sure transfer function is scaled correctly
      im[is.na(im)] = 0
      vmax = max(abs(im))
      pim <- cor.res[[ input$variable ]]$pim[[ input$feature  ]]
      t.index <- which( pim <= (1-input$threshold) )
      im[ t.index ] <- 0
      im[1] = -vmax
      im[2] = vmax
      im = vtkImageData(im)
    }
    background = NULL
    showBackground = input$renderBackground ||
                     input$renderSliceX ||
                     input$renderSliceY ||
                     input$renderSliceZ
    if( session$userData$background != input$background && showBackground  ){
      session$userData$background = input$background
      background = vtkImageData(bg.image, spacing=spacing)
    }
    slices = c(input$sagittalslice, input$coronalslice, input$axialslice)
    slcies = slices/spacing
    incProgress(0.2, detail = "Uploading")
    vtkWidget(data=list(image = im,
                        background=background,
                        isovalue=max(bg.image)*input$backgroundIsovalue,
                        solid = input$backgroundSolid,
                        opacity = input$backgroundTransparency,
                        showBackground=input$renderBackground,
                        sliceX = slices[1],
                        sliceY = slices[2],
                        sliceZ = slices[3],
                        showSliceX = input$renderSliceX,
                        showSliceY = input$renderSliceY,
                        showSliceZ = input$renderSliceZ
                        ),
              render=render.script)
    })
  })

  output$textCor <- renderUI({
    c.value = cor.res[[ input$variable ]]$im[[ input$feature ]][input$sagittalslice, input$coronalslice, input$axialslice]
    p.value = 1-cor.res[[ input$variable ]]$pim[[ input$feature ]][input$sagittalslice, input$coronalslice, input$axialslice]
    HTML(sprintf( "Cor: %.02f <br> p: %.02e", c.value, p.value))
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

  output$variables <- renderDT({
    df = data.frame(mean = rep(NA, length(var.names)),
                    sdev = rep(NA, length(var.names)) )
    rownames(df) <- var.names
    for(i in 1:length(variables) ){
      df$mean[i] = mean(variables[[i]]$value, na.rm=TRUE)
      df$sdev[i] = sd(variables[[i]]$value, na.rm=TRUE)
    }
    datatable(df) %>% formatRound(1:2, 3)
  })

  output$variables.cor <-  renderPlot({
    vars = matrix(NA, ncol=length(variables), nrow=length(variables[[1]]$value))
    for(i in 1:length(variables) ){
      vars[,i] = variables[[i]]$value
    }
    M <- cor(vars, use="pairwise.complete.obs")
    rownames(M) <- var.names
    colnames(M) <- var.names
    corrplot(M, method="circle", type="upper", tl.pos = "dl")
  })

  output$downloadSlices <- downloadHandler(
     filename = function() {
       sprintf("%s-%s.zip", input$variable, input$feature)
     },
     content = function(file) {
       folder <- sprintf("%s-%s", input$variable, input$feature)
       dir.create( folder )
       file1 = sprintf("%s/cor-axial-%s-%s.png", folder, input$variable, input$feature )
       png( file = file1 )
       plot.slice( input$variable, input$feature, input$maxcor,
                   input$transparency, input$threshold, input$background,
                   c(input$sagittalslice,  input$coronalslice, input$axialslice),
                   3, input$anatomy)
       dev.off()
       file2 = sprintf("%s/cor-coronal-%s-%s.png", folder, input$variable, input$feature )
       png( file = file2 )
       plot.slice(input$variable, input$feature, input$maxcor,
                  input$transparency, input$threshold, input$background,
                  c(input$sagittalslice,  input$coronalslice, input$axialslice),
                  2, input$anatomy)
       dev.off()


       file3 = sprintf("%s/cor-sagittal-%s-%s.png", folder, input$variable, input$feature )
       png( file = file3 )
       plot.slice(input$variable, input$feature, input$maxcor,
                  input$transparency, input$threshold, input$background,
                  c(input$sagittalslice,  input$coronalslice, input$axialslice),
                  1, input$anatomy)
       dev.off()
       zip( file, c(file1, file2, file3) )
       unlink( folder )
     }
  )

  output$downloadAllSlices <- downloadHandler(
    filename = function() {
      sprintf("all-%d-%d-%d.zip", input$sagittalslice, input$coronalslice, input$axialslice)
    },
    content = function(file) {
      folder <- sprintf("all-%d-%d-%d", input$sagittalslice,
      input$coronalslice, input$axialslice)
      dir.create( folder )
      files <- c()
      cmax <- input$maxcor
      for(variable in var.names){
        for( feature in names( cor.res[[variable]]$im ) ){
          file1 = sprintf("%s/cor-axial-%s-%s.png", folder, variable, feature )
          png( file = file1 )
          plot.slice(variable, feature, cmax, input$transparency,
                     input$threshold, input$background,
                     c(input$sagittalslice,  input$coronalslice, input$axialslice),
                     3, input$anatomy)
          dev.off()
          file2 = sprintf("%s/cor-coronal-%s-%s.png", folder, variable, feature )
          png( file = file2 )
          plot.slice( variable, feature, cmax,
                      input$transparency, input$threshold, input$background,
                      c(input$sagittalslice,  input$coronalslice, input$axialslice),
                      2, input$anatomy)
          dev.off()
          file3 = sprintf("%s/cor-sagittal-%s-%s.png", folder, variable, feature )
          png( file = file3 )
          plot.slice( variable, feature, cmax,
                      input$transparency, input$threshold, input$background,
                      c(input$sagittalslice,  input$coronalslice, input$axialslice),
                      1, input$anatomy)
          dev.off()
          files <- c(files, file1, file2, file3 )
        }
        file1 <- sprintf("%s/%s-cmax.Rdata", folder, variable )
        save(cmax, file=file1)
        file2 <- sprintf("%s/%s-colorbar.png", folder, variable )
        png(file=file2, width=30, height=200)
        par( mar=c(0,0,0,0))
             t.cols.tmp = rgb( t.cols( seq(0, 1, length.out=99))/256)
             x <- matrix(1:200, nrow=200, ncol=30)
        image( t(x), col=t.cols.tmp, axes=FALSE )
        dev.off()
        files =  c(files, file1, file2)
      }
      zip( file, files )
      unlink( folder, recursive=TRUE )
    }
  )

}

# Create Shiny object
shinyApp(ui = ui, server = server)

