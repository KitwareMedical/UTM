#!/usr/bin/env Rscript


options(rgl.useNULL=TRUE)
options(rgl.inShiny = TRUE)

library(rmarkdown)
library(rgl)
library(misc3d)
library(optparse, quietly=TRUE)
library(stringr, quietly=TRUE)
library(shiny, quietly=TRUE)
library(shinyBS, quietly=TRUE)
library(shinythemes, quietly=TRUE)
library(RColorBrewer, quietly=TRUE)
library(mmand)
library(glmnet)





option_list = list(  
  make_option("--barycenterfolder", type="character", default="Barycenter", 
              help="Subfolder in Analysis for storing barycenters [default=Barycenter]", metavar="character"),
  make_option(c("-a", "--analysisfolder"), type="character", default="Analysis", 
              help="Folder to store results in. [default=Analysis]", metavar="character"),
  make_option(c("-b", "--barycentertype"), type="character", default="Euclidean", 
              help="Barycenter to display [default = Euclidean]", metavar="character"),
  make_option("--workingfolder", type="character", default=".", 
              help="Working directory folder [default=.]", metavar="character"),
  make_option( "--orientation", type="character", default="xyz", 
               help="Permutation of orientation of images", metavar="character"), 
  make_option( "--atlas", type="character", default="atlas.Rdata", 
               help="Atlas for displaying anatomical labels and information", metavar="character"),
  make_option( "--flipX", help="Flip x axis", action="store_false", default=TRUE ),
  make_option( "--flipY", help="Flip y axis", action="store_true", default=FALSE ),
  make_option( "--flipZ", help="Flip z axis", action="store_true", default=FALSE ),
  make_option( "--pcs.reg", type="character", default="VBM-pcs-reg.Rdata", 
               help="Regularized COmponets data file", metavar="character"), 
  make_option( "--pcs", type="character", default="VBM-pcs.Rdata", 
               help="Principal compnents data file", metavar="character") 

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

config$pcs.file = sprintf("./%s/Images/%s", config$analysisfolder, config$pcs ) 
config$pcs.reg.file = sprintf("./%s/Images/%s", config$analysisfolder, config$pcs.reg ) 

config$variablesfile = "./variables.Rdata"

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


try( setwd( config$workingfolder ), silent=TRUE )

#load atlas
atlas <- NULL
try( load(config$atlas), silent=TRUE)
 
load( config$variablesfile  )

load( config$pcs.file )
load( config$pcs.reg.file )

load( config$barycenters$euclidean )
barycenter.euclidean = barycenter
try(suppressWarnings( load( config$barycenters$otdiscrete ) ), silent=TRUE )
barycenter.ot = barycenter
    
t.cols = hcl.colors( palette="PuOr", n = 255, alpha=1)
t.cols = colorRamp( t.cols )

olist = strsplit(config$orientation, "")[[1]]
permutation = c( which( olist == 'x' ),
                 which( olist == 'y' ),
                 which( olist == 'z') )

var.names <- c()
for(i in 1:length(variables) ){
  var.names <- c(var.names, variables[[i]]$name )
}
dims = dim(barycenter.euclidean$image)


background.names <- c( "Average", "Atlas") #, sprintf("Subject%4d", 1:n) )


reorient <- function(im, fX, fY, fZ, p){
  d = dim(im)
  
  x = 1:d[1]
  if(fX){
    x = rev(x)
  }

  y = 1:d[2]
  if(fY){
    y = rev(y)
  } 

  z = 1:d[3]
  if(fZ){
    z = rev(z)
  } 
  im = im[x,y,z]
  im = aperm(im, p)
}

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
  title = "UTM Result Viewer",
  tabsetPanel(
  tabPanel("Slice View", 
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
             textOutput(outputId="max.value"),
             plotOutput(outputId="colorbar", height = "200px", "30px"),
             textOutput(outputId="min.value")
       )
    ),
    fluidRow(
      column(12, align="left",
        radioButtons( "anatomy" , "Anatomy", c("All", "", "", "", "", ""), inline = TRUE)
      )
    )
  ),
  tabPanel("3d View",
        rglwidgetOutput("plot3d", height = "600px", width="auto")
  ),
  tabPanel("Model Summary",
    fluidRow(
      column(8, align="left",
        verbatimTextOutput( outputId = "model.print" )
      ),
      column(4, align="right",
        plotOutput( outputId = "model.plot" )
      )
    )
  )
  ),
  hr(),
  tabsetPanel(
  tabPanel("Controls",            
    fluidRow(
      column(3,
         selectInput( inputId = "feature", label="Feature:", choices=pcs$names ),
         selectInput( inputId = "component", label="Components:", choices=c("PC", "SpatPC" )),
         selectInput( inputId = "variable", label="Variable:", choices=var.names ),
         bsPopover("variable", "Variable Selection", "Show correlations with respect to selection", "right", options = list(container = "body"))
      ),
      column(3,
         selectInput( inputId = "model", label="Model:", choices=c("linear", "elastic", 
                                                                   sprintf("component-%02i", 1:nrow(reg.pcs$components[[1]]) ) )),
         sliderInput( inputId = "ncomps", label = "# Components:",
                      min = 2, 
                      max = min(nrow(pcs$components[[1]]),nrow(reg.pcs$components[[1]]) ), 
                      value = min(nrow(pcs$components[[1]]),nrow(reg.pcs$components[[1]]) ), step = 1),
         selectInput( inputId = "family", label="Family:", choices=c("gaussian", "binomial")),
         sliderInput( inputId = "alpha", label = "Alpha:",
                      min = 0.0, max = 1, value = 0.0, step = 0.001
                    ),
         sliderInput( inputId = "lambda", label = "lambda:",
                      min = 1, max = 100, value = 1, step = 1),
         checkboxGroupInput(inputId = "control", label="Control for:", choices=var.names)  
      ),
      column(3,
         sliderInput( inputId = "transparency", label = "Transparency:",
                                   min = 0.0, max = 1.0, value = 1.0, step = 0.01,
                                   animate = animationOptions(interval = 100) ),
         sliderInput( inputId = "threshold", label = "Threshold:",
                                   min = 0.0, max = 0.2, value = 0.0, step = 0.001,
                                   animate = animationOptions(interval = 50) 
                    ), 
         checkboxInput(inputId = "variance", label="Multipluy by Variance"),
         downloadButton('downloadSlices', 'Download Slice Images')
      ),
      column(3,
         selectInput( inputId = "background", label = "Background Image:",
                     choices = background.names),
          bsPopover( "background",  "Background Image",
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
    if(feature == "vbm" ){
      bg <- barycenter.euclidean$image
    }else{
      bg <- barycenter.ot$image
    }
  }else if(background.id == "Atlas" ){
    if( !is.null(atlas) ){
      bg <-  atlas
    }else{
      if(feature == "vbm" ){
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
  





get.atlas.label <- function(im, slices ){
   index = NA
   if( !is.null(atlas) ){
     da <- dim(atlas)
     di <- dim(im)
     index <- atlas[ as.integer( slices[1] / di[1]*da[1] ),
                   as.integer( slices[2] / di[2]*da[2] ), 
                   as.integer( slices[3] / di[3]*da[3] ) ] 

   }
   index
}

plot.slice <- function(im, feature, transparency, threshold, background, slice, axis, anatomy="All", show.crosshair=TRUE){

    useAtlas <- anatomy != "All" & !is.null(atlas)
    
    bg <- get.background( background, feature )
    rbg <- range(bg) 
    db <- dim(bg)
    d <- dim(im)
    if(useAtlas){
      bg.atlas <- get.background( "Atlas", feature )
      da <- dim(atlas)
    }
    
    c.max = max( abs(im), na.rm=T )
    if( is.infinite( c.max ) ){
      c.max = 0
    } 
    vline = 0 
    hline = 0
    if(axis == 3){
      tim <- im[,,slice[3]]
      bg <- bg[,, slice[3]]
      mask <- barycenter.euclidean$orig.image[,,slice[3]] > 0
      if(useAtlas){
        bg.atlas <- bg.atlas[,, slice[3] / d[3] * da[3] ]
      }
      vline = (slice[1] + 0.5) / d[1]
      hline = (slice[2] + 0.5) / d[2]
    } else if(axis == 2){
      tim <- im[,slice[2],]
      bg <- bg[,slice[2],]
      mask <- barycenter.euclidean$orig.image[,slice[2],] > 0
      if(useAtlas){
        bg.atlas <- bg.atlas[,slice[2] / d[2] * da[2], ]
      }
      vline = (slice[1] + 0.5) / d[1]
      hline = (slice[3] + 0.5) / d[3]
    } else{
      tim <- im[slice[1],,]
      bg <- bg[slice[1],,]
      if(useAtlas){
        bg.atlas <- bg.atlas[slice[1] / d[1] * da[1],, ]
      }
      mask <- barycenter.euclidean$orig.image[slice[1],,] > 0
      vline = (slice[2] + 0.5) / d[2]
      hline = (slice[3] + 0.5) / d[3]
    }
    if( background != "Atlas" ){
      bg <- 0.9*bg/max(bg) + 0.1*(mask)
    }
    #tim = tim * mask
    t.index <- which( abs( tim ) <= threshold )
    tim[ t.index ] <- NA 
    #tim[ bg == 0] <- NA

    par( mar=c(0,0,0,0))

    t.cols.tmp = rgb( t.cols( seq(0, 1, length.out=99))/256, alpha=transparency )
    #t.cols.tmp[50] = "#00000000"
   
    image( bg, col=gray.colors(100), axes=FALSE, zlim=rbg, asp=1  )
    image( tim, col=t.cols.tmp, add=TRUE, zlim=c(-c.max, c.max),  axes=FALSE, asp =1, useRaster=TRUE)

    if( useAtlas ){
      highlight = bg.atlas
      highlight[] = NA
      ls = which( labels$level1 == anatomy |  
                  labels$level2 == anatomy |
                  labels$level3 == anatomy |
                  labels$level4 == anatomy |
                  labels$level5 == anatomy 
                ) 
      index <- which( is.element(bg.atlas, ls) )
      highlight[ -index ] = 1 
      image(highlight, col="#000000AA", add=TRUE, zlim = c(0,1), useRaster=TRUE)
    }

    if(show.crosshair){
      abline(v=vline, col="#0085B8")
      abline(h=hline, col="#0085B8")
    }
    invisible()
}





plot.3d <- function(im,  feature, threshold, background="Average"){
  im  <-  rescale( im, 0.5, "triangle")
  bg <- rescale( get.background( background, feature ), 0.5, "triangle")

  rbg <- range(bg) 
  db <- dim(bg)
  d <- dim(im)

  contour3d(bg, level = min(bg[bg>0]), alpha=0.1, draw=TRUE)
 
  try( contour3d(  im, level = threshold, 
             draw=TRUE, color=rgb(t.cols(1)/255), alpha=1, add=TRUE ), silent=FALSE )
  try( contour3d( -im, level = threshold, 
             draw=TRUE, color=rgb(t.cols(0)/255), alpha=1, add=TRUE ) , silent=FALSE )

}


# Define server function
server <- function(input, output, session) {
  
  fitted  <-reactive({
    correlations = c()
    f.index = which( pcs$names == input$feature )
    v.index = which(var.names == input$variable )
    var <- variables[[v.index]] 
    if(input$component == "PC"){
      df <-data.frame( y=var$values, x=scale( t(pcs$projections.smoothed[[f.index]]) )[, 1:input$ncomps] )
      nx <- ncol(df)
      comps = pcs$components[[f.index]][1:input$ncomps, ]
    }
    else{
      df <-data.frame( y=var$values, x=scale( t(reg.pcs$projections[[f.index]]))[, 1:input$ncomps]  )
      nx <- ncol(df)
      comps = reg.pcs$components[[f.index]][1:input$ncomps, ]
    }
    if(input$family == "binomial"){
       df$y = as.factor(df$y)
    }
    for( cvar in input$control ){
       index = which( cvar == var.names)
       df[ variables[[index]]$name ] = variables[[index]]$values
    }

    if(input$model == "linear"){
      l <- glm( y~., df, family=input$family )
      sl <- summary(l)
      coeff <- sl$coefficients[2:nx, 1]
      zero <- sl$coefficients[2:nx, 4] > 0.05
      #coeff[zero] = 0
      model = l
      g.dir= coeff / sqrt(sum(coeff^2)) 
      image = array(t(comps) %*% g.dir, dim= pcs$dimension[[f.index]] )
      effect=sum(g.dir*coeff)
    }
    else if(input$model=="elastic"){

      measure = "default"
      if(input$family == "binomial"){
         measure = "class"
      }
      index <- complete.cases(df)
      gl <- cv.glmnet( y=df$y[index], x=as.matrix(df[index, -1]), 
                       alpha=input$alpha, nfolds=10, family=input$family, 
                       type.measure=measure  )
      ind = input$lambda
      if(ind > length(gl$lambda) ){
        ind = length(gl$lambda)
      }
      coeff = gl$glmnet.fit$beta[1:(nx-1),ind]     
      model = gl 
      g.dir= coeff / sqrt(sum(coeff^2)) 
      image = array(t(comps) %*% g.dir, dim= pcs$dimension[[f.index]] )
      effect=sum(g.dir*coeff)
    } 
    else{
      comp.id = as.integer(str_sub(input$model, str_length(input$model)-1) )
      image = array(comps[comp.id, ], dim= pcs$dimension[[f.index]] )
      correlations = cor(df[, 2:ncol(df)], as.numeric(df$y))
      if(ncol(df) == nx){
        df = df[ ,c(1, comp.id+1)]
      }
      else{
        df = df[ ,c(1, comp.id+1, (nx+1):(ncol(df)) )]
      }
      l <- glm( y ~., df, family=input$family )
      sl <- summary(l)
      model = l 
      effect = sl$coefficients[2, 1]    
    }
    if( input$variance & !is.null(pcs$variances) ){
      image = image * array(pcs$variances[[f.index]], dim= pcs$dimension[[f.index]])
    } 
    image = reorient(image, config$flipX, config$flipY, config$flipZ, permutation)
    list( image=image, model=model, effect=effect, correlations=correlations)
  })




  observe.fitted <- observe({
    i.max = max(abs(fitted()$image))
    updateSliderInput(session, "threshold", max=i.max, step=i.max/100 ) 
  })


  observe.atlas <- observe({
     index <- get.atlas.label( fitted()$image, c(input$sagittalslice,  input$coronalslice, input$axialslice) )
     if( !is.na(index) ){
       choices = as.vector( c("All", unlist( labels[index, ] ))  )
       updateRadioButtons( session, "anatomy", choices=choices, inline=TRUE)
     }
  })



  

  #axial slice
  output$axial <- renderPlot({
    plot.slice( fitted()$image, input$feature, 
                input$transparency, input$threshold, input$background, 
                c(input$sagittalslice,  input$coronalslice, input$axialslice) ,3, input$anatomy)
  
  })

  observeEvent(input$axial_click, {
     d <- dim( fitted()$image )
     x = as.integer( input$axial_click$x * d[1] )
     y = as.integer( input$axial_click$y * d[2])
     updateSliderInput(session, "sagittalslice", value=x) 
     updateSliderInput(session, "coronalslice", value=y) 
  })



  #coronal slice
  output$coronal <- renderPlot({
    plot.slice( fitted()$image, input$feature, 
                input$transparency, input$threshold, input$background, 
                c(input$sagittalslice,  input$coronalslice, input$axialslice), 2, input$anatomy)
  })
  
  observeEvent(input$coronal_click, {
     d <- dim( fitted()$image )
     x = as.integer( input$coronal_click$x * d[1] )
     y = as.integer( input$coronal_click$y * d[3])
     updateSliderInput(session, "sagittalslice", value=x) 
     updateSliderInput(session, "axialslice", value=y) 
  })


  #sagittal slice
  output$sagittal <- renderPlot({
    plot.slice( fitted()$image, input$feature, 
                input$transparency, input$threshold, input$background, 
                c(input$sagittalslice,  input$coronalslice, input$axialslice), 1, input$anatomy)
  })

  observeEvent(input$sagittal_click, {
     d <- dim( fitted()$image )
     x = as.integer( input$sagittal_click$x * d[2] )
     y = as.integer( input$sagittal_click$y * d[3])
     updateSliderInput(session, "coronalslice", value=x) 
     updateSliderInput(session, "axialslice", value=y) 
  })


  output$plot3d <- renderRglwidget({
   withProgress(message = 'Calculating 3D model',
                 detail = 'This may take a while...', value = 0, {
    incProgress(1/4)
    try(rgl.close(), silent = TRUE)
    incProgress(1/4)
    plot.3d( fitted()$image, input$feature, input$threshold)
    incProgress(1/4)
    widget <- rglwidget()
    })
    widget
  })


  #Info / Controls
  output$model.print <- renderPrint({
    model = fitted()$model
    print("NNZ: ")
    print( sum( abs(fitted()$image)  > input$threshold ) )
    print( sum( prod(dim(fitted()$image) ) ) )
    if(class(model)[[1]] == "glm"){
      print( summary(model) )
      if( is.matrix(fitted()$correlations) ){
        print("Correlations: ")
        print( t(fitted()$correlations) )
      }
    }
    else if( class(model)[[1]] == "cv.glmnet" ){
      #ind <- which(model$lambda.1se == model$lambda)
      ind = input$lambda
      if(ind > length(model$lambda) ){
        ind = length(model$lambda)
      }
      print( "Elasticnet" )
      print( sprintf("CV: %.4f",  model$cvm[ind] ) )
      print( sprintf("Dev Ratio: %.4f",  model$glmnet.fit$dev.ratio[ind] ) )
      print( sprintf("Lambda: %.4f", model$lambda[ind]) )
      print("Coefficients")
      print( model$glmnet.fit$beta[,ind] )  

    }
  })

  output$model.plot <- renderPlot({
    model = fitted()$model
    if(class(model)[[1]] == "glm"){
      plot(model, which=2)
    }
    else if( class(model)[[1]] == "cv.glmnet" ){
      plot( model$lambda, model$cvm, type="l", lwd=3, ylim = c( min(model$cvlo),max(model$cvup) ) )
      lines( model$lambda, model$cvup, lwd=1 )
      lines( model$lambda, model$cvlo, lwd=1 )
      abline(v = model$lambda.1se, lwd="2", col="purple")
      abline(v = model$lambda[input$lambda], col="red", lwd="2")
      abline(v = model$lambda.min, lwd=2, col="orange" )
    }
 
  })

  output$min.value <- renderText({
    sprintf( "%.04f", -max(abs(fitted()$image) ) )
  })
  output$max.value <- renderText({
    sprintf( "%.04f / effect=%.04f", max(abs(fitted()$image)), fitted()$effect )
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
      folder <- sprintf("%s-%s-%s", input$variable, input$feature, input$component) 
      dir.create( folder )
      file1 = sprintf("%s/cor-axial-%s-%s-%s.png", folder, 
                      input$variable, input$feature, input$component) 
      png( file = file1 )
      plot.slice( fitted()$image, 
                 input$feature, input$transparency, input$threshold, input$background, 
                  c(input$sagittalslice,  input$coronalslice, input$axialslice), 3, input$anatomy, FALSE)
      dev.off()

      file2 = sprintf("%s/cor-coronal-%s-%s-%s.png", folder, 
                      input$variable, input$feature, input$component) 
      png( file = file2 )
      plot.slice( fitted()$image, 
                  input$feature, input$transparency, input$threshold, input$background, 
                  c(input$sagittalslice,  input$coronalslice, input$axialslice), 2, input$anatomy, FALSE)
      dev.off()
      
      
      file3 = sprintf("%s/cor-sagittal-%s-%s-%s.png", folder, 
                      input$variable, input$feature, input$component) 
      png( file = file3 )
      plot.slice( fitted()$image,  
                  input$feature, input$transparency, input$threshold, input$background, 
                  c(input$sagittalslice,  input$coronalslice, input$axialslice), 1, input$anatomy, FALSE)
      dev.off()


      zip( file, c(file1, file2, file3) )

      unlink( folder )
    }
  )


}

# Create Shiny object
shinyApp(ui = ui, server = server)

