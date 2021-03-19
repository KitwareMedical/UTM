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
library(mmand, quietly=TRUE)
library(glmnet, quietly=TRUE)
library(broom, quietly=TRUE)
library(DT, quietly=TRUE)
library(shinyAce)
library(corrplot)

option_list = list(
  make_option("--barycenterfolder", type="character", default="Barycenter",
              help="Subfolder in Analysis for storing barycenters [default=Barycenter]", metavar="character"),
  make_option(c("-a", "--analysisfolder"), type="character", default="Analysis",
              help="Folder to store results in. [default=Analysis]", metavar="character"),
  make_option(c("-b", "--barycentertype"), type="character", default="Euclidean",
              help="Barycenter to display [default = Euclidean]", metavar="character"),
  make_option("--workingfolder", type="character", default=".",
              help="Working directory folder [default=.]", metavar="character"),
  make_option( "--atlas", type="character", default="./atlas.Rdata",
               help="Atlas for displaying anatomical labels and information", metavar="character"),
  make_option( "--pcs.reg", type="character", default="VBM-pcs-reg.Rdata",
               help="Regularized Componets data file", metavar="character"),
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

load( config$variablesfile )

load( config$pcs.file )
load( config$pcs.reg.file )

load( config$barycenters$euclidean )
barycenter.euclidean = barycenter
try(suppressWarnings( load( config$barycenters$otdiscrete ) ), silent=TRUE )
barycenter.ot = barycenter

t.cols = hcl.colors( palette="PuOr", n = 255, alpha=1)
t.cols = colorRamp( t.cols )

var.names <- c()
for(i in 1:length(variables) ){
  var.names <- c(var.names, variables[[i]]$name )
}
dims = dim(barycenter.euclidean$image)


background.names <- c(  "Atlas", "Average") #, sprintf("Subject%4d", 1:n) )



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
      column(1, align="left",
             br(),
             textOutput(outputId="max.value"),
             plotOutput(outputId="colorbar", height = "200px", "30px"),
             textOutput(outputId="min.value")
       ),
       column(2, align="left",
         radioButtons( "anatomy" , "Anatomy", c("All"), inline = FALSE)
       )
    )
  ),
  tabPanel("3d View",
        rglwidgetOutput("plot3d", height = "600px", width="auto")
  )
  ),
  hr(),
    fluidRow(
      column(2,
         selectInput( inputId = "background", label = "Background Image:",
                     choices = background.names), bsPopover( "background",  "Background Image",
            "Background to display, either the average or a particular subject.",
            "left", options = list(container = "body") )
        ),
      column(2,
         sliderInput( inputId = "transparency", label = "Transparency:",
                                   min = 0.0, max = 1.0, value = 1.0, step = 0.01,
                                   animate = animationOptions(interval = 100) )
        ),
      column(2,
         sliderInput( inputId = "threshold", label = "Effect Threshold:",
                                   min = 0.0, max = 0.2, value = 0.0, step = 0.001,
                                   animate = animationOptions(interval = 50)
                    )
        ),
      column(2,
         sliderInput( inputId = "p.threshold", label = "P-Value Threshold:",
                                   min = 0.0, max = 1, value = 0.05, step = 0.001,
                                   animate = animationOptions(interval = 50)
                    )
        ),
      column(2,
         sliderInput( inputId = "max.color", label = "Color Level:",
                                   min = 0.0, max = 1.0, value = 1.0, step = 0.01,
                                   animate = animationOptions(interval = 100)
                    )
        ),
      column(2,
         downloadButton('downloadSlices', 'Download Slice Images')
      )
    ),
  hr(style = "border-top: 1px solid #000000;"),
  tabsetPanel(
   tabPanel("Statistical Model",
    fluidRow(
      column(3,
         selectInput( inputId = "feature", label="Feature:", choices=pcs$names ),
         selectInput( inputId = "component", label="Components:", choices=c("PC", "SpatPC" )),
         sliderInput( inputId = "ncomps", label = "# Components:",min = 2,
                      max = min(nrow(pcs$components[[1]]),nrow(reg.pcs$components[[1]]) ),
                      value = min(nrow(pcs$components[[1]]),nrow(reg.pcs$components[[1]]) ), step = 1),
         sliderInput( inputId = "comp.sel", label = "Show Component:",min = 1,
                      max = min(nrow(pcs$components[[1]]),nrow(reg.pcs$components[[1]]) ),value = 1, step = 1),
         selectInput( inputId = "variable", label="Variable:", choices=var.names ),
         bsPopover("variable", "Variable Selection", "Show correlations with respect to selection", "right",
                    options = list(container = "body")),
         selectInput( inputId = "model", label="Model:", choices=c("joint-linear", "elastic", "linear", "correlations") ),
         selectInput( inputId = "family", label="Family:", choices=c("gaussian", "binomial")),
         sliderInput( inputId = "alpha", label = "Alpha:",
                      min = 0.0, max = 1, value = 0.0, step = 0.001
                    ),
         sliderInput( inputId = "lambda", label = "lambda:",
                      min = 1, max = 100, value = 1, step = 1),
         checkboxGroupInput(inputId = "control", label="Control for:", choices=var.names)
      ),
      column(5,
        htmlOutput( outputId = "model.print" ),
        DTOutput( outputId = "model.dt" )
      ),
      column(4,
        plotOutput( outputId = "model.plot" )
      )
     )
   ),
   tabPanel("Variables",
      column(3,
         DTOutput( outputId = "variables" )
      ),
      column(6,
         plotOutput( outputId="variables.cor")
      )
   ),
   tabPanel("Scripting",
    aceEditor("code","summary(df)"),
    actionButton("eval","Evaluate code"),
    verbatimTextOutput("scripting")
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
      bg <-  atlas$bg.image
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

get.atlas.label <- function(im, slices ){
   index = NA
   if( !is.null(atlas) ){
     da <- dim(atlas$label.image)
     di <- dim(im)
     index <- atlas$label.image[ as.integer( slices[1] / di[1]*da[1] ),
                   as.integer( slices[2] / di[2]*da[2] ),
                   as.integer( slices[3] / di[3]*da[3] ) ]

   }
   index
}

plot.slice <- function( im, p.im, feature, c.max, transparency,
                        threshold, p.threshold, background, slice,
                        axis, anatomy="All", show.crosshair=TRUE){

    useAtlas <- anatomy != "All" & !is.null(atlas)

    bg <- get.background( background, feature )
    bg <- bg/max(bg)
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
      bg <- bg[,, slice[3] / d[3] * db[3]]
      mask <- barycenter.euclidean$orig.image[,,slice[3]] > 0
      if(useAtlas){
        bg.atlas <- bg.atlas[,, slice[3] / d[3] * da[3] ]
      }
      vline = (slice[1] + 0.5) / d[1]
      hline = (slice[2] + 0.5) / d[2]
    } else if(axis == 2){
      tim <- im[,slice[2],]
      bg <- bg[,slice[2]/ d[2] * db[2],]
      mask <- barycenter.euclidean$orig.image[,slice[2],] > 0
      if(useAtlas){
        bg.atlas <- bg.atlas[,slice[2] / d[2] * da[2], ]
      }
      vline = (slice[1] + 0.5) / d[1]
      hline = (slice[3] + 0.5) / d[3]
    } else{
      tim <- im[slice[1],,]
      bg <- bg[slice[1]/ d[1] * db[1],,]
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
    tim[tim > c.max ] = c.max
    tim[tim < -c.max ] = -c.max
    t.index <- which( abs( tim ) <= threshold )
    tim[ t.index ] <- NA
    #t.index <- which( abs( pim ) > p.threshold )
    tim[ t.index ] <- NA

    par( mar=c(0,0,0,0))

    t.cols.tmp = rgb( t.cols( seq(0, 1, length.out=99))/256, alpha=transparency )
    #t.cols.tmp[50] = "#00000000"

    image( bg, col=gray.colors(100), axes=FALSE, zlim=rbg, asp=1  )
    image( tim, col=t.cols.tmp, add=TRUE, zlim=c(-c.max, c.max),axes=FALSE, asp =1, useRaster=TRUE)

    if( useAtlas ){
      highlight = bg.atlas
      highlight[] = NA
      ls <- c()
      for( i in 1:ncol(atlas$labels) ){
        ls <- append(ls, which( atlas$labels[,i] == anatomy ))
      }
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

get.max.color <- function(im){
  c.max = max( abs(im), na.rm=T )
  if( is.infinite( c.max ) ){
    c.max = 0
  }
  c.max
}

# Define server function
server <- function(input, output, session) {

  fitted  <- reactive({
    correlations <- NA
    df = get.data.frame()$df
    nx = get.data.frame()$nx
    comps = get.data.frame()$comps
    f.index = get.data.frame()$f.index
    values <- rep(NA, nrow(comps) )
    p.values <- rep(NA, length(values) )
    deviance <- rep(NA, length(values) )
    aic <- rep(NA, length(values) )
    model <- NA
    effect <- NA

    try({
    if(input$model == "joint-linear"){
      l <- glm( y~., df, family=input$family )
      sl <- tidy(l)
      coeff <- sl$estimate[2:nx]
      p.values <- sl$p.value[2:nx]
      model = l
      values <- coeff
      g.dir= coeff / sqrt(sum(coeff^2))
      image = array(t(comps) %*% g.dir, dim= pcs$dimension[[f.index]] )
    }
    else if(input$model=="elastic"){
      gl = elastic.model()
      ind = input$lambda
      if(ind > length(gl$lambda) ){
        ind = length(gl$lambda)
      }
      updateSliderInput(session, "lambda", max=length(gl$lambda), step=1 )
      coeff = gl$glmnet.fit$beta[1:(nx-1),ind]
      model = gl
      values <- coeff
      g.dir= coeff / sqrt(sum(coeff^2))
      image = array(t(comps) %*% g.dir, dim= pcs$dimension[[f.index]] )
      effect=sum(g.dir*coeff)
    }
    else if(input$model=="linear"){
      correlations = rep(NA, nx-1)
      for(i in 2:nx){
        if( nx < ncol(df) ){
          df2 = df[, c(1,i, (nx+1):ncol(df))]
        }
        else{
          df2 = df[, c(1,i)]
        }
        tryCatch({
        l = glm( y ~ ., data=df2, family=input$family, maxit=25)
        sl <- tidy(l)
        p.values[i-1] <- sl$p.value[2]
        if(p.values[i-1] > 0){
          values[i-1] <- sl$estimate[2]
        }
        sl = summary(l)
        deviance[i-1] = sl$deviance
        aic[i-1] <- sl$aic
        },
        warning = function(w){
          values[i-1] = 0
          p.values[i-1] = 1
          aic[i-1] = NA
          deviance[i-1] = NA
        }
        )
      }
      comp.id = input$comp.sel
      image = array(comps[comp.id, ], dim= pcs$dimension[[f.index]] )

    }
    else{
      correlations = rep(NA, nx-1)
      for(i in 2:nx){
        x = df[,i]
        y = as.numeric(df$y)
        corr <- cor.test(x, y)
        correlations[i-1] = corr$estimate
        p.values[i-1] = corr$p.value
      }
      values <- correlations
      comp.id = input$comp.sel
      image = array(comps[comp.id, ], dim= pcs$dimension[[f.index]] )
    }
    })

    im.dim = dim(image)
    p.image = array(1, dim=dims)
    list( image=image, p.image=p.image, model=model, effect=effect,
          values = values, correlations=correlations, p.values=p.values,
          deviance=deviance, aic=aic)
  })

  get.data.frame <- reactive({
    f.index = which( pcs$names == input$feature )
    v.index = which( var.names == input$variable )
    var <- variables[[v.index]]
    if(input$component == "PC"){
      df <-data.frame( y=var$values, x=scale( t(pcs$projections.smoothed[[f.index]]) )[, 1:input$ncomps] )
      comps = pcs$components[[f.index]][1:input$ncomps, ]
    }
    else{
      df <-data.frame( y=var$values, x=scale( t(reg.pcs$projections[[f.index]]))[, 1:input$ncomps]  )
      comps = reg.pcs$components[[f.index]][1:input$ncomps, ]
    }
    nx <- ncol(df)

    if(input$family == "binomial"){
       df$y = as.factor(df$y)
    }
    for( cvar in input$control ){
       index = which( cvar == var.names)
       df[ variables[[index]]$name ] = variables[[index]]$values
    }
    list(df=df, nx=nx, f.index=f.index, comps=comps)

  })

  elastic.model <- reactive({
    measure = "default"
    if(input$family == "binomial"){
      measure = "class"
    }
    df = get.data.frame()$df
    index <- complete.cases(df)
    try({gl <- cv.glmnet( y=df$y[index], x=as.matrix(df[index, -1]),
                     alpha=input$alpha, nfolds=10, family=input$family,
                      type.measure=measure  ) })
    gl
  })


  observe.fitted <- observe({
    try({
    i.max = max(abs(fitted()$image), na.rm=T);
    updateSliderInput(session, "threshold", max=i.max, step=i.max/100 )
    })
  })


  observe.atlas <- observe({
     index <- get.atlas.label( fitted()$image, c(input$sagittalslice,  input$coronalslice, input$axialslice) )
     if( !is.na(index) ){
       choices = as.vector( c("All", unlist( as.character(atlas$labels[index, ] ) ))  )
       updateRadioButtons( session, "anatomy", choices=choices, inline=FALSE)
     }
  })

  observe.fitted <- observe({
    cmax = signif( get.max.color( fitted()$image ), 3 )
    updateSliderInput(session, "max.color", value=cmax, max=cmax, step=cmax/100)
  })

  #axial slice
  output$axial <- renderPlot({
    slices = c(input$sagittalslice,  input$coronalslice, input$axialslice)
    plot.slice( fitted()$image, fitted()$p.image, input$feature, input$max.color,
                input$transparency, input$threshold, input$p.threshold, input$background,
                slices, 3, input$anatomy)

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
    slices = c(input$sagittalslice,  input$coronalslice, input$axialslice)
    plot.slice( fitted()$image, fitted()$p.image, input$feature, input$max.color,
                input$transparency, input$threshold, input$p.threshold, input$background,
                slices, 2, input$anatomy)
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
    slices = c(input$sagittalslice,  input$coronalslice, input$axialslice)
    plot.slice( fitted()$image, fitted()$p.image, input$feature, input$max.color,
                input$transparency, input$threshold, input$p.threshold, input$background,
                slices, 1, input$anatomy)
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
  output$model.print <- renderText({
    model = fitted()$model
    if(input$model == "joint-linear"){
      text = "<b>Joint Linear</b><br/>"
      sm = summary(model)
      text =sprintf( "%s Deviance: %.2f<br/>", text, sm$deviance  )
      text =sprintf( "%s AIC: %.2f", text, sm$aic  )
    }
    else if( input$model == "elastic" ){
      #ind <- which(model$lambda.1se == model$lambda)
      ind = input$lambda
      if(ind > length(model$lambda) ){
        ind = length(model$lambda)
      }
      text = "<b>Elastic Net</b><br/>"
      text = sprintf("%s CV: %.4f <br/>", text, model$cvm[ind] )
      text = sprintf("%s Dev Ratio: %.4f <br/>", text, model$glmnet.fit$dev.ratio[ind] )
      text = sprintf("%s Lambda: %.4f", text, model$lambda[ind])

    }
    else if( input$model == "linear" ){
      text = "<b>Linear Per Component</b>"
    }
    else{
      text = "<b>Correlation Per Component</b>"
    }
    text
  })


    #Info / Controls
  output$model.dt <- renderDT({
    model = fitted()$model
    if(input$model == "joint-linear"){
      df =  tidy(model)
      df = df[order(df$p.value), ]
      dt = datatable(df) %>% formatRound(2:ncol(df), 4)
    }
    else if( input$model == "elastic" ){
      #ind <- which(model$lambda.1se == model$lambda)
      ind = input$lambda
      if(ind > length(model$lambda) ){
        ind = length(model$lambda)
      }
      df = data.frame( terms=rownames(model$glmnet.fit$beta), coefficient = model$glmnet.fit$beta[,ind] )
      df = df[order(abs(df$coefficient), decreasing=TRUE), ]
      dt = datatable(df) %>% formatRound(1:ncol(df), 5)
    }
    else if( input$model == "linear" ){
      df = data.frame(deviance = fitted()$deviance, aic=fitted()$aic, coefficient=fitted()$values, p.coeff=fitted()$p.values)
      rownames(df) = names(fitted()$p.values)
      df = df[order(df$p.coeff), ]
      dt = datatable(df) %>% formatRound(1:4, 4)
    }
    else{
      df = data.frame(cor = fitted()$correlations, p=fitted()$p.values)
      rownames(df) = names(fitted()$correlations)
      df = df[order(df$p), ]
      dt = datatable(df) %>% formatRound(1:2, 4)
    }
    dt
  })


  output$model.plot <- renderPlot({
    try({
    model = fitted()$model
    if(class(model)[[1]] == "glm"){
      plot(model, which=2)    }
    else if( class(model)[[1]] == "cv.glmnet" ){
      x = 1:length(model$cvm)
      plot( x, model$cvm, type="l", lwd=3, ylim = c( min(model$cvlo),max(model$cvup) ) )
      lines( x, model$cvup, lwd=1 )
      lines(x, model$cvlo, lwd=1 )
      abline(v = which(model$lambda==model$lambda.1se), lwd="2", col="purple")
      abline(v = input$lambda, col="red", lwd="2")
      abline(v = which(model$lambda==model$lambda.min), lwd=2, col="orange" )
    }
    })
  })

  output$min.value <- renderText({
    sprintf( "%.04f", -input$max.color )
  })
  output$max.value <- renderText({
    sprintf( "%.04f", input$max.color )
  })

  output$colorbar <- renderPlot({
    par( mar=c(0,0,0,0))
    t.cols.tmp = rgb( t.cols( seq(0, 1, length.out=99))/256)
    x <- matrix(1:200, nrow=200, ncol=30)
    image( t(x), col=t.cols.tmp, axes=FALSE )
  })


  output$variables <- renderDT({
    df = data.frame(mean = rep(NA, length(var.names)), sdev = rep(NA, length(var.names)) )
    rownames(df) <- var.names
    for(i in 1:length(variables) ){
      df$mean[i] = mean(variables[[i]]$value, na.rm=TRUE)
      df$sdev[i] = sd(variables[[i]]$value, na.rm=TRUE)
    }
    dt = datatable(df) %>% formatRound(1:2, 3)
    dt
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

  output$scripting <- renderPrint({
    input$eval
    df <- get.data.frame()$df
    return(isolate(eval(parse(text=input$code))))
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)


