#!/usr/bin/env Rscript
options(rgl.useNULL=TRUE)
options(rgl.inShiny = TRUE)

library(rmarkdown)
library(optparse, quietly=TRUE)
library(stringr, quietly=TRUE)
library(shiny, quietly=TRUE)
library(shinyBS, quietly=TRUE)
library(shinythemes, quietly=TRUE)
library(glmnet, quietly=TRUE)
library(broom, quietly=TRUE)
library(DT, quietly=TRUE)
library(shinyAce)
library(corrplot)
library(vtkwidgets)
library(raster)

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
    help="Atlas for displaying anatomical labels and information", metavar="character"),
  make_option( "--parcels", type="character",
    default="VBM-parcels.Rdata,Conv-parcels.Rdata,UTM-parcels.Rdata",
    help="Parcel projection file", metavar="character")
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
config$parcels =strsplit(config$parcels, ",")[[1]]
config$parcels.file = sprintf("./%s/Images/%s", config$analysisfolder, config$parcels )
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
render.script = readLines("./render.js")
atlas <- NULL
try( load(config$atlas), silent=TRUE)
load( config$variablesfile )
parcels.tmp = list()
for(f in config$parcels.file){
  try({
    load( f)
    if( length(parcels.tmp) == 0 ){
      parcels.tmp = parcels
    }else{
      for( i in 1:length(parcels) ){
        parcels.tmp[[i]] = append(parcels.tmp[[i]], parcels[[i]])
      }
    }
   }, silent=TRUE)
}
parcels= parcels.tmp
parcels$ilist = vector( mode="list", length=length(label.index) )
for(i in 1:length(label.index)){
 parcels$ilist[[i]] = list(label = label.index,
                           index = which( atlas$label.image == label.index[i] ) )
}
print(warnings())
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
dims = dim(atlas$label.image)
background.names <- c(  "Atlas", "Average") #, sprintf("Subject%4d", 1:n) )
default.js.code ="
augment.data.frame <- function(df){
  #Input/ouput a dataframe with a column y
  #and columns with measurements
  #for each parcel and controlled
  #variables
  return( df )
}

augment.model <- function(model, df){
  #Return a list with
  # * estimates - a data.frame with
  #    * coef - estimated coefficeint
  #    * p.values - estimated p.values for each coeffient (or NA if not available)
  #    * additional optionl estimates per predictor
  # * name
  # * summary - list with any model summary statistics
  return( model )
}
"

# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
 navbarPage("VolPA - Volumetric Population Analysis",
  tabPanel("Explore",
   fluidRow(
    column(3, align="left", offset = 0,
     bsCollapse(multiple=TRUE,
      bsCollapsePanel("View Settings",
       sliderInput(inputId = "transparency", label = "Transparency:",
                   min = 0.0, max = 1.0, value = 1.0, step = 0.01),
       sliderInput(inputId = "threshold", label = "Effect Threshold:", ticks=FALSE,
                   min = 0.0, max = 1, value = 0.0, step = 0.01),
       sliderInput(inputId = "max.color", label = "Color Level:", ticks=FALSE,
                   min = 0.0, max = 1.0, value = 1.0, step = 0.01),
       selectInput(inputId = "background", label = "Background Image:",
                   choices = background.names),
       bsPopover("background",  "Background Image",
                 "Background to display, either the average or the atlas if available.",
                 "left", options = list(container = "body") ),
       br(),
       bsCollapse(
        bsCollapsePanel("3D View Settings",
         checkboxInput("renderBackground", "Show 3D Background"),
         sliderInput("backgroundIsovalue", "Isovalue",
                     min=0, max=1, value=0.5, step=0.01),
         sliderInput("backgroundTransparency", "Transparency",
                     min=0, max=1, value=0.1, step=0.01),
         checkboxInput("backgroundSolid", "Solid"),
         checkboxInput("renderSliceZ", "Show Axial Slice"),
         checkboxInput("renderSliceY", "Show Coronal Slice"),
         checkboxInput("renderSliceX", "Show Saggital Slice")
        )
       ),
       hr(),
       downloadButton('downloadSlices', 'Download Slice Images')
       #downloadButton('downloadAllSlices', 'Download All Slice Images')
      ),
      bsCollapsePanel("Statistical Models",
       selectInput( inputId = "feature", label="Analysis:", choices=parcels$names  ),
       bsPopover( "feature",  "Analysis Type",
                    "<b>vbm:</b> Volumetric changes per voxel <br>
                    <b>convolution:</b> Volumetric changes accounting for local volumetric diffferences <br>
                    <b>allocation:</b> Volumetric changes accounting for global volumetric diffferences <br>
                    <b>transport:</b> Volumetric transfers<br>",
                    "right", options = list(container = "body")  ),
       selectInput( inputId = "variable", label="Variable:", choices=var.names ),
       bsPopover("variable", "Variable Selection", "Show correlations with respect to selection",
                   "right", options = list(container = "body")),
       shinyWidgets::sliderTextInput( inputId = "p.threshold", label = "P-Value Threshold:",
                      choices = formatC(c(0,rev(c(rbind(5 * 10^-(1:8), 10^-(1:8))))), digits=1, format="e"),
                      selected = formatC(0.05,digits=1, format="e"), grid=TRUE),
       bsCollapsePanel("Include Control Variables",
        checkboxGroupInput(inputId = "control", label="Control for:", choices=var.names)
       ),
       br(),
       bsCollapse(
        bsCollapsePanel("Regression Models",
        selectInput(inputId = "family", label="Family:",
                    choices=c("gaussian", "binomial")),
        bsCollapse(id = "model",
         bsCollapsePanel("Joint Linear", "Multiple linear regression on all parcels"),
         bsCollapsePanel("Elastic Net",
          "Elastic net regression on all parcels",
          sliderInput(inputId = "alpha", label = "Alpha:",
                      min = 0.0, max = 1, value = 0.0, step = 0.001
          ),
          sliderInput(inputId = "lambda", label = "lambda:",
                      min = 1, max = 100, value = 1, step = 1)
          ),
          bsCollapsePanel("Linear",
           "Linear regression per parcel"
          )
         )
        ),
        bsCollapsePanel("Correlation",
         "Correlations per parcel",
         selectInput( inputId = "corr.type", label = "Type",
                      choices = c("pearson", "kendall", "spearman") )
        )
       )
      )
     )
    ),
    column(9, align="left", style='padding:2px;',
     tabsetPanel(type="pills",
      tabPanel("Slice View",
       fluidRow(
        column(3, align="center", offset = 0, style='padding:2px;',
         plotOutput( outputId = "axial", width = "300px", click = "axial_click",
                          dblclick = "axial_dblclick", hover = "axial_hover" )
        ),
        column(3, align="center", offset = 0, style='padding:2px;',
         plotOutput( outputId = "coronal", width = "300px", click = "coronal_click",
                          dblclick = "coronal_dblclick", hover = "coronal_hover")
        ),
        column(3, align="center", offset = 0, style='padding:2px;',
         plotOutput( outputId = "sagittal", width = "300px",  click = clickOpts(id="sagittal_click"),
                           dblclick = "sagittal_dblclick", hover = "sagittal_hover")
        ),
        column(3, align="left",
         textOutput(outputId="max.value"),
         plotOutput(outputId="colorbar", height = "200px", "30px"),
         textOutput(outputId="min.value"),
         br(),
         htmlOutput( outputId = "text.value" ),
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
       sliderInput(inputId = "axialslice", label = "Axial Slice:",
                   min = 1, max = dims[3],
                   value = round(dims[3]/2), step = 1)
      ),
      column(3, align="center", offset = 0, style='padding:2px;',
       sliderInput(inputId = "coronalslice", label = "Coronal Slice:",
                   min = 1, max = dims[2],
                   value = round(dims[2]/2), step = 1)
      ),
      column(3, align="center", offset = 0, style='padding:2px;',
       sliderInput(inputId = "sagittalslice", label = "Sagittal Slice:",
                   min = 1, max = dims[1],
                   value = round(dims[1]/2), step = 1)
      )
     ),
     hr(),
     bsCollapse(
      bsCollapsePanel("Statistical Model",
       fluidRow(
        column(5,
         htmlOutput( outputId = "model.print" ),
         DTOutput( outputId = "model.dt" )
        ),
        column(4,
         plotOutput( outputId = "model.plot" )
        )
       )
      )
     ),
     bsCollapsePanel("Scripting",
      "Augment data and / or statistical model",
      fluidRow(
       column(6,
        aceEditor("code",default.js.code),
        actionButton("eval","Evaluate code")
       ),
       column(6,
        verbatimTextOutput("scripting")
       )
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
      pim <- p.im[,,slice[3]]
      bg <- bg[,, slice[3] / d[3] * db[3]]
      if(useAtlas){
        bg.atlas <- bg.atlas[,, slice[3] / d[3] * da[3] ]
      }
      vline = (slice[1] + 0.5) / d[1]
      hline = (slice[2] + 0.5) / d[2]
    } else if(axis == 2){
      tim <- im[,slice[2],]
      pim <- p.im[,slice[2],]
      bg <- bg[,slice[2]/ d[2] * db[2],]
      if(useAtlas){
        bg.atlas <- bg.atlas[,slice[2] / d[2] * da[2], ]
      }
      vline = (slice[1] + 0.5) / d[1]
      hline = (slice[3] + 0.5) / d[3]
    } else{
      tim <- im[slice[1],,]
      pim <- p.im[slice[1],,]
      bg <- bg[slice[1]/ d[1] * db[1],,]
      if(useAtlas){
        bg.atlas <- bg.atlas[slice[1] / d[1] * da[1],, ]
      }
      vline = (slice[2] + 0.5) / d[2]
      hline = (slice[3] + 0.5) / d[3]
    }
    #tim = tim * mask
    tim[tim > c.max ] = c.max
    tim[tim < -c.max ] = -c.max
    t.index <- which( abs( tim ) <= threshold )
    tim[ t.index ] <- NA
    t.index <- which( abs( pim ) > p.threshold )
    tim[ t.index ] <- NA

    par( mar=c(0,0,0,0))

    t.cols.tmp = rgb( t.cols( seq(0, 1, length.out=99))/256, alpha=transparency )
    #t.cols.tmp[50] = "#00000000"

    db = dim(bg)
    image( bg, col=gray.colors(100), axes=FALSE, zlim=rbg, asp=db[2]/db[1], useRaster=T)
    di = dim(tim)
    image( tim , col=t.cols.tmp, add=TRUE, zlim=c(-c.max, c.max), axes=FALSE,
           asp=di[2]/di[1], useRaster=T)

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

    if(show.crosshair){
      abline(v=vline, col="#0085B8")
      abline(h=hline, col="#0085B8")
    }
    invisible()
}

get.max.color <- function(im){
  c.max = 0
  try({
  c.max = max( abs(im), na.rm=T )
  if( is.infinite( c.max ) ){
    c.max = 0
  }
  })
  c.max
}

# Define server function
server <- function(input, output, session) {

  session$userData$background = ""
  session$userData$feature = ""
  session$userData$variable = ""
  session$userData$p.value = ""
  session$userData$threshold = ""

  fitted.model  <- reactive({
    df = get.data.frame()$df
    nx = get.data.frame()$nx
    f.index = get.data.frame()$f.index

    estimates = data.frame( coef = rep(NA, ncol(df)-1),
                            p.values = rep(NA, ncol(df)-1 ) )
    rownames(estimates) = colnames(df)[-1]
    model <- list( name = input$model, summary = list(), model=NULL, estimates=estimates )

    try({
    if(length(input$model) == 0){
      model$name <- "Correlation"
      for(i in 2:ncol(df)){
        try({
        x = df[,i]
        y = as.numeric(df$y)
        corr <- cor.test(x, y, method=input$corr.type)
        model$estimates$coef[i-1] = corr$estimate
        model$estimates$p.values[i-1] = corr$p.value
        })
      }
    }
    else if(input$model == "Joint Linear"){
      l <- glm( y~., df, family=input$family )
      sl <- tidy(l)
      model$estimates$coef <- sl$estimate[-1]
      model$estimates$p.values <- sl$p.value[-1]
      model$model = l
      sm = summary(model$model)
      model$summary$deviance = sm$deviance
      model$summary$aic = sm$aic

    }
    else if(input$model=="Elastic Net"){
      gl = elastic.model()
      ind = input$lambda
      if(ind > length(gl$lambda) ){
        ind = length(gl$lambda)
      }
      updateSliderInput(session, "lambda", max=length(gl$lambda), step=1 )
      model$estimates$coef = gl$glmnet.fit$beta[,ind]
      model$model = gl
      model$summary$cv = gl$cvm[ind]
      model$summary$deviance.ratio = gl$glmnet.fit$dev.ratio[ind]
      model$summary$lambda = gl$lambda[ind]
    }
    else if(input$model=="Linear"){
      n = ncol(df)
      model$estimates$aic <- rep(NA, n-1)
      model$estimates$deviance <- rep(NA, n-1)
      for(i in 1:(n-1)){
        if(i < nx && nx < n){
          df2 = df[, c(1,i+1, (nx+1):ncol(df))]
        }
        else{
          df2 = df[, c(1,i+1)]
        }
        tryCatch({
        l = glm( y ~ ., data=df2, family=input$family, maxit=25)
        sl <- tidy(l)
        model$estimates$p.values[i] <- sl$p.value[2]
        if(model$estimates$p.values[i] > 0){
          model$estimates$coef[i] <- sl$estimate[2]
        }
        sl = summary(l)
        model$estimates$deviance[i] = sl$deviance
        model$estimates$aic[i] <- sl$aic
        },
        warning = function(w){
          model$estimates$values[i] = 0
          model$estimates$p.values[i] = 1
        }
        )
      }
    }
    })
    try({
      input$eval
      isolate(eval(parse(text=input$code)))
      return( augment.model(model, df) )
    })
    model
  })

  fitted <- reactive({
    res = fitted.model()
    df = get.data.frame()$df
    try({
      input$eval
      isolate(eval(parse(text=input$code)))
      return( augment.model(res, df) )
    })
    res
  })


  fitted.image <- reactive({
    estimates = fitted()$estimates
    im.dim = dim(atlas$label.image)
    image = array(NA, dim=im.dim )
    p.image = array(1, dim=im.dim)
    for(i in 1:length(label.index)){
      try({
      if( !is.na(estimates$coef[i] ) ){
        image[ parcels$ilist[[i]]$index ] = estimates$coef[i]
        p.image[ parcels$ilist[[i]]$index ] = estimates$p.values[i]
      }
      })
    }
    list( image=image, p.image=p.image )
  })


  get.data.frame <- reactive({
    f.index = which( parcels$names == input$feature )
    v.index = which( var.names == input$variable )
    var <- variables[[v.index]]
    sproj = scale(parcels$projections[[f.index]])
    sproj[is.na(sproj)] = 0
    df <- data.frame( y=var$values,  sproj)

    nx <- ncol(df)

    if(input$family == "binomial"){
       df$y = as.factor(df$y)
    }
    for( cvar in input$control ){
       index = which( cvar == var.names)
       df[ variables[[index]]$name ] = variables[[index]]$values
    }
    res = list(df=df[complete.cases(df$y),], nx=nx, f.index=f.index)
    try({
      input$eval
      isolate(eval(parse(text=input$code)))
      res$df = augment.data.frame(res$df)
      return(res)
    })
    res

  })

  elastic.model <- reactive({
    measure = "default"
    if(input$family == "binomial"){
      measure = "class"
    }
    df = get.data.frame()$df
    index <- complete.cases(df)
    gl <- NULL
    try({
      gl <- cv.glmnet( y=df$y[index], x=as.matrix(df[index, -1]),
                     alpha=input$alpha, nfolds=10, family=input$family,
                     type.measure=measure  )
    })
    gl
  })

  observe.fitted <- observe({
    try({
    i.max = signif(get.max.color( fitted()$estimates$coef ), 3)
    updateSliderInput(session, "threshold", max=i.max, step=i.max/100, value=0)
    })
  })

  observe.atlas <- observe({
     index <- get.atlas.label( fitted.image()$image,
                 c(input$sagittalslice, input$coronalslice, input$axialslice) )
     if( !is.na(index) ){
       choices = as.vector( c("All", unlist( as.character(atlas$labels[index, ] ) ))  )
       updateRadioButtons( session, "anatomy", choices=choices, inline=FALSE)
     }
  })

  observe.fitted <- observe({
    cmax = signif( get.max.color( fitted()$estimates$coef ), 3 )
    updateSliderInput(session, "max.color", value=cmax, max=cmax, step=cmax/100)
  })

  #axial slice
  output$axial <- renderPlot({
    slices = c(input$sagittalslice,  input$coronalslice, input$axialslice)
    plot.slice(fitted.image()$image, fitted.image()$p.image, input$feature,
               input$max.color, input$transparency, input$threshold,
               input$p.threshold, input$background,
              slices, 3, input$anatomy)

  })


  observeEvent(input$axial_click, {
     d <- dim( fitted.image()$image )
     x = as.integer( input$axial_click$x * d[1] )
     y = as.integer( input$axial_click$y * d[2])
     updateSliderInput(session, "sagittalslice", value=x)
     updateSliderInput(session, "coronalslice", value=y)
  })



  #coronal slice
  output$coronal <- renderPlot({
    slices = c(input$sagittalslice,  input$coronalslice, input$axialslice)
    plot.slice( fitted.image()$image, fitted.image()$p.image, input$feature,
               input$max.color,input$transparency, input$threshold,
               input$p.threshold, input$background, slices, 2, input$anatomy)
  })


  observeEvent(input$coronal_click, {
     d <- dim( fitted.image()$image )
     x = as.integer( input$coronal_click$x * d[1] )
     y = as.integer( input$coronal_click$y * d[3])
     updateSliderInput(session, "sagittalslice", value=x)
     updateSliderInput(session, "axialslice", value=y)
  })


  #sagittal slice
  output$sagittal <- renderPlot({
    slices = c(input$sagittalslice,  input$coronalslice, input$axialslice)
    plot.slice( fitted.image()$image, fitted.image()$p.image, input$feature,
                input$max.color, input$transparency, input$threshold,
                input$p.threshold, input$background,
                slices, 1, input$anatomy)
  })

  observeEvent(input$sagittal_click, {
     d <- dim( fitted.image()$image )
     x = as.integer( input$sagittal_click$x * d[2] )
     y = as.integer( input$sagittal_click$y * d[3])
     updateSliderInput(session, "coronalslice", value=x)
     updateSliderInput(session, "axialslice", value=y)
  })

  output$volume1 <- renderVtkWidget({
    withProgress(message = 'Loading Volume', value = 0, {
    incProgress(0.1, detail = "Prepare Volume")

    bg.image <- get.background(input$background, input$feature)
    dim.im = dim(fitted.image()$image)
    dim.bg = dim(bg.image)
    spacing = dim.im / dim.bg
    im <- NULL
    if( session$userData$feature != input$feature ||
        session$userData$variable != input$variable ||
        session$userData$p.value != input$p.threshold ||
        session$userData$threshold != input$threshold
      ){
      session$userData$feature = input$feature
      session$userData$variable = input$variable
      session$userData$p.value = input$p.threshold
      session$userData$threshold = input$threshold

      im  <- fitted.image()$image
      im[is.na(im)] = 0
      vmax = get.max.color(im)
      t.index <- which( abs( im ) <= input$threshold )
      im[ t.index ] <- 0
      pim  <- fitted.image()$p.image
      t.index <- which( pim > input$p.threshold )
      im[ t.index ] <- 0
      #make sure transfer function is scaled correctly
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

  output$model.print <- renderText({
    model = fitted()
    text = sprintf("<h3>%s</h3>", model$name)
    sname = names(model$summary)
    for( name in sname ){
      text =sprintf( "%s %s: %.2f", text, name, model$summary[name])
    }
    text = sprintf("%s <h4>Estimates:</h4>", text)
    text
  })

  output$model.dt <- renderDT({
    df = fitted()$estimates
    dt = datatable(df) %>% formatRound(1:ncol(df), 4)
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
  #Info / Controls
  output$text.value <- renderUI({
    coeff = fitted.image()$image[input$sagittalslice, input$coronalslice, input$axialslice]
    p.value = fitted.image()$p.image[input$sagittalslice, input$coronalslice, input$axialslice]
    HTML(sprintf( "Effect: %.02f <br>p: %.01e", coeff, p.value))
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
      plot.slice( fitted.image()$image,
                 input$feature, input$transparency, input$threshold, input$background,
                  c(input$sagittalslice,  input$coronalslice, input$axialslice), 3, input$anatomy, FALSE)
      dev.off()

      file2 = sprintf("%s/cor-coronal-%s-%s-%s.png", folder,
                      input$variable, input$feature, input$component)
      png( file = file2 )
      plot.slice( fitted.image()$image,
                  input$feature, input$transparency, input$threshold, input$background,
                  c(input$sagittalslice,  input$coronalslice, input$axialslice), 2, input$anatomy, FALSE)
      dev.off()


      file3 = sprintf("%s/cor-sagittal-%s-%s-%s.png", folder,
                      input$variable, input$feature, input$component)
      png( file = file3 )
      plot.slice( fitted.image()$image,
                  input$feature, input$transparency, input$threshold, input$background,
                  c(input$sagittalslice,  input$coronalslice, input$axialslice), 1, input$anatomy, FALSE)
      dev.off()


      zip( file, c(file1, file2, file3) )

      unlink( folder )
    }
  )

  output$scripting <- renderPrint({
    input$eval
    isolate(eval(parse(text=input$code)))
    try({
        df <- get.data.frame()$df
        model <- fitted.model()
        augment.data.frame(df)
        augment.model(model, df)
        "success"
    })
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)

