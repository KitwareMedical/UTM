get.max.cor <- function(variable){
  cmax = 0
  features <- cor.res[[ variable ]]$im
  mask <- barycenter.euclidean$orig.image > 0
  for(i in 1:length(features ) ){
    cmax = max( cmax, max(abs(features[[i]][mask]) ) )
  }
  cmax

}


plot.slice <- function(variable, feature, maxcor, transparency, threshold, background, slice, axis, anatomy="All"){


    useAtlas <- anatomy != "All"
    
    im  <- cor.res[[ variable ]]$im[[ feature ]]
    pim <- cor.res[[ variable ]]$pim[[ feature  ]]
   
    bg <- get.background( background, feature )
    rbg <- range(bg) 
    db <- dim(bg)
    d <- dim(im)
    if(useAtlas){
      bg.atlas <- get.background( "Atlas", feature )
      da <- dim(atlas)
    }
    
    vline = 0 
    hline = 0
    if(axis == 3){
      tim <- im[,,slice[3]]
      pim <- pim[,,slice[3]]
      bg <- bg[,, slice[3]]
      mask <- barycenter.euclidean$orig.image[,,slice[3]] > 0
      if(useAtlas){
        bg.atlas <- bg.atlas[,, slice[3] / d[3] * da[3] ]
      }
      vline = (slice[1] + 0.5) / d[1]
      hline = (slice[2] + 0.5) / d[2]
    } else if(axis == 2){
      tim <- im[,slice[2],]
      pim <- pim[,slice[2],]
      bg <- bg[,slice[2],]
      mask <- barycenter.euclidean$orig.image[,slice[2],] > 0
      if(useAtlas){
        bg.atlas <- bg.atlas[,slice[2] / d[2] * da[2], ]
      }
      vline = (slice[1] + 0.5) / d[1]
      hline = (slice[3] + 0.5) / d[3]
    } else{
      tim <- im[slice[1],,]
      pim <- pim[slice[1],,]
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
    tim = tim * mask
    t.index <- which( pim <= (1-threshold) )
    tim[ t.index ] <- NA 
    tim[ bg == 0] <- NA

    par( mar=c(0,0,0,0))

    c.max <- maxcor
    tim[tim > c.max ] = c.max
    tim[tim < -c.max ] = -c.max
    t.cols.tmp = rgb( t.cols( seq(0, 1, length.out=99))/256, alpha=transparency )
    t.cols.tmp[50] = "#00000000"
    
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
    abline(v=vline, col="#0085B8")
    abline(h=hline, col="#0085B8")
    invisible()
}





plot.3d <- function( variable, feature,  threshold, background="Average"){
  im  <- cor.res[[ variable ]]$im[[ feature ]]
  pim <- cor.res[[ variable ]]$pim[[ feature  ]]
   
  bg <- get.background( background, feature )

  rbg <- range(bg) 
  db <- dim(bg)
  d <- dim(im)

  contour3d(bg, level = min(bg[bg>0]), alpha=0.1, draw=TRUE)
 
  psign <- pim * sign(im)
  contour3d( psign, level = 1-threshold, 
             draw=TRUE, color=rgb(t.cols(1)/255), alpha=0.5, add=TRUE )
  contour3d( -psign, level = 1-threshold, 
             draw=TRUE, color=rgb(t.cols(0)/255), alpha=0.5, add=TRUE )

}


#create.3d <- function(variable, feature,  threshold, background="Average"){
#  library(brainR)
#  
#  plot.3d(variable, feature,  threshold, background)
#
#  writeWebGL_split(dir=getwd(), filename ="3d.html", template = system.file("my_template.html", package="brainR"), width=500)
#
#  rgl.close()
#  #rgl.quit()
#
#}
