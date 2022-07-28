# Ensure png package is available
if (!require('png', quiet=TRUE)) {
  install.packages('png')
}

library(png, quietly=TRUE)
library(shape, quietly=TRUE)
library(ANTsR)
args <- commandArgs( trailingOnly=TRUE )


images.path <- args[1]
var.table.file <- args[2]
n=40

outer = c()
#create images
for(i in 1:n){
  png(width=100, height=100, file="tmp.png", bg="black" )
  par(mar=c(0,0,0,0))
  plot(x=NA, y=NA, xlim = c(-2.0, 2.0), ylim = c(-1.0, 1.0), xlab=NA, ylab=NA, axes=FALSE, asp=1)

  r1 = 1.95
  r2 = 1
  d1 = 0.3
  d2 = 0.3
  m1 = runif(1, 0.25, 0.75) # Random number uniformly sampled from the interval (0.25, 0.75)
  # m1 and m2 are the mass density (i.e. per pixel or voxel) of the outer and inner annulus respectively
  # So setting m2 = 1 - m1 below doesn't make much sense.
  # But, in terms of degrees of freedom, it also doesn't matter. Because we normalize at the end.
  m2 = 1 - m1

  # Draw outer annulus, radius r1, thickness d1, and mass-per-pixel (i.e. grayscale value) m1
  filledellipse( rx1 = r1, rx2=r1-d1, col=rgb(m1, m1, m1) )

  # Draw inner annulus, radius r2, thickness d2, and mass-per-pixel (i.e. grayscale value) m2
  filledellipse( rx1 = r2, rx2=r2-d2, col=rgb(m2, m2, m2) )

  # The m1 here is the tissue density of the outer annlus, not the total mass of the outer annulus.
  # But the two are proportional, with the proportionality constant (the volume of the annulus) being
  # the same across all images. So it will not make a difference for the correlation analysis.
  outer = c(outer, m1)

  dev.off()

  im = readPNG("tmp.png")
  im = im[,,1:3] # Since im is already three channels, this doesn't seem to do anything...
  im = im/sum(im) # Normalize im so that the sum value is 1. This is what enforces that the total mass is always the same.
  imfile = sprintf("%s/annuli%04i.nrrd", images.path, i)
  antsImageWrite(as.antsImage(im), imfile)
}


vars <- data.frame( name = sprintf("annuli%04i.nrrd", 1:n) )
vars$mass1 = outer

write.csv(vars, file=var.table.file, row.names=FALSE)


