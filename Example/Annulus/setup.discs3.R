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

total_mass = c()
outer_mass = c()
inner_mass = c()
independent_measure = c()

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
  m2 = runif(1, 0.25, 0.75) # Random number uniformly sampled from the interval (0.25, 0.75)

  # Draw outer annulus, radius r1, thickness d1, and mass-per-pixel (i.e. grayscale value) m1
  filledellipse( rx1 = r1, rx2=r1-d1, col=rgb(m1, m1, m1) )

  # Draw inner annulus, radius r2, thickness d2, and mass-per-pixel (i.e. grayscale value) m2
  filledellipse( rx1 = r2, rx2=r2-d2, col=rgb(m2, m2, m2) )

  dev.off()

  # The array im below should be 100 by 100 by 3, where the final dimension was originally created for rgb channels.
  # However we are going to view it as a grayscale image with three spatial dimensions, with the third dimension
  # having a tiny extent of 3 and with no variation in values along that third dimension.
  # The reason for this strange way of creating essentially 2D images is that it the shiny app for visualization
  # will complain if it has only one "slice" to visualize, and this hack is easier than fixing the shiny app.
  im = readPNG("tmp.png")

  # Comment out this line for Case 2 of the annulus figure in the paper. Uncomment for Case 1 of the figure.
  im = im/sum(im)

  # The m1, m2 here are the tissue densities of the outer and inner annli respectively, not the total masses of the annuli.
  # But they are proportional to total masses, with the proportionality constants (the respective annulus volumes) being
  # the same across all images. So it will not make a difference for the correlation analysis.
  outer_mass = c(outer_mass, m1)
  inner_mass = c(inner_mass, m2)

  # For total mass it does make a difference, so we use the image sum for total mass
  # (Note that the sum of densities m1+m2 is not in any sense "total mass", because the annuli have different volumes.)
  total_mass = c(total_mass, sum(im))

  # Simulate a measure that is completely independent of of the OTF.
  # Looking at this helps verify that our multiple test correction is working properly,
  # i.e. that we are not getting too many false positives.
  independent_measure = c(independent_measure, runif(1, 0.25, 0.75))

  imfile = sprintf("%s/annuli%04i.nrrd", images.path, i)
  antsImageWrite(as.antsImage(im), imfile)
}


vars <- data.frame( name = sprintf("annuli%04i.nrrd", 1:n) )
vars$outer_mass = outer_mass
vars$inner_mass = inner_mass
vars$total_mass = total_mass
vars$independent_measure = independent_measure

write.csv(vars, file=var.table.file, row.names=FALSE)


