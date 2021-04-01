library(png, quietly=TRUE)
library(shape, quietly=TRUE)
library(ANTsR)
args <- commandArgs( trailingOnly=TRUE )


images.path <- args[1]
var.table.file <- args[2]
n=40

outer = c()
mass = c()
mass1 = c()
mass2 = c()
#create images
for(i in 1:n){
  png(width=100, height=100, file="tmp.png", bg="black" )
  par(mar=c(0,0,0,0))
  plot(x=NA, y=NA, xlim = c(-2.0, 2.0), ylim = c(-1.0, 1.0), xlab=NA, ylab=NA, axes=FALSE, asp=1)

  r1 = 1.95
  r2 = 1
  d1 = 0.3
  d2 = 0.3
  m1 = runif(1, 0.25, 0.75)
  m2 = runif(1, 0.25, 0.75)
  filledellipse( rx1 = r1, rx2=r1-d1, col=rgb(m1, m1, m1) )
  filledellipse( rx1 = r2, rx2=r2-d2, col=rgb(m2, m2, m2) )
  mass = c(mass, m2+m1)
  mass1 = c(mass1, m1)
  mass2 = c(mass2, m2)
  dev.off()

  im = readPNG("tmp.png")
  imfile = sprintf("%s/annuli%04i.nrrd", images.path, i)
  antsImageWrite(as.antsImage(im[,,1:3]), imfile)
}


vars <- data.frame( name = sprintf("annuli%04i.nrrd", 1:n) )
vars$mass1 = mass1
vars$mass2 = mass2
vars$mass = mass

write.csv(vars, file=var.table.file, row.names=FALSE)


