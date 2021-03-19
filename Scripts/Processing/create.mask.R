args <- commandArgs(trailingOnly = TRUE)

images.folder   <- args[1]
mask.file   <- args[2]

image.files <- list.files(images.folder)

total <- 0
for(i in 1:length(images)){
  total <- total + load.images(images[i])
}

mask = total > 0

save(mask, file=mask.file)

