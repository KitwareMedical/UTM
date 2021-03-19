library(ANTsR)

atlas <- list()
im <- antsImageRead("../templates/T1_brain.nii")
atlas$bg.image = as.array(im)
im <- antsImageRead("lpba40.nrrd")
#left right is flipped
atlas$label.image = as.array(im)+1
atlas$label.image = atlas$label.image[rev(1:dim(atlas$label.image)[1]),, ]
lab = read.table("LPBA40-labels.txt")
labels = 1:(max(lab[,1])+1)
labels[lab[,1]+1] = as.character(lab[,2])
atlas$labels = as.data.frame(labels)
save(atlas, file="atlas.Rdata")
