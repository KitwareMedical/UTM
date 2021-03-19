library(glmnet)

k.feature = 2

display.component <- function(comp, d, bg, filename="" ){
  comp = array(comp, dim=d)
  #comp[ abs(comp) < 0.01*zlim ] = NA
  cols = hcl.colors( palette="PuOr", n = 255, alpha=1)
  if(nchar(filename) > 0 ){
    png(filename, width=dims[1], height=dims[2])
    par( mar = c(0,0,0,0) )
  }
  #image(  bg, col=gray.colors(n=255, start=0, end=1), axes=F )
  if(length(d) == 3){
    comp = colSums(comp, 3)
  }
  
  zlim = max(abs(comp), na.rm=TRUE)
  image( comp, zlim=c(-zlim, zlim), col=cols,  useRaster=TRUE )
  
  if(nchar(filename) > 0 ){
    dev.off()
  }
}

display.weighted.component <- function(w, comps, d, bg, filename=""){
  comp = t(comps)%*%w 
  display.component(comp, d, bg, filename) 
}



display.components <- function(comps1, comps2, k, d, bg){
  comp = c()

  for(i in 1:k){
    x1 = array(comps1[i, ], dim=d)
    x2 = array(comps2[i, ], dim=d)
    if( length(d)  == 3 ){
      x1 = colSums(x1, 3)
      x2 = colSums(x2, 3)
    }
    comp = cbind( comp, rbind(x1,x2) )
  }
  cols = hcl.colors( palette="PuOr", n = 255, alpha=1)
  zlim = max(abs(comp), na.rm=TRUE)
  print(zlim)
  try(
  image( t(comp), zlim=c(-zlim, zlim), col=cols, useRaster=TRUE )
  )
}

dims = pcs$dimension[[k.feature]]
#display.component( reg.pcs$components[[k.feature]], 1, dims, barycenter$image)
#dev.new()
#display.component( pcs$components[[k.feature]], 1, dims, barycenter$image )
#dev.new()


for(i in 1:nrow( pcs$components[[k.feature]])){
  display.component( pcs$components[[k.feature]][i, ], dims, barycenter$image, sprintf("pc-comp-%.2i.png", i))
} 
for(i in 1:nrow( reg.pcs$components[[k.feature]])){
  display.component( reg.pcs$components[[k.feature]][i, ], dims, barycenter$image, sprintf("reg-comp-%.2i.png", i)) 
}

display.components(reg.pcs$components[[k.feature]], pcs$components[[k.feature]], 
                   min( nrow(reg.pcs$components[[k.feature]]), nrow(pcs$components[[k.feature]])), 
                   dims,  barycenter$image)


for(k in 1:length(variables) ){
  print (variables[[k]]$name )
  sd.pc = 1 #sqrt(diag( var(t(pcs$projections[[1]])) ))
  sd.reg = 1 #sqrt(diag( var(t(reg.pcs$projections[[1]])) ))
  index = variables[[k]]$index
  df.pc = data.frame(y = variables[[k]]$values, #age = variables[[2]]$values, 
                     x = scale( t(pcs$projections[[k.feature]])) )
  df.reg = data.frame(y = variables[[k]]$values, #age = variables[[2]]$values, 
                      x = scale( t(reg.pcs$projections[[k.feature]])) )
  lm.pc  <- lm( y ~ ., data=df.pc[index, ] )
  lm.reg <- lm( y ~ ., data=df.reg[index, ]) 
  slm.pc = summary(lm.pc )
  slm.reg = summary(lm.reg )
  print(  slm.pc  )
  print(  slm.reg  )

  age.index = which(rownames(slm.reg$coefficients) == "age")
  coeff.reg = slm.reg$coefficients[c(-1, -age.index), 1]
  ind.reg = which(slm.reg$coefficients[c(-1, -age.index), 4] <  0.05) 
  coeff.reg[-ind.reg] = 0
  try({
    display.weighted.component( coeff.reg, reg.pcs$components[[k.feature]], dims, barycenter$image, 
                            sprintf("reg-lm-var-%s.png", variables[[k]]$name ) )
  })
  age.index = which(rownames(slm.pc$coefficients) == "age")
  coeff.pc = slm.pc$coefficients[c(-1, -age.index), 1]
  ind.pc = which(slm.pc$coefficients[c(-1, -age.index), 4] <  0.05)  
  coeff.pc[-ind.pc] = 0
  try({
    display.weighted.component( coeff.pc, pcs$components[[k.feature]], dims, barycenter$image, 
                            sprintf("pc-lm-var-%s.png", variables[[k]]$name ) )
  })



  for(alpha in c(0, 0.01, 0.1, 1) ){
   
    glm.pc = cv.glmnet(y = variables[[k]]$values[index], 
                       x =  scale( t(pcs$projections[[k.feature]])[index,]), 
                       alpha=alpha) 
    pc.ind = which( glm.pc$lambda.min == glm.pc$lambda)
    coeff.pc = glm.pc$glmnet.fit$beta[,pc.ind] * sd.pc 
    try({
     display.weighted.component( coeff.pc, pcs$components[[k.feature]], dims, barycenter$image, 
                            sprintf("pc-glm-alpha-%.2f-var-%s.png", alpha, variables[[k]]$name ) )
    })
    pc.ind1se = which( glm.pc$lambda.1se == glm.pc$lambda)
    coeff.pc = glm.pc$glmnet.fit$beta[,pc.ind1se]  * sd.pc 
    try({
     display.weighted.component( coeff.pc, pcs$components[[k.feature]], dims, barycenter$image, 
                            sprintf("pc-glm1se-alpha-%.2f-var-%s.png", alpha, variables[[k]]$name ) )
    })

    try({
    glm.reg = cv.glmnet(y = variables[[k]]$values[index], 
                        x = scale( t(reg.pcs$projections[[k.feature]])[index,]), 
                        alpha=alpha) 
    reg.ind = which( glm.reg$lambda.min == glm.reg$lambda)
    coeff.reg = glm.reg$glmnet.fit$beta[,reg.ind] * sd.reg
    try({
     display.weighted.component( coeff.reg, reg.pcs$components[[k.feature]], dims, barycenter$image,  
                            sprintf("reg-glm-alpha-%.2f-var-%s.png", alpha, variables[[k]]$name ) )
    })
    reg.ind1se = which( glm.reg$lambda.1se == glm.reg$lambda)
    coeff.reg = glm.reg$glmnet.fit$beta[,reg.ind1se] * sd.reg
    try({
     display.weighted.component( coeff.reg, reg.pcs$components[[k.feature]], dims, barycenter$image,  
                            sprintf("reg-glm1se-alpha-%.2f-var-%s.png", alpha, variables[[k]]$name ) )
    })
    })
   
    print( sprintf("alpha %.2f", alpha)) 
    print( sprintf("glm cv pc    : %f", min(glm.pc$cvm)) )
    print( sprintf("glm cv pc reg: %f", min(glm.reg$cvm)) )
    print( sprintf("glm cv pc    : %f", (glm.pc$cvm[pc.ind])) )
    print( sprintf("glm cv pc reg: %f", (glm.reg$cvm[reg.ind])) )
    print( sprintf("glm cv pc    : %f", (glm.pc$cvm[pc.ind1se])) )
    print( sprintf("glm cv pc reg: %f", (glm.reg$cvm[reg.ind1se])) )
    print( sprintf("glm cv pc     : %f", max(glm.pc$glmnet.fit$dev.ratio))) 
    print( sprintf("glm cv pc reg : %f", max(glm.reg$glmnet.fit$dev.ratio))) 
    print( sprintf("glm cv pc     dr 1: %f", glm.pc$glmnet.fit$dev.ratio[pc.ind])) 
    print( sprintf("glm cv pc reg dr 1: %f", glm.reg$glmnet.fit$dev.ratio[reg.ind])) 
    print( sprintf("glm cv pc     dr 2: %f", glm.pc$glmnet.fit$dev.ratio[pc.ind1se])) 
    print( sprintf("glm cv pc reg dr 2: %f", glm.reg$glmnet.fit$dev.ratio[reg.ind1se]))

  }

}

