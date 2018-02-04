library(refund)
library(dplyr)

crfpca = function(
  count.data,
  logtransform = FALSE,
  knots = 20,
  pve = 0.9
){
  
  count.data$Day = NULL

  if(logtransform){
    count.data[,2:1442] = log(count.data[,2:1442] + 1)
  }
  act = as.data.frame(count.data %>% group_by(ID) %>% summarise_all(funs(mean(.,na.rm = T))))
  ID = act$ID
  t = c(1:1440)/1440
  Y = as.matrix(act[,-1])

  fpca.model = fpca.face(Y,center = TRUE, argvals = t,knots= knots, pve = pve)
  phi = fpca.model$efunctions
  pcs = data.frame(ID = ID,fpca.model$scores)
  pcs$ID = as.character(pcs$ID)
  
  
  return(list(phi = phi, pcs = pcs))
  
}