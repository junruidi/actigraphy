Tvol = function(
  count.data,
  weartime,
  logtransform = FALSE
){
  if(missing(weartime)){
    print("No weartime supplied, calculated based on defualt from 05:00 to 23:00")
    weartime = wear_flag(count.data =  count.data)
  }
  
  uwear = unique(unlist(c(weartime[,-c(1,2)])))
  uwear = as.integer(uwear)
  if (!all(uwear %in% c(0, 1))) {
    stop("weartime matrix has non 0-1 data! Set NA to 0 if missing")
  }
  
  count.mat = as.matrix(count.data[,-c(1:2)])
  wear.mat = as.matrix(weartime[,-c(1:2)])
  
  if(logtransform){
    count.mat = log(count.mat + 1)
  }
  
  adj.ct = count.mat * wear.mat
  tvol = as.data.frame(cbind(count.data[,c(1:2)],apply(adj.ct, 1, sum)))
  if(logtransform){
    names(tvol) = c("ID","Day","TLAC")
  }
  if(!logtransform){
    names(tvol) = c("ID","Day","TAC")
  }
  return(tvol = tvol)
}
