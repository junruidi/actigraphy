library(accelerometry)

Time = function(
  x,
  w,
  thresh,
  smallerthan = TRUE,
  bout.length = 1
){
  if(missing(w)){
    stop("Please input weartime flag vector w with same dimension")
  }
  
  if(length(x) != length(w)){
    stop("count x and weartime w should have the same length")
  }
  
  uwear = unique(c(w))
  uwear = as.integer(uwear)
  if (!all(uwear %in% c(0, 1))) {
    stop("weartime w has non 0-1 data!g")
  }
  
  
  w[w == 0] = NA
  y = accel.bouts(counts = x, thresh.lower = thresh, bout.length = bout.length)
  yw = y * w
  
  if(smallerthan){
    time = sum(yw == 0, na.rm = T)
  }
  if(!smallerthan){
    time = sum(yw == 1, na.rm = T)
  }
  
  return(time = time)
}
