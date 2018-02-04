Time_long = function(
  count.data,
  weartime,
  thresh,
  smallerthan = TRUE,
  bout.length = 1
){
  if(missing(weartime)){
    print("No weartime supplied, calculated based on defualt from 05:00 to 23:00")
    weartime = wear_flag(count.data =  count.data)
  }
  

  mat = cbind(as.matrix(count.data[,-c(1:2)]),as.matrix(weartime[,-c(1:2)]))
  
  result.list =  apply(mat,1,function(x){
    Time(x[1:1440],x[1441:2880],thresh = thresh,bout.length = bout.length, smallerthan = smallerthan)
  })
  
  time_all = as.data.frame(cbind(count.data[,c(1,2)],result.list))
  names(time_all) = c("ID","Day","accel_time")
  
  
  return(time_all = time_all)
}