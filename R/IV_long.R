IV_long = function(
  count.data,
  level = c("minute","hour")
){
  level = match.arg(level)
  x = count.data[,-c(1:2)]
  result.list =  apply(x,1,IV,level = level)
  iv_all = as.data.frame(cbind(count.data[,c(1,2)],result.list))

  if(level == "minute"){
    names(iv_all) = c("ID","Day","IV_min")
  }
  if(level == "hour"){
    names(iv_all) = c("ID","Day","IV_hour")
  }
  return(iv = iv_all)
}
