library(qdap)

wear_flag = function(
  count.data,
  start = "05:00",
  end = "23:00"
){
  if(grepl("(am)|(AM)|(pm)|(PM)",start) | grepl("(am)|(AM)|(pm)|(PM)",end)){
    stop("Please use 24hr format for start and end time withou am/pm")
  }
  count.mat = as.matrix(count.data[,-c(1:2)])
  wear.mat = matrix(0,nrow = nrow(count.mat),ncol = ncol(count.mat))
  start.i = as.numeric(beg2char(start,":")) * 60 + as.numeric(char2end(start,":")) + 1
  end.i = as.numeric(beg2char(end,":")) * 60 + as.numeric(char2end(end,":")) + 1
  
  wear.mat[,c(start.i:end.i)] = 1
  weartime = as.data.frame(cbind(count.data[,c(1,2)],wear.mat))
  names(weartime) = names(count.data)
  return(weartime = weartime)
}
