IV = function(
  x,
  level = c("minute","hour")
){
  level = match.arg(level)
  day.counts = x
  if(level == "hour"){
    day.counts = unname(tapply(day.counts, (seq_along(day.counts)-1) %/% 60, sum))
  }
  
  mean.counts <- mean(day.counts)
  numerator <- sum(diff(day.counts)^2) * length(day.counts)
  denominator <- sum((day.counts-mean.counts)^2) * (length(day.counts) - 1)
  return(numerator/denominator)
}