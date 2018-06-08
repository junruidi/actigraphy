#' @title Interdaily Statbility
#' @description This function calcualte interdaily stability, a nonparametric metric
#' of circadian rhtymicity
#'
#' @param x  \code{data.frame} of dimension p by 1440, representing the p days of activity data for one subjectect
#' @return IS
#'
#'
#' @export
#'
#'
#' @examples
#' data(example_activity_data)
#' count1 = example_activity_data$count[c(1,2,3),-c(1,2)]
#' is = IS(x = count1)
#'
#'

IS = function(
  x
){

  x = convert_2_hr(x)
  p = ncol(x)
  hr_mean = colMeans(x)
  v = c(t(x))
  n = length(v)
  numerator = sum((hr_mean - mean(v))^2)/p
  denominator = sum((v - mean(v))^2)/n
  return(numerator/denominator)
}

convert_2_hr = function(x){
  hr = NULL
  for(i in 1:24){
    temp = x[,(60*(i-1)+1):(60*i)]
    temp.i = rowSums(temp,na.rm = T)
    hr = cbind(hr,temp.i)
  }
  hr = data.frame(hr)
  names(hr) = paste0("hr",c(1:24))
  return(hr)
}
