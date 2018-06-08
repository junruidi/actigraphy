#' @title Relative Amplitude
#' @description This function calcualte relative amplitude, a nonparametric metric
#' reprsenting fragmentation of circadian rhtymicity
#'
#' @param x  \code{vector} vector of dimension 440 which reprsents 1440 minute activity data
#' @return RA
#'
#' @importFrom zoo rollapplyr
#'
#' @export
#'
#'
#' @examples
#' data(example_activity_data)
#' count1 = c(t(example_activity_data$count[1,-c(1,2)]))
#' ra = RA(x = count1)
#'
#'

RA = function(
  x
){
  day.counts = x
  M10 = max(roll(day.counts,600))
  L5 = min(roll(day.counts,300))

  return((M10 - L5)/(M10 + L5))
}

roll = function(day.counts,k){
  kvec = rollapplyr(day.counts, k, function(x) mean(x), fill = NA)
  kvec = kvec[!is.na(kvec)]
  return(kvec)
}
