#' @title Intradaily Variability
#' @description This function calcualte intradaily variability, a nonparametric metric
#' reprsenting fragmentation of circadian rhtymicity
#'
#' @param x  \code{vector} vector of dimension 1440 which reprsents 1440 minute activity data. Or users can bin the data into different resolutions, e.g. 10 minute, 1 hour, etc.
#' @return IV
#'
#'
#' @export
#'
#'
#' @examples
#' data(example_activity_data)
#' count1 = c(t(example_activity_data$count[1,-c(1,2)]))
#' iv = IV(x = count1, level = "hour")
#'
#'

IV = function(
  x,
  level = c("minute","hour")
){
  mean.counts <- mean(x)
  numerator <- sum(diff(x)^2) * length(x)
  denominator <- sum((x-mean.counts)^2) * (length(x) - 1)
  return(numerator/denominator)
}
