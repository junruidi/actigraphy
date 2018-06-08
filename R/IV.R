#' @title Intradaily Variability
#' @description This function calcualte intradaily variability, a nonparametric metric
#' reprsenting fragmentation of circadian rhtymicity
#'
#' @param x  \code{vector} vector of dimension 1440 which reprsents 1440 minute activity data
#' @param level time resolution to calcualte IV. Can be either "minute", or "hour".
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
