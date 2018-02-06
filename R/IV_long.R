#' @title Intradaily Variability for Whole Dataset
#' @description Intradaily variability is a nonparametric metric
#' reprsenting fragmentation of circadian rhtymicity. this function is a whole data wrapper for
#' \code{IV}.
#'
#' @param count.data \code{data.frame} of dimension n * 1442 containing the 1440 minute activity data for all n subject days.
#' The first two columns have to be ID and Day.
#' @param level time resolution to calcualte IV. Can be either "minute", or "hour".
#'
#' @return A \code{data.frame} with following columns
#' \item{ID}{identifier of the person}
#' \item{Day}{indicator of which day of activity it is, can be a numeric vector of sequence 1,2,... or a string of date}
#' \item{IV_min}{minute level IV}
#' \item{IV_hour}{hourly level IV parameter}
#'
#' @export
#' @examples
#' data(example_activity_data)
#' count.data = example_activity_data$count
#' iv_all = IV_long(count.data = count.data, level = "hour")
#'
#'
#'
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
