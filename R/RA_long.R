#' @title Relative Amplitude for Whole Dataset
#' @description Relative Amplitude is a nonparametric metric
#' reprsenting fragmentation of circadian rhtymicity. this function is a whole data wrapper for
#' \code{RA}.
#'
#' @param count.data \code{data.frame} of dimension n * 1442 containing the 1440 minute activity data for all n subject days.
#' The first two columns have to be ID and Day.
#' @return A \code{data.frame} with following columns
#' \item{ID}{identifier of the person}
#' \item{Day}{indicator of which day of activity it is, can be a numeric vector of sequence 1,2,... or a string of date}
#' \item{RA}{RA}
#'
#'#' @importFrom zoo rollapplyr
#'
#' @export
#' @examples
#' data(example_activity_data)
#' count.data = example_activity_data$count
#' ra_all = RA_long(count.data = count.data)
#'
#'
#'
RA_long = function(
  count.data
){
  x = count.data[,-c(1:2)]
  result.list =  apply(x,1,RA)
  ra_all = as.data.frame(cbind(count.data[,c(1,2)],result.list))

  names(ra_all) = c("ID","Day","RA")
  return(ra= ra_all)
}
