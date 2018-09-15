#' @title Timne Metrics for Whole Dataset
#' @description This function is a whole dataset wrapper for \code{Time}
#'
#' @param count.data \code{data.frame} of dimension n*1442 containing the 1440 minute activity data for all n subject days.
#' The first two columns have to be ID and Day.
#' @param weartime \code{data.frame} with dimension of \code{count.data}.
#' The first two columns have to be ID and Day.
#'
#' @param thresh threshold to binarize the data.
#' @param bout.length minimum duration of defining an active bout; defaults to 1.
#' @param smallerthan Find a state that is smaller than a threshold, or greater than or equal to.
#'
#' @importFrom accelerometry bouts
#'
#' @return A dataframe with some of the following columns
#' \item{ID}{identifier of the person}
#' \item{Day}{indicator of which day of activity it is, can be a numeric vector of sequence 1,2,... or a string of date}
#' \item{time}{time of certain state}
#'
#' @export
#'
#' @examples
#' data(example_activity_data)
#' count = example_activity_data$count
#' wear = example_activity_data$wear
#' sed_all = Time_long(count.data = count,weartime = wear,
#' thresh = 100,bout.length = 1,smallerthan = TRUE)
#'
#'


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
  names(time_all) = c("ID","Day","time")


  return(time_all = time_all)
}
