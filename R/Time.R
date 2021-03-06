#' @title Time of A Certain activity State
#' @description Calculate the total time of being in certain state, e.g. sedentary, active, MVPA, etc.
#'
#' @param x \code{vector} of activity data.
#' @param w \code{vector} of wear flag data with same dimension as \code{x}.
#' @param thresh threshold to binarize the data.
#' @param bout.length minimum duration of defining an active bout; defaults to 1.
#' @param smallerthan Find a state that is smaller than a threshold, or greater than or equal to.
#'
#' @return Time
#' @importFrom accelerometry bouts
#'
#' @export
#'
#' @examples
#' data(example_activity_data)
#' count1 = c(t(example_activity_data$count[1,-c(1,2)]))
#' wear1 = c(t(example_activity_data$wear[1,-c(1,2)]))
#' time_sedentary = Time(x = count1, w = wear1, thresh = 100,
#' bout.length = 1, smallerthan = TRUE)
#'
#'

Time = function(
  x,
  w,
  thresh,
  smallerthan = TRUE,
  bout.length = 1
){
  if(missing(w)){
    stop("Please input weartime flag vector w with same dimension")
  }

  if(length(x) != length(w)){
    stop("count x and weartime w should have the same length")
  }

  uwear = unique(c(w))
  uwear = as.integer(uwear)
  if (!all(uwear %in% c(0, 1, NA))) {
    stop("weartime w has non 0-1 data")
  }

  x = na.omit(x)
  w = na.omit(w)


  w[w == 0] = NA
  y = bouts(counts = x, thresh_lower = thresh, bout_length = bout.length)
  yw = y * w

  if(smallerthan){
    time = sum(yw == 0, na.rm = T)
  }
  if(!smallerthan){
    time = sum(yw == 1, na.rm = T)
  }

  return(time = time)
}
