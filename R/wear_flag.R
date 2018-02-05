#' @title Create Wear/Nonwear Flags
#' @description Determine during which time period, subject should wear the device.
#' It is preferable that user provide their own wear/non wear flag which should has the same dimension
#' as the activity data. This function provide wear/non wear flag based on time of day.
#'
#' @param count.data \code{data.frame} of dimension n*1442 containing the 1440 minute activity data for all n subject days.
#' The first two columns have to be ID and Day.
#'
#' @param start start time, a string in the format of 24hr, e.g. "05:00"; defaults to "05:00".
#' @param end end time, a string in the format of 24hr, e.g. "23:00"; defaults to "23:00"
#'
#'
#'
#' @return A \code{data.frame} with same dimension and column name as the \code{count.data}, with 0/1 as the elments
#' reprensting wear, nonwear respectively.

#' @export
#' @details Some of the features can be only calculated at certain time period. For example, fragmentation metrics are usually defined
#' when subject is awake. The \code{weartime} provide time periods on which those features should be extracted.
#' This can be also used as indication of wake/sleep.
#'
#'
#' @examples
#' data(example_activity_data)
#' count = example_activity_data$count
#' weartime = wear_flag(count.data = count)
#'
#'
#'
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

  start.i = as.numeric(gsub(":.[0-9]",replacement = "",start)) * 60 + as.numeric(gsub("[0-9].:",replacement = "",start)) + 1
  end.i = as.numeric(gsub(":.[0-9]",replacement = "",end)) * 60 + as.numeric(gsub("[0-9].:",replacement = "",end)) + 1

  wear.mat[,c(start.i:end.i)] = 1
  weartime = as.data.frame(cbind(count.data[,c(1,2)],wear.mat))
  names(weartime) = names(count.data)
  return(weartime = weartime)
}
