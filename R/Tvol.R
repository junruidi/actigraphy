#' @title Total Volumen of Activity for Whole Dataset
#' @description Calculate total volume of activity level, which includes
#' \code{TLAC} (total log transfored activity counts),
#' \code{TAC} (total activity counts).
#'
#' @param count.data \code{data.frame} of dimension n*1442 containing the 1440 minute activity data for all n subject days.
#' The first two columns have to be ID and Day.
#' @param weartime \code{data.frame} with dimension of \code{count.data}.
#' The first two columns have to be ID and Day.
#' @param logtransform if \code{TRUE}, then calcualte \code{TLAC}. Or calculate \code{TAC}.
#'
#'
#' @return A dataframe with some of the following columns
#' \item{ID}{identifier of the person}
#' \item{Day}{indicator of which day of activity it is, can be a numeric vector of sequence 1,2,... or a string of date}
#' \item{TAC}{total activity count}
#' \item{TLAC}{total log activity count}
#'
#'
#' @export
#' @details log transormation is defined as log(x+1).
#'
#'
#' @examples
#' data(example_activity_data)
#' count = example_activity_data$count
#' wear = example_activity_data$wear
#' tac = Tvol(count.data = count,weartime = wear,logtransform = FALSE)
#'


Tvol = function(
  count.data,
  weartime,
  logtransform = FALSE
){
  if(missing(weartime)){
    print("No weartime supplied, calculated based on defualt from 05:00 to 23:00")
    weartime = wear_flag(count.data =  count.data)
  }

  uwear = unique(unlist(c(weartime[,-c(1,2)])))
  uwear = as.integer(uwear)
  if (!all(uwear %in% c(0, 1))) {
    stop("weartime matrix has non 0-1 data! Set NA to 0 if missing")
  }

  count.mat = as.matrix(count.data[,-c(1:2)])
  wear.mat = as.matrix(weartime[,-c(1:2)])

  if(logtransform){
    count.mat = log(count.mat + 1)
  }

  adj.ct = count.mat * wear.mat
  tvol = as.data.frame(cbind(count.data[,c(1:2)],apply(adj.ct, 1, sum)))
  if(logtransform){
    names(tvol) = c("ID","Day","TLAC")
  }
  if(!logtransform){
    names(tvol) = c("ID","Day","TAC")
  }
  return(tvol = tvol)
}
