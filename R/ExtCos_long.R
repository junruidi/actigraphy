#' @title Extended Cosinor Model for Circadian Rhythmicity for Whole Dataset
#' @description A parametric approach to study circadian rhythmicity assuming cosinor shape.
#' Here we used the anti logistic transformed cosine curves, which provided 5 parameters.
#' This function is a wrapper for \code{ExtCos}.
#'
#' @param count.data \code{data.frame} of dimension n*1442 containing the 1440 minute activity data for all n subject days.
#' The first two columns have to be ID and Day.
#' @param logtransform Conduct log transfomation before. Default is \code{TRUE}.
#'
#' @importFrom minpack.lm nls.lm nls.lm.control
#' @importFrom stats coef reshape
#' @importFrom dplyr group_by %>% do
#'
#' @return A \code{data.frame} with following columns
#' \item{ID}{identifier of the person}
#' \item{min}{minimum}
#' \item{amp}{amplitude}
#' \item{alpha}{alpha parameter}
#' \item{beta}{beta parameter}
#' \item{acro}{acrophase}
#' \item{F_imp}{pseudo-F statistics}
#'
#' @export
#' @examples
#' data(example_activity_data)
#' count.data = example_activity_data$count
#' extcos = ExtCos_long(count.data = count.data, logtransform  = TRUE)
#'
#'


ExtCos_long = function(
  count.data,
  logtransform = TRUE
){

  # stupid NSE problem with dplyr
  ID = values = . = NULL
  rm(list = c("ID", "values", "."))


  long.count = reshape(count.data, varying = names(count.data)[3:1442],direction = "long",
                       timevar = "MIN",idvar = c("ID","Day"),v.names = "values")
  long.count = long.count[
    with(long.count, order(ID, Day,MIN)),
    ]

  result= long.count  %>% group_by(ID) %>% do(out = ExtCos(.$values,logtransform = logtransform))

  idlist = result$ID
  result.list = result$out

  vext= unlist(result.list)
  ext_all = as.data.frame(cbind(idlist,
                                vext[seq(1,length(vext),5)],
                                vext[seq(2,length(vext),5)],
                                vext[seq(3,length(vext),5)],
                                vext[seq(4,length(vext),5)],
                                vext[seq(5,length(vext),5)],
                                vext[seq(6,length(vext),5)]))
  names(ext_all) = c("ID","min","amp","alpha","beta","acro","F_imp")
  row.names(ext_all) = c(1:length(idlist))
  return(ext_all)
}
