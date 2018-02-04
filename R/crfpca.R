#' @title Functional Principal Component Analysis for Whole Day Circadian Rhythmicity
#' @description A nonparametric approach to study variational stucture of whole day
#' activity trajectory. This function provides the principal component loading functions and
#' corresponding principal component scores. FPCA is done via the sandwich smoother for covriance matrix smoothing.=
#'
#'
#' @param count.data \code{data.frame} of dimension n*1442 containing the 1440 minute activity data for all n subject days.
#' The first two columns have to be ID and Day.
#' @param longtransform Conduct log transfomation before FPCA. Default is false.
#' @param knots number of knots to use or the vectors of knots; defaults to 20.
#' @param pve proportion of variation explained; defaults to 0.9.
#'
#' @return Alist with elements
#' \item{phi}{Principal component loadings, aka eigen functions}
#' \item{pcs}{Principal component scores}
#'
#' @importFrom refund fpca.face
#' @importFrom dplyr $>$ group_by summarise_all
#'
#'
#' @examples
#'
#' res = crfpca(count.data = count, logtransform  = TRUE)
#'
#'
crfpca = function(
  count.data,
  logtransform = FALSE,
  knots = 20,
  pve = 0.9
){

  count.data$Day = NULL

  if(logtransform){
    count.data[,2:1442] = log(count.data[,2:1442] + 1)
  }
  act = as.data.frame(count.data %>% group_by(ID) %>% summarise_all(funs(mean(.,na.rm = T))))
  ID = act$ID
  t = c(1:1440)/1440
  Y = as.matrix(act[,-1])

  fpca.model = fpca.face(Y,center = TRUE, argvals = t,knots= knots, pve = pve)
  phi = fpca.model$efunctions
  pcs = data.frame(ID = ID,fpca.model$scores)
  pcs$ID = as.character(pcs$ID)


  return(list(phi = phi, pcs = pcs))

}
