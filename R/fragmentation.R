#' @title Fragmentation Metrics
#' @description Fragmentation methods to study the transition between two states, e.g.
#' sedentary v.s. active.
#'
#' @param x \code{vector} of activity data.
#' @param w \code{vector} of wear flag data with same dimension as \code{x}.
#' @param thresh threshold to binarize the data.
#' @param bout.length minimum duration of defining an active bout; defaults to 1.
#' @param metrics What is the fragmentation metrics to exract. Can be
#' "mean_bout","TP","Gini","power","hazard",or all the above metrics "all".
#'
#' @return A list with elements
#' \item{mean_r}{mean sedentary bout duration}
#' \item{mean_a}{mean active bout duration}
#' \item{SATP}{sedentary to active transition probability}
#' \item{ASTP}{bactive to sedentary transition probability}
#' \item{Gini_r}{Gini index for active bout}
#' \item{Gini_a}{Gini index for sedentary bout}
#' \item{h_r}{hazard function for sedentary bout}
#' \item{h_a}{hazard function for active bout}
#' \item{alpha_r}{power law parameter for sedentary bout}
#' \item{alpha_a}{power law parameter for active bout}
#'
#' @importFrom stats na.omit reshape
#' @importFrom dplyr%>% as_data_frame filter
#' @importFrom accelerometry accel.bouts rle2
#' @importFrom survival survfit Surv
#' @importFrom ineq Gini
#'
#' @export
#'
#' @details Metrics include
#' mean_bout (mean bout duration),
#' TP (between states transition probability),
#' Gini (gini index),
#' power (alapha parameter for power law distribution)
#' hazard (average hazard function)
#'
#'
#' @examples
#' data(example_activity_data)
#' count1 = c(t(example_activity_data$count[1,-c(1,2)]))
#' wear1 = c(t(example_activity_data$wear[1,-c(1,2)]))
#' frag = fragmentation(x = count1, w = wear1, thresh = 100, bout.length = 1, metrics = "mean_bout")
#'
#'


fragmentation = function(
  x,
  w,
  thresh = 1,
  bout.length,
  metrics = c("mean_bout","TP","Gini","power","hazard","all")
){
  metrics = match.arg(metrics)

  if(missing(w)){
    stop("Please input weartime flag vector w with same dimension")
  }


  if(length(x) != length(w)){
    stop("x and w should have the same length")
  }

  uwear = unique(c(w))
  uwear = as.integer(uwear)
  if (!all(uwear %in% c(0, 1))) {
    stop("w has non 0-1 data!")
  }


  w[w == 0] = NA
  y = accel.bouts(counts = x, thresh.lower = thresh, bout.length = bout.length)
  yw = y * w

  uy = unique(na.omit(yw))
  if (length(uy) == 1) {
    #stop("Only one state found in the activity, no transition defined.")

      if(metrics == "mean_bout"){
       frag = list(mean_r = NA, mean_a = NA)
      }

      if(metrics == "TP"){
       frag = list(SATP = NA, ASTP = NA)
      }

      if(metrics == "Gini"){
        frag = list(Gini_r = NA, Gini_a = NA)
      }

      if(metrics == "power"){
        frag = list(alpha_r = NA, alpha_a = NA)
      }

      if(metrics == "hazard"){
        frag = list(h_r = NA, h_a = NA)
      }

      if (metrics == "all"){
      frag = list(mean_r = NA, mean_a = NA,
                  SATP = NA, ASTP = NA,
                  Gini_r = NA,
                  Gini_a = NA,
                  alpha_r = NA,
                  alpha_a = NA,
                  h_r =  NA,
                  h_a = NA
      )
      }
  }


  if (length(uy) > 1) {
  mat = as_data_frame(rle2(yw)) %>%
    filter(!is.na(values))

  A = mat$lengths[which(mat$values == 1)]
  R = mat$lengths[which(mat$values == 0)]

  if(metrics == "mean_bout"){
    frag = list(mean_r = mean(R), mean_a = mean(A))
  }

  if(metrics == "TP"){
    frag = list(SATP = 1/mean(R), ASTP = 1/mean(A))
  }

  if(metrics == "Gini"){
    frag = list(Gini_r = Gini(R,corr = T),
                Gini_a = Gini(A,corr = T))
  }


  if(metrics == "power"){
    nr = length(R)
    na = length(A)

    rmin = min(R)
    amin = min(A)

    frag = list(alpha_r = 1+ nr/sum(log(R/(rmin-0.5))),
                alpha_a = 1+ na/sum(log(A/(amin-0.5))))

  }

  if(metrics == "hazard"){
    fitr = survfit(Surv(R,rep(1,length(R)))~1)
    fita = survfit(Surv(A,rep(1,length(A)))~1)

    frag = list(h_r =  mean(fitr$n.event/fitr$n.risk),
                h_a = mean(fita$n.event/fita$n.risk))
  }

  if(metrics == "all"){

    nr = length(R)
    na = length(A)

    rmin = min(R)
    amin = min(A)

    fitr = survfit(Surv(R,rep(1,length(R)))~1)
    fita = survfit(Surv(A,rep(1,length(A)))~1)

    frag = list(mean_r = mean(R), mean_a = mean(A),
                SATP = 1/mean(R), ASTP = 1/mean(A),
                Gini_r = Gini(R,corr = T),
                Gini_a = Gini(A,corr = T),
                alpha_r = 1+ nr/sum(log(R/(rmin-0.5))),
                alpha_a = 1+ na/sum(log(A/(amin-0.5))),
                h_r =  mean(fitr$n.event/fitr$n.risk),
                h_a = mean(fita$n.event/fita$n.risk)
                )
  }}

  return(frag)
}
