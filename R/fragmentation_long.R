#' @title Fragmentation Metrics for Whole Dataset
#' @description Fragmentation methods to study the transition between two states, e.g.
#' sedentary v.s. active.This function is a whole dataset wrapper for \code{fragmentation}
#'
#' @param count.data \code{data.frame} of dimension n*1442 containing the 1440 minute activity data for all n subject days.
#' The first two columns have to be ID and Day.
#' @param weartime \code{data.frame} with dimension of \code{count.data}.
#' The first two columns have to be ID and Day.
#'
#' @param thresh threshold to binarize the data.
#' @param bout.length minimum duration of defining an active bout; defaults to 1.
#' @param metrics What is the fragmentation metrics to exract. Can be
#' "mean_bout","TP","Gini","power","hazard",or all the above metrics "all".
#' @param by Determine whether fragmentation is calcualted by day or by subjects (i.e. aggregate bouts across days).
#' by-subject is recommended to gain more power.
#'
#'
#'
#'
#' @return A dataframe with some of the following columns
#' \item{ID}{identifier of the person}
#' \item{Day}{indicator of which day of activity it is, can be a numeric vector of sequence 1,2,... or a string of date}
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
#'
#' @importFrom stats na.omit reshape
#' @importFrom dplyr group_by  %>%
#' @importFrom dplyr do as_data_frame filter
#' @importFrom accelerometry accel.bouts rle2
#' @importFrom survival survfit Surv
#' @importFrom ineq Gini
#'
#' @export
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
#' count = example_activity_data$count
#' wear = example_activity_data$wear
#' frag_by_subject = fragmentation_long(count.data = count,
#' weartime = wear,thresh = 100,bout.length = 1,
#' metrics = "all",by = "subject")
#' frag_by_day = fragmentation_long(count.data = count,
#' weartime = wear,thresh = 100,bout.length = 1,
#' metrics = "all",by = "day")
#'
#'




fragmentation_long = function(
  count.data,
  weartime,
  thresh,
  bout.length,
  metrics = c("mean_bout","TP","Gini","power","hazard","all"),
  by = c("day","subject")
){
  metrics = match.arg(metrics)
  by = match.arg(by)


  if(missing(weartime)){
    print("No weartime supplied, calculated based on defualt from 05:00 to 23:00")
    weartime = wear_flag(count.data =  count.data)
  }


  if(by == "day"){
    mat = cbind(as.matrix(count.data[,-c(1:2)]),as.matrix(weartime[,-c(1:2)]))

    result.list =  apply(mat,1,function(x){
      fragmentation(x[1:1440],x[1441:2880],thresh = thresh,bout.length = bout.length, metrics = metrics)
    })

    vfrag = unlist(result.list)

    if(metrics == "all"){
      frag_all = as.data.frame(cbind(count.data[,c(1,2)],
                                     vfrag[seq(1,length(vfrag),10)],
                                     vfrag[seq(2,length(vfrag),10)],
                                     vfrag[seq(3,length(vfrag),10)],
                                     vfrag[seq(4,length(vfrag),10)],
                                     vfrag[seq(5,length(vfrag),10)],
                                     vfrag[seq(6,length(vfrag),10)],
                                     vfrag[seq(7,length(vfrag),10)],
                                     vfrag[seq(8,length(vfrag),10)],
                                     vfrag[seq(9,length(vfrag),10)],
                                     vfrag[seq(10,length(vfrag),10)]))
    }

    if(metrics != "all"){
      frag_all = as.data.frame(cbind(count.data[,c(1,2)],
                                     vfrag[seq(1,length(vfrag),2)],
                                     vfrag[seq(2,length(vfrag),2)]))
    }

    if(metrics == "mean_bout"){
      names(frag_all) = c("ID","Day","mean_r","mean_a")
    }

    if(metrics == "TP"){
      names(frag_all) = c("ID","Day","SATP","ASTP")
    }

    if(metrics == "Gini"){
      names(frag_all) = c("ID","Day","Gini_r","Gini_a")
    }


    if(metrics == "power"){
      names(frag_all) = c("ID","Day","alpha_r","alpha_a")
    }

    if(metrics == "hazard"){
      names(frag_all) = c("ID","Day","h_r","h_a")
    }

    if(metrics == "all"){
      names(frag_all) = c("ID","Day","mean_r","mean_a","SATP","ASTP",
                          "Gini_r","Gini_a","alpha_r","alpha_a","h_r","h_a")
    }
  }

  if(by == "subject"){

    long.count = reshape(count.data, varying = names(count.data)[3:1442],direction = "long",
                         timevar = "MIN",idvar = c("ID","Day"),v.names = "values")
    long.count = long.count[
      with(long.count, order(ID, Day,MIN)),
      ]


    long.wear = reshape(weartime, varying = names(count.data)[3:1442],direction = "long",
                         timevar = "MIN",idvar = c("ID","Day"),v.names = "values")
    long.wear= long.wear[
      with(long.wear, order(ID, Day,MIN)),
      ]


    longdata = data.frame(ID = long.count$ID, count = long.count$values, wear = long.wear$values)

    result= longdata  %>% group_by(ID) %>% do(out = fragmentation(.$count,.$wear,thresh = thresh,
     bout.length = bout.length, metrics = metrics))

    idlist = result$ID
    result.list = result$out

    vfrag = unlist(result.list)

    if(metrics == "all"){
      frag_all = as.data.frame(cbind(idlist,
                                     vfrag[seq(1,length(vfrag),10)],
                                     vfrag[seq(2,length(vfrag),10)],
                                     vfrag[seq(3,length(vfrag),10)],
                                     vfrag[seq(4,length(vfrag),10)],
                                     vfrag[seq(5,length(vfrag),10)],
                                     vfrag[seq(6,length(vfrag),10)],
                                     vfrag[seq(7,length(vfrag),10)],
                                     vfrag[seq(8,length(vfrag),10)],
                                     vfrag[seq(9,length(vfrag),10)],
                                     vfrag[seq(10,length(vfrag),10)]))
    }
    if(metrics != "all"){
      frag_all = as.data.frame(cbind(idlist,
                                     vfrag[seq(1,length(vfrag),2)],
                                     vfrag[seq(2,length(vfrag),2)]))
    }



    if(metrics == "mean_bout"){
      names(frag_all) = c("ID","mean_r","mean_a")
    }

    if(metrics == "TP"){
      names(frag_all) = c("ID","SATP","ASTP")
    }

    if(metrics == "Gini"){
      names(frag_all) = c("ID","Gini_r","Gini_a")
    }


    if(metrics == "power"){
      names(frag_all) = c("ID","alpha_r","alpha_a")
    }

    if(metrics == "hazard"){
      names(frag_all) = c("ID","h_r","h_a")
    }

    if(metrics == "all"){
      names(frag_all) = c("ID", "mean_r","mean_a","SATP","ASTP",
                          "Gini_r","Gini_a","alpha_r","alpha_a","h_r","h_a")
    }

    row.names(frag_all) = c(1:length(idlist))
  }

  return(frag_all)

}
