library(accelerometry)
library(dplyr)
library(ineq)
library(survival)

fragmentation = function(
  x,
  w,
  thresh,
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
