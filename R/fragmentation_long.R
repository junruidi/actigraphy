fragmentation_long = function(
  count.data,
  weartime,
  thresh,
  bout.length,
  metrics = c("mean_bout","TP","Gini","power","hazard","all"),
  by = c("day","subject")
){
  metrics = match.arg(metrics)
  by = match.arg(metrics)
  
  
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