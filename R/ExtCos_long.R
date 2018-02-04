ExtCos_long = function(
  count.data,
  longtrasform = TRUE
){
  long.count = reshape(count.data, varying = names(count.data)[3:1442],direction = "long",
                       timevar = "MIN",idvar = c("ID","Day"),v.names = "values")
  long.count = long.count[
    with(long.count, order(ID, Day,MIN)),
    ]
  
  result= long.count  %>% group_by(ID) %>% do(out = ExtCos(.$values,logtransform = logtransform))c
  
  idlist = result$ID
  result.list = result$out
  
  vext= unlist(result.list)
  ext_all = as.data.frame(cbind(idlist,
                                vext[seq(1,length(vext),5)],
                                vext[seq(2,length(vext),5)],
                                vext[seq(3,length(vext),5)],
                                vext[seq(4,length(vext),5)],
                                vext[seq(5,length(vext),5)]))
  names(ext_all) = c("ID","min","amp","alpha","beta","acro")
  row.names(ext_all) = c(1:length(idlist))
  return(ext_all)
}
