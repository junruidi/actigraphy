## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE---------------------------------------------------------
#  devtools::install_github("junruidi/actigraphy")

## ------------------------------------------------------------------------
library(actigraphy)

## ---- eval=FALSE---------------------------------------------------------
#  data(example_activity_data)
#  count = example_activity_data$count
#  weartime = wear_flag(count.data = count, start = "06:00", end = "23:00")

## ----fig.height = 4, fig.width = 8---------------------------------------
data(example_activity_data)
count1 = c(t(example_activity_data$count[1,-c(1,2)]))
wear1 = c(t(example_activity_data$wear[1,-c(1,2)]))
id = example_activity_data$count$ID[1]
day= example_activity_data$count$Day[1]
plot_profile(x=count1, w=wear1, title = paste0("ID ",id, ", Day  ", day),cex.main = 1.3,cex.lab = 1.2, cex.xaxis = 1,cex.yaxis = 1,hr = 2)

## ---- eval=FALSE---------------------------------------------------------
#  data(example_activity_data)
#  count = example_activity_data$count
#  wear = example_activity_data$wear
#  tac = Tvol(count.data = count,weartime = wear,logtransform = FALSE)
#  tlac = Tvol(count.data = count,weartime = wear,logtransform = TRUE)

## ----eval=FALSE----------------------------------------------------------
#  data(example_activity_data)
#  count1 = c(t(example_activity_data$count[1,-c(1,2)]))
#  wear1 = c(t(example_activity_data$wear[1,-c(1,2)]))
#  tst = Time(x = count1, w = wear1, thresh = 100,smallerthan = TRUE)
#  tat = Time(x = count1, w = wear1, thresh = 100,smallerthan = FALSE)

## ----eval=FALSE----------------------------------------------------------
#  data(example_activity_data)
#  count = example_activity_data$count
#  wear = example_activity_data$wear
#  sed_all = Time_long(count.data = count,weartime = wear,
#  thresh = 100,smallerthan = TRUE)

## ----eval=FALSE----------------------------------------------------------
#  data(example_activity_data)
#  count1 = c(t(example_activity_data$count[1,-c(1,2)]))
#  wear1 = c(t(example_activity_data$wear[1,-c(1,2)]))
#  mb = fragmentation(x = count1, w = wear1, thresh = 100, metrics = "mean_bout",bout.length = 1)
#  tp = fragmentation(x = count1, w = wear1, thresh = 100, metrics = "TP",bout.length = 1)

## ----eval=FALSE----------------------------------------------------------
#  data(example_activity_data)
#  count = example_activity_data$count
#  wear = example_activity_data$wear
#  frag_by_subject = fragmentation_long(count.data = count, weartime = wear,thresh = 100, metrics = "all",by = "subject",bout.length = 1)
#  frag_by_day = fragmentation_long(count.data = count, weartime = wear,thresh = 100, metrics = "all",by = "day",bout.length = 1)

## ---- eval=FALSE---------------------------------------------------------
#  count.days.simu = rpois(1440*5, lambda = 5)
#  extcos = ExtCos(x = count.days.simu, logtransform  = TRUE)
#  
#  data(example_activity_data)
#  count.data = example_activity_data$count
#  extcos = ExtCos_long(count.data = count.data, logtransform  = TRUE)
#  

## ----eval=FALSE----------------------------------------------------------
#  data(example_activity_data)
#  count1 = c(t(example_activity_data$count[1,-c(1,2)]))
#  iv = IV(x = count1, level = "hour")

## ----eval=FALSE----------------------------------------------------------
#  data(example_activity_data)
#  count.data = example_activity_data$count
#  iv_all = IV_long(count.data = count.data, level = "hour")

## ----eval=FALSE----------------------------------------------------------
#  data(example_activity_data)
#  count.data = example_activity_data$count
#  fpca = crfpca(count.data = count, knots = 20, pve = 0.9, logtransform  = TRUE)
#  scores = fpca$pcs
#  eignfunction = fpca$phi

