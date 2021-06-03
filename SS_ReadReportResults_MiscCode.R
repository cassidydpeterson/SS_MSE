

##### FILTER RESULTS FOR COLLAPSED STOCKS ???  #########  ???  ######### 
# Try to re-run with upper bound on catch and see how results pan out for Loh (just HCR1 for now... )
for(HCR in list("HCR_1", "HCR_2", "HCR_3", "HCR_4", "HCR_5", "HCR_6", "HCR_7", "HCR_8", "HCR_9", "HCR_10", "HCR_11", "HCR_12", 
                "HCR_13", "HCR_14", "HCR_15", "HCR_16", "HCR_17", "HCR_18", "HCR_19", "HCR_20", "HCR_21", "HCR_22", "HCR_23", "HCR_24") ){
  bb = OM_Loh[[HCR]]$SSB
  iters_col = which(bb[nrow(bb),]<1)
  # bb_collapsed = bb[,which(bb[nrow(bb),]<1)]
  #   names(bb)[iters_col]
  #   print(HCR)
  #   print( names(bb)[iters_col] )
  #   
  # }
  
  yr_col = c()
  for(i in iters_col){
    yr_col = c(yr_col, min(which(bb[,i]<1)) )
  }
  
  # cbind(iters_col, yr_col)
  View(OM_Loh[[HCR]]$FM_FMMSY[,which(bb[nrow(bb),]<1)])
  View(OM_Loh[[HCR]]$Com_catch[,which(bb[nrow(bb),]<1)])
  
  for(j in 1:length(iters_col)){
    OM_Loh[[HCR]]$FM_FMMSY[(yr_col[j]:nrow(bb)),iters_col[j]] = rep(NA, length=length(OM_Loh[[HCR]]$FM_FMMSY[(yr_col[j]:nrow(bb)),iters_col[j]]) )
    OM_Loh[[HCR]]$Com_catch[(yr_col[j]:nrow(bb)),iters_col[j]] = rep(NA, length=length(OM_Loh[[HCR]]$FM_FMMSY[(yr_col[j]:nrow(bb)),iters_col[j]]) )
    
  }
  
  
} # END HCR Loop

bb = OM_Loh$HCR_1$SSB
list=which(bb[nrow(bb),]<1)
bb_collapse = bb[,which(bb[nrow(bb),]<1)]
View(round(bb_collapse))




### FOR Base_MexRec #######
# Try to re-run with upper bound on catch and see how results pan out for Loh (just HCR1 for now... )
for(HCR in list("HCR_1", "HCR_2", "HCR_3", "HCR_4", "HCR_5", "HCR_6", "HCR_7", "HCR_8", "HCR_9", "HCR_10", "HCR_11", "HCR_12", 
                "HCR_13", "HCR_14", "HCR_15", "HCR_16", "HCR_17", "HCR_18", "HCR_19", "HCR_20", "HCR_21", "HCR_22", "HCR_23", "HCR_24") ){
  # bb = OM_BASE_MexRec[[HCR]]$FM_FMMSY
  # bb = OM_BASE_MexRec[[HCR]]$SSB_SSBMSY
  # # bb = OM_BASE_MexRec[[HCR]]$Com_catch
  # # iters_col = which(bb[nrow(bb),]>2)
  # iters_col = which(bb[nrow(bb),]<0.6)
  # bb_collapsed = bb[,which(bb[nrow(bb),]>20)]
  #   names(bb)[iters_col]
  
  bb = OM_BASE_MexRec[[HCR]]$Tot_catch
  bb= subset(bb, rownames(bb)>2015)
  iters_col = which(apply(bb, 2, max)>1000)
  
  print(HCR)
  print( names(bb)[iters_col] )
  
}


HCR="HCR_3"
bb = OM_BASE_MexRec[[HCR]]$Com_catch
iters_col = which(bb[nrow(bb),]>300)

# cbind(iters_col, yr_col)
View(OM_BASE_MexRec[[HCR]]$FM_FMMSY[,iters_col])
View(OM_BASE_MexRec[[HCR]]$Com_catch[,iters_col])






### FOR Loh_MexRec
# Try to re-run with upper bound on catch and see how results pan out for Loh (just HCR1 for now... )
for(HCR in list("HCR_1", "HCR_2", "HCR_3", "HCR_4", "HCR_5", "HCR_6", "HCR_7", "HCR_8", "HCR_9", "HCR_10", "HCR_11", "HCR_12", 
                "HCR_13", "HCR_14", "HCR_15", "HCR_16", "HCR_17", "HCR_18", "HCR_19", "HCR_20", "HCR_21", "HCR_22", "HCR_23", "HCR_24") ){
  # bb = OM_Loh_MexRec[[HCR]]$FM_FMMSY
  # bb = OM_Loh_MexRec[[HCR]]$Tot_catch
  # iters_col = which(bb[nrow(bb),]>200)
  # bb = OM_Loh_MexRec[[HCR]]$SSB_SSBMSY
  # iters_col = which(bb[nrow(bb),]<0.4)
  # bb_collapsed = bb[,which(bb[nrow(bb),]>20)]
  #   names(bb)[iters_col]
  
  bb = OM_Loh_MexRec[[HCR]]$Tot_catch
  bb= subset(bb, rownames(bb)>2015)
  iters_col = which(apply(bb, 2, max)>1000)
  
  print(HCR)
  print( names(bb)[iters_col] )
  
}

HCR="HCR_3"
bb = OM_BASE_MexRec[[HCR]]$Com_catch
iters_col = which(bb[nrow(bb),]>300)

# cbind(iters_col, yr_col)
View(OM_BASE_MexRec[[HCR]]$FM_FMMSY[,iters_col])
View(OM_BASE_MexRec[[HCR]]$Com_catch[,iters_col])

# for(j in 1:length(iters_col)){
#   OM_BASE_MexRec[[HCR]]$FM_FMMSY[(yr_col[j]:nrow(bb)),iters_col[j]] = rep(NA, length=length(OM_BASE_MexRec[[HCR]]$FM_FMMSY[(yr_col[j]:nrow(bb)),iters_col[j]]) )
#   OM_BASE_MexRec[[HCR]]$Com_catch[(yr_col[j]:nrow(bb)),iters_col[j]] = rep(NA, length=length(OM_BASE_MexRec[[HCR]]$FM_FMMSY[(yr_col[j]:nrow(bb)),iters_col[j]]) )
#   
# }




bb = OM_Loh$HCR_1$SSB
list=which(bb[nrow(bb),]<1)
bb_collapse = bb[,which(bb[nrow(bb),]<1)]
View(round(bb_collapse))




### FOR Loh_LoMexRec
# Try to re-run with upper bound on catch and see how results pan out for Loh (just HCR1 for now... )
for(HCR in list("HCR_1", "HCR_2", "HCR_3", "HCR_4", "HCR_5", "HCR_6", "HCR_7", "HCR_8", "HCR_9", "HCR_10", "HCR_11", "HCR_12", 
                "HCR_13", "HCR_14", "HCR_15", "HCR_16", "HCR_17", "HCR_18", "HCR_19", "HCR_20", "HCR_21", "HCR_22", "HCR_23", "HCR_24") ){
  # bb = OM_Loh_MexRec[[HCR]]$FM_FMMSY
  # bb = OM_Loh_MexRec[[HCR]]$Tot_catch
  # iters_col = which(bb[nrow(bb),]>200)
  # bb = OM_Loh_MexRec[[HCR]]$SSB_SSBMSY
  # iters_col = which(bb[nrow(bb),]<0.4)
  # bb_collapsed = bb[,which(bb[nrow(bb),]>20)]
  #   names(bb)[iters_col]
  
  bb = OM_Loh_LoMexRec[[HCR]]$Tot_catch
  bb= subset(bb, rownames(bb)>2085)
  iters_col = which(apply(bb, 2, max)>600)
  
  print(HCR)
  print( names(bb)[iters_col] )
  
}

tail(OM_Loh_LoMexRec$HCR_16$Com_catch, 10)







### FOR lnR0_MexRec
# Try to re-run with upper bound on catch and see how results pan out for Loh (just HCR1 for now... )
for(HCR in list("HCR_1", "HCR_2", "HCR_3", "HCR_4", "HCR_5", "HCR_6", "HCR_7", "HCR_8", "HCR_9", "HCR_10", "HCR_11", "HCR_12", 
                "HCR_13", "HCR_14", "HCR_15", "HCR_16", "HCR_17", "HCR_18", "HCR_19", "HCR_20", "HCR_21", "HCR_22", "HCR_23", "HCR_24") ){
  # bb = OM_lnR0_MexRec[[HCR]]$FM_FMMSY
  bb = OM_lnR0_MexRec[[HCR]]$SSB_SSBMSY
  # bb = OM_lnR0_MexRec[[HCR]]$Tot_catch
  # iters_col = which(bb[nrow(bb),]>2)
  iters_col = which(bb[nrow(bb),]<0.6)
  # iters_col = which(bb[nrow(bb),]>600)
  # bb_collapsed = bb[,which(bb[nrow(bb),]>20)]
  #   names(bb)[iters_col]
  
  bb = OM_lnR0_MexRec[[HCR]]$Tot_catch
  bb= subset(bb, rownames(bb)>2015)
  iters_col = which(apply(bb, 2, max)>2000)
  
  print(HCR)
  print( names(bb)[iters_col] )
  
}


OM_lnR0_MexRec$HCR_4$Tot_catch[,"201"]






### FOR M_BH_MexRec
# Try to re-run with upper bound on catch and see how results pan out for Loh (just HCR1 for now... )
for(HCR in list("HCR_1", "HCR_2", "HCR_3", "HCR_4", "HCR_5", "HCR_6", "HCR_7", "HCR_8", "HCR_9", "HCR_10", "HCR_11", "HCR_12", "HCR_13", "HCR_14", "HCR_15", "HCR_16", "HCR_17", "HCR_18", "HCR_19", "HCR_20", "HCR_21", "HCR_22", "HCR_23", "HCR_24") ){
  # bb = OM_Loh_MexRec[[HCR]]$FM_FMMSY
  # bb = OM_Loh_MexRec[[HCR]]$Tot_catch
  # iters_col = which(bb[nrow(bb),]>200)
  # bb = OM_M_BH_MexRec[[HCR]]$SSB_SSBMSY
  # iters_col = which(apply(bb, 2, min)<0.6)
  bb = OM_M_BH_MexRec[[HCR]]$Tot_catch
  bb= subset(bb, rownames(bb)>2015)
  iters_col = which(apply(bb, 2, max)>2000)
  # iters_col = which(bb[nrow(bb),]<0.6)
  # bb_collapsed = bb[,which(bb[nrow(bb),]>20)]
  #   names(bb)[iters_col]
  print(HCR)
  print( names(bb)[iters_col] )
  
}
cbind(as.numeric(rownames(OM_M_BH_MexRec[["HCR_9"]]$Tot_catch)),OM_M_BH_MexRec[["HCR_9"]]$Tot_catch$"121")


### FOR M_BH
# Try to re-run with upper bound on catch and see how results pan out for Loh (just HCR1 for now... )
for(HCR in list("HCR_1", "HCR_2", "HCR_3", "HCR_4", "HCR_5", "HCR_6", "HCR_7", "HCR_8", "HCR_9", "HCR_10", "HCR_11", "HCR_12", "HCR_13", "HCR_14", "HCR_15", "HCR_16", "HCR_17", "HCR_18", "HCR_19", "HCR_20", "HCR_21", "HCR_22", "HCR_23", "HCR_24") ){
  # bb = OM_M_BH[[HCR]]$FM_FMMSY
  # bb = OM_M_BH[[HCR]]$Tot_catch
  # iters_col = which(bb[nrow(bb),]>200)
  bb = OM_M_BH[[HCR]]$SSB_SSBMSY
  iters_col = which(apply(bb, 2, min)<0.3)
  
  # bb = OM_M_BH[[HCR]]$Tot_catch
  # bb= subset(bb, rownames(bb)>2015)
  # iters_col = which(apply(bb, 2, max)>2000)
  
  # iters_col = which(bb[nrow(bb),]<0.6)
  # bb_collapsed = bb[,which(bb[nrow(bb),]>20)]
  #   names(bb)[iters_col]
  print(HCR)
  print( names(bb)[iters_col] )
  
}
cbind(as.numeric(rownames(OM_M_BH[["HCR_9"]]$Tot_catch)),OM_M_BH[["HCR_9"]]$Tot_catch$"121")



### END ----------------------------------------------------------------------------------------------------------



######### NOTES ##########


# apply(OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY, 1, median)[length(years)] 
# apply(OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY, 1, median)[length(years)] 
# mean( apply(OM_Plot[[paste0("HCR_",h)]]$Com_catch, 1, median)[(length(years)-4):length(years)] ) 
# AAV_HCR[h] 
# mean( apply(OM_Plot[[paste0("HCR_",h)]]$AvgLen_F, 1, median)[(length(years)-4):length(years)] ) 







iters = seq(47,245, by=2)
years = 1960:2115
col_list = c(rgb(0.75, 0.75, 0.75, 0.20),rgb(0.33, 0.80, 0.92, 0.15),rgb(0.79, 0.9, 0.44, 0.3),rgb(0.86, 0.44, 0.84, 0.2))
col_list2 = c("black","deepskyblue3","forestgreen","darkorchid")
lty_list = c(2,1,2,1)

col_lista = rep(col_list, (24/4))
col_list2a = rep(col_list2, (24/4))
lty_lista = rep(lty_list, (24/4))

## Worm Plots ##

# col2rgb(col="skyblue")/255
# col2rgb(col="darkolivegreen1")/255
# col2rgb(col="orchid")/255
# col2rgb(col="grey")/255
# rgb(0, 0.75, 1, 0.2)

# SSB/SSBMSY
par(mfrow=c(2,3))
for(k in 1:(24/4)){
  kh = k*4
  hlist=c(kh, kh-1, kh-2, kh-3)
  plot(years, OM_Plot$HCR_1$SSB_SSBMSY[,1], type='l', col="white", ylim=c(0, 3))
  abline(h=1)
  for(h in hlist){
    for(i in 1:length(iters)){
      lines(years, OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY[,i], col=col_lista[h])
    }
  }
  for(h in hlist){
    lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY, 1, median), type='l', lwd=2, col=col_list2a[h], lty=lty_lista[h])
  }
}
# plot(years, OM_Plot$HCR_1$SSB_SSBMSY[,1], type='l', col="white", ylim=c(0, 3))
# abline(h=1)


# F/FMSY
par(mfrow=c(2,3))
col_lista = rep(col_list, (24/4))
col_list2a = rep(col_list2, (24/4))
lty_lista = rep(lty_list, (24/4))
for(k in 1:(24/4)){
  kh = k*4
  hlist=c(kh, kh-1, kh-2, kh-3)
  plot(years, OM_Plot$HCR_1$FM_FMMSY[,1], type='l', col="white", ylim=c(0, 10))
  abline(h=1)
  for(h in hlist){
    for(i in 1:length(iters)){
      lines(years, OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY[,i], col=col_lista[h])
    }
  }
  abline(h=1)
  for(h in hlist){
    lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY, 1, median), type='l', lwd=2, col=col_list2a[h], lty=lty_lista[h])
  }
}
# plot(years, OM_Plot$HCR_1$FM_FMMSY[,1], type='l', col="white", ylim=c(0, 10))
# abline(h=1)
# 
# for(h in 4:1){
#   for(i in 1:length(iters)){
#     lines(years, OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY[,i], col=col_list[h])
#   }
# }
# abline(h=1)
# for(h in 4:1){
#   lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY, 1, median), type='l', lwd=2, col=col_list2[h], lty=lty_list[h])
# }



# Commercial Catch
par(mfrow=c(2,3))
col_lista = rep(col_list, (24/4))
col_list2a = rep(col_list2, (24/4))
lty_lista = rep(lty_list, (24/4))
for(k in 1:(24/4)){
  kh = k*4
  hlist=c(kh, kh-1, kh-2, kh-3)
  plot(years, OM_Plot$HCR_1$Com_catch[,1], type='l', col="white", ylim=c(0, 1000))
  abline(h=1)
  for(h in hlist){
    for(i in 1:length(iters)){
      lines(years, OM_Plot[[paste0("HCR_",h)]]$Com_catch[,i], col=col_lista[h])
    }
  }
  for(h in hlist){
    lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$Com_catch, 1, median), type='l', lwd=2, col=col_list2a[h], lty=lty_lista[h])
  }
}
# plot(years, OM_Plot$HCR_1$Com_catch[,1], type='l', col="white", ylim=c(0, 1000))
# 
# for(h in 4:1){
#   for(i in 1:length(iters)){
#     lines(years, OM_Plot[[paste0("HCR_",h)]]$Com_catch[,i], col=col_list[h])
#   }
# }
# for(h in 4:1){
#   lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$Com_catch, 1, median), type='l', lwd=2, col=col_list2[h], lty=lty_list[h])
# }


# SSB/SSB0
par(mfrow=c(2,3))
for(k in 1:(24/4)){
  kh = k*4
  hlist=c(kh, kh-1, kh-2, kh-3)
  plot(years, OM_Plot$HCR_1$SSB_SSB0[,1], type='l', col="white", ylim=c(0, 1.1))
  abline(h=1)
  for(h in hlist){
    for(i in 1:length(iters)){
      lines(years, OM_Plot[[paste0("HCR_",h)]]$SSB_SSB0[,i], col=col_lista[h])
    }
  }
  for(h in hlist){
    lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$SSB_SSB0, 1, median), type='l', lwd=2, col=col_list2a[h], lty=lty_lista[h])
  }
}
# plot(years, OM_Plot$HCR_1$SSB_SSB0[,1], type='l', col="white")
# abline(h=1)
# 
# for(h in 4:1){
#   for(i in 1:length(iters)){
#     lines(years, OM_Plot[[paste0("HCR_",h)]]$SSB_SSB0[,i], col=col_list[h])
#   }
# }
# for(h in 4:1){
#   lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$SSB_SSB0, 1, median), type='l', lwd=2, col=col_list2[h], lty=lty_list[h])
# }


# avg length 
par(mfrow=c(2,3))
for(k in 1:(24/4)){
  kh = k*4
  hlist=c(kh, kh-1, kh-2, kh-3)
  plot(years, OM_Plot$HCR_1$AvgLen_F[,1], type='l', col="white", ylim=c(85, 120))
  abline(h=1)
  for(h in hlist){
    for(i in 1:length(iters)){
      lines(years, OM_Plot[[paste0("HCR_",h)]]$AvgLen_F[,i], col=col_lista[h])
    }
  }
  for(h in hlist){
    lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$AvgLen_F, 1, median), type='l', lwd=2, col=col_list2a[h], lty=lty_lista[h])
  }
}
# plot(years, OM_Plot$HCR_1$AvgLen_F[,1], type='l', col="white", ylim=c(85, 120))
# abline(h=1)
# 
# for(h in 4:1){
#   for(i in 1:length(iters)){
#     lines(years, OM_Plot[[paste0("HCR_",h)]]$AvgLen_F[,i], col=col_list[h])
#   }
# }
# for(h in 4:1){
#   lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$AvgLen_F, 1, median), type='l', lwd=2, col=col_list2[h], lty=lty_list[h])
# }


# h=1
# OM_Plot$paste0("HCR_",h)$SSB_SSBMSY[,1]
# 
# OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY[,1]








## RADAR PLOTS ##
library(fmsb)




# FOR HCR 1-4:
h=1:24
h2= c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4), rep(5, 4), rep(6, 4) )
# h=1

data1 = matrix(c(1, 3, 300, 1, 120, 0, 0, 0, 0, 80), ncol=5, byrow=T)
data2 = matrix(c(1, 3, 300, 1, 120, 0, 0, 0, 0, 80), ncol=5, byrow=T)
data3 = matrix(c(1, 3, 300, 1, 120, 0, 0, 0, 0, 80), ncol=5, byrow=T)
data4 = matrix(c(1, 3, 300, 1, 120, 0, 0, 0, 0, 80), ncol=5, byrow=T)
data5 = matrix(c(1, 3, 300, 1, 120, 0, 0, 0, 0, 80), ncol=5, byrow=T)
data6 = matrix(c(1, 3, 300, 1, 120, 0, 0, 0, 0, 80), ncol=5, byrow=T)
for(h in h){
  if(h2[h]==1){
    data1 = rbind(data1, c(
      apply(OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY, 1, median)[length(years)] ,
      apply(OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY, 1, median)[length(years)] ,
      mean( apply(OM_Plot[[paste0("HCR_",h)]]$Com_catch, 1, median)[(length(years)-4):length(years)] ) ,
      AAV_HCR[h] ,
      mean( apply(OM_Plot[[paste0("HCR_",h)]]$AvgLen_F, 1, median)[(length(years)-4):length(years)] ) 
    ) )
  } # end if h2=1
  
  if(h2[h]==2){
    data2 = rbind(data2, c(
      apply(OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY, 1, median)[length(years)] ,
      apply(OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY, 1, median)[length(years)] ,
      mean( apply(OM_Plot[[paste0("HCR_",h)]]$Com_catch, 1, median)[(length(years)-4):length(years)] ) ,
      AAV_HCR[h] ,
      mean( apply(OM_Plot[[paste0("HCR_",h)]]$AvgLen_F, 1, median)[(length(years)-4):length(years)] ) 
    ) )
  } # end if h2=2
  
  if(h2[h]==3){
    data3 = rbind(data3, c(
      apply(OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY, 1, median)[length(years)] ,
      apply(OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY, 1, median)[length(years)] ,
      mean( apply(OM_Plot[[paste0("HCR_",h)]]$Com_catch, 1, median)[(length(years)-4):length(years)] ) ,
      AAV_HCR[h] ,
      mean( apply(OM_Plot[[paste0("HCR_",h)]]$AvgLen_F, 1, median)[(length(years)-4):length(years)] ) 
    ) )
  } # end if h2=3
  
  if(h2[h]==4){
    data4 = rbind(data4, c(
      apply(OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY, 1, median)[length(years)] ,
      apply(OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY, 1, median)[length(years)] ,
      mean( apply(OM_Plot[[paste0("HCR_",h)]]$Com_catch, 1, median)[(length(years)-4):length(years)] ) ,
      AAV_HCR[h] ,
      mean( apply(OM_Plot[[paste0("HCR_",h)]]$AvgLen_F, 1, median)[(length(years)-4):length(years)] ) 
    ) )
  } # end if h2=4
  
  if(h2[h]==5){
    data5 = rbind(data5, c(
      apply(OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY, 1, median)[length(years)] ,
      apply(OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY, 1, median)[length(years)] ,
      mean( apply(OM_Plot[[paste0("HCR_",h)]]$Com_catch, 1, median)[(length(years)-4):length(years)] ) ,
      AAV_HCR[h] ,
      mean( apply(OM_Plot[[paste0("HCR_",h)]]$AvgLen_F, 1, median)[(length(years)-4):length(years)] ) 
    ) )
  } # end if h2=5
  
  if(h2[h]==6){
    data6 = rbind(data6, c(
      apply(OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY, 1, median)[length(years)] ,
      apply(OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY, 1, median)[length(years)] ,
      mean( apply(OM_Plot[[paste0("HCR_",h)]]$Com_catch, 1, median)[(length(years)-4):length(years)] ) ,
      AAV_HCR[h] ,
      mean( apply(OM_Plot[[paste0("HCR_",h)]]$AvgLen_F, 1, median)[(length(years)-4):length(years)] ) 
    ) )
  } # end if h2=6
  
  
} # end h loop. 

colnames(data1) <- c("SSB2015/SSBMSY","F2015/FMSY", "Com Catch 2110-2115","AAV","Avg Len 2110-2115")
row.names(data1) <- c("max",'min','HCR1','HCR2','HCR3','HCR4')
data1 = as.data.frame(data1)
colnames(data2) <- c("SSB2015/SSBMSY","F2015/FMSY", "Com Catch 2110-2115","AAV","Avg Len 2110-2115")
row.names(data2) <- c("max",'min','HCR5','HCR6','HCR7','HCR8')
data2 = as.data.frame(data2)
colnames(data3) <- c("SSB2015/SSBMSY","F2015/FMSY", "Com Catch 2110-2115","AAV","Avg Len 2110-2115")
row.names(data3) <- c("max",'min','HCR9','HCR10','HCR11','HCR12')
data3 = as.data.frame(data3)
colnames(data4) <- c("SSB2015/SSBMSY","F2015/FMSY", "Com Catch 2110-2115","AAV","Avg Len 2110-2115")
row.names(data4) <- c("max",'min','HCR13','HCR14','HCR15','HCR16')
data4 = as.data.frame(data4)
colnames(data5) <- c("SSB2015/SSBMSY","F2015/FMSY", "Com Catch 2110-2115","AAV","Avg Len 2110-2115")
row.names(data5) <- c("max",'min','HCR17','HCR18','HCR19','HCR20')
data5 = as.data.frame(data5)
colnames(data6) <- c("SSB2015/SSBMSY","F2015/FMSY", "Com Catch 2110-2115","AAV","Avg Len 2110-2115")
row.names(data6) <- c("max",'min','HCR21','HCR22','HCR23','HCR24')
data6 = as.data.frame(data6)

par(mfrow=c(2,3), mar=c(1, 1, 1, 1))
colors_border=c( rgb(0,0,1,1), rgb(1,0,0,1) , rgb(0,1,0,1), rgb(0.627, 0.125, 0.941, 1) )
colors_in=c( rgb(0,0,1,0.2), rgb(1,0,0,0.2) , rgb(0,1,0,0.2) , rgb(0.627, 0.125, 0.941, 0.2))
plty_list=c(1, 2, 1, 2)
for(d in list(data1, data2, data3, data4, data5, data6)){
  radarchart( d  , axistype=1 ,
              #custom polygon
              pcol=colors_border , pfcol=colors_in , plwd=2 , plty=plty_list, #plty is line type of polygon
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
              #custom labels
              vlcex=1
  )
}














## Violin Plots ##
library(vioplot)


## Get SSB/SSBMSY 2115 ##
par(mfrow=c(2,3), mar=c(1.1, 2.6, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=1)
for(r in seq(1, 24, by=4)){
  vioplot(SSB_SSBMSY_2115[,r], SSB_SSBMSY_2115[,(r+1)], SSB_SSBMSY_2115[,(r+2)], SSB_SSBMSY_2115[,(r+3)], 
          col=c('blue','red','green','purple'), ylim=c(0, 1), names=c(rep("", 4)) )
  if(r==1){ mtext('SSB_2115/SSB_MSY', side=2, line=1.2, cex=1.1) }
  abline(h=1)
}



## Get SSB/SSBMSY 2065 ##
par(mfrow=c(2,3), mar=c(1.1, 2.6, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=1)
for(r in seq(1, 24, by=4)){
  vioplot(SSB_SSBMSY_2065[,r], SSB_SSBMSY_2065[,(r+1)], SSB_SSBMSY_2065[,(r+2)], SSB_SSBMSY_2065[,(r+3)], 
          col=c('blue','red','green','purple'), ylim=c(0, 1))
  abline(h=1)
}


## Get Get F/FBMSY 2115 ##
par(mfrow=c(2,3), mar=c(1.1, 2.6, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=1)
for(r in seq(1, 24, by=4)){
  vioplot(FM_FMMSY_2115[,r], FM_FMMSY_2115[,(r+1)], FM_FMMSY_2115[,(r+2)], FM_FMMSY_2115[,(r+3)], 
          col=c('blue','red','green','purple'), ylim=c(0, 2.5))
  abline(h=1)
}


## Get F/FBMSY 2065 ##
par(mfrow=c(2,3), mar=c(1.1, 2.6, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=1)
for(r in seq(1, 24, by=4)){
  vioplot(FM_FMMSY_2065[,r], FM_FMMSY_2065[,(r+1)], FM_FMMSY_2065[,(r+2)], FM_FMMSY_2065[,(r+3)], 
          col=c('blue','red','green','purple'), ylim=c(0, 6))
  abline(h=1)
}



## Get Avg Catch 2111-2115 ##
par(mfrow=c(2,3), mar=c(1.1, 2.6, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=1)
for(r in seq(1, 24, by=4)){
  vioplot(Com_Catch_2115[,r], Com_Catch_2115[,(r+1)], Com_Catch_2115[,(r+2)], Com_Catch_2115[,(r+3)], 
          col=c('blue','red','green','purple'), ylim=c(0, 1000))
}


## Get Avg Catch 2061-2065 ##
par(mfrow=c(2,3), mar=c(1.1, 2.6, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=1)
for(r in seq(1, 24, by=4)){
  vioplot(Com_Catch_2065[,r], Com_Catch_2065[,(r+1)], Com_Catch_2065[,(r+2)], Com_Catch_2065[,(r+3)], 
          col=c('blue','red','green','purple'), ylim=c(0, 600))
}



# Build data frame (see notes below)
# data <- data.frame()
# # add 2 lines to the dataframe: the max and min of each variable to show on the plot!
# data <- rbind(rep(MAXXX,ncol(data)) , rep(0,ncol(data)) , data)
# 
# #Define colors
# # Color vector
# colors_border=c( rgb(0,0,1,1), rgb(1,0,0,1) , rgb(0,1,0,1) )
# colors_in=c( rgb(0,0,1,0.2), rgb(1,0,0,0.2) , rgb(0,1,0,0.2) )
# 
# # plot with default options:
# radarchart( data  , axistype=1 , 
#             #custom polygon
#             pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1, #plty is line type of polygon
#             #custom the grid
#             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
#             #custom labels
#             vlcex=0.8 
# )

# # RADAR NOTES
# # From: https://www.r-graph-gallery.com/143-spider-chart-with-saveral-individuals.html
# 
# library(fmsb)
# 
# 
# # Create data: note in High school for several students
# set.seed(99)
# data <- as.data.frame(matrix( sample( 0:20 , 21 , replace=F) , ncol=7))
# colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding" ,"X","Y")
# rownames(data) <- paste("mister" , letters[1:3] , sep="-")
# 
# # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
# data <- rbind(rep(20,ncol(data)) , rep(0,ncol(data)) , data)
# 
# # plot with default options:
# radarchart(data)
# 
# # The radarchart() function offers several options to customize the chart:
# #   
# # Polygon features:
# #   pcol ??? line color
# #   pfcol ??? fill color
# #   plwd ??? line width
# # 
# # Grid features:
# #   cglcol ??? color of the net
# #   cglty ??? net line type (see possibilities)
# #   axislabcol ??? color of axis labels
# #   caxislabels ??? vector of axis labels to display
# #   cglwd ??? net width
# # 
# # Labels:
# #   vlcex ??? group labels size
# 
# radarchart(data, cglty=1, cglcol='grey', pcol=c('blue','red','green'), plwd=2, pfcol=c('blue','red','green'), pdensity=c(5, 10, 30))
# 
# col2rgb('blue')
# col2rgb('red')
# col2rgb('green')
# 
# 
# # Color vector
# colors_border=c( rgb(0,0,1,1), rgb(1,0,0,1) , rgb(0,1,0,1) )
# colors_in=c( rgb(0,0,1,0.2), rgb(1,0,0,0.2) , rgb(0,1,0,0.2) )
# radarchart(data, cglty=1, cglcol='grey', pcol=colors_border, plwd=2, pfcol=colors_in, 
#            axistype=1, axislabcol='grey', caxislabels=seq(0,20,5))
# 
# # plot with default options:
# radarchart( data  , axistype=1 , 
#             #custom polygon
#             pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1, #plty is line type of polygon
#             #custom the grid
#             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
#             #custom labels
#             vlcex=0.8 
# )






# # EXAMPLE COPIED FROM SB SIMULATION RESULTS:
# png(filename="D:/vspace1/DFA_Simulation/SB\\Plots\\Depletion_MISS.png",
#     type="cairo",
#     units="mm",
#     width=400,
#     height=200,
#     pointsize=18,
#     res=600)
# par(mfrow=c(3,1), mar=c(1.1, 2.6, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=1)
# vioplot(dep_M1$conflicting / dep_M1$actual, dep_M1$DFA / dep_M1$actual,
#         
#         dep_M9$conflicting / dep_M9$actual, dep_M9$DFA / dep_M9$actual, 
#         dep_M10$conflicting / dep_M10$actual, dep_M10$DFA / dep_M10$actual,
#         dep_M11$conflicting / dep_M11$actual, dep_M11$DFA / dep_M11$actual, 
#         dep_M12$conflicting / dep_M12$actual, dep_M12$DFA / dep_M12$actual, 
#         dep_M13$conflicting / dep_M13$actual, dep_M13$DFA / dep_M13$actual,
#         dep_M14$conflicting / dep_M14$actual, dep_M14$DFA / dep_M14$actual, 
#         dep_M15$conflicting / dep_M15$actual, dep_M15$DFA / dep_M15$actual, 
#         
#         dep_M2$conflicting / dep_M2$actual, dep_M2$DFA / dep_M2$actual, 
#         dep_M3$conflicting / dep_M3$actual, dep_M3$DFA / dep_M3$actual, 
#         dep_M4$conflicting / dep_M4$actual, dep_M4$DFA / dep_M4$actual,
#         dep_M5$conflicting / dep_M5$actual, dep_M5$DFA / dep_M5$actual, 
#         dep_M6$conflicting / dep_M6$actual, dep_M6$DFA / dep_M6$actual, 
#         dep_M7$conflicting / dep_M7$actual, dep_M7$DFA / dep_M7$actual,
#         dep_M8$conflicting / dep_M8$actual, dep_M8$DFA / dep_M8$actual,
#         
#         na.rm=T,
#         col=c('dimgrey', 'grey', rep(c('darkolivegreen4','olivedrab1'), 7), rep(c('deepskyblue4','deepskyblue'), 7)),
#         names=c(rep('',30)) , ylim=c(0.4, 2.5) )
# axis(1, at=seq(length=15, from=1.5, by=2), 
#      c('M1','M2','M3','M4','M5','M6','M7','M8','M9','M10','M11','M12','M13','M14','M15'), tck=0, cex=0.75)
# abline(h=1, lwd=2)
# mtext('Relative Depletion', side=2, line=1.2, cex=1.1)
# mtext("Missing data", side=3, line=-1.1, cex=1.1)
# legend("topright",c("confl indices","DFA trend"), fill=c("dimgrey","grey"),bty='n',
#        border='white', cex=1.0)
# 
# vioplot(dep_M16$conflicting / dep_M16$actual, dep_M16$DFA / dep_M16$actual,
#         dep_M17$conflicting / dep_M17$actual, dep_M17$DFA / dep_M17$actual, 
#         dep_M18$conflicting / dep_M18$actual, dep_M18$DFA / dep_M18$actual, 
#         dep_M19$conflicting / dep_M19$actual, dep_M19$DFA / dep_M19$actual,
#         dep_M20$conflicting / dep_M20$actual, dep_M20$DFA / dep_M20$actual, 
#         dep_M21$conflicting / dep_M21$actual, dep_M21$DFA / dep_M21$actual, 
#         dep_M22$conflicting / dep_M22$actual, dep_M22$DFA / dep_M22$actual,
#         dep_M23$conflicting / dep_M23$actual, dep_M23$DFA / dep_M23$actual, 
#         dep_M24$conflicting / dep_M24$actual, dep_M24$DFA / dep_M24$actual, 
#         dep_M25$conflicting / dep_M25$actual, dep_M25$DFA / dep_M25$actual,
#         dep_M26$conflicting / dep_M26$actual, dep_M26$DFA / dep_M26$actual, 
#         dep_M27$conflicting / dep_M27$actual, dep_M27$DFA / dep_M27$actual, 
#         
#         na.rm=T,
#         col=c(rep(c('darkseagreen4','darkseagreen1'), 3), rep(c('darkslategray4','darkslategray1'), 3), rep(c('orchid3','orchid1'),3),
#               rep(c('darkorchid4','darkorchid1'), 3) ),
#         names=c(rep('',24)) , ylim=c(0.4, 2.5) )
# axis(1, at=seq(length=12, from=1.5, by=2), 
#      c('M16','M17','M18','M19','M20','M21','M22','M23','M24','M25','M26','M27'), tck=0, cex=0.75)
# abline(h=1, lwd=2)
# mtext('Relative Depletion', side=2, line=1.2, cex=1.1)
# # mtext("Missing data", side=3, line=-1.1, cex=1.1)
# 
# 
# vioplot(dep_M28$conflicting / dep_M28$actual, dep_M28$DFA / dep_M28$actual,
#         dep_M29$conflicting / dep_M29$actual, dep_M29$DFA / dep_M29$actual, 
#         dep_M30$conflicting / dep_M30$actual, dep_M30$DFA / dep_M30$actual, 
#         dep_M31$conflicting / dep_M31$actual, dep_M31$DFA / dep_M31$actual, 
#         dep_M32$conflicting / dep_M32$actual, dep_M32$DFA / dep_M32$actual, 
#         dep_M33$conflicting / dep_M33$actual, dep_M33$DFA / dep_M33$actual, 
#         
#         dep_M37$conflicting / dep_M37$actual, dep_M37$DFA / dep_M37$actual, 
#         dep_M38$conflicting / dep_M38$actual, dep_M38$DFA / dep_M38$actual, 
#         dep_M39$conflicting / dep_M39$actual, dep_M39$DFA / dep_M39$actual, 
#         
#         dep_M34$conflicting / dep_M34$actual, dep_M34$DFA / dep_M34$actual, 
#         dep_M35$conflicting / dep_M35$actual, dep_M35$DFA / dep_M35$actual, 
#         dep_M36$conflicting / dep_M36$actual, dep_M36$DFA / dep_M36$actual, 
#         na.rm=T,
#         col=c(rep(c('springgreen4','springgreen'), 3), rep(c('steelblue4','steelblue1'), 3), rep(c('mediumpurple4','mediumpurple1'),3),
#               rep(c('purple4','purple'), 3) ),
#         names=c(rep('',24)) , ylim=c(0.4, 2.5))
# axis(1, at=seq(length=12, from=1.5, by=2), 
#      c('M28','M29','M30','M31','M32','M33','M34','M35','M36','M37','M38','M39'), tck=0, cex=0.75)
# abline(h=1, lwd=2)
# mtext('Relative Depletion', side=2, line=1.2, cex=1.1)
# # mtext("Missing data", side=3, line=-1.1, cex=1.1)
# 
# dev.off()

















########################################################################################################################################################
# NOTES 
########################################################################################################################################################

names(xx)
l = as.numeric(names(xx$natlen[xx$natlen$'Beg/Mid'=="B",13:ncol(xx$natlen)]))
# sapply(l, rep, nrow(xx$natlen))sapply(l, rep, nrow(xx$natlen))
# 
# a = xx$natlen[xx$natlen$'Beg/Mid'=="B",13:ncol(xx$natlen)]
# b = xx$natlen[xx$natlen$'Beg/Mid'=="B",13:ncol(xx$natlen)] * sapply(l, rep, nrow(xx$natlen[xx$natlen$'Beg/Mid'=="B",13:ncol(xx$natlen)]))
avgL = apply(xx$natlen[xx$natlen$'Beg/Mid'=="B",13:ncol(xx$natlen)] * sapply(l, rep, nrow(xx$natlen[xx$natlen$'Beg/Mid'=="B",13:ncol(xx$natlen)])), 1, sum) / 
  apply(xx$natlen[xx$natlen$'Beg/Mid'=="B",13:ncol(xx$natlen)] , 1, sum)
avgL2 = as.data.frame(cbind(xx$natlen[xx$natlen$'Beg/Mid'=="B",]$Yr,xx$natlen[xx$natlen$'Beg/Mid'=="B",]$Sex, avgL))
colnames(avgL2) <- c("Yr","Sex","avgL")
avgL2_F = avgL2[avgL2$Sex==1 & avgL2$Yr<2116,]
avgL2_M = avgL2[avgL2$Sex==2 & avgL2$Yr<2116,]


plot(avgL2_F$avgL, type='l', ylim=c(93, 109))
lines(avgL2_M$avgL)
lines((avgL2_F$avgL+avgL2_M$avgL)/2, col='blue')


SSB_MSY = xx$derived_quants["SSB_MSY","Value"]
SSB = xx$timeseries$SpawnBio[3:(nrow(xx$timeseries)-1)]
Bio_all = xx$timeseries$Bio_all[3:(nrow(xx$timeseries)-1)]
GOM_catch = xx$timeseries$`retain(B):_1`[3:(nrow(xx$timeseries)-1)]
Atl_catch = xx$timeseries$`retain(B):_2`[3:(nrow(xx$timeseries)-1)]
MEX_Rec_catch = xx$timeseries$`retain(B):_3`[3:(nrow(xx$timeseries)-1)]
MEN_bycatch = xx$timeseries$`retain(B):_4`[3:(nrow(xx$timeseries)-1)]







plot(xx$timeseries$SpawnBio~xx$timeseries$Yr, type='l')
lines(yy$timeseries$SpawnBio~yy$timeseries$Yr, type='l', col='blue')
lines(xx2$timeseries$SpawnBio~yy$timeseries$Yr, type='l', col='black', lty=2)
abline(h=mean(c(xx$derived_quants["SSB_MSY","Value"], yy$derived_quants["SSB_MSY","Value"])))

plot(xx$timeseries$`obs_cat:_1`+xx$timeseries$`obs_cat:_2` ~xx$timeseries$Yr, type='l', 
     ylim=c(0, mean(c(xx$derived_quants["SSB_MSY","Value"], yy$derived_quants["SSB_MSY","Value"]))) )
lines(yy$timeseries$`obs_cat:_1`+yy$timeseries$`obs_cat:_2` ~yy$timeseries$Yr, type='l', col='blue')
lines(xx2$timeseries$`obs_cat:_1`+xx2$timeseries$`obs_cat:_2` ~xx2$timeseries$Yr, type='l', lty=2)
lines(xx$timeseries$`obs_cat:_1`+xx$timeseries$`obs_cat:_2`+xx$timeseries$`obs_cat:_3`+xx$timeseries$`obs_cat:_4` ~xx$timeseries$Yr, type='l')
lines(yy$timeseries$`obs_cat:_1`+yy$timeseries$`obs_cat:_2`+yy$timeseries$`obs_cat:_3`+yy$timeseries$`obs_cat:_4` ~yy$timeseries$Yr, type='l', col='blue')
lines(xx2$timeseries$`obs_cat:_1`+xx2$timeseries$`obs_cat:_2`+xx2$timeseries$`obs_cat:_3`+xx2$timeseries$`obs_cat:_4` ~xx2$timeseries$Yr, type='l', lty=2)


abline(h=mean(c(xx$derived_quants["SSB_MSY","Value"], yy$derived_quants["SSB_MSY","Value"])))

#### CALC MEAN & MEDIAN AGE ###
xx$natage[xx$natage$`Beg/Mid`=='M' & xx$natage$Era=="TIME",]
nn = xx$natage[xx$natage$`Beg/Mid`=='M' & xx$natage$Era=="TIME", -c(1:12)]

MeanAge = vector()
MedAge = vector()
for(y in 1:nrow(nn)){
  n1 = vector()
  for(i in 1:ncol(nn)){
    n1 = c(n1, rep(i-1, nn[y,i]) )
  }
  MeanAge = c( MeanAge, mean(n1) )
  MedAge = c( MedAge, median(n1) )
}





# 
# library(r4ss)
# EMdat = SS_readdat("D:\\MSE_Run\\OM_BH\\HCR1\\StoreResults\\EMdata_46.ss_new")
# 
# 
# ll = subset(EMdat$lencomp, EMdat$lencomp$Yr==2000)
# ll2 = ll[,-c(1:6)]
# ll3 = ll2[which(ll2!=0)]
# apply(EMdat$lencomp, 2, median)