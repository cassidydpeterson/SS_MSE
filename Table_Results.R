###########################
### COllate MSE Results ###
### Oct 2020 ##############
###########################


Results = c('Results_BASE', 'Results_BH', 'Results_Hih', 'Results_Loh', 'Results_lnR0', 'Results_M_BH')


BuildTable <- function(Results = c('Results_BASE', 'Results_BH', 'Results_Hih', 'Results_Loh', 'Results_lnR0', 'Results_M_BH'), stat = "median"){
  ### AAV ###
  # AAV = rbind(Results_BASE$AAV_HCR, Results_BH$AAV_HCR, Results_Hih$AAV_HCR, Results_Loh$AAV_HCR, Results_lnR0$AAV_HCR, Results_M_BH$AAV_HCR) 
  # AAV
  AAV = get(Results[1])$AAV_HCR
  for(r in 2:length(Results)){
    AAV <- rbind(AAV, get(Results[r])$AAV_HCR)
  }
  

  
  
  ### Tot commercial catch ###
  # TotComCatch = rbind(Results_BASE$Com_Catch_cumulative, Results_BH$Com_Catch_cumulative, Results_Hih$Com_Catch_cumulative, Results_Loh$Com_Catch_cumulative, Results_lnR0$Com_Catch_cumulative, Results_M_BH$Com_Catch_cumulative) 
  # TotComCatch
  TotComCatch = get(Results[1])$Com_Catch_cumulative
  for(r in 2:length(Results)){
    TotComCatch <- rbind(TotComCatch, get(Results[r])$Com_Catch_cumulative)
  }
  
  
  
  
  ### SSB/SSBMSY ###
  # SSB_SSBMSY_2115 = rbind(Results_BASE$SSB_SSBMSY_2115, Results_BH$SSB_SSBMSY_2115, Results_Hih$SSB_SSBMSY_2115, Results_Loh$SSB_SSBMSY_2115, Results_lnR0$SSB_SSBMSY_2115, Results_M_BH$SSB_SSBMSY_2115) 
  # dim(SSB_SSBMSY_2115)
  SSB_SSBMSY_2115 = get(Results[1])$SSB_SSBMSY_2115
  for(r in 2:length(Results)){
    SSB_SSBMSY_2115 <- rbind(SSB_SSBMSY_2115, get(Results[r])$SSB_SSBMSY_2115)
  }
  
  # SSB_SSBMSY_2065 = rbind(Results_BASE$SSB_SSBMSY_2065, Results_BH$SSB_SSBMSY_2065, Results_Hih$SSB_SSBMSY_2065, Results_Loh$SSB_SSBMSY_2065, Results_lnR0$SSB_SSBMSY_2065, Results_M_BH$SSB_SSBMSY_2065) 
  # dim(SSB_SSBMSY_2065)
  SSB_SSBMSY_2065 = get(Results[1])$SSB_SSBMSY_2065
  for(r in 2:length(Results)){
    SSB_SSBMSY_2065 <- rbind(SSB_SSBMSY_2065, get(Results[r])$SSB_SSBMSY_2065)
  }
  
  
  
  
  ### F/FMSY ##
  # F_FMSY_2115 = rbind(Results_BASE$FM_FMMSY_2115, Results_BH$FM_FMMSY_2115, Results_Hih$FM_FMMSY_2115, Results_Loh$FM_FMMSY_2115, Results_lnR0$FM_FMMSY_2115, Results_M_BH$FM_FMMSY_2115) 
  # dim(F_FMSY_2115)
  F_FMSY_2115 = get(Results[1])$FM_FMMSY_2115
  for(r in 2:length(Results)){
    F_FMSY_2115 <- rbind(F_FMSY_2115, get(Results[r])$FM_FMMSY_2115)
  }

  
  # F_FMSY_2065 = rbind(Results_BASE$FM_FMMSY_2065, Results_BH$FM_FMMSY_2065, Results_Hih$FM_FMMSY_2065, Results_Loh$FM_FMMSY_2065, Results_lnR0$FM_FMMSY_2065, Results_M_BH$FM_FMMSY_2065) 
  # dim(F_FMSY_2065)
  # View(F_FMSY_2065)
  F_FMSY_2065 = get(Results[1])$FM_FMMSY_2065
  for(r in 2:length(Results)){
    F_FMSY_2065 <- rbind(F_FMSY_2065, get(Results[r])$FM_FMMSY_2065)
  }

  
  
  
  
  ### Avg len F ###
  # AvgLen_F_2065 = rbind(Results_BASE$AvgLen_F_2065, Results_BH$AvgLen_F_2065, Results_Hih$AvgLen_F_2065, Results_Loh$AvgLen_F_2065, Results_lnR0$AvgLen_F_2065, Results_M_BH$AvgLen_F_2065) 
  # dim(AvgLen_F_2065)
  AvgLen_F_2065 = get(Results[1])$AvgLen_F_2065
  for(r in 2:length(Results)){
    AvgLen_F_2065 <- rbind(AvgLen_F_2065, get(Results[r])$AvgLen_F_2065)
  }

  
  # AvgLen_F_2115 = rbind(Results_BASE$AvgLen_F_2115, Results_BH$AvgLen_F_2115, Results_Hih$AvgLen_F_2115, Results_Loh$AvgLen_F_2115, Results_lnR0$AvgLen_F_2115, Results_M_BH$AvgLen_F_2115) 
  # dim(AvgLen_F_2115)
  # View(AvgLen_F_2115)
  AvgLen_F_2115 = get(Results[1])$AvgLen_F_2115
  for(r in 2:length(Results)){
    AvgLen_F_2115 <- rbind(AvgLen_F_2115, get(Results[r])$AvgLen_F_2115)
  }
  
  
  
  ### prob Recov 2115 ###
  SSB_SSBMSY_2115
  Recov <- ifelse(SSB_SSBMSY_2115>1, 1, 0) 

  
  
  
  ### BUILD TABLE ####
  
  if(stat=="median"){
    table <- rbind( "Prob of recovery by 2115" = round( apply(Recov, 2, sum, na.rm=T) / nrow(Recov), digits=3) ,
                    "total US com. catch" = round( apply(TotComCatch, 2, median, na.rm=T), digits=2) ,
                    'SSB2065 / SSBMSY' = round( apply(SSB_SSBMSY_2065, 2, median, na.rm=T), digits=3) ,
                    'SSB2115 / SSBMSY'  = round( apply(SSB_SSBMSY_2115, 2, median, na.rm=T), digits=3) ,
                    'AAV' =  round( apply(AAV, 2, median, na.rm=T), digits=3) ,
                    'F2065 / FMSY' = round( apply(F_FMSY_2065, 2, median, na.rm=T), digits=3) ,
                    'F2115 / FMSY' = round( apply(F_FMSY_2115, 2, median, na.rm=T), digits=3) ,
                    "Fem Length 2065" = round( apply(AvgLen_F_2065, 2, median, na.rm=T), digits=2) , 
                    "Fem Length 2115" = round( apply(AvgLen_F_2115, 2, median, na.rm=T), digits=2)  )
    # table <- round(table, digits=3)
  } # if stat==median
  
  if(stat=="mean"){
    table <- rbind( "Prob of recovery by 2115" = round( apply(Recov, 2, sum, na.rm=T) / nrow(Recov), digits=3)  ,
                    "total US com. catch" = round( apply(TotComCatch, 2, mean, na.rm=T), digits=2) ,
                    'SSB2065 / SSBMSY' = round( apply(SSB_SSBMSY_2065, 2, mean, na.rm=T), digits=3) ,
                    'SSB2115 / SSBMSY'  = round( apply(SSB_SSBMSY_2115, 2, mean, na.rm=T), digits=3) ,
                    'AAV' =  round( apply(AAV, 2, mean, na.rm=T), digits=3) ,
                    'F2065 / FMSY' = round( apply(F_FMSY_2065, 2, mean, na.rm=T), digits=3) ,
                    'F2115 / FMSY' = round( apply(F_FMSY_2115, 2, mean, na.rm=T), digits=3) ,
                    "Fem Length 2065" = round( apply(AvgLen_F_2065, 2, mean, na.rm=T), digits=2) , 
                    "Fem Length 2115" = round( apply(AvgLen_F_2115, 2, mean, na.rm=T), digits=2)  )
    # table <- round(table, digits=3)
  } # if stat==mean
  
  return(list("table" = table))

} # END FUNCTION


BuildTable()



Results_MexRec = c('Results_Base_MexRec', 'Results_BH_MexRec', 'Results_Hih_MexRec', 
                   'Results_Loh_MexRec', 'Results_lnR0_MexRec', 'Results_M_BH_MexRec')
BuildTable(Results=Results_MexRec)
BuildTable(Results=Results_MexRec, stat='mean')



Results_LoMexRec = c('Results_Base_LoMexRec', 'Results_BH_LoMexRec', 'Results_Hih_LoMexRec', 
                   'Results_Loh_LoMexRec', 'Results_lnR0_LoMexRec', 'Results_M_BH_LoMexRec')
BuildTable(Results=Results_LoMexRec)
BuildTable(Results=Results_LoMexRec, stat='mean')


######### BUILD TABLE PLOT ######
### Results are differentiated by a C (conceptual), H (hi), and L (lo)

for(Imp in c('C','H','L')){
  
  if(Imp=='C'){Results = c('Results_Base_MexRec', 'Results_BH_MexRec', 'Results_Hih_MexRec', 
                           'Results_Loh_MexRec', 'Results_lnR0_MexRec', 'Results_M_BH_MexRec')}
  if(Imp=='H'){Results = c('Results_BASE', 'Results_BH', 'Results_Hih', 'Results_Loh', 'Results_lnR0', 'Results_M_BH')}
  if(Imp=='L'){Results = c('Results_Base_LoMexRec', 'Results_BH_LoMexRec', 'Results_Hih_LoMexRec', 
                           'Results_Loh_LoMexRec', 'Results_lnR0_LoMexRec', 'Results_M_BH_LoMexRec')}
  
  ### AAV ###
  AAV = get(Results[1])$AAV_HCR
  for(r in 2:length(Results)){
    AAV <- rbind(AAV, get(Results[r])$AAV_HCR)
  }
  
  
  ### Tot commercial catch ###
  TotComCatch = get(Results[1])$Com_Catch_cumulative
  for(r in 2:length(Results)){
    TotComCatch <- rbind(TotComCatch, get(Results[r])$Com_Catch_cumulative)
  }
  
  
  
  
  ### SSB/SSBMSY ###
  SSB_SSBMSY_2115 = get(Results[1])$SSB_SSBMSY_2115
  for(r in 2:length(Results)){
    SSB_SSBMSY_2115 <- rbind(SSB_SSBMSY_2115, get(Results[r])$SSB_SSBMSY_2115)
  }
  
  SSB_SSBMSY_2065 = get(Results[1])$SSB_SSBMSY_2065
  for(r in 2:length(Results)){
    SSB_SSBMSY_2065 <- rbind(SSB_SSBMSY_2065, get(Results[r])$SSB_SSBMSY_2065)
  }
  
  
  
  
  ### F/FMSY ##
  F_FMSY_2115 = get(Results[1])$FM_FMMSY_2115
  for(r in 2:length(Results)){
    F_FMSY_2115 <- rbind(F_FMSY_2115, get(Results[r])$FM_FMMSY_2115)
  }
  
  
  F_FMSY_2065 = get(Results[1])$FM_FMMSY_2065
  for(r in 2:length(Results)){
    F_FMSY_2065 <- rbind(F_FMSY_2065, get(Results[r])$FM_FMMSY_2065)
  }
  
  
  
  
  
  ### Avg len F ###
  AvgLen_F_2065 = get(Results[1])$AvgLen_F_2065
  for(r in 2:length(Results)){
    AvgLen_F_2065 <- rbind(AvgLen_F_2065, get(Results[r])$AvgLen_F_2065)
  }
  
  
  AvgLen_F_2115 = get(Results[1])$AvgLen_F_2115
  for(r in 2:length(Results)){
    AvgLen_F_2115 <- rbind(AvgLen_F_2115, get(Results[r])$AvgLen_F_2115)
  }
  
  
  
  ### prob Recov 2115 ###
  SSB_SSBMSY_2115
  Recov <- ifelse(SSB_SSBMSY_2115>1, 1, 0) 
  
  
  assign(paste0("Recov_",Imp), Recov)
  assign(paste0("PRecov_",Imp), apply(Recov, 2, sum, na.rm=T) / nrow(Recov))
  assign(paste0("TotComCatch_",Imp), TotComCatch)
  assign(paste0("Med_TotComCatch_",Imp), apply(TotComCatch, 2, median, na.rm=T) )
  assign(paste0("SSB_SSBMSY_2065_",Imp), SSB_SSBMSY_2065)
  assign(paste0("SSB_SSBMSY_2115_",Imp), SSB_SSBMSY_2115)
  assign(paste0("Med_SSB_SSBMSY_2115_",Imp), apply(SSB_SSBMSY_2115, 2, median, na.rm=T) )
  assign(paste0("AAV_",Imp), AAV)
  assign(paste0("Med_AAV_",Imp), apply(AAV, 2, median, na.rm=T) )
  assign(paste0("F_FMSY_2065_",Imp), F_FMSY_2065)
  assign(paste0("F_FMSY_2115_",Imp), F_FMSY_2115)
  assign(paste0("Med_F_FMSY_2115_",Imp), apply(F_FMSY_2115, 2, median, na.rm=T) )
  assign(paste0("AvgLen_F_2065_",Imp), AvgLen_F_2065)
  assign(paste0("AvgLen_F_2115_",Imp), AvgLen_F_2115)
  assign(paste0("Med_AvgLen_F_2115_",Imp), apply(AvgLen_F_2115, 2, median, na.rm=T) )
  
} # end Imp loop 


# PLOT

png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\Barplot_Decision_Table.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=22,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(0, 0, 0, 0),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=1, oma = c(1.5, 2, 1.5, 0.1))

#Prob of recovery 
barplot(as.table(rbind(PRecov_C[1:4],  PRecov_L[1:4], PRecov_H[1:4])), beside=T, col=c('grey','dodgerblue','red'), border=NA, 
        #density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
        ylim=c(0,1),ylab=NULL, names.arg=NULL, axisnames=F, axes=F, cex.axis = 0.5)
axis(2, cex.axis=0.7)
axis(1, labels=FALSE)
abline(h=0.7)
box()
mtext("Prob Recover", 2, line=0.75, cex=0.7)
mtext(expression("F"["lim"]*"=F"["MSY"]), 3, line=0, cex=0.7)
for(i in seq(5, 21, by=4)){
  barplot(as.table(rbind(PRecov_C[i:(i+3)],PRecov_L[i:(i+3)], PRecov_H[i:(i+3)]) ), beside=T, col=c('grey','dodgerblue','red'),  border=NA, 
          ylim=c(0,1),ylab=NULL, axisnames=F, axes=F)
  axis(2, labels=F)
  box()
  abline(h=0.7)
  if(i==5){mtext(expression("F"["lim"]*"=M"), 3, line=0, cex=0.7)}
  if(i==9){mtext(expression("F"["lim"]*"=0.8M"), 3, line=0, cex=0.7)}
  if(i==13){mtext(expression("F"["lim"]*"=0.6M"), 3, line=0, cex=0.7)}
  if(i==17){mtext(expression("F"["lim"]*"=0.4M"), 3, line=0, cex=0.7)}
  if(i==21){mtext(expression("F"["lim"]*"=0.2M"), 3, line=0, cex=0.7)}
}



# Total US Commercial catch 
barplot(as.table(rbind(Med_TotComCatch_C[1:4],  Med_TotComCatch_L[1:4], Med_TotComCatch_H[1:4])), beside=T, col=c('grey','dodgerblue','red'), border=NA, 
        #density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
        ylim=c(0,16000),ylab=NULL, names.arg=NULL, axisnames=F, axes=F)
axis(2, cex.axis=0.7)
axis(1, labels=FALSE)
box()
mtext("US Catch", 2, line=0.75, cex=0.7)
for(i in seq(5, 21, by=4)){
  barplot(as.table(rbind(Med_TotComCatch_C[i:(i+3)],Med_TotComCatch_L[i:(i+3)], Med_TotComCatch_H[i:(i+3)]) ), beside=T, col=c('grey','dodgerblue','red'),  border=NA, 
          ylim=c(0,16000),ylab=NULL, axisnames=F, axes=F)
  axis(2, labels=F)
  box()
}




# SSB / SSB_MSY 
barplot(as.table(rbind(Med_SSB_SSBMSY_2115_C[1:4],  Med_SSB_SSBMSY_2115_L[1:4], Med_SSB_SSBMSY_2115_H[1:4])), beside=T, col=c('grey','dodgerblue','red'), border=NA, 
        #density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
        ylim=c(0.5,1.5),ylab=NULL, names.arg=NULL, axisnames=F, axes=F)
abline(h=1)
axis(2, cex.axis=0.7)
axis(1, labels=FALSE)
box()
mtext(expression("SSB"[2115]*"/SSB"["MSY"]), 2, line=0.75, cex=0.7)
for(i in seq(5, 21, by=4)){
  barplot(as.table(rbind(Med_SSB_SSBMSY_2115_C[i:(i+3)],Med_SSB_SSBMSY_2115_L[i:(i+3)], Med_SSB_SSBMSY_2115_H[i:(i+3)]) ), beside=T, col=c('grey','dodgerblue','red'),  border=NA, 
          ylim=c(0.5,1.5),ylab=NULL, axisnames=F, axes=F)
  abline(h=1)
  axis(2, labels=F)
  box()
}




# F / F_MSY 
barplot(as.table(rbind(Med_F_FMSY_2115_C[1:4],  Med_F_FMSY_2115_L[1:4], Med_F_FMSY_2115_H[1:4])), beside=T, col=c('grey','dodgerblue','red'), border=NA, 
        #density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
        ylim=c(0,1.05),ylab=NULL, names.arg=NULL, axisnames=F, axes=F)
abline(h=1)
axis(2, cex.axis=0.7)
axis(1, labels=FALSE)
box()
mtext(expression("F"[2115]*"/F"["MSY"]), 2, line=0.75, cex=0.7)
for(i in seq(5, 21, by=4)){
  barplot(as.table(rbind(Med_F_FMSY_2115_C[i:(i+3)],Med_F_FMSY_2115_L[i:(i+3)], Med_F_FMSY_2115_H[i:(i+3)]) ), beside=T, col=c('grey','dodgerblue','red'),  border=NA, 
          ylim=c(0,1.05),ylab=NULL, axisnames=F, axes=F)
  abline(h=1)
  axis(2, labels=F)
  box()
}



# AAV
barplot(as.table(rbind(Med_AAV_C[1:4],  Med_AAV_L[1:4], Med_AAV_H[1:4])), beside=T, col=c('grey','dodgerblue','red'), border=NA, 
        #density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
        ylim=c(0,0.5),ylab=NULL, names.arg=NULL, axisnames=F, axes=F)
abline(h=1)
axis(2, cex.axis=0.7)
axis(1, labels=FALSE)
box()
mtext("AAV", 2, line=0.75, cex=0.7)
for(i in seq(5, 21, by=4)){
  barplot(as.table(rbind(Med_AAV_C[i:(i+3)],Med_AAV_L[i:(i+3)], Med_AAV_H[i:(i+3)]) ), beside=T, col=c('grey','dodgerblue','red'),  border=NA, 
          ylim=c(0,0.5),ylab=NULL, axisnames=F, axes=F)
  abline(h=1)
  axis(2, labels=F)
  box()
}






# length
barplot(as.table(rbind(Med_AvgLen_F_2115_C[1:4],  Med_AvgLen_F_2115_L[1:4], Med_AvgLen_F_2115_H[1:4])), beside=T, col=c('grey','dodgerblue','red'), border=NA, 
        #density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
        ylim=c(100,105),ylab=NULL, names.arg=NULL,  axes=F, cex.names = 0.5)
abline(h=1)
axis(2, cex.axis=0.7)
axis(1, labels=F)
box()
mtext("Avg. Len", 2, line=0.75, cex=0.7)
for(i in seq(5, 21, by=4)){
  barplot(as.table(rbind(Med_AvgLen_F_2115_C[i:(i+3)],Med_AvgLen_F_2115_L[i:(i+3)], Med_AvgLen_F_2115_H[i:(i+3)]) ), beside=T, col=c('grey','dodgerblue','red'),  border=NA, 
          ylim=c(100,105),ylab=NULL, axes=F, cex.names = 0.5)
  abline(h=1)
  axis(2, labels=F)
  box()
}

##########
dev.off()