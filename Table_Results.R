###########################
### COllate MSE Results ###
### Oct 2020 ##############
### Updated: May 2021 #####
###########################


Results_HiMexRec = c('Results_Base_HiMexRec', 'Results_BH_HiMexRec', 'Results_Hih_HiMexRec', 'Results_Loh_HiMexRec', 'Results_lnR0_HiMexRec', 'Results_M_BH_HiMexRec')


BuildTable <- function(Results = c('Results_Base_HiMexRec', 'Results_BH_HiMexRec', 'Results_Hih_HiMexRec', 'Results_Loh_HiMexRec', 'Results_lnR0_HiMexRec', 'Results_M_BH_HiMexRec'), stat = "median"){
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



Results_Concept = c('Results_Base_Concept', 'Results_BH_Concept', 'Results_Hih_Concept', 
                   'Results_Loh_Concept', 'Results_lnR0_Concept', 'Results_M_BH_Concept')
BuildTable(Results=Results_Concept)
BuildTable(Results=Results_Concept, stat='mean')



Results_LoMexRec = c('Results_Base_LoMexRec', 'Results_BH_LoMexRec', 'Results_Hih_LoMexRec', 
                   'Results_Loh_LoMexRec', 'Results_lnR0_LoMexRec', 'Results_M_BH_LoMexRec')
BuildTable(Results=Results_LoMexRec)
BuildTable(Results=Results_LoMexRec, stat='mean')


######### BUILD TABLE PLOT ######
### Results are differentiated by a C (conceptual), H (hi), and L (lo)

for(Imp in c('C','H','L')){
  
  if(Imp=='C'){Results = c('Results_Base_Concept', 'Results_BH_Concept', 'Results_Hih_Concept', 
                           'Results_Loh_Concept', 'Results_lnR0_Concept', 'Results_M_BH_Concept')}
  if(Imp=='H'){Results = c('Results_Base_HiMexRec', 'Results_BH_HiMexRec', 'Results_Hih_HiMexRec', 
                           'Results_Loh_HiMexRec', 'Results_lnR0_HiMexRec', 'Results_M_BH_HiMexRec')}
  if(Imp=='L'){Results = c('Results_Base_LoMexRec', 'Results_BH_LoMexRec', 'Results_Hih_LoMexRec', 
                           'Results_Loh_LoMexRec', 'Results_lnR0_LoMexRec', 'Results_M_BH_LoMexRec')}
  
  ### AAV ###
  AAV = get(Results[1])$AAV_HCR
  for(r in 2:length(Results)){
    AAV <- rbind(AAV, get(Results[r])$AAV_HCR)
  }
  ### AAV_ALL ###
  AAV_ALL = get(Results[1])$AAV_all
  for(r in 2:length(Results)){
    AAV_ALL <- rbind(AAV_ALL, get(Results[r])$AAV_all)
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
  Recov <- ifelse(SSB_SSBMSY_2115>0.9, 1, 0) 
  
  
  assign(paste0("Recov_",Imp), Recov)
  assign(paste0("PRecov_",Imp), apply(Recov, 2, sum, na.rm=T) / nrow(Recov))
  assign(paste0("POF_",Imp), get(Results[1])$ProbOF )
  assign(paste0("TotComCatch_",Imp), TotComCatch)
  assign(paste0("Med_TotComCatch_",Imp), apply(TotComCatch, 2, median, na.rm=T) )
  assign(paste0("SSB_SSBMSY_2065_",Imp), SSB_SSBMSY_2065)
  assign(paste0("SSB_SSBMSY_2115_",Imp), SSB_SSBMSY_2115)
  assign(paste0("Med_SSB_SSBMSY_2115_",Imp), apply(SSB_SSBMSY_2115, 2, median, na.rm=T) )
  assign(paste0("AAV_",Imp), AAV)
  assign(paste0("AAV_ALL_",Imp), AAV_ALL)
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
    height=300,
    pointsize=22,
    res=300)
#####
par(mfrow=c(7,6),  mar=c(0, 0, 0, 0),tcl = -0.1, mgp = c(0.5, 0, 0.01), cex=1, oma = c(2.5, 2, 2.5, 0.1))

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
  abline(h=0.7, col='grey45')
  if(i==5){mtext(expression("F"["lim"]*"=M"), 3, line=0, cex=0.7)}
  if(i==9){mtext(expression("F"["lim"]*"=0.8M"), 3, line=0, cex=0.7)}
  if(i==13){mtext(expression("F"["lim"]*"=0.6M"), 3, line=0, cex=0.7)}
  if(i==17){mtext(expression("F"["lim"]*"=0.4M"), 3, line=0, cex=0.7)}
  if(i==21){mtext(expression("F"["lim"]*"=0.2M"), 3, line=0, cex=0.7)}
}

mtext("Graphical Decision Table", side=3, line=1, cex=1, outer=T)



# POF
barplot(as.table(rbind(apply(POF_C,2,median)[1:4],  apply(POF_L ,2,median)[1:4], apply(POF_H,2,median)[1:4])), beside=T, col=c('grey','dodgerblue','red'), border=NA, 
        #density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
        ylim=c(0,0.45),ylab=NULL, names.arg=NULL, axisnames=F, axes=F)
axis(2, cex.axis=0.7)
axis(1, labels=FALSE)
box()
mtext("POF", 2, line=0.75, cex=0.7)
for(i in seq(5, 21, by=4)){
  barplot(as.table(rbind(apply(POF_C,2,median)[i:(i+3)],apply(POF_L,2,median)[i:(i+3)], apply(POF_H,2,median)[i:(i+3)]) ), beside=T, col=c('grey','dodgerblue','red'),  border=NA, 
          ylim=c(0,0.45),ylab=NULL, axisnames=F, axes=F)
  axis(2, labels=F)
  box()
}




# Total US Commercial catch 
barplot(as.table(rbind(Med_TotComCatch_C[1:4],  Med_TotComCatch_L[1:4], Med_TotComCatch_H[1:4])), beside=T, col=c('grey','dodgerblue','red'), border=NA, 
        #density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
        ylim=c(0,17000),ylab=NULL, names.arg=NULL, axisnames=F, axes=F)
axis(2, cex.axis=0.7)
axis(1, labels=FALSE)
box()
mtext("US Catch", 2, line=0.75, cex=0.7)
for(i in seq(5, 21, by=4)){
  barplot(as.table(rbind(Med_TotComCatch_C[i:(i+3)],Med_TotComCatch_L[i:(i+3)], Med_TotComCatch_H[i:(i+3)]) ), beside=T, col=c('grey','dodgerblue','red'),  border=NA, 
          ylim=c(0,17000),ylab=NULL, axisnames=F, axes=F)
  axis(2, labels=F)
  box()
}




# SSB / SSB_MSY 
barplot(as.table(rbind(Med_SSB_SSBMSY_2115_C[1:4],  Med_SSB_SSBMSY_2115_L[1:4], Med_SSB_SSBMSY_2115_H[1:4])), beside=T, col=c('grey','dodgerblue','red'), border=NA, 
        #density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
        ylim=c(0.5,1.5),ylab=NULL, names.arg=NULL, axisnames=F, axes=F)
abline(h=1, col="grey45")
axis(2, cex.axis=0.7)
axis(1, labels=FALSE)
box()
mtext(expression("SSB"[2115]*"/SSB"["MSY"]), 2, line=0.75, cex=0.7)
for(i in seq(5, 21, by=4)){
  barplot(as.table(rbind(Med_SSB_SSBMSY_2115_C[i:(i+3)],Med_SSB_SSBMSY_2115_L[i:(i+3)], Med_SSB_SSBMSY_2115_H[i:(i+3)]) ), beside=T, col=c('grey','dodgerblue','red'),  border=NA, 
          ylim=c(0.5,1.5),ylab=NULL, axisnames=F, axes=F)
  abline(h=1, col="grey45")
  axis(2, labels=F)
  box()
}




# F / F_MSY 
barplot(as.table(rbind(Med_F_FMSY_2115_C[1:4],  Med_F_FMSY_2115_L[1:4], Med_F_FMSY_2115_H[1:4])), beside=T, col=c('grey','dodgerblue','red'), border=NA, 
        #density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
        ylim=c(0,1.05),ylab=NULL, names.arg=NULL, axisnames=F, axes=F)
abline(h=1, col="grey45")
axis(2, cex.axis=0.7)
axis(1, labels=FALSE)
box()
mtext(expression("F"[2115]*"/F"["MSY"]), 2, line=0.75, cex=0.7)
for(i in seq(5, 21, by=4)){
  barplot(as.table(rbind(Med_F_FMSY_2115_C[i:(i+3)],Med_F_FMSY_2115_L[i:(i+3)], Med_F_FMSY_2115_H[i:(i+3)]) ), beside=T, col=c('grey','dodgerblue','red'),  border=NA, 
          ylim=c(0,1.05),ylab=NULL, axisnames=F, axes=F)
  abline(h=1, col="grey45")
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




add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

add_legend("bottom", legend=c("Concept", "LoMexRec","HiMexRec"), pch=15, 
           col=c("grey","dodgerblue","red"),
           horiz=TRUE, bty='n', cex=0.7, pt.cex=2, inset=c(0.2, 0))

##########
dev.off()





Results = Results_HiMexRec  
SSB_SSBMSY_2115 = get(Results[1])$SSB_SSBMSY_2115
for(r in 2:length(Results)){
  SSB_SSBMSY_2115 <- rbind(SSB_SSBMSY_2115, get(Results[r])$SSB_SSBMSY_2115)
}
Recov <- ifelse(SSB_SSBMSY_2115>1, 1, 0)
HMR_Recov_tot = sum(Recov, na.rm=T) / sum(apply(Recov, 2, function(x) length(which(!is.na(x)))) )


Results = Results_LoMexRec
SSB_SSBMSY_2115 = get(Results[1])$SSB_SSBMSY_2115
for(r in 2:length(Results)){
  SSB_SSBMSY_2115 <- rbind(SSB_SSBMSY_2115, get(Results[r])$SSB_SSBMSY_2115)
}
Recov <- ifelse(SSB_SSBMSY_2115>1, 1, 0)
LMR_Recov_tot = sum(Recov, na.rm=T) / sum(apply(Recov, 2, function(x) length(which(!is.na(x)))) )


Results = Results_Concept
SSB_SSBMSY_2115 = get(Results[1])$SSB_SSBMSY_2115
for(r in 2:length(Results)){
  SSB_SSBMSY_2115 <- rbind(SSB_SSBMSY_2115, get(Results[r])$SSB_SSBMSY_2115)
}
Recov <- ifelse(SSB_SSBMSY_2115>1, 1, 0)
Con_Recov_tot = sum(Recov, na.rm=T) / sum(apply(Recov, 2, function(x) length(which(!is.na(x)))) )



HMR_Recov_tot; LMR_Recov_tot; Con_Recov_tot










# for(i in c(1, 5, 9, 13, 17, 21)){ #i=1
#   boxplot(Results_Base_Concept$SSB_SSBMSY_2115[paste0("HCR_",i)], Results_Base_LoMexRec$SSB_SSBMSY_2115[paste0("HCR_",i)], Results_Base_HiMexRec$SSB_SSBMSY_2115[paste0("HCR_",i)],
#           Results_Base_Concept$SSB_SSBMSY_2115[paste0("HCR_",(i+1))], Results_Base_LoMexRec$SSB_SSBMSY_2115[paste0("HCR_",(i+1))], Results_Base_HiMexRec$SSB_SSBMSY_2115[paste0("HCR_",(i+1))],
#           Results_Base_Concept$SSB_SSBMSY_2115[paste0("HCR_",(i+2))], Results_Base_LoMexRec$SSB_SSBMSY_2115[paste0("HCR_",(i+2))], Results_Base_HiMexRec$SSB_SSBMSY_2115[paste0("HCR_",(i+2))],
#           Results_Base_Concept$SSB_SSBMSY_2115[paste0("HCR_",(i+3))], Results_Base_LoMexRec$SSB_SSBMSY_2115[paste0("HCR_",(i+3))], Results_Base_HiMexRec$SSB_SSBMSY_2115[paste0("HCR_",(i+3))],
#           col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6))
# }
# PLOT WITH BOXPLOTS 
##############
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



#Tot US Com Catch 
boxplot(TotComCatch_C$HCR_1, TotComCatch_L$HCR_1, TotComCatch_H$HCR_1,
        TotComCatch_C$HCR_2, TotComCatch_L$HCR_2, TotComCatch_H$HCR_2,
        TotComCatch_C$HCR_3, TotComCatch_L$HCR_3, TotComCatch_H$HCR_3,
        TotComCatch_C$HCR_4, TotComCatch_L$HCR_4, TotComCatch_H$HCR_4,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,25000), axes=F)
axis(2, cex.axis=0.7)
box()
mtext("US Catch", 2, line=0.75, cex=0.7)
boxplot(TotComCatch_C$HCR_5, TotComCatch_L$HCR_5, TotComCatch_H$HCR_5,
        TotComCatch_C$HCR_6, TotComCatch_L$HCR_6, TotComCatch_H$HCR_6,
        TotComCatch_C$HCR_7, TotComCatch_L$HCR_7, TotComCatch_H$HCR_7,
        TotComCatch_C$HCR_8, TotComCatch_L$HCR_8, TotComCatch_H$HCR_8,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,25000), axes=F)
axis(2, labels=F)
box()
boxplot(TotComCatch_C$HCR_9, TotComCatch_L$HCR_9, TotComCatch_H$HCR_9,
        TotComCatch_C$HCR_10, TotComCatch_L$HCR_10, TotComCatch_H$HCR_10,
        TotComCatch_C$HCR_11, TotComCatch_L$HCR_11, TotComCatch_H$HCR_11,
        TotComCatch_C$HCR_12, TotComCatch_L$HCR_12, TotComCatch_H$HCR_12,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,25000), axes=F)
axis(2, labels=F)
box()
boxplot(TotComCatch_C$HCR_13, TotComCatch_L$HCR_13, TotComCatch_H$HCR_13,
        TotComCatch_C$HCR_14, TotComCatch_L$HCR_14, TotComCatch_H$HCR_14,
        TotComCatch_C$HCR_15, TotComCatch_L$HCR_15, TotComCatch_H$HCR_15,
        TotComCatch_C$HCR_16, TotComCatch_L$HCR_16, TotComCatch_H$HCR_16,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,25000), axes=F)
axis(2, labels=F)
box()
boxplot(TotComCatch_C$HCR_17, TotComCatch_L$HCR_17, TotComCatch_H$HCR_17,
        TotComCatch_C$HCR_18, TotComCatch_L$HCR_18, TotComCatch_H$HCR_18,
        TotComCatch_C$HCR_19, TotComCatch_L$HCR_19, TotComCatch_H$HCR_19,
        TotComCatch_C$HCR_20, TotComCatch_L$HCR_20, TotComCatch_H$HCR_20,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,25000), axes=F)
axis(2, labels=F)
box()
boxplot(TotComCatch_C$HCR_21, TotComCatch_L$HCR_21, TotComCatch_H$HCR_21,
        TotComCatch_C$HCR_22, TotComCatch_L$HCR_22, TotComCatch_H$HCR_22,
        TotComCatch_C$HCR_23, TotComCatch_L$HCR_23, TotComCatch_H$HCR_23,
        TotComCatch_C$HCR_24, TotComCatch_L$HCR_24, TotComCatch_H$HCR_24,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,25000), axes=F)
axis(2, labels=F)
box()


#SSB2115/SSBMSY
boxplot(SSB_SSBMSY_2115_C$HCR_1, SSB_SSBMSY_2115_L$HCR_1, SSB_SSBMSY_2115_H$HCR_1,
        SSB_SSBMSY_2115_C$HCR_2, SSB_SSBMSY_2115_L$HCR_2, SSB_SSBMSY_2115_H$HCR_2,
        SSB_SSBMSY_2115_C$HCR_3, SSB_SSBMSY_2115_L$HCR_3, SSB_SSBMSY_2115_H$HCR_3,
        SSB_SSBMSY_2115_C$HCR_4, SSB_SSBMSY_2115_L$HCR_4, SSB_SSBMSY_2115_H$HCR_4,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,2.5), axes=F)
axis(2, cex.axis=0.7)
mtext(expression("SSB"[2115]*"/SSB"["MSY"]), 2, line=0.75, cex=0.7)
box()
boxplot(SSB_SSBMSY_2115_C$HCR_5, SSB_SSBMSY_2115_L$HCR_5, SSB_SSBMSY_2115_H$HCR_5,
        SSB_SSBMSY_2115_C$HCR_6, SSB_SSBMSY_2115_L$HCR_6, SSB_SSBMSY_2115_H$HCR_6,
        SSB_SSBMSY_2115_C$HCR_7, SSB_SSBMSY_2115_L$HCR_7, SSB_SSBMSY_2115_H$HCR_7,
        SSB_SSBMSY_2115_C$HCR_8, SSB_SSBMSY_2115_L$HCR_8, SSB_SSBMSY_2115_H$HCR_8,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,2.5), axes=F)
axis(2, labels=F)
box()
boxplot(SSB_SSBMSY_2115_C$HCR_9, SSB_SSBMSY_2115_L$HCR_9, SSB_SSBMSY_2115_H$HCR_9,
        SSB_SSBMSY_2115_C$HCR_10, SSB_SSBMSY_2115_L$HCR_10, SSB_SSBMSY_2115_H$HCR_10,
        SSB_SSBMSY_2115_C$HCR_11, SSB_SSBMSY_2115_L$HCR_11, SSB_SSBMSY_2115_H$HCR_11,
        SSB_SSBMSY_2115_C$HCR_12, SSB_SSBMSY_2115_L$HCR_12, SSB_SSBMSY_2115_H$HCR_12,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,2.5), axes=F)
axis(2, labels=F)
box()
boxplot(SSB_SSBMSY_2115_C$HCR_13, SSB_SSBMSY_2115_L$HCR_13, SSB_SSBMSY_2115_H$HCR_13,
        SSB_SSBMSY_2115_C$HCR_14, SSB_SSBMSY_2115_L$HCR_14, SSB_SSBMSY_2115_H$HCR_14,
        SSB_SSBMSY_2115_C$HCR_15, SSB_SSBMSY_2115_L$HCR_15, SSB_SSBMSY_2115_H$HCR_15,
        SSB_SSBMSY_2115_C$HCR_16, SSB_SSBMSY_2115_L$HCR_16, SSB_SSBMSY_2115_H$HCR_16,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,2.5), axes=F)
axis(2, labels=F)
box()
boxplot(SSB_SSBMSY_2115_C$HCR_17, SSB_SSBMSY_2115_L$HCR_17, SSB_SSBMSY_2115_H$HCR_17,
        SSB_SSBMSY_2115_C$HCR_18, SSB_SSBMSY_2115_L$HCR_18, SSB_SSBMSY_2115_H$HCR_18,
        SSB_SSBMSY_2115_C$HCR_19, SSB_SSBMSY_2115_L$HCR_19, SSB_SSBMSY_2115_H$HCR_19,
        SSB_SSBMSY_2115_C$HCR_20, SSB_SSBMSY_2115_L$HCR_20, SSB_SSBMSY_2115_H$HCR_20,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,2.5), axes=F)
axis(2, labels=F)
box()
boxplot(SSB_SSBMSY_2115_C$HCR_21, SSB_SSBMSY_2115_L$HCR_21, SSB_SSBMSY_2115_H$HCR_21,
        SSB_SSBMSY_2115_C$HCR_22, SSB_SSBMSY_2115_L$HCR_22, SSB_SSBMSY_2115_H$HCR_22,
        SSB_SSBMSY_2115_C$HCR_23, SSB_SSBMSY_2115_L$HCR_23, SSB_SSBMSY_2115_H$HCR_23,
        SSB_SSBMSY_2115_C$HCR_24, SSB_SSBMSY_2115_L$HCR_24, SSB_SSBMSY_2115_H$HCR_24,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,2.5), axes=F)
axis(2, labels=F)
box()




#F2115/FMSY
boxplot(F_FMSY_2115_C$HCR_1, F_FMSY_2115_L$HCR_1, F_FMSY_2115_H$HCR_1,
        F_FMSY_2115_C$HCR_2, F_FMSY_2115_L$HCR_2, F_FMSY_2115_H$HCR_2,
        F_FMSY_2115_C$HCR_3, F_FMSY_2115_L$HCR_3, F_FMSY_2115_H$HCR_3,
        F_FMSY_2115_C$HCR_4, F_FMSY_2115_L$HCR_4, F_FMSY_2115_H$HCR_4,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,3), axes=F)
axis(2, cex.axis=0.7)
box()
mtext(expression("F"[2115]*"/F"["MSY"]), 2, line=0.75, cex=0.7)
boxplot(F_FMSY_2115_C$HCR_5, F_FMSY_2115_L$HCR_5, F_FMSY_2115_H$HCR_5,
        F_FMSY_2115_C$HCR_6, F_FMSY_2115_L$HCR_6, F_FMSY_2115_H$HCR_6,
        F_FMSY_2115_C$HCR_7, F_FMSY_2115_L$HCR_7, F_FMSY_2115_H$HCR_7,
        F_FMSY_2115_C$HCR_8, F_FMSY_2115_L$HCR_8, F_FMSY_2115_H$HCR_8,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,3), axes=F)
axis(2, labels=F)
box()
boxplot(F_FMSY_2115_C$HCR_9, F_FMSY_2115_L$HCR_9, F_FMSY_2115_H$HCR_9,
        F_FMSY_2115_C$HCR_10, F_FMSY_2115_L$HCR_10, F_FMSY_2115_H$HCR_10,
        F_FMSY_2115_C$HCR_11, F_FMSY_2115_L$HCR_11, F_FMSY_2115_H$HCR_11,
        F_FMSY_2115_C$HCR_12, F_FMSY_2115_L$HCR_12, F_FMSY_2115_H$HCR_12,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,3), axes=F)
axis(2, labels=F)
box()
boxplot(F_FMSY_2115_C$HCR_13, F_FMSY_2115_L$HCR_13, F_FMSY_2115_H$HCR_13,
        F_FMSY_2115_C$HCR_14, F_FMSY_2115_L$HCR_14, F_FMSY_2115_H$HCR_14,
        F_FMSY_2115_C$HCR_15, F_FMSY_2115_L$HCR_15, F_FMSY_2115_H$HCR_15,
        F_FMSY_2115_C$HCR_16, F_FMSY_2115_L$HCR_16, F_FMSY_2115_H$HCR_16,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,3), axes=F)
axis(2, labels=F)
box()
boxplot(F_FMSY_2115_C$HCR_17, F_FMSY_2115_L$HCR_17, F_FMSY_2115_H$HCR_17,
        F_FMSY_2115_C$HCR_18, F_FMSY_2115_L$HCR_18, F_FMSY_2115_H$HCR_18,
        F_FMSY_2115_C$HCR_19, F_FMSY_2115_L$HCR_19, F_FMSY_2115_H$HCR_19,
        F_FMSY_2115_C$HCR_20, F_FMSY_2115_L$HCR_20, F_FMSY_2115_H$HCR_20,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,3), axes=F)
axis(2, labels=F)
box()
boxplot(F_FMSY_2115_C$HCR_21, F_FMSY_2115_L$HCR_21, F_FMSY_2115_H$HCR_21,
        F_FMSY_2115_C$HCR_22, F_FMSY_2115_L$HCR_22, F_FMSY_2115_H$HCR_22,
        F_FMSY_2115_C$HCR_23, F_FMSY_2115_L$HCR_23, F_FMSY_2115_H$HCR_23,
        F_FMSY_2115_C$HCR_24, F_FMSY_2115_L$HCR_24, F_FMSY_2115_H$HCR_24,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,3), axes=F)
axis(2, labels=F)
box()



#AAV
boxplot(AAV_ALL_C[,1], AAV_ALL_L[,1], AAV_ALL_H[,1],
        AAV_ALL_C[,2], AAV_ALL_L[,2], AAV_ALL_H[,2],
        AAV_ALL_C[,3], AAV_ALL_L[,3], AAV_ALL_H[,3],
        AAV_ALL_C[,4], AAV_ALL_L[,4], AAV_ALL_H[,4],
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,1), axes=F)
axis(2, cex.axis=0.7)
box()
mtext("AAV", 2, line=0.75, cex=0.7)
boxplot(AAV_ALL_C[,5], AAV_ALL_L[,5], AAV_ALL_H[,5],
        AAV_ALL_C[,6], AAV_ALL_L[,6], AAV_ALL_H[,6],
        AAV_ALL_C[,7], AAV_ALL_L[,7], AAV_ALL_H[,7],
        AAV_ALL_C[,8], AAV_ALL_L[,8], AAV_ALL_H[,8],
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,1), axes=F)
axis(2, labels=F)
box()
boxplot(AAV_ALL_C[,9], AAV_ALL_L[,9], AAV_ALL_H[,9],
        AAV_ALL_C[,10], AAV_ALL_L[,10], AAV_ALL_H[,10],
        AAV_ALL_C[,11], AAV_ALL_L[,11], AAV_ALL_H[,11],
        AAV_ALL_C[,12], AAV_ALL_L[,12], AAV_ALL_H[,12],
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,1), axes=F)
axis(2, labels=F)
box()
boxplot(AAV_ALL_C[,13], AAV_ALL_L[,13], AAV_ALL_H[,13],
        AAV_ALL_C[,14], AAV_ALL_L[,14], AAV_ALL_H[,14],
        AAV_ALL_C[,15], AAV_ALL_L[,15], AAV_ALL_H[,15],
        AAV_ALL_C[,16], AAV_ALL_L[,16], AAV_ALL_H[,16],
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,1), axes=F)
axis(2, labels=F)
box()
boxplot(AAV_ALL_C[,17], AAV_ALL_L[,17], AAV_ALL_H[,17],
        AAV_ALL_C[,18], AAV_ALL_L[,18], AAV_ALL_H[,18],
        AAV_ALL_C[,19], AAV_ALL_L[,19], AAV_ALL_H[,19],
        AAV_ALL_C[,20], AAV_ALL_L[,20], AAV_ALL_H[,20],
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,1), axes=F)
axis(2, labels=F)
box()
boxplot(AAV_ALL_C[,21], AAV_ALL_L[,21], AAV_ALL_H[,21],
        AAV_ALL_C[,22], AAV_ALL_L[,22], AAV_ALL_H[,22],
        AAV_ALL_C[,23], AAV_ALL_L[,23], AAV_ALL_H[,23],
        AAV_ALL_C[,24], AAV_ALL_L[,24], AAV_ALL_H[,24],
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,1), axes=F)
axis(2, labels=F)
box()





#Avg length
boxplot(AvgLen_F_2115_C$HCR_1, AvgLen_F_2115_L$HCR_1, AvgLen_F_2115_H$HCR_1,
        AvgLen_F_2115_C$HCR_2, AvgLen_F_2115_L$HCR_2, AvgLen_F_2115_H$HCR_2,
        AvgLen_F_2115_C$HCR_3, AvgLen_F_2115_L$HCR_3, AvgLen_F_2115_H$HCR_3,
        AvgLen_F_2115_C$HCR_4, AvgLen_F_2115_L$HCR_4, AvgLen_F_2115_H$HCR_4,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(80,125), axes=F)
axis(2, cex.axis=0.7)
box()
mtext("Avg. Len.", 2, line=0.75, cex=0.7)
boxplot(AvgLen_F_2115_C$HCR_5, AvgLen_F_2115_L$HCR_5, AvgLen_F_2115_H$HCR_5,
        AvgLen_F_2115_C$HCR_6, AvgLen_F_2115_L$HCR_6, AvgLen_F_2115_H$HCR_6,
        AvgLen_F_2115_C$HCR_7, AvgLen_F_2115_L$HCR_7, AvgLen_F_2115_H$HCR_7,
        AvgLen_F_2115_C$HCR_8, AvgLen_F_2115_L$HCR_8, AvgLen_F_2115_H$HCR_8,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(80,125), axes=F)
axis(2, labels=F)
box()
boxplot(AvgLen_F_2115_C$HCR_9, AvgLen_F_2115_L$HCR_9, AvgLen_F_2115_H$HCR_9,
        AvgLen_F_2115_C$HCR_10, AvgLen_F_2115_L$HCR_10, AvgLen_F_2115_H$HCR_10,
        AvgLen_F_2115_C$HCR_11, AvgLen_F_2115_L$HCR_11, AvgLen_F_2115_H$HCR_11,
        AvgLen_F_2115_C$HCR_12, AvgLen_F_2115_L$HCR_12, AvgLen_F_2115_H$HCR_12,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(80,125), axes=F)
axis(2, labels=F)
box()
boxplot(AvgLen_F_2115_C$HCR_13, AvgLen_F_2115_L$HCR_13, AvgLen_F_2115_H$HCR_13,
        AvgLen_F_2115_C$HCR_14, AvgLen_F_2115_L$HCR_14, AvgLen_F_2115_H$HCR_14,
        AvgLen_F_2115_C$HCR_15, AvgLen_F_2115_L$HCR_15, AvgLen_F_2115_H$HCR_15,
        AvgLen_F_2115_C$HCR_16, AvgLen_F_2115_L$HCR_16, AvgLen_F_2115_H$HCR_16,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(80,125), axes=F)
axis(2, labels=F)
box()
boxplot(AvgLen_F_2115_C$HCR_17, AvgLen_F_2115_L$HCR_17, AvgLen_F_2115_H$HCR_17,
        AvgLen_F_2115_C$HCR_18, AvgLen_F_2115_L$HCR_18, AvgLen_F_2115_H$HCR_18,
        AvgLen_F_2115_C$HCR_19, AvgLen_F_2115_L$HCR_19, AvgLen_F_2115_H$HCR_19,
        AvgLen_F_2115_C$HCR_20, AvgLen_F_2115_L$HCR_20, AvgLen_F_2115_H$HCR_20,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(80,125), axes=F)
axis(2, labels=F)
box()
boxplot(AvgLen_F_2115_C$HCR_21, AvgLen_F_2115_L$HCR_21, AvgLen_F_2115_H$HCR_21,
        AvgLen_F_2115_C$HCR_22, AvgLen_F_2115_L$HCR_22, AvgLen_F_2115_H$HCR_22,
        AvgLen_F_2115_C$HCR_23, AvgLen_F_2115_L$HCR_23, AvgLen_F_2115_H$HCR_23,
        AvgLen_F_2115_C$HCR_24, AvgLen_F_2115_L$HCR_24, AvgLen_F_2115_H$HCR_24,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(80,125), axes=F)
axis(2, labels=F)
box()

######################







############ EXTRAS ################

#SSB2065/SSBMSY
boxplot(SSB_SSBMSY_2065_C$HCR_1, SSB_SSBMSY_2065_L$HCR_1, SSB_SSBMSY_2065_H$HCR_1,
        SSB_SSBMSY_2065_C$HCR_2, SSB_SSBMSY_2065_L$HCR_2, SSB_SSBMSY_2065_H$HCR_2,
        SSB_SSBMSY_2065_C$HCR_3, SSB_SSBMSY_2065_L$HCR_3, SSB_SSBMSY_2065_H$HCR_3,
        SSB_SSBMSY_2065_C$HCR_4, SSB_SSBMSY_2065_L$HCR_4, SSB_SSBMSY_2065_H$HCR_4,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,2.5), axes=F)
axis(2, cex.axis=0.7)
mtext(expression("SSB"[2065]*"/SSB"["MSY"]), 2, line=0.75, cex=0.7)
box()
boxplot(SSB_SSBMSY_2065_C$HCR_5, SSB_SSBMSY_2065_L$HCR_5, SSB_SSBMSY_2065_H$HCR_5,
        SSB_SSBMSY_2065_C$HCR_6, SSB_SSBMSY_2065_L$HCR_6, SSB_SSBMSY_2065_H$HCR_6,
        SSB_SSBMSY_2065_C$HCR_7, SSB_SSBMSY_2065_L$HCR_7, SSB_SSBMSY_2065_H$HCR_7,
        SSB_SSBMSY_2065_C$HCR_8, SSB_SSBMSY_2065_L$HCR_8, SSB_SSBMSY_2065_H$HCR_8,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,2.5), axes=F)
axis(2, labels=F)
box()
boxplot(SSB_SSBMSY_2065_C$HCR_9, SSB_SSBMSY_2065_L$HCR_9, SSB_SSBMSY_2065_H$HCR_9,
        SSB_SSBMSY_2065_C$HCR_10, SSB_SSBMSY_2065_L$HCR_10, SSB_SSBMSY_2065_H$HCR_10,
        SSB_SSBMSY_2065_C$HCR_11, SSB_SSBMSY_2065_L$HCR_11, SSB_SSBMSY_2065_H$HCR_11,
        SSB_SSBMSY_2065_C$HCR_12, SSB_SSBMSY_2065_L$HCR_12, SSB_SSBMSY_2065_H$HCR_12,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,2.5), axes=F)
axis(2, labels=F)
box()
boxplot(SSB_SSBMSY_2065_C$HCR_13, SSB_SSBMSY_2065_L$HCR_13, SSB_SSBMSY_2065_H$HCR_13,
        SSB_SSBMSY_2065_C$HCR_14, SSB_SSBMSY_2065_L$HCR_14, SSB_SSBMSY_2065_H$HCR_14,
        SSB_SSBMSY_2065_C$HCR_15, SSB_SSBMSY_2065_L$HCR_15, SSB_SSBMSY_2065_H$HCR_15,
        SSB_SSBMSY_2065_C$HCR_16, SSB_SSBMSY_2065_L$HCR_16, SSB_SSBMSY_2065_H$HCR_16,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,2.5), axes=F)
axis(2, labels=F)
box()
boxplot(SSB_SSBMSY_2065_C$HCR_17, SSB_SSBMSY_2065_L$HCR_17, SSB_SSBMSY_2065_H$HCR_17,
        SSB_SSBMSY_2065_C$HCR_18, SSB_SSBMSY_2065_L$HCR_18, SSB_SSBMSY_2065_H$HCR_18,
        SSB_SSBMSY_2065_C$HCR_19, SSB_SSBMSY_2065_L$HCR_19, SSB_SSBMSY_2065_H$HCR_19,
        SSB_SSBMSY_2065_C$HCR_20, SSB_SSBMSY_2065_L$HCR_20, SSB_SSBMSY_2065_H$HCR_20,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,2.5), axes=F)
axis(2, labels=F)
box()
boxplot(SSB_SSBMSY_2065_C$HCR_21, SSB_SSBMSY_2065_L$HCR_21, SSB_SSBMSY_2065_H$HCR_21,
        SSB_SSBMSY_2065_C$HCR_22, SSB_SSBMSY_2065_L$HCR_22, SSB_SSBMSY_2065_H$HCR_22,
        SSB_SSBMSY_2065_C$HCR_23, SSB_SSBMSY_2065_L$HCR_23, SSB_SSBMSY_2065_H$HCR_23,
        SSB_SSBMSY_2065_C$HCR_24, SSB_SSBMSY_2065_L$HCR_24, SSB_SSBMSY_2065_H$HCR_24,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,2.5), axes=F)
axis(2, labels=F)
box()


#F2065/FMSY
boxplot(F_FMSY_2065_C$HCR_1, F_FMSY_2065_L$HCR_1, F_FMSY_2065_H$HCR_1,
        F_FMSY_2065_C$HCR_2, F_FMSY_2065_L$HCR_2, F_FMSY_2065_H$HCR_2,
        F_FMSY_2065_C$HCR_3, F_FMSY_2065_L$HCR_3, F_FMSY_2065_H$HCR_3,
        F_FMSY_2065_C$HCR_4, F_FMSY_2065_L$HCR_4, F_FMSY_2065_H$HCR_4,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,2.5), axes=F)
axis(2, cex.axis=0.7)
mtext(expression("F"[2065]*"/F"["MSY"]), 2, line=0.75, cex=0.7)
box()
boxplot(F_FMSY_2065_C$HCR_5, F_FMSY_2065_L$HCR_5, F_FMSY_2065_H$HCR_5,
        F_FMSY_2065_C$HCR_6, F_FMSY_2065_L$HCR_6, F_FMSY_2065_H$HCR_6,
        F_FMSY_2065_C$HCR_7, F_FMSY_2065_L$HCR_7, F_FMSY_2065_H$HCR_7,
        F_FMSY_2065_C$HCR_8, F_FMSY_2065_L$HCR_8, F_FMSY_2065_H$HCR_8,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,2.5), axes=F)
axis(2, labels=F)
box()
boxplot(F_FMSY_2065_C$HCR_9, F_FMSY_2065_L$HCR_9, F_FMSY_2065_H$HCR_9,
        F_FMSY_2065_C$HCR_10, F_FMSY_2065_L$HCR_10, F_FMSY_2065_H$HCR_10,
        F_FMSY_2065_C$HCR_11, F_FMSY_2065_L$HCR_11, F_FMSY_2065_H$HCR_11,
        F_FMSY_2065_C$HCR_12, F_FMSY_2065_L$HCR_12, F_FMSY_2065_H$HCR_12,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,2.5), axes=F)
axis(2, labels=F)
box()
boxplot(F_FMSY_2065_C$HCR_13, F_FMSY_2065_L$HCR_13, F_FMSY_2065_H$HCR_13,
        F_FMSY_2065_C$HCR_14, F_FMSY_2065_L$HCR_14, F_FMSY_2065_H$HCR_14,
        F_FMSY_2065_C$HCR_15, F_FMSY_2065_L$HCR_15, F_FMSY_2065_H$HCR_15,
        F_FMSY_2065_C$HCR_16, F_FMSY_2065_L$HCR_16, F_FMSY_2065_H$HCR_16,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,2.5), axes=F)
axis(2, labels=F)
box()
boxplot(F_FMSY_2065_C$HCR_17, F_FMSY_2065_L$HCR_17, F_FMSY_2065_H$HCR_17,
        F_FMSY_2065_C$HCR_18, F_FMSY_2065_L$HCR_18, F_FMSY_2065_H$HCR_18,
        F_FMSY_2065_C$HCR_19, F_FMSY_2065_L$HCR_19, F_FMSY_2065_H$HCR_19,
        F_FMSY_2065_C$HCR_20, F_FMSY_2065_L$HCR_20, F_FMSY_2065_H$HCR_20,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,2.5), axes=F)
axis(2, labels=F)
box()
boxplot(F_FMSY_2065_C$HCR_21, F_FMSY_2065_L$HCR_21, F_FMSY_2065_H$HCR_21,
        F_FMSY_2065_C$HCR_22, F_FMSY_2065_L$HCR_22, F_FMSY_2065_H$HCR_22,
        F_FMSY_2065_C$HCR_23, F_FMSY_2065_L$HCR_23, F_FMSY_2065_H$HCR_23,
        F_FMSY_2065_C$HCR_24, F_FMSY_2065_L$HCR_24, F_FMSY_2065_H$HCR_24,
        col=c('grey','deepskyblue','red'), at=c(1,1.8,2.6,4,4.8,5.6,7,7.8,8.6,10,10.8,11.6), ylab="", xlab="",ylim=c(0,2.5), axes=F)
axis(2, labels=F)
box()

  