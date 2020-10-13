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
