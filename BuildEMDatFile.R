######################################
# BUILT EM DATA FILE FROM OM (MSE2)
# Oct. 2018
# CPeterson
######################################

# requires r4ss and OMdir and EM dir specified. 
# library(r4ss)
# 
# OMdir = "R:\\Management Strategy Evaluation\\SB\\TEST_Base\\HCR1\\OM" # OM directory
# EMdir = "R:\\Management Strategy Evaluation\\SB\\TEST_Base\\HCR1\\EM" # EM directory

### MAKE INTO A FUNCTION:
BuildEM = function(EMdir, OMdir, ...) {
  # Get EM data file and add results from MSE_Step2
  ### NOTE: Make data Corrections for variance adjustment!!!
  
  EMdat = SS_readdat(file=paste(EMdir,"\\SB.dat", sep=""), version="3.30")
  OMboot_dat = SS_readdat(file=paste(OMdir,"\\data.ss_new", sep=""), section=3, version="3.30") 
  
  
  # calculate years
  EndYr = EMdat$endyr
  
  # get new EM data
  NewEMdat = EMdat
  
  # Replace catch with bootstrapped catch for existing years
  newcatch = subset(OMboot_dat$catch, OMboot_dat$catch$year<=EndYr)
  NewEMdat$catch = rbind(EMdat$catch, newcatch )
  
  # Replace CPUE with bootstrapped CPUE for existing years
  newCPUE = subset(OMboot_dat$CPUE, OMboot_dat$CPUE$year<=EndYr)
  NewEMdat$CPUE = rbind(EMdat$CPUE,  newCPUE)
  
  
  # replace lencomp data with bootstrapped lencomp data
  newlen = subset(OMboot_dat$lencomp, OMboot_dat$lencomp$Yr<=EndYr)
  newlenFish = subset(newlen, newlen$FltSvy==1)
  ObsLen = ifelse(newcatch$catch==0, NA, newcatch$year)
  newlenFish$Nsamp = ifelse(is.na(ObsLen), 0, newlenFish$Nsamp)
  newlenFish = newlenFish[newlenFish$Nsamp > 0,]
  
  newlenSurv = subset(newlen, newlen$FltSvy==2)
  newlenSurv$Nsamp = newCPUE$obs
  newlenSurv = newlenSurv[newlenSurv$Nsamp > 0,]
  
  NewEMdat$lencomp = rbind(newlenFish, newlenSurv)
  
  # re-write new EM data file with bootstrapped historical data
  SS_writedat(NewEMdat, outfile=paste(EMdir,"\\SB.dat", sep=""), version="3.30", overwrite=T)
  
}

BuildOM = function(OMdir,...){
  # re-write new OM data file with historical expected values
  OMexpect_dat = SS_readdat(file=paste(OMdir,"\\data.ss_new", sep=""), section=2, version="3.30") 
  SS_writedat(OMexpect_dat, outfile=paste(OMdir,"\\SB.dat", sep=""), version="3.30", overwrite=T)
  
}



