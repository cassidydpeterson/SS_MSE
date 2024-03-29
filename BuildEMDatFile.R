######################################
# BUILT EM DATA FILE FROM OM (MSE2)
# Feb 2020
# CPeterson
######################################

# requires r4ss and OMdir and EM dir specified.
# library(r4ss)
#

### MAKE INTO A FUNCTION:
BuildEM <- function(EMdir, OMdir, tt, ...) {
  # Get EM data file and add results from MSE_Step2
  ### NOTE: Make data Corrections for variance adjustment!!!

  EMdat <- SS_readdat(file = file.path(EMdir, "SB.dat"), version = "3.30")
  OMboot_dat <- SS_readdat(file = file.path(OMdir, "data.ss_new"), section = 3, version = "3.30")


  # get new EM data
  NewEMdat <- EMdat

  # calculate end year of estimation model
  EndYr <- tt - 1
  NewEMdat$endyr <- EndYr

  # Replace catch with bootstrapped catch for existing years
  newcatch <- subset(OMboot_dat$catch, OMboot_dat$catch$year <= EndYr)

  newcatch <- round(newcatch, digits = 5)
  NewEMdat$catch <- newcatch

  # Replace CPUE with bootstrapped CPUE for existing years
  newCPUE <- subset(OMboot_dat$CPUE, OMboot_dat$CPUE$year <= EndYr)
  newCPUE <- round(newCPUE, digits = 5)
  NewEMdat$CPUE <- newCPUE


  # replace lencomp data with bootstrapped lencomp data
  newlen <- subset(OMboot_dat$lencomp, OMboot_dat$lencomp$Yr <= EndYr) # subset length comps
  newlen$Yr <- ifelse(newlen$Nsamp < 1, newlen$Yr * -1, newlen$Yr) # if Nsamp <=0, make year negative
  newlen <- round(newlen, digits = 4)
  NewEMdat$lencomp <- newlen

  # re-write new EM data file with bootstrapped historical data
  SS_writedat(NewEMdat, outfile = file.path(EMdir, "SB.dat"), version = "3.30", overwrite = T)
} # end BuildEM function



BuildOM <- function(OMdir, tt, ...) {
  # re-write new OM data file with historical expected values
  OMexpect_dat <- SS_readdat(file = file.path(OMdir, "data.ss_new"), section = 2, version = "3.30")
  OMdat <- SS_readdat(file = file.path(OMdir, "data.ss_new"), section = 1, version = "3.30")
  newdat <- OMdat

  # Update Data
  newdat$catch[newdat$catch$year < tt, ] <- OMexpect_dat$catch[OMexpect_dat$catch$year < tt, ]
  newdat$catch <- round(newdat$catch, digits = 5)
  newdat$CPUE[newdat$CPUE$year < tt, ] <- OMexpect_dat$CPUE[OMexpect_dat$CPUE$year < tt, ]
  newdat$CPUE <- round(newdat$CPUE, digits = 5)
  newdat$lencomp[newdat$lencomp$Yr > 0 & newdat$lencomp$Yr < tt, ] <- OMexpect_dat$lencomp[OMexpect_dat$lencomp$Yr < tt, ]
  newdat$lencomp <- round(newdat$lencomp, digits = 4)

  SS_writedat(newdat, outfile = file.path(OMdir, "SB.dat"), version = "3.30", overwrite = T)
} # END BuildOM function
