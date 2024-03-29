## RUN MSE IN PARALLEL
## Note long run times.


#### Define directories ####

MCMCdir <- "MCMC" # OM version that undergoes MCMC directory

Basedir <- "OM_Base\\"
for (i in 1:24) {
  assign(paste0("OMdirHCR", i), paste0(Basedir, "HCR", i, "\\OM"))
  assign(paste0("EMdirHCR", i), paste0(Basedir, "HCR", i, "\\EM"))
  assign(paste0("SR_HCR", i), paste0(Basedir, "HCR", i, "\\StoreResults"))
}

#### Define HCR trials ####
# HCR iterations:
# list.hcr=c(HCR1, HCR2, HCR3, HCR4, HCR5, HCR6, HCR7, HCR8, HCR9, HCR10, HCR11, HCR12,
#            HCR13, HCR14, HCR15, HCR16, HCR17, HCR18, HCR19, HCR20, HCR21, HCR22, HCR23, HCR24)
list.hcr <- list()
list.hcr[["HCR1"]] <- list(Ftarg = "FMSY", Fconst = 1, a = 0, b = 1, OMdir = OMdirHCR1, EMdir = EMdirHCR1, StoreResults = SR_HCR1)
list.hcr[["HCR2"]] <- list(Ftarg = "FMSY", Fconst = 1, a = 0, b = 0.8, OMdir = OMdirHCR2, EMdir = EMdirHCR2, StoreResults = SR_HCR2)
list.hcr[["HCR3"]] <- list(Ftarg = "FMSY", Fconst = 1, a = 0.3, b = 1, OMdir = OMdirHCR3, EMdir = EMdirHCR3, StoreResults = SR_HCR3)
list.hcr[["HCR4"]] <- list(Ftarg = "FMSY", Fconst = 1, a = 0.3, b = 0.8, OMdir = OMdirHCR4, EMdir = EMdirHCR4, StoreResults = SR_HCR4)

list.hcr[["HCR5"]] <- list(Ftarg = "F=M", Fconst = 1, a = 0, b = 1, OMdir = OMdirHCR5, EMdir = EMdirHCR5, StoreResults = SR_HCR5)
list.hcr[["HCR6"]] <- list(Ftarg = "F=M", Fconst = 1, a = 0, b = 0.8, OMdir = OMdirHCR6, EMdir = EMdirHCR6, StoreResults = SR_HCR6)
list.hcr[["HCR7"]] <- list(Ftarg = "F=M", Fconst = 1, a = 0.3, b = 1, OMdir = OMdirHCR7, EMdir = EMdirHCR7, StoreResults = SR_HCR7)
list.hcr[["HCR8"]] <- list(Ftarg = "F=M", Fconst = 1, a = 0.3, b = 0.8, OMdir = OMdirHCR8, EMdir = EMdirHCR8, StoreResults = SR_HCR8)

list.hcr[["HCR9"]] <- list(Ftarg = "F=M", Fconst = 0.8, a = 0, b = 1, OMdir = OMdirHCR9, EMdir = EMdirHCR9, StoreResults = SR_HCR9)
list.hcr[["HCR10"]] <- list(Ftarg = "F=M", Fconst = 0.8, a = 0, b = 0.8, OMdir = OMdirHCR10, EMdir = EMdirHCR10, StoreResults = SR_HCR10)
list.hcr[["HCR11"]] <- list(Ftarg = "F=M", Fconst = 0.8, a = 0.3, b = 1, OMdir = OMdirHCR11, EMdir = EMdirHCR11, StoreResults = SR_HCR11)
list.hcr[["HCR12"]] <- list(Ftarg = "F=M", Fconst = 0.8, a = 0.3, b = 0.8, OMdir = OMdirHCR12, EMdir = EMdirHCR12, StoreResults = SR_HCR12)

list.hcr[["HCR13"]] <- list(Ftarg = "F=M", Fconst = 0.6, a = 0, b = 1, OMdir = OMdirHCR13, EMdir = EMdirHCR13, StoreResults = SR_HCR13)
list.hcr[["HCR14"]] <- list(Ftarg = "F=M", Fconst = 0.6, a = 0, b = 0.8, OMdir = OMdirHCR14, EMdir = EMdirHCR14, StoreResults = SR_HCR14)
list.hcr[["HCR15"]] <- list(Ftarg = "F=M", Fconst = 0.6, a = 0.3, b = 1, OMdir = OMdirHCR15, EMdir = EMdirHCR15, StoreResults = SR_HCR15)
list.hcr[["HCR16"]] <- list(Ftarg = "F=M", Fconst = 0.6, a = 0.3, b = 0.8, OMdir = OMdirHCR16, EMdir = EMdirHCR16, StoreResults = SR_HCR16)

list.hcr[["HCR17"]] <- list(Ftarg = "F=M", Fconst = 0.4, a = 0, b = 1, OMdir = OMdirHCR17, EMdir = EMdirHCR17, StoreResults = SR_HCR17)
list.hcr[["HCR18"]] <- list(Ftarg = "F=M", Fconst = 0.4, a = 0, b = 0.8, OMdir = OMdirHCR18, EMdir = EMdirHCR18, StoreResults = SR_HCR18)
list.hcr[["HCR19"]] <- list(Ftarg = "F=M", Fconst = 0.4, a = 0.3, b = 1, OMdir = OMdirHCR19, EMdir = EMdirHCR19, StoreResults = SR_HCR19)
list.hcr[["HCR20"]] <- list(Ftarg = "F=M", Fconst = 0.4, a = 0.3, b = 0.8, OMdir = OMdirHCR20, EMdir = EMdirHCR20, StoreResults = SR_HCR20)

list.hcr[["HCR21"]] <- list(Ftarg = "F=M", Fconst = 0.2, a = 0, b = 1, OMdir = OMdirHCR21, EMdir = EMdirHCR21, StoreResults = SR_HCR21)
list.hcr[["HCR22"]] <- list(Ftarg = "F=M", Fconst = 0.2, a = 0, b = 0.8, OMdir = OMdirHCR22, EMdir = EMdirHCR22, StoreResults = SR_HCR22)
list.hcr[["HCR23"]] <- list(Ftarg = "F=M", Fconst = 0.2, a = 0.3, b = 1, OMdir = OMdirHCR23, EMdir = EMdirHCR23, StoreResults = SR_HCR23)
list.hcr[["HCR24"]] <- list(Ftarg = "F=M", Fconst = 0.2, a = 0.3, b = 0.8, OMdir = OMdirHCR24, EMdir = EMdirHCR24, StoreResults = SR_HCR24)



######

# reminder of inputs to MSE_func
# MSE_func(MCMCdir, OMdir, EMdir, StoreResults, FRQ=5,
#          Btarg="BMSY", Bconst=1, Ftarg="FMSY", Fconst=1, a=0.1, b=1,
#          BuildPar=T, OMdirs=list(OMdir),
#          simYrs=100, niters=NA, ...)
library(foreach)
library(doSNOW)
library(parallel)

NoC <- detectCores() # determine the number of clusters you want to use.
c1 <- makeCluster(4)
registerDoSNOW(c1)

source("MSE_Master.R")
foreach(h = names(list.hcr)) %dopar% {
  # GET MSE_func. #--> you need to put functions and packages within loop.
  MSE_func(MCMCdir,
    OMdir = list.hcr[[h]]$OMdir, EMdir = list.hcr[[h]]$EMdir,
    StoreResults = list.hcr[[h]]$StoreResults, FRQ = 5,
    Btarg = "BMSY", Bconst = 1, Ftarg = list.hcr[[h]]$Ftarg,
    Fconst = list.hcr[[h]]$Fconst, a = list.hcr[[h]]$a, b = list.hcr[[h]]$b,
    niters = seq(from = 47, to = 245, by = 2),
    sourcedir = sourcedir, SR = "LFSR", implement = "MexRec", MaxCatch = NA
  )
}


stopCluster(c1)
