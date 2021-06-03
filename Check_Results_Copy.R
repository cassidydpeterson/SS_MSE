wd<- getwd()


##### Base  #####
curr_WD = "D:\\MSE_Run\\MSE_Results\\OM_Base_HiMexRec"
Base_HMR_count = vector(length=24)
names(Base_HMR_count)<- paste("HCR",1:24)
for(hcr in 1:24){
  
  lst_files = list.files(path=file.path(curr_WD, paste0("HCR",hcr),"StoreResults") )
  Base_HMR_count[hcr] <- length(lst_files)
}
Base_HMR_count


curr_WD = "D:\\MSE_Run\\MSE_Results\\OM_Base_Concept"
Base_Conc_count = vector(length=24)
names(Base_Conc_count)<- paste("HCR",1:24)
for(hcr in 1:24){
  
  lst_files = list.files(path=file.path(curr_WD, paste0("HCR",hcr),"StoreResults") )
  Base_Conc_count[hcr] <- length(lst_files)
}
Base_Conc_count



curr_WD = "D:\\MSE_Run\\MSE_Results\\OM_Base_LoMexRec"
Base_LMR_count = vector(length=24)
names(Base_LMR_count)<- paste("HCR",1:24)
for(hcr in 1:24){
  
  lst_files = list.files(path=file.path(curr_WD, paste0("HCR",hcr),"StoreResults") )
  Base_LMR_count[hcr] <- length(lst_files)
}
Base_LMR_count


####### BH ########

curr_WD = "D:\\MSE_Run\\MSE_Results\\OM_BH_HiMexRec"
BH_HMR_count = vector(length=24)
names(BH_HMR_count)<- paste("HCR",1:24)
for(hcr in 1:24){
  
  lst_files = list.files(path=file.path(curr_WD, paste0("HCR",hcr),"StoreResults") )
  BH_HMR_count[hcr] <- length(lst_files)
}
BH_HMR_count


curr_WD = "D:\\MSE_Run\\MSE_Results\\OM_BH_Concept"
BH_Conc_count = vector(length=24)
names(BH_Conc_count)<- paste("HCR",1:24)
for(hcr in 1:24){
  
  lst_files = list.files(path=file.path(curr_WD, paste0("HCR",hcr),"StoreResults") )
  BH_Conc_count[hcr] <- length(lst_files)
}
BH_Conc_count



curr_WD = "D:\\MSE_Run\\MSE_Results\\OM_BH_LoMexRec"
BH_LMR_count = vector(length=24)
names(BH_LMR_count)<- paste("HCR",1:24)
for(hcr in 1:24){
  
  lst_files = list.files(path=file.path(curr_WD, paste0("HCR",hcr),"StoreResults") )
  BH_LMR_count[hcr] <- length(lst_files)
}
BH_LMR_count


###### Hih ########

curr_WD = "D:\\MSE_Run\\MSE_Results\\OM_Hih_HiMexRec"
Hih_HMR_count = vector(length=24)
names(Hih_HMR_count)<- paste("HCR",1:24)
for(hcr in 1:24){
  
  lst_files = list.files(path=file.path(curr_WD, paste0("HCR",hcr),"StoreResults") )
  Hih_HMR_count[hcr] <- length(lst_files)
}
Hih_HMR_count


curr_WD = "D:\\MSE_Run\\MSE_Results\\OM_Hih_Concept"
Hih_Conc_count = vector(length=24)
names(Hih_Conc_count)<- paste("HCR",1:24)
for(hcr in 1:24){
  
  lst_files = list.files(path=file.path(curr_WD, paste0("HCR",hcr),"StoreResults") )
  Hih_Conc_count[hcr] <- length(lst_files)
}
Hih_Conc_count



curr_WD = "D:\\MSE_Run\\MSE_Results\\OM_Hih_LoMexRec"
Hih_LMR_count = vector(length=24)
names(Hih_LMR_count)<- paste("HCR",1:24)
for(hcr in 1:24){
  
  lst_files = list.files(path=file.path(curr_WD, paste0("HCR",hcr),"StoreResults") )
  Hih_LMR_count[hcr] <- length(lst_files)
}
Hih_LMR_count



### lnR0 ###############


curr_WD = "D:\\MSE_Run\\MSE_Results\\OM_lnR0_HiMexRec"
lnR0_HMR_count = vector(length=24)
names(lnR0_HMR_count)<- paste("HCR",1:24)
for(hcr in 1:24){
  
  lst_files = list.files(path=file.path(curr_WD, paste0("HCR",hcr),"StoreResults") )
  lnR0_HMR_count[hcr] <- length(lst_files)
}
lnR0_HMR_count


curr_WD = "D:\\MSE_Run\\MSE_Results\\OM_lnR0_Concept"
lnR0_Conc_count = vector(length=24)
names(lnR0_Conc_count)<- paste("HCR",1:24)
for(hcr in 1:24){
  
  lst_files = list.files(path=file.path(curr_WD, paste0("HCR",hcr),"StoreResults") )
  lnR0_Conc_count[hcr] <- length(lst_files)
}
lnR0_Conc_count



curr_WD = "D:\\MSE_Run\\MSE_Results\\OM_lnR0_LoMexRec"
lnR0_LMR_count = vector(length=24)
names(lnR0_LMR_count)<- paste("HCR",1:24)
for(hcr in 1:24){
  
  lst_files = list.files(path=file.path(curr_WD, paste0("HCR",hcr),"StoreResults") )
  lnR0_LMR_count[hcr] <- length(lst_files)
}
lnR0_LMR_count


# Loh ###############

curr_WD = "D:\\MSE_Run\\MSE_Results\\OM_Loh_HiMexRec"
Loh_HMR_count = vector(length=24)
names(Loh_HMR_count)<- paste("HCR",1:24)
for(hcr in 1:24){
  
  lst_files = list.files(path=file.path(curr_WD, paste0("HCR",hcr),"StoreResults") )
  Loh_HMR_count[hcr] <- length(lst_files)
}
Loh_HMR_count


curr_WD = "D:\\MSE_Run\\MSE_Results\\OM_Loh_Concept"
Loh_Conc_count = vector(length=24)
names(Loh_Conc_count)<- paste("HCR",1:24)
for(hcr in 1:24){
  
  lst_files = list.files(path=file.path(curr_WD, paste0("HCR",hcr),"StoreResults") )
  Loh_Conc_count[hcr] <- length(lst_files)
}
Loh_Conc_count



curr_WD = "D:\\MSE_Run\\MSE_Results\\OM_Loh_LoMexRec"
Loh_LMR_count = vector(length=24)
names(Loh_LMR_count)<- paste("HCR",1:24)
for(hcr in 1:24){
  
  lst_files = list.files(path=file.path(curr_WD, paste0("HCR",hcr),"StoreResults") )
  Loh_LMR_count[hcr] <- length(lst_files)
}
Loh_LMR_count


# M_BH ###############

curr_WD = "D:\\MSE_Run\\MSE_Results\\OM_M_BH_HiMexRec"
M_BH_HMR_count = vector(length=24)
names(M_BH_HMR_count)<- paste("HCR",1:24)
for(hcr in 1:24){
  
  lst_files = list.files(path=file.path(curr_WD, paste0("HCR",hcr),"StoreResults") )
  M_BH_HMR_count[hcr] <- length(lst_files)
}
M_BH_HMR_count


curr_WD = "D:\\MSE_Run\\MSE_Results\\OM_M_BH_Concept"
M_BH_Conc_count = vector(length=24)
names(M_BH_Conc_count)<- paste("HCR",1:24)
for(hcr in 1:24){
  
  lst_files = list.files(path=file.path(curr_WD, paste0("HCR",hcr),"StoreResults") )
  M_BH_Conc_count[hcr] <- length(lst_files)
}
M_BH_Conc_count



curr_WD = "D:\\MSE_Run\\MSE_Results\\OM_M_BH_LoMexRec"
M_BH_LMR_count = vector(length=24)
names(M_BH_LMR_count)<- paste("HCR",1:24)
for(hcr in 1:24){
  
  lst_files = list.files(path=file.path(curr_WD, paste0("HCR",hcr),"StoreResults") )
  M_BH_LMR_count[hcr] <- length(lst_files)
}
M_BH_LMR_count


#### DISPLAY ####
cbind(Base_HMR_count, Base_Conc_count, Base_LMR_count)
cbind(BH_HMR_count, BH_Conc_count, BH_LMR_count)
cbind(Hih_HMR_count, Hih_Conc_count, Hih_LMR_count)
cbind(lnR0_HMR_count, lnR0_Conc_count, lnR0_LMR_count)
cbind(Loh_HMR_count, Loh_Conc_count, Loh_LMR_count)
cbind(M_BH_HMR_count, M_BH_Conc_count, M_BH_LMR_count)

setwd(wd)
