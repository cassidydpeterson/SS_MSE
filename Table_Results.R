###########################
### COllate MSE Results ###
### Oct 2020 ##############
### Updated: May 2021 #####
###########################


Results_HiMexRec <- c("Results_Base_HiMexRec", "Results_BH_HiMexRec", "Results_Hih_HiMexRec", "Results_Loh_HiMexRec", "Results_lnR0_HiMexRec", "Results_M_BH_HiMexRec")


BuildTable <- function(Results = c("Results_Base_HiMexRec", "Results_BH_HiMexRec", "Results_Hih_HiMexRec", "Results_Loh_HiMexRec", "Results_lnR0_HiMexRec", "Results_M_BH_HiMexRec"), stat = "median") {
  ### AAV ###
  # AAV = rbind(Results_BASE$AAV_HCR, Results_BH$AAV_HCR, Results_Hih$AAV_HCR, Results_Loh$AAV_HCR, Results_lnR0$AAV_HCR, Results_M_BH$AAV_HCR)
  # AAV
  AAV <- get(Results[1])$AAV_HCR
  for (r in 2:length(Results)) {
    AAV <- rbind(AAV, get(Results[r])$AAV_HCR)
  }




  ### Tot commercial catch ###
  # TotComCatch = rbind(Results_BASE$Com_Catch_cumulative, Results_BH$Com_Catch_cumulative, Results_Hih$Com_Catch_cumulative, Results_Loh$Com_Catch_cumulative, Results_lnR0$Com_Catch_cumulative, Results_M_BH$Com_Catch_cumulative)
  # TotComCatch
  TotComCatch <- get(Results[1])$Com_Catch_cumulative
  for (r in 2:length(Results)) {
    TotComCatch <- rbind(TotComCatch, get(Results[r])$Com_Catch_cumulative)
  }


  ### POF ###
  POF <- get(Results[1])$ProbOF
  for (r in 2:length(Results)) {
    POF <- rbind(POF, get(Results[r])$ProbOF)
  }




  ### SSB/SSBMSY ###
  # SSB_SSBMSY_2115 = rbind(Results_BASE$SSB_SSBMSY_2115, Results_BH$SSB_SSBMSY_2115, Results_Hih$SSB_SSBMSY_2115, Results_Loh$SSB_SSBMSY_2115, Results_lnR0$SSB_SSBMSY_2115, Results_M_BH$SSB_SSBMSY_2115)
  # dim(SSB_SSBMSY_2115)
  SSB_SSBMSY_2115 <- get(Results[1])$SSB_SSBMSY_2115
  for (r in 2:length(Results)) {
    SSB_SSBMSY_2115 <- rbind(SSB_SSBMSY_2115, get(Results[r])$SSB_SSBMSY_2115)
  }

  # SSB_SSBMSY_2070 = rbind(Results_BASE$SSB_SSBMSY_2070, Results_BH$SSB_SSBMSY_2070, Results_Hih$SSB_SSBMSY_2070, Results_Loh$SSB_SSBMSY_2070, Results_lnR0$SSB_SSBMSY_2070, Results_M_BH$SSB_SSBMSY_2070)
  # dim(SSB_SSBMSY_2070)
  SSB_SSBMSY_2070 <- get(Results[1])$SSB_SSBMSY_2070
  for (r in 2:length(Results)) {
    SSB_SSBMSY_2070 <- rbind(SSB_SSBMSY_2070, get(Results[r])$SSB_SSBMSY_2070)
  }




  ### F/FMSY ##
  # F_FMSY_2115 = rbind(Results_BASE$FM_FMMSY_2115, Results_BH$FM_FMMSY_2115, Results_Hih$FM_FMMSY_2115, Results_Loh$FM_FMMSY_2115, Results_lnR0$FM_FMMSY_2115, Results_M_BH$FM_FMMSY_2115)
  # dim(F_FMSY_2115)
  F_FMSY_2115 <- get(Results[1])$FM_FMMSY_2115
  for (r in 2:length(Results)) {
    F_FMSY_2115 <- rbind(F_FMSY_2115, get(Results[r])$FM_FMMSY_2115)
  }


  # F_FMSY_2070 = rbind(Results_BASE$FM_FMMSY_2070, Results_BH$FM_FMMSY_2070, Results_Hih$FM_FMMSY_2070, Results_Loh$FM_FMMSY_2070, Results_lnR0$FM_FMMSY_2070, Results_M_BH$FM_FMMSY_2070)
  # dim(F_FMSY_2070)
  # View(F_FMSY_2070)
  F_FMSY_2070 <- get(Results[1])$FM_FMMSY_2070
  for (r in 2:length(Results)) {
    F_FMSY_2070 <- rbind(F_FMSY_2070, get(Results[r])$FM_FMMSY_2070)
  }





  ### Avg len F ###
  # AvgLen_F_2070 = rbind(Results_BASE$AvgLen_F_2070, Results_BH$AvgLen_F_2070, Results_Hih$AvgLen_F_2070, Results_Loh$AvgLen_F_2070, Results_lnR0$AvgLen_F_2070, Results_M_BH$AvgLen_F_2070)
  # dim(AvgLen_F_2070)
  AvgLen_F_2070 <- get(Results[1])$AvgLen_F_2070
  for (r in 2:length(Results)) {
    AvgLen_F_2070 <- rbind(AvgLen_F_2070, get(Results[r])$AvgLen_F_2070)
  }


  # AvgLen_F_2115 = rbind(Results_BASE$AvgLen_F_2115, Results_BH$AvgLen_F_2115, Results_Hih$AvgLen_F_2115, Results_Loh$AvgLen_F_2115, Results_lnR0$AvgLen_F_2115, Results_M_BH$AvgLen_F_2115)
  # dim(AvgLen_F_2115)
  # View(AvgLen_F_2115)
  AvgLen_F_2115 <- get(Results[1])$AvgLen_F_2115
  for (r in 2:length(Results)) {
    AvgLen_F_2115 <- rbind(AvgLen_F_2115, get(Results[r])$AvgLen_F_2115)
  }



  ### prob Recov 2115 ###
  SSB_SSBMSY_2115
  Recov <- ifelse(SSB_SSBMSY_2115 > 1, 1, 0)


  ### prob Recov 2070 ###
  # SSB_SSBMSY_2070
  Recov2070 <- ifelse(SSB_SSBMSY_2070 > 1, 1, 0)




  ### BUILD TABLE ####

  if (stat == "median") {
    table <- rbind(
      "Prob of recovery by 2115" = round(apply(Recov, 2, sum, na.rm = T) / nrow(Recov), digits = 3),
      "Prob of recovery by 2070" = round(apply(Recov2070, 2, sum, na.rm = T) / nrow(Recov2070), digits = 3),
      "POF" = round(apply(POF, 2, median, na.rm = T), digits = 3),
      "total US com. catch" = round(apply(TotComCatch, 2, median, na.rm = T), digits = 2),
      "SSB2070 / SSBMSY" = round(apply(SSB_SSBMSY_2070, 2, median, na.rm = T), digits = 3),
      "SSB2115 / SSBMSY" = round(apply(SSB_SSBMSY_2115, 2, median, na.rm = T), digits = 3),
      "AAV" = round(apply(AAV, 2, median, na.rm = T), digits = 3),
      "F2070 / FMSY" = round(apply(F_FMSY_2070, 2, median, na.rm = T), digits = 3),
      "F2115 / FMSY" = round(apply(F_FMSY_2115, 2, median, na.rm = T), digits = 3),
      "Fem Length 2070" = round(apply(AvgLen_F_2070, 2, median, na.rm = T), digits = 2),
      "Fem Length 2115" = round(apply(AvgLen_F_2115, 2, median, na.rm = T), digits = 2)
    )
    # table <- round(table, digits=3)
  } # if stat==median

  if (stat == "mean") {
    table <- rbind(
      "Prob of recovery by 2115" = round(apply(Recov, 2, sum, na.rm = T) / nrow(Recov), digits = 3),
      "Prob of recovery by 2070" = round(apply(Recov2070, 2, sum, na.rm = T) / nrow(Recov2070), digits = 3),
      "total US com. catch" = round(apply(TotComCatch, 2, mean, na.rm = T), digits = 2),
      "POF" = round(apply(POF, 2, mean, na.rm = T), digits = 3),
      "SSB2070 / SSBMSY" = round(apply(SSB_SSBMSY_2070, 2, mean, na.rm = T), digits = 3),
      "SSB2115 / SSBMSY" = round(apply(SSB_SSBMSY_2115, 2, mean, na.rm = T), digits = 3),
      "AAV" = round(apply(AAV, 2, mean, na.rm = T), digits = 3),
      "F2070 / FMSY" = round(apply(F_FMSY_2070, 2, mean, na.rm = T), digits = 3),
      "F2115 / FMSY" = round(apply(F_FMSY_2115, 2, mean, na.rm = T), digits = 3),
      "Fem Length 2070" = round(apply(AvgLen_F_2070, 2, mean, na.rm = T), digits = 2),
      "Fem Length 2115" = round(apply(AvgLen_F_2115, 2, mean, na.rm = T), digits = 2)
    )
    # table <- round(table, digits=3)
  } # if stat==mean

  return(list("table" = table))
} # END FUNCTION


BuildTable()



Results_Concept <- c(
  "Results_Base_Concept", "Results_BH_Concept", "Results_Hih_Concept",
  "Results_Loh_Concept", "Results_lnR0_Concept", "Results_M_BH_Concept"
)
BuildTable(Results = Results_Concept)
BuildTable(Results = Results_Concept, stat = "mean")



Results_LoMexRec <- c(
  "Results_Base_LoMexRec", "Results_BH_LoMexRec", "Results_Hih_LoMexRec",
  "Results_Loh_LoMexRec", "Results_lnR0_LoMexRec", "Results_M_BH_LoMexRec"
)
BuildTable(Results = Results_LoMexRec)
BuildTable(Results = Results_LoMexRec, stat = "mean")


######### BUILD TABLE PLOT ######
### Results are differentiated by a C (conceptual), H (hi), and L (lo)

for (Imp in c("C", "H", "L")) {
  if (Imp == "C") {
    Results <- c(
      "Results_Base_Concept", "Results_BH_Concept", "Results_Hih_Concept",
      "Results_Loh_Concept", "Results_lnR0_Concept", "Results_M_BH_Concept"
    )
  }
  if (Imp == "H") {
    Results <- c(
      "Results_Base_HiMexRec", "Results_BH_HiMexRec", "Results_Hih_HiMexRec",
      "Results_Loh_HiMexRec", "Results_lnR0_HiMexRec", "Results_M_BH_HiMexRec"
    )
  }
  if (Imp == "L") {
    Results <- c(
      "Results_Base_LoMexRec", "Results_BH_LoMexRec", "Results_Hih_LoMexRec",
      "Results_Loh_LoMexRec", "Results_lnR0_LoMexRec", "Results_M_BH_LoMexRec"
    )
  }

  ### AAV ###
  AAV <- get(Results[1])$AAV_HCR
  for (r in 2:length(Results)) {
    AAV <- rbind(AAV, get(Results[r])$AAV_HCR)
  }
  ### AAV_ALL ###
  AAV_ALL <- get(Results[1])$AAV_all
  for (r in 2:length(Results)) {
    AAV_ALL <- rbind(AAV_ALL, get(Results[r])$AAV_all)
  }


  ### Tot commercial catch ###
  TotComCatch <- get(Results[1])$Com_Catch_cumulative
  for (r in 2:length(Results)) {
    TotComCatch <- rbind(TotComCatch, get(Results[r])$Com_Catch_cumulative)
  }




  ### SSB/SSBMSY ###
  SSB_SSBMSY_2115 <- get(Results[1])$SSB_SSBMSY_2115
  for (r in 2:length(Results)) {
    SSB_SSBMSY_2115 <- rbind(SSB_SSBMSY_2115, get(Results[r])$SSB_SSBMSY_2115)
  }

  SSB_SSBMSY_2070 <- get(Results[1])$SSB_SSBMSY_2070
  for (r in 2:length(Results)) {
    SSB_SSBMSY_2070 <- rbind(SSB_SSBMSY_2070, get(Results[r])$SSB_SSBMSY_2070)
  }




  ### F/FMSY ##
  F_FMSY_2115 <- get(Results[1])$FM_FMMSY_2115
  for (r in 2:length(Results)) {
    F_FMSY_2115 <- rbind(F_FMSY_2115, get(Results[r])$FM_FMMSY_2115)
  }


  F_FMSY_2070 <- get(Results[1])$FM_FMMSY_2070
  for (r in 2:length(Results)) {
    F_FMSY_2070 <- rbind(F_FMSY_2070, get(Results[r])$FM_FMMSY_2070)
  }





  ### Avg len F ###
  AvgLen_F_2070 <- get(Results[1])$AvgLen_F_2070
  for (r in 2:length(Results)) {
    AvgLen_F_2070 <- rbind(AvgLen_F_2070, get(Results[r])$AvgLen_F_2070)
  }


  AvgLen_F_2115 <- get(Results[1])$AvgLen_F_2115
  for (r in 2:length(Results)) {
    AvgLen_F_2115 <- rbind(AvgLen_F_2115, get(Results[r])$AvgLen_F_2115)
  }



  ### prob Recov 2115 ###
  SSB_SSBMSY_2115
  Recov <- ifelse(SSB_SSBMSY_2115 > 1, 1, 0)
  Recov2070 <- ifelse(SSB_SSBMSY_2070 > 1, 1, 0)


  assign(paste0("Recov_", Imp), Recov)
  assign(paste0("PRecov_", Imp), apply(Recov, 2, sum, na.rm = T) / nrow(Recov))
  assign(paste0("Recov2070_", Imp), Recov2070)
  assign(paste0("PRecov2070_", Imp), apply(Recov2070, 2, sum, na.rm = T) / nrow(Recov2070))
  assign(paste0("POF_", Imp), get(Results[1])$ProbOF)
  assign(paste0("TotComCatch_", Imp), TotComCatch)
  assign(paste0("Med_TotComCatch_", Imp), apply(TotComCatch, 2, median, na.rm = T))
  assign(paste0("SSB_SSBMSY_2070_", Imp), SSB_SSBMSY_2070)
  assign(paste0("SSB_SSBMSY_2115_", Imp), SSB_SSBMSY_2115)
  assign(paste0("Med_SSB_SSBMSY_2115_", Imp), apply(SSB_SSBMSY_2115, 2, median, na.rm = T))
  assign(paste0("AAV_", Imp), AAV)
  assign(paste0("AAV_ALL_", Imp), AAV_ALL)
  assign(paste0("Med_AAV_", Imp), apply(AAV, 2, median, na.rm = T))
  assign(paste0("F_FMSY_2070_", Imp), F_FMSY_2070)
  assign(paste0("F_FMSY_2115_", Imp), F_FMSY_2115)
  assign(paste0("Med_F_FMSY_2115_", Imp), apply(F_FMSY_2115, 2, median, na.rm = T))
  assign(paste0("AvgLen_F_2070_", Imp), AvgLen_F_2070)
  assign(paste0("AvgLen_F_2115_", Imp), AvgLen_F_2115)
  assign(paste0("Med_AvgLen_F_2115_", Imp), apply(AvgLen_F_2115, 2, median, na.rm = T))
} # end Imp loop


summary(PRecov2070_C)
summary(PRecov2070_H)
summary(PRecov2070_L)
summary(c(PRecov_C, PRecov_H, PRecov_L))
summary(PRecov_C)
summary(PRecov_L)
summary(PRecov_H)

# PLOT

png(
  filename = "Barplot_Decision_Table.png",
  type = "cairo",
  units = "mm",
  width = 300,
  height = 325,
  pointsize = 22,
  res = 600
)
#####
par(mfrow = c(8, 6), mar = c(0, 0, 0, 0), tcl = -0.1, mgp = c(0.5, 0, 0.01), cex = 1, oma = c(2.5, 2, 2, 0.1))

# Prob of recovery
barplot(as.table(rbind(PRecov_C[1:4], PRecov_L[1:4], PRecov_H[1:4])),
  beside = T, col = c("grey", "dodgerblue", "red"), border = NA,
  # density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
  ylim = c(0, 1), ylab = NULL, names.arg = NULL, axisnames = F, axes = F, cex.axis = 0.5
)
axis(2, cex.axis = 0.5)
axis(1, labels = FALSE)
abline(h = 0.7, col = "grey45")
box()
mtext(expression("PRecov"[2115]), 2, line = 0.75, cex = 0.7)
mtext(expression("F"["lim"] * "=F"["MSY"]), 3, line = 0, cex = 0.7)
for (i in seq(5, 21, by = 4)) {
  barplot(as.table(rbind(PRecov_C[i:(i + 3)], PRecov_L[i:(i + 3)], PRecov_H[i:(i + 3)])),
    beside = T, col = c("grey", "dodgerblue", "red"), border = NA,
    ylim = c(0, 1), ylab = NULL, axisnames = F, axes = F
  )
  axis(2, labels = F)
  box()
  abline(h = 0.7, col = "grey45")
  if (i == 5) {
    mtext(expression("F"["lim"] * "=M"), 3, line = 0, cex = 0.7)
  }
  if (i == 9) {
    mtext(expression("F"["lim"] * "=0.8M"), 3, line = 0, cex = 0.7)
  }
  if (i == 13) {
    mtext(expression("F"["lim"] * "=0.6M"), 3, line = 0, cex = 0.7)
  }
  if (i == 17) {
    mtext(expression("F"["lim"] * "=0.4M"), 3, line = 0, cex = 0.7)
  }
  if (i == 21) {
    mtext(expression("F"["lim"] * "=0.2M"), 3, line = 0, cex = 0.7)
  }
}

# mtext("Graphical Decision Table", side=3, line=1, cex=1, outer=T)


# Prob of recovery
barplot(as.table(rbind(PRecov2070_C[1:4], PRecov2070_L[1:4], PRecov2070_H[1:4])),
  beside = T, col = c("grey", "dodgerblue", "red"), border = NA,
  # density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
  ylim = c(0, 1), ylab = NULL, names.arg = NULL, axisnames = F, axes = F, cex.axis = 0.5
)
axis(2, cex.axis = 0.5)
axis(1, labels = FALSE)
abline(h = 0.7, col = "grey45")
box()
mtext(expression("PRecov"[2070]), 2, line = 0.75, cex = 0.7)
for (i in seq(5, 21, by = 4)) {
  barplot(as.table(rbind(PRecov2070_C[i:(i + 3)], PRecov2070_L[i:(i + 3)], PRecov2070_H[i:(i + 3)])),
    beside = T, col = c("grey", "dodgerblue", "red"), border = NA,
    ylim = c(0, 1), ylab = NULL, axisnames = F, axes = F
  )
  axis(2, labels = F)
  box()
  abline(h = 0.7, col = "grey45")
}




# POF
barplot(as.table(rbind(apply(POF_C, 2, median)[1:4], apply(POF_L, 2, median)[1:4], apply(POF_H, 2, median)[1:4])),
  beside = T, col = c("grey", "dodgerblue", "red"), border = NA,
  # density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
  ylim = c(0, 0.45), ylab = NULL, names.arg = NULL, axisnames = F, axes = F
)
axis(2, cex.axis = 0.5)
axis(1, labels = FALSE)
box()
mtext("POF", 2, line = 0.75, cex = 0.7)
for (i in seq(5, 21, by = 4)) {
  barplot(as.table(rbind(apply(POF_C, 2, median)[i:(i + 3)], apply(POF_L, 2, median)[i:(i + 3)], apply(POF_H, 2, median)[i:(i + 3)])),
    beside = T, col = c("grey", "dodgerblue", "red"), border = NA,
    ylim = c(0, 0.45), ylab = NULL, axisnames = F, axes = F
  )
  axis(2, labels = F)
  box()
}




# Total US Commercial catch
barplot(as.table(rbind(Med_TotComCatch_C[1:4], Med_TotComCatch_L[1:4], Med_TotComCatch_H[1:4])),
  beside = T, col = c("grey", "dodgerblue", "red"), border = NA,
  # density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
  ylim = c(0, 17000), ylab = NULL, names.arg = NULL, axisnames = F, axes = F
)
axis(2, cex.axis = 0.5)
axis(1, labels = FALSE)
box()
mtext("US Catch", 2, line = 0.75, cex = 0.7)
for (i in seq(5, 21, by = 4)) {
  barplot(as.table(rbind(Med_TotComCatch_C[i:(i + 3)], Med_TotComCatch_L[i:(i + 3)], Med_TotComCatch_H[i:(i + 3)])),
    beside = T, col = c("grey", "dodgerblue", "red"), border = NA,
    ylim = c(0, 17000), ylab = NULL, axisnames = F, axes = F
  )
  axis(2, labels = F)
  box()
}




# SSB / SSB_MSY
barplot(as.table(rbind(Med_SSB_SSBMSY_2115_C[1:4], Med_SSB_SSBMSY_2115_L[1:4], Med_SSB_SSBMSY_2115_H[1:4])),
  beside = T, col = c("grey", "dodgerblue", "red"), border = NA,
  # density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
  ylim = c(0.5, 1.5), ylab = NULL, names.arg = NULL, axisnames = F, axes = F
)
abline(h = 1, col = "grey45")
axis(2, cex.axis = 0.5)
axis(1, labels = FALSE)
box()
mtext(expression("SSB"[2115] * "/SSB"["MSY"]), 2, line = 0.75, cex = 0.7)
for (i in seq(5, 21, by = 4)) {
  barplot(as.table(rbind(Med_SSB_SSBMSY_2115_C[i:(i + 3)], Med_SSB_SSBMSY_2115_L[i:(i + 3)], Med_SSB_SSBMSY_2115_H[i:(i + 3)])),
    beside = T, col = c("grey", "dodgerblue", "red"), border = NA,
    ylim = c(0.5, 1.5), ylab = NULL, axisnames = F, axes = F
  )
  abline(h = 1, col = "grey45")
  axis(2, labels = F)
  box()
}




# F / F_MSY
barplot(as.table(rbind(Med_F_FMSY_2115_C[1:4], Med_F_FMSY_2115_L[1:4], Med_F_FMSY_2115_H[1:4])),
  beside = T, col = c("grey", "dodgerblue", "red"), border = NA,
  # density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
  ylim = c(0, 1.05), ylab = NULL, names.arg = NULL, axisnames = F, axes = F
)
abline(h = 1, col = "grey45")
axis(2, cex.axis = 0.5)
axis(1, labels = FALSE)
box()
mtext(expression("F"[2115] * "/F"["MSY"]), 2, line = 0.75, cex = 0.7)
for (i in seq(5, 21, by = 4)) {
  barplot(as.table(rbind(Med_F_FMSY_2115_C[i:(i + 3)], Med_F_FMSY_2115_L[i:(i + 3)], Med_F_FMSY_2115_H[i:(i + 3)])),
    beside = T, col = c("grey", "dodgerblue", "red"), border = NA,
    ylim = c(0, 1.05), ylab = NULL, axisnames = F, axes = F
  )
  abline(h = 1, col = "grey45")
  axis(2, labels = F)
  box()
}



# AAV
barplot(as.table(rbind(Med_AAV_C[1:4], Med_AAV_L[1:4], Med_AAV_H[1:4])),
  beside = T, col = c("grey", "dodgerblue", "red"), border = NA,
  # density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
  ylim = c(0, 0.5), ylab = NULL, names.arg = NULL, axisnames = F, axes = F
)
abline(h = 1)
axis(2, cex.axis = 0.5)
axis(1, labels = FALSE)
box()
mtext("AAV", 2, line = 0.75, cex = 0.7)
for (i in seq(5, 21, by = 4)) {
  barplot(as.table(rbind(Med_AAV_C[i:(i + 3)], Med_AAV_L[i:(i + 3)], Med_AAV_H[i:(i + 3)])),
    beside = T, col = c("grey", "dodgerblue", "red"), border = NA,
    ylim = c(0, 0.5), ylab = NULL, axisnames = F, axes = F
  )
  abline(h = 1)
  axis(2, labels = F)
  box()
}






# length
barplot(as.table(rbind(Med_AvgLen_F_2115_C[1:4], Med_AvgLen_F_2115_L[1:4], Med_AvgLen_F_2115_H[1:4])),
  beside = T, col = c("grey", "dodgerblue", "red"), border = NA,
  # density=c(-1, -1, -1, -1, -1, -1, 50,50,50,50,50,50),
  ylim = c(100, 105), ylab = NULL, names.arg = NULL, axes = F, cex.names = 0.5
)
abline(h = 1)
axis(2, cex.axis = 0.5)
axis(1, labels = F)
box()
mtext(expression("Avg Len"["2115"]), 2, line = 0.75, cex = 0.7)
for (i in seq(5, 21, by = 4)) {
  barplot(as.table(rbind(Med_AvgLen_F_2115_C[i:(i + 3)], Med_AvgLen_F_2115_L[i:(i + 3)], Med_AvgLen_F_2115_H[i:(i + 3)])),
    beside = T, col = c("grey", "dodgerblue", "red"), border = NA,
    ylim = c(100, 105), ylab = NULL, axes = F, cex.names = 0.5
  )
  abline(h = 1)
  axis(2, labels = F)
  box()
}




add_legend <- function(...) {
  opar <- par(
    fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0),
    mar = c(0, 0, 0, 0), new = TRUE
  )
  on.exit(par(opar))
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend(...)
}

add_legend("bottom",
  legend = c("Concept", "LoMexRec", "HiMexRec"), pch = 15,
  col = c("grey", "dodgerblue", "red"),
  horiz = TRUE, bty = "n", cex = 0.7, pt.cex = 2, inset = c(0.2, 0)
)

##########
dev.off()











Results <- Results_HiMexRec
SSB_SSBMSY_2115 <- get(Results[1])$SSB_SSBMSY_2115
for (r in 2:length(Results)) {
  SSB_SSBMSY_2115 <- rbind(SSB_SSBMSY_2115, get(Results[r])$SSB_SSBMSY_2115)
}
Recov <- ifelse(SSB_SSBMSY_2115 > 1, 1, 0)
HMR_Recov_tot <- sum(Recov, na.rm = T) / sum(apply(Recov, 2, function(x) length(which(!is.na(x)))))


Results <- Results_LoMexRec
SSB_SSBMSY_2115 <- get(Results[1])$SSB_SSBMSY_2115
for (r in 2:length(Results)) {
  SSB_SSBMSY_2115 <- rbind(SSB_SSBMSY_2115, get(Results[r])$SSB_SSBMSY_2115)
}
Recov <- ifelse(SSB_SSBMSY_2115 > 1, 1, 0)
LMR_Recov_tot <- sum(Recov, na.rm = T) / sum(apply(Recov, 2, function(x) length(which(!is.na(x)))))


Results <- Results_Concept
SSB_SSBMSY_2115 <- get(Results[1])$SSB_SSBMSY_2115
for (r in 2:length(Results)) {
  SSB_SSBMSY_2115 <- rbind(SSB_SSBMSY_2115, get(Results[r])$SSB_SSBMSY_2115)
}
Recov <- ifelse(SSB_SSBMSY_2115 > 1, 1, 0)
Con_Recov_tot <- sum(Recov, na.rm = T) / sum(apply(Recov, 2, function(x) length(which(!is.na(x)))))



HMR_Recov_tot
LMR_Recov_tot
Con_Recov_tot







Results <- Results_HiMexRec
SSB_SSBMSY_2070 <- get(Results[1])$SSB_SSBMSY_2070
for (r in 2:length(Results)) {
  SSB_SSBMSY_2070 <- rbind(SSB_SSBMSY_2070, get(Results[r])$SSB_SSBMSY_2070)
}
Recov2070 <- ifelse(SSB_SSBMSY_2070 > 1, 1, 0)
HMR_Recov_tot2070 <- sum(Recov2070, na.rm = T) / sum(apply(Recov2070, 2, function(x) length(which(!is.na(x)))))


Results <- Results_LoMexRec
SSB_SSBMSY_2070 <- get(Results[1])$SSB_SSBMSY_2070
for (r in 2:length(Results)) {
  SSB_SSBMSY_2070 <- rbind(SSB_SSBMSY_2070, get(Results[r])$SSB_SSBMSY_2070)
}
Recov2070 <- ifelse(SSB_SSBMSY_2070 > 1, 1, 0)
LMR_Recov_tot2070 <- sum(Recov2070, na.rm = T) / sum(apply(Recov2070, 2, function(x) length(which(!is.na(x)))))


Results <- Results_Concept
SSB_SSBMSY_2070 <- get(Results[1])$SSB_SSBMSY_2070
for (r in 2:length(Results)) {
  SSB_SSBMSY_2070 <- rbind(SSB_SSBMSY_2070, get(Results[r])$SSB_SSBMSY_2070)
}
Recov2070 <- ifelse(SSB_SSBMSY_2070 > 1, 1, 0)
Con_Recov_tot2070 <- sum(Recov2070, na.rm = T) / sum(apply(Recov2070, 2, function(x) length(which(!is.na(x)))))



HMR_Recov_tot2070
LMR_Recov_tot2070
Con_Recov_tot2070
