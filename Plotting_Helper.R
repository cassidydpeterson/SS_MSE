library(fmsb)
library(vioplot)

iters = seq(47,245, by=2)
years = 1960:2115
col_list = c(rgb(0.75, 0.75, 0.75, 0.20),rgb(0.33, 0.80, 0.92, 0.15),rgb(0.79, 0.9, 0.44, 0.3),rgb(0.86, 0.44, 0.84, 0.2))
col_list2 = c("black","deepskyblue3","forestgreen","darkorchid")
lty_list = c(2,1,2,1)

col_lista = rep(col_list, (24/4))
col_list2a = rep(col_list2, (24/4))
lty_lista = rep(lty_list, (24/4))


plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
legend("center",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                    expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
       col=col_list2, lty=lty_list, lwd=3, cex=1.5, bty='n')

#####


######################### HiMexRec ################################

####### Worm Plots -----

# SSB/SSBMSY
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_HiMexRec_WORM_SSBSSBMSY.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
# OM_list = c('OM_BASE','OM_BH','OM_M_BH','OM_Hih','OM_Loh','OM_lnR0')
OM_labs = c("Base","BH","Hih","Loh", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { OM_Plot=OM_Base_HiMexRec } 
  if(o==2) { OM_Plot=OM_BH_HiMexRec } 
  if(o==3) { OM_Plot=OM_Hih_HiMexRec } 
  if(o==4) { OM_Plot=OM_Loh_HiMexRec } 
  if(o==5) { OM_Plot=OM_lnR0_HiMexRec }
  if(o==6) { OM_Plot=OM_M_BH_HiMexRec } 
  for(k in 1:(24/4)){
    kh = k*4
    hlist=c(kh, kh-1, kh-2, kh-3)
    plot(years, OM_Plot$HCR_1$SSB_SSBMSY[,1], type='l', col="white", ylim=c(0, 2.5), ylab="")
    if(k==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(o==1 & k==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(o==1 & k==2) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==3) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==4) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==5) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==6) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==6) { legend("topright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                          expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                             col=col_list2, lty=lty_list, cex=0.7, bty='n') }
    abline(h=1)
    for(h in hlist){
      for(i in 1:dim(OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY)[2]){
        lines(years, OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY[,i], col=col_lista[h])
      } # end i loop
    } # end h loop
    for(h in hlist){
      lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY, 1, median, na.rm=T), type='l', lwd=2, col=col_list2a[h], lty=lty_lista[h])
    } # end h loop
    
  } # end k loop
} # end o loop
mtext(expression("SSB/SSB"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.75)
mtext("HiMexRec", side=3, outer = TRUE, cex = 1, line=1)

#####
dev.off()



# F/FMSY
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_HiMexRec_WORM_FFMSY.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
# OM_list = c('OM_BASE','OM_BH','OM_M_BH','OM_Hih','OM_Loh','OM_lnR0')
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { OM_Plot=OM_Base_HiMexRec } 
  if(o==2) { OM_Plot=OM_BH_HiMexRec } 
  if(o==3) { OM_Plot=OM_Hih_HiMexRec } 
  if(o==4) { OM_Plot=OM_Loh_HiMexRec } 
  if(o==5) { OM_Plot=OM_lnR0_HiMexRec }
  if(o==6) { OM_Plot=OM_M_BH_HiMexRec } 
  for(k in 1:(24/4)){
    kh = k*4
    hlist=c(kh, kh-1, kh-2, kh-3)
    plot(years, OM_Plot$HCR_1$FM_FMMSY[,1], type='l', col="white", ylim=c(0, 4), ylab="")
    if(k==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(o==1 & k==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0.15, cex=0.8) }
    if(o==1 & k==2) { mtext(expression("F"["lim"]*"= M"), side=3, line=0.15, cex=0.8) }
    if(o==1 & k==3) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0.15, cex=0.8) }
    if(o==1 & k==4) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0.15, cex=0.8) }
    if(o==1 & k==5) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0.15, cex=0.8) }
    if(o==1 & k==6) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0.15, cex=0.8) }
    if(o==1 & k==6) { legend("topright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                          expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                             col=col_list2, lty=lty_list, cex=0.7, bty='n') }
    abline(h=1)
    for(h in hlist){
      for(i in 1:dim(OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY)[2]){
        lines(years, OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY[,i], col=col_lista[h])
      } # end i loop
    } # end h loop
    abline(h=1)
    for(h in hlist){
      lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY, 1, median, na.rm=T), type='l', lwd=2, col=col_list2a[h], lty=lty_lista[h])
    } # end h loop
  } # end k loop
} # end o loop
mtext(expression("F/F"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.75)
mtext("HiMexRec", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()




# Commercial Catch
# F/FMSY
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_HiMexRec_WORM_ComCatch.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
# OM_list = c('OM_BASE','OM_BH','OM_M_BH','OM_Hih','OM_Loh','OM_lnR0')
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { OM_Plot=OM_Base_HiMexRec } 
  if(o==2) { OM_Plot=OM_BH_HiMexRec } 
  if(o==3) { OM_Plot=OM_Hih_HiMexRec } 
  if(o==4) { OM_Plot=OM_Loh_HiMexRec } 
  if(o==5) { OM_Plot=OM_lnR0_HiMexRec }
  if(o==6) { OM_Plot=OM_M_BH_HiMexRec } 
  for(k in 1:(24/4)){
    kh = k*4
    hlist=c(kh, kh-1, kh-2, kh-3)
    plot(years, OM_Plot$HCR_1$Com_catch[,1], type='l', col="white", ylim=c(0, 500), ylab="")
    if(k==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(o==1 & k==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0.15, cex=0.8) }
    if(o==1 & k==2) { mtext(expression("F"["lim"]*"= M"), side=3, line=0.15, cex=0.8) }
    if(o==1 & k==3) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0.15, cex=0.8) }
    if(o==1 & k==4) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0.15, cex=0.8) }
    if(o==1 & k==5) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0.15, cex=0.8) }
    if(o==1 & k==6) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0.15, cex=0.8) }
    if(o==1 & k==6) { legend("topright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                          expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                             col=col_list2, lty=lty_list, cex=0.7, bty='n') }
    abline(h=1)
    for(h in hlist){
      for(i in 1:dim(OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY)[2]){
        lines(years, OM_Plot[[paste0("HCR_",h)]]$Com_catch[,i], col=col_lista[h])
      } # end i loop
    } # end h loop
    abline(h=1)
    for(h in hlist){
      lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$Com_catch, 1, median, na.rm=T), type='l', lwd=2, col=col_list2a[h], lty=lty_lista[h])
    } # end h loop
  } # end k loop
} # end o loop
mtext("US Commercial Catch"  , side=2, outer = TRUE, cex = 1, line=0.75)
mtext("HiMexRec", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()





### VIOLIN PLOTS -----
# SSB/SSBMSY #
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_HiMexRec_VIOPLOT_SSBMSY2115.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_Base_HiMexRec } 
  if(o==2) { Results=Results_BH_HiMexRec } 
  if(o==3) { Results=Results_Hih_HiMexRec } 
  if(o==4) { Results=Results_Loh_HiMexRec } 
  if(o==5) { Results=Results_lnR0_HiMexRec }
  if(o==6) { Results=Results_M_BH_HiMexRec } 
  for(r in seq(1, 24, by=4)){
    vioplot(Results$SSB_SSBMSY_2115[,r], Results$SSB_SSBMSY_2115[,(r+1)], Results$SSB_SSBMSY_2115[,(r+2)], Results$SSB_SSBMSY_2115[,(r+3)], 
            col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 1.5), names=c(rep("", 4)) )
    abline(h=1)
    if(r==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(r==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(r==5) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(r==9) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(r==13) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(r==17) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(r==21) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(r==21 & o==6) { legend("bottomright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                              expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                              fill=col_list2, cex=0.7, bty='n') }
  } # end r loop
  # mtext(OM_Name, outer = TRUE, cex = 1.5, line=0)
} # end o loop
mtext(expression("SSB"[2115]*"/SSB"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.8)
mtext("HiMexRec", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()


# SSB/SSBMSY 2070#
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_HiMexRec_VIOPLOT_SSBMSY2070.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_Base_HiMexRec } 
  if(o==2) { Results=Results_BH_HiMexRec } 
  if(o==3) { Results=Results_Hih_HiMexRec } 
  if(o==4) { Results=Results_Loh_HiMexRec } 
  if(o==5) { Results=Results_lnR0_HiMexRec }
  if(o==6) { Results=Results_M_BH_HiMexRec } 
  for(r in seq(1, 24, by=4)){
    vioplot(Results$SSB_SSBMSY_2070[,r], Results$SSB_SSBMSY_2070[,(r+1)], Results$SSB_SSBMSY_2070[,(r+2)], Results$SSB_SSBMSY_2070[,(r+3)], 
            col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 1.5), names=c(rep("", 4)) )
    abline(h=1)
    if(r==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(r==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(r==5) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(r==9) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(r==13) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(r==17) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(r==21) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(r==21 & o==6) { legend("bottomright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                              expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                              fill=col_list2, cex=0.7, bty='n') }
  } # end r loop
  # mtext(OM_Name, outer = TRUE, cex = 1.5, line=0)
} # end o loop
mtext(expression("SSB"[2070]*"/SSB"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.8)
mtext("HiMexRec", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()



# F/FMSY 2115 #
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_HiMexRec_VIOPLOT_FMSY2115.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_Base_HiMexRec } 
  if(o==2) { Results=Results_BH_HiMexRec } 
  if(o==3) { Results=Results_Hih_HiMexRec } 
  if(o==4) { Results=Results_Loh_HiMexRec } 
  if(o==5) { Results=Results_lnR0_HiMexRec }
  if(o==6) { Results=Results_M_BH_HiMexRec } 
  for(r in seq(1, 24, by=4)){
    vioplot(Results$FM_FMMSY_2115[,r], Results$FM_FMMSY_2115[,(r+1)], Results$FM_FMMSY_2115[,(r+2)], Results$FM_FMMSY_2115[,(r+3)], 
            col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 2), names=c(rep("", 4)) )
    abline(h=1)
    if(r==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(r==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(r==5) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(r==9) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(r==13) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(r==17) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(r==21) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(r==21 & o==6) { legend("topright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                           expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                              fill=col_list2, cex=0.7, bty='n') }
  } # end r loop
  # mtext(OM_Name, outer = TRUE, cex = 1.5, line=0)
} # end o loop
mtext(expression("F"[2115]*"/F"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.8)
mtext("HiMexRec", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()




# F/FMSY 2070 #
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_HiMexRec_VIOPLOT_FMSY2070.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_Base_HiMexRec } 
  if(o==2) { Results=Results_BH_HiMexRec } 
  if(o==3) { Results=Results_Hih_HiMexRec } 
  if(o==4) { Results=Results_Loh_HiMexRec } 
  if(o==5) { Results=Results_lnR0_HiMexRec }
  if(o==6) { Results=Results_M_BH_HiMexRec } 
  for(r in seq(1, 24, by=4)){
    vioplot(Results$FM_FMMSY_2070[,r], Results$FM_FMMSY_2070[,(r+1)], Results$FM_FMMSY_2070[,(r+2)], Results$FM_FMMSY_2070[,(r+3)], 
            col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 4), names=c(rep("", 4)) )
    abline(h=1)
    if(r==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(r==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(r==5) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(r==9) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(r==13) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(r==17) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(r==21) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(r==21 & o==6) { legend("bottomright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                              expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                              fill=col_list2, cex=0.7, bty='n') }
  } # end r loop
  # mtext(OM_Name, outer = TRUE, cex = 1.5, line=0)
} # end o loop
mtext(expression("F"[2070]*"/F"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.8)
mtext("HiMexRec", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()






# POF #
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_HiMexRec_VIOPLOT_POF.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_Base_HiMexRec } 
  if(o==2) { Results=Results_BH_HiMexRec } 
  if(o==3) { Results=Results_Hih_HiMexRec } 
  if(o==4) { Results=Results_Loh_HiMexRec } 
  if(o==5) { Results=Results_lnR0_HiMexRec }
  if(o==6) { Results=Results_M_BH_HiMexRec } 
  for(r in seq(1, 24, by=4)){
    vioplot(Results$ProbOF[,r], Results$ProbOF[,(r+1)], Results$ProbOF[,(r+2)], Results$ProbOF[,(r+3)], 
            col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 1), names=c(rep("", 4)) )
    abline(h=1)
    if(r==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(r==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(r==5) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(r==9) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(r==13) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(r==17) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(r==21) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(r==21 & o==6) { legend("bottomright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                              expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                              fill=col_list2, cex=0.7, bty='n') }
  } # end r loop
  # mtext(OM_Name, outer = TRUE, cex = 1.5, line=0)
} # end o loop
mtext("POF"  , side=2, outer = TRUE, cex = 1, line=0.8)
mtext("HiMexRec", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()





# AAV #
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_HiMexRec_VIOPLOT_AAV.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_Base_HiMexRec } 
  if(o==2) { Results=Results_BH_HiMexRec } 
  if(o==3) { Results=Results_Hih_HiMexRec } 
  if(o==4) { Results=Results_Loh_HiMexRec } 
  if(o==5) { Results=Results_lnR0_HiMexRec }
  if(o==6) { Results=Results_M_BH_HiMexRec } 
  for(r in seq(1, 24, by=4)){
    vioplot(Results$AAV_all[,r], Results$AAV_all[,(r+1)], Results$AAV_all[,(r+2)], Results$AAV_all[,(r+3)], 
            col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 1), names=c(rep("", 4)) )
    abline(h=1)
    if(r==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(r==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(r==5) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(r==9) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(r==13) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(r==17) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(r==21) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(r==21 & o==6) { legend("bottomright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                              expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                              fill=col_list2, cex=0.7, bty='n') }
  } # end r loop
  # mtext(OM_Name, outer = TRUE, cex = 1.5, line=0)
} # end o loop
mtext("AAV"  , side=2, outer = TRUE, cex = 1, line=0.8)
mtext("HiMexRec", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()






######################### LoMexRec ################################

####### Worm Plots -----

# SSB/SSBMSY
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_LoMexRec_WORM_SSBSSBMSY.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
# OM_list = c('OM_BASE','OM_BH','OM_M_BH','OM_Hih','OM_Loh','OM_lnR0')
OM_labs = c("Base","BH","Hih","Loh", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { OM_Plot=OM_Base_LoMexRec } 
  if(o==2) { OM_Plot=OM_BH_LoMexRec } 
  if(o==3) { OM_Plot=OM_Hih_LoMexRec } 
  if(o==4) { OM_Plot=OM_Loh_LoMexRec } 
  if(o==5) { OM_Plot=OM_lnR0_LoMexRec }
  if(o==6) { OM_Plot=OM_M_BH_LoMexRec } 
  for(k in 1:(24/4)){
    kh = k*4
    hlist=c(kh, kh-1, kh-2, kh-3)
    plot(years, OM_Plot$HCR_1$SSB_SSBMSY[,1], type='l', col="white", ylim=c(0, 2.5), ylab="")
    if(k==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(o==1 & k==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(o==1 & k==2) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==3) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==4) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==5) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==6) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==6) { legend("topright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                          expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                             col=col_list2, lty=lty_list, cex=0.7, bty='n') }
    abline(h=1)
    for(h in hlist){
      for(i in 1:dim(OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY)[2]){
        lines(years, OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY[,i], col=col_lista[h])
      } # end i loop
    } # end h loop
    for(h in hlist){
      lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY, 1, median, na.rm=T), type='l', lwd=2, col=col_list2a[h], lty=lty_lista[h])
    } # end h loop
  } # end k loop
} # end o loop
mtext(expression("SSB/SSB"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.75)
mtext("LoMexRec", side=3, outer = TRUE, cex = 1, line=1)

#####
dev.off()




# F/FMSY
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_LoMexRec_WORM_FFMSY.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
# OM_list = c('OM_BASE','OM_BH','OM_M_BH','OM_Hih','OM_Loh','OM_lnR0')
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { OM_Plot=OM_Base_LoMexRec } 
  if(o==2) { OM_Plot=OM_BH_LoMexRec } 
  if(o==3) { OM_Plot=OM_Hih_LoMexRec } 
  if(o==4) { OM_Plot=OM_Loh_LoMexRec } 
  if(o==5) { OM_Plot=OM_lnR0_LoMexRec }
  if(o==6) { OM_Plot=OM_M_BH_LoMexRec } 
  for(k in 1:(24/4)){
    kh = k*4
    hlist=c(kh, kh-1, kh-2, kh-3)
    plot(years, OM_Plot$HCR_1$FM_FMMSY[,1], type='l', col="white", ylim=c(0, 4), ylab="")
    if(k==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(o==1 & k==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(o==1 & k==2) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==3) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==4) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==5) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==6) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==6) { legend("topright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                          expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                             col=col_list2, lty=lty_list, cex=0.7, bty='n') }
    abline(h=1)
    for(h in hlist){
      for(i in 1:dim(OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY)[2]){
        lines(years, OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY[,i], col=col_lista[h])
      } # end i loop
    } # end h loop
    abline(h=1)
    for(h in hlist){
      lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY, 1, median, na.rm=T), type='l', lwd=2, col=col_list2a[h], lty=lty_lista[h])
    } # end h loop
  } # end k loop
} # end o loop
mtext(expression("F/F"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.75)
mtext("LoMexRec", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()




# Commercial Catch
# F/FMSY
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_LoMexRec_WORM_ComCatch.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
# OM_list = c('OM_BASE','OM_BH','OM_M_BH','OM_Hih','OM_Loh','OM_lnR0')
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { OM_Plot=OM_Base_LoMexRec } 
  if(o==2) { OM_Plot=OM_BH_LoMexRec } 
  if(o==3) { OM_Plot=OM_Hih_LoMexRec } 
  if(o==4) { OM_Plot=OM_Loh_LoMexRec } 
  if(o==5) { OM_Plot=OM_lnR0_LoMexRec }
  if(o==6) { OM_Plot=OM_M_BH_LoMexRec } 
  for(k in 1:(24/4)){
    kh = k*4
    hlist=c(kh, kh-1, kh-2, kh-3)
    plot(years, OM_Plot$HCR_1$Com_catch[,1], type='l', col="white", ylim=c(0, 500), ylab="")
    if(k==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(o==1 & k==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(o==1 & k==2) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==3) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==4) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==5) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==6) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==6) { legend("topright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                          expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                             col=col_list2, lty=lty_list, cex=0.7, bty='n') }
    abline(h=1)
    for(h in hlist){
      for(i in 1:dim(OM_Plot[[paste0("HCR_",h)]]$Com_catch)[2]){
        lines(years, OM_Plot[[paste0("HCR_",h)]]$Com_catch[,i], col=col_lista[h])
      } # end i loop
    } # end h loop
    abline(h=1)
    for(h in hlist){
      lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$Com_catch, 1, median, na.rm=T), type='l', lwd=2, col=col_list2a[h], lty=lty_lista[h])
    } # end h loop
  } # end k loop
} # end o loop
mtext("US Commercial Catch"  , side=2, outer = TRUE, cex = 1, line=0.75)
mtext("LoMexRec", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()





### VIOLIN PLOTS -----
# SSB/SSBMSY #
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_LoMexRec_VIOPLOT_SSBMSY2115.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_Base_LoMexRec } 
  if(o==2) { Results=Results_BH_LoMexRec } 
  if(o==3) { Results=Results_Hih_LoMexRec } 
  if(o==4) { Results=Results_Loh_LoMexRec } 
  if(o==5) { Results=Results_lnR0_LoMexRec }
  if(o==6) { Results=Results_M_BH_LoMexRec } 
  for(r in seq(1, 24, by=4)){
    vioplot(Results$SSB_SSBMSY_2115[,r], Results$SSB_SSBMSY_2115[,(r+1)], Results$SSB_SSBMSY_2115[,(r+2)], Results$SSB_SSBMSY_2115[,(r+3)], 
            col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 1.5), names=c(rep("", 4)) )
    abline(h=1)
    if(r==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(r==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(r==5) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(r==9) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(r==13) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(r==17) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(r==21) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(r==21 & o==6) { legend("bottomright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                              expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                              fill=col_list2, cex=0.7, bty='n') }
  } # end r loop
  # mtext(OM_Name, outer = TRUE, cex = 1.5, line=0)
} # end o loop
mtext(expression("SSB"[2115]*"/SSB"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.8)
mtext("LoMexRec", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()


# SSB/SSBMSY 2070#
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_LoMexRec_VIOPLOT_SSBMSY2070.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_Base_LoMexRec } 
  if(o==2) { Results=Results_BH_LoMexRec } 
  if(o==3) { Results=Results_Hih_LoMexRec } 
  if(o==4) { Results=Results_Loh_LoMexRec } 
  if(o==5) { Results=Results_lnR0_LoMexRec }
  if(o==6) { Results=Results_M_BH_LoMexRec } 
  for(r in seq(1, 24, by=4)){
    vioplot(Results$SSB_SSBMSY_2070[,r], Results$SSB_SSBMSY_2070[,(r+1)], Results$SSB_SSBMSY_2070[,(r+2)], Results$SSB_SSBMSY_2070[,(r+3)], 
            col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 1.5), names=c(rep("", 4)) )
    abline(h=1)
    if(r==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(r==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(r==5) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(r==9) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(r==13) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(r==17) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(r==21) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(r==21 & o==6) { legend("bottomright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                              expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                              fill=col_list2, cex=0.7, bty='n') }
  } # end r loop
  # mtext(OM_Name, outer = TRUE, cex = 1.5, line=0)
} # end o loop
mtext(expression("SSB"[2070]*"/SSB"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.8)
mtext("LoMexRec", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()



# F/FMSY 2115 #
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_LoMexRec_VIOPLOT_FMSY2115.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_Base_LoMexRec } 
  if(o==2) { Results=Results_BH_LoMexRec } 
  if(o==3) { Results=Results_Hih_LoMexRec } 
  if(o==4) { Results=Results_Loh_LoMexRec } 
  if(o==5) { Results=Results_lnR0_LoMexRec }
  if(o==6) { Results=Results_M_BH_LoMexRec } 
  for(r in seq(1, 24, by=4)){
    vioplot(Results$FM_FMMSY_2115[,r], Results$FM_FMMSY_2115[,(r+1)], Results$FM_FMMSY_2115[,(r+2)], Results$FM_FMMSY_2115[,(r+3)], 
            col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 2), names=c(rep("", 4)) )
    abline(h=1)
    if(r==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(r==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(r==5) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(r==9) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(r==13) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(r==17) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(r==21) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(r==21 & o==6) { legend("topright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                           expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                              fill=col_list2, cex=0.7, bty='n') }
  } # end r loop
  # mtext(OM_Name, outer = TRUE, cex = 1.5, line=0)
} # end o loop
mtext(expression("F"[2115]*"/F"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.8)
mtext("LoMexRec", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()




# F/FMSY 2070 #
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_LoMexRec_VIOPLOT_FMSY2070.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_Base_LoMexRec } 
  if(o==2) { Results=Results_BH_LoMexRec } 
  if(o==3) { Results=Results_Hih_LoMexRec } 
  if(o==4) { Results=Results_Loh_LoMexRec } 
  if(o==5) { Results=Results_lnR0_LoMexRec }
  if(o==6) { Results=Results_M_BH_LoMexRec } 
  for(r in seq(1, 24, by=4)){
    vioplot(Results$FM_FMMSY_2070[,r], Results$FM_FMMSY_2070[,(r+1)], Results$FM_FMMSY_2070[,(r+2)], Results$FM_FMMSY_2070[,(r+3)], 
            col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 4), names=c(rep("", 4)) )
    abline(h=1)
    if(r==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(r==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(r==5) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(r==9) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(r==13) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(r==17) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(r==21) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(r==21 & o==6) { legend("bottomright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                              expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                              fill=col_list2, cex=0.7, bty='n') }
  } # end r loop
  # mtext(OM_Name, outer = TRUE, cex = 1.5, line=0)
} # end o loop
mtext(expression("F"[2070]*"/F"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.8)
mtext("LoMexRec", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()






# POF #
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_LoMexRec_VIOPLOT_POF.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_Base_LoMexRec } 
  if(o==2) { Results=Results_BH_LoMexRec } 
  if(o==3) { Results=Results_Hih_LoMexRec } 
  if(o==4) { Results=Results_Loh_LoMexRec } 
  if(o==5) { Results=Results_lnR0_LoMexRec }
  if(o==6) { Results=Results_M_BH_LoMexRec } 
  for(r in seq(1, 24, by=4)){
    vioplot(Results$ProbOF[,r], Results$ProbOF[,(r+1)], Results$ProbOF[,(r+2)], Results$ProbOF[,(r+3)], 
            col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 1), names=c(rep("", 4)) )
    abline(h=1)
    if(r==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(r==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(r==5) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(r==9) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(r==13) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(r==17) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(r==21) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(r==21 & o==6) { legend("bottomright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                              expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                              fill=col_list2, cex=0.7, bty='n') }
  } # end r loop
  # mtext(OM_Name, outer = TRUE, cex = 1.5, line=0)
} # end o loop
mtext("POF"  , side=2, outer = TRUE, cex = 1, line=0.8)
mtext("LoMexRec", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()





# AAV #
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_HiMexRec_VIOPLOT_AAV.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_Base_LoMexRec } 
  if(o==2) { Results=Results_BH_LoMexRec } 
  if(o==3) { Results=Results_Hih_LoMexRec } 
  if(o==4) { Results=Results_Loh_LoMexRec } 
  if(o==5) { Results=Results_lnR0_LoMexRec }
  if(o==6) { Results=Results_M_BH_LoMexRec } 
  for(r in seq(1, 24, by=4)){
    vioplot(Results$AAV_all[,r], Results$AAV_all[,(r+1)], Results$AAV_all[,(r+2)], Results$AAV_all[,(r+3)], 
            col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 1), names=c(rep("", 4)) )
    abline(h=1)
    if(r==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(r==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(r==5) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(r==9) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(r==13) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(r==17) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(r==21) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(r==21 & o==6) { legend("bottomright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                              expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                              fill=col_list2, cex=0.7, bty='n') }
  } # end r loop
  # mtext(OM_Name, outer = TRUE, cex = 1.5, line=0)
} # end o loop
mtext("AAV"  , side=2, outer = TRUE, cex = 1, line=0.8)
mtext("LoMexRec", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()







######################### Conceptual ################################

####### Worm Plots -----

# SSB/SSBMSY
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_Conceptual_WORM_SSBSSBMSY.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
# OM_list = c('OM_BASE','OM_BH','OM_M_BH','OM_Hih','OM_Loh','OM_lnR0')
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { OM_Plot=OM_Base_Concept } 
  if(o==2) { OM_Plot=OM_BH_Concept } 
  if(o==3) { OM_Plot=OM_Hih_Concept } 
  if(o==4) { OM_Plot=OM_Loh_Concept } 
  if(o==5) { OM_Plot=OM_lnR0_Concept }
  if(o==6) { OM_Plot=OM_M_BH_Concept } 
  for(k in 1:(24/4)){
    kh = k*4
    hlist=c(kh, kh-1, kh-2, kh-3)
    plot(years, OM_Plot$HCR_1$SSB_SSBMSY[,1], type='l', col="white", ylim=c(0, 2.5), ylab="")
    if(k==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(o==1 & k==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(o==1 & k==2) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==3) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==4) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==5) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==6) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==6) { legend("topright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                          expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                             col=col_list2, lty=lty_list, cex=0.7, bty='n') }
    abline(h=1)
    for(h in hlist){
      for(i in 1:dim(OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY)[2]){
        lines(years, OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY[,i], col=col_lista[h])
      } # end i loop
    } # end h loop
    for(h in hlist){
      lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY, 1, median, na.rm=T), type='l', lwd=2, col=col_list2a[h], lty=lty_lista[h])
    } # end h loop
  } # end k loop
} # end o loop
mtext(expression("SSB/SSB"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.75)
mtext("Conceptual", side=3, outer = TRUE, cex = 1, line=1)

#####
dev.off()




# F/FMSY
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_Conceptual_WORM_FFMSY.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
# OM_list = c('OM_BASE','OM_BH','OM_M_BH','OM_Hih','OM_Loh','OM_lnR0')
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { OM_Plot=OM_Base_Concept } 
  if(o==2) { OM_Plot=OM_BH_Concept } 
  if(o==3) { OM_Plot=OM_Hih_Concept } 
  if(o==4) { OM_Plot=OM_Loh_Concept } 
  if(o==5) { OM_Plot=OM_lnR0_Concept }
  if(o==6) { OM_Plot=OM_M_BH_Concept } 
  for(k in 1:(24/4)){
    kh = k*4
    hlist=c(kh, kh-1, kh-2, kh-3)
    plot(years, OM_Plot$HCR_1$FM_FMMSY[,1], type='l', col="white", ylim=c(0, 4), ylab="")
    if(k==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(o==1 & k==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(o==1 & k==2) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==3) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==4) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==5) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==6) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==6) { legend("topright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                          expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                             col=col_list2, lty=lty_list, cex=0.7, bty='n') }
    abline(h=1)
    for(h in hlist){
      for(i in 1:dim(OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY)[2]){
        lines(years, OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY[,i], col=col_lista[h])
      } # end i loop
    } # end h loop
    abline(h=1)
    for(h in hlist){
      lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY, 1, median, na.rm=T), type='l', lwd=2, col=col_list2a[h], lty=lty_lista[h])
    } # end h loop
  } # end k loop
} # end o loop
mtext(expression("F/F"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.75)
mtext("Conceptual", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()




# Commercial Catch
# F/FMSY
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_Conceptual_WORM_ComCatch.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
# OM_list = c('OM_BASE','OM_BH','OM_M_BH','OM_Hih','OM_Loh','OM_lnR0')
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { OM_Plot=OM_Base_Concept } 
  if(o==2) { OM_Plot=OM_BH_Concept } 
  if(o==3) { OM_Plot=OM_Hih_Concept } 
  if(o==4) { OM_Plot=OM_Loh_Concept } 
  if(o==5) { OM_Plot=OM_lnR0_Concept }
  if(o==6) { OM_Plot=OM_M_BH_Concept } 
  for(k in 1:(24/4)){
    kh = k*4
    hlist=c(kh, kh-1, kh-2, kh-3)
    plot(years, OM_Plot$HCR_1$Com_catch[,1], type='l', col="white", ylim=c(0, 500), ylab="")
    if(k==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(o==1 & k==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(o==1 & k==2) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==3) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==4) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==5) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==6) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(o==1 & k==6) { legend("topright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                          expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                             col=col_list2, lty=lty_list, cex=0.7, bty='n') }
    abline(h=1)
    for(h in hlist){
      for(i in 1:dim(OM_Plot[[paste0("HCR_",h)]]$Com_catch)[2]){
        lines(years, OM_Plot[[paste0("HCR_",h)]]$Com_catch[,i], col=col_lista[h])
      } # end i loop
    } # end h loop
    abline(h=1)
    for(h in hlist){
      lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$Com_catch, 1, median, na.rm=T), type='l', lwd=2, col=col_list2a[h], lty=lty_lista[h])
    } # end h loop
  } # end k loop
} # end o loop
mtext("US Commercial Catch"  , side=2, outer = TRUE, cex = 1, line=0.75)
mtext("Conceptual", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()





### VIOLIN PLOTS -----
# SSB/SSBMSY #
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_Conceptual_VIOPLOT_SSBMSY2115.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_Base_Concept } 
  if(o==2) { Results=Results_BH_Concept } 
  if(o==3) { Results=Results_Hih_Concept } 
  if(o==4) { Results=Results_Loh_Concept } 
  if(o==5) { Results=Results_lnR0_Concept }
  if(o==6) { Results=Results_M_BH_Concept } 
  for(r in seq(1, 24, by=4)){
    vioplot(Results$SSB_SSBMSY_2115[,r], Results$SSB_SSBMSY_2115[,(r+1)], Results$SSB_SSBMSY_2115[,(r+2)], Results$SSB_SSBMSY_2115[,(r+3)], 
            col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 1.5), names=c(rep("", 4)) )
    abline(h=1)
    if(r==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(r==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(r==5) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(r==9) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(r==13) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(r==17) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(r==21) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(r==21 & o==6) { legend("bottomright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                              expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                              fill=col_list2, cex=0.7, bty='n') }
  } # end r loop
  # mtext(OM_Name, outer = TRUE, cex = 1.5, line=0)
} # end o loop
mtext(expression("SSB"[2115]*"/SSB"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.8)
mtext("Conceptual", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()


# SSB/SSBMSY 2070#
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_Conceptual_VIOPLOT_SSBMSY2070.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_Base_Concept } 
  if(o==2) { Results=Results_BH_Concept } 
  if(o==3) { Results=Results_Hih_Concept } 
  if(o==4) { Results=Results_Loh_Concept } 
  if(o==5) { Results=Results_lnR0_Concept }
  if(o==6) { Results=Results_M_BH_Concept } 
  for(r in seq(1, 24, by=4)){
    vioplot(Results$SSB_SSBMSY_2070[,r], Results$SSB_SSBMSY_2070[,(r+1)], Results$SSB_SSBMSY_2070[,(r+2)], Results$SSB_SSBMSY_2070[,(r+3)], 
            col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 1.5), names=c(rep("", 4)) )
    abline(h=1)
    if(r==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(r==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(r==5) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(r==9) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(r==13) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(r==17) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(r==21) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(r==21 & o==6) { legend("bottomright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                              expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                              fill=col_list2, cex=0.7, bty='n') }
  } # end r loop
  # mtext(OM_Name, outer = TRUE, cex = 1.5, line=0)
} # end o loop
mtext(expression("SSB"[2070]*"/SSB"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.8)
mtext("Conceptual", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()



# F/FMSY 2115 #
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_Conceptual_VIOPLOT_FMSY2115.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_Base_Concept } 
  if(o==2) { Results=Results_BH_Concept } 
  if(o==3) { Results=Results_Hih_Concept } 
  if(o==4) { Results=Results_Loh_Concept } 
  if(o==5) { Results=Results_lnR0_Concept }
  if(o==6) { Results=Results_M_BH_Concept } 
  for(r in seq(1, 24, by=4)){
    vioplot(Results$FM_FMMSY_2115[,r], Results$FM_FMMSY_2115[,(r+1)], Results$FM_FMMSY_2115[,(r+2)], Results$FM_FMMSY_2115[,(r+3)], 
            col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 2), names=c(rep("", 4)) )
    abline(h=1)
    if(r==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(r==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(r==5) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(r==9) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(r==13) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(r==17) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(r==21) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(r==21 & o==6) { legend("topright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                           expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                              fill=col_list2, cex=0.7, bty='n') }
  } # end r loop
  # mtext(OM_Name, outer = TRUE, cex = 1.5, line=0)
} # end o loop
mtext(expression("F"[2115]*"/F"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.8)
mtext("Conceptual", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()




# F/FMSY 2070 #
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_Conceptual_VIOPLOT_FMSY2070.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_Base_Concept } 
  if(o==2) { Results=Results_BH_Concept } 
  if(o==3) { Results=Results_Hih_Concept } 
  if(o==4) { Results=Results_Loh_Concept } 
  if(o==5) { Results=Results_lnR0_Concept }
  if(o==6) { Results=Results_M_BH_Concept } 
  for(r in seq(1, 24, by=4)){
    vioplot(Results$FM_FMMSY_2070[,r], Results$FM_FMMSY_2070[,(r+1)], Results$FM_FMMSY_2070[,(r+2)], Results$FM_FMMSY_2070[,(r+3)], 
            col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 4), names=c(rep("", 4)) )
    abline(h=1)
    if(r==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(r==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(r==5) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(r==9) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(r==13) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(r==17) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(r==21) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(r==21 & o==6) { legend("bottomright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                              expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                              fill=col_list2, cex=0.7, bty='n') }
  } # end r loop
  # mtext(OM_Name, outer = TRUE, cex = 1.5, line=0)
} # end o loop
mtext(expression("F"[2070]*"/F"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.8)
mtext("Conceptual", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()






# POF #
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_Concept_VIOPLOT_POF.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_Base_Concept } 
  if(o==2) { Results=Results_BH_Concept } 
  if(o==3) { Results=Results_Hih_Concept } 
  if(o==4) { Results=Results_Loh_Concept } 
  if(o==5) { Results=Results_lnR0_Concept }
  if(o==6) { Results=Results_M_BH_Concept } 
  for(r in seq(1, 24, by=4)){
    vioplot(Results$ProbOF[,r], Results$ProbOF[,(r+1)], Results$ProbOF[,(r+2)], Results$ProbOF[,(r+3)], 
            col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 1), names=c(rep("", 4)) )
    abline(h=1)
    if(r==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(r==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(r==5) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(r==9) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(r==13) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(r==17) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(r==21) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(r==21 & o==6) { legend("bottomright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                              expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                              fill=col_list2, cex=0.7, bty='n') }
  } # end r loop
  # mtext(OM_Name, outer = TRUE, cex = 1.5, line=0)
} # end o loop
mtext("POF"  , side=2, outer = TRUE, cex = 1, line=0.8)
mtext("Concept", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()





# AAV #
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_Concept_VIOPLOT_AAV.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*R0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_Base_Concept } 
  if(o==2) { Results=Results_BH_Concept } 
  if(o==3) { Results=Results_Hih_Concept } 
  if(o==4) { Results=Results_Loh_Concept } 
  if(o==5) { Results=Results_lnR0_Concept }
  if(o==6) { Results=Results_M_BH_Concept } 
  for(r in seq(1, 24, by=4)){
    vioplot(Results$AAV_all[,r], Results$AAV_all[,(r+1)], Results$AAV_all[,(r+2)], Results$AAV_all[,(r+3)], 
            col=c("dimgrey","deepskyblue3","forestgreen","darkorchid"), ylim=c(0, 1), names=c(rep("", 4)) )
    abline(h=1)
    if(r==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(r==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0, cex=0.8) }
    if(r==5) { mtext(expression("F"["lim"]*"= M"), side=3, line=0, cex=0.8) }
    if(r==9) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0, cex=0.8) }
    if(r==13) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0, cex=0.8) }
    if(r==17) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0, cex=0.8) }
    if(r==21) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0, cex=0.8) }
    if(r==21 & o==6) { legend("bottomright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                              expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                              fill=col_list2, cex=0.7, bty='n') }
  } # end r loop
  # mtext(OM_Name, outer = TRUE, cex = 1.5, line=0)
} # end o loop
mtext("AAV"  , side=2, outer = TRUE, cex = 1, line=0.8)
mtext("Concept", side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()







############# BASE ACROSS IMPLEMENTATION MODELS --------

# SSB/SSBMSY
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\BASE_AllIs_WORM_SSBSSBMSY.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
#####
par(mfrow=c(3,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
# OM_list = c('OM_BASE','OM_BH','OM_M_BH','OM_Hih','OM_Loh','OM_lnR0')
OM_labs = c("Concept","LoMexRec","HiMexRec")
for(o in 1:3){
  if(o==1) { OM_Plot=OM_Base_Concept } 
  if(o==2) { OM_Plot=OM_Base_LoMexRec } 
  if(o==3) { OM_Plot=OM_Base_HiMexRec } 
  for(k in 1:(24/4)){
    kh = k*4
    hlist=c(kh, kh-1, kh-2, kh-3)
    plot(years, OM_Plot$HCR_1$SSB_SSBMSY[,1], type='l', col="white", ylim=c(0, 2.5), ylab="")
    if(k==1) { mtext(OM_labs[o], side=2, line=1, cex=0.8) }
    if(o==1 & k==1) { mtext(expression("F"["lim"]*"= F"["MSY"]), side=3, line=0.25, cex=0.8) }
    if(o==1 & k==2) { mtext(expression("F"["lim"]*"= M"), side=3, line=0.25, cex=0.8) }
    if(o==1 & k==3) { mtext(expression("F"["lim"]*"= 0.8M"), side=3, line=0.25, cex=0.8) }
    if(o==1 & k==4) { mtext(expression("F"["lim"]*"= 0.6M"), side=3, line=0.25, cex=0.8) }
    if(o==1 & k==5) { mtext(expression("F"["lim"]*"= 0.4M"), side=3, line=0.25, cex=0.8) }
    if(o==1 & k==6) { mtext(expression("F"["lim"]*"= 0.2M"), side=3, line=0.25, cex=0.8) }
    if(o==1 & k==6) { legend("topright",c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]),
                                          expression("a=0.3B"[0]*", b=B"["MSY"]), expression("a=0.3B"[0]*", b=0.8B"["MSY"])),
                             col=col_list2, lty=lty_list, cex=0.7, bty='n') }
    abline(h=1)
    for(h in hlist){
      for(i in 1:dim(OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY)[2]){
        lines(years, OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY[,i], col=col_lista[h])
      } # end i loop
    } # end h loop
    for(h in hlist){
      lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY, 1, median, na.rm=T), type='l', lwd=2, col=col_list2a[h], lty=lty_lista[h])
    } # end h loop
  } # end k loop
} # end o loop
mtext(expression("SSB/SSB"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.75)
mtext("Base OM"  , side=3, outer = TRUE, cex = 1, line=1)
#####
dev.off()




### Tradeoff plot ###

png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_tradeoff_Base_ComCatch.png",
    type="cairo",
    units="mm",
    width=200,
    height=250,
    pointsize=18,
    res=300)
#####


par(mfrow=c(3,1), mar=c(0.3, 2.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), oma = c(2.1, 0.3, 0.3, 0.3), cex=1)

cols=c("black",'deepskyblue','purple','darkolivegreen')
pchs=c(15, 15, 15, 15, 16, 16, 16, 16, 17, 17, 17, 17, 18, 18, 18, 25, 25, 25, 25, 8, 8, 8, 8)
cols2=rep(cols, 6)

Results = Results_Base_Concept

plot(Results$SSB_SSBMSY_2115[,1]~Results$Com_Catch_cumulative[,1], col='white', ylim=c(0, 2), xlim=c(0,15500), ylab="", xlab="", axes=F)
axis(1, labels=F)
axis(2)
box()
# for(i in 1:4){
#   points(Results$SSB_SSBMSY_2115[,i]~Results$Com_Catch_cumulative[,i], col=cols[i])
# }
Results = Results_Base_Concept
for(i in 1:24){
  points(median(Results$SSB_SSBMSY_2115[,i])~median(Results$Com_Catch_cumulative[,i]), col=cols2[i],pch=pchs[i])
  arrows(x0=median(Results$Com_Catch_cumulative[,i]), 
         y0=median(Results$SSB_SSBMSY_2115[,i]) - sd(Results$SSB_SSBMSY_2115[,i]),
         y1=median(Results$SSB_SSBMSY_2115[,i]) + sd(Results$SSB_SSBMSY_2115[,i]),
         code=3, angle=90, length=0, col=cols2[i])
  arrows(y0=median(Results$SSB_SSBMSY_2115[,i]), 
         x0=median(Results$Com_Catch_cumulative[,i]) - sd(Results$Com_Catch_cumulative[,i]),
         x1=median(Results$Com_Catch_cumulative[,i]) + sd(Results$Com_Catch_cumulative[,i]),
         code=3, angle=90, length=0, col=cols2[i])
}
mtext("Conceptual", side=3, line=-1, cex=1)
mtext(expression("SSB"["2115"]*" / SSB"["MSY"]), side=2, line=1, cex=1)

legend("right", c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]), 
                  expression("a=0.3B"["0"]*"b=B"["MSY"]),expression("a=0.3B"["0"]*"b=0.8B"["MSY"]),
                  expression("F"["lim"]*"=F"["MSY"]), expression("F"["lim"]*"=M"), 
                  expression("F"["lim"]*"=0.8M"), expression("F"["lim"]*"=0.6M"), 
                  expression("F"["lim"]*"=0.4M"), expression("F"["lim"]*"=0.2M") ),
       col=c("black",'deepskyblue','purple','darkolivegreen','grey','grey','grey','grey','grey','grey'),
       pch=c(3,3,3,3,15, 16, 17, 18, 25, 8),
       bty='n',cex=0.75, pt.cex=1, ncol=1)

Results = Results_Base_LoMexRec
plot(Results$SSB_SSBMSY_2115[,1]~Results$Com_Catch_cumulative[,1], col='white', ylim=c(0, 2), xlim=c(0,15500), ylab="", xlab="", axes=F)
axis(1, labels=F)
axis(2)
box()
# for(i in 1:4){
#   points(Results$SSB_SSBMSY_2115[,i]~Results$Com_Catch_cumulative[,i], col=cols[i])
# }
for(i in 1:24){
  points(median(Results$SSB_SSBMSY_2115[,i])~median(Results$Com_Catch_cumulative[,i]), col=cols2[i],pch=pchs[i])
  arrows(x0=median(Results$Com_Catch_cumulative[,i]), 
         y0=median(Results$SSB_SSBMSY_2115[,i]) - sd(Results$SSB_SSBMSY_2115[,i]),
         y1=median(Results$SSB_SSBMSY_2115[,i]) + sd(Results$SSB_SSBMSY_2115[,i]),
         code=3, angle=90, length=0, col=cols2[i])
  arrows(y0=median(Results$SSB_SSBMSY_2115[,i]), 
         x0=median(Results$Com_Catch_cumulative[,i]) - sd(Results$Com_Catch_cumulative[,i]),
         x1=median(Results$Com_Catch_cumulative[,i]) + sd(Results$Com_Catch_cumulative[,i]),
         code=3, angle=90, length=0, col=cols2[i])
}
mtext("LoMexRec", side=3, line=-1, cex=1)
mtext(expression("SSB"["2115"]*" / SSB"["MSY"]), side=2, line=1, cex=1)
# mtext("Cumulative US Commercial Catch", side=1, line=1, cex=1)

Results = Results_Base_HiMexRec
plot(Results$SSB_SSBMSY_2115[,1]~Results$Com_Catch_cumulative[,1], col='white', ylim=c(0, 2), xlim=c(0,15500), ylab="", xlab="", axes=F)
axis(1)
axis(2)
box()
# for(i in 1:4){
#   points(Results$SSB_SSBMSY_2115[,i]~Results$Com_Catch_cumulative[,i], col=cols[i])
# }
for(i in 1:24){
  points(median(Results$SSB_SSBMSY_2115[,i])~median(Results$Com_Catch_cumulative[,i]), col=cols2[i],pch=pchs[i])
  arrows(x0=median(Results$Com_Catch_cumulative[,i]), 
         y0=median(Results$SSB_SSBMSY_2115[,i]) - sd(Results$SSB_SSBMSY_2115[,i]),
         y1=median(Results$SSB_SSBMSY_2115[,i]) + sd(Results$SSB_SSBMSY_2115[,i]),
         code=3, angle=90, length=0, col=cols2[i])
  arrows(y0=median(Results$SSB_SSBMSY_2115[,i]), 
         x0=median(Results$Com_Catch_cumulative[,i]) - sd(Results$Com_Catch_cumulative[,i]),
         x1=median(Results$Com_Catch_cumulative[,i]) + sd(Results$Com_Catch_cumulative[,i]),
         code=3, angle=90, length=0, col=cols2[i])
}
mtext("HiMexRec", side=3, line=-1, cex=1)
mtext(expression("SSB"["2115"]*" / SSB"["MSY"]), side=2, line=1, cex=1)
mtext("Cumulative US Commercial Catch (mt)", side=1, line=1, cex=1)




#####
dev.off()



tiff(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_tradeoff_Base_ComCatch.tiff",
    type="cairo",
    units="mm",
    width=200,
    height=250,
    pointsize=18,
    res=300)
#####


par(mfrow=c(3,1), mar=c(0.3, 2.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), oma = c(2.1, 0.3, 0.3, 0.3), cex=1)

cols=c("black",'deepskyblue','purple','darkolivegreen')
pchs=c(15, 15, 15, 15, 16, 16, 16, 16, 17, 17, 17, 17, 18, 18, 18, 25, 25, 25, 25, 8, 8, 8, 8)
cols2=rep(cols, 6)

Results = Results_Base_Concept

plot(Results$SSB_SSBMSY_2115[,1]~Results$Com_Catch_cumulative[,1], col='white', ylim=c(0, 2), xlim=c(0,15500), ylab="", xlab="", axes=F)
axis(1, labels=F)
axis(2)
box()
# for(i in 1:4){
#   points(Results$SSB_SSBMSY_2115[,i]~Results$Com_Catch_cumulative[,i], col=cols[i])
# }
Results = Results_Base_Concept
for(i in 1:24){
  points(median(Results$SSB_SSBMSY_2115[,i])~median(Results$Com_Catch_cumulative[,i]), col=cols2[i],pch=pchs[i])
  arrows(x0=median(Results$Com_Catch_cumulative[,i]), 
         y0=median(Results$SSB_SSBMSY_2115[,i]) - sd(Results$SSB_SSBMSY_2115[,i]),
         y1=median(Results$SSB_SSBMSY_2115[,i]) + sd(Results$SSB_SSBMSY_2115[,i]),
         code=3, angle=90, length=0, col=cols2[i])
  arrows(y0=median(Results$SSB_SSBMSY_2115[,i]), 
         x0=median(Results$Com_Catch_cumulative[,i]) - sd(Results$Com_Catch_cumulative[,i]),
         x1=median(Results$Com_Catch_cumulative[,i]) + sd(Results$Com_Catch_cumulative[,i]),
         code=3, angle=90, length=0, col=cols2[i])
}
mtext("Conceptual", side=3, line=-1, cex=1)
mtext(expression("SSB"["2115"]*" / SSB"["MSY"]), side=2, line=1, cex=1)

legend("right", c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]), 
                  expression("a=0.3B"["0"]*"b=B"["MSY"]),expression("a=0.3B"["0"]*"b=0.8B"["MSY"]),
                  expression("F"["lim"]*"=F"["MSY"]), expression("F"["lim"]*"=M"), 
                  expression("F"["lim"]*"=0.8M"), expression("F"["lim"]*"=0.6M"), 
                  expression("F"["lim"]*"=0.4M"), expression("F"["lim"]*"=0.2M") ),
       col=c("black",'deepskyblue','purple','darkolivegreen','grey','grey','grey','grey','grey','grey'),
       pch=c(3,3,3,3,15, 16, 17, 18, 25, 8),
       bty='n',cex=0.75, pt.cex=1, ncol=1)

Results = Results_Base_LoMexRec
plot(Results$SSB_SSBMSY_2115[,1]~Results$Com_Catch_cumulative[,1], col='white', ylim=c(0, 2), xlim=c(0,15500), ylab="", xlab="", axes=F)
axis(1, labels=F)
axis(2)
box()
# for(i in 1:4){
#   points(Results$SSB_SSBMSY_2115[,i]~Results$Com_Catch_cumulative[,i], col=cols[i])
# }
for(i in 1:24){
  points(median(Results$SSB_SSBMSY_2115[,i])~median(Results$Com_Catch_cumulative[,i]), col=cols2[i],pch=pchs[i])
  arrows(x0=median(Results$Com_Catch_cumulative[,i]), 
         y0=median(Results$SSB_SSBMSY_2115[,i]) - sd(Results$SSB_SSBMSY_2115[,i]),
         y1=median(Results$SSB_SSBMSY_2115[,i]) + sd(Results$SSB_SSBMSY_2115[,i]),
         code=3, angle=90, length=0, col=cols2[i])
  arrows(y0=median(Results$SSB_SSBMSY_2115[,i]), 
         x0=median(Results$Com_Catch_cumulative[,i]) - sd(Results$Com_Catch_cumulative[,i]),
         x1=median(Results$Com_Catch_cumulative[,i]) + sd(Results$Com_Catch_cumulative[,i]),
         code=3, angle=90, length=0, col=cols2[i])
}
mtext("LoMexRec", side=3, line=-1, cex=1)
mtext(expression("SSB"["2115"]*" / SSB"["MSY"]), side=2, line=1, cex=1)
# mtext("Cumulative US Commercial Catch", side=1, line=1, cex=1)

Results = Results_Base_HiMexRec
plot(Results$SSB_SSBMSY_2115[,1]~Results$Com_Catch_cumulative[,1], col='white', ylim=c(0, 2), xlim=c(0,15500), ylab="", xlab="", axes=F)
axis(1)
axis(2)
box()
# for(i in 1:4){
#   points(Results$SSB_SSBMSY_2115[,i]~Results$Com_Catch_cumulative[,i], col=cols[i])
# }
for(i in 1:24){
  points(median(Results$SSB_SSBMSY_2115[,i])~median(Results$Com_Catch_cumulative[,i]), col=cols2[i],pch=pchs[i])
  arrows(x0=median(Results$Com_Catch_cumulative[,i]), 
         y0=median(Results$SSB_SSBMSY_2115[,i]) - sd(Results$SSB_SSBMSY_2115[,i]),
         y1=median(Results$SSB_SSBMSY_2115[,i]) + sd(Results$SSB_SSBMSY_2115[,i]),
         code=3, angle=90, length=0, col=cols2[i])
  arrows(y0=median(Results$SSB_SSBMSY_2115[,i]), 
         x0=median(Results$Com_Catch_cumulative[,i]) - sd(Results$Com_Catch_cumulative[,i]),
         x1=median(Results$Com_Catch_cumulative[,i]) + sd(Results$Com_Catch_cumulative[,i]),
         code=3, angle=90, length=0, col=cols2[i])
}
mtext("HiMexRec", side=3, line=-1, cex=1)
mtext(expression("SSB"["2115"]*" / SSB"["MSY"]), side=2, line=1, cex=1)
mtext("Cumulative US Commercial Catch (mt)", side=1, line=1, cex=1)




#####
dev.off()



# TRADEOFF TOT CATCH #
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_tradeoff_Base_TotCatch.png",
    type="cairo",
    units="mm",
    width=200,
    height=250,
    pointsize=18,
    res=600)
#####
# l1 = lm(apply(Results_Base_Concept$SSB_SSBMSY_2115,2,median)~apply(Results_Base_Concept$Tot_Catch_cumulative,2,median) )
# l2 = lm(apply(Results_Base_LoMexRec$SSB_SSBMSY_2115,2,median)~apply(Results_Base_LoMexRec$Tot_Catch_cumulative,2,median) )
# l3 = lm(apply(Results_Base_HiMexRec$SSB_SSBMSY_2115,2,median)~apply(Results_Base_HiMexRec$Tot_Catch_cumulative,2,median) )

par(mfrow=c(3,1), mar=c(0.3, 2.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), oma = c(2.1, 0.3, 0.3, 0.3), cex=1)

cols=c("black",'deepskyblue','purple','darkolivegreen')
pchs=c(15, 15, 15, 15, 16, 16, 16, 16, 17, 17, 17, 17, 18, 18, 18, 25, 25, 25, 25, 8, 8, 8, 8)
cols2=rep(cols, 6)

Results = Results_Base_Concept

plot(Results$SSB_SSBMSY_2115[,1]~Results$Tot_Catch_cumulative[,1], col='white', ylim=c(0, 2), xlim=c(0,35000), ylab="", xlab="", axes=F)
axis(1, labels=F)
axis(2)
box()
# for(i in 1:4){
#   points(Results$SSB_SSBMSY_2115[,i]~Results$Tot_Catch_cumulative[,i], col=cols[i])
# }
Results = Results_Base_Concept
for(i in 1:24){
  points(median(Results$SSB_SSBMSY_2115[,i])~median(Results$Tot_Catch_cumulative[,i]), col=cols2[i],pch=pchs[i])
  arrows(x0=median(Results$Tot_Catch_cumulative[,i]), 
         y0=median(Results$SSB_SSBMSY_2115[,i]) - sd(Results$SSB_SSBMSY_2115[,i]),
         y1=median(Results$SSB_SSBMSY_2115[,i]) + sd(Results$SSB_SSBMSY_2115[,i]),
         code=3, angle=90, length=0, col=cols2[i])
  arrows(y0=median(Results$SSB_SSBMSY_2115[,i]), 
         x0=median(Results$Tot_Catch_cumulative[,i]) - sd(Results$Tot_Catch_cumulative[,i]),
         x1=median(Results$Tot_Catch_cumulative[,i]) + sd(Results$Tot_Catch_cumulative[,i]),
         code=3, angle=90, length=0, col=cols2[i])
}
# abline(l1)
# abline(l2)
# abline(l3)
mtext("Conceptual", side=3, line=-1, cex=1)
mtext(expression("SSB"["2115"]*" / SSB"["MSY"]), side=2, line=1, cex=1)
legend("right", c(expression("a=0, b=B"["MSY"]), expression("a=0, b=0.8B"["MSY"]), 
                  expression("a=0.3B"["0"]*"b=B"["MSY"]),expression("a=0.3B"["0"]*"b=0.8B"["MSY"]),
                  expression("F"["lim"]*"=F"["MSY"]), expression("F"["lim"]*"=M"), 
                  expression("F"["lim"]*"=0.8M"), expression("F"["lim"]*"=0.6M"), 
                  expression("F"["lim"]*"=0.4M"), expression("F"["lim"]*"=0.2M") ),
       col=c("black",'deepskyblue','purple','darkolivegreen','grey','grey','grey','grey','grey','grey'),
       pch=c(3,3,3,3,15, 16, 17, 18, 25, 8),
       bty='n',cex=0.75, pt.cex=1, ncol=1)

Results = Results_Base_LoMexRec
plot(Results$SSB_SSBMSY_2115[,1]~Results$Tot_Catch_cumulative[,1], col='white', ylim=c(0, 2), xlim=c(0,35000), ylab="", xlab="", axes=F)
axis(1, labels=F)
axis(2)
box()
# for(i in 1:4){
#   points(Results$SSB_SSBMSY_2115[,i]~Results$Tot_Catch_cumulative[,i], col=cols[i])
# }
for(i in 1:24){
  points(median(Results$SSB_SSBMSY_2115[,i])~median(Results$Tot_Catch_cumulative[,i]), col=cols2[i],pch=pchs[i])
  arrows(x0=median(Results$Tot_Catch_cumulative[,i]), 
         y0=median(Results$SSB_SSBMSY_2115[,i]) - sd(Results$SSB_SSBMSY_2115[,i]),
         y1=median(Results$SSB_SSBMSY_2115[,i]) + sd(Results$SSB_SSBMSY_2115[,i]),
         code=3, angle=90, length=0, col=cols2[i])
  arrows(y0=median(Results$SSB_SSBMSY_2115[,i]), 
         x0=median(Results$Tot_Catch_cumulative[,i]) - sd(Results$Tot_Catch_cumulative[,i]),
         x1=median(Results$Tot_Catch_cumulative[,i]) + sd(Results$Tot_Catch_cumulative[,i]),
         code=3, angle=90, length=0, col=cols2[i])
}
mtext("LoMexRec", side=3, line=-1, cex=1)
mtext(expression("SSB"["2115"]*" / SSB"["MSY"]), side=2, line=1, cex=1)
# mtext("Cumulative US Commercial Catch", side=1, line=1, cex=1)

Results = Results_Base_HiMexRec
plot(Results$SSB_SSBMSY_2115[,1]~Results$Tot_Catch_cumulative[,1], col='white', ylim=c(0, 2), xlim=c(0,35000), ylab="", xlab="", axes=F)
axis(1)
axis(2)
box()
# for(i in 1:4){
#   points(Results$SSB_SSBMSY_2115[,i]~Results$Tot_Catch_cumulative[,i], col=cols[i])
# }
for(i in 1:24){
  points(median(Results$SSB_SSBMSY_2115[,i])~median(Results$Tot_Catch_cumulative[,i]), col=cols2[i],pch=pchs[i])
  arrows(x0=median(Results$Tot_Catch_cumulative[,i]), 
         y0=median(Results$SSB_SSBMSY_2115[,i]) - sd(Results$SSB_SSBMSY_2115[,i]),
         y1=median(Results$SSB_SSBMSY_2115[,i]) + sd(Results$SSB_SSBMSY_2115[,i]),
         code=3, angle=90, length=0, col=cols2[i])
  arrows(y0=median(Results$SSB_SSBMSY_2115[,i]), 
         x0=median(Results$Tot_Catch_cumulative[,i]) - sd(Results$Tot_Catch_cumulative[,i]),
         x1=median(Results$Tot_Catch_cumulative[,i]) + sd(Results$Tot_Catch_cumulative[,i]),
         code=3, angle=90, length=0, col=cols2[i])
}
mtext("HiMexRec", side=3, line=-1, cex=1)
mtext(expression("SSB"["2115"]*" / SSB"["MSY"]), side=2, line=1, cex=1)
mtext("Cumulative Total Catch (mt)", side=1, line=1, cex=1)




#####
dev.off()



