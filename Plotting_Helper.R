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



####### Worm Plots ----------------------------------------------------------------------------------------------------------

# SSB/SSBMSY
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_WORM_SSBSSBMSY.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 1.5, 0))
# OM_list = c('OM_BASE','OM_BH','OM_M_BH','OM_Hih','OM_Loh','OM_lnR0')
OM_labs = c("Base","BH","Hi h","Lo h", "2*lnR0","M/2 - BH")
for(o in 1:6){
  if(o==1) { OM_Plot=OM_BASE } 
  if(o==2) { OM_Plot=OM_BH } 
  if(o==3) { OM_Plot=OM_Hih } 
  if(o==4) { OM_Plot=OM_Loh } 
  if(o==5) { OM_Plot=OM_lnR0 }
  if(o==6) { OM_Plot=OM_M_BH } 
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
      for(i in 1:length(iters)){
        lines(years, OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY[,i], col=col_lista[h])
      } # end i loop
    } # end h loop
    for(h in hlist){
      lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY, 1, median), type='l', lwd=2, col=col_list2a[h], lty=lty_lista[h])
    } # end h loop
  } # end k loop
} # end o loop
mtext(expression("SSB/SSB"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.75)

dev.off()




# F/FMSY
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_WORM_FFMSY.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 1.5, 0))
# OM_list = c('OM_BASE','OM_BH','OM_M_BH','OM_Hih','OM_Loh','OM_lnR0')
OM_labs = c("Base","BH","Hi h","Lo h", "2*lnR0","M/2 - BH")
for(o in 1:6){
  if(o==1) { OM_Plot=OM_BASE } 
  if(o==2) { OM_Plot=OM_BH } 
  if(o==3) { OM_Plot=OM_Hih } 
  if(o==4) { OM_Plot=OM_Loh } 
  if(o==5) { OM_Plot=OM_lnR0 }
  if(o==6) { OM_Plot=OM_M_BH } 
  for(k in 1:(24/4)){
    kh = k*4
    hlist=c(kh, kh-1, kh-2, kh-3)
    plot(years, OM_Plot$HCR_1$FM_FMMSY[,1], type='l', col="white", ylim=c(0, 4), ylab="")
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
      for(i in 1:length(iters)){
        lines(years, OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY[,i], col=col_lista[h])
      } # end i loop
    } # end h loop
    abline(h=1)
    for(h in hlist){
      lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$FM_FMMSY, 1, median), type='l', lwd=2, col=col_list2a[h], lty=lty_lista[h])
    } # end h loop
  } # end k loop
} # end o loop
mtext(expression("F/F"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.75)

dev.off()




# Commercial Catch
# F/FMSY
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_WORM_ComCatch.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 1.5, 0))
# OM_list = c('OM_BASE','OM_BH','OM_M_BH','OM_Hih','OM_Loh','OM_lnR0')
OM_labs = c("Base","BH","Hi h","Lo h", "2*lnR0","M/2 - BH")
for(o in 1:6){
  if(o==1) { OM_Plot=OM_BASE } 
  if(o==2) { OM_Plot=OM_BH } 
  if(o==3) { OM_Plot=OM_Hih } 
  if(o==4) { OM_Plot=OM_Loh } 
  if(o==5) { OM_Plot=OM_lnR0 }
  if(o==6) { OM_Plot=OM_M_BH } 
  for(k in 1:(24/4)){
    kh = k*4
    hlist=c(kh, kh-1, kh-2, kh-3)
    plot(years, OM_Plot$HCR_1$Com_catch[,1], type='l', col="white", ylim=c(0, 500), ylab="")
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
      for(i in 1:length(iters)){
        lines(years, OM_Plot[[paste0("HCR_",h)]]$Com_catch[,i], col=col_lista[h])
      } # end i loop
    } # end h loop
    abline(h=1)
    for(h in hlist){
      lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$Com_catch, 1, median), type='l', lwd=2, col=col_list2a[h], lty=lty_lista[h])
    } # end h loop
  } # end k loop
} # end o loop
mtext("US Commercial Catch"  , side=2, outer = TRUE, cex = 1, line=0.75)

dev.off()





### VIOLIN PLOTS #####----------------------------------------------------------------------
# SSB/SSBMSY #
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_VIOPLOT_SSBMSY2115.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 1.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*lnR0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_BASE } 
  if(o==2) { Results=Results_BH } 
  if(o==3) { Results=Results_Hih } 
  if(o==4) { Results=Results_Loh } 
  if(o==5) { Results=Results_lnR0 }
  if(o==6) { Results=Results_M_BH } 
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

dev.off()


# SSB/SSBMSY 2065#
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_VIOPLOT_SSBMSY2065.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 1.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*lnR0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_BASE } 
  if(o==2) { Results=Results_BH } 
  if(o==3) { Results=Results_Hih } 
  if(o==4) { Results=Results_Loh } 
  if(o==5) { Results=Results_lnR0 }
  if(o==6) { Results=Results_M_BH } 
  for(r in seq(1, 24, by=4)){
    vioplot(Results$SSB_SSBMSY_2065[,r], Results$SSB_SSBMSY_2065[,(r+1)], Results$SSB_SSBMSY_2065[,(r+2)], Results$SSB_SSBMSY_2065[,(r+3)], 
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
mtext(expression("SSB"[2065]*"/SSB"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.8)

dev.off()



# F/FMSY 2115 #
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_VIOPLOT_FMSY2115.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 1.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*lnR0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_BASE } 
  if(o==2) { Results=Results_BH } 
  if(o==3) { Results=Results_Hih } 
  if(o==4) { Results=Results_Loh } 
  if(o==5) { Results=Results_lnR0 }
  if(o==6) { Results=Results_M_BH } 
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

dev.off()




# F/FMSY 2065 #
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\ALL_VIOPLOT_FMSY2065.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
par(mfrow=c(6,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 1.5, 0))
OM_labs = c("Base","BH","Hi h","Lo h", "2*lnR0","M/2 - BH")
for(o in 1:6){
  if(o==1) { Results=Results_BASE } 
  if(o==2) { Results=Results_BH } 
  if(o==3) { Results=Results_Hih } 
  if(o==4) { Results=Results_Loh } 
  if(o==5) { Results=Results_lnR0 }
  if(o==6) { Results=Results_M_BH } 
  for(r in seq(1, 24, by=4)){
    vioplot(Results$FM_FMMSY_2065[,r], Results$FM_FMMSY_2065[,(r+1)], Results$FM_FMMSY_2065[,(r+2)], Results$FM_FMMSY_2065[,(r+3)], 
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
mtext(expression("F"[2065]*"/F"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.8)

dev.off()




############# WORM BASE ## -----------------------------------------------------------------------------------

# SSB/SSBMSY
png(filename="D:\\MSE_Run\\MSE_Results\\Plots\\BASE_AllIs_WORM_SSBSSBMSY.png",
    type="cairo",
    units="mm",
    width=300,
    height=250,
    pointsize=18,
    res=300)
par(mfrow=c(3,6),  mar=c(1.1, 1.1, 0.3, 0.3),tcl = -0.1, mgp = c(0.8, 0.1, 0), cex=0.7, oma = c(0, 2.5, 2.5, 0))
# OM_list = c('OM_BASE','OM_BH','OM_M_BH','OM_Hih','OM_Loh','OM_lnR0')
OM_labs = c("Hi_MexRec","Conceptual","Lo_MexRec")
for(o in 1:3){
  if(o==1) { OM_Plot=OM_BASE } 
  if(o==2) { OM_Plot=OM_BASE_MexRec } 
  if(o==3) { OM_Plot=OM_BASE_LoMexRec } 
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
      for(i in 1:length(iters)){
        lines(years, OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY[,i], col=col_lista[h])
      } # end i loop
    } # end h loop
    for(h in hlist){
      lines(years, apply(OM_Plot[[paste0("HCR_",h)]]$SSB_SSBMSY, 1, median), type='l', lwd=2, col=col_list2a[h], lty=lty_lista[h])
    } # end h loop
  } # end k loop
} # end o loop
mtext(expression("SSB/SSB"["MSY"])  , side=2, outer = TRUE, cex = 1, line=0.75)
mtext("Base OM"  , side=3, outer = TRUE, cex = 1, line=1)

dev.off()


