# plots
# source("master.R")

# UMB color palette
UMBred <- rgb(200, 16, 46, maxColorValue = 255)
UMByellow <- rgb(255, 205, 0, maxColorValue = 255)
UMBblue <- rgb(0, 118, 152, maxColorValue = 255)
UMBslate <- rgb(93, 135, 161, maxColorValue = 255)
UMBgray <- rgb(149, 160, 169, maxColorValue = 255)
UMBcharcoal <- rgb(105, 106, 109, maxColorValue = 255)
UMBplum <- rgb(73, 24, 45, maxColorValue = 255)
UMBforest <- rgb(51, 70, 13, maxColorValue = 255)
UMBsea <- rgb(180, 204, 149, maxColorValue = 255)
UMBtan <- rgb(200, 177, 139, maxColorValue = 255)

# For figures on slides, make font larger
# cex.lab = 1.5, cex.axis = 1.5

# Figure 1
# Plot cost per DALYs averted 
quartz("CE plane", 8, 8)
par(xaxs="i", yaxs="i")
plot(DALYS_lost_no - DALYS_lost_llAb, totalcost_llAb- totalcost_no, col = UMBblue, bty = "n", pch = 19, cex = 1.5, xlim = c(0,2500), ylim = c(0,7000000), xlab = "DALYs averted", ylab = 
       "Incremental cost compared to status quo (USD)")
points(DALYS_lost_no - DALYS_lost_mVax, totalcost_mVax- totalcost_no, col = UMBforest, pch = 19, cex =1.5)
points(DALYS_lost_no - DALYS_lost_pVax, totalcost_pVax- totalcost_no, col = UMBplum, pch = 19, cex = 1.5)
points(DALYS_lost_no - DALYS_lost_pVax_intflo, totalcost_pVax_intflo - totalcost_no, col = UMBplum, pch = 17, cex = 1.5)
points(DALYS_lost_no - DALYS_lost_pVax_intfhi, totalcost_pVax_intfhi - totalcost_no, col = UMBplum, pch = 15, cex = 1.5)
points(DALYS_lost_no - DALYS_lost_joint_llAb_pVax, totalcost_joint_llAb_pVax - totalcost_no, col = UMByellow, pch = 19, cex = 1.5)
points(DALYS_lost_no - DALYS_lost_joint_mVax_pVax, totalcost_joint_mVax_pVax - totalcost_no, col = UMBcharcoal, pch = 19, cex = 1.5)
points(DALYS_lost_no - DALYS_lost_intfhi_llAb_pVax, totalcost_intfhi_llAb_pVax - totalcost_no, col = UMByellow, pch =15, cex = 1.5)
points(DALYS_lost_no - DALYS_lost_intfhi_mVax_pVax, totalcost_intfhi_mVax_pVax - totalcost_no, col = UMBcharcoal, pch =15, cex = 1.5)
points(DALYS_lost_no - DALYS_lost_intflo_llAb_pVax, totalcost_intflo_llAb_pVax - totalcost_no, col = UMByellow, pch =17, cex = 1.5)
points(DALYS_lost_no - DALYS_lost_intflo_mVax_pVax, totalcost_intflo_mVax_pVax - totalcost_no, col = UMBcharcoal, pch =17, cex = 1.5)
segments(0, 0, x1 = DALYS_lost_no - DALYS_lost_pVax, y1 = totalcost_pVax- totalcost_no,
         col = par("fg"), lty = par("lty"), lwd = 2)
segments(DALYS_lost_no - DALYS_lost_pVax, totalcost_pVax- totalcost_no, x1 = DALYS_lost_no -DALYS_lost_joint_llAb_pVax, y1 = totalcost_joint_llAb_pVax - totalcost_no,
         col = par("fg"), lty = par("lty"), lwd = 2)
segments(0, 0, x1 = DALYS_lost_no - DALYS_lost_pVax_intflo, y1 = totalcost_pVax_intflo- totalcost_no,
         col = par("fg"), lty = 2, lwd = 2)
segments(DALYS_lost_no - DALYS_lost_pVax_intflo, totalcost_pVax_intflo- totalcost_no, x1 = DALYS_lost_no - DALYS_lost_intflo_llAb_pVax, y1 = totalcost_intflo_llAb_pVax- totalcost_no,
         col = par("fg"), lty = 2, lwd = 2)
segments(0, 0, x1 = DALYS_lost_no - DALYS_lost_llAb, y1 = totalcost_llAb- totalcost_no,
         col = par("fg"), lty = 3, lwd = 2)
segments(DALYS_lost_no - DALYS_lost_llAb, totalcost_llAb- totalcost_no, x1 = DALYS_lost_no - DALYS_lost_intfhi_llAb_pVax, y1 = totalcost_intfhi_llAb_pVax- totalcost_no,
         col = par("fg"), lty = 3, lwd = 2)
legend("bottomright", legend = c("mAb", "mVax", "pVax 70% efficacy", "pVax 50% efficacy", "pVax 30% efficacy", "mAb + pVax 70% efficacy", "mAb + pVax 50% efficacy", "mAb + pVax 30% efficacy", "mVax + pVax 70% efficacy", "mVax + pVax 50% efficacy", "mVax + pVax 30% efficacy"),
       bty = "n", pch = c(19,19,19,17,15,19,17,15,19,17,15), col = c(UMBblue, UMBforest, UMBplum, UMBplum, UMBplum, UMByellow, UMByellow, UMByellow, UMBcharcoal, UMBcharcoal, UMBcharcoal))
quartz.save(file = "Figures/CE_plane.pdf", type = "pdf")

# # plot cost per DALYs averted with uncertainty
# plot(DALYS_lost_no_u - DALYS_lost_llAb_u, totalcost_llAb_u- totalcost_no_u, col = "blue", pch = 19, xlim = c(0,6000), ylim = c(0,7000000), xlab = "DALYs averted", ylab = 
#        "Added cost (USD)")
# points(DALYS_lost_no_u - DALYS_lost_mVax_u, totalcost_mVax_u- totalcost_no_u, col = "blue", pch = 15)
# points(DALYS_lost_no_u - DALYS_lost_pVax_u, totalcost_pVax_u- totalcost_no_u, col = "blue", pch = 17)
# points(DALYS_lost_no_u - DALYS_lost_joint_llAb_pVax_u, totalcost_joint_llAb_pVax_u - totalcost_no_u, col ="blue", pch = 18)
# points(DALYS_lost_no_u - DALYS_lost_joint_mVax_pVax_u, totalcost_joint_mVax_pVax_u - totalcost_no_u, col = "blue", pch = 10)
# points(DALYS_lost_no - DALYS_lost_llAb, totalcost_llAb- totalcost_no, col = "goldenrod", pch = 19)
# points(DALYS_lost_no - DALYS_lost_mVax, totalcost_mVax- totalcost_no, col = "goldenrod", pch = 15)
# points(DALYS_lost_no - DALYS_lost_pVax, totalcost_pVax- totalcost_no, col = "goldenrod", pch = 17)
# points(DALYS_lost_no - DALYS_lost_joint_llAb_pVax, totalcost_joint_llAb_pVax - totalcost_no, col = "goldenrod", pch = 18)
# points(DALYS_lost_no - DALYS_lost_joint_mVax_pVax, totalcost_joint_mVax_pVax - totalcost_no, col = "goldenrod", pch = 10)
# legend("bottomright", legend = c("llAb", "mVax", "pVax", "llAb + pVax, no intf", "mVax + pVax, no intf"),
#        pch = c(19,15,17,18,10), col = c("blue","blue", "blue", "blue", "blue"))

# # plot probability cost-effective across WTP values
# quartz("pCE by WTP", 8, 8)
# par(xaxs="i", yaxs="i")
# plot(WTP_sp, pce_pVax, ylim = c(0, 1), xlim = c(0,10000), bty = "l",
#      type = "l", lwd = 2, col = UMBplum,
#      xlab = "Society willingness to pay (USD)",
#      ylab = "Probability cost-effective")
# lines(WTP_sp, pce_llAb, col = UMBblue, lty = 1, lwd = 2)
# lines(WTP_sp, pce_mVax, col = UMBforest, lty = 1, lwd = 2)
# lines(WTP_sp, pce_joint_llAb_pVax, col = UMByellow, lty = 1, lwd = 2)
# lines(WTP_sp, pce_joint_mVax_pVax, col = UMBcharcoal, lty = 1, lwd = 2)
# lines(WTP_sp, pce_pVax_older, col = UMBsea, lty = 1, lwd = 2)
# lines(WTP_sp, pce_joint_llAb_pVax_older, col = UMBtan, lty = 1, lwd =2)
# lines(WTP_sp, pce_joint_mVax_pVax_older, col = UMBslate, lty = 1, lwd = 2)
# abline(v = CET_Mali_GDP, col = UMBgray, lty = 3)
# abline(v = 3*CET_Mali_GDP, col = UMBgray, lty = 3)
# text(CET_Mali_GDP, 0.92, labels = "1xGDP", srt = 45, cex = 0.80)
# text(3*CET_Mali_GDP, 0.92, labels = "3xGDP", srt = 45, cex = 0.80)
# legend("bottomright", legend = c("llAb", "mVax", "pVax", "llAb + pVax", "mVax + pVax", "pVax mo. 8 & 9", "llAb + pVax mo. 8 & 9", "mVax + pVax mo. 8 & 9"),
#        lty = 1, lwd = 2, bty = "n", col = c( UMBblue, UMBforest, UMBplum, UMByellow, UMBcharcoal, UMBsea, UMBtan, UMBslate))
# quartz.save(file = "Figures/pCE_by_WTP.pdf", type = "pdf")

# Figure 2
# plot probability optimal across WTP values
quartz("pOptimal by WTP", 10, 8)
par(xaxs="i", yaxs="i")
plot(WTP_sp, pO_pVax, ylim = c(0, 1), xlim = c(0,10000), bty = "l",
     type = "l", lwd = 3, col = UMBplum,
     xlab = "Society willingness to pay (USD)",
     ylab = "Probability optimal")
lines(WTP_sp, pO_no, col = UMBred, lty = 1, lwd = 3)
lines(WTP_sp, pO_llAb, col = UMBblue, lty = 1, lwd = 3)
lines(WTP_sp, pO_mVax, col = UMBforest, lty = 1, lwd = 3)
lines(WTP_sp, pO_llAb_pVax, col = UMByellow, lty = 1, lwd = 3)
lines(WTP_sp, pO_mVax_pVax, col = UMBcharcoal, lty = 1, lwd = 3)
lines(WTP_sp, pO_pVax_older, col = UMBsea, lty = 1, lwd = 3)
lines(WTP_sp, pO_llAb_pVax_older, col = UMBtan, lty = 1, lwd = 3)
lines(WTP_sp, pO_mVax_pVax_older, col = UMBslate, lty = 1, lwd = 3)
abline(v = CET_Mali_GDP, col = UMBgray, lty = 3, lwd = 2)
abline(v = 3*CET_Mali_GDP, col = UMBgray, lty = 3, lwd = 2)
text(CET_Mali_GDP, 0.92, labels = "1xGDP", srt = 45, cex = 0.80)
text(3*CET_Mali_GDP, 0.92, labels = "3xGDP", srt = 45, cex = 0.80)
legend("topright", ncol = 2, legend = c("status quo","mAb", "mVax", "pVax 10 & 14 wks", "mAb + pVax 10 & 14 wks", "mVax + pVax 10 & 14 wks", "pVax 8 & 9 mos", "mAb + pVax 8 & 9 mos", "mVax + pVax 8 & 9 mos"),
       lty = 1, lwd = 3, bty = "n", col = c(UMBred, UMBblue, UMBforest, UMBplum, UMByellow, UMBcharcoal, UMBsea, UMBtan, UMBslate))
quartz.save(file = "Figures/pOptimal_by_WTP.pdf", type = "pdf")

##
# quartz("SuppFig3", 10, 8)
# par(xaxs="i", yaxs="i")
# plot(WTP_sp, pO_pVax, ylim = c(0, 1), xlim = c(0,10000), bty = "l",
#      type = "l", lwd = 3, col = UMBplum,
#      xlab = "Society willingness to pay (USD)",
#      ylab = "Probability optimal")
# lines(WTP_sp, pO_no, col = UMBred, lty = 1, lwd = 3)
# lines(WTP_sp, pO_llAb, col = UMBblue, lty = 1, lwd = 3)
# lines(WTP_sp, pO_mVax, col = UMBforest, lty = 1, lwd = 3)
# lines(WTP_sp, pO_llAb_pVax, col = UMByellow, lty = 1, lwd = 3)
# lines(WTP_sp, pO_mVax_pVax, col = UMBcharcoal, lty = 1, lwd = 3)
# lines(WTP_sp, pO_pVax_older, col = UMBsea, lty = 1, lwd = 3)
# lines(WTP_sp, pO_llAb_pVax_older, col = UMBtan, lty = 1, lwd = 3)
# lines(WTP_sp, pO_mVax_pVax_older, col = UMBslate, lty = 1, lwd = 3)
# abline(v = CET_Mali_GDP, col = UMBgray, lty = 3, lwd = 2)
# abline(v = 3*CET_Mali_GDP, col = UMBgray, lty = 3, lwd = 2)
# text(CET_Mali_GDP, 0.92, labels = "1xGDP", srt = 45, cex = 0.80)
# text(3*CET_Mali_GDP, 0.92, labels = "3xGDP", srt = 45, cex = 0.80)
# legend(2800, 0.98, ncol = 2, cex = 1, legend = c("status quo","mAb", "mVax", "pVax 10 & 14 wks", "mAb + pVax 10 & 14 wks", "mVax + pVax 10 & 14 wks", "pVax 8 & 9 mos", "mAb + pVax 8 & 9 mos", "mVax + pVax 8 & 9 mos"),
#        lty = 1, lwd = 3, bty = "n", col = c(UMBred, UMBblue, UMBforest, UMBplum, UMByellow, UMBcharcoal, UMBsea, UMBtan, UMBslate))
# quartz.save(file = "Figures/SuppFig3.pdf", type = "pdf")
##

#plot probability optimal across increasing pVax efficacy
# when efficacy reduction is based on immune immaturity
quartz("efficacy reduction, immaturity", 10, 8)
par(xaxs="i", yaxs="i")
plot(eff_red*100, pO_pVax_5k_ser, ylim = c(0, 1), xlim = c(0,100), bty = "l",
     type = "l", col = UMBplum, lty = 1, lwd = 3,
     xlab = "Pediatric vaccine efficacy when administered at 10 & 14 weeks (%)",
     ylab = "Probability optimal")
lines(eff_red*100, pO_no_5k_ser, col = UMBred, lty = 1, lwd = 3)
lines(eff_red*100, pO_llAb_5k_ser, col = UMBblue, lty = 1, lwd = 3)
lines(eff_red*100, pO_mVax_5k_ser, col = UMBforest, lty = 1, lwd = 3)
lines(eff_red*100, pO_llAb_pVax_5k_ser, col = UMByellow, lty = 1, lwd = 3)
lines(eff_red*100, pO_mVax_pVax_5k_ser, col = UMBcharcoal, lty = 1, lwd = 3)
lines(eff_red*100, pO_pVax_older_5k_ser, col = UMBsea, lty = 1, lwd = 3)
lines(eff_red*100, pO_llAb_pVax_older_5k_ser, col = UMBtan, lty = 1, lwd = 3)
lines(eff_red*100, pO_mVax_pVax_older_5k_ser, col = UMBslate, lty = 1, lwd = 3)
legend("top", ncol =2, legend = c("status quo","mAb", "mVax", "pVax 10 & 14 wks", "mAb + pVax 10 & 14 wks", "mVax + pVax 10 & 14 wks", "pVax 8 & 9 mos", "mAb + pVax 8 & 9 mos", "mVax + pVax 8 & 9 mos"),
       lty = 1, lwd = 3, bty = "n", col = c(UMBred, UMBblue, UMBforest, UMBplum, UMByellow, UMBcharcoal, UMBsea, UMBtan, UMBslate))
quartz.save(file = "Figures/efficacy_reduction_immaturity.pdf", type = "pdf")

# plot probability optimal across increasing pVax efficacy 
# when efficacy reduction is based on interference
quartz("efficacy reduction, interference", 8, 8)
par(xaxs="i", yaxs="i")
plot(eff_red, pO_pVax_5k, ylim = c(0, 1), xlim = c(0,1), bty = "l",
     type = "l", lty = 4, lwd = 3, col = UMBplum,
     xlab = "Pediatric vaccine efficacy when administered at 10 & 14 weeks as part of a combination strategy",
     ylab = "Probability optimal")
lines(eff_red, pO_no_5k, col = UMBred, lty = 4, lwd = 3)
lines(eff_red, pO_llAb_5k, col = UMBblue, lty = 4, lwd = 3)
lines(eff_red, pO_mVax_5k, col = UMBforest, lty = 4, lwd = 3)
lines(eff_red, pO_llAb_pVax_5k, col = UMByellow, lty = 4, lwd = 3)
lines(eff_red, pO_mVax_pVax_5k, col = UMBcharcoal, lty = 4, lwd =3)
lines(eff_red, pO_pVax_older_5k, col = UMBsea, lty = 4, lwd = 3)
lines(eff_red, pO_llAb_pVax_older_5k, col = UMBtan, lty = 4, lwd = 3)
lines(eff_red, pO_mVax_pVax_older_5k, col = UMBslate, lty = 4, lwd =3)
legend("top", ncol =2, legend = c("status quo","mAb", "mVax", "pVax 10 & 14 wks", "mAb + pVax 10 & 14 wks", "mVax + pVax 10 & 14 wks", "pVax 8 & 9 mos", "mAb + pVax 8 & 9 mos", "mVax + pVax 8 & 9 mos"),
       lty = 4, lwd = 3, bty = "n", col = c(UMBred, UMBblue, UMBforest, UMBplum, UMByellow, UMBcharcoal, UMBsea, UMBtan, UMBslate))
quartz.save(file = "Figures/efficacy_reduction_interference.pdf", type = "pdf")

# Figure 3
# two-way sensitivity plot 1
# cost of adding EPI visit vs efficacy of pVax
quartz("cost EPI vs efficacy pVax", 8, 8)
par(mar = c(5.1, 4.1, 4.1, 2.1))
par(xaxs="i", yaxs="i")
image(x = c(eff_red[1]- 0.005, eff_red + 0.005)*100 ,
      y = c(EPI_cost[1]- 0.05, EPI_cost + 0.05),
      z = t(figdata),
      col = c(UMBred, NA, NA, NA, UMByellow, NA, NA, UMBtan, NA, NA),
      xlab="Efficacy of pediatric vaccine administered at 10 & 14 weeks (%)",
      ylab="Cost of adding a new immunization visit (USD)")
quartz.save(file = "Figures/costEPI_effpVax", type = "pdf")

# two-way sensitivity plot 3
# cost of llAb product vs. efficacy of pVax at 10 & 14 wks.
quartz("cost of llAb vs pVax eff", 8, 8)
par(mar = c(5.1, 4.1, 4.1, 2.1))
par(xaxs="i", yaxs="i")
image(x = c(eff_red[1]-0.005, eff_red + 0.005)*100,
      y = c(llAb_cost[1]- 0.125, llAb_cost + 0.125),
      z = t(SA_ller),
      col =c(UMBred, UMBforest, UMBplum, UMBcharcoal, UMBsea, UMBslate, UMBblue,
             UMByellow, UMBtan, NA),
      xlab="Efficacy of pediatric vaccine administered at 10 & 14 weeks (%)",
      ylab="Price of long-acting antibody product (USD)")
quartz.save(file = "Figures/costllAb_effpVax", type = "pdf")

# Legend on its own
quartz("color legend", 8,5)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend =c("status quo","long-acting mAb",
                            "maternal vaccine", "pediatric vaccine 10 & 14 wks",
                            "long-acting mAb + pediatric vaccine 10 & 14 wks",
                            "maternal vaccine + pediatric vaccine 10 & 14 wks",
                            "pediatric vaccine 8 & 9 mos",
                            "long acting mAb + pediatric vaccine 8 & 9 mos",
                            "maternal vaccine + pediatric vaccine 8 & 9 mos"),
       pch=15, pt.cex=3, cex = 1.5, bty='n',
       col = c(UMBred, UMBblue, UMBforest, UMBplum, UMByellow, UMBcharcoal,
               UMBsea, UMBtan, UMBslate))
# mtext("Intervention strategy", at=0.25, cex = 2)
quartz.save(file = "Figures/legend", type = "pdf")

######
