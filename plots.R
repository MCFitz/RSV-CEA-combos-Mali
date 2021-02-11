# plots
source("master.R")

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

# Plot cost per DALYs averted 
plot(DALYS_lost_no - DALYS_lost_llAb, totalcost_llAb- totalcost_no, col = UMBforest, pch = 19, xlim = c(0,6000), ylim = c(0,7000000), xlab = "DALYs averted", ylab = 
       "Added cost (USD)")
points(DALYS_lost_no - DALYS_lost_mVax, totalcost_mVax- totalcost_no, col = UMBblue, pch = 15)
points(DALYS_lost_no - DALYS_lost_pVax, totalcost_pVax- totalcost_no, col = UMBplum, pch = 17)
points(DALYS_lost_no - DALYS_lost_joint_llAb_pVax, totalcost_joint_llAb_pVax - totalcost_no, col = UMByellow, pch = 18)
points(DALYS_lost_no - DALYS_lost_joint_mVax_pVax, totalcost_joint_mVax_pVax - totalcost_no, col = UMBcharcoal, pch = 10)
points(DALYS_lost_no - DALYS_lost_intfhi_llAb_pVax, totalcost_intfhi_llAb_pVax - totalcost_no, col = UMByellow, pch =18)
points(DALYS_lost_no - DALYS_lost_intfhi_mVax_pVax, totalcost_intfhi_mVax_pVax - totalcost_no, col = UMBcharcoal, pch =10)
points(DALYS_lost_no - DALYS_lost_intflo_llAb_pVax, totalcost_intflo_llAb_pVax - totalcost_no, col = UMByellow, pch =18)
points(DALYS_lost_no - DALYS_lost_intflo_mVax_pVax, totalcost_intflo_mVax_pVax - totalcost_no, col = UMBcharcoal, pch =10)
legend("bottomright", legend = c("llAb", "mVax", "pVax", "llAb + pVax, no efficacy reduction", "llAb + pVax, hi efficacy reduction", "llAb + pVax, lo efficacy reduction", "mVax + pVax, no efficacy reduction", "mVax + pVax, hi efficacy reduction", "mVax + pVax, lo efficacy reduction"),
       bty = "n", pch = c(19,15,17,18,18,18,10,10,10), col = c(UMBforest, UMBblue, UMBplum, UMByellow, UMByellow, UMByellow, UMBcharcoal, UMBcharcoal, UMBcharcoal))

# plot cost per DALYs averted with uncertainty
plot(DALYS_lost_no_u - DALYS_lost_llAb_u, totalcost_llAb_u- totalcost_no_u, col = "blue", pch = 19, xlim = c(0,6000), ylim = c(0,7000000), xlab = "DALYs averted", ylab = 
       "Added cost (USD)")
points(DALYS_lost_no_u - DALYS_lost_mVax_u, totalcost_mVax_u- totalcost_no_u, col = "blue", pch = 15)
points(DALYS_lost_no_u - DALYS_lost_pVax_u, totalcost_pVax_u- totalcost_no_u, col = "blue", pch = 17)
points(DALYS_lost_no_u - DALYS_lost_joint_llAb_pVax_u, totalcost_joint_llAb_pVax_u - totalcost_no_u, col ="blue", pch = 18)
points(DALYS_lost_no_u - DALYS_lost_joint_mVax_pVax_u, totalcost_joint_mVax_pVax_u - totalcost_no_u, col = "blue", pch = 10)
points(DALYS_lost_no - DALYS_lost_llAb, totalcost_llAb- totalcost_no, col = "goldenrod", pch = 19)
points(DALYS_lost_no - DALYS_lost_mVax, totalcost_mVax- totalcost_no, col = "goldenrod", pch = 15)
points(DALYS_lost_no - DALYS_lost_pVax, totalcost_pVax- totalcost_no, col = "goldenrod", pch = 17)
points(DALYS_lost_no - DALYS_lost_joint_llAb_pVax, totalcost_joint_llAb_pVax - totalcost_no, col = "goldenrod", pch = 18)
points(DALYS_lost_no - DALYS_lost_joint_mVax_pVax, totalcost_joint_mVax_pVax - totalcost_no, col = "goldenrod", pch = 10)
legend("bottomright", legend = c("llAb", "mVax", "pVax", "llAb + pVax, no intf", "mVax + pVax, no intf"),
       pch = c(19,15,17,18,10), col = c("blue","blue", "blue", "blue", "blue"))

# plot probability cost-effective across WTP values
plot(WTP_sp, pce_pVax, ylim = c(0, 1), xlim = c(0,10000), bty = "l",
     type = "l", lwd = 2, col = UMBplum,
     xlab = "Society willingness to pay (USD)",
     ylab = "Probability cost-effective")
lines(WTP_sp, pce_llAb, col = UMBforest, lty = 1, lwd = 2)
lines(WTP_sp, pce_mVax, col = UMBblue, lty = 1, lwd = 2)
lines(WTP_sp, pce_joint_llAb_pVax, col = UMByellow, lty = 1, lwd = 2)
lines(WTP_sp, pce_joint_mVax_pVax, col = UMBcharcoal, lty = 1, lwd = 2)
lines(WTP_sp, pce_pVax_older, col = UMBsea, lty = 1, lwd = 2)
lines(WTP_sp, pce_joint_llAb_pVax_older, col = UMBtan, lty = 1, lwd =2)
lines(WTP_sp, pce_joint_mVax_pVax_older, col = UMBslate, lty = 1, lwd = 2)
abline(v = CET_Mali_GDP, col = UMBgray, lty = 3)
abline(v = 3*CET_Mali_GDP, col = UMBgray, lty = 3)
text(CET_Mali_GDP, 0.92, labels = "1xGDP", srt = 45, cex = 0.80)
text(3*CET_Mali_GDP, 0.92, labels = "3xGDP", srt = 45, cex = 0.80)
legend("bottomright", legend = c("llAb", "mVax", "pVax", "llAb + pVax", "mVax + pVax", "pVax mo. 6 & 7", "llAb + pVax mo. 6 & 7", "mVax + pVax mo. 6 & 7"),
       lty = 1, lwd = 2, bty = "n", col = c( UMBforest, UMBblue, UMBplum, UMByellow, UMBcharcoal, UMBsea, UMBtan, UMBslate))

# plot probability optimal across WTP values
plot(WTP_sp, pO_pVax, ylim = c(0, 1), xlim = c(0,10000), bty = "l",
     type = "l", lwd = 2, col = UMBplum,
     xlab = "Society willingness to pay (USD)",
     ylab = "Probability optimal")
lines(WTP_sp, pO_no, col = UMBred, lty = 1, lwd = 2)
lines(WTP_sp, pO_llAb, col = UMBforest, lty = 1, lwd = 2)
lines(WTP_sp, pO_mVax, col = UMBblue, lty = 1, lwd = 2)
lines(WTP_sp, pO_llAb_pVax, col = UMByellow, lty = 1, lwd = 2)
lines(WTP_sp, pO_mVax_pVax, col = UMBcharcoal, lty = 1, lwd =2)
lines(WTP_sp, pO_pVax_older, col = UMBsea, lty = 1, lwd = 2)
lines(WTP_sp, pO_llAb_pVax_older, col = UMBtan, lty = 1, lwd = 2)
lines(WTP_sp, pO_mVax_pVax_older, col = UMBslate, lty = 1, lwd =2)
abline(v = CET_Mali_GDP, col = UMBgray, lty = 3)
abline(v = 3*CET_Mali_GDP, col = UMBgray, lty = 3)
text(CET_Mali_GDP, 0.92, labels = "1xGDP", srt = 45, cex = 0.80)
text(3*CET_Mali_GDP, 0.92, labels = "3xGDP", srt = 45, cex = 0.80)
legend("right", legend = c("status quo","llAb", "mVax", "pVax", "llAb + pVax", "mVax + pVax", "pVax mo. 6 & 7", "llAb + pVax mo. 6 & 7", "mVax + pVax mo. 6 & 7"),
       lty = 1, lwd = 2, bty = "n", col = c(UMBred, UMBforest, UMBblue, UMBplum, UMByellow, UMBcharcoal, UMBsea, UMBtan, UMBslate))

#plot probability optimal across increasing pVax efficacy reduction
plot(eff_red, pO_pVax_5k, ylim = c(0, 1), xlim = c(0,1), bty = "l",
     type = "l", lwd = 2, col = UMBplum,
     xlab = "Efficacy reduction in pediatric vaccine",
     ylab = "Probability optimal")
lines(eff_red, pO_no_5k, col = UMBred, lty = 1, lwd = 2)
lines(eff_red, pO_llAb_5k, col = UMBforest, lty = 1, lwd = 2)
lines(eff_red, pO_mVax_5k, col = UMBblue, lty = 1, lwd = 2)
lines(eff_red, pO_llAb_pVax_5k, col = UMByellow, lty = 1, lwd = 2)
lines(eff_red, pO_mVax_pVax_5k, col = UMBcharcoal, lty = 1, lwd =2)
lines(eff_red, pO_pVax_older_5k, col = UMBsea, lty = 1, lwd = 2)
lines(eff_red, pO_llAb_pVax_older_5k, col = UMBtan, lty = 1, lwd = 2)
lines(eff_red, pO_mVax_pVax_older_5k, col = UMBslate, lty = 1, lwd =2)
lines(eff_red, pO_pVax_5k_ser, col = UMBplum, lty = 4, lwd = 2)
lines(eff_red, pO_no_5k_ser, col = UMBred, lty = 4, lwd = 2)
lines(eff_red, pO_llAb_5k_ser, col = UMBforest, lty = 4, lwd = 2)
lines(eff_red, pO_mVax_5k_ser, col = UMBblue, lty = 4, lwd = 2)
lines(eff_red, pO_llAb_pVax_5k_ser, col = UMByellow, lty = 4, lwd = 2)
lines(eff_red, pO_mVax_pVax_5k_ser, col = UMBcharcoal, lty = 4, lwd =2)
lines(eff_red, pO_pVax_older_5k_ser, col = UMBsea, lty = 4, lwd = 2)
lines(eff_red, pO_llAb_pVax_older_5k_ser, col = UMBtan, lty = 4, lwd = 2)
lines(eff_red, pO_mVax_pVax_older_5k_ser, col = UMBslate, lty = 4, lwd =2)
legend("top", ncol =2, legend = c("status quo","llAb", "mVax", "pVax", "llAb + pVax", "mVax + pVax", "pVax mo. 6 & 7", "llAb + pVax mo. 6 & 7", "mVax + pVax mo. 6 & 7"),
       lty = 1, lwd = 2, bty = "n", col = c(UMBred, UMBforest, UMBblue, UMBplum, UMByellow, UMBcharcoal, UMBsea, UMBtan, UMBslate))

######
