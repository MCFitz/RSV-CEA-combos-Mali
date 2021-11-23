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

# make standard vector for colors
UMB1 <- c(UMBred, UMBblue, UMBforest, UMBplum, UMByellow, UMBcharcoal,
          UMBsea, UMBtan, UMBslate)

# For figures on slides, make font larger
# cex.lab = 1.5, cex.axis = 1.5

# Figure 1
# CE plane, Plot cost per DALYs averted 
quartz("CE plane", 8, 8)
par(xaxs="i", yaxs="i")
plot(DALYS_lost_no - DALYS_lost_llAb, totalcost_llAb- totalcost_no, col = UMBblue, bty = "n", pch = 19, cex = 1.5, xlim = c(0,2500), ylim = c(0,5000000), xlab = "DALYs averted", ylab = 
       "Incremental cost compared to status quo (USD)")
points(DALYS_lost_no - DALYS_lost_mVax, totalcost_mVax- totalcost_no, col = UMBforest, pch = 19, cex =1.5)
points(DALYS_lost_no - DALYS_lost_pVax, totalcost_pVax- totalcost_no, col = UMBplum, pch = 19, cex = 1.5)
points(DALYS_lost_no - DALYS_lost_joint_llAb_pVax, totalcost_joint_llAb_pVax - totalcost_no, col = UMByellow, pch = 19, cex = 1.5)
points(DALYS_lost_no - DALYS_lost_joint_mVax_pVax, totalcost_joint_mVax_pVax - totalcost_no, col = UMBcharcoal, pch = 19, cex = 1.5)
points(DALYS_lost_no - DALYS_lost_pVax_older, totalcost_pVax_older - totalcost_no, col = UMBtan, pch = 19,  cex = 1.5)
points(DALYS_lost_no - DALYS_lost_joint_llAb_pVax_older, totalcost_joint_llAb_pVax_older - totalcost_no, col = UMBred, pch = 19, cex = 1.5)
points(DALYS_lost_no - DALYS_lost_joint_mVax_pVax_older, totalcost_joint_mVax_pVax_older - totalcost_no, col = UMBsea, pch = 19, cex = 1.5)

segments(0, 0, x1 = DALYS_lost_no - DALYS_lost_llAb, y1 = totalcost_llAb- totalcost_no,
         col = par("fg"), lty = 2, lwd = 2)
segments(DALYS_lost_no - DALYS_lost_llAb, totalcost_llAb- totalcost_no, x1 = DALYS_lost_no - DALYS_lost_joint_llAb_pVax, y1 = totalcost_joint_llAb_pVax- totalcost_no,
         col = par("fg"), lty = 2, lwd = 2)
legend("bottomright", legend = c("mAb", "mVax", "pVax", "mAb + pVax", "mVax + pVax", "pVax older", "mAb + pVax older", "mVax + pVax older"),
       bty = "n", pch = 19, col = c(UMBblue, UMBforest, UMBplum, UMByellow, UMBcharcoal, UMBtan, UMBred, UMBsea))
quartz.save(file = "Figures/CE_plane.pdf", type = "pdf")

# plot cost per DALYs averted with uncertainty
par(mfrow=c(1,1))
plot(DALYS_lost_no_u - DALYS_lost_llAb_u, totalcost_llAb_u- totalcost_no_u, col = "blue", pch = 19, xlim = c(-1000,6000), ylim = c(0,4000000), xlab = "DALYs averted", ylab =
       "Added cost (USD)")
points(DALYS_lost_no_u - DALYS_lost_mVax_u, totalcost_mVax_u- totalcost_no_u, col = "red", pch = 15)
points(DALYS_lost_no_u - DALYS_lost_pVax_u, totalcost_pVax_u- totalcost_no_u, col = "green", pch = 17)
points(DALYS_lost_no_u - DALYS_lost_joint_llAb_pVax_u, totalcost_joint_llAb_pVax_u - totalcost_no_u, col ="orange", pch = 18)
points(DALYS_lost_no_u - DALYS_lost_joint_mVax_pVax_u, totalcost_joint_mVax_pVax_u - totalcost_no_u, col = "purple", pch = 10)
points(DALYS_lost_no - DALYS_lost_llAb, totalcost_llAb- totalcost_no, col = "goldenrod", pch = 19)
points(DALYS_lost_no - DALYS_lost_mVax, totalcost_mVax- totalcost_no, col = "goldenrod", pch = 15)
points(DALYS_lost_no - DALYS_lost_pVax, totalcost_pVax- totalcost_no, col = "goldenrod", pch = 17)
points(DALYS_lost_no - DALYS_lost_joint_llAb_pVax, totalcost_joint_llAb_pVax - totalcost_no, col = "goldenrod", pch = 18)
points(DALYS_lost_no - DALYS_lost_joint_mVax_pVax, totalcost_joint_mVax_pVax - totalcost_no, col = "goldenrod", pch = 10)
legend("bottomright", legend = c("llAb", "mVax", "pVax", "llAb + pVax", "mVax + pVax"),
       pch = c(19,15,17,18,10), col = c("blue","red", "green", "orange", "purple"))

# plot CE plane with clouds
# show how top and bottom 10% of prob LRTI given RSV influences ICERs
par(mfrow=c(1,1))
plot(DALYS_lost_no_u - DALYS_lost_llAb_u, totalcost_llAb_u- totalcost_no_u, col = "blue", pch = 19, xlim = c(-1000,6000), ylim = c(0,4000000), xlab = "DALYs averted", ylab =
        "Added cost (USD)")
points(DALYS_lost_no_u[dec_pneum_set_l] - DALYS_lost_llAb_u[dec_pneum_set_l], totalcost_llAb_u[dec_pneum_set_l] - totalcost_no_u[dec_pneum_set_l], col = "cadetblue1", pch = 19)
points(DALYS_lost_no_u[dec_pneum_set_u] - DALYS_lost_llAb_u[dec_pneum_set_u], totalcost_llAb_u[dec_pneum_set_u] - totalcost_no_u[dec_pneum_set_u], col = "darkblue", pch = 19)
points(DALYS_lost_no_u - DALYS_lost_mVax_u, totalcost_mVax_u- totalcost_no_u, col = "red", pch = 15)
points(DALYS_lost_no_u[dec_pneum_set_l] - DALYS_lost_mVax_u[dec_pneum_set_l], totalcost_mVax_u[dec_pneum_set_l] - totalcost_no_u[dec_pneum_set_l], col = "indianred1", pch = 15)
points(DALYS_lost_no_u[dec_pneum_set_u] - DALYS_lost_mVax_u[dec_pneum_set_u], totalcost_mVax_u[dec_pneum_set_u] - totalcost_no_u[dec_pneum_set_u], col = "darkred", pch = 15)
points(DALYS_lost_no_u - DALYS_lost_pVax_u, totalcost_pVax_u- totalcost_no_u, col = "yellowgreen", pch = 17)
points(DALYS_lost_no_u[dec_pneum_set_l] - DALYS_lost_pVax_u[dec_pneum_set_l], totalcost_pVax_u[dec_pneum_set_l] - totalcost_no_u[dec_pneum_set_l], col = "green", pch = 17)
points(DALYS_lost_no_u[dec_pneum_set_u] - DALYS_lost_pVax_u[dec_pneum_set_u], totalcost_pVax_u[dec_pneum_set_u] - totalcost_no_u[dec_pneum_set_u], col = "darkgreen", pch = 17)
points(DALYS_lost_no_u - DALYS_lost_joint_llAb_pVax_u, totalcost_joint_llAb_pVax_u - totalcost_no_u, col ="coral", pch = 18)
points(DALYS_lost_no_u[dec_pneum_set_l] - DALYS_lost_joint_llAb_pVax_u[dec_pneum_set_l], totalcost_joint_llAb_pVax_u[dec_pneum_set_l] - totalcost_no_u[dec_pneum_set_l], col = "orange", pch =18)
points(DALYS_lost_no_u[dec_pneum_set_u] - DALYS_lost_joint_llAb_pVax_u[dec_pneum_set_u], totalcost_joint_llAb_pVax_u[dec_pneum_set_u] - totalcost_no_u[dec_pneum_set_u], col = "darkorange3", pch =18)
points(DALYS_lost_no_u - DALYS_lost_joint_mVax_pVax_u, totalcost_joint_mVax_pVax_u - totalcost_no_u, col = "purple", pch = 10)
points(DALYS_lost_no_u[dec_pneum_set_l] - DALYS_lost_joint_mVax_pVax_u[dec_pneum_set_l], totalcost_joint_mVax_pVax_u[dec_pneum_set_l] - totalcost_no_u[dec_pneum_set_l], col = "mediumorchid1", pch = 10)
points(DALYS_lost_no_u[dec_pneum_set_u] - DALYS_lost_joint_mVax_pVax_u[dec_pneum_set_u], totalcost_joint_mVax_pVax_u[dec_pneum_set_u] - totalcost_no_u[dec_pneum_set_u], col = "darkorchid4", pch = 10)
points(DALYS_lost_no - DALYS_lost_llAb, totalcost_llAb- totalcost_no, col = "goldenrod", pch = 19)
points(DALYS_lost_no - DALYS_lost_mVax, totalcost_mVax- totalcost_no, col = "goldenrod", pch = 15)
points(DALYS_lost_no - DALYS_lost_pVax, totalcost_pVax- totalcost_no, col = "goldenrod", pch = 17)
points(DALYS_lost_no - DALYS_lost_joint_llAb_pVax, totalcost_joint_llAb_pVax - totalcost_no, col = "goldenrod", pch = 18)
points(DALYS_lost_no - DALYS_lost_joint_mVax_pVax, totalcost_joint_mVax_pVax - totalcost_no, col = "goldenrod", pch = 10)
legend("bottomright", legend = c("llAb", "mVax", "pVax", "llAb + pVax", "mVax + pVax"),
       pch = c(19,15,17,18,10), col = c("blue","red", "green", "orange", "purple"))





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
# abline(v = 3*CET_Mali_GDP, col = UMBgray, lty = 3, lwd = 2)
text(CET_Mali_GDP, 0.92, labels = "1xGDP", srt = 45, cex = 0.80)
# text(3*CET_Mali_GDP, 0.92, labels = "3xGDP", srt = 45, cex = 0.80)
legend("topright", ncol = 2, legend = c("status quo","mAb", "mVax", "pVax 10 & 14 wks", "mAb + pVax 10 & 14 wks", "mVax + pVax 10 & 14 wks", "pVax 8 & 9 mos", "mAb + pVax 8 & 9 mos", "mVax + pVax 8 & 9 mos"),
       lty = 1, lwd = 3, bty = "n", col = UMB1)
quartz.save(file = "Figures/pOptimal_by_WTP.pdf", type = "pdf")


# plot probability optimal across product cost
quartz("pOptimal by product cost", 10, 8)
par(xaxs="i", yaxs="i")
plot(cprod, pO_pVax_pspan, ylim = c(0, 1), xlim = c(0, max(cprod)), bty = "l",
     type = "l", lwd = 3, col = UMBplum,
     xlab = "Product cost (USD)",
     ylab = "Probability optimal")
lines(cprod, pO_no_pspan, col = UMBred, lty = 1, lwd = 3)
lines(cprod, pO_llAb_pspan, col = UMBblue, lty = 1, lwd = 3)
lines(cprod, pO_mVax_pspan, col = UMBforest, lty = 1, lwd = 3)
lines(cprod, pO_llAb_pVax_pspan, col = UMByellow, lty = 1, lwd = 3)
lines(cprod, pO_mVax_pVax_pspan, col = UMBcharcoal, lty = 1, lwd = 3)
lines(cprod, pO_pVax_older_pspan, col = UMBsea, lty = 1, lwd = 3)
lines(cprod, pO_llAb_pVax_older_pspan, col = UMBtan, lty = 1, lwd = 3)
lines(cprod, pO_mVax_pVax_older_pspan, col = UMBslate, lty = 1, lwd = 3)
abline(v = 1.03, col = UMBgray, lty = 3, lwd = 2)
abline(v = 1.50, col = UMBgray, lty = 3, lwd = 2)
text(1.03, 0.95, labels = "Penta", srt = 45, cex = 0.80)
text(1.50, 0.95, labels = "TCV", srt = 45, cex = 0.80)
legend("right", ncol = 1, legend = c("status quo","mAb", "mVax", "pVax 10 & 14 wks", "mAb + pVax 10 & 14 wks", "mVax + pVax 10 & 14 wks", "pVax 8 & 9 mos", "mAb + pVax 8 & 9 mos", "mVax + pVax 8 & 9 mos"),
       lty = 1, lwd = 3, bty = "n", col = UMB1)
quartz.save(file = "Figures/pOptimal_by_product_cost.pdf", type = "pdf")


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

# plot probability optimal across increasing pVax efficacy
# when efficacy reduction is based on immune immaturity
quartz("efficacy reduction, immaturity", 10, 8)
par(xaxs="i", yaxs="i")
plot(eff_red*100, pO_pVax_ser, ylim = c(0, 1), xlim = c(0,100), bty = "l",
     type = "l", col = UMBplum, lty = 1, lwd = 3,
     xlab = "Pediatric vaccine efficacy when administered at 10 & 14 weeks (%)",
     ylab = "Probability optimal")
lines(eff_red*100, pO_no_ser, col = UMBred, lty = 1, lwd = 3)
lines(eff_red*100, pO_llAb_ser, col = UMBblue, lty = 1, lwd = 3)
lines(eff_red*100, pO_mVax_ser, col = UMBforest, lty = 1, lwd = 3)
lines(eff_red*100, pO_llAb_pVax_ser, col = UMByellow, lty = 1, lwd = 3)
lines(eff_red*100, pO_mVax_pVax_ser, col = UMBcharcoal, lty = 1, lwd = 3)
lines(eff_red*100, pO_pVax_older_ser, col = UMBsea, lty = 1, lwd = 3)
lines(eff_red*100, pO_llAb_pVax_older_ser, col = UMBtan, lty = 1, lwd = 3)
lines(eff_red*100, pO_mVax_pVax_older_ser, col = UMBslate, lty = 1, lwd = 3)
abline(v = 70, col = UMBgray, lty = 3, lwd = 2)
text(70, 0.92, labels = "70% efficacy", srt = 45, cex = 0.80)
legend("left", ncol =1, legend = c("status quo","mAb", "mVax", "pVax 10 & 14 wks", "mAb + pVax 10 & 14 wks", "mVax + pVax 10 & 14 wks", "pVax 8 & 9 mos", "mAb + pVax 8 & 9 mos", "mVax + pVax 8 & 9 mos"),
       lty = 1, lwd = 3, bty = "n", col = UMB1)
quartz.save(file = "Figures/efficacy_reduction_immaturity.pdf", type = "pdf")


# plot probability optimal across increasing pVax efficacy 
# when efficacy reduction is based on interference
quartz("efficacy reduction, interference", 10, 8)
par(xaxs="i", yaxs="i")
plot(eff_red*100, pO_pVax_er, ylim = c(0, 1), xlim = c(0,100), bty = "l",
     type = "l", lty = 4, lwd = 3, col = UMBplum,
     xlab = "Pediatric vaccine efficacy when administered at 10 & 14 weeks as part of a combination strategy (%)",
     ylab = "Probability optimal")
lines(eff_red*100, pO_no_er, col = UMBred, lty = 4, lwd = 3)
lines(eff_red*100, pO_llAb_er, col = UMBblue, lty = 4, lwd = 3)
lines(eff_red*100, pO_mVax_er, col = UMBforest, lty = 4, lwd = 3)
lines(eff_red*100, pO_llAb_pVax_er, col = UMByellow, lty = 4, lwd = 3)
lines(eff_red*100, pO_mVax_pVax_er, col = UMBcharcoal, lty = 4, lwd =3)
lines(eff_red*100, pO_pVax_older_er, col = UMBsea, lty = 4, lwd = 3)
lines(eff_red*100, pO_llAb_pVax_older_er, col = UMBtan, lty = 4, lwd = 3)
lines(eff_red*100, pO_mVax_pVax_older_er, col = UMBslate, lty = 4, lwd =3)
abline(v = 70, col = UMBgray, lty = 3, lwd = 2)
text(70, 0.92, labels = "70% efficacy", srt = 45, cex = 0.80)
legend("left", ncol =1, legend = c("status quo","mAb", "mVax", "pVax 10 & 14 wks", "mAb + pVax 10 & 14 wks", "mVax + pVax 10 & 14 wks", "pVax 8 & 9 mos", "mAb + pVax 8 & 9 mos", "mVax + pVax 8 & 9 mos"),
       lty = 4, lwd = 3, bty = "n", col = UMB1)
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
      col = c(UMB1, NA),
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
      col =c(UMB1, NA),
      xlab="Efficacy of pediatric vaccine administered at 10 & 14 weeks (%)",
      ylab="Price of long-acting antibody product (USD)")
quartz.save(file = "Figures/costllAb_effpVax", type = "pdf")

# two-way sensitivity plot 4
# cost llAb product vs cost pVax product per dose
quartz("cost of llAb vs cost pVax", 8, 8)
par(mar = c(5.1, 4.1, 4.1, 2.1))
par(xaxs="i", yaxs="i")
image(x = c(pVax_cost[1] - 0.025, pVax_cost + 0.025),
      y = c(llAb_cost[1] - 0.025, llAb_cost + 0.025),
      z = t(SA_llpv),
      col = c(UMB1, NA),
      xlab = "Price of pediatric vaccine product per dose (USD)",
      ylab = "Price of long-acting antibody product (USD)")
quartz.save(file = "Figures/costllAb_costpVax", type = "pdf")


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
       col = UMB1)
# mtext("Intervention strategy", at=0.25, cex = 2)
quartz.save(file = "Figures/legend", type = "pdf")
###

####
# 4 Panel Figure with Health Outcomes by month of age
# [RSV cases, LRTI episodes, hospitalizations, deaths]
# rep(cases_no_age, ni)
# cases_no_age, cases_llAb_age, cases_mVax_age, cases_pVax_age, cases_joint_llAb_pVax_age, cases_pVax_older_age
  
ni <- 6 # number of interventions to include in the plot
nm <- 4 # number of health metrics to include in the plot
   
HO_df <- data.frame(age = rep(rep(months, ni), nm),
                    intervention = rep(c(rep("no intervention", length(months)),
                                     rep("llAb", length(months)),
                                     rep("mVax", length(months)),
                                     rep("pVax", length(months)),
                                     rep("llAb + pVax", length(months)),
                                     rep("pVax older", length(months))), nm),
                    metric = c(rep("RSV cases", length(months) * ni),
                                                     rep("LRTI episodes", length(months) * ni),
                                                     rep("Hospitalizations", length(months) * ni),
                                                     rep("Deaths", length(months) * ni)),
                    value = c(rep(cases_no_age, ni),
                              LRTI_no_age, LRTI_llAb_age, LRTI_mVax_age, LRTI_pVax_age, LRTI_joint_llAb_pVax_age, LRTI_pVax_older_age,
                              inpat_no_age, inpat_llAb_age, inpat_mVax_age, inpat_pVax_age, inpat_joint_llAb_pVax_age, inpat_pVax_older_age,
                              deaths_no_age, deaths_llAb_age, deaths_mVax_age, deaths_pVax_age, deaths_joint_llAb_pVax_age, deaths_pVax_older_age))

HO_df$metric <- factor(HO_df$metric, levels = c("RSV cases", "LRTI episodes", "Hospitalizations", "Deaths"))
HO_df$intervention <- factor(HO_df$intervention, levels = c("no intervention", "pVax older", "mVax", "pVax", "llAb", "llAb + pVax"))

temp_HO <- HO_df %>% mutate(bin = floor((age-1) / 6) + 1) %>% 
   group_by(bin, intervention, metric) %>% 
   summarise(tot = sum(value))

###
quartz("Health Outcomes Barplot", 12, 8)
ggplot(temp_HO, aes(x = bin, y = tot, fill = intervention)) +
   geom_bar(position = 'dodge', stat = 'identity') +
   scale_x_continuous(breaks = seq(1, 6, by = 1), labels = c("<6", "6-<12", "12-<18", "18-<24", "24-<30", "30-<36")) +
   xlab(NULL) +
   facet_wrap(~metric, scales = "free") +
   scale_fill_manual(values = brewer.pal(ni, "Set2")) +
   ylab("") +
   xlab("Month of age") +
   theme_bw() +
   theme(legend.title = element_blank())
quartz.save(file = "Figures/Health_Outcomes_Barplot", type = "pdf")
###

# quartz("Health Outcomes Barplot, Secondary Analysis", 12, 8)
# ggplot(temp_HO, aes(x = bin, y = tot, fill = intervention)) +
#    geom_bar(position = 'dodge', stat = 'identity') +
#    scale_x_continuous(breaks = seq(1, 6, by = 1), labels = c("<6", "6-<12", "12-<18", "18-<24", "24-<30", "30-<36")) +
#    xlab(NULL) +
#    facet_wrap(~metric, scales = "free") +
#    scale_fill_manual(values = brewer.pal(ni, "Set2")) +
#    ylab("") +
#    xlab("Month of age") +
#    theme_bw() +
#    theme(legend.title = element_blank())
# quartz.save(file = "Figures/RSV_prevented_Health_Outcomes_Barplot", type = "pdf")

###
# Input Params Histogram Panel Plots
# do ARs and CFRs by age separately
# need to add uncertainty for probability inpatient (by age)
param_names <- c("probability LRTI given RSV", "cost outpatient care", "cost inpatient care",
                 "duration of illness (years)", "disability weight LRTI severe", "disability weight LRTI moderate")
params_df <- data.frame(id = rep(param_names, each = trials),
                        value = c(p_pneum_u, cost_outpatient_u, cost_hosp_u, di_yrs_u, dw_LRTI_severe_u, dw_LRTI_mod_u),
                        pe = c(rep(p_pneum, trials), rep(cost_outpatient, trials),
                               rep(cost_hosp, trials), rep(di_yrs, trials), rep(dw_LRTI_severe, trials), rep(dw_LRTI_mod, trials)))
quartz("Input Parameter Histograms", 12, 8)
ggplot(params_df, aes(value)) + geom_histogram(color = "black", fill = "grey", bins = 15) +
   facet_wrap(~id, scales = "free") +
   geom_vline(params_df, mapping = aes(xintercept= pe), color="red") +
   theme_bw()
quartz.save(file ="Figures/Input_Parameter_Histograms.pdf", type = "pdf")

AR_age_df <- data.frame(month = rep(1:36, each = trials),
                         value = c(AR_age_y_u[,1], AR_age_y_u[,2], AR_age_y_u[,3], AR_age_y_u[,4], AR_age_y_u[,5], AR_age_y_u[,6],
                                   AR_age_y_u[,7], AR_age_y_u[,8], AR_age_y_u[,9], AR_age_y_u[,10], AR_age_y_u[,11], AR_age_y_u[,12],
                                   AR_age_y_u[,13], AR_age_y_u[,14], AR_age_y_u[,15], AR_age_y_u[,16], AR_age_y_u[,17], AR_age_y_u[,18],
                                   AR_age_y_u[,19], AR_age_y_u[,20], AR_age_y_u[,21], AR_age_y_u[,22], AR_age_y_u[,23], AR_age_y_u[,24],
                                   AR_age_y_u[,25], AR_age_y_u[,26], AR_age_y_u[,27], AR_age_y_u[,28], AR_age_y_u[,29], AR_age_y_u[,30],
                                   AR_age_y_u[,31], AR_age_y_u[,32], AR_age_y_u[,33], AR_age_y_u[,34], AR_age_y_u[,35], AR_age_y_u[,36]),
                         pe = rep(age_AR_y, each = trials))
quartz("Attack Rates by Month of Age Histograms", 12, 8)
ggplot(AR_age_df, aes(value)) + geom_histogram(color = "black", fill = "grey", bins = 15) +
   facet_wrap(~month, scales = "free") +
   geom_vline(AR_age_df, mapping = aes(xintercept= pe), color="red") +
   theme_bw()
quartz.save(file ="Figures/Attack_Rates_by_Month_of_Age_Histograms.pdf", type = pdf)

CFR_age_df <- data.frame(month = rep(1:36, each = trials),
                        value = c(CFR_age_mat_3y[,1], CFR_age_mat_3y[,2], CFR_age_mat_3y[,3], CFR_age_mat_3y[,4], CFR_age_mat_3y[,5], CFR_age_mat_3y[,6],
                                  CFR_age_mat_3y[,7], CFR_age_mat_3y[,8], CFR_age_mat_3y[,9], CFR_age_mat_3y[,10], CFR_age_mat_3y[,11], CFR_age_mat_3y[,12],
                                  CFR_age_mat_3y[,13], CFR_age_mat_3y[,14], CFR_age_mat_3y[,15], CFR_age_mat_3y[,16], CFR_age_mat_3y[,17], CFR_age_mat_3y[,18],
                                  CFR_age_mat_3y[,19], CFR_age_mat_3y[,20], CFR_age_mat_3y[,21], CFR_age_mat_3y[,22], CFR_age_mat_3y[,23], CFR_age_mat_3y[,24],
                                  CFR_age_mat_3y[,25], CFR_age_mat_3y[,26], CFR_age_mat_3y[,27], CFR_age_mat_3y[,28], CFR_age_mat_3y[,29], CFR_age_mat_3y[,30],
                                  CFR_age_mat_3y[,31], CFR_age_mat_3y[,32], CFR_age_mat_3y[,33], CFR_age_mat_3y[,34], CFR_age_mat_3y[,35], CFR_age_mat_3y[,36]),
                        pe = rep(CFR_by_age, each = trials))
quartz("Case Fatality Rate by Month of Age", 12, 8)
ggplot(CFR_age_df, aes(value)) + geom_histogram(color = "black", fill = "grey", bins = 15) +
   facet_wrap(~month, scales = "free") +
   geom_vline(CFR_age_df, mapping = aes(xintercept= pe), color="red") +
   theme_bw()
quartz.save(file = "Figures/Case_Fatality_Rates_by_Month_of_Age.pdf", type = pdf)

######
# looking at ARs
# trial 34
par(mfrow=c(1,2))
plot(1:47, AR_y_u[1,,34], type ="l", ylim = c(0, max(AR_y_u[,,34])))
for (x in 2:12){
   lines(1:47, AR_y_u[x,,34], col = x)
}
# base case
plot(1:47, AR_y_bc[1,], type ="l", ylim = c(0, max(AR_y_u[,,34])))
for (x in 2:12){
   lines(1:47, AR_y_bc[x,], col = x)
}
legend("topright", col = 1:12, legend = 1:12, lty = 1)

# raw calendar month ARs
plot(1:47, cal_AR_y, type = "l")

par(mfrow=c(1,1))
plot(1:36, adj_func(AR_y_bc), type = "l")
lines(1:36, adj_func(AR_y_u[,,34]), type = "l", col = "red")


plot(1:36, AR_age_y_u[34,], type = "l")
lines(1:36, age_AR_y, col = "red")
lines(1:36, AR_age_y_u[185,], col = "blue")
