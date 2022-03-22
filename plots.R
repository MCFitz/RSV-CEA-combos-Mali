# plots
# source("master.R")
source("figure_label_function.R")

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

# make standard vector for UMB colors
UMB1 <- c(UMBgray, UMBblue, UMBred, UMByellow, UMBforest, UMBtan,
          UMBplum, UMBslate, UMBsea)

# Cross color palette
Cross_vec <- met.brewer("Cross", n = 8, "discrete")

# color palette for our plots
# col_vec <- c("gray80", Cross_vec[7], Cross_vec[2], Cross_vec[5], Cross_vec[6],
#              Cross_vec[4], Cross_vec[1], Cross_vec[8], Cross_vec[3])

col_vec <- c(UMBred, UMBblue, UMBplum, UMByellow, UMBforest,
             Cross_vec[4], Cross_vec[1], Cross_vec[8], Cross_vec[3])

# For figures on slides, make font larger
# cex.lab = 1.5, cex.axis = 1.5

# Figure 1
# CE plane, Plot cost per DALYs averted 
quartz("CE plane", 8, 8)
par(xaxs="i", yaxs="i")
plot(DALYS_lost_no - DALYS_lost_llAb, totalcost_llAb- totalcost_no, col = col_vec[2], bty = "n", pch = 19, cex = 1.5, xlim = c(0,2250), ylim = c(0,2500000), xlab = "DALYs averted", ylab = 
       "Incremental cost compared to status quo (USD)")
points(DALYS_lost_no - DALYS_lost_mVax, totalcost_mVax- totalcost_no, col = col_vec[3], pch = 19, cex =1.5)
points(DALYS_lost_no - DALYS_lost_pVax, totalcost_pVax- totalcost_no, col = col_vec[4], pch = 19, cex = 1.5)
points(DALYS_lost_no - DALYS_lost_joint_llAb_pVax, totalcost_joint_llAb_pVax - totalcost_no, col = col_vec[5], pch = 19, cex = 1.5)
points(DALYS_lost_no - DALYS_lost_joint_mVax_pVax, totalcost_joint_mVax_pVax - totalcost_no, col = col_vec[6], pch = 19, cex = 1.5)
points(DALYS_lost_no - DALYS_lost_pVax_older, totalcost_pVax_older - totalcost_no, col = col_vec[7], pch = 19,  cex = 1.5)
points(DALYS_lost_no - DALYS_lost_joint_llAb_pVax_older, totalcost_joint_llAb_pVax_older - totalcost_no, col = col_vec[8], pch = 19, cex = 1.5)
points(DALYS_lost_no - DALYS_lost_joint_mVax_pVax_older, totalcost_joint_mVax_pVax_older - totalcost_no, col = col_vec[9], pch = 19, cex = 1.5)

segments(0, 0, x1 = DALYS_lost_no - DALYS_lost_llAb, y1 = totalcost_llAb- totalcost_no,
         col = par("fg"), lty = 2, lwd = 2)
segments(DALYS_lost_no - DALYS_lost_llAb, totalcost_llAb- totalcost_no, x1 = DALYS_lost_no - DALYS_lost_joint_llAb_pVax, y1 = totalcost_joint_llAb_pVax- totalcost_no,
         col = par("fg"), lty = 2, lwd = 2)
legend("bottomright", legend = c("mAb", "mVax", "pVax 10 & 14 wks", "mAb + pVax 10 & 14 wks", "mVax + pVax 10 & 14 wks", "pVax 6 & 7 mos", "mAb + pVax 6 & 7 mos", "mVax + pVax 6 & 7 mos"),
       bty = "n", pch = 19, col = col_vec[2:9])
quartz.save(file = "Figures/CE_plane.pdf", type = "pdf")

# plot cost per DALYs averted with uncertainty
par(mfrow=c(1,1))
plot(DALYS_lost_no_u - DALYS_lost_llAb_u, totalcost_llAb_u- totalcost_no_u, col = col_vec[2], pch = 19, xlim = c(-1000,6000), ylim = c(0,4000000), xlab = "DALYs averted", ylab =
       "Added cost (USD)")
points(DALYS_lost_no_u - DALYS_lost_mVax_u, totalcost_mVax_u- totalcost_no_u, col = col_vec[3], pch = 15)
points(DALYS_lost_no_u - DALYS_lost_pVax_u, totalcost_pVax_u- totalcost_no_u, col = col_vec[4], pch = 17)
points(DALYS_lost_no_u - DALYS_lost_joint_llAb_pVax_u, totalcost_joint_llAb_pVax_u - totalcost_no_u, col = col_vec[5], pch = 18)
points(DALYS_lost_no_u - DALYS_lost_joint_mVax_pVax_u, totalcost_joint_mVax_pVax_u - totalcost_no_u, col = col_vec[6], pch = 10)
points(DALYS_lost_no - DALYS_lost_llAb, totalcost_llAb- totalcost_no, col = "blue", pch = 19)
points(DALYS_lost_no - DALYS_lost_mVax, totalcost_mVax- totalcost_no, col = "blue", pch = 15)
points(DALYS_lost_no - DALYS_lost_pVax, totalcost_pVax- totalcost_no, col = "blue", pch = 17)
points(DALYS_lost_no - DALYS_lost_joint_llAb_pVax, totalcost_joint_llAb_pVax - totalcost_no, col = "blue", pch = 18)
points(DALYS_lost_no - DALYS_lost_joint_mVax_pVax, totalcost_joint_mVax_pVax - totalcost_no, col = "blue", pch = 10)
legend("bottomright", legend = c("llAb", "mVax", "pVax", "llAb + pVax", "mVax + pVax"),
       pch = c(19,15,17,18,10), col = col_vec[2:6])


# plot probability optimal across product price
quartz("Two panel plot:pOptimal across product price and societal WTP", 12, 8)
par(mar = c(9, 4.1, 4.1, 2.1))
par(mfrow =c(1,2))
par(xaxs="i", yaxs="i")
plot(cprod, pO_pVax_pspan, ylim = c(0, 1), xlim = c(0, max(cprod)), bty = "l",
     type = "l", lwd = 3, col = col_vec[4],
     xlab = "Product price (USD)",
     ylab = "Probability optimal")
lines(cprod, pO_no_pspan, col = col_vec[1], lty = 1, lwd = 3)
lines(cprod, pO_llAb_pspan, col = col_vec[2], lty = 1, lwd = 3)
lines(cprod, pO_mVax_pspan, col = col_vec[3], lty = 1, lwd = 3)
lines(cprod, pO_llAb_pVax_pspan, col = col_vec[5], lty = 1, lwd = 3)
lines(cprod, pO_mVax_pVax_pspan, col = col_vec[6], lty = 1, lwd = 3)
lines(cprod, pO_pVax_older_pspan, col = col_vec[7], lty = 1, lwd = 3)
lines(cprod, pO_llAb_pVax_older_pspan, col = col_vec[8], lty = 1, lwd = 3)
lines(cprod, pO_mVax_pVax_older_pspan, col = col_vec[9], lty = 1, lwd = 3)
abline(v = cost_prod, col = UMBgray, lty = 3, lwd = 2)
abline(v = 1.50, col = UMBgray, lty = 3, lwd = 2)
abline(v= MR_cost, col = UMBgray, lty = 3, lwd = 2)
text(cost_prod, 0.95, labels = "Penta", srt = 45, cex = 0.80)
text(1.50, 0.95, labels = "TCV", srt = 45, cex = 0.80)
text(MR_cost, 0.95, labels = "MR", srt = 45, cex = 0.80)
fig_label("A", "figure", "topleft")
# legend("right", ncol = 1, legend = c("status quo","mAb", "mVax", "pVax 10 & 14 wks", "mAb + pVax 10 & 14 wks", "mVax + pVax 10 & 14 wks", "pVax 6 & 7 mos", "mAb + pVax 6 & 7 mos", "mVax + pVax 6 & 7 mos"),
#        lty = 1, lwd = 3, bty = "n", col = col_vec)

# plot probability optimal across WTP values, societal perspective
plot(WTP_sp, pO_pVax, ylim = c(0, 1), xlim = c(0,7500), bty = "l",
     type = "l", lwd = 3, col = col_vec[4],
     xlab = "Society willingness to pay (USD)",
     ylab = "Probability optimal")
lines(WTP_sp, pO_no, col = col_vec[1], lty = 1, lwd = 3)
lines(WTP_sp, pO_llAb, col = col_vec[2], lty = 1, lwd = 3)
lines(WTP_sp, pO_mVax, col = col_vec[3], lty = 1, lwd = 3)
lines(WTP_sp, pO_llAb_pVax, col = col_vec[5], lty = 1, lwd = 3)
lines(WTP_sp, pO_mVax_pVax, col = col_vec[6], lty = 1, lwd = 3)
lines(WTP_sp, pO_pVax_older, col = col_vec[7], lty = 1, lwd = 3)
lines(WTP_sp, pO_llAb_pVax_older, col = col_vec[8], lty = 1, lwd = 3)
lines(WTP_sp, pO_mVax_pVax_older, col = col_vec[9], lty = 1, lwd = 3)
abline(v = CET_Mali_GDP, col = UMBgray, lty = 3, lwd = 2)
abline(v = 3*CET_Mali_GDP, col = UMBgray, lty = 3, lwd = 2)
text(CET_Mali_GDP, 0.92, labels = "1xGDP", srt = 45, cex = 0.80)
text(3*CET_Mali_GDP, 0.92, labels = "3xGDP", srt = 45, cex = 0.80)
fig_label("B", "figure", "topleft")
# legend("right", ncol = 1, legend = c("status quo","mAb", "mVax", "pVax 10 & 14 wks", "mAb + pVax 10 & 14 wks", "mVax + pVax 10 & 14 wks", "pVax 6 & 7 mos", "mAb + pVax 6 & 7 mos", "mVax + pVax 6 & 7 mos"),
#        lty = 1, lwd = 3, bty = "n", col = col_vec)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
par(xpd=TRUE)
legend("bottom", ncol = 3, legend = c("status quo","mAb", "mVax", "pVax 10 & 14 wks", "mAb + pVax 10 & 14 wks", "mVax + pVax 10 & 14 wks", "pVax 6 & 7 mos", "mAb + pVax 6 & 7 mos", "mVax + pVax 6 & 7 mos"),
       lty = 1, lwd = 3, bty = "n", col = col_vec, xpd = TRUE)
quartz.save(file = "Figures/2panel_pOptimal_by_prodcost_and_WTP.pdf", type = "pdf")

#####
# government perspective probability optimal by WTP
quartz("Donor and Gov perspective, pOptimal by WTP", 12, 8)
par(mar = c(9, 4.1, 4.1, 2.1))
par(mfrow =c(1,2))
par(xaxs="i", yaxs="i")
plot(WTP_sp, gov_pO_pVax, ylim = c(0, 1), xlim = c(0,5000), bty = "l",
     type = "l", lwd = 3, col = col_vec[4],
     xlab = "Government willingness to pay (USD)",
     ylab = "Probability optimal")
lines(WTP_sp, gov_pO_no, col = col_vec[1], lty = 1, lwd = 3)
lines(WTP_sp, gov_pO_llAb, col = col_vec[2], lty = 1, lwd = 3)
lines(WTP_sp, gov_pO_mVax, col = col_vec[3], lty = 1, lwd = 3)
lines(WTP_sp, gov_pO_llAb_pVax, col = col_vec[5], lty = 1, lwd = 3)
lines(WTP_sp, gov_pO_mVax_pVax, col = col_vec[6], lty = 1, lwd = 3)
lines(WTP_sp, gov_pO_pVax_older, col = col_vec[7], lty = 1, lwd = 3)
lines(WTP_sp, gov_pO_llAb_pVax_older, col = col_vec[8], lty = 1, lwd = 3)
lines(WTP_sp, gov_pO_mVax_pVax_older, col = col_vec[9], lty = 1, lwd = 3)
abline(v = CET_Mali_GDP, col = UMBgray, lty = 3, lwd = 2)
text(CET_Mali_GDP, 0.92, labels = "1xGDP", srt = 45, cex = 0.80)
# abline(v = 3*CET_Mali_GDP, col = UMBgray, lty = 3, lwd = 2)
# text(3*CET_Mali_GDP, 0.92, labels = "3xGDP", srt = 45, cex = 0.80)
fig_label("A", "figure", "topleft")

# donor perspective probability optimal by WTP
plot(WTP_sp, dnr_pO_pVax, ylim = c(0, 1), xlim = c(0,5000), bty = "l",
     type = "l", lwd = 3, col = col_vec[4],
     xlab = "Donor willingness to pay (USD)",
     ylab = "Probability optimal")
lines(WTP_sp, dnr_pO_no, col = col_vec[1], lty = 1, lwd = 3)
lines(WTP_sp, dnr_pO_llAb, col = col_vec[2], lty = 1, lwd = 3)
lines(WTP_sp, dnr_pO_mVax, col = col_vec[3], lty = 1, lwd = 3)
lines(WTP_sp, dnr_pO_llAb_pVax, col = col_vec[5], lty = 1, lwd = 3)
lines(WTP_sp, dnr_pO_mVax_pVax, col = col_vec[6], lty = 1, lwd = 3)
lines(WTP_sp, dnr_pO_pVax_older, col = col_vec[7], lty = 1, lwd = 3)
lines(WTP_sp, dnr_pO_llAb_pVax_older, col = col_vec[8], lty = 1, lwd = 3)
lines(WTP_sp, dnr_pO_mVax_pVax_older, col = col_vec[9], lty = 1, lwd = 3)
abline(v = CET_Mali_GDP, col = UMBgray, lty = 3, lwd = 2)
abline(v = 3*CET_Mali_GDP, col = UMBgray, lty = 3, lwd = 2)
text(CET_Mali_GDP, 0.92, labels = "1xGDP", srt = 45, cex = 0.80)
text(3*CET_Mali_GDP, 0.92, labels = "3xGDP", srt = 45, cex = 0.80)
fig_label("B", "figure", "topleft")
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
par(xpd=TRUE)
legend("bottom", ncol = 3, legend = c("status quo","mAb", "mVax", "pVax 10 & 14 wks", "mAb + pVax 10 & 14 wks", "mVax + pVax 10 & 14 wks", "pVax 6 & 7 mos", "mAb + pVax 6 & 7 mos", "mVax + pVax 6 & 7 mos"),
       lty = 1, lwd = 3, bty = "n", col = col_vec, xpd = TRUE)

quartz.save(file = "Figures/donr_and_gov_pOptimal_by_WTP.pdf", type = "pdf")
###############




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
# legend(2800, 0.98, ncol = 2, cex = 1, legend = c("status quo","mAb", "mVax", "pVax 10 & 14 wks", "mAb + pVax 10 & 14 wks", "mVax + pVax 10 & 14 wks", "pVax 6 & 7 mos", "mAb + pVax 6 & 7 mos", "mVax + pVax 6 & 7 mos"),
#        lty = 1, lwd = 3, bty = "n", col = c(UMBred, UMBblue, UMBforest, UMBplum, UMByellow, UMBcharcoal, UMBsea, UMBtan, UMBslate))
# quartz.save(file = "Figures/SuppFig3.pdf", type = "pdf")
##

# plot probability optimal across increasing pVax efficacy
# when efficacy reduction is based on immune immaturity
quartz("efficacy reduction", 14, 8)
par(mfrow = c(1,2))
par(xaxs="i", yaxs="i")
plot(eff_red*100, pO_pVax_ser, ylim = c(0, 1), xlim = c(0,100), bty = "l",
     type = "l", col = col_vec[4], lty = 1, lwd = 3,
     xlab = "Pediatric vaccine efficacy when administered at 10/14 weeks (%)",
     ylab = "Probability optimal")
lines(eff_red*100, pO_no_ser, col = col_vec[1], lty = 1, lwd = 3)
lines(eff_red*100, pO_llAb_ser, col = col_vec[2], lty = 1, lwd = 3)
lines(eff_red*100, pO_mVax_ser, col = col_vec[3], lty = 1, lwd = 3)
lines(eff_red*100, pO_llAb_pVax_ser, col = col_vec[5], lty = 1, lwd = 3)
lines(eff_red*100, pO_mVax_pVax_ser, col = col_vec[6], lty = 1, lwd = 3)
lines(eff_red*100, pO_pVax_older_ser, col = col_vec[7], lty = 1, lwd = 3)
lines(eff_red*100, pO_llAb_pVax_older_ser, col = col_vec[8], lty = 1, lwd = 3)
lines(eff_red*100, pO_mVax_pVax_older_ser, col = col_vec[9], lty = 1, lwd = 3)
abline(v = 70, col = UMBgray, lty = 3, lwd = 2)
text(70, 0.92, labels = "70% efficacy", srt = 45, cex = 0.80)
fig_label("A", "figure", "topleft")
legend("topleft", ncol =1, legend = c("status quo","mAb", "mVax", "pVax 10 & 14 wks", "mAb + pVax 10 & 14 wks", "mVax + pVax 10 & 14 wks", "pVax 6 & 7 mos", "mAb + pVax 6 & 7 mos", "mVax + pVax 6 & 7 mos"),
       lty = 1, lwd = 3, bty = "n", col = col_vec)

# plot probability optimal across increasing pVax efficacy 
# when efficacy reduction is based on interference
plot(eff_red*100, pO_pVax_er, ylim = c(0, 1), xlim = c(0,100), bty = "l",
     type = "l", lty = 4, lwd = 3, col = col_vec[4],
     xlab = "Pediatric vaccine efficacy when administered at 10/14 weeks \n as part of a combination strategy (%)",
     ylab = "Probability optimal")
lines(eff_red*100, pO_no_er, col = col_vec[1], lty = 4, lwd = 3)
lines(eff_red*100, pO_llAb_er, col = col_vec[2], lty = 4, lwd = 3)
lines(eff_red*100, pO_mVax_er, col = col_vec[3], lty = 4, lwd = 3)
lines(eff_red*100, pO_llAb_pVax_er, col = col_vec[5], lty = 4, lwd = 3)
lines(eff_red*100, pO_mVax_pVax_er, col = col_vec[6], lty = 4, lwd =3)
lines(eff_red*100, pO_pVax_older_er, col = col_vec[7], lty = 4, lwd = 3)
lines(eff_red*100, pO_llAb_pVax_older_er, col = col_vec[8], lty = 4, lwd = 3)
lines(eff_red*100, pO_mVax_pVax_older_er, col = col_vec[9], lty = 4, lwd =3)
abline(v = 70, col = UMBgray, lty = 3, lwd = 2)
text(70, 0.92, labels = "70% efficacy", srt = 45, cex = 0.80)
fig_label("B", "figure", "topleft")
legend("topleft", ncol =1, legend = c("status quo","mAb", "mVax", "pVax 10 & 14 wks", "mAb + pVax 10 & 14 wks", "mVax + pVax 10 & 14 wks", "pVax 6 & 7 mos", "mAb + pVax 6 & 7 mos", "mVax + pVax 6 & 7 mos"),
       lty = 4, lwd = 3, bty = "n", col = col_vec)
quartz.save(file = "Figures/efficacy_reduction.pdf", type = "pdf")


# # price of llAb product vs. efficacy of pVax at 10 & 14 wks.
# quartz("cost of llAb vs pVax eff", 8, 8)
# col_clpe <- col_vec[1:2]
# ggplot(data = SA_clpe_df, aes(x = llAb_price, y = pVax_efficacy)) + 
#   geom_tile(aes(fill = strategy, alpha = probwin)) +
#   scale_alpha_continuous(limits = c(0.25,1), breaks = c(0.25,0.5,0.75,1)) +
#   scale_fill_manual(values = c(col_clpe, "white")) +
#   scale_x_continuous(limits = c(llAb_cost[1]-0.01, max(llAb_cost)+0.01), expand = c(-0.01, -0.005)) +
#   scale_y_continuous(limits = 100*c(eff_red[1]-0.005, max(eff_red)+0.005), expand = c(-0.01, -0.005)) +
#   xlab("Price of long-acting antibody (USD)") +
#   ylab("Efficacy of pediatric vaccine (%)") +
#   theme_cowplot(12)
# quartz.save(file = "Figures/costllAb_effpVax", type = "pdf")

# price llAb product vs price pVax product per dose
SA_llpv_df$probwin2 <- ifelse(SA_llpv_df$probwin<0.5, 0.5, SA_llpv_df$probwin)
quartz("4panel two-way analyses", 10, 10)
col_llpv <- c(col_vec[1], col_vec[2], col_vec[4], col_vec[5])
p3 <- ggplot(data = SA_llpv_df, aes(x = llAb_price, y = pVax_price)) +
  geom_tile(aes(fill = strategy, alpha = probwin2), show.legend = FALSE) +
  scale_alpha_continuous(limits = c(0.5,1), breaks = c(0.5, 0.6, 0.7, 0.8, 0.9)) +
  scale_fill_manual(values = c(col_llpv, "white")) +
  scale_x_continuous(limits = c(llAb_cost[1]-0.01, max(llAb_cost+0.01)), expand = c(-0.01, -0.01)) +
  scale_y_continuous(limits = c(pVax_cost[1]-0.01, max(pVax_cost)+0.01), expand = c(-0.01, -0.01)) +
  geom_point(aes(x = cost_prod, y = cost_prod), shape = 10, size = 3) +
  coord_fixed(ratio = 1) +
  xlab("Price of long-acting mAb (USD)") +
  ylab("Price of pediatric vaccine at 10/14 weeks (USD)") +
  theme_cowplot(12)

# price llAb product per dose vs efficacy of llAb
SA_ll_ce_df$probwin2 <- ifelse(SA_ll_ce_df$probwin<0.5, 0.5, SA_ll_ce_df$probwin)
col_ll_ce <- col_vec[1:2]
p1 <- ggplot(data = SA_ll_ce_df, aes(x = llAb_price, y = llAb_efficacy)) + 
  geom_tile(aes(fill = strategy, alpha = probwin2), show.legend = FALSE) +
  scale_alpha_continuous(limits = c(0.5,1), breaks = c(0.5, 0.6, 0.7, 0.8, 0.9)) +
  scale_fill_manual(values = c(col_ll_ce, "white")) +
  scale_x_continuous(limits = c(llAb_cost[1]-0.01, max(llAb_cost)+0.01), expand = c(-0.01, -0.005)) +
  scale_y_continuous(limits = 100*c(eff_red[1]-0.005, max(eff_red)+0.005), expand = c(-0.01, -0.005)) +
  geom_point(aes(x = cost_prod, y = efficacy[1]*100), shape = 10, size = 3) +
  coord_fixed(ratio = 2.50/100) +
  xlab("Price of long-acting mAb (USD)") +
  ylab("Efficacy of long-acting mAb (%)") +
  theme_cowplot(12)

# price pVax product per dose vs efficacy of pVax (10 & 14 wk.)
SA_pe_df$probwin2 <- ifelse(SA_pe_df$probwin<0.5, 0.5, SA_pe_df$probwin)
col_pe <- c(col_vec[1], col_vec[2], col_vec[4], col_vec[5])
p4 <- ggplot(data = SA_pe_df, aes(x = pVax_efficacy, y = pVax_price)) + 
  geom_tile(aes(fill = strategy, alpha = probwin2), show.legend = FALSE) +
  scale_alpha_continuous(limits = c(0.5,1), breaks = c(0.5, 0.6, 0.7, 0.8, 0.9)) +
  scale_fill_manual(values = c(col_pe, "white")) +
  scale_x_continuous(limits = 100*c(eff_red[1]-0.005, max(eff_red)+0.005), expand = c(-0.01, -0.005)) +
  scale_y_continuous(limits = c(pVax_cost[1]-0.01, max(pVax_cost)+0.01), expand = c(-0.01, -0.005)) +
  geom_point(aes(x = efficacy[1]*100, y = cost_prod), shape = 10, size = 3) +
  coord_fixed(ratio = 100/2.50) +
  xlab("Efficacy of pediatric vaccine at 10/14 weeks (%)") +
  ylab("Price of pediatric vaccine at 10/14 weeks (USD)") +
  theme_cowplot(12)

# efficacy of llAb vs efficacy of pVax (10 & 14 wk.)
SA_eff_df$probwin2 <- ifelse(SA_eff_df$probwin<0.5, 0.5, SA_eff_df$probwin)
col_eff <- c(col_vec[1], col_vec[2])
p2 <- ggplot(data = SA_eff_df, aes(x = pVax_efficacy, y = llAb_efficacy)) + 
  geom_tile(aes(fill = strategy, alpha = probwin2), show.legend = FALSE) +
  scale_alpha_continuous(limits = c(0.5,1), breaks = c(0.5, 0.6, 0.7, 0.8, 0.9)) +
  scale_fill_manual(values = c(col_eff, "white")) +
  scale_x_continuous(limits = 100*c(eff_red[1]-0.005, max(eff_red)+0.005), expand = c(-0.01, -0.005)) +
  scale_y_continuous(limits = 100*c(eff_red[1]-0.005, max(eff_red)+0.005), expand = c(-0.01, -0.005)) +
  geom_point(aes(x = efficacy[1]* 100, y = efficacy[1]*100), shape = 10, size = 3) +
  coord_fixed(ratio = 1) +
  xlab("Efficacy of pediatric vaccine at 10/14 weeks (%)") +
  ylab("Efficacy of long-acting mAb (%)") +
  theme_cowplot(12)

plot_grid(p1, p2, p3, p4, labels = "AUTO")

quartz.save(file = "Figures/4panel_2way_analyses", type = "pdf")

# Legend for panel plot
SA_colors <- c(col_vec[1], col_vec[2], col_vec[4], col_vec[5])
SA_alpha <- seq(0.5, 0.9, by = 0.10)


# Legend on its own
quartz("color legend", 8,5)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend =c("status quo","long-acting mAb",
                            "maternal vaccine", "pediatric vaccine 10 & 14 wks",
                            "long-acting mAb + pediatric vaccine 10 & 14 wks",
                            "maternal vaccine + pediatric vaccine 10 & 14 wks",
                            "pediatric vaccine 6 & 7 mos",
                            "long acting mAb + pediatric vaccine 6 & 7 mos",
                            "maternal vaccine + pediatric vaccine 6 & 7 mos"),
       pch=15, pt.cex=3, cex = 1.5, bty='n',
       col = col_vec)
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
                                     rep("mAb", length(months)),
                                     rep("mVax", length(months)),
                                     rep("pVax 10 & 14 wks", length(months)),
                                     rep("mAb + pVax 10 & 14 wks", length(months)),
                                     rep("pVax 6 & 7 mos", length(months))), nm),
                    metric = c(rep("RSV cases", length(months) * ni),
                                                     rep("LRTI episodes", length(months) * ni),
                                                     rep("Hospitalizations", length(months) * ni),
                                                     rep("Deaths", length(months) * ni)),
                    value = c(rep(cases_no_age, ni),
                              LRTI_no_age, LRTI_llAb_age, LRTI_mVax_age, LRTI_pVax_age, LRTI_joint_llAb_pVax_age, LRTI_pVax_older_age,
                              inpat_no_age, inpat_llAb_age, inpat_mVax_age, inpat_pVax_age, inpat_joint_llAb_pVax_age, inpat_pVax_older_age,
                              deaths_no_age, deaths_llAb_age, deaths_mVax_age, deaths_pVax_age, deaths_joint_llAb_pVax_age, deaths_pVax_older_age))

HO_df$metric <- factor(HO_df$metric, levels = c("RSV cases", "LRTI episodes", "Hospitalizations", "Deaths"))
HO_df$intervention <- factor(HO_df$intervention, levels = c("no intervention", "pVax 6 & 7 mos", "mVax", "pVax 10 & 14 wks", "mAb", "mAb + pVax 10 & 14 wks"))

temp_HO <- HO_df %>% mutate(bin = floor((age-1) / 6) + 1) %>% 
   group_by(bin, intervention, metric) %>% 
   summarise(tot = sum(value))

###
# brewer.pal(ni, "Set2")
# met.brewer("Cross", ni, "discrete"))

quartz("Health Outcomes Barplot", 12, 8)
ggplot(temp_HO, aes(x = bin, y = tot, fill = intervention)) +
   geom_bar(position = 'dodge', stat = 'identity') +
   scale_x_continuous(breaks = seq(1, 6, by = 1), labels = c("<6", "6-<12", "12-<18", "18-<24", "24-<30", "30-<36")) +
   xlab(NULL) +
   facet_wrap(~metric, scales = "free") +
   scale_fill_manual(values = c(col_vec[1], col_vec[7], col_vec[3], col_vec[4], col_vec[2], col_vec[5])) +
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
quartz.save(file ="Figures/Attack_Rates_by_Month_of_Age_Histograms.pdf", type = "pdf")

hosp_age_df <- data.frame(month = rep(c("0<6", "6<12", "12<24", "24<36"), each = trials),
                        value = c(p_hosp_u[,1], p_hosp_u[,7], p_hosp_u[,13], p_hosp_u[,25]),
                        pe = rep(c(p_hosp_new[1], p_hosp_new[7], p_hosp_new[13], p_hosp_new[25]), each = trials))
hosp_age_df$month <- factor(hosp_age_df$month, levels=c("0<6", "6<12", "12<24", "24<36"))
quartz("Hospitalization Rates by Month of Age Histograms", 12, 8)
ggplot(hosp_age_df, aes(value)) + geom_histogram(color = "black", fill = "grey", bins = 15) +
  facet_wrap(~month, scales = "free") +
  geom_vline(hosp_age_df, mapping = aes(xintercept= pe), color="red") +
  theme_bw()
quartz.save(file ="Figures/Hospitalization_Rates_by_Month_of_Age_Histograms.pdf", type = "pdf")

CFR_age_df <- data.frame(month = rep(1:36, each = trials),
                        value = c(CFR_by_age_u[,1], CFR_by_age_u[,2], CFR_by_age_u[,3], CFR_by_age_u[,4], CFR_by_age_u[,5], CFR_by_age_u[,6],
                                  CFR_by_age_u[,7], CFR_by_age_u[,8], CFR_by_age_u[,9], CFR_by_age_u[,10], CFR_by_age_u[,11], CFR_by_age_u[,12],
                                  CFR_by_age_u[,13], CFR_by_age_u[,14], CFR_by_age_u[,15], CFR_by_age_u[,16], CFR_by_age_u[,17], CFR_by_age_u[,18],
                                  CFR_by_age_u[,19], CFR_by_age_u[,20], CFR_by_age_u[,21], CFR_by_age_u[,22], CFR_by_age_u[,23], CFR_by_age_u[,24],
                                  CFR_by_age_u[,25], CFR_by_age_u[,26], CFR_by_age_u[,27], CFR_by_age_u[,28], CFR_by_age_u[,29], CFR_by_age_u[,30],
                                  CFR_by_age_u[,31], CFR_by_age_u[,32], CFR_by_age_u[,33], CFR_by_age_u[,34], CFR_by_age_u[,35], CFR_by_age_u[,36]),
                        pe = rep(CFR_by_age, each = trials))
quartz("Case Fatality Rate by Month of Age", 12, 8)
ggplot(CFR_age_df, aes(value)) + geom_histogram(color = "black", fill = "grey", bins = 15) +
   facet_wrap(~month) +
   geom_vline(CFR_age_df, mapping = aes(xintercept= pe), color="red") +
   theme_bw()
quartz.save(file = "Figures/Case_Fatality_Rates_by_Month_of_Age.pdf", type = "pdf")

CFR_LMIC_vec <- apply(CFR_by_age_u[,1:6], 1, mean)
CFR_barplot_df <- data.frame(Source = c("LMICs", "Mali"), CFR = c(mean(CFR_by_age[1:6]), CFR_inpatient),
                              lower = c(CI_func(CFR_LMIC_vec)[1], CI_func(CFR_inpatient_u)[1]),
                              upper = c(CI_func(CFR_LMIC_vec)[2], CI_func(CFR_inpatient_u)[2]))

CFR_by_source <- ggplot(data = CFR_barplot_df, aes(x = Source, y = CFR, fill = Source)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin= lower, ymax= upper), width=.2,
                position=position_dodge(.9)) +
  xlab("Data Source") +
  ylab("CFR") +
  scale_fill_grey() +
  theme_classic()
quartz("CFR by Source", 4,4)
CFR_by_source +
  theme(legend.title = element_blank())
quartz.save(file = "Figures/CFR_by_source.pdf", type = "pdf")
######

