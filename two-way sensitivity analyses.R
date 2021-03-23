#####
# for two-way sensitivity analysis figure
# cost of llAb product vs. WTP

winner_llc <- function (llcost, NHB1, NHB2, NHB3, NHB4, NHB5, NHB6) {
  llAb_tcost <- sum(llAb_admin * coverage[1]* num_infants * (llcost + cost_nd)) +  medcost_llAb_u
  llAb_pVax_tcost <- sum(llAb_admin * coverage[1] * num_infants * (llcost + cost_nd)) + sum(pVax_older_admin * cov_pVax_o * num_infants * (cost_nd + cost_prod)) + medcost_joint_llAb_pVax_u
  llAb_pVax_o_tcost <- sum(llAb_admin * coverage[1] * num_infants * (llcost + cost_nd)) + sum(pVax_older_admin * cov_pVax_o * num_infants * (0.5* cost_EPI + 0.5 * cost_nd + cost_prod)) + medcost_joint_llAb_pVax_u_older
  NHB_llAb <- (DALYS_lost_no_u - DALYS_lost_llAb_u) - (llAb_tcost - medcost_no_u) / WTP_sp
  NHB_llAb_pVax <- (DALYS_lost_no_u - DALYS_lost_joint_llAb_pVax_u) - (llAb_pVax_tcost - medcost_no_u) / WTP_sp
  NHB_llAb_pVax_o <- (DALYS_lost_no_u - DALYS_lost_joint_llAb_pVax_older_u) - (llAb_pVax_o_tcost - medcost_no_u) / WTP_sp
  NHBall <- array(c(NHB1, NHB2, NHB3, NHB4, NHB5, NHB6, NHB_llAb, NHB_llAb_pVax, NHB_llAb_pVax_o), dim = c(trials, length(WTP_sp), 9))
  winners <- apply(NHBall, MARGIN = c(1,2), FUN = which.max)
  wintakeall <- apply(winners, MARGIN = 2, FUN = getmode)
  wintakeall
}

# llAb_cost <- seq(0, 50, by = 1)

llcost_fig <- matrix(NA, nrow = 12, ncol = length(WTP_sp))
for (llc in 1:12) {
  lcost_fig [llc,] <- winner_llc(llc, NHB_no, NHB_m, NHB_p, NHB_mp, NHB_p_older, NHB_mp_older)
}

# rotate so x-axis = efficacy in pVax at younger ages from 0 to 100 percent
# and the y-axis = cost of creating a new EPI visit for older infants receiving pVax
# col = c(UMBred, UMByellow, UMBtan)

par(mar = c(5.1, 4.1, 4.1, 2.1))
par(xaxs="i", yaxs="i")
rotate <- function(x) t(apply(x, 2, rev))
image.plot(rotate(llcost_fig), axes = TRUE, legend.shrink = 0.5, legend.mar = 10.1,
           xlab="Society willingness to pay", ylab="Cost of llAb product")

# legend = c("status quo", "llAb", "mVax", "pVax 10 & 14 wks", "llAb + pVax 10 & 14 wks", "mVax + pVax 10 & 14 wks",
#            "pVax 8 & 9 mos", "llAb + pVax 8 & 9 mos", "mVax + pVax 8 & 9 mos")




#####
# for two-way sensitivity analysis figure
# cost of llAb product vs. pVax vaccine efficacy at 10 & 14 wks.

winner_ll_er <- function (llcost, WTP, NHB1, NHB2, NHB3, NHB4, NHB5, NHB6) {
  llAb_tcost <- sum(llAb_admin * coverage[1]* num_infants * (llcost + cost_nd)) +  medcost_llAb_u
  llAb_pVax_tcost <- sum(llAb_admin * coverage[1] * num_infants * (llcost + cost_nd)) + sum(pVax_older_admin * cov_pVax_o * num_infants * (cost_nd + cost_prod)) + medcost_er_llAb_pVax
  llAb_pVax_o_tcost <- sum(llAb_admin * coverage[1] * num_infants * (llcost + cost_nd)) + sum(pVax_older_admin * cov_pVax_o * num_infants * (0.5* cost_EPI + 0.5 * cost_nd + cost_prod)) + medcost_er_llAb_pVax_older
  NHB_llAb <- (DALYS_lost_er_no - DALYS_lost_llAb_u) - (llAb_tcost - medcost_no_u) / WTP
  NHB_llAb_pVax <- (DALYS_lost_er_no - DALYS_er_llAb_pVax) - (llAb_pVax_tcost - medcost_no_u) / WTP
  NHB_llAb_pVax_o <- (DALYS_lost_er_no - DALYS_er_llAb_pVax_older) - (llAb_pVax_o_tcost - medcost_no_u) / WTP
  NHBall <- array(c(NHB1, NHB2, NHB3, NHB4, NHB5, NHB6, NHB_llAb, NHB_llAb_pVax, NHB_llAb_pVax_o), dim = c(trials, length(eff_red), 9))
  winners <- apply(NHBall, MARGIN = c(1,2), FUN = which.max)
  wintakeall <- apply(winners, MARGIN = 2, FUN = getmode)
  wintakeall
}

# llAb_cost <- seq(0,50, by = 1)
  
figdata_ller <- matrix(NA, nrow = 50, ncol = length(eff_red))
for (ll in 1:50) {
  figdata_ller [ll,] <- winner_ll_er(ll, WTP_5k, NHB_no_5k, NHB_m_5k, NHB_p_5k_er, NHB_mp_5k, NHB_p_older_5k, NHB_mp_older_5k)
}

# rotate so x-axis = efficacy in pVax at younger ages from 0 to 100 percent
# and the y-axis = cost of llAb product
# col = c(UMBred, UMByellow, UMBtan)

par(mar = c(5.1, 4.1, 4.1, 2.1))
par(xaxs="i", yaxs="i")
rotate <- function(x) t(apply(x, 2, rev))
image.plot(rotate(figdata_ller), axes = TRUE, legend.shrink = 0.5, legend.mar = 10.1,
           xlab="Efficacy in pediatric vaccine administered at 10 & 14 weeks", ylab="Cost of llAb product")










