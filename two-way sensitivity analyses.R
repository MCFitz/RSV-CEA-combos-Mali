#####

# for two-way sensitivity analysis figure 1
# cost of adding new EPI visit vs. pVax vaccine efficacy at 10 & 14 wks.
# to calculate NHBs, inputs needed are total costs and DALYs lost

winner <- function (EPIcost, WTP, NHB1, NHB2, NHB3, NHB4, NHB5, NHB6) {
  pVax_old_tcost <- sum(pVax_older_admin * cov_pVax_o* num_infants * (0.5* EPIcost + 0.5 * cost_nd + cost_prod)) +  medcost_er_pVax_older
  llAb_pVax_old_tcost <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * cov_pVax_o * num_infants * (0.5* EPIcost + 0.5 * cost_nd + cost_prod)) + medcost_er_llAb_pVax_older
  mVax_pVax_old_tcost <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_older_admin * cov_pVax_o * num_infants * (0.5* EPIcost + 0.5 * cost_nd + cost_prod)) + medcost_er_mVax_pVax_older
  NHB_pVax_o <- (DALYS_lost_er_no - DALYS_er_pVax_older) - (pVax_old_tcost - medcost_no_u) / WTP
  NHB_llAb_pVax_o <- (DALYS_lost_er_no - DALYS_er_llAb_pVax_older) - (llAb_pVax_old_tcost - medcost_no_u) / WTP
  NHB_mVax_pVax_o <- (DALYS_lost_er_no - DALYS_er_mVax_pVax_older) - (mVax_pVax_old_tcost - medcost_no_u) / WTP
  NHBall <- array(c(NHB1, NHB2, NHB3, NHB4, NHB5, NHB6, NHB_pVax_o, NHB_llAb_pVax_o, NHB_mVax_pVax_o), dim = c(trials, length(eff_red), 9))
  winners <- apply(NHBall, MARGIN = c(1,2), FUN = which.max)
  wintakeall <- apply(winners, MARGIN = 2, FUN = getmode)
  wintakeall
}

EPI_cost <- seq(0, 8, by = 0.10)

figdata <- matrix(NA, nrow = length(EPI_cost), ncol = length(eff_red))
for (epi in 1:length(EPI_cost)) {
  ec <- EPI_cost[epi]
  figdata [epi,] <- winner(ec, WTP_5k, NHB_no_5k, NHB_l_5k, NHB_m_5k, NHB_p_5k_er, NHB_lp_5k, NHB_mp_5k)
}

# for two-way sensitivity analysis figure 2
# cost of llAb product vs. WTP

# winner_llc <- function (llcost, NHB1, NHB2, NHB3, NHB4, NHB5, NHB6) {
#   llAb_tcost <- sum(llAb_admin * coverage[1]* num_infants * (llcost + cost_nd)) +  medcost_llAb_u
#   llAb_pVax_tcost <- sum(llAb_admin * coverage[1] * num_infants * (llcost + cost_nd)) + sum(pVax_admin * cov_pVax_o * num_infants * (cost_nd + cost_prod)) + medcost_joint_llAb_pVax_u
#   llAb_pVax_o_tcost <- sum(llAb_admin * coverage[1] * num_infants * (llcost + cost_nd)) + sum(pVax_older_admin * cov_pVax_o * num_infants * (0.5* cost_EPI + 0.5 * cost_nd + cost_prod)) + medcost_joint_llAb_pVax_u_older
#   NHB_llAb <- (DALYS_lost_no_u - DALYS_lost_llAb_u) - (llAb_tcost - medcost_no_u) / WTP_sp
#   NHB_llAb_pVax <- (DALYS_lost_no_u - DALYS_lost_joint_llAb_pVax_u) - (llAb_pVax_tcost - medcost_no_u) / WTP_sp
#   NHB_llAb_pVax_o <- (DALYS_lost_no_u - DALYS_lost_joint_llAb_pVax_older_u) - (llAb_pVax_o_tcost - medcost_no_u) / WTP_sp
#   NHBall <- array(c(NHB1, NHB2, NHB3, NHB4, NHB5, NHB6, NHB_llAb, NHB_llAb_pVax, NHB_llAb_pVax_o), dim = c(trials, length(WTP_sp), 9))
#   winners <- apply(NHBall, MARGIN = c(1,2), FUN = which.max)
#   wintakeall <- apply(winners, MARGIN = 2, FUN = getmode)
#   wintakeall
# }
# 
# # llAb_cost <- seq(0, 50, by = 1)
# 
# llcost_fig <- matrix(NA, nrow = 12, ncol = length(WTP_sp))
# for (llc in 1:12) {
#   llcost_fig [llc,] <- winner_llc(llc, NHB_no, NHB_m, NHB_p, NHB_mp, NHB_p_older, NHB_mp_older)
# }
# 
# 
# par(mar = c(5.1, 4.1, 4.1, 2.1))
# par(xaxs="i", yaxs="i")
# image.plot(rotate(llcost_fig), axes = TRUE, legend.shrink = 0.5, legend.mar = 10.1,
#            xlab="Society willingness to pay", ylab="Cost of llAb product")


#####
# for two-way sensitivity analysis figure 3
# cost of llAb product vs. pVax vaccine efficacy at 10 & 14 wks.

winner_ll_er <- function (llcost, WTP, NHB1, NHB2, NHB3, NHB4, NHB5, NHB6) {
  llAb_tcost <- sum(llAb_admin * coverage[1]* num_infants * (llcost + cost_nd)) +  medcost_llAb_u
  llAb_pVax_tcost <- sum(llAb_admin * coverage[1] * num_infants * (llcost + cost_nd)) + sum(pVax_admin * cov_pVax_o * num_infants * (cost_nd + cost_prod)) + medcost_er_llAb_pVax
  llAb_pVax_o_tcost <- sum(llAb_admin * coverage[1] * num_infants * (llcost + cost_nd)) + sum(pVax_older_admin * cov_pVax_o * num_infants * (0.5* cost_EPI + 0.5 * cost_nd + cost_prod)) + medcost_er_llAb_pVax_older
  NHB_llAb <- (DALYS_lost_er_no - DALYS_lost_llAb_u) - (llAb_tcost - medcost_no_u) / WTP
  NHB_llAb_pVax <- (DALYS_lost_er_no - DALYS_er_llAb_pVax) - (llAb_pVax_tcost - medcost_no_u) / WTP
  NHB_llAb_pVax_o <- (DALYS_lost_er_no - DALYS_er_llAb_pVax_older) - (llAb_pVax_o_tcost - medcost_no_u) / WTP
  NHB_all <- array(c(NHB1, NHB_llAb, NHB2, NHB3, NHB_llAb_pVax, NHB4, NHB5, NHB_llAb_pVax_o, NHB6), dim = c(trials, length(eff_red), 9))
  winners <- apply(NHB_all, MARGIN = c(1,2), FUN = which.max)
  wintakeall <- apply(winners, MARGIN = 2, FUN = getmode)
  wintakeall
}

llAb_cost <- seq(0, 20, by = 0.25)
  
SA_ller <- matrix(NA, nrow = length(llAb_cost), ncol = length(eff_red))
for (ll in 1:length(llAb_cost)) {
  llcc <- llAb_cost[ll]
  SA_ller [ll,] <- winner_ll_er(llcc, WTP_5k, NHB_no_5k, NHB_m_5k, NHB_p_5k_er, NHB_mp_5k, NHB_p_older_5k, NHB_mp_older_5k)
}


####
# for two-way sensitivity analysis figure 4
# cost of llAb product vs. cost of pVax product per dose

pVax_cost <- seq(0, 10, by = 0.25)

winner_lp <- function (llcost, pvcost, WTP, NHB1, NHB2) {
  llAb_tcost <- sum(llAb_admin * coverage[1]* num_infants * (llcost + cost_nd)) +  medcost_llAb_u
  pVax_tcost <- sum(pVax_admin *coverage[3]* num_infants * (pvcost + cost_nd)) + medcost_pVax_u
  pVax_o_tcost <- sum(pVax_older_admin * cov_pVax_o * num_infants * (0.5* cost_EPI + 0.5 * cost_nd + pvcost)) + medcost_pVax_u_older
  llAb_pVax_tcost <- sum(llAb_admin * coverage[1] * num_infants * (llcost + cost_nd)) + sum(pVax_admin * coverage[3] * num_infants * (pvcost + cost_nd)) + medcost_joint_llAb_pVax_u
  llAb_pVax_o_tcost <- sum(llAb_admin * coverage[1] * num_infants * (llcost + cost_nd)) + sum(pVax_older_admin * cov_pVax_o * num_infants * (0.5* cost_EPI + 0.5 * cost_nd + pvcost)) + medcost_joint_llAb_pVax_u_older
  mVax_pVax_tcost <- sum(mVax_admin * coverage[2]* num_infants * cost_prod) + sum(pVax_admin * coverage[3] * num_infants * (pvcost + cost_nd)) + medcost_joint_mVax_pVax_u
  mVax_pVax_o_tcost <- sum(mVax_admin * coverage[2]* num_infants * cost_prod) + sum(pVax_older_admin * cov_pVax_o * num_infants * (0.5* cost_EPI + 0.5 * cost_nd + pvcost)) + medcost_joint_mVax_pVax_u_older
  NHB_llAb <- (DALYS_lost_no_u - DALYS_lost_llAb_u) - (llAb_tcost - medcost_no_u) / WTP
  NHB_pVax <- (DALYS_lost_no_u - DALYS_lost_pVax_u) - (pVax_tcost - medcost_no_u) / WTP
  NHB_pVax_o <- (DALYS_lost_no_u - DALYS_lost_pVax_older_u) - (pVax_o_tcost - medcost_no_u) / WTP
  NHB_llAb_pVax <- (DALYS_lost_no_u - DALYS_lost_joint_llAb_pVax_u) - (llAb_pVax_tcost - medcost_no_u) / WTP
  NHB_llAb_pVax_o <- (DALYS_lost_no_u - DALYS_lost_joint_llAb_pVax_older_u) - (llAb_pVax_o_tcost - medcost_no_u) / WTP
  NHB_mVax_pVax <- (DALYS_lost_no_u - DALYS_lost_joint_mVax_pVax_u) - (mVax_pVax_tcost - medcost_no_u) / WTP
  NHB_mVax_pVax_o <- (DALYS_lost_no_u - DALYS_lost_joint_mVax_pVax_older_u) - (mVax_pVax_o_tcost - medcost_no_u) / WTP
  NHB_all <- array(c(NHB1, NHB_llAb, NHB2, NHB_pVax, NHB_llAb_pVax, NHB_mVax_pVax, NHB_pVax_o, NHB_llAb_pVax_o, NHB_mVax_pVax_o), dim = c(trials, length(llcost), 9))
  winners <- apply(NHB_all, MARGIN = c(1,2), FUN = which.max)
  wintakeall <- apply(winners, MARGIN = 2, FUN = getmode)
  wintakeall
}


SA_llpv <- matrix(NA, nrow = length(llAb_cost), ncol = length(pVax_cost))
for (lp in 1:length(llAb_cost)) {
  for(pv in 1:length(pVax_cost)){
  llc <- llAb_cost[lp]
  pvc <- pVax_cost[pv]
  SA_llpv [lp,pv] <- winner_lp(llc, pvc, CET_Mali_GDP, NHB_no_GDP, NHB_m_GDP)
}
}


