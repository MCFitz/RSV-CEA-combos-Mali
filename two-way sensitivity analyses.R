#####

# for two-way sensitivity analysis figure 1
# cost of adding new EPI visit vs. pVax vaccine efficacy at 10 & 14 wks.
# to calculate NHBs, inputs needed are total costs and DALYs lost

# winner <- function (EPIcost, WTP, NHB1, NHB2, NHB3, NHB4, NHB5, NHB6) {
#   pVax_old_tcost <- sum(pVax_older_admin * cov_pVax_o* num_infants * (cost_nd + cost_prod)) +  medcost_er_pVax_older
#   llAb_pVax_old_tcost <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * cov_pVax_o * num_infants * (cost_nd + cost_prod)) + medcost_er_llAb_pVax_older
#   mVax_pVax_old_tcost <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_older_admin * cov_pVax_o * num_infants * (cost_nd + cost_prod)) + medcost_er_mVax_pVax_older
#   NHB_pVax_o <- (DALYS_lost_er_no - DALYS_er_pVax_older) - (pVax_old_tcost - medcost_no_u) / WTP
#   NHB_llAb_pVax_o <- (DALYS_lost_er_no - DALYS_er_llAb_pVax_older) - (llAb_pVax_old_tcost - medcost_no_u) / WTP
#   NHB_mVax_pVax_o <- (DALYS_lost_er_no - DALYS_er_mVax_pVax_older) - (mVax_pVax_old_tcost - medcost_no_u) / WTP
#   NHBall <- array(c(NHB1, NHB2, NHB3, NHB4, NHB5, NHB6, NHB_pVax_o, NHB_llAb_pVax_o, NHB_mVax_pVax_o), dim = c(trials, length(eff_red), 9))
#   winners <- apply(NHBall, MARGIN = c(1,2), FUN = which.max)
#   wintakeall <- apply(winners, MARGIN = 2, FUN = getmode)
#   wintakeall
# }
# 
# EPI_cost <- seq(0, 8, by = 0.10)
# 
# figdata <- matrix(NA, nrow = length(EPI_cost), ncol = length(eff_red))
# for (epi in 1:length(EPI_cost)) {
#   ec <- EPI_cost[epi]
#   figdata [epi,] <- winner(ec, WTP_5k, NHB_no_5k, NHB_l_5k, NHB_m_5k, NHB_p_5k_er, NHB_lp_5k, NHB_mp_5k)
# }

# for two-way sensitivity analysis figure 2
# cost of llAb product vs. WTP

# winner_llc <- function (llcost, NHB1, NHB2, NHB3, NHB4, NHB5, NHB6) {
#   llAb_tcost <- sum(llAb_admin * coverage[1]* num_infants * (llcost + cost_nd)) +  medcost_llAb_u
#   llAb_pVax_tcost <- sum(llAb_admin * coverage[1] * num_infants * (llcost + cost_nd)) + sum(pVax_admin * cov_pVax_o * num_infants * (cost_nd + cost_prod)) + medcost_joint_llAb_pVax_u
#   llAb_pVax_o_tcost <- sum(llAb_admin * coverage[1] * num_infants * (llcost + cost_nd)) + sum(pVax_older_admin * cov_pVax_o * num_infants * (cost_nd + cost_prod)) + medcost_joint_llAb_pVax_u_older
#   NHB_llAb <- (DALYS_lost_no_u - DALYS_lost_llAb_u) - (llAb_tcost - medcost_no_u) / WTP_sp
#   NHB_llAb_pVax <- (DALYS_lost_no_u - DALYS_lost_joint_llAb_pVax_u) - (llAb_pVax_tcost - medcost_no_u) / WTP_sp
#   NHB_llAb_pVax_o <- (DALYS_lost_no_u - DALYS_lost_joint_llAb_pVax_older_u) - (llAb_pVax_o_tcost - medcost_no_u) / WTP_sp
#   NHBall <- array(c(NHB1, NHB2, NHB3, NHB4, NHB5, NHB6, NHB_llAb, NHB_llAb_pVax, NHB_llAb_pVax_o), dim = c(trials, length(WTP_sp), 9))
#   winners <- apply(NHBall, MARGIN = c(1,2), FUN = which.max)
#   wintakeall <- apply(winners, MARGIN = 2, FUN = getmode)
#   wintakeall
# }
# 
# llAb_cost <- seq(0, 50, by = 1)
# 
# llcost_fig <- matrix(NA, nrow = 12, ncol = length(WTP_sp))
# for (llc in 1:12) {
#   llcost_fig [llc,] <- winner_llc(llc, NHB_no, NHB_m, NHB_p, NHB_mp, NHB_p_older, NHB_mp_older)
# }


# par(mar = c(5.1, 4.1, 4.1, 2.1))
# par(xaxs="i", yaxs="i")
# image.plot(rotate(llcost_fig), axes = TRUE, legend.shrink = 0.5, legend.mar = 10.1,
#            xlab="Society willingness to pay", ylab="Cost of llAb product")



####
# for two-way sensitivity analysis figure 4
# which product has the greatest probability of being optimal across the following margins:
# cost of llAb product vs. cost of pVax product per dose

llAb_cost <- seq(0, 3, by = 0.02)
pVax_cost <- seq(0, 2, by = 0.01)

winner_lp <- function (llcost, pvcost, WTP, NHB1, NHB2) {
  llAb_tcost <- sum(llAb_admin * coverage[1]* num_infants * (llcost + cost_nd)) +  medcost_llAb_u
  pVax_tcost <- sum(pVax_admin *coverage[3]* num_infants * (pvcost + cost_nd)) + medcost_pVax_u
  pVax_o_tcost <- sum(pVax_older_admin * cov_pVax_o * num_infants * (cost_nd + pvcost)) + medcost_pVax_u_older
  llAb_pVax_tcost <- sum(llAb_admin * coverage[1] * num_infants * (llcost + cost_nd)) + sum(pVax_admin * coverage[3] * num_infants * (pvcost + cost_nd)) + medcost_joint_llAb_pVax_u
  llAb_pVax_o_tcost <- sum(llAb_admin * coverage[1] * num_infants * (llcost + cost_nd)) + sum(pVax_older_admin * cov_pVax_o * num_infants * (cost_nd + pvcost)) + medcost_joint_llAb_pVax_u_older
  mVax_pVax_tcost <- sum(mVax_admin * coverage[2]* num_infants * (cost_prod + cost_nd)) + sum(pVax_admin * coverage[3] * num_infants * (pvcost + cost_nd)) + medcost_joint_mVax_pVax_u
  mVax_pVax_o_tcost <- sum(mVax_admin * coverage[2]* num_infants * (cost_prod + cost_nd)) + sum(pVax_older_admin * cov_pVax_o * num_infants * (cost_nd + pvcost)) + medcost_joint_mVax_pVax_u_older
  NHB_llAb <- (DALYS_lost_no_u - DALYS_lost_llAb_u) - (llAb_tcost - medcost_no_u) / WTP
  NHB_pVax <- (DALYS_lost_no_u - DALYS_lost_pVax_u) - (pVax_tcost - medcost_no_u) / WTP
  NHB_pVax_o <- (DALYS_lost_no_u - DALYS_lost_pVax_older_u) - (pVax_o_tcost - medcost_no_u) / WTP
  NHB_llAb_pVax <- (DALYS_lost_no_u - DALYS_lost_joint_llAb_pVax_u) - (llAb_pVax_tcost - medcost_no_u) / WTP
  NHB_llAb_pVax_o <- (DALYS_lost_no_u - DALYS_lost_joint_llAb_pVax_older_u) - (llAb_pVax_o_tcost - medcost_no_u) / WTP
  NHB_mVax_pVax <- (DALYS_lost_no_u - DALYS_lost_joint_mVax_pVax_u) - (mVax_pVax_tcost - medcost_no_u) / WTP
  NHB_mVax_pVax_o <- (DALYS_lost_no_u - DALYS_lost_joint_mVax_pVax_older_u) - (mVax_pVax_o_tcost - medcost_no_u) / WTP
  NHB_all <- matrix(c(NHB1, NHB_llAb, NHB2, NHB_pVax, NHB_llAb_pVax, NHB_mVax_pVax, NHB_pVax_o, NHB_llAb_pVax_o, NHB_mVax_pVax_o), nrow = trials, ncol = 9)
  winners <- apply(NHB_all, MARGIN = 1, FUN = which.max)
  wintakeall <- getmode(winners)
  probwin <-sum(winners == wintakeall)/length(winners)
  list(wintakeall, probwin)
}


SA_llpv <- matrix(NA, nrow = length(llAb_cost), ncol = length(pVax_cost))
alpha_llpv <- matrix(NA, nrow = length(llAb_cost), ncol = length(pVax_cost))
for (lpp in 1:length(llAb_cost)) {
  for(pv in 1:length(pVax_cost)){
  llc <- llAb_cost[lpp]
  pvc <- pVax_cost[pv]
  temp <- winner_lp(llc, pvc, CET_Mali_GDP, NHB_no_GDP, NHB_m_GDP)
  SA_llpv[lpp,pv] <- temp[[1]]
  alpha_llpv[lpp,pv] <- temp[[2]]
}
}

# then construct new data frame with 4 columns: x-value = price llAb,
# y-value = price pVax, winner strategy, alpha value (pOptimal)

SA_llpv_df <- melt(
  SA_llpv,
  varnames = names(dimnames(SA_llpv)),
  na.rm = FALSE,
  as.is = FALSE,
  value.name = "value"
)

colnames(SA_llpv_df) <- c("llAb_price", "pVax_price", "strategy")

SA_llpv_df$llAb_price <- rep(llAb_cost, times = length(pVax_cost))
SA_llpv_df$pVax_price <- rep(pVax_cost, each = length(llAb_cost))


# then do the same thing for alpha (pOptimal matrix)
# take the last column of values and add it to SA_llpv_df

alpha_llpv_df <- melt(
  alpha_llpv,
  varnames = names(dimnames(alpha_llpv)),
  na.rm = FALSE,
  as.is = FALSE,
  value.name = "value"
)
colnames(alpha_llpv_df) <- c("llAb_price", "pVax_price", "probwin")

SA_llpv_df$probwin <- alpha_llpv_df$probwin
SA_llpv_df$strategy <- factor(SA_llpv_df$strategy, levels = 1:10)

# for two-way sensitivity analysis figure 5
# cost of llAb product vs. llAb vaccine efficacy
# NOTE: objects pertaining to this analyses have suffix "ce"

DALYS_lost_l_ce <- matrix(NA, trials, length(eff_red))
medcost_l_ce <- matrix(NA, trials, length(eff_red))
for (l in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_LRTI_l <- LRTI_func(eff_red[er], coverage[1], mat_eff_llAb, p_pneum_u[l], cases_no_u_bic[,,l]) # number of LRTI episodes
    temp_adj_LRTI_l <- adj_func(temp_LRTI_l)
    temp_inpat_l <- inpat_func(p_hosp_u[l,], temp_adj_LRTI_l) # number of inpatient episodes
    temp_outpat_l <- outpat_func(temp_adj_LRTI_l, temp_inpat_l) # number of outpatient episodes
    temp_nrcare_l <- nr_care_func(temp_inpat_l) # number not receiving care
    temp_death_l <- mort_inpat_func(CFR_by_age_u[l,], temp_inpat_l, CFR_nr_care_u[l,], temp_nrcare_l) # number of deaths
    temp_YLL_l <- YLL_func(temp_death_l) # YLL
    temp_YLD_l <- YLD_func(temp_inpat_l, temp_death_l, di_yrs_u[l], dw_LRTI_severe_u[l], temp_adj_LRTI_l, dw_LRTI_mod_u[l]) # YLD
    DALYS_lost_l_ce[l, er] <- sum(temp_YLD_l + temp_YLL_l) # DALYs lost
    medcost_l_ce[l, er] <- sum(medcost_func(cost_hosp_u[l], temp_inpat_l, cost_outpatient_u[l], temp_outpat_l)) # medcosts
  }}


DALYS_lost_lp_ce <- matrix(NA, trials, length(eff_red))
medcost_lp_ce <- matrix(NA, trials, length(eff_red))
for (lp in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_LRTI_lp <- LRTI_func_joint(eff_red[er], efficacy[3], coverage[1], coverage[3], mat_eff_llAb, mat_eff_pVax, p_pneum_u[lp], cases_no_u_bic[,,lp]) # number of LRTI episodes
    temp_adj_LRTI_lp <- adj_func(temp_LRTI_lp)
    temp_inpat_lp <- inpat_func(p_hosp_u[lp,], temp_adj_LRTI_lp) # number of inpatient episodes
    temp_outpat_lp <- outpat_func(temp_adj_LRTI_lp, temp_inpat_lp) # number of outpatient episodes
    temp_nrcare_lp <- nr_care_func(temp_inpat_lp) # number not receiving care
    temp_death_lp <- mort_inpat_func(CFR_by_age_u[lp,], temp_inpat_lp, CFR_nr_care_u[lp,], temp_nrcare_lp) # number of deaths
    temp_YLL_lp <- YLL_func(temp_death_lp) # YLL
    temp_YLD_lp <- YLD_func(temp_inpat_lp, temp_death_lp, di_yrs_u[lp], dw_LRTI_severe_u[lp], temp_adj_LRTI_lp, dw_LRTI_mod_u[lp]) # YLD
    DALYS_lost_lp_ce[lp, er] <- sum(temp_YLD_lp + temp_YLL_lp) # DALYs lost
    medcost_lp_ce[lp, er] <- sum(medcost_func(cost_hosp_u[lp], temp_inpat_lp, cost_outpatient_u[lp], temp_outpat_lp)) # medcosts
  }}

DALYS_lost_lp_o_ce <- matrix(NA, trials, length(eff_red))
medcost_lp_o_ce <- matrix(NA, trials, length(eff_red))
for (lpo in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_LRTI_lpo <- LRTI_func_joint(eff_red[er], efficacy[3], coverage[1], cov_pVax_o, mat_eff_llAb, mat_eff_older_pVax, p_pneum_u[lpo], cases_no_u_bic[,,lpo]) # number of LRTI episodes
    temp_adj_LRTI_lpo <- adj_func(temp_LRTI_lpo)
    temp_inpat_lpo <- inpat_func(p_hosp_u[lpo,], temp_adj_LRTI_lpo) # number of inpatient episodes
    temp_outpat_lpo <- outpat_func(temp_adj_LRTI_lpo, temp_inpat_lpo) # number of outpatient episodes
    temp_nrcare_lpo <- nr_care_func(temp_inpat_lpo) # number not receiving care
    temp_death_lpo <- mort_inpat_func(CFR_by_age_u[lpo,], temp_inpat_lpo, CFR_nr_care_u[lpo,], temp_nrcare_lpo) # number of deaths
    temp_YLL_lpo <- YLL_func(temp_death_lpo) # YLL
    temp_YLD_lpo <- YLD_func(temp_inpat_lpo, temp_death_lpo, di_yrs_u[lpo], dw_LRTI_severe_u[lpo], temp_adj_LRTI_lpo, dw_LRTI_mod_u[lpo]) # YLD
    DALYS_lost_lp_o_ce[lpo, er] <- sum(temp_YLD_lpo + temp_YLL_lpo) # DALYs lost
    medcost_lp_o_ce[lpo, er] <- sum(medcost_func(cost_hosp_u[lpo], temp_inpat_lpo, cost_outpatient_u[lpo], temp_outpat_lpo)) # medcosts
  }}

pwin <- function(x){
  y <- getmode(x)
  w <- sum((x == y)/ length(x))
  w
}

winner_llAb_cost_ce <- function (llcost, WTP, NHB1, NHB2, NHB3, NHB4, NHB5, NHB6) {
  NHB1_m <- rep.col(NHB1, length(eff_red))
  NHB2_m <- rep.col(NHB2, length(eff_red))
  NHB3_m <- rep.col(NHB3, length(eff_red))
  NHB4_m <- rep.col(NHB4, length(eff_red))
  NHB5_m <- rep.col(NHB5, length(eff_red))
  NHB6_m <- rep.col(NHB6, length(eff_red))

  llAb_tcost <- sum(llAb_admin * coverage[1]* num_infants * (llcost + cost_nd)) +  medcost_l_ce
  llAb_pVax_tcost <- sum(llAb_admin * coverage[1] * num_infants * (llcost + cost_nd)) + sum(pVax_admin * coverage[3] * num_infants * (cost_nd + cost_prod)) + medcost_lp_ce
  llAb_pVax_o_tcost <- sum(llAb_admin * coverage[1] * num_infants * (llcost + cost_nd)) + sum(pVax_older_admin * cov_pVax_o * num_infants * (cost_nd + cost_prod)) + medcost_lp_o_ce
  
  DALYS_no_u_m <- rep.col(DALYS_lost_no_u, length(eff_red))
  medcost_no_u_m <- rep.col(medcost_no_u, length(eff_red))
  
  NHB_llAb <- (DALYS_no_u_m - DALYS_lost_l_ce) - (llAb_tcost - medcost_no_u_m) / WTP
  NHB_llAb_pVax <- (DALYS_no_u_m - DALYS_lost_lp_ce) - (llAb_pVax_tcost - medcost_no_u_m) / WTP
  NHB_llAb_pVax_o <- (DALYS_no_u_m - DALYS_lost_lp_o_ce) - (llAb_pVax_o_tcost - medcost_no_u_m) / WTP
  
  NHB_all <- array(c(NHB1_m, NHB_llAb, NHB2_m, NHB3_m, NHB_llAb_pVax, NHB4_m, NHB5_m, NHB_llAb_pVax_o, NHB6_m), dim = c(trials, length(eff_red), 9))
  winners <- apply(NHB_all, MARGIN = c(1,2), FUN = which.max)
  wintakeall <- apply(winners, MARGIN = 2, FUN = getmode)
  probwin <- apply(winners, MARGIN = 2, FUN = pwin)
  list(wintakeall, probwin)
}



SA_ll_ce <- matrix(NA, nrow = length(llAb_cost), ncol = length(eff_red))
alpha_ll_ce <- matrix(NA, nrow = length(llAb_cost), ncol = length(eff_red))
for (ll in 1:length(llAb_cost)){
  llcc <- llAb_cost[ll]
  temp_ce <- winner_llAb_cost_ce(llcc, CET_Mali_GDP, NHB_no_GDP, NHB_m_GDP, NHB_p_GDP, NHB_mp_GDP, NHB_p_older_GDP, NHB_mp_older_GDP)
  SA_ll_ce[ll,] <- temp_ce[[1]]
  alpha_ll_ce[ll,] <- temp_ce[[2]]
  }

SA_ll_ce_df <- melt(
  SA_ll_ce,
  varnames = names(dimnames(SA_ll_ce)),
  na.rm = FALSE,
  as.is = FALSE,
  value.name = "value"
)

colnames(SA_ll_ce_df) <- c("llAb_price", "llAb_efficacy", "strategy")

SA_ll_ce_df$llAb_price <- rep(llAb_cost, times = length(eff_red))
SA_ll_ce_df$llAb_efficacy <- rep(eff_red*100, each = length(llAb_cost))

alpha_ll_ce_df <- melt(
  alpha_ll_ce,
  varnames = names(dimnames(alpha_ll_ce)),
  na.rm = FALSE,
  as.is = FALSE,
  value.name = "value"
)
colnames(alpha_ll_ce_df) <- c("llAb_price", "llAb_efficacy", "probwin")

SA_ll_ce_df$probwin <- alpha_ll_ce_df$probwin
SA_ll_ce_df$strategy <- factor(SA_ll_ce_df$strategy, levels = 1:10)


# Two-way sensitvity analyis: 
# price of pVax vaccine vs efficacy of pVax (10 and 14 wks)
# Use "pe" suffix for this analysis

DALYS_lost_p_pe <- matrix(NA, trials, length(eff_red))
medcost_p_pe <- matrix(NA, trials, length(eff_red))
for (p in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_LRTI_p <- LRTI_func(eff_red[er], coverage[3], mat_eff_pVax, p_pneum_u[p], cases_no_u_bic[,,p]) # number of LRTI episodes
    temp_adj_LRTI_p <- adj_func(temp_LRTI_p)
    temp_inpat_p <- inpat_func(p_hosp_u[p,], temp_adj_LRTI_p) # number of inpatient episodes
    temp_outpat_p <- outpat_func(temp_adj_LRTI_p, temp_inpat_p) # number of outpatient episodes
    temp_nrcare_p <- nr_care_func(temp_inpat_p) # number not receiving care
    temp_death_p <- mort_inpat_func(CFR_by_age_u[p,], temp_inpat_p, CFR_nr_care_u[p,], temp_nrcare_p) # number of deaths
    temp_YLL_p<- YLL_func(temp_death_p) # YLL
    temp_YLD_p <- YLD_func(temp_inpat_p, temp_death_p, di_yrs_u[p], dw_LRTI_severe_u[p], temp_adj_LRTI_p, dw_LRTI_mod_u[p]) # YLD
    DALYS_lost_p_pe[p, er] <- sum(temp_YLD_p + temp_YLL_p) # DALYs lost
    medcost_p_pe[p, er] <- sum(medcost_func(cost_hosp_u[p], temp_inpat_p, cost_outpatient_u[p], temp_outpat_p)) # medcosts
  }}


DALYS_lost_lp_pe <- matrix(NA, trials, length(eff_red))
medcost_lp_pe <- matrix(NA, trials, length(eff_red))
for (lp in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_LRTI_lp_pe <- LRTI_func_joint(efficacy[1], eff_red[er], coverage[1], coverage[3], mat_eff_llAb, mat_eff_pVax, p_pneum_u[lp], cases_no_u_bic[,,lp]) # number of LRTI episodes
    temp_adj_LRTI_lp_pe <- adj_func(temp_LRTI_lp_pe)
    temp_inpat_lp_pe <- inpat_func(p_hosp_u[lp,], temp_adj_LRTI_lp_pe) # number of inpatient episodes
    temp_outpat_lp_pe <- outpat_func(temp_adj_LRTI_lp_pe, temp_inpat_lp_pe) # number of outpatient episodes
    temp_nrcare_lp_pe <- nr_care_func(temp_inpat_lp_pe) # number not receiving care
    temp_death_lp_pe <- mort_inpat_func(CFR_by_age_u[lp,], temp_inpat_lp_pe, CFR_nr_care_u[lp,], temp_nrcare_lp_pe) # number of deaths
    temp_YLL_lp_pe <- YLL_func(temp_death_lp_pe) # YLL
    temp_YLD_lp_pe <- YLD_func(temp_inpat_lp_pe, temp_death_lp_pe, di_yrs_u[lp], dw_LRTI_severe_u[lp], temp_adj_LRTI_lp_pe, dw_LRTI_mod_u[lp]) # YLD
    DALYS_lost_lp_pe[lp, er] <- sum(temp_YLD_lp_pe + temp_YLL_lp_pe) # DALYs lost
    medcost_lp_pe[lp, er] <- sum(medcost_func(cost_hosp_u[lp], temp_inpat_lp_pe, cost_outpatient_u[lp], temp_outpat_lp_pe)) # medcosts
  }}

winner_pe <- function (pvcost, WTP, NHB1, NHB2, NHB3, NHB4, NHB5, NHB6, NHB7) {
  NHB1_m <- rep.col(NHB1, length(eff_red))
  NHB2_m <- rep.col(NHB2, length(eff_red))
  NHB3_m <- rep.col(NHB3, length(eff_red))
  NHB4_m <- rep.col(NHB4, length(eff_red))
  NHB5_m <- rep.col(NHB5, length(eff_red))
  NHB6_m <- rep.col(NHB6, length(eff_red))
  NHB7_m <- rep.col(NHB7, length(eff_red))
  
  pVax_tcost <- sum(pVax_admin * coverage[3]* num_infants * (pvcost + cost_nd)) +  medcost_p_pe
  llAb_pVax_tcost <- sum(llAb_admin * coverage[1] * num_infants * (cost_prod + cost_nd)) + sum(pVax_admin * coverage[3] * num_infants * (cost_nd + pvcost)) + medcost_lp_pe
  
  DALYS_no_u_m <- rep.col(DALYS_lost_no_u, length(eff_red))
  medcost_no_u_m <- rep.col(medcost_no_u, length(eff_red))
  
  NHB_pVax <- (DALYS_no_u_m - DALYS_lost_p_pe) - (pVax_tcost - medcost_no_u_m) / WTP
  NHB_llAb_pVax <- (DALYS_no_u_m - DALYS_lost_lp_pe) - (llAb_pVax_tcost - medcost_no_u_m) / WTP
  
  NHB_all <- array(c(NHB1_m, NHB2_m, NHB3_m, NHB_pVax, NHB_llAb_pVax, NHB4_m, NHB5_m, NHB6_m, NHB7_m), dim = c(trials, length(eff_red), 9))
  winners <- apply(NHB_all, MARGIN = c(1,2), FUN = which.max)
  wintakeall <- apply(winners, MARGIN = 2, FUN = getmode)
  probwin <- apply(winners, MARGIN = 2, FUN = pwin)
  list(wintakeall, probwin)
}

SA_pe <- matrix(NA, nrow = length(pVax_cost), ncol = length(eff_red))
alpha_pe <- matrix(NA, nrow = length(pVax_cost), ncol = length(eff_red))
for (pe in 1:length(pVax_cost)){
  pecost <- pVax_cost[pe]
  temp_pe <- winner_pe(pecost, CET_Mali_GDP, NHB_no_GDP, NHB_l_GDP, NHB_m_GDP, NHB_mp_GDP, NHB_p_older_GDP, NHB_lp_older_GDP, NHB_mp_older_GDP)
  SA_pe[pe,] <- temp_pe[[1]]
  alpha_pe[pe,] <- temp_pe[[2]]
  }

SA_pe_df <- melt(
  SA_pe,
  varnames = names(dimnames(SA_pe)),
  na.rm = FALSE,
  as.is = FALSE,
  value.name = "value"
)

colnames(SA_pe_df) <- c("pVax_price", "pVax_efficacy", "strategy")

SA_pe_df$pVax_price <- rep(pVax_cost, times = length(eff_red))
SA_pe_df$pVax_efficacy <- rep(eff_red*100, each = length(pVax_cost))

alpha_pe_df <- melt(
  alpha_pe,
  varnames = names(dimnames(alpha_pe)),
  na.rm = FALSE,
  as.is = FALSE,
  value.name = "value"
)
colnames(alpha_pe_df) <- c("pVax_price", "pVax_efficacy", "probwin")

SA_pe_df$probwin <- alpha_pe_df$probwin
SA_pe_df$strategy <- factor(SA_pe_df$strategy, levels = 1:10)
#####

####
# cost of llAb product vs. pVax vaccine efficacy at 10 & 14 wks.
# use "clpe" suffix for objects in this analysis

DALYS_lost_p_clpe <- matrix(NA, trials, length(eff_red))
medcost_p_clpe <- matrix(NA, trials, length(eff_red))
for (p in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_LRTI_p_clpe <- LRTI_func(eff_red[er], coverage[3], mat_eff_pVax, p_pneum_u[p], cases_no_u_bic[,,p]) # number of LRTI episodes
    temp_adj_LRTI_p_clpe <- adj_func(temp_LRTI_p_clpe)
    temp_inpat_p_clpe <- inpat_func(p_hosp_u[p,], temp_adj_LRTI_p_clpe) # number of inpatient episodes
    temp_outpat_p_clpe <- outpat_func(temp_adj_LRTI_p_clpe, temp_inpat_p_clpe) # number of outpatient episodes
    temp_nrcare_p_clpe <- nr_care_func(temp_inpat_p_clpe) # number not receiving care
    temp_death_p_clpe <- mort_inpat_func(CFR_by_age_u[p,], temp_inpat_p_clpe, CFR_nr_care_u[p,], temp_nrcare_p_clpe) # number of deaths
    temp_YLL_p_clpe <- YLL_func(temp_death_p_clpe) # YLL
    temp_YLD_p_clpe <- YLD_func(temp_inpat_p_clpe, temp_death_p_clpe, di_yrs_u[p], dw_LRTI_severe_u[p], temp_adj_LRTI_p_clpe, dw_LRTI_mod_u[p]) # YLD
    DALYS_lost_p_clpe[p, er] <- sum(temp_YLD_p_clpe + temp_YLL_p_clpe) # DALYs lost
    medcost_p_clpe[p, er] <- sum(medcost_func(cost_hosp_u[p], temp_inpat_p_clpe, cost_outpatient_u[p], temp_outpat_p_clpe)) # medcosts
  }}

DALYS_lost_lp_clpe <- matrix(NA, trials, length(eff_red))
medcost_lp_clpe <- matrix(NA, trials, length(eff_red))
for (lp in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_LRTI_lp_clpe <- LRTI_func_joint(efficacy[1], eff_red[er], coverage[1], coverage[3], mat_eff_llAb, mat_eff_pVax, p_pneum_u[lp], cases_no_u_bic[,,lp]) # number of LRTI episodes
    temp_adj_LRTI_lp_clpe <- adj_func(temp_LRTI_lp_clpe)
    temp_inpat_lp_clpe <- inpat_func(p_hosp_u[lp,], temp_adj_LRTI_lp_clpe) # number of inpatient episodes
    temp_outpat_lp_clpe <- outpat_func(temp_adj_LRTI_lp_clpe, temp_inpat_lp_clpe) # number of outpatient episodes
    temp_nrcare_lp_clpe <- nr_care_func(temp_inpat_lp_clpe) # number not reclpeiving care
    temp_death_lp_clpe <- mort_inpat_func(CFR_by_age_u[lp,], temp_inpat_lp_clpe, CFR_nr_care_u[lp,], temp_nrcare_lp_clpe) # number of deaths
    temp_YLL_lp_clpe <- YLL_func(temp_death_lp_clpe) # YLL
    temp_YLD_lp_clpe <- YLD_func(temp_inpat_lp_clpe, temp_death_lp_clpe, di_yrs_u[lp], dw_LRTI_severe_u[lp], temp_adj_LRTI_lp_clpe, dw_LRTI_mod_u[lp]) # YLD
    DALYS_lost_lp_clpe[lp, er] <- sum(temp_YLD_lp_clpe + temp_YLL_lp_clpe) # DALYs lost
    medcost_lp_clpe[lp, er] <- sum(medcost_func(cost_hosp_u[lp], temp_inpat_lp_clpe, cost_outpatient_u[lp], temp_outpat_lp_clpe)) # medcosts
  }}

winner_clpe <- function (llcost, WTP, NHB1, NHB2) {
  NHB1_m <- rep.col(NHB1, length(eff_red))
  NHB2_m <- rep.col(NHB2, length(eff_red))  
  DALYS_no_u_m <- rep.col(DALYS_lost_no_u, length(eff_red))
  DALYS_llAb_u_m <- rep.col(DALYS_lost_llAb_u, length(eff_red))
  DALYS_pVax_u_older_m <-rep.col(DALYS_lost_pVax_older_u, length(eff_red))
  DALYS_llAb_pVax_u_older_m <-rep.col(DALYS_lost_joint_llAb_pVax_older_u, length(eff_red))
  DALYS_mVax_pVax_u_older_m <-rep.col(DALYS_lost_joint_mVax_pVax_older_u, length(eff_red))
  medcost_no_u_m <- rep.col(medcost_no_u, length(eff_red))
  medcost_llAb_u_m <- rep.col(medcost_llAb_u, length(eff_red))
  medcost_pVax_u_older_m <- rep.col(medcost_pVax_u_older, length(eff_red))
  medcost_llAb_pVax_u_older_m <- rep.col(medcost_joint_llAb_pVax_u_older, length(eff_red))
  medcost_mVax_pVax_u_older_m <-rep.col(medcost_joint_mVax_pVax_u_older, length(eff_red))
  
  llAb_tcost <- sum(llAb_admin * coverage[1]* num_infants * (llcost + cost_nd)) +  medcost_llAb_u_m
  pVax_tcost <- sum(pVax_admin * coverage[3]* num_infants * (cost_prod + cost_nd)) +  medcost_p_clpe
  llAb_pVax_tcost <- sum(llAb_admin * coverage[1] * num_infants * (llcost + cost_nd)) + sum(pVax_admin * coverage[3] * num_infants * (cost_nd + cost_prod)) + medcost_lp_clpe
  mVax_pVax_tcost <- sum(mVax_admin * coverage[2] * num_infants * (cost_prod + cost_nd)) + sum(pVax_admin * coverage[3] * num_infants * (cost_nd + cost_prod)) + medcost_er_mp
  pVax_o_tcost <- sum(pVax_older_admin * cov_pVax_o * num_infants * (cost_nd + cost_prod)) + medcost_pVax_u_older_m 
  llAb_pVax_o_tcost <- sum(llAb_admin * coverage[1] * num_infants * (llcost + cost_nd)) + sum(pVax_older_admin * cov_pVax_o * num_infants * (cost_nd + cost_prod)) + medcost_llAb_pVax_u_older_m
  mVax_pVax_o_tcost <- sum(mVax_admin * coverage[2] * num_infants * (cost_prod + cost_nd)) + sum(pVax_older_admin * cov_pVax_o * num_infants * (cost_nd + cost_prod)) + medcost_mVax_pVax_u_older_m
  
  NHB_llAb <- (DALYS_no_u_m - DALYS_llAb_u_m) - (llAb_tcost - medcost_no_u_m) / WTP
  NHB_pVax <- (DALYS_no_u_m - DALYS_lost_p_clpe) - (pVax_tcost - medcost_no_u_m) / WTP
  NHB_llAb_pVax <- (DALYS_no_u_m - DALYS_lost_lp_clpe) - (llAb_pVax_tcost - medcost_no_u_m) / WTP
  NHB_mVax_pVax <- (DALYS_no_u_m - DALYS_lost_er_mp) - (mVax_pVax_tcost - medcost_no_u_m) / WTP
  NHB_pVax_o <- (DALYS_no_u_m - DALYS_pVax_u_older_m) - (pVax_o_tcost - medcost_no_u_m) / WTP
  NHB_llAb_pVax_o <- (DALYS_no_u_m - DALYS_llAb_pVax_u_older_m) - (llAb_pVax_o_tcost - medcost_no_u_m) / WTP
  NHB_mVax_pVax_o <- (DALYS_no_u_m - DALYS_mVax_pVax_u_older_m) - (mVax_pVax_o_tcost - medcost_no_u_m) / WTP
  
  NHB_all <- array(c(NHB1_m, NHB_llAb, NHB2_m, NHB_pVax, NHB_llAb_pVax, NHB_mVax_pVax, NHB_pVax_o, NHB_llAb_pVax_o, NHB_mVax_pVax_o), dim = c(trials, length(eff_red), 9))
  winners <- apply(NHB_all, MARGIN = c(1,2), FUN = which.max)
  wintakeall <- apply(winners, MARGIN = 2, FUN = getmode)
  probwin <- apply(winners, MARGIN = 2, FUN = pwin)
  list(wintakeall, probwin)
}


SA_clpe <- matrix(NA, nrow = length(llAb_cost), ncol = length(eff_red))
alpha_clpe <- matrix(NA, nrow = length(llAb_cost), ncol = length(eff_red))
for (ll in 1:length(llAb_cost)) {
  llcc <- llAb_cost[ll]
  temp_clpe <- winner_clpe(llcc, CET_Mali_GDP, NHB_no_GDP, NHB_m_GDP)
  SA_clpe[ll,] <- temp_clpe[[1]]
  alpha_clpe[ll,] <- temp_clpe[[2]]
}

SA_clpe_df <- melt(
  SA_clpe,
  varnames = names(dimnames(SA_clpe)),
  na.rm = FALSE,
  as.is = FALSE,
  value.name = "value"
)

colnames(SA_clpe_df) <- c("llAb_price", "pVax_efficacy", "strategy")

SA_clpe_df$llAb_price <- rep(llAb_cost, times = length(eff_red))
SA_clpe_df$pVax_efficacy <- rep(eff_red*100, each = length(llAb_cost))

alpha_clpe_df <- melt(
  alpha_clpe,
  varnames = names(dimnames(alpha_clpe)),
  na.rm = FALSE,
  as.is = FALSE,
  value.name = "value"
)
colnames(alpha_clpe_df) <- c("llAb_price", "pVax_efficacy", "probwin")

SA_clpe_df$probwin <- alpha_clpe_df$probwin
SA_clpe_df$strategy <- factor(SA_clpe_df$strategy, levels = 1:10)
