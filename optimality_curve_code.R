
# Code to produce plot for probability each intervention is optimal by WTP

# Calculate number of RSV cases w/ uncertainty
cases_no_u_age <- t(apply(AR_y_u, 3, RSVcases, babies = num_infants, mort = mort_mat))

pd_llAb_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (ll in 1:trials) {
  pd_llAb_array[,,ll] <- pd_calc(efficacy[1], coverage[1], AR_y_u[,,ll], mat_eff_llAb)
} 
cases_llAb_u_age <- t(apply(pd_llAb_array, 3, RSVcases, babies = num_infants, mort = mort_mat))

pd_mVax_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (m in 1:trials) {
  pd_mVax_array[,,m] <- pd_calc(efficacy[2], coverage[2], AR_y_u[,,m], mat_eff_mVax)
} 
cases_mVax_u_age <- t(apply(pd_mVax_array, 3, RSVcases, babies = num_infants, mort = mort_mat))

pd_pVax_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (p in 1:trials) {
  pd_pVax_array[,,p] <- pd_calc(efficacy[3], coverage[3], AR_y_u[,,p], mat_eff_pVax)
} 
cases_pVax_u_age <- t(apply(pd_pVax_array, 3, RSVcases, babies = num_infants, mort = mort_mat))

# scenario analysis when efficacy reduction occurs only when pVax is in combo
pd_pVax_array_SA <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (p in 1:trials) {
  pd_pVax_array_SA[,,p] <- pd_calc(efficacy[3], coverage[3], AR_y_u[,,p], mat_eff_pVax)
} 
cases_pVax_u_SA_age <- t(apply(pd_pVax_array, 3, RSVcases, babies = num_infants, mort = mort_mat))

pd_llAb_pVax_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (lp in 1:trials) {
  pd_llAb_pVax_array[,,lp] <- pd_joint(efficacy[1], efficacy[3], coverage[1], coverage[3], AR_y_u[,,lp], mat_eff_llAb, mat_eff_pVax)
}
cases_joint_llAb_pVax_u_age <- t(apply(pd_llAb_pVax_array, 3, RSVcases, babies = num_infants, mort = mort_mat))

pd_mVax_pVax_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (mp in 1:trials) {
  pd_mVax_pVax_array[,,mp] <- pd_joint(efficacy[2], efficacy[3], coverage[2], coverage[3], AR_y_u[,,mp], mat_eff_mVax, mat_eff_pVax)
}
cases_joint_mVax_pVax_u_age <- t(apply(pd_mVax_pVax_array, 3, RSVcases, babies = num_infants, mort = mort_mat))

pd_pVax_older_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (p in 1:trials) {
  pd_pVax_older_array[,,p] <- pd_calc(efficacy[3], cov_pVax_o, AR_y_u[,,p], mat_eff_older_pVax)
} 
cases_pVax_older_u_age <- t(apply(pd_pVax_older_array, 3, RSVcases, babies = num_infants, mort = mort_mat))

pd_llAb_pVax_older_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (lp in 1:trials) {
  pd_llAb_pVax_older_array[,,lp] <- pd_joint(efficacy[1], efficacy[3], coverage[1], cov_pVax_o, AR_y_u[,,lp], mat_eff_llAb, mat_eff_older_pVax)
}
cases_joint_llAb_pVax_older_u_age <- t(apply(pd_llAb_pVax_older_array, 3, RSVcases, babies = num_infants, mort = mort_mat))

pd_mVax_pVax_older_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (mp in 1:trials) {
  pd_mVax_pVax_older_array[,,mp] <- pd_joint(efficacy[2], efficacy[3], coverage[2], cov_pVax_o, AR_y_u[,,mp], mat_eff_mVax, mat_eff_older_pVax)
}
cases_joint_mVax_pVax_older_u_age <- t(apply(pd_mVax_pVax_older_array, 3, RSVcases, babies = num_infants, mort = mort_mat))

# sum the rows to get total number of cases by trial simulation
cases_no_u <- rowSums(cases_no_u_age)
cases_llAb_u <- rowSums(cases_llAb_u_age)
cases_mVax_u <- rowSums(cases_mVax_u_age)
cases_pVax_u <- rowSums(cases_pVax_u_age)
cases_pVax_u_SA <- rowSums(cases_pVax_u_SA_age)
cases_joint_llAb_pVax_u <- rowSums(cases_joint_llAb_pVax_u_age)
cases_joint_mVax_pVax_u <- rowSums(cases_joint_mVax_pVax_u_age)
cases_pVax_older_u <- rowSums(cases_pVax_older_u_age)
cases_joint_llAb_pVax_older_u <- rowSums(cases_joint_llAb_pVax_older_u_age)
cases_joint_mVax_pVax_older_u <- rowSums(cases_joint_mVax_pVax_older_u_age)

# hospitalizations with uncertainty by age
inpat_no_u_age <- t(inpat_func(p_hosp_new, pneum_func(p_pneum_u, t(cases_no_u_age))))
inpat_llAb_u_age <- t(inpat_func(p_hosp_new, pneum_func(p_pneum_u, t(cases_llAb_u_age))))
inpat_mVax_u_age <- t(inpat_func(p_hosp_new, pneum_func(p_pneum_u, t(cases_mVax_u_age))))
inpat_pVax_u_age <- t(inpat_func(p_hosp_new, pneum_func(p_pneum_u, t(cases_pVax_u_age))))
inpat_pVax_u_SA_age <- t(inpat_func(p_hosp_new, pneum_func(p_pneum_u, t(cases_pVax_u_SA_age))))
inpat_joint_llAb_pVax_u_age <- t(inpat_func(p_hosp_new, pneum_func(p_pneum_u, t(cases_joint_llAb_pVax_u_age))))
inpat_joint_mVax_pVax_u_age <- t(inpat_func(p_hosp_new, pneum_func(p_pneum_u, t(cases_joint_mVax_pVax_u_age))))
inpat_pVax_older_u_age <- t(inpat_func(p_hosp_new, pneum_func(p_pneum_u, t(cases_pVax_older_u_age))))
inpat_joint_llAb_pVax_older_u_age <- t(inpat_func(p_hosp_new, pneum_func(p_pneum_u, t(cases_joint_llAb_pVax_older_u_age))))
inpat_joint_mVax_pVax_older_u_age <- t(inpat_func(p_hosp_new, pneum_func(p_pneum_u, t(cases_joint_mVax_pVax_older_u_age))))

# total hospitalizations with uncertainty
inpat_no_u <- apply(inpat_no_u_age, MARGIN = 1, sum)
inpat_llAb_u <- apply(inpat_llAb_u_age, MARGIN = 1, sum)
inpat_mVax_u <- apply(inpat_mVax_u_age, MARGIN = 1, sum)
inpat_pVax_u <- apply(inpat_pVax_u_age, MARGIN = 1, sum)
inpat_pVax_u_SA <- apply(inpat_pVax_u_SA_age, MARGIN = 1, sum)
inpat_joint_llAb_pVax_u <- apply(inpat_joint_llAb_pVax_u_age, MARGIN = 1, sum)
inpat_joint_mVax_pVax_u <- apply(inpat_joint_mVax_pVax_u_age, MARGIN = 1, sum)
inpat_pVax_older_u <- apply(inpat_pVax_older_u_age, MARGIN = 1, sum)
inpat_joint_llAb_pVax_older_u <- apply(inpat_joint_llAb_pVax_older_u_age, MARGIN = 1, sum)
inpat_joint_mVax_pVax_older_u <- apply(inpat_joint_mVax_pVax_older_u_age, MARGIN = 1, sum)

# Calculate number of deaths with uncertainty, age-weighted hospitalization rates AND age-weighted CFRs
## NOTE: need to incorporate uncertainty for hospitalizations by age
deaths_no_u_age <- mort_inpat_func(CFR_by_age_u, inpat_no_u_age, CFR_nr_care_u, nr_care_func(inpat_no_u_age))
deaths_llAb_u_age <- mort_inpat_func(CFR_by_age_u, inpat_llAb_u_age, CFR_nr_care_u, nr_care_func(inpat_llAb_u_age))
deaths_mVax_u_age <- mort_inpat_func(CFR_by_age_u, inpat_mVax_u_age, CFR_nr_care_u, nr_care_func(inpat_mVax_u_age))
deaths_pVax_u_age <- mort_inpat_func(CFR_by_age_u, inpat_pVax_u_age, CFR_nr_care_u, nr_care_func(inpat_pVax_u_age))
deaths_pVax_u_SA_age <- mort_inpat_func(CFR_by_age_u, inpat_pVax_u_age, CFR_nr_care_u, nr_care_func(inpat_pVax_u_SA_age))
deaths_joint_llAb_pVax_u_age <-mort_inpat_func(CFR_by_age_u, inpat_joint_llAb_pVax_u_age, CFR_nr_care_u, nr_care_func(inpat_joint_llAb_pVax_u_age))
deaths_joint_mVax_pVax_u_age <- mort_inpat_func(CFR_by_age_u, inpat_joint_mVax_pVax_u_age, CFR_nr_care_u, nr_care_func(inpat_joint_mVax_pVax_u_age))
deaths_pVax_older_u_age <- mort_inpat_func(CFR_by_age_u, inpat_pVax_older_u_age, CFR_nr_care_u, nr_care_func(inpat_pVax_older_u_age))
deaths_joint_llAb_pVax_older_u_age <- mort_inpat_func(CFR_by_age_u, inpat_joint_llAb_pVax_older_u_age, CFR_nr_care_u, nr_care_func(inpat_joint_llAb_pVax_older_u_age))
deaths_joint_mVax_pVax_older_u_age <-mort_inpat_func(CFR_by_age_u, inpat_joint_mVax_pVax_older_u_age, CFR_nr_care_u, nr_care_func(inpat_joint_mVax_pVax_older_u_age))

# sum the rows
deaths_no_u <- apply(deaths_no_u_age, MARGIN = 1, sum)
deaths_llAb_u <- apply(deaths_llAb_u_age, MARGIN = 1, sum)
deaths_mVax_u <- apply(deaths_mVax_u_age, MARGIN = 1, sum)
deaths_pVax_u <- apply(deaths_pVax_u_age, MARGIN = 1, sum)
deaths_pVax_u_SA <- apply(deaths_pVax_u_SA_age, MARGIN = 1, sum)
deaths_joint_llAb_pVax_u <- apply(deaths_joint_llAb_pVax_u_age, MARGIN = 1, sum)
deaths_joint_mVax_pVax_u <- apply(deaths_joint_mVax_pVax_u_age, MARGIN = 1, sum)
deaths_pVax_older_u <- apply(deaths_pVax_older_u_age, MARGIN = 1, sum)
deaths_joint_llAb_pVax_older_u <- apply(deaths_joint_llAb_pVax_older_u_age, MARGIN = 1, sum)
deaths_joint_mVax_pVax_older_u <- apply(deaths_joint_mVax_pVax_older_u_age, MARGIN = 1, sum)

# Calculate DALYs lost with uncertainty
DALYS_lost_no_u <- YLL_func(deaths_no_u) + YLD_func(inpat_no_u, deaths_no_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_no_u), dw_LRTI_mod_u)
DALYS_lost_llAb_u <- YLL_func(deaths_llAb_u) + YLD_func(inpat_llAb_u, deaths_llAb_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_llAb_u), dw_LRTI_mod_u)
DALYS_lost_mVax_u <- YLL_func(deaths_mVax_u) + YLD_func(inpat_mVax_u, deaths_mVax_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_mVax_u), dw_LRTI_mod_u)
DALYS_lost_pVax_u <- YLL_func(deaths_pVax_u) + YLD_func(inpat_pVax_u, deaths_pVax_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_pVax_u), dw_LRTI_mod_u)
DALYS_lost_pVax_u_SA <- YLL_func(deaths_pVax_u_SA) + YLD_func(inpat_pVax_u_SA, deaths_pVax_u_SA, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_pVax_u_SA), dw_LRTI_mod_u)
DALYS_lost_joint_llAb_pVax_u <- YLL_func(deaths_joint_llAb_pVax_u) + YLD_func(inpat_joint_llAb_pVax_u, deaths_joint_llAb_pVax_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_u), dw_LRTI_mod_u)
DALYS_lost_joint_mVax_pVax_u <- YLL_func(deaths_joint_mVax_pVax_u) + YLD_func(inpat_joint_mVax_pVax_u, deaths_joint_mVax_pVax_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_u), dw_LRTI_mod_u)
DALYS_lost_pVax_older_u <- YLL_func(deaths_pVax_older_u) + YLD_func(inpat_pVax_older_u, deaths_pVax_older_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_pVax_older_u), dw_LRTI_mod_u)
DALYS_lost_joint_llAb_pVax_older_u <- YLL_func(deaths_joint_llAb_pVax_older_u) + YLD_func(inpat_joint_llAb_pVax_older_u, deaths_joint_llAb_pVax_older_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_older_u), dw_LRTI_mod_u)
DALYS_lost_joint_mVax_pVax_older_u <- YLL_func(deaths_joint_mVax_pVax_older_u) + YLD_func(inpat_joint_mVax_pVax_older_u, deaths_joint_mVax_pVax_older_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_older_u), dw_LRTI_mod_u)

# Calculate medical costs with uncertainty
medcost_no_u <- medcost_func(cost_hosp_u, inpat_no_u, cost_outpatient_u, outpat_func(pneum_func(p_pneum_u, cases_no_u), inpat_no_u))
medcost_llAb_u <- medcost_func(cost_hosp_u, inpat_llAb_u, cost_outpatient_u, outpat_func(pneum_func(p_pneum_u, cases_llAb_u), inpat_llAb_u))
medcost_mVax_u <- medcost_func(cost_hosp_u, inpat_mVax_u, cost_outpatient_u, outpat_func(pneum_func(p_pneum_u, cases_mVax_u), inpat_mVax_u))
medcost_pVax_u <- medcost_func(cost_hosp_u, inpat_pVax_u, cost_outpatient_u, outpat_func( pneum_func(p_pneum_u, cases_pVax_u), inpat_pVax_u))
medcost_pVax_u_SA <- medcost_func(cost_hosp_u, inpat_pVax_u_SA, cost_outpatient_u, outpat_func(pneum_func(p_pneum_u, cases_pVax_u_SA), inpat_pVax_u_SA))
medcost_joint_llAb_pVax_u <- medcost_func(cost_hosp_u, inpat_joint_llAb_pVax_u, cost_outpatient_u, outpat_func(pneum_func(p_pneum_u, cases_joint_llAb_pVax_u), inpat_joint_llAb_pVax_u))
medcost_joint_mVax_pVax_u <- medcost_func(cost_hosp_u, inpat_joint_mVax_pVax_u, cost_outpatient_u, outpat_func(pneum_func(p_pneum_u, cases_joint_mVax_pVax_u), inpat_joint_mVax_pVax_u))
medcost_pVax_u_older <- medcost_func(cost_hosp_u, inpat_pVax_older_u, cost_outpatient_u, outpat_func(pneum_func(p_pneum_u, cases_pVax_older_u), inpat_pVax_older_u))
medcost_joint_llAb_pVax_u_older <- medcost_func(cost_hosp_u, inpat_joint_llAb_pVax_older_u, cost_outpatient_u, outpat_func(pneum_func(p_pneum_u, cases_joint_llAb_pVax_older_u), inpat_joint_llAb_pVax_older_u))
medcost_joint_mVax_pVax_u_older <- medcost_func(cost_hosp_u, inpat_joint_mVax_pVax_older_u, cost_outpatient_u, outpat_func(pneum_func(p_pneum_u, cases_joint_mVax_pVax_older_u), inpat_joint_mVax_pVax_older_u))

# Calculate total intervention costs with uncertainty
totalcost_no_u <- medcost_no_u
totalcost_llAb_u <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + medcost_llAb_u
totalcost_mVax_u <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + medcost_mVax_u
totalcost_pVax_u <- sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_pVax_u
totalcost_pVax_u_SA <- sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_pVax_u_SA
totalcost_joint_llAb_pVax_u <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_joint_llAb_pVax_u
totalcost_joint_mVax_pVax_u <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_joint_mVax_pVax_u
totalcost_pVax_older_u <- sum(pVax_older_admin * cov_pVax_o * num_infants * (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_pVax_u_older
totalcost_joint_llAb_pVax_older_u <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * cov_pVax_o * num_infants * (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_joint_llAb_pVax_u_older
totalcost_joint_mVax_pVax_older_u <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * cov_pVax_o * num_infants * (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_joint_mVax_pVax_u_older

# calculate probability of being cost-effective across WTP

prep_llAb <- cbind(DALYS_lost_no_u, DALYS_lost_llAb_u, totalcost_llAb_u, totalcost_no_u)
pce_llAb <- rep(0, length(WTP_sp))
NHB_l <- matrix(0, trials, length(WTP_sp))
for (l in 1: length(WTP_sp)){
  NHB_l[,l] <- NHB_func(prep_llAb, WTP_sp[l])
  pce_llAb[l] <- sum(NHB_l[,l] >0)/trials
}

prep_mVax <- cbind(DALYS_lost_no_u, DALYS_lost_mVax_u, totalcost_mVax_u, totalcost_no_u)
pce_mVax <- rep(0, length(WTP_sp))
NHB_m <- matrix(0, trials, length(WTP_sp))
for (m in 1: length(WTP_sp)){
  NHB_m[,m] <- NHB_func(prep_mVax, WTP_sp[m])
  pce_mVax[m] <- sum(NHB_m[,m] >0)/trials
}

prep_pVax <- cbind(DALYS_lost_no_u, DALYS_lost_pVax_u_SA, totalcost_pVax_u_SA, totalcost_no_u)
pce_pVax <- rep(0, length(WTP_sp))
NHB_p <- matrix(0, trials, length(WTP_sp))
for (p in 1: length(WTP_sp)){
  NHB_p[,p] <- NHB_func(prep_pVax, WTP_sp[p])
  pce_pVax[p] <- sum(NHB_p[,p] >0)/trials
}

prep_joint_llAb_pVax <- cbind(DALYS_lost_no_u, DALYS_lost_joint_llAb_pVax_u, totalcost_joint_llAb_pVax_u, totalcost_no_u)
pce_joint_llAb_pVax <- rep(0, length(WTP_sp))
NHB_lp <- matrix(0, trials, length(WTP_sp))
for (lp in 1: length(WTP_sp)){
  NHB_lp[,lp] <- NHB_func(prep_joint_llAb_pVax, WTP_sp[lp])
  pce_joint_llAb_pVax[lp] <- sum(NHB_lp[,lp] >0)/trials
}

prep_joint_mVax_pVax <- cbind(DALYS_lost_no_u, DALYS_lost_joint_mVax_pVax_u, totalcost_joint_mVax_pVax_u, totalcost_no_u)
pce_joint_mVax_pVax <- rep(0, length(WTP_sp))
NHB_mp <- matrix(0, trials, length(WTP_sp))
for (mp in 1: length(WTP_sp)){
  NHB_mp[,mp] <- NHB_func(prep_joint_mVax_pVax, WTP_sp[mp])
  pce_joint_mVax_pVax[mp] <- sum(NHB_mp[,mp] >0)/trials
}

prep_pVax_older <- cbind(DALYS_lost_no_u, DALYS_lost_pVax_older_u, totalcost_pVax_older_u, totalcost_no_u)
pce_pVax_older <- rep(0, length(WTP_sp))
NHB_p_older <- matrix(0, trials, length(WTP_sp))
for (po in 1: length(WTP_sp)){
  NHB_p_older[,po] <- NHB_func(prep_pVax_older, WTP_sp[po])
  pce_pVax_older[po] <- sum(NHB_p_older[,po] >0)/trials
}

prep_joint_llAb_pVax_older <- cbind(DALYS_lost_no_u, DALYS_lost_joint_llAb_pVax_older_u, totalcost_joint_llAb_pVax_older_u, totalcost_no_u)
pce_joint_llAb_pVax_older <- rep(0, length(WTP_sp))
NHB_lp_older <- matrix(0, trials, length(WTP_sp))
for (lpo in 1: length(WTP_sp)){
  NHB_lp_older[,lpo] <- NHB_func(prep_joint_llAb_pVax_older, WTP_sp[lpo])
  pce_joint_llAb_pVax_older[lpo] <- sum(NHB_lp_older[,lpo] >0)/trials
}

prep_joint_mVax_pVax_older <- cbind(DALYS_lost_no_u, DALYS_lost_joint_mVax_pVax_older_u, totalcost_joint_mVax_pVax_older_u, totalcost_no_u)
pce_joint_mVax_pVax_older <- rep(0, length(WTP_sp))
NHB_mp_older <- matrix(0, trials, length(WTP_sp))
for (mpo in 1: length(WTP_sp)){
  NHB_mp_older[,mpo] <- NHB_func(prep_joint_mVax_pVax_older, WTP_sp[mpo])
  pce_joint_mVax_pVax_older[mpo] <- sum(NHB_mp_older[,mpo] >0)/trials
}

## Calculate probability of each strategy being optimal across WTP

# calculate NHB for no intervention at all
prep_no <- cbind(DALYS_lost_no_u, DALYS_lost_no_u, totalcost_no_u, totalcost_no_u)
NHB_no <- matrix(0, trials, length(WTP_sp))
for (no in 1: length(WTP_sp)){
  NHB_no[,no] <- NHB_func(prep_no, WTP_sp[no])}

# compare NHB across all strategies
compare_NHB <- array(c(NHB_no, NHB_l, NHB_m, NHB_p, NHB_lp, NHB_mp, NHB_p_older, NHB_lp_older, NHB_mp_older), dim = c(trials, length(WTP_sp), 9))
win_NHB <- apply(compare_NHB, MARGIN = c(1,2), which.max)

pO_no <- rep(0, length(WTP_sp))
for(no in 1: length(WTP_sp)){
  pO_no[no] <- sum(win_NHB[,no] == 1)/trials
}

pO_llAb <- rep(0, length(WTP_sp))
for(Ol in 1: length(WTP_sp)){
  pO_llAb[Ol] <- sum(win_NHB[,Ol] == 2)/trials
}

pO_mVax <- rep(0, length(WTP_sp))
for(Om in 1: length(WTP_sp)){
  pO_mVax[Om] <- sum(win_NHB[,Om] == 3)/trials
}

pO_pVax <- rep(0, length(WTP_sp))
for(Op in 1: length(WTP_sp)){
  pO_pVax[Op] <- sum(win_NHB[,Op] == 4)/trials
}

pO_llAb_pVax <- rep(0, length(WTP_sp))
for(Olp in 1: length(WTP_sp)){
  pO_llAb_pVax[Olp] <- sum(win_NHB[,Olp] == 5)/trials
}

pO_mVax_pVax <- rep(0, length(WTP_sp))
for(Omp in 1: length(WTP_sp)){
  pO_mVax_pVax[Omp] <- sum(win_NHB[,Omp] == 6)/trials
}

pO_pVax_older <- rep(0, length(WTP_sp))
for(Opo in 1: length(WTP_sp)){
  pO_pVax_older[Opo] <- sum(win_NHB[,Opo] == 7)/trials
}

pO_llAb_pVax_older <- rep(0, length(WTP_sp))
for(Olpo in 1: length(WTP_sp)){
  pO_llAb_pVax_older[Olpo] <- sum(win_NHB[,Olpo] == 8)/trials
}

pO_mVax_pVax_older <- rep(0, length(WTP_sp))
for(Ompo in 1: length(WTP_sp)){
  pO_mVax_pVax_older[Ompo] <- sum(win_NHB[,Ompo] == 9)/trials
}

######