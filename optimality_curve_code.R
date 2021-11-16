
# Code to produce plot for probability each intervention is optimal by WTP

# Calculate number of RSV cases w/ uncertainty across birth cohorts
cases_no_u_bic <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (x in 1:trials){
  cases_no_u_bic[,,x] <- bic_cases(AR_y_u[,,x], babies = num_infants, mort = mort_mat)
}

# by age
cases_no_u_age <- t(apply(AR_y_u, 3, RSVcases, babies = num_infants, mort = mort_mat))
# sum the rows to get total number of cases by trial simulation
cases_no_u <- rowSums(cases_no_u_age)

# LRTI with uncertainty by age

LRTI_no_u_bic <- LRTI_func(0, 0, 1, p_pneum_u, cases_no_u_bic)

LRTI_llAb_u_bic <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (y in 1: trials){
  LRTI_llAb_u_bic[,,y] <- LRTI_func(efficacy[1], coverage[1], mat_eff_llAb, p_pneum_u[y], cases_no_u_bic[,,y])
}
  
LRTI_mVax_u_bic <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (y in 1: trials){
  LRTI_mVax_u_bic[,,y] <- LRTI_func(efficacy[2], coverage[2], mat_eff_mVax, p_pneum_u[y], cases_no_u_bic[,,y])
}

LRTI_pVax_u_bic <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (y in 1: trials){
  LRTI_pVax_u_bic[,,y] <- LRTI_func(efficacy[3], coverage[3], mat_eff_pVax, p_pneum_u[y], cases_no_u_bic[,,y])
}

LRTI_joint_llAb_pVax_u_bic <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (y in 1: trials){
  LRTI_joint_llAb_pVax_u_bic[,,y] <- LRTI_func_joint(efficacy[1], efficacy[3], coverage[1], coverage[3], mat_eff_llAb, mat_eff_pVax, p_pneum_u[y], cases_no_u_bic[,,y])
}

LRTI_joint_mVax_pVax_u_bic <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (y in 1: trials){
  LRTI_joint_mVax_pVax_u_bic[,,y] <- LRTI_func_joint(efficacy[2], efficacy[3], coverage[2], coverage[3], mat_eff_mVax, mat_eff_pVax, p_pneum_u[y], cases_no_u_bic[,,y])
}

LRTI_pVax_older_u_bic <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (y in 1:trials){
  LRTI_pVax_older_u_bic[,,y] <- LRTI_func(efficacy[3], cov_pVax_o, mat_eff_older_pVax, p_pneum_u[y], cases_no_u_bic[,,y])
}

LRTI_joint_llAb_pVax_older_u_bic <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for(y in 1:trials){
  LRTI_joint_llAb_pVax_older_u_bic[,,y] <- LRTI_func_joint(efficacy[1], efficacy[3], coverage[1], cov_pVax_o, mat_eff_llAb, mat_eff_older_pVax, p_pneum_u[y], cases_no_u_bic[,,y])
}

LRTI_joint_mVax_pVax_older_u_bic <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for(y in 1:trials){
  LRTI_joint_mVax_pVax_older_u_bic[,,y] <- LRTI_func_joint(efficacy[2], efficacy[3], coverage[2], cov_pVax_o, mat_eff_mVax, mat_eff_older_pVax, p_pneum_u[y], cases_no_u_bic[,,y])
}

LRTI_no_u_age <- t(apply(LRTI_no_u_bic, 3, adj_func))
LRTI_llAb_u_age <- t(apply(LRTI_llAb_u_bic, 3, adj_func))
LRTI_mVax_u_age <- t(apply(LRTI_mVax_u_bic, 3, adj_func))
LRTI_pVax_u_age <- t(apply(LRTI_pVax_u_bic, 3, adj_func))
LRTI_joint_llAb_pVax_u_age <-  t(apply(LRTI_joint_llAb_pVax_u_bic, 3, adj_func))
LRTI_joint_mVax_pVax_u_age <-  t(apply(LRTI_joint_mVax_pVax_u_bic, 3, adj_func))
LRTI_pVax_older_u_age <-  t(apply(LRTI_pVax_older_u_bic, 3, adj_func))
LRTI_joint_llAb_pVax_older_u_age <-  t(apply(LRTI_joint_llAb_pVax_older_u_bic, 3, adj_func))
LRTI_joint_mVax_pVax_older_u_age <-  t(apply(LRTI_joint_mVax_pVax_older_u_bic, 3, adj_func))

# Sum the rows to determine total LRTI episodes
# in the birth cohort through 3 years, with uncertainty
LRTI_no_u <- rowSums(LRTI_no_u_age)
LRTI_llAb_u <-rowSums(LRTI_llAb_u_age)
LRTI_mVax_u <- rowSums(LRTI_mVax_u_age)
LRTI_pVax_u<- rowSums(LRTI_pVax_u_age)
LRTI_joint_llAb_pVax_u <-  rowSums(LRTI_joint_llAb_pVax_u_age)
LRTI_joint_mVax_pVax_u <-  rowSums(LRTI_joint_mVax_pVax_u_age)
LRTI_pVax_older_u <-  rowSums(LRTI_pVax_older_u_age)
LRTI_joint_llAb_pVax_older_u <- rowSums(LRTI_joint_llAb_pVax_older_u_age)
LRTI_joint_mVax_pVax_older_u <-  rowSums(LRTI_joint_mVax_pVax_older_u_age)

# NOTE: still need to add in additional uncertainty from hospitalizations by age
# hospitalizations with uncertainty by age
inpat_no_u_age <- t(inpat_func(p_hosp_new, t(LRTI_no_u_age)))
inpat_llAb_u_age <- t(inpat_func(p_hosp_new, t(LRTI_llAb_u_age)))
inpat_mVax_u_age <- t(inpat_func(p_hosp_new, t(LRTI_mVax_u_age)))
inpat_pVax_u_age <- t(inpat_func(p_hosp_new,  t(LRTI_pVax_u_age)))
inpat_joint_llAb_pVax_u_age <- t(inpat_func(p_hosp_new,  t(LRTI_joint_llAb_pVax_u_age)))
inpat_joint_mVax_pVax_u_age <- t(inpat_func(p_hosp_new, t(LRTI_joint_mVax_pVax_u_age)))
inpat_pVax_older_u_age <- t(inpat_func(p_hosp_new, t(LRTI_pVax_older_u_age)))
inpat_joint_llAb_pVax_older_u_age <- t(inpat_func(p_hosp_new, t(LRTI_joint_llAb_pVax_older_u_age)))
inpat_joint_mVax_pVax_older_u_age <- t(inpat_func(p_hosp_new, t(LRTI_joint_mVax_pVax_older_u_age)))

# total hospitalizations with uncertainty
inpat_no_u <- apply(inpat_no_u_age, MARGIN = 1, sum)
inpat_llAb_u <- apply(inpat_llAb_u_age, MARGIN = 1, sum)
inpat_mVax_u <- apply(inpat_mVax_u_age, MARGIN = 1, sum)
inpat_pVax_u <- apply(inpat_pVax_u_age, MARGIN = 1, sum)
inpat_joint_llAb_pVax_u <- apply(inpat_joint_llAb_pVax_u_age, MARGIN = 1, sum)
inpat_joint_mVax_pVax_u <- apply(inpat_joint_mVax_pVax_u_age, MARGIN = 1, sum)
inpat_pVax_older_u <- apply(inpat_pVax_older_u_age, MARGIN = 1, sum)
inpat_joint_llAb_pVax_older_u <- apply(inpat_joint_llAb_pVax_older_u_age, MARGIN = 1, sum)
inpat_joint_mVax_pVax_older_u <- apply(inpat_joint_mVax_pVax_older_u_age, MARGIN = 1, sum)

# Calculate number of deaths with uncertainty, age-weighted hospitalization rates AND age-weighted CFRs
deaths_no_u_age <- mort_inpat_func(CFR_by_age_u, inpat_no_u_age, CFR_nr_care_u, nr_care_func(inpat_no_u_age))
deaths_llAb_u_age <- mort_inpat_func(CFR_by_age_u, inpat_llAb_u_age, CFR_nr_care_u, nr_care_func(inpat_llAb_u_age))
deaths_mVax_u_age <- mort_inpat_func(CFR_by_age_u, inpat_mVax_u_age, CFR_nr_care_u, nr_care_func(inpat_mVax_u_age))
deaths_pVax_u_age <- mort_inpat_func(CFR_by_age_u, inpat_pVax_u_age, CFR_nr_care_u, nr_care_func(inpat_pVax_u_age))
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
deaths_joint_llAb_pVax_u <- apply(deaths_joint_llAb_pVax_u_age, MARGIN = 1, sum)
deaths_joint_mVax_pVax_u <- apply(deaths_joint_mVax_pVax_u_age, MARGIN = 1, sum)
deaths_pVax_older_u <- apply(deaths_pVax_older_u_age, MARGIN = 1, sum)
deaths_joint_llAb_pVax_older_u <- apply(deaths_joint_llAb_pVax_older_u_age, MARGIN = 1, sum)
deaths_joint_mVax_pVax_older_u <- apply(deaths_joint_mVax_pVax_older_u_age, MARGIN = 1, sum)

# Calculate DALYs lost with uncertainty
DALYS_lost_no_u <- YLL_func(deaths_no_u) + YLD_func(inpat_no_u, deaths_no_u, di_yrs_u, dw_LRTI_severe_u, LRTI_no_u, dw_LRTI_mod_u)
DALYS_lost_llAb_u <- YLL_func(deaths_llAb_u) + YLD_func(inpat_llAb_u, deaths_llAb_u, di_yrs_u, dw_LRTI_severe_u, LRTI_llAb_u, dw_LRTI_mod_u)
DALYS_lost_mVax_u <- YLL_func(deaths_mVax_u) + YLD_func(inpat_mVax_u, deaths_mVax_u, di_yrs_u, dw_LRTI_severe_u, LRTI_mVax_u, dw_LRTI_mod_u)
DALYS_lost_pVax_u <- YLL_func(deaths_pVax_u) + YLD_func(inpat_pVax_u, deaths_pVax_u, di_yrs_u, dw_LRTI_severe_u, LRTI_pVax_u, dw_LRTI_mod_u)
DALYS_lost_joint_llAb_pVax_u <- YLL_func(deaths_joint_llAb_pVax_u) + YLD_func(inpat_joint_llAb_pVax_u, deaths_joint_llAb_pVax_u, di_yrs_u, dw_LRTI_severe_u, LRTI_joint_llAb_pVax_u, dw_LRTI_mod_u)
DALYS_lost_joint_mVax_pVax_u <- YLL_func(deaths_joint_mVax_pVax_u) + YLD_func(inpat_joint_mVax_pVax_u, deaths_joint_mVax_pVax_u, di_yrs_u, dw_LRTI_severe_u, LRTI_joint_mVax_pVax_u, dw_LRTI_mod_u)
DALYS_lost_pVax_older_u <- YLL_func(deaths_pVax_older_u) + YLD_func(inpat_pVax_older_u, deaths_pVax_older_u, di_yrs_u, dw_LRTI_severe_u, LRTI_pVax_older_u, dw_LRTI_mod_u)
DALYS_lost_joint_llAb_pVax_older_u <- YLL_func(deaths_joint_llAb_pVax_older_u) + YLD_func(inpat_joint_llAb_pVax_older_u, deaths_joint_llAb_pVax_older_u, di_yrs_u, dw_LRTI_severe_u, LRTI_joint_llAb_pVax_older_u, dw_LRTI_mod_u)
DALYS_lost_joint_mVax_pVax_older_u <- YLL_func(deaths_joint_mVax_pVax_older_u) + YLD_func(inpat_joint_mVax_pVax_older_u, deaths_joint_mVax_pVax_older_u, di_yrs_u, dw_LRTI_severe_u, LRTI_joint_mVax_pVax_older_u, dw_LRTI_mod_u)

# Calculate medical costs with uncertainty
medcost_no_u <- medcost_func(cost_hosp_u, inpat_no_u, cost_outpatient_u, outpat_func(LRTI_no_u, inpat_no_u))
medcost_llAb_u <- medcost_func(cost_hosp_u, inpat_llAb_u, cost_outpatient_u, outpat_func(LRTI_llAb_u, inpat_llAb_u))
medcost_mVax_u <- medcost_func(cost_hosp_u, inpat_mVax_u, cost_outpatient_u, outpat_func(LRTI_mVax_u, inpat_mVax_u))
medcost_pVax_u <- medcost_func(cost_hosp_u, inpat_pVax_u, cost_outpatient_u, outpat_func(LRTI_pVax_u, inpat_pVax_u))
medcost_joint_llAb_pVax_u <- medcost_func(cost_hosp_u, inpat_joint_llAb_pVax_u, cost_outpatient_u, outpat_func(LRTI_joint_llAb_pVax_u, inpat_joint_llAb_pVax_u))
medcost_joint_mVax_pVax_u <- medcost_func(cost_hosp_u, inpat_joint_mVax_pVax_u, cost_outpatient_u, outpat_func(LRTI_joint_mVax_pVax_u, inpat_joint_mVax_pVax_u))
medcost_pVax_u_older <- medcost_func(cost_hosp_u, inpat_pVax_older_u, cost_outpatient_u, outpat_func(LRTI_pVax_older_u, inpat_pVax_older_u))
medcost_joint_llAb_pVax_u_older <- medcost_func(cost_hosp_u, inpat_joint_llAb_pVax_older_u, cost_outpatient_u, outpat_func(LRTI_joint_llAb_pVax_older_u, inpat_joint_llAb_pVax_older_u))
medcost_joint_mVax_pVax_u_older <- medcost_func(cost_hosp_u, inpat_joint_mVax_pVax_older_u, cost_outpatient_u, outpat_func(LRTI_joint_mVax_pVax_older_u, inpat_joint_mVax_pVax_older_u))

# Calculate total intervention costs with uncertainty
totalcost_no_u <- medcost_no_u
totalcost_llAb_u <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + medcost_llAb_u
totalcost_mVax_u <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + medcost_mVax_u
totalcost_pVax_u <- sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_pVax_u
totalcost_joint_llAb_pVax_u <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_joint_llAb_pVax_u
totalcost_joint_mVax_pVax_u <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_joint_mVax_pVax_u
totalcost_pVax_older_u <- sum(pVax_older_admin * cov_pVax_o * num_infants * (0.5*cost_EPI + 0.5*cost_nd + cost_prod)) + medcost_pVax_u_older
totalcost_joint_llAb_pVax_older_u <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * cov_pVax_o * num_infants * (0.5*cost_EPI + 0.5*cost_nd + cost_prod)) + medcost_joint_llAb_pVax_u_older
totalcost_joint_mVax_pVax_older_u <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * cov_pVax_o * num_infants * (0.5*cost_EPI + 0.5*cost_nd + cost_prod)) + medcost_joint_mVax_pVax_u_older

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

prep_pVax <- cbind(DALYS_lost_no_u, DALYS_lost_pVax_u, totalcost_pVax_u, totalcost_no_u)
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

# Calculate total intervention costs across change in price of the product
# pspan
# generate a matrix where rows = trials, columns = costs
cprod <- seq(.01, 4, by = 0.01)

tcost_no_pspan <- rep.col(medcost_no_u, length(cprod))

tcost_llAb_pspan <- matrix(data = NA, nrow = trials, ncol = length(cprod))
for (l in 1:length(cprod)) {
  tcost_llAb_pspan[,l] <- sum(llAb_admin * coverage[1] * num_infants * (cprod[l] + cost_nd)) + medcost_llAb_u
}

tcost_mVax_pspan <- matrix(data = NA, nrow = trials, ncol = length(cprod))
for (m in 1:length(cprod)) {
  tcost_mVax_pspan[,m] <- sum(mVax_admin * coverage[2] * num_infants * (cprod[m] + cost_nd)) + medcost_mVax_u
}

tcost_pVax_pspan <- matrix(data = NA, nrow = trials, ncol = length(cprod))
for (p in 1:length(cprod)) {
  tcost_pVax_pspan[,p] <- sum(pVax_admin * coverage[3] * num_infants * (cprod[p] + cost_nd)) + medcost_pVax_u
}

tcost_llAb_pVax_pspan <- matrix(data = NA, nrow = trials, ncol = length(cprod))
for(lp in 1:length(cprod)) {
  tcost_llAb_pVax_pspan[,lp] <- sum(llAb_admin * coverage[1] * num_infants * (cprod[lp] + cost_nd)) +
    sum(pVax_admin * coverage[3] * num_infants * (cprod[lp] + cost_nd)) + medcost_joint_llAb_pVax_u
}

tcost_mVax_pVax_pspan <- matrix(data = NA, nrow = trials, ncol = length(cprod))
for(mp in 1:length(cprod)) {
  tcost_mVax_pVax_pspan[,mp] <- sum(mVax_admin * coverage[2] * num_infants * (cprod[mp] + cost_nd)) +
    sum(pVax_admin * coverage[3] * num_infants * (cprod[mp] + cost_nd)) + medcost_joint_mVax_pVax_u
}

tcost_pVax_older_pspan <- matrix(data = NA, nrow = trials, ncol = length(cprod))
for(po in 1:length(cprod)){
  tcost_pVax_older_pspan[,po] <- sum(pVax_older_admin * cov_pVax_o * num_infants * (0.5*cost_EPI + 0.5*cost_nd + cprod[po])) + medcost_pVax_u_older
}

tcost_llAb_pVax_older_pspan <- matrix(data = NA, nrow = trials, ncol = length(cprod))
for(lpo in 1:length(cprod)) {
  tcost_llAb_pVax_older_pspan[,lpo] <- sum(llAb_admin * coverage[1] * num_infants * (cprod[lpo] + cost_nd)) +
    sum(pVax_admin * cov_pVax_o * num_infants * (0.5*cost_EPI + 0.5*cost_nd + cprod[lpo])) +
    medcost_joint_llAb_pVax_u_older
}

tcost_mVax_pVax_older_pspan <- matrix(data = NA, nrow = trials, ncol = length(cprod))
for(mpo in 1:length(cprod)){
  tcost_mVax_pVax_older_pspan[,mpo] <- sum(mVax_admin * coverage[2] * num_infants * (cprod[mpo] + cost_nd)) +
    sum(pVax_admin * cov_pVax_o * num_infants * (0.5*cost_EPI + 0.5*cost_nd + cprod[mpo])) +
    medcost_joint_mVax_pVax_u_older
}

# calculate NHB across changing product cost from $0-3 and WTP = 1XGDP
NHB_no_pspan <- matrix(NA, trials, length(cprod))
for(no in 1:length(cprod)){
  NHB_no_pspan[,no] <- (DALYS_lost_no_u - DALYS_lost_no_u) -
    (tcost_no_pspan[,no] - tcost_no_pspan[,no]) / CET_Mali_GDP
}

NHB_llAb_pspan <- matrix(NA, trials, length(cprod))
for(l in 1:length(cprod)){
  NHB_llAb_pspan[,l] <- (DALYS_lost_no_u - DALYS_lost_llAb_u) -
    (tcost_llAb_pspan[,l] - tcost_no_pspan[,no]) / CET_Mali_GDP
}

NHB_mVax_pspan <- matrix(NA, trials, length(cprod))
for(m in 1:length(cprod)){
  NHB_mVax_pspan[,m] <- (DALYS_lost_no_u - DALYS_lost_mVax_u) -
    (tcost_mVax_pspan[,m] - tcost_no_pspan[,no]) / CET_Mali_GDP
}

NHB_pVax_pspan <- matrix(NA, trials, length(cprod))
for(p in 1:length(cprod)){
  NHB_pVax_pspan[,p] <- (DALYS_lost_no_u - DALYS_lost_pVax_u) -
    (tcost_pVax_pspan[,p] - tcost_no_pspan[,no]) / CET_Mali_GDP
}

NHB_llAb_pVax_pspan <- matrix(NA, trials, length(cprod))
for(lp in 1:length(cprod)){
  NHB_llAb_pVax_pspan[,lp] <- (DALYS_lost_no_u - DALYS_lost_joint_llAb_pVax_u) -
    (tcost_llAb_pVax_pspan[,lp] - tcost_no_pspan[,lp]) / CET_Mali_GDP
}

NHB_mVax_pVax_pspan <- matrix(NA, trials, length(cprod))
for(mp in 1:length(cprod)){
  NHB_mVax_pVax_pspan[,mp] <- (DALYS_lost_no_u - DALYS_lost_joint_mVax_pVax_u) -
    (tcost_mVax_pVax_pspan[,mp] - tcost_no_pspan[,mp]) / CET_Mali_GDP
}

NHB_pVax_older_pspan <- matrix(NA, trials, length(cprod))
for(po in 1:length(cprod)){
  NHB_pVax_older_pspan[,po] <- (DALYS_lost_no_u - DALYS_lost_pVax_older_u) -
    (tcost_pVax_older_pspan[,po] - tcost_no_pspan[,po]) / CET_Mali_GDP
}

NHB_llAb_pVax_older_pspan <- matrix(NA, trials, length(cprod))
for(lpo in 1:length(cprod)){
  NHB_llAb_pVax_older_pspan[,lpo] <- (DALYS_lost_no_u - DALYS_lost_joint_llAb_pVax_older_u) -
    (tcost_llAb_pVax_older_pspan[,lpo] - tcost_no_pspan[,lpo]) / CET_Mali_GDP
}

NHB_mVax_pVax_older_pspan <- matrix(NA, trials, length(cprod))
for(mpo in 1:length(cprod)){
  NHB_mVax_pVax_older_pspan[,mpo] <- (DALYS_lost_no_u - DALYS_lost_joint_mVax_pVax_older_u) -
    (tcost_mVax_pVax_older_pspan[,mpo] - tcost_no_pspan[,mpo]) / CET_Mali_GDP
}

# compare NHB across changing product cost from $0-3 for all strategies
compare_NHB_pspan <- array(c(NHB_no_pspan, NHB_llAb_pspan, NHB_mVax_pspan,
                             NHB_pVax_pspan, NHB_llAb_pVax_pspan, NHB_mVax_pVax_pspan,
                             NHB_pVax_older_pspan, NHB_llAb_pVax_older_pspan,
                             NHB_mVax_pVax_older_pspan), dim = c(trials, length(cprod), 9))

win_NHB_pspan <- apply(compare_NHB_pspan, MARGIN = c(1,2), which.max)

pO_no_pspan <- rep(0, length(cprod))
for(no in 1: length(cprod)){
  pO_no_pspan[no] <- sum(win_NHB_pspan[,no] == 1)/trials
}

pO_llAb_pspan <- rep(0, length(cprod))
for(Ol in 1: length(cprod)){
  pO_llAb_pspan[Ol] <- sum(win_NHB_pspan[,Ol] == 2)/trials
}

pO_mVax_pspan <- rep(0, length(cprod))
for(Om in 1: length(cprod)){
  pO_mVax_pspan[Om] <- sum(win_NHB_pspan[,Om] == 3)/trials
}

pO_pVax_pspan <- rep(0, length(cprod))
for(Op in 1: length(cprod)){
  pO_pVax_pspan[Op] <- sum(win_NHB_pspan[,Op] == 4)/trials
}

pO_llAb_pVax_pspan <- rep(0, length(cprod))
for(Olp in 1: length(cprod)){
  pO_llAb_pVax_pspan[Olp] <- sum(win_NHB_pspan[,Olp] == 5)/trials
}

pO_mVax_pVax_pspan <- rep(0, length(cprod))
for(Omp in 1: length(cprod)){
  pO_mVax_pVax_pspan[Omp] <- sum(win_NHB_pspan[,Omp] == 6)/trials
}

pO_pVax_older_pspan <- rep(0, length(cprod))
for(Opo in 1: length(cprod)){
  pO_pVax_older_pspan[Opo] <- sum(win_NHB_pspan[,Opo] == 7)/trials
}

pO_llAb_pVax_older_pspan <- rep(0, length(cprod))
for(Olpo in 1: length(cprod)){
  pO_llAb_pVax_older_pspan[Olpo] <- sum(win_NHB_pspan[,Olpo] == 8)/trials
}

pO_mVax_pVax_older_pspan <- rep(0, length(cprod))
for(Ompo in 1: length(cprod)){
  pO_mVax_pVax_older_pspan[Ompo] <- sum(win_NHB_pspan[,Ompo] == 9)/trials
}
######
