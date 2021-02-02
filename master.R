# Master Script
trials <- 10000
source("health_functions.R")
source("health_outcome_parameters.R")
source("attack_rates.R")
source("intervention_schedules.R")
source("economic functions.R")
source("economic outcome parameters.R")

# Create data frame for running intervention scenarios
# Add in: "ss_llAb", "ss_mVax", "ss1_pVax", "ss2_pVax"
int_names <- c("llAb", "mVax", "pVax", "llAb + pVax, no intf", "mVax + pVax, no intf", "llAb + pVax, hi intf", "mVax + pVax, hi intf", "llAb + pVax, lo intf", "mVax + pVax, lo intf")
efficacy <- c(0.70, 0.70, 0.70, NA, NA, NA, NA, NA, NA)
duration <- c(5, 4, 12, NA, NA, NA, NA, NA, NA)
intf <- c(NA, NA, NA, 1, 1, 0.2, 0.2, 0.8, 0.8) # 20% and 80% interference
coverage <- c(0.830, 0.355, 0.77, NA, NA, NA, NA, NA, NA)
costs <- c(4.35, 4.35, 4.35, NA, NA, NA, NA, NA, NA)

# Calculate number of RSV cases under status quo and each intervention
cases_no <- RSVcases(pd_calc(0, 0, AR_y_bc, 0), num_infants)
cases_llAb <- RSVcases(pd_calc(efficacy[1], coverage[1], AR_y_bc, mat_eff_llAb), num_infants)
cases_mVax <- RSVcases(pd_calc(efficacy[2], coverage[2], AR_y_bc, mat_eff_mVax), num_infants)
cases_pVax <- RSVcases(pd_calc(efficacy[3], coverage[3], AR_y_bc, mat_eff_pVax), num_infants)
cases_joint_llAb_pVax <-  RSVcases(pd_joint(efficacy[1], efficacy[3], coverage[1], coverage[3], AR_y_bc, mat_eff_llAb, mat_eff_pVax, intf[4]), num_infants)
cases_joint_mVax_pVax <- RSVcases(pd_joint(efficacy[2], efficacy[3], coverage[2], coverage[3], AR_y_bc, mat_eff_mVax, mat_eff_pVax, intf[5]), num_infants)
cases_intfhi_llAb_pVax <- RSVcases(pd_joint(efficacy[1], efficacy[3], coverage[1], coverage[3], AR_y_bc, mat_eff_llAb, mat_eff_pVax, intf[6]), num_infants)
cases_intfhi_mVax_pVax <- RSVcases(pd_joint(efficacy[2], efficacy[3], coverage[2], coverage[3], AR_y_bc, mat_eff_mVax, mat_eff_pVax, intf[7]), num_infants)
cases_intflo_llAb_pVax <- RSVcases(pd_joint(efficacy[1], efficacy[3], coverage[1], coverage[3], AR_y_bc, mat_eff_llAb, mat_eff_pVax, intf[8]), num_infants)
cases_intflo_mVax_pVax <- RSVcases(pd_joint(efficacy[2], efficacy[3], coverage[2], coverage[3], AR_y_bc, mat_eff_mVax, mat_eff_pVax, intf[9]), num_infants)

# Calculate number of RSV cases w/ uncertainty
cases_no_u <- apply(AR_y_u, 3, RSVcases, babies = num_infants)

pd_llAb_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (ll in 1:trials) {
  pd_llAb_array[,,ll] <- pd_calc(efficacy[1], coverage[1], AR_y_u[,,ll], mat_eff_llAb)
} 
cases_llAb_u <- apply(pd_llAb_array, 3, RSVcases, babies = num_infants)

pd_mVax_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (m in 1:trials) {
  pd_mVax_array[,,m] <- pd_calc(efficacy[2], coverage[2], AR_y_u[,,m], mat_eff_mVax)
} 
cases_mVax_u <- apply(pd_mVax_array, 3, RSVcases, babies = num_infants)

pd_pVax_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (p in 1:trials) {
  pd_pVax_array[,,p] <- pd_calc(efficacy[3], coverage[3], AR_y_u[,,p], mat_eff_pVax)
} 
cases_pVax_u <- apply(pd_pVax_array, 3, RSVcases, babies = num_infants)

pd_llAb_pVax_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (lp in 1:trials) {
  pd_llAb_pVax_array[,,lp] <- pd_joint(efficacy[1], efficacy[3], coverage[1], coverage[3], AR_y_u[,,lp], mat_eff_llAb, mat_eff_pVax, intf[4])
}
cases_joint_llAb_pVax_u <- apply(pd_llAb_pVax_array, 3, RSVcases, babies = num_infants)

pd_mVax_pVax_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (mp in 1:trials) {
  pd_mVax_pVax_array[,,mp] <- pd_joint(efficacy[2], efficacy[3], coverage[2], coverage[3], AR_y_u[,,mp], mat_eff_llAb, mat_eff_pVax, intf[5])
}
cases_joint_mVax_pVax_u <- apply(pd_mVax_pVax_array, 3, RSVcases, babies = num_infants)


# Calculate number of deaths under status quo and each intervention
deaths_no <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_no)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_no)))
deaths_llAb <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_llAb)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_llAb)))
deaths_mVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_mVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_mVax)))
deaths_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_pVax)))
deaths_joint_llAb_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax)))
deaths_joint_mVax_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax)))
deaths_intfhi_llAb_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_llAb_pVax )), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_llAb_pVax)))
deaths_intfhi_mVax_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_mVax_pVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_mVax_pVax)))
deaths_intflo_llAb_pVax <-mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_llAb_pVax )), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_intflo_llAb_pVax)))
deaths_intflo_mVax_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_mVax_pVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_intflo_mVax_pVax)))

# Calculate number of deaths with uncertainty
deaths_no_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_no_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_no_u)))
deaths_llAb_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_llAb_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_llAb_u)))
deaths_mVax_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_mVax_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_mVax_u)))
deaths_pVax_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_u)))
deaths_joint_llAb_pVax_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_u)))
deaths_joint_mVax_pVax_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_u)))

# Calculate DALYs lost under status quo and each intervention
DALYS_lost_no <- YLL_func(deaths_no) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_no)), deaths_no, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_no), dw_LRTI_mod)
DALYS_lost_llAb <- YLL_func(deaths_llAb) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_llAb)), deaths_llAb, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_llAb), dw_LRTI_mod)
DALYS_lost_mVax <- YLL_func(deaths_mVax) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_mVax)), deaths_mVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_mVax), dw_LRTI_mod)
DALYS_lost_pVax <- YLL_func(deaths_pVax) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax)), deaths_pVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_pVax), dw_LRTI_mod)
DALYS_lost_joint_llAb_pVax <- YLL_func(deaths_joint_llAb_pVax) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax)), deaths_joint_llAb_pVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_joint_llAb_pVax), dw_LRTI_mod)
DALYS_lost_joint_mVax_pVax <- YLL_func(deaths_joint_mVax_pVax) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax)), deaths_joint_mVax_pVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_joint_mVax_pVax), dw_LRTI_mod)
DALYS_lost_intfhi_llAb_pVax <- YLL_func(deaths_intfhi_llAb_pVax) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_llAb_pVax)), deaths_intfhi_llAb_pVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_intfhi_llAb_pVax), dw_LRTI_mod)
DALYS_lost_intfhi_mVax_pVax <- YLL_func(deaths_intfhi_mVax_pVax ) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_mVax_pVax)), deaths_intfhi_mVax_pVax , di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_intfhi_mVax_pVax), dw_LRTI_mod)
DALYS_lost_intflo_llAb_pVax <- YLL_func(deaths_intflo_llAb_pVax) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_llAb_pVax)), deaths_intfhi_llAb_pVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_intflo_llAb_pVax), dw_LRTI_mod)
DALYS_lost_intflo_mVax_pVax <- YLL_func(deaths_intflo_mVax_pVax ) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_mVax_pVax)), deaths_intflo_mVax_pVax , di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_intflo_mVax_pVax), dw_LRTI_mod)

# Calculate DALYs lost with uncertainty
DALYS_lost_no_u <- YLL_func(deaths_no_u) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_no_u)), deaths_no_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_no_u), dw_LRTI_mod_u)
DALYS_lost_llAb_u <- YLL_func(deaths_llAb_u) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_llAb_u)), deaths_llAb_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_llAb_u), dw_LRTI_mod_u)
DALYS_lost_mVax_u <- YLL_func(deaths_mVax_u) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_mVax_u)), deaths_mVax_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_mVax_u), dw_LRTI_mod_u)
DALYS_lost_pVax_u <- YLL_func(deaths_pVax_u) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_u)), deaths_pVax_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_pVax_u), dw_LRTI_mod_u)
DALYS_lost_joint_llAb_pVax_u <- YLL_func(deaths_joint_llAb_pVax_u) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_u)), deaths_joint_llAb_pVax_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_u), dw_LRTI_mod_u)
DALYS_lost_joint_mVax_pVax_u <- YLL_func(deaths_joint_mVax_pVax_u) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_u)), deaths_joint_mVax_pVax_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_u), dw_LRTI_mod_u)

# Calculate medical costs
medcost_no <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_no)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_no)))
medcost_llAb <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_llAb)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_llAb)))
medcost_mVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_mVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_mVax)))
medcost_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax)))
medcost_joint_llAb_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax)))
medcost_joint_mVax_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax)))
medcost_intfhi_llAb_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_llAb_pVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_llAb_pVax)))
medcost_intfhi_mVax_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_mVax_pVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_mVax_pVax)))
medcost_intflo_llAb_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_llAb_pVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_llAb_pVax)))
medcost_intflo_mVax_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_mVax_pVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_mVax_pVax)))

# Calculate medical costs with uncertainty
# Need to come back in and add additional uncertainty besides cases
medcost_no_u <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_no_u)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_no_u)))
medcost_llAb_u <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_llAb_u)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_llAb_u)))
medcost_mVax_u <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_mVax_u)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_mVax_u)))
medcost_pVax_u <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_u)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_u)))
medcost_joint_llAb_pVax_u <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_u)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_u)))
medcost_joint_mVax_pVax_u <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_u)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_u)))

# Calculate total intervention costs
totalcost_no <- medcost_no
totalcost_llAb <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + medcost_llAb
totalcost_mVax <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + medcost_mVax
totalcost_pVax <- sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_pVax
totalcost_joint_llAb_pVax <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_joint_llAb_pVax
totalcost_joint_mVax_pVax <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_joint_mVax_pVax
totalcost_intfhi_llAb_pVax <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_intfhi_llAb_pVax
totalcost_intfhi_mVax_pVax <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_intfhi_mVax_pVax
totalcost_intflo_llAb_pVax <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_intflo_llAb_pVax
totalcost_intflo_mVax_pVax <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_intflo_mVax_pVax

# Calculate total intervention costs with uncertainty
totalcost_no_u <- medcost_no_u
totalcost_llAb_u <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + medcost_llAb_u
totalcost_mVax_u <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + medcost_mVax_u
totalcost_pVax_u <- sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_pVax_u
totalcost_joint_llAb_pVax_u <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_joint_llAb_pVax_u
totalcost_joint_mVax_pVax_u <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_joint_mVax_pVax_u

# calculate probability of being cost-effective across WTP

prep_llAb <- cbind(DALYS_lost_no_u, DALYS_lost_llAb_u, totalcost_llAb_u, totalcost_no_u)
pce_llAb <- rep(0, length(WTP_sp))
for (l in 1: length(WTP_sp)){
  NHB_l <- NHB_func(prep_llAb, WTP_sp[l])
  pce_llAb[l] <- sum(NHB_l >0)/trials
}

prep_mVax <- cbind(DALYS_lost_no_u, DALYS_lost_mVax_u, totalcost_mVax_u, totalcost_no_u)
pce_mVax <- rep(0, length(WTP_sp))
for (m in 1: length(WTP_sp)){
  NHB_m <- NHB_func(prep_mVax, WTP_sp[m])
  pce_mVax[m] <- sum(NHB_m >0)/trials
}

prep_pVax <- cbind(DALYS_lost_no_u, DALYS_lost_pVax_u, totalcost_pVax_u, totalcost_no_u)
pce_pVax <- rep(0, length(WTP_sp))
for (p in 1: length(WTP_sp)){
  NHB_p <- NHB_func(prep_pVax, WTP_sp[p])
  pce_pVax[p] <- sum(NHB_p >0)/trials
}

prep_joint_llAb_pVax <- cbind(DALYS_lost_no_u, DALYS_lost_joint_llAb_pVax_u, totalcost_joint_llAb_pVax_u, totalcost_no_u)
pce_joint_llAb_pVax <- rep(0, length(WTP_sp))
for (lp in 1: length(WTP_sp)){
  NHB_lp <- NHB_func(prep_joint_llAb_pVax, WTP_sp[lp])
  pce_joint_llAb_pVax[lp] <- sum(NHB_lp >0)/trials
}

prep_joint_mVax_pVax <- cbind(DALYS_lost_no_u, DALYS_lost_joint_mVax_pVax_u, totalcost_joint_mVax_pVax_u, totalcost_no_u)
pce_joint_mVax_pVax <- rep(0, length(WTP_sp))
for (mp in 1: length(WTP_sp)){
  NHB_mp <- NHB_func(prep_joint_mVax_pVax, WTP_sp[mp])
  pce_joint_mVax_pVax[mp] <- sum(NHB_mp >0)/trials
}

####
prep_no <- cbind(DALYS_lost_no_u, DALYS_lost_no_u, totalcost_no_u, totalcost_no_u)
for (no in 1: length(WTP_sp)){
NHB_no <- NHB_func(prep_no, WTP_sp[no])}
###

compare_NHB <- cbind(NHB_no, NHB_l, NHB_m, NHB_p, NHB_lp, NHB_mp)
compare_NHB$win <- max.col(compare_NHB, ties.method = "first")

pO_llAb <- rep(0, length(WTP_sp))
for(Ol in 1: length(WTP_sp)){
  pO_llAb[Ol] <- sum(compare_NHB$win == 2)/trials
}

pO_mVax <- rep(0, length(WTP_sp))
for(Om in 1: length(WTP_sp)){
  pO_mVax[Om] <- sum(compare_NHB$win == 3)/trials
}

pO_pVax <- rep(0, length(WTP_sp))
for(Op in 1: length(WTP_sp)){
  pO_mVax[Op] <- sum(compare_NHB$win == 4)/trials
}

pO_llAb_pVax <- rep(0, length(WTP_sp))
for(Olp in 1: length(WTP_sp)){
  pO_llAb_pVax[Olp] <- sum(compare_NHB$win == 5)/trials
}

pO_mVax_pVax <- rep(0, length(WTP_sp))
for(Omp in 1: length(WTP_sp)){
  pO_mVax_pVax[Omp] <- sum(compare_NHB$win == 6)/trials
}

####
additional_cost <- c(totalcost_llAb - totalcost_no, totalcost_mVax - totalcost_no, totalcost_pVax- totalcost_no, totalcost_joint_llAb_pVax - totalcost_no, totalcost_joint_mVax_pVax - totalcost_no,
                     totalcost_intfhi_llAb_pVax - totalcost_no, totalcost_intfhi_mVax_pVax - totalcost_no, totalcost_intflo_llAb_pVax- totalcost_no, totalcost_intflo_mVax_pVax- totalcost_no)
deaths_averted <- c(deaths_no - deaths_llAb, deaths_no - deaths_mVax, deaths_no - deaths_pVax, deaths_no - deaths_joint_llAb_pVax, deaths_no - deaths_joint_mVax_pVax,
                    deaths_no - deaths_intfhi_llAb_pVax, deaths_no - deaths_intfhi_mVax_pVax, deaths_no - deaths_intflo_llAb_pVax, deaths_no - deaths_intflo_mVax_pVax)
DALYs_averted <- c(DALYS_lost_no - DALYS_lost_llAb, DALYS_lost_no - DALYS_lost_mVax, DALYS_lost_no - DALYS_lost_pVax, DALYS_lost_no - DALYS_lost_joint_llAb_pVax, DALYS_lost_no - DALYS_lost_joint_mVax_pVax,
                   DALYS_lost_no - DALYS_lost_intfhi_llAb_pVax, DALYS_lost_no - DALYS_lost_intfhi_mVax_pVax, DALYS_lost_no - DALYS_lost_intflo_llAb_pVax, DALYS_lost_no - DALYS_lost_intflo_mVax_pVax)

# All ICERs compared to status quo:
ICERs_base <- additional_cost/DALYs_averted
# Calculate ICER moving from pVax to mVax + pVax:
ICER_mVax_pVax <- (totalcost_joint_mVax_pVax - totalcost_pVax) / (DALYS_lost_pVax - DALYS_lost_joint_mVax_pVax)
# Calculate ICER moving from pVax to llAb + pVax:
ICER_llAb_pVax <- (totalcost_joint_llAb_pVax - totalcost_pVax) / (DALYS_lost_pVax - DALYS_lost_joint_llAb_pVax)
# Calculate ICER mocing from pVax to llAb + pVax with 20% interference
ICER_intflo_llAb_pVax <- (totalcost_intflo_llAb_pVax - totalcost_pVax) / (DALYS_lost_pVax - DALYS_lost_intflo_llAb_pVax)

interventions <- data.frame(int_names, efficacy, duration, coverage, additional_cost, deaths_averted, DALYs_averted, ICERs_base)
