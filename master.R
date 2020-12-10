# Master Script
source("health_functions.R")
source("health_outcome_parameters.R")
source("attack_rates.R")
source("intervention_schedules.R")
source("economic functions.R")
source("economic outcome parameters.R")

# Create data frame for running intervention scenarios
# ADd in: "ss_llAb", "ss_mVax", "ss1_pVax", "ss2_pVax"
int_names <- c("llAb", "mVax", "pVax")
efficacy <- c(rep.int(0.70, length(int_names)))
duration <- c(5, 4, 12)
coverage <- c(0.830, 0.355, 0.77)
costs <- c(rep.int(4.35, length(int_names)))

# Calculate number of RSV cases under status quo and each intervention

cases_no <- RSVcases(pd_calc(0, 0, AR_y_bc, 0), num_infants)
cases_llAb <- RSVcases(pd_calc(efficacy[1], coverage[1], AR_y_bc, mat_eff_llAb), num_infants)
cases_mVax <- RSVcases(pd_calc(efficacy[2], coverage[2], AR_y_bc, mat_eff_mVax), num_infants)
cases_pVax <- RSVcases(pd_calc(efficacy[3], coverage[3], AR_y_bc, mat_eff_pVax), num_infants)
cases_joint_llAb_pVax <-  RSVcases(pd_joint(efficacy[1], efficacy[3], coverage[1], coverage[3], AR_y_bc, mat_eff_llAb, mat_eff_pVax), num_infants)
cases_joint_mVax_pVax <- RSVcases(pd_joint(efficacy[2], efficacy[3], coverage[2], coverage[3], AR_y_bc, mat_eff_mVax, mat_eff_pVax), num_infants)

# Calculate number of deaths under status quo and each intervention
deaths_no <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_no)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_no)))
deaths_llAb <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_llAb)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_llAb)))
deaths_mVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_mVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_mVax)))
deaths_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_pVax)))
deaths_joint_llAb_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax)))
deaths_joint_mVax_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax)))

# Calculate DALYs lost under status quo and each intervention
DALYS_lost_no <- YLL_func(deaths_no) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_no)), deaths_no, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_no), dw_LRTI_mod)
DALYS_lost_llAb <- YLL_func(deaths_llAb) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_llAb)), deaths_llAb, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_llAb), dw_LRTI_mod)
DALYS_lost_mVax <- YLL_func(deaths_mVax) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_mVax)), deaths_mVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_mVax), dw_LRTI_mod)
DALYS_lost_pVax <- YLL_func(deaths_pVax) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax)), deaths_pVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_pVax), dw_LRTI_mod)
DALYS_lost_joint_llAb_pVax <- YLL_func(deaths_joint_llAb_pVax) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax)), deaths_joint_llAb_pVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_joint_llAb_pVax), dw_LRTI_mod)
DALYS_lost_joint_mVax_pVax <- YLL_func(deaths_joint_mVax_pVax) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax)), deaths_joint_mVax_pVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_joint_mVax_pVax), dw_LRTI_mod)

# Calculate medical costs

medcost_no <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_no)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_no)))
medcost_llAb <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_llAb)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_llAb)))
medcost_mVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_mVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_mVax)))
medcost_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax)))
medcost_joint_llAb_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax)))
medcost_joint_mVax_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax)))

# Calculate total intervention costs
totalcost_no <- medcost_no
totalcost_llAb <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + medcost_llAb
totalcost_mVax <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + medcost_mVax
totalcost_pVax <- sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_pVax
totalcost_joint_llAb_pVax <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_joint_llAb_pVax
totalcost_joint_mVax_pVax <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_joint_mVax_pVax

####
additional_cost <- c(totalcost_llAb - totalcost_no, totalcost_mVax - totalcost_no, totalcost_pVax- totalcost_no)
deaths_averted <- c(deaths_no - deaths_llAb, deaths_no - deaths_mVax, deaths_no - deaths_pVax)
DALYs_averted <- c(DALYS_lost_no - DALYS_lost_llAb, DALYS_lost_no - DALYS_lost_mVax, DALYS_lost_no - DALYS_lost_pVax)
interventions <- data.frame(int_names, efficacy, duration, coverage, additional_cost, deaths_averted, DALYs_averted)

# Plot cost per DALYs averted 
plot(DALYS_lost_no - DALYS_lost_llAb, totalcost_llAb- totalcost_no, col = "red", pch = 19, xlim = c(0,3500), ylim = c(0,7000000), xlab = "DALYs averted", ylab = 
       "Added cost (USD)")
points(DALYS_lost_no - DALYS_lost_mVax, totalcost_mVax- totalcost_no, col = "blue", pch = 19)
points(DALYS_lost_no - DALYS_lost_pVax, totalcost_pVax- totalcost_no, col = "chartreuse4", pch = 19)
points(DALYS_lost_no - DALYS_lost_joint_llAb_pVax, totalcost_joint_llAb_pVax - totalcost_no, col = "orange", pch = 19)
points(DALYS_lost_no - DALYS_lost_joint_mVax_pVax, totalcost_joint_mVax_pVax - totalcost_no, col = "maroon", pch = 19)
legend("topleft", legend = c("llAb", "mVax", "pVax", "llAb + pVax", "mVax + pVax"), pch = 19, col = c("red","blue", "chartreuse4","orange", "maroon", pch = 19))


