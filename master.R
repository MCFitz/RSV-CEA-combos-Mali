# Master Script
trials <- 1000

# load required packages
library(abind)
library(readr)
library("triangle")

# generic functions for code
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

# source all necessary scripts
source("health_functions.R")
source("health_outcome_parameters.R")
source("attack_rates.R")
source("intervention_schedules.R")
source("economic functions.R")
source("economic outcome parameters.R")

# Create data frame for running intervention scenarios
int_names <- c("llAb", "mVax", "pVax", "llAb + pVax, no intf", "mVax + pVax, no intf", "llAb + pVax, hi intf", "mVax + pVax, hi intf", "llAb + pVax, lo intf", "mVax + pVax, lo intf")
efficacy <- c(0.70, 0.70, 0.70, 0.70, 0.70, 0.30, 0.30, 0.50, 0.50)
duration <- c(5, 4, 12, NA, NA, NA, NA, NA, NA)
# intf <- c(NA, NA, NA, 0, 0, 0.25, 0.25, 0.50, 0.50) # 25% and 50% interference
coverage <- c(0.830, 0.433, 0.77, NA, NA, NA, NA, NA, NA)
costs <- c(4.35, 4.35, 4.35, NA, NA, NA, NA, NA, NA)

# fully loaded EPI visit intervention costs
# estimate for now
cost_EPI <- 3.73
cost_prod <- 3

# Calculate number of RSV cases under status quo and each intervention
cases_no <- RSVcases(pd_calc(0, 0, AR_y_bc, empty_year_cohort), num_infants)
cases_llAb <- RSVcases(pd_calc(efficacy[1], coverage[1], AR_y_bc, mat_eff_llAb), num_infants)
cases_mVax <- RSVcases(pd_calc(efficacy[2], coverage[2], AR_y_bc, mat_eff_mVax), num_infants)
cases_pVax <- RSVcases(pd_calc(efficacy[3], coverage[3], AR_y_bc, mat_eff_pVax), num_infants)
cases_pVax_intflo <- RSVcases(pd_calc(efficacy[8], coverage[3], AR_y_bc, mat_eff_pVax), num_infants)
cases_pVax_intfhi <- RSVcases(pd_calc(efficacy[6], coverage[3], AR_y_bc, mat_eff_pVax), num_infants)
cases_joint_llAb_pVax <-  RSVcases(pd_joint(efficacy[1], efficacy[4], coverage[1], coverage[3], AR_y_bc, mat_eff_llAb, mat_eff_pVax), num_infants)
cases_joint_mVax_pVax <- RSVcases(pd_joint(efficacy[2], efficacy[5], coverage[2], coverage[3], AR_y_bc, mat_eff_mVax, mat_eff_pVax), num_infants)
cases_intflo_llAb_pVax <- RSVcases(pd_joint(efficacy[1], efficacy[8], coverage[1], coverage[3], AR_y_bc, mat_eff_llAb, mat_eff_pVax), num_infants)
cases_intflo_mVax_pVax <- RSVcases(pd_joint(efficacy[2], efficacy[9], coverage[2], coverage[3], AR_y_bc, mat_eff_mVax, mat_eff_pVax), num_infants)
cases_intfhi_llAb_pVax <- RSVcases(pd_joint(efficacy[1], efficacy[6], coverage[1], coverage[3], AR_y_bc, mat_eff_llAb, mat_eff_pVax), num_infants)
cases_intfhi_mVax_pVax <- RSVcases(pd_joint(efficacy[2], efficacy[7], coverage[2], coverage[3], AR_y_bc, mat_eff_mVax, mat_eff_pVax), num_infants)

cases_pVax_older <- RSVcases(pd_calc(efficacy[3], coverage[3], AR_y_bc, mat_eff_older_pVax), num_infants)
cases_joint_llAb_pVax_older  <-  RSVcases(pd_joint(efficacy[1], efficacy[3], coverage[1], coverage[3], AR_y_bc, mat_eff_llAb, mat_eff_older_pVax), num_infants)
cases_joint_mVax_pVax_older  <- RSVcases(pd_joint(efficacy[2], efficacy[3], coverage[2], coverage[3], AR_y_bc, mat_eff_mVax, mat_eff_older_pVax), num_infants)

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
  pd_pVax_array[,,p] <- pd_calc(efficacy[8], coverage[3], AR_y_u[,,p], mat_eff_pVax)
} 
cases_pVax_u <- apply(pd_pVax_array, 3, RSVcases, babies = num_infants)

# scenario analysis when efficacy reduction occurs only when pVax is in combo
pd_pVax_array_SA <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (p in 1:trials) {
  pd_pVax_array_SA[,,p] <- pd_calc(efficacy[3], coverage[3], AR_y_u[,,p], mat_eff_pVax)
} 
cases_pVax_u_SA <- apply(pd_pVax_array, 3, RSVcases, babies = num_infants)

pd_llAb_pVax_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (lp in 1:trials) {
  pd_llAb_pVax_array[,,lp] <- pd_joint(efficacy[1], efficacy[8], coverage[1], coverage[3], AR_y_u[,,lp], mat_eff_llAb, mat_eff_pVax)
}
cases_joint_llAb_pVax_u <- apply(pd_llAb_pVax_array, 3, RSVcases, babies = num_infants)

pd_mVax_pVax_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (mp in 1:trials) {
  pd_mVax_pVax_array[,,mp] <- pd_joint(efficacy[2], efficacy[9], coverage[2], coverage[3], AR_y_u[,,mp], mat_eff_mVax, mat_eff_pVax)
}
cases_joint_mVax_pVax_u <- apply(pd_mVax_pVax_array, 3, RSVcases, babies = num_infants)

pd_pVax_older_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (p in 1:trials) {
  pd_pVax_older_array[,,p] <- pd_calc(efficacy[3], coverage[3], AR_y_u[,,p], mat_eff_older_pVax)
} 
cases_pVax_older_u <- apply(pd_pVax_older_array, 3, RSVcases, babies = num_infants)

pd_llAb_pVax_older_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (lp in 1:trials) {
  pd_llAb_pVax_older_array[,,lp] <- pd_joint(efficacy[1], efficacy[3], coverage[1], coverage[3], AR_y_u[,,lp], mat_eff_llAb, mat_eff_older_pVax)
}
cases_joint_llAb_pVax_older_u <- apply(pd_llAb_pVax_older_array, 3, RSVcases, babies = num_infants)

pd_mVax_pVax_older_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
for (mp in 1:trials) {
  pd_mVax_pVax_older_array[,,mp] <- pd_joint(efficacy[2], efficacy[3], coverage[2], coverage[3], AR_y_u[,,mp], mat_eff_mVax, mat_eff_older_pVax)
}
cases_joint_mVax_pVax_older_u <- apply(pd_mVax_pVax_older_array, 3, RSVcases, babies = num_infants)

# Calculating number of cases across pVax efficacy (for 10 & 14 wk doses) from 0 to 100% (due to interference)
eff_red <- c(0.01, seq(0.05, 1, by = 0.01))

# pediatric vaccine
er_p_cases <- matrix(NA, trials, length(eff_red))
for (p in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_p <- pd_calc(eff_red[er], coverage[3], AR_y_u[,,p], mat_eff_pVax)
    er_p_cases[p, er] <- RSVcases(temp_p, babies = num_infants)
  }}

# long-acting Ab plus pediatric vaccine
er_lp_cases <- matrix(NA, trials, length(eff_red))
for (lp in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_lp <- pd_joint(efficacy[3], eff_red[er], coverage[1], coverage[3], AR_y_u[,,lp], 
                     mat_eff_llAb, mat_eff_pVax)
    er_lp_cases[lp, er] <- RSVcases(temp_lp, babies = num_infants)
}}

# maternal vaccine plus pediatric vaccine
er_mp_cases <- matrix(NA, trials, length(eff_red))
for (mp in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_mp <- pd_joint(efficacy[3], eff_red[er], coverage[2], coverage[3], AR_y_u[,,mp],
                        mat_eff_mVax, mat_eff_pVax)
er_mp_cases[mp, er] <- RSVcases(temp_mp, babies = num_infants)
  }}

# pediatric vaccine admin to older infants
er_p_older_cases <- matrix(NA, trials, length(eff_red))
for (p in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_p <- pd_calc(efficacy[3], coverage[3], AR_y_u[,,p], mat_eff_older_pVax)
    er_p_older_cases[p, er] <- RSVcases(temp_p, babies = num_infants)
  }}
# long-acting Ab plus pediatric vaccine admin to older infants
er_lp_older_cases <- matrix(NA, trials, length(eff_red))
for (lp in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_lp_older <- pd_joint(efficacy[1], efficacy[3], coverage[1], coverage[3], AR_y_u[,,lp], 
                        mat_eff_llAb, mat_eff_older_pVax)
    er_lp_older_cases[lp, er] <- RSVcases(temp_lp_older, babies = num_infants)
  }}
# maternal vaccine plus pediatric vaccine admin to older infants
er_mp_older_cases <- matrix(NA, trials, length(eff_red))
for (mp in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_mp_older <- pd_joint(efficacy[2], efficacy[3], coverage[2], coverage[3], AR_y_u[,,mp],
                        mat_eff_mVax, mat_eff_older_pVax)
    er_mp_older_cases[mp, er] <- RSVcases(temp_mp_older, babies = num_infants)
  }}

# Calculate number of deaths under status quo and each intervention
deaths_no <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_no)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_no)))
deaths_llAb <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_llAb)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_llAb)))
deaths_mVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_mVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_mVax)))
deaths_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_pVax)))
deaths_pVax_intflo <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intflo)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intflo)))
deaths_pVax_intfhi <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intfhi)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intfhi)))
deaths_joint_llAb_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax)))
deaths_joint_mVax_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax)))
deaths_intfhi_llAb_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_llAb_pVax )), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_llAb_pVax)))
deaths_intfhi_mVax_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_mVax_pVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_mVax_pVax)))
deaths_intflo_llAb_pVax <-mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_llAb_pVax )), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_intflo_llAb_pVax)))
deaths_intflo_mVax_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_mVax_pVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_intflo_mVax_pVax)))

deaths_pVax_older <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_older)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_pVax_older)))
deaths_joint_llAb_pVax_older <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax_older)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax_older)))
deaths_joint_mVax_pVax_older <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax_older)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax_older)))

# Calculate number of deaths with uncertainty
# Switch code block ON if using flat CFR (based on low-income country spline from Li et al. 2020)
deaths_no_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_no_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_no_u)))
deaths_llAb_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_llAb_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_llAb_u)))
deaths_mVax_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_mVax_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_mVax_u)))
deaths_pVax_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_u)))
deaths_pVax_u_SA <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_u_SA)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_u_SA)))
deaths_joint_llAb_pVax_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_u)))
deaths_joint_mVax_pVax_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_u)))

deaths_pVax_older_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_older_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_older_u)))
deaths_joint_llAb_pVax_older_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_older_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_older_u)))
deaths_joint_mVax_pVax_older_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_older_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_older_u)))

# # # Calculate number of deaths with uncertainty and age-weighted CFRs
# # # Switch code block ON to age-weight the CFRs (based on Lower-middle income country spline from Li et al. 2020)
# deaths_no_u_age <- mort_inpat_func(CFR_sub, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, rep.col(cases_no_u, length(AR_age_weights)) * AR_age_weights)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, rep.col(cases_no_u, length(AR_age_weights)) * AR_age_weights)))
# deaths_llAb_u_age <- mort_inpat_func(CFR_sub, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, rep.col(cases_llAb_u, length(AR_age_weights)) * AR_age_weights)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, rep.col(cases_llAb_u, length(AR_age_weights)) * AR_age_weights)))
# deaths_mVax_u_age <- mort_inpat_func(CFR_sub, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, rep.col(cases_mVax_u, length(AR_age_weights)) * AR_age_weights)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, rep.col(cases_mVax_u, length(AR_age_weights)) * AR_age_weights)))
# deaths_pVax_u_age <- mort_inpat_func(CFR_sub, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, rep.col(cases_pVax_u, length(AR_age_weights)) * AR_age_weights)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, rep.col(cases_pVax_u, length(AR_age_weights)) * AR_age_weights)))
# deaths_joint_llAb_pVax_u_age <- mort_inpat_func(CFR_sub, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, rep.col(cases_joint_llAb_pVax_u, length(AR_age_weights)) * AR_age_weights)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, rep.col(cases_joint_llAb_pVax_u, length(AR_age_weights)) * AR_age_weights)))
# deaths_joint_mVax_pVax_u_age <- mort_inpat_func(CFR_sub, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, rep.col(cases_joint_mVax_pVax_u, length(AR_age_weights)) * AR_age_weights)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, rep.col(cases_joint_mVax_pVax_u, length(AR_age_weights)) * AR_age_weights)))
# 
# deaths_pVax_older_u_age <- mort_inpat_func(CFR_sub, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, rep.col(cases_pVax_older_u, length(AR_age_weights)) * AR_age_weights)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, rep.col(cases_pVax_older_u, length(AR_age_weights)) * AR_age_weights)))
# deaths_joint_llAb_pVax_older_u_age <- mort_inpat_func(CFR_sub, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, rep.col(cases_joint_llAb_pVax_older_u, length(AR_age_weights)) * AR_age_weights)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, rep.col(cases_joint_llAb_pVax_older_u, length(AR_age_weights)) * AR_age_weights)))
# deaths_joint_mVax_pVax_older_u_age <- mort_inpat_func(CFR_sub, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, rep.col(cases_joint_mVax_pVax_older_u, length(AR_age_weights)) * AR_age_weights)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, rep.col(cases_joint_mVax_pVax_older_u, length(AR_age_weights)) * AR_age_weights)))
# 
# # # sum the rows
# deaths_no_u <- apply(deaths_no_u_age, MARGIN = 1, sum)
# deaths_llAb_u <- apply(deaths_llAb_u_age, MARGIN = 1, sum)
# deaths_mVax_u <- apply(deaths_mVax_u_age, MARGIN = 1, sum)
# deaths_pVax_u <- apply(deaths_pVax_u_age, MARGIN = 1, sum)
# deaths_joint_llAb_pVax_u <- apply(deaths_joint_llAb_pVax_u_age, MARGIN = 1, sum)
# deaths_joint_mVax_pVax_u <- apply(deaths_joint_mVax_pVax_u_age, MARGIN = 1, sum)
# deaths_pVax_older_u <- apply(deaths_pVax_older_u_age, MARGIN = 1, sum)
# deaths_joint_llAb_pVax_older_u <- apply(deaths_joint_llAb_pVax_older_u_age, MARGIN = 1, sum)
# deaths_joint_mVax_pVax_older_u <- apply(deaths_joint_mVax_pVax_older_u_age, MARGIN = 1, sum)

# Calculate number of deaths across pVax efficacy from 0 to 100%

deaths_er_pVax <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_p_cases)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, er_p_cases)))
deaths_er_llAb_pVax <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_lp_cases)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, er_lp_cases)))
deaths_er_mVax_pVax <-  mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_mp_cases)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, er_mp_cases)))

deaths_er_pVax_older <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_p_older_cases)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, er_p_older_cases)))
deaths_er_llAb_pVax_older <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_lp_older_cases)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, er_lp_older_cases)))
deaths_er_mVax_pVax_older <-  mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_mp_older_cases)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, er_mp_older_cases)))

# Calculate DALYs lost under status quo and each intervention
DALYS_lost_no <- YLL_func(deaths_no) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_no)), deaths_no, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_no), dw_LRTI_mod)
DALYS_lost_llAb <- YLL_func(deaths_llAb) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_llAb)), deaths_llAb, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_llAb), dw_LRTI_mod)
DALYS_lost_mVax <- YLL_func(deaths_mVax) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_mVax)), deaths_mVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_mVax), dw_LRTI_mod)
DALYS_lost_pVax <- YLL_func(deaths_pVax) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax)), deaths_pVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_pVax), dw_LRTI_mod)
DALYS_lost_pVax_intflo <- YLL_func(deaths_pVax_intflo) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intflo)), deaths_pVax_intflo, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_pVax_intflo), dw_LRTI_mod)
DALYS_lost_pVax_intfhi <- YLL_func(deaths_pVax_intfhi) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intfhi)), deaths_pVax_intfhi, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_pVax_intfhi), dw_LRTI_mod)   
DALYS_lost_joint_llAb_pVax <- YLL_func(deaths_joint_llAb_pVax) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax)), deaths_joint_llAb_pVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_joint_llAb_pVax), dw_LRTI_mod)
DALYS_lost_joint_mVax_pVax <- YLL_func(deaths_joint_mVax_pVax) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax)), deaths_joint_mVax_pVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_joint_mVax_pVax), dw_LRTI_mod)
DALYS_lost_intfhi_llAb_pVax <- YLL_func(deaths_intfhi_llAb_pVax) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_llAb_pVax)), deaths_intfhi_llAb_pVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_intfhi_llAb_pVax), dw_LRTI_mod)
DALYS_lost_intfhi_mVax_pVax <- YLL_func(deaths_intfhi_mVax_pVax ) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_mVax_pVax)), deaths_intfhi_mVax_pVax , di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_intfhi_mVax_pVax), dw_LRTI_mod)
DALYS_lost_intflo_llAb_pVax <- YLL_func(deaths_intflo_llAb_pVax) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_llAb_pVax)), deaths_intfhi_llAb_pVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_intflo_llAb_pVax), dw_LRTI_mod)
DALYS_lost_intflo_mVax_pVax <- YLL_func(deaths_intflo_mVax_pVax ) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_mVax_pVax)), deaths_intflo_mVax_pVax , di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_intflo_mVax_pVax), dw_LRTI_mod)

DALYS_lost_pVax_older <- YLL_func(deaths_pVax_older) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_older)), deaths_pVax_older, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_pVax_older), dw_LRTI_mod)
DALYS_lost_joint_llAb_pVax_older <- YLL_func(deaths_joint_llAb_pVax_older) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax_older)), deaths_joint_llAb_pVax_older, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_joint_llAb_pVax_older), dw_LRTI_mod)
DALYS_lost_joint_mVax_pVax_older <- YLL_func(deaths_joint_mVax_pVax_older) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax_older)), deaths_joint_mVax_pVax_older, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_joint_mVax_pVax_older), dw_LRTI_mod)

# Calculate DALYs lost with uncertainty
DALYS_lost_no_u <- YLL_func(deaths_no_u) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_no_u)), deaths_no_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_no_u), dw_LRTI_mod_u)
DALYS_lost_llAb_u <- YLL_func(deaths_llAb_u) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_llAb_u)), deaths_llAb_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_llAb_u), dw_LRTI_mod_u)
DALYS_lost_mVax_u <- YLL_func(deaths_mVax_u) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_mVax_u)), deaths_mVax_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_mVax_u), dw_LRTI_mod_u)
DALYS_lost_pVax_u <- YLL_func(deaths_pVax_u) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_u)), deaths_pVax_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_pVax_u), dw_LRTI_mod_u)
DALYS_lost_pVax_u_SA <- YLL_func(deaths_pVax_u_SA) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_u_SA)), deaths_pVax_u_SA, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_pVax_u_SA), dw_LRTI_mod_u)
DALYS_lost_joint_llAb_pVax_u <- YLL_func(deaths_joint_llAb_pVax_u) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_u)), deaths_joint_llAb_pVax_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_u), dw_LRTI_mod_u)
DALYS_lost_joint_mVax_pVax_u <- YLL_func(deaths_joint_mVax_pVax_u) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_u)), deaths_joint_mVax_pVax_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_u), dw_LRTI_mod_u)

DALYS_lost_pVax_older_u <- YLL_func(deaths_pVax_older_u) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_older_u)), deaths_pVax_older_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_pVax_older_u), dw_LRTI_mod_u)
DALYS_lost_joint_llAb_pVax_older_u <- YLL_func(deaths_joint_llAb_pVax_older_u) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_older_u)), deaths_joint_llAb_pVax_older_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_older_u), dw_LRTI_mod_u)
DALYS_lost_joint_mVax_pVax_older_u <- YLL_func(deaths_joint_mVax_pVax_older_u) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_older_u)), deaths_joint_mVax_pVax_older_u, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_older_u), dw_LRTI_mod_u)

# Calculate DALYs lost across pVax efficacy from 0 to 100%
DALYS_er_pVax <- YLL_func(deaths_er_pVax) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_p_cases)), deaths_er_pVax, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, er_p_cases), dw_LRTI_mod_u)
DALYS_er_llAb_pVax <-  YLL_func(deaths_er_llAb_pVax) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_lp_cases)), deaths_er_llAb_pVax, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, er_lp_cases), dw_LRTI_mod_u)
DALYS_er_mVax_pVax <- YLL_func(deaths_er_mVax_pVax) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_mp_cases)), deaths_er_mVax_pVax , di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, er_mp_cases), dw_LRTI_mod_u)

DALYS_er_pVax_older <- YLL_func(deaths_er_pVax_older) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_p_older_cases)), deaths_er_pVax_older, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, er_p_older_cases), dw_LRTI_mod_u)
DALYS_er_llAb_pVax_older <- YLL_func(deaths_er_llAb_pVax_older) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_lp_older_cases)), deaths_er_llAb_pVax_older, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, er_lp_older_cases), dw_LRTI_mod_u)
DALYS_er_mVax_pVax_older <- YLL_func(deaths_er_mVax_pVax_older) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_mp_older_cases)), deaths_er_mVax_pVax_older , di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, er_mp_older_cases), dw_LRTI_mod_u)

# Calculate medical costs
medcost_no <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_no)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_no)))
medcost_llAb <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_llAb)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_llAb)))
medcost_mVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_mVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_mVax)))
medcost_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax)))
medcost_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax)))
medcost_pVax_intfhi <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intfhi)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intfhi)))
medcost_pVax_intflo <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intflo)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intflo)))
medcost_joint_llAb_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax)))
medcost_joint_mVax_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax)))
medcost_intfhi_llAb_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_llAb_pVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_llAb_pVax)))
medcost_intfhi_mVax_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_mVax_pVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_mVax_pVax)))
medcost_intflo_llAb_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_llAb_pVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_llAb_pVax)))
medcost_intflo_mVax_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_mVax_pVax)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_mVax_pVax)))

medcost_pVax_older <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_older)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_older)))
medcost_joint_llAb_pVax_older <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax_older)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax_older)))
medcost_joint_mVax_pVax_older <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax_older)), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax_older)))

# Calculate medical costs with uncertainty
# Need to come back in and add additional uncertainty besides cases
medcost_no_u <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_no_u)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_no_u)))
medcost_llAb_u <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_llAb_u)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_llAb_u)))
medcost_mVax_u <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_mVax_u)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_mVax_u)))
medcost_pVax_u <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_u)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_u)))
medcost_pVax_u_SA <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_u_SA)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_u_SA)))
medcost_joint_llAb_pVax_u <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_u)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_u)))
medcost_joint_mVax_pVax_u <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_u)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_u)))

medcost_pVax_u_older <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_older_u)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_older_u)))
medcost_joint_llAb_pVax_u_older <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_older_u)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_older_u)))
medcost_joint_mVax_pVax_u_older <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_older_u)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_older_u)))

# Calculate medical costs across efficacy from 0 to 100%
medcost_er_pVax <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_p_cases)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_p_cases)))
medcost_er_llAb_pVax <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_lp_cases)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_lp_cases)))
medcost_er_mVax_pVax <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_mp_cases)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_mp_cases)))

medcost_er_pVax_older <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_p_older_cases)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_p_older_cases)))
medcost_er_llAb_pVax_older <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_lp_older_cases)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_lp_older_cases)))
medcost_er_mVax_pVax_older <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_mp_older_cases)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_mp_older_cases)))

# Calculate total intervention costs
totalcost_no <- medcost_no
totalcost_llAb <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + medcost_llAb
totalcost_mVax <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + medcost_mVax
totalcost_pVax <- sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_pVax
totalcost_pVax_intfhi <- sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_pVax_intfhi
totalcost_pVax_intflo <- sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_pVax_intflo
totalcost_joint_llAb_pVax <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_joint_llAb_pVax
totalcost_joint_mVax_pVax <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_joint_mVax_pVax
totalcost_intfhi_llAb_pVax <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_intfhi_llAb_pVax
totalcost_intfhi_mVax_pVax <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_intfhi_mVax_pVax
totalcost_intflo_llAb_pVax <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_intflo_llAb_pVax
totalcost_intflo_mVax_pVax <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_intflo_mVax_pVax

totalcost_pVax_older <- sum(pVax_older_admin * coverage[3] * num_infants * (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_pVax_older
totalcost_joint_llAb_pVax_older <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_older_admin * coverage[3] * num_infants *  (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_joint_llAb_pVax_older
totalcost_joint_mVax_pVax_older <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_older_admin * coverage[3] * num_infants *  (0.5*cost_EPI + 0.5*1.35 +cost_prod)) + medcost_joint_mVax_pVax_older

# Calculate total intervention costs with uncertainty
totalcost_no_u <- medcost_no_u
totalcost_llAb_u <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + medcost_llAb_u
totalcost_mVax_u <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + medcost_mVax_u
totalcost_pVax_u <- sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_pVax_u
totalcost_pVax_u_SA <- sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_pVax_u_SA
totalcost_joint_llAb_pVax_u <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_joint_llAb_pVax_u
totalcost_joint_mVax_pVax_u <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_joint_mVax_pVax_u

totalcost_pVax_older_u <- sum(pVax_older_admin * coverage[3] * num_infants * (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_pVax_u_older
totalcost_joint_llAb_pVax_older_u <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * coverage[3] * num_infants * (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_joint_llAb_pVax_u_older
totalcost_joint_mVax_pVax_older_u <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * coverage[3] * num_infants * (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_joint_mVax_pVax_u_older

# Calculate total intervention costs across efficacy from 0 to 100%
totalcost_er_pVax <-  sum(pVax_admin * coverage[1] * num_infants * costs[1]) + medcost_er_pVax
totalcost_er_llAb_pVax <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_er_llAb_pVax
totalcost_er_mVax_pVax <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_er_mVax_pVax

totalcost_er_pVax_older <- sum(pVax_older_admin * coverage[1] * num_infants * (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_er_pVax_older
totalcost_er_llAb_pVax_older <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_older_admin * coverage[3] * num_infants * (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_er_llAb_pVax_older
totalcost_er_mVax_pVax_older <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_older_admin * coverage[3] * num_infants * (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_er_mVax_pVax_older

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

# Calculate probability of each strategy being optimal across WTP

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

# Calculate the probability of each strategy being optimal at WTP = $5000
# and as the efficacy in pVax as combo strategy at 10 & 14 wks (driven by interference) increases
NHB_no_5k <-rep.col(NHB_func(prep_no, WTP_5k), length(eff_red))
NHB_l_5k <- rep.col(NHB_func(prep_llAb, WTP_5k), length(eff_red))
NHB_m_5k <- rep.col(NHB_func(prep_mVax, WTP_5k), length(eff_red))
NHB_p_5k <- rep.col(NHB_func(prep_pVax, WTP_5k), length(eff_red))
NHB_p_older_5k <- rep.col(NHB_func(prep_pVax_older, WTP_5k), length(eff_red))
NHB_lp_older_5k <- rep.col(NHB_func(prep_joint_llAb_pVax_older, WTP_5k), length(eff_red))
NHB_mp_older_5k <- rep.col(NHB_func(prep_joint_mVax_pVax_older, WTP_5k), length(eff_red))

DALYS_lost_er_no <- rep.col(DALYS_lost_no_u, length(eff_red))
totalcost_er_no <- rep.col(totalcost_no_u, length(eff_red))

prep_p_5k <- array(c(DALYS_lost_er_no, DALYS_er_pVax, totalcost_er_pVax, totalcost_er_no), dim = c(trials, length(eff_red), 4))
prep_lp_5k <- array(c(DALYS_lost_er_no, DALYS_er_llAb_pVax, totalcost_er_llAb_pVax, totalcost_er_no), dim = c(trials, length(eff_red), 4))
prep_mp_5k <- array(c(DALYS_lost_er_no, DALYS_er_mVax_pVax, totalcost_er_mVax_pVax, totalcost_er_no), dim = c(trials, length(eff_red), 4))

NHB_p_5k_er <- NHB_func_er(prep_p_5k, WTP_5k)
NHB_lp_5k <- NHB_func_er(prep_lp_5k, WTP_5k)
NHB_mp_5k <- NHB_func_er(prep_mp_5k, WTP_5k)

compare_NHB_5k <- array(c(NHB_no_5k, NHB_l_5k, NHB_m_5k, NHB_p_5k, NHB_lp_5k, NHB_mp_5k, NHB_p_older_5k, NHB_lp_older_5k, NHB_mp_older_5k), dim = c(trials, length(eff_red), 9))
win_NHB_5k <- apply(compare_NHB_5k, MARGIN = c(1,2), which.max)

pO_no_5k <- rep(0, length(eff_red))
for(no in 1: length(eff_red)){
  pO_no_5k[no] <- sum(win_NHB_5k[,no] == 1)/trials
}

pO_llAb_5k <- rep(0, length(eff_red))
for(Ol in 1: length(eff_red)){
  pO_llAb_5k[Ol] <- sum(win_NHB_5k[,Ol] == 2)/trials
}

pO_mVax_5k <- rep(0, length(eff_red))
for(Om in 1: length(eff_red)){
  pO_mVax_5k[Om] <- sum(win_NHB_5k[,Om] == 3)/trials
}

pO_pVax_5k <- rep(0, length(eff_red))
for(Op in 1: length(eff_red)){
  pO_pVax_5k[Op] <- sum(win_NHB_5k[,Op] == 4)/trials
}

pO_llAb_pVax_5k <- rep(0, length(eff_red))
for(lp in 1: length(eff_red)){
  pO_llAb_pVax_5k[lp] <- sum(win_NHB_5k[,lp] == 5)/trials
}

pO_mVax_pVax_5k <- rep(0, length(eff_red))
for(Omp in 1: length(eff_red)){
  pO_mVax_pVax_5k[Omp] <- sum(win_NHB_5k[,Omp] == 6)/trials
}

pO_pVax_older_5k <- rep(0, length(eff_red))
for(Opo in 1: length(eff_red)){
  pO_pVax_older_5k[Opo] <- sum(win_NHB_5k[,Opo] == 7)/trials
}

pO_llAb_pVax_older_5k <- rep(0, length(eff_red))
for(zp in 1: length(eff_red)){
  pO_llAb_pVax_older_5k[zp] <- sum(win_NHB_5k[,zp] == 8)/trials
}

pO_mVax_pVax_older_5k <- rep(0, length(eff_red))
for(Ompo in 1: length(eff_red)){
  pO_mVax_pVax_older_5k[Ompo] <- sum(win_NHB_5k[,Ompo] == 9)/trials
}

##
# When efficacy reduction is driven by infant immaturity
compare_NHB_5k_ser <- array(c(NHB_no_5k, NHB_l_5k, NHB_m_5k, NHB_p_5k_er, NHB_lp_5k, NHB_mp_5k, NHB_p_older_5k, NHB_lp_older_5k, NHB_mp_older_5k), dim = c(trials, length(eff_red), 9))
win_NHB_5k_ser <- apply(compare_NHB_5k_ser, MARGIN = c(1,2), which.max)

pO_no_5k_ser <- rep(0, length(eff_red))
for(no in 1: length(eff_red)){
  pO_no_5k_ser[no] <- sum(win_NHB_5k_ser[,no] == 1)/trials
}

pO_llAb_5k_ser <- rep(0, length(eff_red))
for(Ol in 1: length(eff_red)){
  pO_llAb_5k_ser[Ol] <- sum(win_NHB_5k_ser[,Ol] == 2)/trials
}

pO_mVax_5k_ser <- rep(0, length(eff_red))
for(Om in 1: length(eff_red)){
  pO_mVax_5k_ser[Om] <- sum(win_NHB_5k_ser[,Om] == 3)/trials
}

pO_pVax_5k_ser <- rep(0, length(eff_red))
for(Op in 1: length(eff_red)){
  pO_pVax_5k_ser[Op] <- sum(win_NHB_5k_ser[,Op] == 4)/trials
}

pO_llAb_pVax_5k_ser <- rep(0, length(eff_red))
for(Olp in 1: length(eff_red)){
  pO_llAb_pVax_5k_ser[Olp] <- sum(win_NHB_5k_ser[,Olp] == 5)/trials
}

pO_mVax_pVax_5k_ser <- rep(0, length(eff_red))
for(Omp in 1: length(eff_red)){
  pO_mVax_pVax_5k_ser[Omp] <- sum(win_NHB_5k_ser[,Omp] == 6)/trials
}

pO_pVax_older_5k_ser <- rep(0, length(eff_red))
for(Opo in 1: length(eff_red)){
  pO_pVax_older_5k_ser[Opo] <- sum(win_NHB_5k_ser[,Opo] == 7)/trials
}

pO_llAb_pVax_older_5k_ser <- rep(0, length(eff_red))
for(Olpo in 1: length(eff_red)){
  pO_llAb_pVax_older_5k_ser[Olpo] <- sum(win_NHB_5k_ser[,Olpo] == 8)/trials
}

pO_mVax_pVax_older_5k_ser <- rep(0, length(eff_red))
for(Ompo in 1: length(eff_red)){
  pO_mVax_pVax_older_5k_ser[Ompo] <- sum(win_NHB_5k_ser[,Ompo] == 9)/trials
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
# Calculate ICER moving from pVax to llAb + pVax with 20% interference
ICER_intflo_llAb_pVax <- (totalcost_intflo_llAb_pVax - totalcost_pVax) / (DALYS_lost_pVax - DALYS_lost_intflo_llAb_pVax)

interventions <- data.frame(int_names, efficacy, duration, coverage, additional_cost, deaths_averted, DALYs_averted, ICERs_base)

#####
# for two-way sensitivity analysis figure
# cost of adding new EPI visit vs. pVax vaccine efficacy at 10 & 14 wks.
# to calculate NHBs, inputs needed are total costs and DALYs lost





