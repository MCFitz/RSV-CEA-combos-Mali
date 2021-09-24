# Master Script
trials <- 1000

# load required packages
library(ggplot2)
library(abind)
library(readr)
library("triangle")
library("mgcv")

# generic functions for code
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

getmode <- function (x) {
  xtab <- table(x)
  modes <- xtab[max(xtab)==xtab]
  themodes <- as.numeric(names(modes))
  tm <- ifelse(length(themodes) ==1, themodes, 10)
  tm
}

# generating 95% confidence intervals
CI_func <- function(param) {
  c(quantile(param, probs = 0.05), quantile(param, probs = 0.95))
}

# source all necessary scripts
source("attack_rates.R")
source("health_functions.R")
source("health_outcome_parameters.R")
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

cov_pVax_o <- 0.70

# Calculate number of RSV cases by age under status quo and each intervention
cases_no_age <- RSVcases(pd_calc(0, 0, AR_y_bc, empty_year_cohort), num_infants, mort_mat)
cases_llAb_age <- RSVcases(pd_calc(efficacy[1], coverage[1], AR_y_bc, mat_eff_llAb), num_infants, mort_mat)
cases_mVax_age <- RSVcases(pd_calc(efficacy[2], coverage[2], AR_y_bc, mat_eff_mVax), num_infants, mort_mat)
cases_pVax_age <- RSVcases(pd_calc(efficacy[3], coverage[3], AR_y_bc, mat_eff_pVax), num_infants, mort_mat)
cases_joint_llAb_pVax_age <-  RSVcases(pd_joint(efficacy[1], efficacy[4], coverage[1], coverage[3], AR_y_bc, mat_eff_llAb, mat_eff_pVax), num_infants, mort_mat)
cases_joint_mVax_pVax_age <- RSVcases(pd_joint(efficacy[2], efficacy[5], coverage[2], coverage[3], AR_y_bc, mat_eff_mVax, mat_eff_pVax), num_infants, mort_mat)
cases_pVax_older_age <- RSVcases(pd_calc(efficacy[3], cov_pVax_o, AR_y_bc, mat_eff_older_pVax), num_infants, mort_mat)
cases_joint_llAb_pVax_older_age  <-  RSVcases(pd_joint(efficacy[1], efficacy[3], coverage[1], cov_pVax_o, AR_y_bc, mat_eff_llAb, mat_eff_older_pVax), num_infants, mort_mat)
cases_joint_mVax_pVax_older_age  <- RSVcases(pd_joint(efficacy[2], efficacy[3], coverage[2], cov_pVax_o, AR_y_bc, mat_eff_mVax, mat_eff_older_pVax), num_infants, mort_mat)

cases_pVax_intflo_age <- RSVcases(pd_calc(efficacy[8], coverage[3], AR_y_bc, mat_eff_pVax), num_infants, mort_mat)
cases_pVax_intfhi_age <- RSVcases(pd_calc(efficacy[6], coverage[3], AR_y_bc, mat_eff_pVax), num_infants, mort_mat)
cases_intflo_llAb_pVax_age <- RSVcases(pd_joint(efficacy[1], efficacy[8], coverage[1], coverage[3], AR_y_bc, mat_eff_llAb, mat_eff_pVax), num_infants, mort_mat)
cases_intflo_mVax_pVax_age <- RSVcases(pd_joint(efficacy[2], efficacy[9], coverage[2], coverage[3], AR_y_bc, mat_eff_mVax, mat_eff_pVax), num_infants, mort_mat)
cases_intfhi_llAb_pVax_age <- RSVcases(pd_joint(efficacy[1], efficacy[6], coverage[1], coverage[3], AR_y_bc, mat_eff_llAb, mat_eff_pVax), num_infants, mort_mat)
cases_intfhi_mVax_pVax_age <- RSVcases(pd_joint(efficacy[2], efficacy[7], coverage[2], coverage[3], AR_y_bc, mat_eff_mVax, mat_eff_pVax), num_infants, mort_mat)

# Sum the cases by age vectors to get total number of cases
cases_no <- sum(cases_no_age[[2]])
cases_llAb <- sum(cases_llAb_age[[2]])
cases_mVax <- sum(cases_mVax_age[[2]])
cases_pVax <- sum(cases_pVax_age[[2]])
cases_joint_llAb_pVax <- sum(cases_joint_llAb_pVax_age[[2]])
cases_joint_mVax_pVax <- sum(cases_joint_mVax_pVax_age[[2]])
cases_pVax_older <- sum(cases_pVax_older_age[[2]])
cases_joint_llAb_pVax_older <- sum(cases_joint_llAb_pVax_age[[2]])
cases_joint_mVax_pVax_older <- sum(cases_joint_mVax_pVax_older_age[[2]])

cases_pVax_intflo <- sum(cases_pVax_intflo_age[[2]])
cases_pVax_intfhi <- sum(cases_pVax_intfhi_age[[2]])
cases_intflo_llAb_pVax <- sum(cases_intflo_llAb_pVax_age[[2]])
cases_intflo_mVax_pVax <- sum(cases_intflo_mVax_pVax_age[[2]])
cases_intfhi_llAb_pVax <- sum(cases_intfhi_llAb_pVax_age[[2]])
cases_intfhi_mVax_pVax <- sum(cases_intfhi_mVax_pVax_age[[2]])

# RSV-LRTI episiodes by age in months
LRTI_no_age <- pneum_func(p_pneum, cases_no_age[[2]])
LRTI_llAb_age <- pneum_func(p_pneum, cases_llAb_age[[2]])
LRTI_mVax_age <- pneum_func(p_pneum, cases_mVax_age[[2]])
LRTI_pVax_age <- pneum_func(p_pneum, cases_pVax_age[[2]])
LRTI_joint_llAb_pVax_age <- pneum_func(p_pneum, cases_joint_llAb_pVax_age[[2]])
LRTI_joint_mVax_pVax_age <- pneum_func(p_pneum, cases_joint_mVax_pVax_age[[2]])
LRTI_pVax_older_age <- pneum_func(p_pneum, cases_pVax_older_age[[2]])
LRTI_joint_llAb_pVax_older_age <- pneum_func(p_pneum, cases_joint_llAb_pVax_older_age[[2]])
LRTI_joint_mVax_pVax_older_age <- pneum_func(p_pneum, cases_joint_mVax_pVax_older_age[[2]])

LRTI_no_age_bin <- c(LRTI_no_age[1], mean(LRTI_no_age[2:3]), mean(LRTI_no_age[2:3]), rep(mean(LRTI_no_age[4:6]), 3),
                    rep(mean(LRTI_no_age[7:9]), 3), rep(mean(LRTI_no_age[10:12]), 3),
                    rep(mean(LRTI_no_age[13:24]), 12), rep(mean(LRTI_no_age[25:36]), 12))

# Li Hosps by month
Li_hosps <- p_inpatient_shi_li* mean(num_infants)
rsv_lrti_hosps <- Li_hosps/LRTI_no_age_bin

source("hospitalizations by age.R")
source("optimality_curve_code.R")

# Calculating number of cases across pVax efficacy (for 10 & 14 wk doses) from 0 to 100% (due to interference)
eff_red <- seq(0.0, 1, by = 0.01)

# pediatric vaccine
er_p_cases <- matrix(NA, trials, length(eff_red))
for (p in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_p <- pd_calc(eff_red[er], coverage[3], AR_y_u[,,p], mat_eff_pVax)
    er_p_cases[p, er] <- RSVcases(temp_p, babies = num_infants, mort_mat)
  }}

# long-acting Ab plus pediatric vaccine
er_lp_cases <- matrix(NA, trials, length(eff_red))
for (lp in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_lp <- pd_joint(efficacy[3], eff_red[er], coverage[1], coverage[3], AR_y_u[,,lp], 
                     mat_eff_llAb, mat_eff_pVax)
    er_lp_cases[lp, er] <- RSVcases(temp_lp, babies = num_infants, mort_mat)
}}

# maternal vaccine plus pediatric vaccine
er_mp_cases <- matrix(NA, trials, length(eff_red))
for (mp in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_mp <- pd_joint(efficacy[3], eff_red[er], coverage[2], coverage[3], AR_y_u[,,mp],
                        mat_eff_mVax, mat_eff_pVax)
er_mp_cases[mp, er] <- RSVcases(temp_mp, babies = num_infants, mort_mat)
  }}

# pediatric vaccine admin to older infants
er_p_older_cases <- matrix(NA, trials, length(eff_red))
for (p in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_p <- pd_calc(efficacy[3], cov_pVax_o, AR_y_u[,,p], mat_eff_older_pVax)
    er_p_older_cases[p, er] <- RSVcases(temp_p, babies = num_infants, mort_mat)
  }}
# long-acting Ab plus pediatric vaccine admin to older infants
er_lp_older_cases <- matrix(NA, trials, length(eff_red))
for (lp in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_lp_older <- pd_joint(efficacy[1], efficacy[3], coverage[1], cov_pVax_o, AR_y_u[,,lp], 
                        mat_eff_llAb, mat_eff_older_pVax)
    er_lp_older_cases[lp, er] <- RSVcases(temp_lp_older, babies = num_infants, mort_mat)
  }}
# maternal vaccine plus pediatric vaccine admin to older infants
er_mp_older_cases <- matrix(NA, trials, length(eff_red))
for (mp in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_mp_older <- pd_joint(efficacy[2], efficacy[3], coverage[2], cov_pVax_o, AR_y_u[,,mp],
                        mat_eff_mVax, mat_eff_older_pVax)
    er_mp_older_cases[mp, er] <- RSVcases(temp_mp_older, babies = num_infants, mort_mat)
  }}

# hospitalizations point estimate
inpat_no_age <- inpat_func(p_hosp_new, pneum_func(p_pneum, cases_no_age[[2]]))
inpat_llAb_age <- inpat_func(p_hosp_new, pneum_func(p_pneum, cases_llAb_age[[2]]))
inpat_mVax_age <- inpat_func(p_hosp_new, pneum_func(p_pneum, cases_mVax_age[[2]]))
inpat_pVax_age <- inpat_func(p_hosp_new, pneum_func(p_pneum, cases_pVax_age[[2]]))
inpat_joint_llAb_pVax_age <- inpat_func(p_hosp_new, pneum_func(p_pneum, cases_joint_llAb_pVax_age[[2]]))
inpat_joint_mVax_pVax_age <- inpat_func(p_hosp_new, pneum_func(p_pneum, cases_joint_mVax_pVax_age[[2]]))
inpat_pVax_older_age <- inpat_func(p_hosp_new, pneum_func(p_pneum, cases_pVax_older_age[[2]]))
inpat_joint_llAb_pVax_older_age <- inpat_func(p_hosp_new, pneum_func(p_pneum, cases_joint_llAb_pVax_older_age[[2]]))
inpat_joint_mVax_pVax_older_age <- inpat_func(p_hosp_new, pneum_func(p_pneum, cases_joint_mVax_pVax_older_age[[2]]))

# sum the rows
inpat_no <- sum(inpat_no_age)
inpat_llAb <- sum(inpat_llAb_age)
inpat_mVax <- sum(inpat_mVax_age)
inpat_pVax <- sum(inpat_pVax_age)
inpat_joint_llAb_pVax <- sum(inpat_joint_llAb_pVax_age)
inpat_joint_mVax_pVax <- sum(inpat_joint_mVax_pVax_age)
inpat_pVax_older <- sum(inpat_pVax_older_age)
inpat_joint_llAb_pVax_older <- sum(inpat_joint_llAb_pVax_older_age)
inpat_joint_mVax_pVax_older <- sum(inpat_joint_mVax_pVax_older_age)

# deaths by age in months when using age-stratified inpatient rates
# point estimates
deaths_no_age <- mort_inpat_func(CFR_by_age, inpat_no_age, CFR_nr_care, nr_care_func(inpat_no_age))
deaths_llAb_age <- mort_inpat_func(CFR_by_age, inpat_llAb_age, CFR_nr_care, nr_care_func(inpat_llAb_age))
deaths_mVax_age <- mort_inpat_func(CFR_by_age, inpat_mVax_age, CFR_nr_care, nr_care_func(inpat_mVax_age))
deaths_pVax_age <- mort_inpat_func(CFR_by_age, inpat_pVax_age, CFR_nr_care, nr_care_func(inpat_pVax_age))
deaths_joint_llAb_pVax_age <- mort_inpat_func(CFR_by_age, inpat_joint_llAb_pVax_age, CFR_nr_care, nr_care_func(inpat_joint_llAb_pVax_age))
deaths_joint_mVax_pVax_age <- mort_inpat_func(CFR_by_age, inpat_joint_mVax_pVax_age, CFR_nr_care, nr_care_func(inpat_joint_mVax_pVax_age))
deaths_pVax_older_age <- mort_inpat_func(CFR_by_age, inpat_pVax_older_age, CFR_nr_care, nr_care_func(inpat_pVax_older_age))
deaths_joint_llAb_pVax_older_age <- mort_inpat_func(CFR_by_age, inpat_joint_llAb_pVax_older_age, CFR_nr_care, nr_care_func(inpat_joint_llAb_pVax_older_age))
deaths_joint_mVax_pVax_older_age <- mort_inpat_func(CFR_by_age, inpat_joint_mVax_pVax_older_age, CFR_nr_care, nr_care_func(inpat_joint_mVax_pVax_older_age))

# total number of deaths
# sum the number of deaths by age in months
deaths_no <- sum(deaths_no_age)
deaths_llAb <- sum(deaths_llAb_age)
deaths_mVax <- sum(deaths_mVax_age)
deaths_pVax <- sum(deaths_pVax_age)
deaths_joint_llAb_pVax <- sum(deaths_joint_llAb_pVax_age)
deaths_joint_mVax_pVax <- sum(deaths_joint_mVax_pVax_age)
deaths_pVax_older <- sum(deaths_pVax_older_age)
deaths_joint_llAb_pVax_older <- sum(deaths_joint_llAb_pVax_older_age)
deaths_joint_mVax_pVax_older <- sum(deaths_joint_mVax_pVax_older_age)

# NOTE: these are for the CE plane figure, needs to be updated
# deaths_pVax_intflo <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intflo)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intflo)))
# deaths_pVax_intfhi <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intfhi)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intfhi)))
# deaths_intfhi_llAb_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_llAb_pVax )), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_llAb_pVax)))
# deaths_intfhi_mVax_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_mVax_pVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_mVax_pVax)))
# deaths_intflo_llAb_pVax <-mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_llAb_pVax )), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_intflo_llAb_pVax)))
# deaths_intflo_mVax_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_mVax_pVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_intflo_mVax_pVax)))

### Health Outcomes by year [cases, hospitalizations, deaths]
## create lists of deaths by year (1 through 3), for all trials
outcome_by_year_func <- function(outcomes_input){
  list(apply(outcomes_input[, 1:12], MARGIN = 1, sum), apply(outcomes_input[, 13:24], MARGIN = 1, sum), apply(outcomes_input[, 25:36], MARGIN = 1, sum))
}

cases_year_no_u <- outcome_by_year_func(cases_no_u_age[[2]])
cases_year_llAb_u <- outcome_by_year_func(cases_llAb_u_age[[2]])
cases_year_mVax_u <- outcome_by_year_func(cases_mVax_u_age[[2]])
cases_year_pVax_u <- outcome_by_year_func(cases_pVax_u_age[[2]])
cases_year_joint_llAb_pVax_u <- outcome_by_year_func(cases_joint_llAb_pVax_u_age[[2]])
cases_year_joint_mVax_pVax_u <- outcome_by_year_func(cases_joint_mVax_pVax_u_age[[2]])
cases_year_pVax_older_u <- outcome_by_year_func(cases_pVax_older_u_age[[2]])
cases_year_joint_llAb_pVax_older_u <- outcome_by_year_func(cases_joint_llAb_pVax_older_u_age[[2]])
cases_year_joint_mVax_pVax_older_u <- outcome_by_year_func(cases_joint_mVax_pVax_older_u_age[[2]])

hosp_year_no_u <- outcome_by_year_func(inpat_no_u_age)
hosp_year_llAb_u <- outcome_by_year_func(inpat_llAb_u_age)
hosp_year_mVax_u <- outcome_by_year_func(inpat_mVax_u_age)
hosp_year_pVax_u <- outcome_by_year_func(inpat_pVax_u_age)
hosp_year_joint_llAb_pVax_u <- outcome_by_year_func(inpat_joint_llAb_pVax_u_age)
hosp_year_joint_mVax_pVax_u <- outcome_by_year_func(inpat_joint_mVax_pVax_u_age)
hosp_year_pVax_older_u <- outcome_by_year_func(inpat_pVax_older_u_age)
hosp_year_joint_llAb_pVax_older_u <- outcome_by_year_func(inpat_joint_llAb_pVax_older_u_age)
hosp_year_joint_mVax_pVax_older_u <- outcome_by_year_func(inpat_joint_mVax_pVax_older_u_age)

deaths_year_no_u <- outcome_by_year_func(deaths_no_u_age)
deaths_year_llAb_u <- outcome_by_year_func(deaths_llAb_u_age)
deaths_year_mVax_u <- outcome_by_year_func(deaths_mVax_u_age)
deaths_year_pVax_u <- outcome_by_year_func(deaths_pVax_u_age)
deaths_year_joint_llAb_pVax_u <- outcome_by_year_func(deaths_joint_llAb_pVax_u_age)
deaths_year_joint_mVax_pVax_u <- outcome_by_year_func(deaths_joint_mVax_pVax_u_age)
deaths_year_pVax_older_u <-outcome_by_year_func(deaths_pVax_older_u_age)
deaths_year_joint_llAb_pVax_older_u <- outcome_by_year_func(deaths_joint_llAb_pVax_older_u_age)
deaths_year_joint_mVax_pVax_older_u <- outcome_by_year_func(deaths_joint_mVax_pVax_older_u_age)

# Calculate number of deaths across pVax efficacy from 0 to 100%
# deaths_no_u_age <- t(mort_inpat_func(CFR_sub, inpat_no_u, CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_no_u, length(AR_age_weights))) * AR_age_weights))))

# deaths_er_pVax <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_p_cases)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, er_p_cases)))
# deaths_er_llAb_pVax <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_lp_cases)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, er_lp_cases)))
# deaths_er_mVax_pVax <-  mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_mp_cases)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, er_mp_cases)))
# 
# deaths_er_pVax_older <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_p_older_cases)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, er_p_older_cases)))
# deaths_er_llAb_pVax_older <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_lp_older_cases)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, er_lp_older_cases)))
# deaths_er_mVax_pVax_older <-  mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_mp_older_cases)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, er_mp_older_cases)))

# Calculate DALYs lost under status quo and each intervention
DALYS_lost_no <- YLL_func(deaths_no) + YLD_func(inpat_no, deaths_no, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_no[[2]]), dw_LRTI_mod)
DALYS_lost_llAb <- YLL_func(deaths_llAb) + YLD_func(inpat_llAb, deaths_llAb, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_llAb[[2]]), dw_LRTI_mod)
DALYS_lost_mVax <- YLL_func(deaths_mVax) + YLD_func(inpat_mVax, deaths_mVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_mVax[[2]]), dw_LRTI_mod)
DALYS_lost_pVax <- YLL_func(deaths_pVax) + YLD_func(inpat_pVax, deaths_pVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_pVax[[2]]), dw_LRTI_mod)
DALYS_lost_joint_llAb_pVax <- YLL_func(deaths_joint_llAb_pVax) + YLD_func(inpat_joint_llAb_pVax, deaths_joint_llAb_pVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_joint_llAb_pVax[[2]]), dw_LRTI_mod)
DALYS_lost_joint_mVax_pVax <- YLL_func(deaths_joint_mVax_pVax) + YLD_func(inpat_joint_mVax_pVax, deaths_joint_mVax_pVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_joint_mVax_pVax[[2]]), dw_LRTI_mod)
DALYS_lost_pVax_older <- YLL_func(deaths_pVax_older) + YLD_func(inpat_pVax_older, deaths_pVax_older, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_pVax_older[[2]]), dw_LRTI_mod)
DALYS_lost_joint_llAb_pVax_older <- YLL_func(deaths_joint_llAb_pVax_older) + YLD_func(inpat_joint_llAb_pVax_older, deaths_joint_llAb_pVax_older, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_joint_llAb_pVax_older[[2]]), dw_LRTI_mod)
DALYS_lost_joint_mVax_pVax_older <- YLL_func(deaths_joint_mVax_pVax_older) + YLD_func(inpat_joint_mVax_pVax_older, deaths_joint_mVax_pVax_older, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_joint_mVax_pVax_older[[2]]), dw_LRTI_mod)

# DALYS_lost_pVax_intflo <- YLL_func(deaths_pVax_intflo) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intflo[[2]])), deaths_pVax_intflo, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_pVax_intflo[[2]]), dw_LRTI_mod)
# DALYS_lost_pVax_intfhi <- YLL_func(deaths_pVax_intfhi) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intfhi[[2]])), deaths_pVax_intfhi, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_pVax_intfhi[[2]]), dw_LRTI_mod)   
# DALYS_lost_intfhi_llAb_pVax <- YLL_func(deaths_intfhi_llAb_pVax) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_llAb_pVax[[2]])), deaths_intfhi_llAb_pVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_intfhi_llAb_pVax[[2]]), dw_LRTI_mod)
# DALYS_lost_intfhi_mVax_pVax <- YLL_func(deaths_intfhi_mVax_pVax ) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_mVax_pVax[[2]])), deaths_intfhi_mVax_pVax , di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_intfhi_mVax_pVax[[2]]), dw_LRTI_mod)
# DALYS_lost_intflo_llAb_pVax <- YLL_func(deaths_intflo_llAb_pVax) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_llAb_pVax[[2]])), deaths_intfhi_llAb_pVax, di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_intflo_llAb_pVax[[2]]), dw_LRTI_mod)
# DALYS_lost_intflo_mVax_pVax <- YLL_func(deaths_intflo_mVax_pVax ) + YLD_func(inpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_mVax_pVax[[2]])), deaths_intflo_mVax_pVax , di_yrs, dw_LRTI_severe, pneum_func(p_pneum, cases_intflo_mVax_pVax[[2]]), dw_LRTI_mod)

# Calculate DALYs lost across pVax efficacy from 0 to 100%
# DALYS_er_pVax <- YLL_func(deaths_er_pVax) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_p_cases)), deaths_er_pVax, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, er_p_cases), dw_LRTI_mod_u)
# DALYS_er_llAb_pVax <-  YLL_func(deaths_er_llAb_pVax) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_lp_cases)), deaths_er_llAb_pVax, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, er_lp_cases), dw_LRTI_mod_u)
# DALYS_er_mVax_pVax <- YLL_func(deaths_er_mVax_pVax) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_mp_cases)), deaths_er_mVax_pVax , di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, er_mp_cases), dw_LRTI_mod_u)
# 
# DALYS_er_pVax_older <- YLL_func(deaths_er_pVax_older) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_p_older_cases)), deaths_er_pVax_older, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, er_p_older_cases), dw_LRTI_mod_u)
# DALYS_er_llAb_pVax_older <- YLL_func(deaths_er_llAb_pVax_older) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_lp_older_cases)), deaths_er_llAb_pVax_older, di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, er_lp_older_cases), dw_LRTI_mod_u)
# DALYS_er_mVax_pVax_older <- YLL_func(deaths_er_mVax_pVax_older) + YLD_func(inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_mp_older_cases)), deaths_er_mVax_pVax_older , di_yrs_u, dw_LRTI_severe_u, pneum_func(p_pneum_u, er_mp_older_cases), dw_LRTI_mod_u)

# Calculate medical costs
medcost_no <- medcost_func(cost_hosp, inpat_no, cost_outpatient, outpat_func(pneum_func(p_pneum, cases_no[[2]]), inpat_no))
medcost_llAb <- medcost_func(cost_hosp, inpat_llAb, cost_outpatient, outpat_func(pneum_func(p_pneum, cases_llAb[[2]]), inpat_llAb))
medcost_mVax <- medcost_func(cost_hosp, inpat_mVax, cost_outpatient, outpat_func(pneum_func(p_pneum, cases_mVax[[2]]), inpat_mVax))
medcost_pVax <- medcost_func(cost_hosp, inpat_pVax, cost_outpatient, outpat_func(pneum_func(p_pneum, cases_pVax[[2]]), inpat_pVax))
medcost_joint_llAb_pVax <- medcost_func(cost_hosp, inpat_joint_llAb_pVax, cost_outpatient, outpat_func(pneum_func(p_pneum, cases_joint_llAb_pVax[[2]]), inpat_joint_llAb_pVax))
medcost_joint_mVax_pVax <- medcost_func(cost_hosp, inpat_joint_mVax_pVax, cost_outpatient, outpat_func(pneum_func(p_pneum, cases_joint_mVax_pVax[[2]]), inpat_joint_mVax_pVax))
medcost_pVax_older <- medcost_func(cost_hosp, inpat_pVax_older, cost_outpatient, outpat_func(pneum_func(p_pneum, cases_pVax_older[[2]]), inpat_pVax_older))
medcost_joint_llAb_pVax_older <- medcost_func(cost_hosp, inpat_joint_llAb_pVax_older, cost_outpatient, outpat_func(pneum_func(p_pneum, cases_joint_llAb_pVax_older[[2]]), inpat_joint_llAb_pVax_older))
medcost_joint_mVax_pVax_older <- medcost_func(cost_hosp, inpat_joint_mVax_pVax_older, cost_outpatient, outpat_func(pneum_func(p_pneum, cases_joint_mVax_pVax_older[[2]]), inpat_joint_mVax_pVax_older))

# medcost_pVax_intfhi <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intfhi[[2]])), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intfhi[[2]])))
# medcost_pVax_intflo <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intflo[[2]])), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intflo[[2]])))
# medcost_intfhi_llAb_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_llAb_pVax[[2]])), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_llAb_pVax[[2]])))
# medcost_intfhi_mVax_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_mVax_pVax[[2]])), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_mVax_pVax[[2]])))
# medcost_intflo_llAb_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_llAb_pVax[[2]])), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_llAb_pVax[[2]])))
# medcost_intflo_mVax_pVax <- medcost_func(cost_hosp, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_mVax_pVax[[2]])), cost_outpatient, outpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_mVax_pVax[[2]])))

# Calculate medical costs across efficacy from 0 to 100%
# medcost_er_pVax <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_p_cases)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_p_cases)))
# medcost_er_llAb_pVax <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_lp_cases)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_lp_cases)))
# medcost_er_mVax_pVax <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_mp_cases)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_mp_cases)))
# 
# medcost_er_pVax_older <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_p_older_cases)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_p_older_cases)))
# medcost_er_llAb_pVax_older <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_lp_older_cases)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_lp_older_cases)))
# medcost_er_mVax_pVax_older <- medcost_func(cost_hosp_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_mp_older_cases)), cost_outpatient_u, outpat_func(p_inpatient_u, pneum_func(p_pneum_u, er_mp_older_cases)))

# Calculate total intervention costs
totalcost_no <- medcost_no
totalcost_llAb <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + medcost_llAb
totalcost_mVax <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + medcost_mVax
totalcost_pVax <- sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_pVax
# totalcost_pVax_intfhi <- sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_pVax_intfhi
# totalcost_pVax_intflo <- sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_pVax_intflo
totalcost_joint_llAb_pVax <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_joint_llAb_pVax
totalcost_joint_mVax_pVax <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_joint_mVax_pVax
# totalcost_intfhi_llAb_pVax <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_intfhi_llAb_pVax
# totalcost_intfhi_mVax_pVax <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_intfhi_mVax_pVax
# totalcost_intflo_llAb_pVax <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_intflo_llAb_pVax
# totalcost_intflo_mVax_pVax <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_intflo_mVax_pVax

totalcost_pVax_older <- sum(pVax_older_admin * cov_pVax_o * num_infants * (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_pVax_older
totalcost_joint_llAb_pVax_older <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_older_admin * cov_pVax_o * num_infants *  (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_joint_llAb_pVax_older
totalcost_joint_mVax_pVax_older <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_older_admin * cov_pVax_o * num_infants *  (0.5*cost_EPI + 0.5*1.35 +cost_prod)) + medcost_joint_mVax_pVax_older

# Calculate total intervention costs across efficacy from 0 to 100%
# totalcost_er_pVax <-  sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_er_pVax
# totalcost_er_llAb_pVax <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_er_llAb_pVax
# totalcost_er_mVax_pVax <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_er_mVax_pVax
# 
# totalcost_er_pVax_older <- sum(pVax_older_admin * cov_pVax_o * num_infants * (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_er_pVax_older
# totalcost_er_llAb_pVax_older <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_older_admin * cov_pVax_o * num_infants * (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_er_llAb_pVax_older
# totalcost_er_mVax_pVax_older <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_older_admin * cov_pVax_o * num_infants * (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_er_mVax_pVax_older

# Calculate the probability of each strategy being optimal at WTP = $5000
# and as the efficacy in pVax as combo strategy at 10 & 14 wks (driven by interference) increases
NHB_no_5k <-rep.col(NHB_func(prep_no, WTP_5k), length(eff_red))
NHB_l_5k <- rep.col(NHB_func(prep_llAb, WTP_5k), length(eff_red))
NHB_m_5k <- rep.col(NHB_func(prep_mVax, WTP_5k), length(eff_red))
NHB_p_5k <- rep.col(NHB_func(prep_pVax, WTP_5k), length(eff_red))
NHB_p_older_5k <- rep.col(NHB_func(prep_pVax_older, WTP_5k), length(eff_red))
NHB_lp_older_5k <- rep.col(NHB_func(prep_joint_llAb_pVax_older, WTP_5k), length(eff_red))
NHB_mp_older_5k <- rep.col(NHB_func(prep_joint_mVax_pVax_older, WTP_5k), length(eff_red))

# DALYS_lost_er_no <- rep.col(DALYS_lost_no_u, length(eff_red))
# totalcost_er_no <- rep.col(totalcost_no_u, length(eff_red))

# Calculate NHB for strategies in SA #4 at WTP  = 1 X GDP
NHB_no_GDP <- NHB_func(prep_no, CET_Mali_GDP)
NHB_m_GDP <- NHB_func(prep_mVax, CET_Mali_GDP)

# reduction in efficacy of pVax driven by immaturity
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

# output 
strategies <- c( "llAb", "mVax", "pVax 10 & 14 wk", "llAb + pVax 10 and 14 wk", "mVax + pVax 10 & 14 wk", "pVax 8 & 9 months", "llAb + pVax 8 & 9 months", "mVax + pVax 8 & 9 months")

# incremental cost to society
ics_func <- function(cost_sq, cost_int){
  cost_int - cost_sq
}

 totalcosts_vec <- c(totalcost_llAb, totalcost_mVax, totalcost_pVax, totalcost_joint_llAb_pVax,
                     totalcost_joint_mVax_pVax, totalcost_pVax_older, totalcost_joint_llAb_pVax_older,
                     totalcost_joint_mVax_pVax_older)
 
ics_vec <- ics_func(totalcost_no, totalcosts_vec)

#####

# donor costs
donorcost_llAb <- sum(llAb_admin * coverage[1] * num_infants * (cost_prod-0.20))
donorcost_mVax <- sum(mVax_admin * coverage[2] * num_infants * (cost_prod-0.20))
donorcost_pVax <- sum(pVax_admin * coverage[3] * num_infants * (cost_prod-0.20))
donorcost_joint_llAb_pVax <- sum(llAb_admin * coverage[1] * num_infants * (cost_prod-0.20)) + sum(pVax_admin * coverage[3] * num_infants * (cost_prod-0.20))
donorcost_joint_mVax_pVax <- sum(mVax_admin * coverage[2] * num_infants * (cost_prod-0.20)) + sum(pVax_admin * coverage[3] * num_infants * (cost_prod-0.20))
donorcost_pVax_older <- sum(pVax_older_admin * cov_pVax_o * num_infants * (cost_prod-0.20))
donorcost_joint_llAb_pVax_older <- sum(llAb_admin * coverage[1] * num_infants * (cost_prod-0.20)) + sum(pVax_older_admin * cov_pVax_o * num_infants *  (cost_prod-0.20))
donorcost_joint_mVax_pVax_older <- sum(mVax_admin * coverage[2] * num_infants * (cost_prod-0.20)) + sum(pVax_older_admin * cov_pVax_o * num_infants *  (cost_prod-0.20))

dnrcosts <- c(donorcost_llAb, donorcost_mVax, donorcost_pVax,
              donorcost_joint_llAb_pVax, donorcost_joint_mVax_pVax,
              donorcost_pVax_older, donorcost_joint_llAb_pVax_older,
              donorcost_joint_mVax_pVax_older)

##
gov_pt <- 0.20

govcost_llAb <- sum(llAb_admin * coverage[1] * num_infants * (cost_nd + gov_pt))
govcost_mVax <- sum(mVax_admin * coverage[2] * num_infants * (cost_nd + gov_pt))
govcost_pVax <- sum(pVax_admin * coverage[3] * num_infants * (cost_nd + gov_pt))
govcost_joint_llAb_pVax <- sum(llAb_admin * coverage[1] * num_infants * (cost_nd + gov_pt)) + sum(pVax_admin * coverage[3] * num_infants * (cost_nd + gov_pt))
govcost_joint_mVax_pVax <- sum(mVax_admin * coverage[2] * num_infants * (cost_nd + gov_pt)) + sum(pVax_admin * coverage[3] * num_infants * (cost_nd + gov_pt))
govcost_pVax_older <- sum(pVax_older_admin * cov_pVax_o * num_infants * (0.5*cost_EPI + 0.5*cost_nd + gov_pt))
govcost_joint_llAb_pVax_older <- sum(llAb_admin * coverage[1] * num_infants * (cost_nd + gov_pt)) + sum(pVax_older_admin * cov_pVax_o * num_infants *  (0.5*cost_EPI + 0.5*cost_nd + gov_pt))
govcost_joint_mVax_pVax_older <- sum(mVax_admin * coverage[2] * num_infants * (cost_nd + gov_pt)) + sum(pVax_older_admin * cov_pVax_o * num_infants *  (0.5*cost_EPI + 0.5*cost_nd + gov_pt))

gvcosts <- c(govcost_llAb, govcost_mVax, govcost_pVax, govcost_joint_llAb_pVax,
             govcost_joint_mVax_pVax, govcost_pVax_older,
             govcost_joint_llAb_pVax_older, govcost_joint_mVax_pVax_older)

output <- data.frame(strategies, gvcosts, dnrcosts, ics_vec)
