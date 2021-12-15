# Master Script
trials <- 1000
set.seed(281992)

# load required packages
library(ggplot2)
library(dplyr)
library(abind)
library(readr)
library("triangle")
library("mgcv")
library("RColorBrewer")
library(MetBrewer) 

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

# collapsing birth cohort structure to age by months structure
adj_func <- function(bic_mat){
  adj <- matrix(NA, nrow = 12, ncol = 36)
  for(i in 1:12){
    adj[i,] <- bic_mat[i ,i:(i+35)]
  }
  adj2 <- colSums(adj)
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
coverage <- c(0.830, 0.433, 0.77, NA, NA, NA, NA, NA, NA)
costs <- c(cost_prod + 1.35, cost_prod + 1.35, cost_prod + 1.35, NA, NA, NA, NA, NA, NA)

cov_pVax_o <- 0.70

# Calculate number of RSV cases by age under status quo and each intervention
# here we assume the interventions are preventing RSV cases
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
cases_no <- sum(cases_no_age)
cases_llAb <- sum(cases_llAb_age)
cases_mVax <- sum(cases_mVax_age)
cases_pVax <- sum(cases_pVax_age)
cases_joint_llAb_pVax <- sum(cases_joint_llAb_pVax_age)
cases_joint_mVax_pVax <- sum(cases_joint_mVax_pVax_age)
cases_pVax_older <- sum(cases_pVax_older_age)
cases_joint_llAb_pVax_older <- sum(cases_joint_llAb_pVax_age)
cases_joint_mVax_pVax_older <- sum(cases_joint_mVax_pVax_older_age)

cases_pVax_intflo <- sum(cases_pVax_intflo_age)
cases_pVax_intfhi <- sum(cases_pVax_intfhi_age)
cases_intflo_llAb_pVax <- sum(cases_intflo_llAb_pVax_age)
cases_intflo_mVax_pVax <- sum(cases_intflo_mVax_pVax_age)
cases_intfhi_llAb_pVax <- sum(cases_intfhi_llAb_pVax_age)
cases_intfhi_mVax_pVax <- sum(cases_intfhi_mVax_pVax_age)

# RSV-LRTI episiodes by age in months
LRTI_no_age <- pneum_func(p_pneum, cases_no_age)
LRTI_llAb_age <- pneum_func(p_pneum, cases_llAb_age)
LRTI_mVax_age <- pneum_func(p_pneum, cases_mVax_age)
LRTI_pVax_age <- pneum_func(p_pneum, cases_pVax_age)
LRTI_joint_llAb_pVax_age <- pneum_func(p_pneum, cases_joint_llAb_pVax_age)
LRTI_joint_mVax_pVax_age <- pneum_func(p_pneum, cases_joint_mVax_pVax_age)
LRTI_pVax_older_age <- pneum_func(p_pneum, cases_pVax_older_age)
LRTI_joint_llAb_pVax_older_age <- pneum_func(p_pneum, cases_joint_llAb_pVax_older_age)
LRTI_joint_mVax_pVax_older_age <- pneum_func(p_pneum, cases_joint_mVax_pVax_older_age)

# LRTI for no intervention base case, binned to match Shi age groups
LRTI_no_age_bin <- c(LRTI_no_age[1], mean(LRTI_no_age[2:3]), mean(LRTI_no_age[2:3]), rep(mean(LRTI_no_age[4:6]), 3),
                    rep(mean(LRTI_no_age[7:9]), 3), rep(mean(LRTI_no_age[10:12]), 3),
                    rep(mean(LRTI_no_age[13:24]), 12), rep(mean(LRTI_no_age[25:36]), 12))

# Shi Hosps by month
Shi_hosps <- p_inpatient_shi_li* mean(num_infants)
# Shi_hosps <- p_inpatient_gambia* mean(num_infants)
rsv_lrti_hosps <- Shi_hosps/LRTI_no_age_bin

## hospitalizations from Buchwald 0-6 months
Buchwald_hosps <- sum(LRTI_no_age[1:6] * p_inpatient) / sum(num_infants) * 1000
# number hospitalized given RSV-LRTI in Buchwald / total number of infants 

source("hospitalizations by age.R")
source("temp script for hosps.R")
source("optimality_curve_code.R")

# Calculating number of LRTI episodes across pVax efficacy (for 10 & 14 wk doses) from 0 to 100% (due to interference)
eff_red <- seq(0.0, 1, by = 0.01)

# pediatric vaccine
DALYS_lost_er_p <- matrix(NA, trials, length(eff_red))
medcost_er_p <- matrix(NA, trials, length(eff_red))
for (p in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_LRTI_p <- LRTI_func(eff_red[er], coverage[3], mat_eff_pVax, p_pneum_u[p], cases_no_u_bic[,,p]) # number of LRTI episodes
    temp_adj_LRTI_p <- adj_func(temp_LRTI_p)
    temp_inpat_p <- inpat_func(p_hosp_new, temp_adj_LRTI_p) # number of inpatient episodes
    temp_outpat_p <- outpat_func(temp_adj_LRTI_p, temp_inpat_p) # number of outpatient episodes
    temp_nrcare_p <- nr_care_func(temp_inpat_p) # number not receiving care
    temp_death_p <- mort_inpat_func(CFR_by_age_u[p,], temp_inpat_p, CFR_nr_care_u[p,], temp_nrcare_p) # number of deaths
    temp_YLL_p <- YLL_func(temp_death_p) # YLL
    temp_YLD_p <- YLD_func(temp_inpat_p, temp_death_p, di_yrs_u[p], dw_LRTI_severe_u[p], temp_adj_LRTI_p, dw_LRTI_mod_u[p]) # YLD
    DALYS_lost_er_p[p, er] <- sum(temp_YLD_p + temp_YLL_p) # DALYs lost
    medcost_er_p[p, er] <- sum(medcost_func(cost_hosp_u[p], temp_inpat_p, cost_outpatient_u[p], temp_outpat_p)) # medcosts
  }}


# long-acting Ab plus pediatric vaccine
DALYS_lost_er_lp <- matrix(NA, trials, length(eff_red))
medcost_er_lp <- matrix(NA, trials, length(eff_red))
for (lp in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_LRTI_lp <- LRTI_func_joint(efficacy[1], eff_red[er], coverage[1], coverage[3], mat_eff_llAb, mat_eff_pVax, p_pneum_u[lp], cases_no_u_bic[,,lp]) # number of LRTI episodes
    temp_adj_LRTI_lp <- adj_func(temp_LRTI_lp)
    temp_inpat_lp <- inpat_func(p_hosp_new, temp_adj_LRTI_lp) # number of inpatient episodes
    temp_outpat_lp <- outpat_func(temp_adj_LRTI_lp, temp_inpat_lp) # number of outpatient episodes
    temp_nrcare_lp <- nr_care_func(temp_inpat_lp) # number not receiving care
    temp_death_lp <- mort_inpat_func(CFR_by_age_u[lp,], temp_inpat_lp, CFR_nr_care_u[lp,], temp_nrcare_lp) # number of deaths
    temp_YLL_lp <- YLL_func(temp_death_lp) # YLL
    temp_YLD_lp <- YLD_func(temp_inpat_lp, temp_death_lp, di_yrs_u[lp], dw_LRTI_severe_u[lp], temp_adj_LRTI_lp, dw_LRTI_mod_u[lp]) # YLD
    DALYS_lost_er_lp[lp, er] <- sum(temp_YLD_lp + temp_YLL_lp) # DALYs lost
    medcost_er_lp[lp, er] <- sum(medcost_func(cost_hosp_u[lp], temp_inpat_lp, cost_outpatient_u[lp], temp_outpat_lp)) # medcosts
  }}

# maternal vaccine plus pediatric vaccine
DALYS_lost_er_mp <- matrix(NA, trials, length(eff_red))
medcost_er_mp <- matrix(NA, trials, length(eff_red))
for (mp in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_LRTI_mp <- LRTI_func_joint(efficacy[2], eff_red[er], coverage[2], coverage[3], mat_eff_mVax, mat_eff_pVax, p_pneum_u[mp], cases_no_u_bic[,,mp]) # number of LRTI episodes
    temp_adj_LRTI_mp <- adj_func(temp_LRTI_mp)
    temp_inpat_mp <- inpat_func(p_hosp_new, temp_adj_LRTI_mp) # number of inpatient episodes
    temp_outpat_mp <- outpat_func(temp_adj_LRTI_mp, temp_inpat_mp) # number of outpatient episodes
    temp_nrcare_mp <- nr_care_func(temp_inpat_mp) # number not receiving care
    temp_death_mp <- mort_inpat_func(CFR_by_age_u[mp,], temp_inpat_mp, CFR_nr_care_u[mp,], temp_nrcare_mp) # number of deaths
    temp_YLL_mp <- YLL_func(temp_death_mp) # YLL
    temp_YLD_mp <- YLD_func(temp_inpat_mp, temp_death_mp, di_yrs_u[mp], dw_LRTI_severe_u[mp], temp_adj_LRTI_mp, dw_LRTI_mod_u[mp]) # YLD
    DALYS_lost_er_mp[mp, er] <- sum(temp_YLD_mp + temp_YLL_mp) # DALYs lost
    medcost_er_mp[mp, er] <- sum(medcost_func(cost_hosp_u[mp], temp_inpat_mp, cost_outpatient_u[mp], temp_outpat_mp)) # medcosts
  }}

# pediatric vaccine admin to older infants
DALYS_lost_er_po <- matrix(NA, trials, length(eff_red))
medcost_er_po <- matrix(NA, trials, length(eff_red))
for (po in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_LRTI_po <- LRTI_func(efficacy[3], cov_pVax_o, mat_eff_older_pVax, p_pneum_u[po], cases_no_u_bic[,,po]) # number of LRTI episodes
    temp_adj_LRTI_po <- adj_func(temp_LRTI_po)
    temp_inpat_po <- inpat_func(p_hosp_new, temp_adj_LRTI_po) # number of inpatient episodes
    temp_outpat_po <- outpat_func(temp_adj_LRTI_po, temp_inpat_po) # number of outpatient episodes
    temp_nrcare_po <- nr_care_func(temp_inpat_po) # number not receiving care
    temp_death_po <- mort_inpat_func(CFR_by_age_u[po,], temp_inpat_po, CFR_nr_care_u[po,], temp_nrcare_po) # number of deaths
    temp_YLL_po <- YLL_func(temp_death_po) # YLL
    temp_YLD_po <- YLD_func(temp_inpat_po, temp_death_po, di_yrs_u[po], dw_LRTI_severe_u[po], temp_adj_LRTI_po, dw_LRTI_mod_u[po]) # YLD
    DALYS_lost_er_po[po, er] <- sum(temp_YLD_po + temp_YLL_po) # DALYs lost
    medcost_er_po[po, er] <- sum(medcost_func(cost_hosp_u[po], temp_inpat_po, cost_outpatient_u[po], temp_outpat_po)) # medcosts
  }}


# long-acting Ab plus pediatric vaccine admin to older infants
DALYS_lost_er_lpo <- matrix(NA, trials, length(eff_red))
medcost_er_lpo <- matrix(NA, trials, length(eff_red))
for (lpo in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_LRTI_lpo <- LRTI_func_joint(efficacy[1], efficacy[3], coverage[1], cov_pVax_o, mat_eff_llAb, mat_eff_older_pVax, p_pneum_u[lpo], cases_no_u_bic[,,lpo]) # number of LRTI episodes
    temp_adj_LRTI_lpo <- adj_func(temp_LRTI_lpo)
    temp_inpat_lpo <- inpat_func(p_hosp_new, temp_adj_LRTI_lpo) # number of inpatient episodes
    temp_outpat_lpo <- outpat_func(temp_adj_LRTI_lpo, temp_inpat_lpo) # number of outpatient episodes
    temp_nrcare_lpo <- nr_care_func(temp_inpat_lpo) # number not receiving care
    temp_death_lpo <- mort_inpat_func(CFR_by_age_u[lpo,], temp_inpat_lpo, CFR_nr_care_u[lpo,], temp_nrcare_lpo) # number of deaths
    temp_YLL_lpo <- YLL_func(temp_death_lpo) # YLL
    temp_YLD_lpo <- YLD_func(temp_inpat_lpo, temp_death_lpo, di_yrs_u[lpo], dw_LRTI_severe_u[lpo], temp_adj_LRTI_lpo, dw_LRTI_mod_u[lpo]) # YLD
    DALYS_lost_er_lpo[lpo, er] <- sum(temp_YLD_lpo + temp_YLL_lpo) # DALYs lost
    medcost_er_lpo[lpo, er] <- sum(medcost_func(cost_hosp_u[lpo], temp_inpat_lpo, cost_outpatient_u[lpo], temp_outpat_lpo)) # medcosts
  }}

# maternal vaccine plus pediatric vaccine admin to older infants
DALYS_lost_er_mpo <- matrix(NA, trials, length(eff_red))
medcost_er_mpo <- matrix(NA, trials, length(eff_red))
for (mpo in 1:trials) {
  for(er in 1:length(eff_red)){
    temp_LRTI_mpo <- LRTI_func_joint(efficacy[2], efficacy[3], coverage[1], cov_pVax_o, mat_eff_mVax, mat_eff_older_pVax, p_pneum_u[mpo], cases_no_u_bic[,,mpo]) # number of LRTI episodes
    temp_adj_LRTI_mpo <- adj_func(temp_LRTI_mpo)
    temp_inpat_mpo <- inpat_func(p_hosp_new, temp_adj_LRTI_mpo) # number of inpatient episodes
    temp_outpat_mpo <- outpat_func(temp_adj_LRTI_mpo, temp_inpat_mpo) # number of outpatient episodes
    temp_nrcare_mpo <- nr_care_func(temp_inpat_mpo) # number not receiving care
    temp_death_mpo <- mort_inpat_func(CFR_by_age_u[mpo,], temp_inpat_mpo, CFR_nr_care_u[mpo,], temp_nrcare_mpo) # number of deaths
    temp_YLL_mpo <- YLL_func(temp_death_mpo) # YLL
    temp_YLD_mpo <- YLD_func(temp_inpat_mpo, temp_death_mpo, di_yrs_u[mpo], dw_LRTI_severe_u[mpo], temp_adj_LRTI_mpo, dw_LRTI_mod_u[mpo]) # YLD
    DALYS_lost_er_mpo[mpo, er] <- sum(temp_YLD_mpo + temp_YLL_mpo) # DALYs lost
    medcost_er_mpo[mpo, er] <- sum(medcost_func(cost_hosp_u[mpo], temp_inpat_mpo, cost_outpatient_u[mpo], temp_outpat_mpo)) # medcosts
  }}

source("LRTI_prevention_scenario.R")
# hospitalizations point estimate
inpat_no_age <- inpat_func(p_hosp_new, LRTI_no_age)
inpat_llAb_age <- inpat_func(p_hosp_new, LRTI_llAb_age)
inpat_mVax_age <- inpat_func(p_hosp_new, LRTI_mVax_age)
inpat_pVax_age <- inpat_func(p_hosp_new, LRTI_pVax_age)
inpat_joint_llAb_pVax_age <- inpat_func(p_hosp_new, LRTI_joint_llAb_pVax_age)
inpat_joint_mVax_pVax_age <- inpat_func(p_hosp_new, LRTI_joint_mVax_pVax_age)
inpat_pVax_older_age <- inpat_func(p_hosp_new, LRTI_pVax_older_age)
inpat_joint_llAb_pVax_older_age <- inpat_func(p_hosp_new, LRTI_joint_llAb_pVax_older_age)
inpat_joint_mVax_pVax_older_age <- inpat_func(p_hosp_new, LRTI_joint_mVax_pVax_older_age)

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

# Calculate DALYs lost under status quo and each intervention
DALYS_lost_no <- YLL_func(deaths_no) + YLD_func(inpat_no, deaths_no, di_yrs, dw_LRTI_severe, LRTI_no, dw_LRTI_mod)
DALYS_lost_llAb <- YLL_func(deaths_llAb) + YLD_func(inpat_llAb, deaths_llAb, di_yrs, dw_LRTI_severe, LRTI_llAb, dw_LRTI_mod)
DALYS_lost_mVax <- YLL_func(deaths_mVax) + YLD_func(inpat_mVax, deaths_mVax, di_yrs, dw_LRTI_severe, LRTI_mVax, dw_LRTI_mod)
DALYS_lost_pVax <- YLL_func(deaths_pVax) + YLD_func(inpat_pVax, deaths_pVax, di_yrs, dw_LRTI_severe, LRTI_pVax, dw_LRTI_mod)
DALYS_lost_joint_llAb_pVax <- YLL_func(deaths_joint_llAb_pVax) + YLD_func(inpat_joint_llAb_pVax, deaths_joint_llAb_pVax, di_yrs, dw_LRTI_severe, LRTI_joint_llAb_pVax, dw_LRTI_mod)
DALYS_lost_joint_mVax_pVax <- YLL_func(deaths_joint_mVax_pVax) + YLD_func(inpat_joint_mVax_pVax, deaths_joint_mVax_pVax, di_yrs, dw_LRTI_severe, LRTI_joint_mVax_pVax, dw_LRTI_mod)
DALYS_lost_pVax_older <- YLL_func(deaths_pVax_older) + YLD_func(inpat_pVax_older, deaths_pVax_older, di_yrs, dw_LRTI_severe, LRTI_pVax_older, dw_LRTI_mod)
DALYS_lost_joint_llAb_pVax_older <- YLL_func(deaths_joint_llAb_pVax_older) + YLD_func(inpat_joint_llAb_pVax_older, deaths_joint_llAb_pVax_older, di_yrs, dw_LRTI_severe, LRTI_joint_llAb_pVax_older, dw_LRTI_mod)
DALYS_lost_joint_mVax_pVax_older <- YLL_func(deaths_joint_mVax_pVax_older) + YLD_func(inpat_joint_mVax_pVax_older, deaths_joint_mVax_pVax_older, di_yrs, dw_LRTI_severe, LRTI_joint_mVax_pVax_older, dw_LRTI_mod)

# Calculate medical costs
medcost_no <- medcost_func(cost_hosp, inpat_no, cost_outpatient, outpat_func(LRTI_no, inpat_no))
medcost_llAb <- medcost_func(cost_hosp, inpat_llAb, cost_outpatient, outpat_func(LRTI_llAb, inpat_llAb))
medcost_mVax <- medcost_func(cost_hosp, inpat_mVax, cost_outpatient, outpat_func(LRTI_mVax, inpat_mVax))
medcost_pVax <- medcost_func(cost_hosp, inpat_pVax, cost_outpatient, outpat_func(LRTI_pVax, inpat_pVax))
medcost_joint_llAb_pVax <- medcost_func(cost_hosp, inpat_joint_llAb_pVax, cost_outpatient, outpat_func(LRTI_joint_llAb_pVax, inpat_joint_llAb_pVax))
medcost_joint_mVax_pVax <- medcost_func(cost_hosp, inpat_joint_mVax_pVax, cost_outpatient, outpat_func(LRTI_joint_mVax_pVax, inpat_joint_mVax_pVax))
medcost_pVax_older <- medcost_func(cost_hosp, inpat_pVax_older, cost_outpatient, outpat_func(LRTI_pVax_older, inpat_pVax_older))
medcost_joint_llAb_pVax_older <- medcost_func(cost_hosp, inpat_joint_llAb_pVax_older, cost_outpatient, outpat_func(LRTI_joint_llAb_pVax_older, inpat_joint_llAb_pVax_older))
medcost_joint_mVax_pVax_older <- medcost_func(cost_hosp, inpat_joint_mVax_pVax_older, cost_outpatient, outpat_func(LRTI_joint_mVax_pVax_older, inpat_joint_mVax_pVax_older))


# Calculate total intervention costs
totalcost_no <- medcost_no
totalcost_llAb <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + medcost_llAb
totalcost_mVax <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + medcost_mVax
totalcost_pVax <- sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_pVax
totalcost_joint_llAb_pVax <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_joint_llAb_pVax
totalcost_joint_mVax_pVax <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_joint_mVax_pVax
totalcost_pVax_older <- sum(pVax_older_admin * cov_pVax_o * num_infants * (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_pVax_older
totalcost_joint_llAb_pVax_older <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_older_admin * cov_pVax_o * num_infants *  (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_joint_llAb_pVax_older
totalcost_joint_mVax_pVax_older <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_older_admin * cov_pVax_o * num_infants *  (0.5*cost_EPI + 0.5*1.35 +cost_prod)) + medcost_joint_mVax_pVax_older

# Calculate total intervention costs across efficacy from 0 to 100%
totalcost_er_pVax <-  sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_er_p
totalcost_er_llAb_pVax <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_er_lp
totalcost_er_mVax_pVax <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_admin * coverage[3] * num_infants * costs[3]) + medcost_er_mp

totalcost_er_pVax_older <- sum(pVax_older_admin * cov_pVax_o * num_infants * (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_er_po
totalcost_er_llAb_pVax_older <- sum(llAb_admin * coverage[1] * num_infants * costs[1]) + sum(pVax_older_admin * cov_pVax_o * num_infants * (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_er_lpo
totalcost_er_mVax_pVax_older <- sum(mVax_admin * coverage[2] * num_infants * costs[2]) + sum(pVax_older_admin * cov_pVax_o * num_infants * (0.5*cost_EPI + 0.5*1.35 + cost_prod)) + medcost_er_mpo

# Calculate the probability of each strategy being optimal at WTP = 1XGDP
# as the efficacy in pVax as combo strategy at 10 & 14 wks (driven by interference) increases
NHB_no_er <-rep.col(NHB_func(prep_no, CET_Mali_GDP), length(eff_red))
NHB_l_er <- rep.col(NHB_func(prep_llAb, CET_Mali_GDP), length(eff_red))
NHB_m_er <- rep.col(NHB_func(prep_mVax, CET_Mali_GDP), length(eff_red))
NHB_p_er <- rep.col(NHB_func(prep_pVax, CET_Mali_GDP), length(eff_red))
NHB_p_older_er <- rep.col(NHB_func(prep_pVax_older, CET_Mali_GDP), length(eff_red))
NHB_lp_older_er <- rep.col(NHB_func(prep_joint_llAb_pVax_older, CET_Mali_GDP), length(eff_red))
NHB_mp_older_er <- rep.col(NHB_func(prep_joint_mVax_pVax_older, CET_Mali_GDP), length(eff_red))

DALYS_lost_er_no <- rep.col(DALYS_lost_no_u, length(eff_red))
totalcost_er_no <- rep.col(totalcost_no_u, length(eff_red))

# inputs for reduction in efficacy of pVax driven by interference
prep_lp <- array(c(DALYS_lost_er_no, DALYS_lost_er_lp, totalcost_er_llAb_pVax, totalcost_er_no), dim = c(trials, length(eff_red), 4))
prep_mp <- array(c(DALYS_lost_er_no, DALYS_lost_er_mp, totalcost_er_mVax_pVax, totalcost_er_no), dim = c(trials, length(eff_red), 4))
# NHB for reduction in efficacy of pVax driven by interference
NHB_lp_er <- NHB_func_er(prep_lp, CET_Mali_GDP)
NHB_mp_er <- NHB_func_er(prep_mp, CET_Mali_GDP)

# reduction in efficacy of pVax driven by interference (pVax efficacy only changes when in combo)
compare_NHB_er <- array(c(NHB_no_er, NHB_l_er, NHB_m_er, NHB_p_er, NHB_lp_er, NHB_mp_er, NHB_p_older_er, NHB_lp_older_er, NHB_mp_older_er), dim = c(trials, length(eff_red), 9))
win_NHB_er <- apply(compare_NHB_er, MARGIN = c(1,2), which.max)

pO_no_er <- rep(0, length(eff_red))
for(no in 1: length(eff_red)){
  pO_no_er[no] <- sum(win_NHB_er[,no] == 1)/trials
}

pO_llAb_er <- rep(0, length(eff_red))
for(Ol in 1: length(eff_red)){
  pO_llAb_er[Ol] <- sum(win_NHB_er[,Ol] == 2)/trials
}

pO_mVax_er <- rep(0, length(eff_red))
for(Om in 1: length(eff_red)){
  pO_mVax_er[Om] <- sum(win_NHB_er[,Om] == 3)/trials
}

pO_pVax_er <- rep(0, length(eff_red))
for(Op in 1: length(eff_red)){
  pO_pVax_er[Op] <- sum(win_NHB_er[,Op] == 4)/trials
}

pO_llAb_pVax_er <- rep(0, length(eff_red))
for(lp in 1: length(eff_red)){
  pO_llAb_pVax_er[lp] <- sum(win_NHB_er[,lp] == 5)/trials
}

pO_mVax_pVax_er <- rep(0, length(eff_red))
for(Omp in 1: length(eff_red)){
  pO_mVax_pVax_er[Omp] <- sum(win_NHB_er[,Omp] == 6)/trials
}

pO_pVax_older_er <- rep(0, length(eff_red))
for(Opo in 1: length(eff_red)){
  pO_pVax_older_er[Opo] <- sum(win_NHB_er[,Opo] == 7)/trials
}

pO_llAb_pVax_older_er <- rep(0, length(eff_red))
for(zp in 1: length(eff_red)){
  pO_llAb_pVax_older_er[zp] <- sum(win_NHB_er[,zp] == 8)/trials
}

pO_mVax_pVax_older_er <- rep(0, length(eff_red))
for(Ompo in 1: length(eff_red)){
  pO_mVax_pVax_older_er[Ompo] <- sum(win_NHB_er[,Ompo] == 9)/trials
}

##
# When efficacy reduction is driven by infant immaturity (NOTE: I'm calling this "ser")
prep_p_ser <- array(c(DALYS_lost_er_no, DALYS_lost_er_p, totalcost_er_pVax, totalcost_er_no), dim = c(trials, length(eff_red), 4)) #for immaturity
NHB_p_ser <- NHB_func_er(prep_p_ser, CET_Mali_GDP) # for immaturity

compare_NHB_ser <- array(c(NHB_no_er, NHB_l_er, NHB_m_er, NHB_p_ser, NHB_lp_er, NHB_mp_er, NHB_p_older_er, NHB_lp_older_er, NHB_mp_older_er), dim = c(trials, length(eff_red), 9))
win_NHB_ser <- apply(compare_NHB_ser, MARGIN = c(1,2), which.max)

pO_no_ser <- rep(0, length(eff_red))
for(no in 1: length(eff_red)){
  pO_no_ser[no] <- sum(win_NHB_ser[,no] == 1)/trials
}

pO_llAb_ser <- rep(0, length(eff_red))
for(Ol in 1: length(eff_red)){
  pO_llAb_ser[Ol] <- sum(win_NHB_ser[,Ol] == 2)/trials
}

pO_mVax_ser <- rep(0, length(eff_red))
for(Om in 1: length(eff_red)){
  pO_mVax_ser[Om] <- sum(win_NHB_ser[,Om] == 3)/trials
}

pO_pVax_ser <- rep(0, length(eff_red))
for(Op in 1: length(eff_red)){
  pO_pVax_ser[Op] <- sum(win_NHB_ser[,Op] == 4)/trials
}

pO_llAb_pVax_ser <- rep(0, length(eff_red))
for(Olp in 1: length(eff_red)){
  pO_llAb_pVax_ser[Olp] <- sum(win_NHB_ser[,Olp] == 5)/trials
}

pO_mVax_pVax_ser <- rep(0, length(eff_red))
for(Omp in 1: length(eff_red)){
  pO_mVax_pVax_ser[Omp] <- sum(win_NHB_ser[,Omp] == 6)/trials
}

pO_pVax_older_ser <- rep(0, length(eff_red))
for(Opo in 1: length(eff_red)){
  pO_pVax_older_ser[Opo] <- sum(win_NHB_ser[,Opo] == 7)/trials
}

pO_llAb_pVax_older_ser <- rep(0, length(eff_red))
for(Olpo in 1: length(eff_red)){
  pO_llAb_pVax_older_ser[Olpo] <- sum(win_NHB_ser[,Olpo] == 8)/trials
}

pO_mVax_pVax_older_ser <- rep(0, length(eff_red))
for(Ompo in 1: length(eff_red)){
  pO_mVax_pVax_older_ser[Ompo] <- sum(win_NHB_ser[,Ompo] == 9)/trials
}

####
# Calculate NHB for strategies in SA #4 at WTP  = 1 X GDP
NHB_no_GDP <- NHB_func(prep_no, CET_Mali_GDP)
NHB_m_GDP <- NHB_func(prep_mVax, CET_Mali_GDP)
# and fo SA # 5 at WTP = 1 X GDP
NHB_p_GDP <- NHB_func(prep_pVax, CET_Mali_GDP)
NHB_mp_GDP <- NHB_func(prep_joint_mVax_pVax, CET_Mali_GDP)
NHB_p_older_GDP <- NHB_func(prep_pVax_older, CET_Mali_GDP)
NHB_mp_older_GDP <- NHB_func(prep_joint_mVax_pVax_older, CET_Mali_GDP)
####

####
additional_cost <- c(totalcost_llAb - totalcost_no, totalcost_mVax - totalcost_no,
                     totalcost_pVax- totalcost_no, totalcost_joint_llAb_pVax - totalcost_no,
                     totalcost_joint_mVax_pVax - totalcost_no, totalcost_pVax_older - totalcost_no,
                     totalcost_joint_llAb_pVax_older - totalcost_no,
                     totalcost_joint_mVax_pVax_older - totalcost_no)
deaths_averted <- c(deaths_no - deaths_llAb, deaths_no - deaths_mVax,
                    deaths_no - deaths_pVax, deaths_no - deaths_joint_llAb_pVax,
                    deaths_no - deaths_joint_mVax_pVax, deaths_no - deaths_pVax_older,
                    deaths_no - deaths_joint_llAb_pVax_older, deaths_no - deaths_joint_mVax_pVax_older)
DALYs_averted <- c(DALYS_lost_no - DALYS_lost_llAb, DALYS_lost_no - DALYS_lost_mVax,
                   DALYS_lost_no - DALYS_lost_pVax, DALYS_lost_no - DALYS_lost_joint_llAb_pVax,
                   DALYS_lost_no - DALYS_lost_joint_mVax_pVax, DALYS_lost_no - DALYS_lost_pVax_older,
                   DALYS_lost_no - DALYS_lost_joint_llAb_pVax_older, DALYS_lost_no - DALYS_lost_joint_mVax_pVax_older)

# All ICERs compared to status quo:
ICERs_base <- additional_cost/DALYs_averted
# # Calculate ICER moving from pVax to mVax + pVax:
# ICER_mVax_pVax <- (totalcost_joint_mVax_pVax - totalcost_pVax) / (DALYS_lost_pVax - DALYS_lost_joint_mVax_pVax)
# # Calculate ICER moving from pVax to llAb + pVax:
# ICER_llAb_pVax <- (totalcost_joint_llAb_pVax - totalcost_pVax) / (DALYS_lost_pVax - DALYS_lost_joint_llAb_pVax)
# interventions <- data.frame(int_names, efficacy, duration, coverage, additional_cost, deaths_averted, DALYs_averted, ICERs_base)

# output 
strategies <- c( "llAb", "mVax", "pVax 10 & 14 wk", "llAb + pVax 10 and 14 wk", "mVax + pVax 10 & 14 wk", "pVax 8 & 9 months", "llAb + pVax 8 & 9 months", "mVax + pVax 8 & 9 months")

ICERS_output <- tibble(strategies, ICERs_base)

# All ICERs compared to status quo:
ICER_llAb_u <- (totalcost_llAb_u - totalcost_no_u) / (DALYS_lost_no_u - DALYS_lost_llAb_u)
ICER_mVax_u <- (totalcost_mVax_u - totalcost_no_u) / (DALYS_lost_no_u - DALYS_lost_mVax_u)
ICER_pVax_u <- (totalcost_pVax_u - totalcost_no_u) / (DALYS_lost_no_u - DALYS_lost_pVax_u)
ICER_joint_llAb_pVax_u <- (totalcost_joint_llAb_pVax_u - totalcost_no_u) / (DALYS_lost_no_u - DALYS_lost_joint_llAb_pVax_u)
ICER_joint_mVax_pVax_u <- (totalcost_joint_mVax_pVax_u - totalcost_no_u) / (DALYS_lost_no_u - DALYS_lost_joint_mVax_pVax_u)
ICER_pVax_older_u <- (totalcost_pVax_older_u - totalcost_no_u) / (DALYS_lost_no_u - DALYS_lost_pVax_older_u)
ICER_joint_llAb_pVax_older_u <- (totalcost_joint_llAb_pVax_older_u - totalcost_no_u) / (DALYS_lost_no_u - DALYS_lost_joint_llAb_pVax_older_u)
ICER_joint_mVax_pVax_older_u <- (totalcost_joint_mVax_pVax_older_u - totalcost_no_u) / (DALYS_lost_no_u - DALYS_lost_joint_mVax_pVax_older_u)

par(mfrow =c(2,4))
hist(ICER_llAb_u)
abline(v=ICERs_base[1], col = "red")
hist(ICER_mVax_u)
abline(v=ICERs_base[2], col = "red")
hist(ICER_pVax_u)
abline(v=ICERs_base[3], col = "red")
hist(ICER_joint_llAb_pVax_u)
abline(v=ICERs_base[4], col = "red")
hist(ICER_joint_mVax_pVax_u)
abline(v=ICERs_base[5], col = "red")
hist(ICER_pVax_older_u)
abline(v=ICERs_base[6], col = "red")
hist(ICER_joint_llAb_pVax_older_u)
abline(v=ICERs_base[7], col = "red")
hist(ICER_joint_mVax_pVax_older_u)
abline(v=ICERs_base[8], col = "red")

par(mfrow = c(1,1))
plot(cut(ICER_llAb_u, breaks = c(-170000, seq(0, 10000, by = 1000))))

par(mfrow =c(3,3))
hist(DALYS_lost_no_u)
abline(v = DALYS_lost_no, col = "red")
hist(DALYS_lost_llAb_u)
abline(v=DALYS_lost_llAb, col = "red")
hist(DALYS_lost_mVax_u)
abline(v=DALYS_lost_mVax, col = "red")
hist(DALYS_lost_pVax_u)
abline(v=DALYS_lost_pVax, col = "red")
hist(DALYS_lost_joint_llAb_pVax_u)
abline(v=DALYS_lost_joint_llAb_pVax, col = "red")
hist(DALYS_lost_joint_mVax_pVax_u)
abline(v=DALYS_lost_joint_mVax_pVax, col = "red")
hist(DALYS_lost_pVax_older_u)
abline(v=DALYS_lost_pVax_older, col = "red")
hist(DALYS_lost_joint_llAb_pVax_older_u)
abline(v=DALYS_lost_joint_llAb_pVax_older, col = "red")
hist(DALYS_lost_joint_mVax_pVax_older_u)
abline(v=DALYS_lost_joint_mVax_pVax_older, col = "red")


par(mfrow =c(3,3))
hist(totalcost_no_u)
abline(v = totalcost_no, col = "red")
hist(totalcost_llAb_u)
abline(v=totalcost_llAb, col = "red")
hist(totalcost_mVax_u)
abline(v=totalcost_mVax, col = "red")
hist(totalcost_pVax_u)
abline(v=totalcost_pVax, col = "red")
hist(totalcost_joint_llAb_pVax_u)
abline(v=totalcost_joint_llAb_pVax, col = "red")
hist(totalcost_joint_mVax_pVax_u)
abline(v=totalcost_joint_mVax_pVax, col = "red")
hist(totalcost_pVax_older_u)
abline(v=totalcost_pVax_older, col = "red")
hist(totalcost_joint_llAb_pVax_older_u)
abline(v=totalcost_joint_llAb_pVax_older, col = "red")
hist(totalcost_joint_mVax_pVax_older_u)
abline(v=totalcost_joint_mVax_pVax_older, col = "red")


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
# donor pays cost of product - 20 cents per dose paid by the government
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

source("donorcost_optimality_curve_code.R")
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

source("govcost_optimality_curve_code.R")

output <- data.frame(strategies, gvcosts, dnrcosts, ics_vec)

####
# Find lower and upper 10% of all inputs
dec_func <- function(vec){
  quantile(vec, c(0.10, 0.90))
}

dec_p_pneum <- dec_func(p_pneum_u)
dec_pneum_set_l <- which(p_pneum_u <= dec_p_pneum[1]) # ask which trials are in the bottom 10%
dec_pneum_set_u <- which(p_pneum_u >= dec_p_pneum[2]) # ask which trials are in the top 10%

dec_cost_outpat <- dec_func(cost_outpatient_u)
dec_c_outpat_set_l <- which(cost_outpatient_u <= dec_cost_outpat[1])
dec_c_outpat_set_u <- which(cost_outpatient_u >= dec_cost_outpat[2])

dec_cost_hosp <- dec_func(cost_hosp_u)
dec_c_hosp_l <- which(cost_hosp_u <= dec_cost_hosp[1])
dec_c_hosp_u <- which(cost_hosp_u >= dec_cost_hosp[2])
