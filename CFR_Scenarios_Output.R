################################################################################
########################## CFR Scenarios Output ################################

# Start with same number of RSV-LRTI hospitalizations for status quo and llAb
# then apply diff CFR scenarios

# number of deaths for status quo under different CFR scenarios
deaths_no_cs <- matrix(0, nrow = length(months), ncol = no_scenarios)
for(c in 1:no_scenarios){
  deaths_no_cs[,c] <- mort_inpat_func(CFR_scenarios[,c], inpat_no_age, CFR_nr_care, nr_care_func(inpat_no_age)) 
}
colnames(deaths_no_cs) <- CFR_scenario_names
deaths_no_CFR_scenarios <- colSums(deaths_no_cs)

# number of deaths for llAb under different CFR scenarios
deaths_llAb_cs <- matrix(0, nrow = length(months), ncol = no_scenarios)
for(c in 1: no_scenarios){
  deaths_llAb_cs[,c] <- mort_inpat_func(CFR_scenarios[,c], inpat_llAb_age, CFR_nr_care, nr_care_func(inpat_llAb_age))
  }
colnames(deaths_llAb_cs) <- CFR_scenario_names
deaths_llAb_CFR_scenarios <- colSums(deaths_llAb_cs)

# DALYs for status quo under different CFR scenarios
DALYS_lost_no_cs <- YLL_func(deaths_no_cs) + YLD_func(inpat_no_age, deaths_no_cs, di_yrs, dw_LRTI_severe, LRTI_no_age, dw_LRTI_mod)

# DALYs for llAb under different CFR scenarios
DALYS_lost_llAb_cs <- YLL_func(deaths_llAb_cs) + YLD_func(inpat_llAb_age, deaths_llAb_cs, di_yrs, dw_LRTI_severe, LRTI_llAb_age, dw_LRTI_mod)

# ICERs for llAb compared to status quo under different CFR scenarios
ICER_CFR_scenarios <- rep(0, no_scenarios)
for (c in 1:no_scenarios){
ICER_CFR_scenarios[c] <- (totalcost_llAb - totalcost_no) / (sum(DALYS_lost_no_cs[,c]) - sum(DALYS_lost_llAb_cs[,c]))
}

output_CFR_scenario <- tibble(CFR_scenario_names, deaths_no_CFR_scenarios, deaths_llAb_CFR_scenarios, ICER_CFR_scenarios)

# save output as .csv file
write.csv(output_CFR_scenario ,"Output_CFR_Scenarios.csv", row.names = FALSE)

# generate table with RVS-LRTI hospitalizations by age for both status quo and llAb
RSV_LRTI_hosps_by_age <- tibble(months, inpat_no_age, inpat_llAb_age)
write.csv(RSV_LRTI_hosps_by_age, "RSV_LRTI_hosps_by_age.csv", row.names = FALSE)

####################################3
# CFR scenarios calculations with uncertainty

# number of deaths for status quo under different CFR scenarios
deaths_no_cs_u <- array(0, dim = c(trials, length(months), no_scenarios))
for(c in 1:no_scenarios){
  for(t in 1: trials){
  deaths_no_cs_u[t,,c] <- mort_inpat_func(CFR_scenarios_u[t, ,c], inpat_no_u_age[t,],
                                        CFR_nr_care_u[t,], nr_care_func(inpat_no_u_age[t,])) 
  }}

deaths_no_CFR_scenarios_u <- apply(deaths_no_cs_u, 3, rowSums)

# number of deaths for llAb under different CFR scenarios
deaths_llAb_cs_u <- array(0, dim = c(trials, length(months), no_scenarios))
for(c in 1: no_scenarios){
  for(t in 1: trials){
  deaths_llAb_cs_u[t,,c] <- mort_inpat_func(CFR_scenarios_u[t,,c], inpat_llAb_u_age[t,],
                                            CFR_nr_care_u[t,], nr_care_func(inpat_llAb_u_age[t,]))
  }}

deaths_llAb_CFR_scenarios_u <- apply(deaths_llAb_cs_u, c(1,3), sum)

# DALYs for status quo under different CFR scenarios
DALYS_lost_no_cs_u <- array(0, dim=c(trials, length(months), no_scenarios))
for(c in 1:no_scenarios){
DALYS_lost_no_cs_u[,,c] <- YLL_func(deaths_no_cs_u[,,c]) +
  YLD_func(inpat_no_u_age, deaths_no_cs_u[,,c], di_yrs_u, dw_LRTI_severe_u,
           LRTI_no_u_age, dw_LRTI_mod_u)
}

# DALYs for llAb under different CFR scenarios
DALYS_lost_llAb_cs_u <- array(0, dim=c(trials, length(months), no_scenarios))
for(c in 1:no_scenarios){
  DALYS_lost_llAb_cs_u[,,c] <- YLL_func(deaths_llAb_cs_u[,,c]) +
    YLD_func(inpat_llAb_u_age, deaths_llAb_cs_u[,,c], di_yrs_u, dw_LRTI_severe_u,
             LRTI_llAb_u_age, dw_LRTI_mod_u)
}

# ICERs for llAb compared to status quo under different CFR scenarios
ICER_CFR_scenarios_u <- matrix(0, nrow = trials, ncol = no_scenarios)
for (c in 1:no_scenarios){
  ICER_CFR_scenarios_u[,c] <- (totalcost_llAb_u - totalcost_no_u) / (rowSums(DALYS_lost_no_cs_u[,,c]) - rowSums(DALYS_lost_llAb_cs_u[,,c]))
}

# Generate confidence intervals for deaths and ICERs under different CFR scenarios
CI_deaths_no_CFR_scenarios <- apply(na.omit(deaths_no_CFR_scenarios_u), 2, CI_func)
CI_deaths_llAb_CFR_scenarios <- apply(na.omit(deaths_llAb_CFR_scenarios_u), 2, CI_func)
CI_ICER_CFR_scenarios <- apply(na.omit(ICER_CFR_scenarios_u), 2, CI_func)

output_CFR_scenarios_u <- tibble(CFR_scenario_names,
                                 CI_deaths_no_CFR_scenarios[1,],
                                 CI_deaths_no_CFR_scenarios[2,],
                                 CI_deaths_llAb_CFR_scenarios[1,],
                                 CI_deaths_llAb_CFR_scenarios[2,],
                                 CI_ICER_CFR_scenarios[1,],
                                 CI_ICER_CFR_scenarios[2,])

colnames(output_CFR_scenarios_u) <- c("scenario",
                                      "deaths, status quo, LB",
                                      "deaths, status quo, UB",
                                      "deaths, llAb, LB",
                                      "deaths, llAb, UB",
                                      "ICER, llAb, LB",
                                      "ICER, llAb, UB")

write.csv(output_CFR_scenarios_u ,"Output_CFR_Scenarios_uncertainty.csv", row.names = FALSE)
