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
