
# incidence of hospitalization in Gambia, per child
i_inpatient_gambia <- c(rep(32.2, 6), rep(17.4, 6), rep(6.3, 12), rep(1.6, 12))/1000

# incidence of LRTI in Mali, per child
i_LRTI_mali <- sum(LRTI_no_age[1:6])/ sum(num_infants)

# probability of hosps with Gambia rates and Mali RSV-LRTI
p_hosp_temp <- i_inpatient_gambia[1] / i_LRTI_mali 