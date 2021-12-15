source('living_babies.R')

# incidence of hospitalization in Gambia, per child
i_inpatient_gambia <- c(rep(32.2, 6), rep(17.4, 6), rep(6.3, 12), rep(1.6, 12))/1000/2

# incidence of LRTI in Mali, per child
i_LRTI_mali <- rep(NA, 36)
i_LRTI_mali[1:6] <- sum(LRTI_no_age[1:6])/ sum(simple_babies[1:6])
i_LRTI_mali[7:12] <- sum(LRTI_no_age[7:12])/ sum(simple_babies[7:12])
i_LRTI_mali[13:24] <- sum(LRTI_no_age[13:24]) / sum(simple_babies[13:24])
i_LRTI_mali[25:36] <- sum(LRTI_no_age[25:36]) / sum(simple_babies[25:36])

# prob hospitalization given RSV-LRTI for 0-6 months, Gambia (w/ Mali RSV-LRTI)
p_hosp_temp <- i_inpatient_gambia / i_LRTI_mali

# ratio of prob hosp given RSV-LRTI in Mali compared to Gambia
r_mali_gambia <- p_inpatient/p_hosp_temp[1]

# multiply this ratio to Gambia rate of hosps given LRTI across ages
new_hosps <- r_mali_gambia * p_hosp_temp

p_hosp_new <- new_hosps

# Uncertainty
phosp_func <- function(p_inpat){
  r_m_g <- p_inpat/p_hosp_temp[1]
  new_p_hosps <- r_m_g * p_hosp_temp
}

p_hosp_u <- matrix(NA, nrow = trials, ncol = 36)
for (i in 1:trials){
p_hosp_u[i, ] <- phosp_func(p_inpatient_u[i])
}

################################################################################