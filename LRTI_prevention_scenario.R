################################################################################
# all prevention products prevent LRTI, not RSV infection

# for all interventions, the number of cases will be the same across age
# here is a matrix of cases for each monthly birth cohort
# followed to three years of age
cases_no_bic <- bic_cases(pd_calc(0, 0, AR_y_bc, empty_year_cohort), num_infants, mort_mat)

# calculate number of RSV-LRTI episodes
LRTI_no_bic <- LRTI_func(0, 0, 0, p_pneum, cases_no_bic)
LRTI_llAb_bic <- LRTI_func(efficacy[1], coverage[1], mat_eff_llAb, p_pneum, cases_no_bic)
LRTI_mVax_bic <- LRTI_func(efficacy[2], coverage[2], mat_eff_mVax, p_pneum, cases_no_bic)
LRTI_pVax_bic <- LRTI_func(efficacy[3], coverage[3], mat_eff_pVax, p_pneum, cases_no_bic)
LRTI_joint_llAb_pVax_bic <- LRTI_func_joint(efficacy[1], efficacy[3], coverage[1], coverage[3], mat_eff_llAb, mat_eff_pVax, p_pneum, cases_no_bic)
LRTI_joint_mVax_pVax_bic <- LRTI_func_joint(efficacy[2], efficacy[3], coverage[2], coverage[3], mat_eff_mVax, mat_eff_pVax, p_pneum, cases_no_bic)
LRTI_pVax_older_bic <- LRTI_func(efficacy[3], cov_pVax_o, mat_eff_older_pVax, p_pneum, cases_no_bic)
LRTI_joint_llAb_pVax_older_bic <- LRTI_func_joint(efficacy[1], efficacy[3], coverage[1], cov_pVax_o, mat_eff_llAb, mat_eff_older_pVax, p_pneum, cases_no_bic)
LRTI_joint_mVax_pVax_older_bic <- LRTI_func_joint(efficacy[2], efficacy[3], coverage[2], cov_pVax_o, mat_eff_mVax, mat_eff_older_pVax, p_pneum, cases_no_bic)

# return a vector of RSV-LRTI episodes by age
LRTI_no_age <- adj_func(LRTI_no_bic)
LRTI_llAb_age <- adj_func(LRTI_llAb_bic)
LRTI_mVax_age <- adj_func(LRTI_mVax_bic)
LRTI_pVax_age <- adj_func(LRTI_pVax_bic)
LRTI_joint_llAb_pVax_age <- adj_func(LRTI_joint_llAb_pVax_bic)
LRTI_joint_mVax_pVax_age <- adj_func(LRTI_joint_mVax_pVax_bic)
LRTI_pVax_older_age <- adj_func(LRTI_pVax_older_bic)
LRTI_joint_llAb_pVax_older_age <- adj_func(LRTI_joint_llAb_pVax_older_bic)
LRTI_joint_mVax_pVax_older_age <- adj_func(LRTI_joint_mVax_pVax_older_bic)

# matrix of when babies are allive through 3 years
babies_mat <- empty_year_cohort
for (m in 1:12) {
  babies_mat[m,] <- c(rep(0, times = m-1), rep(1, times = 36), rep(0, times = 12-m))
}
