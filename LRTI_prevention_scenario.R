
# all prevention products prevent LRTI, not RSV infection
# for all interventions, the number of cases will be the same across age
cases_no_age <- RSVcases(pd_calc(0, 0, AR_y_bc, empty_year_cohort), num_infants, mort_mat)

# calculate number of RSV-LRTI episodes

LRTI_no_age <- LRTI_func(0, 0, 1, p_pneum, cases_no_age)

LRTI_llAb_age <- LRTI_func(efficacy[1], coverage[1], mat_eff_llAb, p_pneum, cases_llAb_age[[1]])

# LRTI_mVax_age <- LRTI_func(p_pneum, cases_mVax_age)
# LRTI_pVax_age <- LRTI_func(p_pneum, cases_pVax_age)
# LRTI_joint_llAb_pVax_age <- pneum_func(p_pneum, cases_joint_llAb_pVax_age)
# LRTI_joint_mVax_pVax_age <- pneum_func(p_pneum, cases_joint_mVax_pVax_age)
# LRTI_pVax_older_age <- pneum_func(p_pneum, cases_pVax_older_age)
# LRTI_joint_llAb_pVax_older_age <- pneum_func(p_pneum, cases_joint_llAb_pVax_older_age)
# LRTI_joint_mVax_pVax_older_age <- pneum_func(p_pneum, cases_joint_mVax_pVax_older_age)


