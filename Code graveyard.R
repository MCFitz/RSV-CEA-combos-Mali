## code graveyard

# p_inpatient_shi_lmic <- c(9.3, 22.9, 22.9, 20.5, 20.5, 20.5, 14, 14, 14,
#                           9.4, 9.4, 9.4, rep(5.6, 12), rep(1.1, 12))/1000
# p_inpatient_shi_umic <- c(46.4, 74.5, 74.5, 25.7, 25.7, 25.7, 16.9, 16.9, 16.9,
#                           18.8, 18.8, 18.8, rep(7.1, 12), rep(1.4, 12))/1000
# p_inpatient_shi_hi <- c(31.9, 67.4, 67.4, 36.7, 36.7, 36.7, 40.8, 40.8, 40.8,
#                         25.9, 25.9, 25.9, rep(4.2, 12), rep(0.9, 12))/1000
# p_inpatient_shi_dev <- c(15.9, 26.1, 26.1, 20.7, 20.7, 20.7, 12.0, 12.0, 12.0,
#                          11.3, 11.3, 11.3, rep(5.0, 12), rep(1.0, 12))/1000
# p_inpatient_shi_ind <- c(31.0, 54.6, 54.6, 27.5, 27.5, seq(27.5, 3.9, length.out = 8),
#                          rep(3.9, 11), rep(0.8, 12))/1000

# p_inpatient_shi_umic_LC <- c(3.9, 28.2, 28.2, 11.6, 11.6, 11.6, 9.1, 9.1, 9.1,
#                           9.6, 9.6, 9.6, rep(4.6, 12), rep(0.8, 12))/100
# p_inpatient_shi_umic_UC <- c(549.5, 197, 197, 56.7, 56.7, 56.7, 31.5, 31.5, 31.5,
#                              36.9, 36.9, 36.9, rep(10.7, 12), rep(2.7, 12))/100
# 
# p_inpatient_shi_hi_LC <- c(26.9, 45.3, 45.3, 19.1, 19.1, 19.1, 36.9, 36.9, 36.9,
#                              22.9, 22.9, 22.9, rep(3.4, 12), rep(0.6, 12))/100
# p_inpatient_shi_hi_UC <- c(37.8, 100.5, 100.5, 70.6, 70.6, 70.6, 45.1, 45.1, 45.1,
#                              29.4, 29.4, 29.4, rep(5.4, 12), rep(1.2, 12))/100

#############
# # calculate cumulative hospitalizations that result from flat line hospitalization rate
# inpat_no_u_flat <- inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_no_u))
# inpat_no_flat <- inpat_func(p_inpatient, pneum_func(p_pneum, cases_no))
# 
# # calculate cumulative hospitalizations that result from age-stratified hospitalization rates
# inpat_no <- apply(t(inpat_func(p_inpatient_shi_umic[1:6], pneum_func(p_pneum, t(rep.col(cases_no, length(AR_age_weights[1:6])))* AR_age_weights[1:6]))), MARGIN = 1, sum)

#######################

# # Calculate number of inpatient cases for low-income countries from Shi (by narrow age-band )
# inpat_no_u <- apply(t(inpat_func(p_inpatient_shi_li, pneum_func(p_pneum_u, t(rep.col(cases_no_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_llAb_u <- apply(t(inpat_func(p_inpatient_shi_li, pneum_func(p_pneum_u, t(rep.col(cases_llAb_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_mVax_u <- apply(t(inpat_func(p_inpatient_shi_li, pneum_func(p_pneum_u, t(rep.col(cases_mVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_pVax_u <- apply(t(inpat_func(p_inpatient_shi_li, pneum_func(p_pneum_u, t(rep.col(cases_pVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_llAb_pVax_u <- apply(t(inpat_func(p_inpatient_shi_li, pneum_func(p_pneum_u, t(rep.col(cases_joint_llAb_pVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_mVax_pVax_u <- apply(t(inpat_func(p_inpatient_shi_li, pneum_func(p_pneum_u, t(rep.col(cases_joint_mVax_pVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_pVax_older_u <- apply(t(inpat_func(p_inpatient_shi_li, pneum_func(p_pneum_u, t(rep.col(cases_pVax_older_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_llAb_pVax_older_u <- apply(t(inpat_func(p_inpatient_shi_li, pneum_func(p_pneum_u, t(rep.col(cases_joint_llAb_pVax_older_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_mVax_pVax_older_u <- apply(t(inpat_func(p_inpatient_shi_li, pneum_func(p_pneum_u, t(rep.col(cases_joint_mVax_pVax_older_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)

# for LMICs from Shi (by narrow age-band)
# inpat_no_u <- apply(t(inpat_func(p_inpatient_shi_lmic, pneum_func(p_pneum_u, t(rep.col(cases_no_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_llAb_u <- apply(t(inpat_func(p_inpatient_shi_lmic, pneum_func(p_pneum_u, t(rep.col(cases_llAb_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_mVax_u <- apply(t(inpat_func(p_inpatient_shi_lmic, pneum_func(p_pneum_u, t(rep.col(cases_mVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_pVax_u <- apply(t(inpat_func(p_inpatient_shi_lmic, pneum_func(p_pneum_u, t(rep.col(cases_pVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_llAb_pVax_u <- apply(t(inpat_func(p_inpatient_shi_lmic, pneum_func(p_pneum_u, t(rep.col(cases_joint_llAb_pVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_mVax_pVax_u <- apply(t(inpat_func(p_inpatient_shi_lmic, pneum_func(p_pneum_u, t(rep.col(cases_joint_mVax_pVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_pVax_older_u <- apply(t(inpat_func(p_inpatient_shi_lmic, pneum_func(p_pneum_u, t(rep.col(cases_pVax_older_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_llAb_pVax_older_u <- apply(t(inpat_func(p_inpatient_shi_lmic, pneum_func(p_pneum_u, t(rep.col(cases_joint_llAb_pVax_older_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_mVax_pVax_older_u <- apply(t(inpat_func(p_inpatient_shi_lmic, pneum_func(p_pneum_u, t(rep.col(cases_joint_mVax_pVax_older_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)

# For UMICs from Shi (by narrow age-band)
# inpat_no_u <- apply(t(inpat_func(p_inpatient_shi_umic, pneum_func(p_pneum_u, t(rep.col(cases_no_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_llAb_u <- apply(t(inpat_func(p_inpatient_shi_umic, pneum_func(p_pneum_u, t(rep.col(cases_llAb_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_mVax_u <- apply(t(inpat_func(p_inpatient_shi_umic, pneum_func(p_pneum_u, t(rep.col(cases_mVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_pVax_u <- apply(t(inpat_func(p_inpatient_shi_umic, pneum_func(p_pneum_u, t(rep.col(cases_pVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_llAb_pVax_u <- apply(t(inpat_func(p_inpatient_shi_umic, pneum_func(p_pneum_u, t(rep.col(cases_joint_llAb_pVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_mVax_pVax_u <- apply(t(inpat_func(p_inpatient_shi_umic, pneum_func(p_pneum_u, t(rep.col(cases_joint_mVax_pVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_pVax_older_u <- apply(t(inpat_func(p_inpatient_shi_umic, pneum_func(p_pneum_u, t(rep.col(cases_pVax_older_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_llAb_pVax_older_u <- apply(t(inpat_func(p_inpatient_shi_umic, pneum_func(p_pneum_u, t(rep.col(cases_joint_llAb_pVax_older_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_mVax_pVax_older_u <- apply(t(inpat_func(p_inpatient_shi_umic, pneum_func(p_pneum_u, t(rep.col(cases_joint_mVax_pVax_older_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)

# For high-income countries from Shi (by narrow age-band)
# inpat_no_u <- apply(t(inpat_func(p_inpatient_shi_hi, pneum_func(p_pneum_u, t(rep.col(cases_no_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_llAb_u <- apply(t(inpat_func(p_inpatient_shi_hi, pneum_func(p_pneum_u, t(rep.col(cases_llAb_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_mVax_u <- apply(t(inpat_func(p_inpatient_shi_hi, pneum_func(p_pneum_u, t(rep.col(cases_mVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_pVax_u <- apply(t(inpat_func(p_inpatient_shi_hi, pneum_func(p_pneum_u, t(rep.col(cases_pVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_llAb_pVax_u <- apply(t(inpat_func(p_inpatient_shi_hi, pneum_func(p_pneum_u, t(rep.col(cases_joint_llAb_pVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_mVax_pVax_u <- apply(t(inpat_func(p_inpatient_shi_hi, pneum_func(p_pneum_u, t(rep.col(cases_joint_mVax_pVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_pVax_older_u <- apply(t(inpat_func(p_inpatient_shi_hi, pneum_func(p_pneum_u, t(rep.col(cases_pVax_older_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_llAb_pVax_older_u <- apply(t(inpat_func(p_inpatient_shi_hi, pneum_func(p_pneum_u, t(rep.col(cases_joint_llAb_pVax_older_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_mVax_pVax_older_u <- apply(t(inpat_func(p_inpatient_shi_hi, pneum_func(p_pneum_u, t(rep.col(cases_joint_mVax_pVax_older_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)

# For developing countries from Shi (by narrow-age band)
# inpat_no_u <- apply(t(inpat_func(p_inpatient_shi_dev, pneum_func(p_pneum_u, t(rep.col(cases_no_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_llAb_u <- apply(t(inpat_func(p_inpatient_shi_dev, pneum_func(p_pneum_u, t(rep.col(cases_llAb_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_mVax_u <- apply(t(inpat_func(p_inpatient_shi_dev, pneum_func(p_pneum_u, t(rep.col(cases_mVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_pVax_u <- apply(t(inpat_func(p_inpatient_shi_dev, pneum_func(p_pneum_u, t(rep.col(cases_pVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_llAb_pVax_u <- apply(t(inpat_func(p_inpatient_shi_dev, pneum_func(p_pneum_u, t(rep.col(cases_joint_llAb_pVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_mVax_pVax_u <- apply(t(inpat_func(p_inpatient_shi_dev, pneum_func(p_pneum_u, t(rep.col(cases_joint_mVax_pVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_pVax_older_u <- apply(t(inpat_func(p_inpatient_shi_dev, pneum_func(p_pneum_u, t(rep.col(cases_pVax_older_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_llAb_pVax_older_u <- apply(t(inpat_func(p_inpatient_shi_dev, pneum_func(p_pneum_u, t(rep.col(cases_joint_llAb_pVax_older_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_mVax_pVax_older_u <- apply(t(inpat_func(p_inpatient_shi_dev, pneum_func(p_pneum_u, t(rep.col(cases_joint_mVax_pVax_older_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)

# For industrialized countries from Shi (by narrow age-band)
# inpat_no_u <- apply(t(inpat_func(p_inpatient_shi_ind, pneum_func(p_pneum_u, t(rep.col(cases_no_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_llAb_u <- apply(t(inpat_func(p_inpatient_shi_ind, pneum_func(p_pneum_u, t(rep.col(cases_llAb_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_mVax_u <- apply(t(inpat_func(p_inpatient_shi_ind, pneum_func(p_pneum_u, t(rep.col(cases_mVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_pVax_u <- apply(t(inpat_func(p_inpatient_shi_ind, pneum_func(p_pneum_u, t(rep.col(cases_pVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_llAb_pVax_u <- apply(t(inpat_func(p_inpatient_shi_ind, pneum_func(p_pneum_u, t(rep.col(cases_joint_llAb_pVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_mVax_pVax_u <- apply(t(inpat_func(p_inpatient_shi_ind, pneum_func(p_pneum_u, t(rep.col(cases_joint_mVax_pVax_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_pVax_older_u <- apply(t(inpat_func(p_inpatient_shi_ind, pneum_func(p_pneum_u, t(rep.col(cases_pVax_older_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_llAb_pVax_older_u <- apply(t(inpat_func(p_inpatient_shi_ind, pneum_func(p_pneum_u, t(rep.col(cases_joint_llAb_pVax_older_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)
# inpat_joint_mVax_pVax_older_u <- apply(t(inpat_func(p_inpatient_shi_ind, pneum_func(p_pneum_u, t(rep.col(cases_joint_mVax_pVax_older_u, length(AR_age_weights)))* AR_age_weights))), MARGIN = 1, sum)

# Calculate number of deaths under status quo and each intervention 

# # Using inpatient rates from Buchwald, not age-stratified
# deaths_no <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_no)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_no)))
# deaths_llAb <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_llAb)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_llAb)))
# deaths_mVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_mVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_mVax)))
# deaths_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_pVax)))
# deaths_joint_llAb_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax)))
# deaths_joint_mVax_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax)))
# deaths_pVax_older <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_older)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_pVax_older)))
# deaths_joint_llAb_pVax_older <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax_older)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_joint_llAb_pVax_older)))
# deaths_joint_mVax_pVax_older <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax_older)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_joint_mVax_pVax_older)))

# Calculate number of deaths with uncertainty
# Switch code block ON if using flat CFR and flat hosp rate (based on low-income country spline from Li et al. 2020)
# deaths_no_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_no_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_no_u)))
# deaths_llAb_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_llAb_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_llAb_u)))
# deaths_mVax_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_mVax_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_mVax_u)))
# deaths_pVax_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_u)))
# deaths_pVax_u_SA <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_u_SA)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_u_SA)))
# deaths_joint_llAb_pVax_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_u)))
# deaths_joint_mVax_pVax_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_u)))
# 
# deaths_pVax_older_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_older_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_older_u)))
# deaths_joint_llAb_pVax_older_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_older_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_older_u)))
# deaths_joint_mVax_pVax_older_u <- mort_inpat_func(CFR_inpatient_u, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_older_u)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_older_u)))

##### Number of deaths with uncertainty when hospitalizations vary by narrow age band
# deaths_no_u <- mort_inpat_func(CFR_inpatient_u, inpat_no_u, CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_no_u)))
# deaths_llAb_u <- mort_inpat_func(CFR_inpatient_u, inpat_llAb_u, CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_llAb_u)))
# deaths_mVax_u <- mort_inpat_func(CFR_inpatient_u, inpat_mVax_u, CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_mVax_u)))
# deaths_pVax_u <- mort_inpat_func(CFR_inpatient_u, inpat_pVax_u, CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_u)))
# deaths_joint_llAb_pVax_u <- mort_inpat_func(CFR_inpatient_u, inpat_joint_llAb_pVax_u, CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_u)))
# deaths_joint_mVax_pVax_u <- mort_inpat_func(CFR_inpatient_u, inpat_joint_mVax_pVax_u, CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_u)))
# 
# deaths_pVax_older_u <- mort_inpat_func(CFR_inpatient_u, inpat_pVax_u, CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_pVax_older_u)))
# deaths_joint_llAb_pVax_older_u <- mort_inpat_func(CFR_inpatient_u, inpat_joint_llAb_pVax_older_u, CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_llAb_pVax_older_u)))
# deaths_joint_mVax_pVax_older_u <- mort_inpat_func(CFR_inpatient_u, inpat_joint_mVax_pVax_older_u, CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, cases_joint_mVax_pVax_older_u)))


# # # Calculate number of deaths with uncertainty and age-weighted CFRs
# # # Switch code block ON to age-weight the CFRs (based on Lower-middle income country spline from Li et al. 2020)
# deaths_no_u_age <- t(mort_inpat_func(CFR_sub, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_no_u, length(AR_age_weights)))* AR_age_weights)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_no_u, length(AR_age_weights))) * AR_age_weights))))
# deaths_llAb_u_age <- t(mort_inpat_func(CFR_sub, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_llAb_u, length(AR_age_weights))) * AR_age_weights)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_llAb_u, length(AR_age_weights))) * AR_age_weights))))
# deaths_mVax_u_age <- t(mort_inpat_func(CFR_sub, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_mVax_u, length(AR_age_weights))) * AR_age_weights)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_mVax_u, length(AR_age_weights))) * AR_age_weights))))
# deaths_pVax_u_age <- t(mort_inpat_func(CFR_sub, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_pVax_u, length(AR_age_weights))) * AR_age_weights)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_pVax_u, length(AR_age_weights))) * AR_age_weights))))
# deaths_pVax_u_SA_age <- t(mort_inpat_func(CFR_sub, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_pVax_u_SA, length(AR_age_weights))) * AR_age_weights)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_pVax_u_SA, length(AR_age_weights))) * AR_age_weights))))
# deaths_joint_llAb_pVax_u_age <-t(mort_inpat_func(CFR_sub, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_joint_llAb_pVax_u, length(AR_age_weights))) * AR_age_weights)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_joint_llAb_pVax_u, length(AR_age_weights))) * AR_age_weights))))
# deaths_joint_mVax_pVax_u_age <- t(mort_inpat_func(CFR_sub, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_joint_mVax_pVax_u, length(AR_age_weights))) * AR_age_weights)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_joint_mVax_pVax_u, length(AR_age_weights))) * AR_age_weights))))
# 
# deaths_pVax_older_u_age <- t(mort_inpat_func(CFR_sub, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_pVax_older_u, length(AR_age_weights))) * AR_age_weights)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_pVax_older_u, length(AR_age_weights))) * AR_age_weights))))
# deaths_joint_llAb_pVax_older_u_age <- t(mort_inpat_func(CFR_sub, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_joint_llAb_pVax_older_u, length(AR_age_weights))) * AR_age_weights)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_joint_llAb_pVax_older_u, length(AR_age_weights))) * AR_age_weights))))
# deaths_joint_mVax_pVax_older_u_age <-t(mort_inpat_func(CFR_sub, inpat_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_joint_mVax_pVax_older_u, length(AR_age_weights))) * AR_age_weights)), CFR_nr_care_u, nr_care_func(p_inpatient_u, pneum_func(p_pneum_u, t(rep.col(cases_joint_mVax_pVax_older_u, length(AR_age_weights))) * AR_age_weights))))

