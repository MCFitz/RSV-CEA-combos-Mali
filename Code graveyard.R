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


### old CE plot
# quartz("CE plane", 8, 8)
# par(xaxs="i", yaxs="i")
# plot(DALYS_lost_no - DALYS_lost_llAb, totalcost_llAb- totalcost_no, col = UMBblue, bty = "n", pch = 19, cex = 1.5, xlim = c(0,2500), ylim = c(0,7000000), xlab = "DALYs averted", ylab = 
#        "Incremental cost compared to status quo (USD)")
# points(DALYS_lost_no - DALYS_lost_mVax, totalcost_mVax- totalcost_no, col = UMBforest, pch = 19, cex =1.5)
# points(DALYS_lost_no - DALYS_lost_pVax, totalcost_pVax- totalcost_no, col = UMBplum, pch = 19, cex = 1.5)
# # points(DALYS_lost_no - DALYS_lost_pVax_intflo, totalcost_pVax_intflo - totalcost_no, col = UMBplum, pch = 17, cex = 1.5)
# # points(DALYS_lost_no - DALYS_lost_pVax_intfhi, totalcost_pVax_intfhi - totalcost_no, col = UMBplum, pch = 15, cex = 1.5)
# points(DALYS_lost_no - DALYS_lost_joint_llAb_pVax, totalcost_joint_llAb_pVax - totalcost_no, col = UMByellow, pch = 19, cex = 1.5)
# points(DALYS_lost_no - DALYS_lost_joint_mVax_pVax, totalcost_joint_mVax_pVax - totalcost_no, col = UMBcharcoal, pch = 19, cex = 1.5)
# # points(DALYS_lost_no - DALYS_lost_intfhi_llAb_pVax, totalcost_intfhi_llAb_pVax - totalcost_no, col = UMByellow, pch =15, cex = 1.5)
# # points(DALYS_lost_no - DALYS_lost_intfhi_mVax_pVax, totalcost_intfhi_mVax_pVax - totalcost_no, col = UMBcharcoal, pch =15, cex = 1.5)
# # points(DALYS_lost_no - DALYS_lost_intflo_llAb_pVax, totalcost_intflo_llAb_pVax - totalcost_no, col = UMByellow, pch =17, cex = 1.5)
# # points(DALYS_lost_no - DALYS_lost_intflo_mVax_pVax, totalcost_intflo_mVax_pVax - totalcost_no, col = UMBcharcoal, pch =17, cex = 1.5)
# segments(0, 0, x1 = DALYS_lost_no - DALYS_lost_pVax, y1 = totalcost_pVax- totalcost_no,
#          col = par("fg"), lty = par("lty"), lwd = 2)
# segments(DALYS_lost_no - DALYS_lost_pVax, totalcost_pVax- totalcost_no, x1 = DALYS_lost_no -DALYS_lost_joint_llAb_pVax, y1 = totalcost_joint_llAb_pVax - totalcost_no,
#          col = par("fg"), lty = par("lty"), lwd = 2)
# segments(0, 0, x1 = DALYS_lost_no - DALYS_lost_pVax_intflo, y1 = totalcost_pVax_intflo- totalcost_no,
#          col = par("fg"), lty = 2, lwd = 2)
# segments(DALYS_lost_no - DALYS_lost_pVax_intflo, totalcost_pVax_intflo- totalcost_no, x1 = DALYS_lost_no - DALYS_lost_intflo_llAb_pVax, y1 = totalcost_intflo_llAb_pVax- totalcost_no,
#          col = par("fg"), lty = 2, lwd = 2)
# segments(0, 0, x1 = DALYS_lost_no - DALYS_lost_llAb, y1 = totalcost_llAb- totalcost_no,
#          col = par("fg"), lty = 3, lwd = 2)
# segments(DALYS_lost_no - DALYS_lost_llAb, totalcost_llAb- totalcost_no, x1 = DALYS_lost_no - DALYS_lost_intfhi_llAb_pVax, y1 = totalcost_intfhi_llAb_pVax- totalcost_no,
#          col = par("fg"), lty = 3, lwd = 2)
# legend("bottomright", legend = c("mAb", "mVax", "pVax 70% efficacy", "pVax 50% efficacy", "pVax 30% efficacy", "mAb + pVax 70% efficacy", "mAb + pVax 50% efficacy", "mAb + pVax 30% efficacy", "mVax + pVax 70% efficacy", "mVax + pVax 50% efficacy", "mVax + pVax 30% efficacy"),
#        bty = "n", pch = c(19,19,19,17,15,19,17,15,19,17,15), col = c(UMBblue, UMBforest, UMBplum, UMBplum, UMBplum, UMByellow, UMByellow, UMByellow, UMBcharcoal, UMBcharcoal, UMBcharcoal))
# quartz.save(file = "Figures/CE_plane.pdf", type = "pdf")


# ### Health Outcomes by year [cases, hospitalizations, deaths]
# ## create lists of deaths by year (1 through 3), for all trials
# outcome_by_year_func <- function(outcomes_input){
#   list(apply(outcomes_input[, 1:12], MARGIN = 1, sum), apply(outcomes_input[, 13:24], MARGIN = 1, sum), apply(outcomes_input[, 25:36], MARGIN = 1, sum))
# }
# 
# cases_year_no_u <- outcome_by_year_func(cases_no_u_age)
# cases_year_llAb_u <- outcome_by_year_func(cases_llAb_u_age)
# cases_year_mVax_u <- outcome_by_year_func(cases_mVax_u_age)
# cases_year_pVax_u <- outcome_by_year_func(cases_pVax_u_age)
# cases_year_joint_llAb_pVax_u <- outcome_by_year_func(cases_joint_llAb_pVax_u_age)
# cases_year_joint_mVax_pVax_u <- outcome_by_year_func(cases_joint_mVax_pVax_u_age)
# cases_year_pVax_older_u <- outcome_by_year_func(cases_pVax_older_u_age)
# cases_year_joint_llAb_pVax_older_u <- outcome_by_year_func(cases_joint_llAb_pVax_older_u_age)
# cases_year_joint_mVax_pVax_older_u <- outcome_by_year_func(cases_joint_mVax_pVax_older_u_age)
# 
# hosp_year_no_u <- outcome_by_year_func(inpat_no_u_age)
# hosp_year_llAb_u <- outcome_by_year_func(inpat_llAb_u_age)
# hosp_year_mVax_u <- outcome_by_year_func(inpat_mVax_u_age)
# hosp_year_pVax_u <- outcome_by_year_func(inpat_pVax_u_age)
# hosp_year_joint_llAb_pVax_u <- outcome_by_year_func(inpat_joint_llAb_pVax_u_age)
# hosp_year_joint_mVax_pVax_u <- outcome_by_year_func(inpat_joint_mVax_pVax_u_age)
# hosp_year_pVax_older_u <- outcome_by_year_func(inpat_pVax_older_u_age)
# hosp_year_joint_llAb_pVax_older_u <- outcome_by_year_func(inpat_joint_llAb_pVax_older_u_age)
# hosp_year_joint_mVax_pVax_older_u <- outcome_by_year_func(inpat_joint_mVax_pVax_older_u_age)
# 
# deaths_year_no_u <- outcome_by_year_func(deaths_no_u_age)
# deaths_year_llAb_u <- outcome_by_year_func(deaths_llAb_u_age)
# deaths_year_mVax_u <- outcome_by_year_func(deaths_mVax_u_age)
# deaths_year_pVax_u <- outcome_by_year_func(deaths_pVax_u_age)
# deaths_year_joint_llAb_pVax_u <- outcome_by_year_func(deaths_joint_llAb_pVax_u_age)
# deaths_year_joint_mVax_pVax_u <- outcome_by_year_func(deaths_joint_mVax_pVax_u_age)
# deaths_year_pVax_older_u <-outcome_by_year_func(deaths_pVax_older_u_age)
# deaths_year_joint_llAb_pVax_older_u <- outcome_by_year_func(deaths_joint_llAb_pVax_older_u_age)
# deaths_year_joint_mVax_pVax_older_u <- outcome_by_year_func(deaths_joint_mVax_pVax_older_u_age)

# NOTE: these are for the CE plane figure, needs to be updated
# deaths_pVax_intflo <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intflo)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intflo)))
# deaths_pVax_intfhi <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intfhi)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_pVax_intfhi)))
# deaths_intfhi_llAb_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_llAb_pVax )), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_llAb_pVax)))
# deaths_intfhi_mVax_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_mVax_pVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_intfhi_mVax_pVax)))
# deaths_intflo_llAb_pVax <-mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_llAb_pVax )), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_intflo_llAb_pVax)))
# deaths_intflo_mVax_pVax <- mort_inpat_func(CFR_inpatient, inpat_func(p_inpatient, pneum_func(p_pneum, cases_intflo_mVax_pVax)), CFR_nr_care, nr_care_func(p_inpatient, pneum_func(p_pneum, cases_intflo_mVax_pVax)))


# pd_llAb_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
# for (ll in 1:trials) {
#   pd_llAb_array[,,ll] <- pd_calc(efficacy[1], coverage[1], AR_y_u[,,ll], mat_eff_llAb)
# } 
# cases_llAb_u_age <- t(apply(pd_llAb_array, 3, RSVcases, babies = num_infants, mort = mort_mat))
# 
# pd_mVax_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
# for (m in 1:trials) {
#   pd_mVax_array[,,m] <- pd_calc(efficacy[2], coverage[2], AR_y_u[,,m], mat_eff_mVax)
# } 
# cases_mVax_u_age <- t(apply(pd_mVax_array, 3, RSVcases, babies = num_infants, mort = mort_mat))
# 
# pd_pVax_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
# for (p in 1:trials) {
#   pd_pVax_array[,,p] <- pd_calc(efficacy[3], coverage[3], AR_y_u[,,p], mat_eff_pVax)
# } 
# cases_pVax_u_age <- t(apply(pd_pVax_array, 3, RSVcases, babies = num_infants, mort = mort_mat))
# 
# # scenario analysis when efficacy reduction occurs only when pVax is in combo
# pd_pVax_array_SA <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
# for (p in 1:trials) {
#   pd_pVax_array_SA[,,p] <- pd_calc(efficacy[3], coverage[3], AR_y_u[,,p], mat_eff_pVax)
# } 
# cases_pVax_u_SA_age <- t(apply(pd_pVax_array, 3, RSVcases, babies = num_infants, mort = mort_mat))
# 
# pd_llAb_pVax_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
# for (lp in 1:trials) {
#   pd_llAb_pVax_array[,,lp] <- pd_joint(efficacy[1], efficacy[3], coverage[1], coverage[3], AR_y_u[,,lp], mat_eff_llAb, mat_eff_pVax)
# }
# cases_joint_llAb_pVax_u_age <- t(apply(pd_llAb_pVax_array, 3, RSVcases, babies = num_infants, mort = mort_mat))
# 
# pd_mVax_pVax_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
# for (mp in 1:trials) {
#   pd_mVax_pVax_array[,,mp] <- pd_joint(efficacy[2], efficacy[3], coverage[2], coverage[3], AR_y_u[,,mp], mat_eff_mVax, mat_eff_pVax)
# }
# cases_joint_mVax_pVax_u_age <- t(apply(pd_mVax_pVax_array, 3, RSVcases, babies = num_infants, mort = mort_mat))
# 
# pd_pVax_older_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
# for (p in 1:trials) {
#   pd_pVax_older_array[,,p] <- pd_calc(efficacy[3], cov_pVax_o, AR_y_u[,,p], mat_eff_older_pVax)
# } 
# cases_pVax_older_u_age <- t(apply(pd_pVax_older_array, 3, RSVcases, babies = num_infants, mort = mort_mat))
# 
# pd_llAb_pVax_older_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
# for (lp in 1:trials) {
#   pd_llAb_pVax_older_array[,,lp] <- pd_joint(efficacy[1], efficacy[3], coverage[1], cov_pVax_o, AR_y_u[,,lp], mat_eff_llAb, mat_eff_older_pVax)
# }
# cases_joint_llAb_pVax_older_u_age <- t(apply(pd_llAb_pVax_older_array, 3, RSVcases, babies = num_infants, mort = mort_mat))
# 
# pd_mVax_pVax_older_array <- array(NA, dim = c(dim(AR_y_bc)[1], dim(AR_y_bc)[2], trials))
# for (mp in 1:trials) {
#   pd_mVax_pVax_older_array[,,mp] <- pd_joint(efficacy[2], efficacy[3], coverage[2], cov_pVax_o, AR_y_u[,,mp], mat_eff_mVax, mat_eff_older_pVax)
# }
# cases_joint_mVax_pVax_older_u_age <- t(apply(pd_mVax_pVax_older_array, 3, RSVcases, babies = num_infants, mort = mort_mat))

# cases_llAb_u <- rowSums(cases_llAb_u_age)
# cases_mVax_u <- rowSums(cases_mVax_u_age)
# cases_pVax_u <- rowSums(cases_pVax_u_age)
# cases_pVax_u_SA <- rowSums(cases_pVax_u_SA_age)
# cases_joint_llAb_pVax_u <- rowSums(cases_joint_llAb_pVax_u_age)
# cases_joint_mVax_pVax_u <- rowSums(cases_joint_mVax_pVax_u_age)
# cases_pVax_older_u <- rowSums(cases_pVax_older_u_age)
# cases_joint_llAb_pVax_older_u <- rowSums(cases_joint_llAb_pVax_older_u_age)
# cases_joint_mVax_pVax_older_u <- rowSums(cases_joint_mVax_pVax_older_u_age)

# # 3 panel perspectives plot
# # NOTES: need to put legend below the plots instead of on last panel
# quartz("3_panel_perspectives_plot", 16, 8)
# par(mfrow =c(1,3))
# par(xaxs="i", yaxs="i")
# plot(WTP_sp, pO_pVax, ylim = c(0, 1), xlim = c(0,7500), bty = "l",
#      type = "l", lwd = 3, col = col_vec[4],
#      xlab = "Society willingness to pay (USD)",
#      ylab = "Probability optimal")
# lines(WTP_sp, pO_no, col = col_vec[1], lty = 1, lwd = 3)
# lines(WTP_sp, pO_llAb, col = col_vec[2], lty = 1, lwd = 3)
# lines(WTP_sp, pO_mVax, col = col_vec[3], lty = 1, lwd = 3)
# lines(WTP_sp, pO_llAb_pVax, col = col_vec[5], lty = 1, lwd = 3)
# lines(WTP_sp, pO_mVax_pVax, col = col_vec[6], lty = 1, lwd = 3)
# lines(WTP_sp, pO_pVax_older, col = col_vec[7], lty = 1, lwd = 3)
# lines(WTP_sp, pO_llAb_pVax_older, col = col_vec[8], lty = 1, lwd = 3)
# lines(WTP_sp, pO_mVax_pVax_older, col = col_vec[9], lty = 1, lwd = 3)
# abline(v = CET_Mali_GDP, col = UMBgray, lty = 3, lwd = 2)
# # abline(v = 3*CET_Mali_GDP, col = UMBgray, lty = 3, lwd = 2)
# text(CET_Mali_GDP, 0.92, labels = "1xGDP", srt = 45, cex = 0.80)
# # text(3*CET_Mali_GDP, 0.92, labels = "3xGDP", srt = 45, cex = 0.80)
# 
# plot(WTP_sp, dnr_pO_pVax, ylim = c(0, 1), xlim = c(0,5000), bty = "l",
#      type = "l", lwd = 3, col = col_vec[4],
#      xlab = "Donor willingness to pay (USD)",
#      ylab = "Probability optimal")
# lines(WTP_sp, dnr_pO_no, col = col_vec[1], lty = 1, lwd = 3)
# lines(WTP_sp, dnr_pO_llAb, col = col_vec[2], lty = 1, lwd = 3)
# lines(WTP_sp, dnr_pO_mVax, col = col_vec[3], lty = 1, lwd = 3)
# lines(WTP_sp, dnr_pO_llAb_pVax, col = col_vec[5], lty = 1, lwd = 3)
# lines(WTP_sp, dnr_pO_mVax_pVax, col = col_vec[6], lty = 1, lwd = 3)
# lines(WTP_sp, dnr_pO_pVax_older, col = col_vec[7], lty = 1, lwd = 3)
# lines(WTP_sp, dnr_pO_llAb_pVax_older, col = col_vec[8], lty = 1, lwd = 3)
# lines(WTP_sp, dnr_pO_mVax_pVax_older, col = col_vec[9], lty = 1, lwd = 3)
# abline(v = CET_Mali_GDP, col = UMBgray, lty = 3, lwd = 2)
# abline(v = 3*CET_Mali_GDP, col = UMBgray, lty = 3, lwd = 2)
# text(CET_Mali_GDP, 0.92, labels = "1xGDP", srt = 45, cex = 0.80)
# text(3*CET_Mali_GDP, 0.92, labels = "3xGDP", srt = 45, cex = 0.80)
# 
# plot(WTP_sp, gov_pO_pVax, ylim = c(0, 1), xlim = c(0,5000), bty = "l",
#      type = "l", lwd = 3, col = col_vec[4],
#      xlab = "Government willingness to pay (USD)",
#      ylab = "Probability optimal")
# lines(WTP_sp, gov_pO_no, col = col_vec[1], lty = 1, lwd = 3)
# lines(WTP_sp, gov_pO_llAb, col = col_vec[2], lty = 1, lwd = 3)
# lines(WTP_sp, gov_pO_mVax, col = col_vec[3], lty = 1, lwd = 3)
# lines(WTP_sp, gov_pO_llAb_pVax, col = col_vec[5], lty = 1, lwd = 3)
# lines(WTP_sp, gov_pO_mVax_pVax, col = col_vec[6], lty = 1, lwd = 3)
# lines(WTP_sp, gov_pO_pVax_older, col = col_vec[7], lty = 1, lwd = 3)
# lines(WTP_sp, gov_pO_llAb_pVax_older, col = col_vec[8], lty = 1, lwd = 3)
# lines(WTP_sp, gov_pO_mVax_pVax_older, col = col_vec[9], lty = 1, lwd = 3)
# abline(v = CET_Mali_GDP, col = UMBgray, lty = 3, lwd = 2)
# # abline(v = 3*CET_Mali_GDP, col = UMBgray, lty = 3, lwd = 2)
# text(CET_Mali_GDP, 0.92, labels = "1xGDP", srt = 45, cex = 0.80)
# # text(3*CET_Mali_GDP, 0.92, labels = "3xGDP", srt = 45, cex = 0.80)
# legend("right", inset = c(-0.3, 0), ncol = 1, legend = c("status quo","mAb", "mVax", "pVax 10 & 14 wks", "mAb + pVax 10 & 14 wks", "mVax + pVax 10 & 14 wks", "pVax 6 & 7 mos", "mAb + pVax 6 & 7 mos", "mVax + pVax 6 & 7 mos"),
#        lty = 1, lwd = 3, bty = "n", col = col_vec)
# quartz.save(file = "Figures/3_panel_perspectives_plot.pdf", type = "pdf")

# par(mfrow =c(1,1))
# par(mar = c(5.1, 4.1, 4.1, 2.1))
# par(xaxs="i", yaxs="i")
# image(x = c(pVax_cost[1] - 0.025, pVax_cost + 0.025),
#       y = c(llAb_cost[1] - 0.025, llAb_cost + 0.025),
#       z = t(SA_llpv),
#       col = c(col_vec, NA),
#       xlab = "Price of pediatric vaccine product per dose (USD)",
#       ylab = "Price of long-acting antibody product (USD)")

# SA_llpv_df$probwin2 <- replace(x = SA_llpv_df$probwin, SA_llpv_df$probwin<0.25, 0.25)

# par(mar = c(5.1, 4.1, 4.1, 2.1))
# par(xaxs="i", yaxs="i")
# image(x = c(eff_red[1]-0.005, eff_red + 0.005) * 100,
#       y = c(llAb_cost[1] - 0.025, llAb_cost + 0.025),
#       z = t(SA_ll_ce),
#       col = c(col_vec, NA),
#       xlab = "Efficacy of long-acting antibody",
#       ylab = "Price of long-acting antibody product (USD)")

# par(mar = c(5.1, 4.1, 4.1, 2.1))
# par(xaxs="i", yaxs="i")
# image(x = c(eff_red[1]-0.005, eff_red + 0.005) * 100,
#       y = c(pVax_cost[1] - 0.025, pVax_cost + 0.025),
#       z = t(SA_pe),
#       col = c(col_vec, NA),
#       xlab = "Efficacy of pediatric vaccine",
#       ylab = "Price of pediatric vaccine (USD)")

# cost_EPI <- 3.73 # fully loaded EPI visit intervention costs



