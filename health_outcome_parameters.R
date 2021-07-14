# health outcome parameters

ILI_positivity <- rbeta(trials, 110, 379-110)
p_pneum_u <- rbeta(trials, 43, 110 + ILI_positivity*839)
p_pneum <- 43/(110+839*(110/379))

p_seek_care <- 0.47
p_inpatient_u <- rbeta(trials, 13, 30)
p_inpatient <- 13/43

p_inpatient_shi_li <- c(5, 5.2, 5.2, 6.8, 6.8, 6.8, 2.5, 2.5, 2.5, 1.8, 1.8, 1.8,
                        rep(1.3, 12), rep(0.4, 12))/100
p_inpatient_shi_lmic <- c(9.3, 22.9, 22.9, 20.5, 20.5, 20.5, 14, 14, 14,
                          9.4, 9.4, 9.4, rep(5.6, 12), rep(1.1, 12))/100
p_inpatient_shi_umic <- c(46.4, 74.5, 74.5, 25.7, 25.7, 25.7, 16.9, 16.9, 16.9,
                          18.8, 18.8, 18.8, rep(7.1, 12), rep(1.4, 12))/100
p_inpatient_shi_hi <- c(31.9, 67.4, 67.4, 36.7, 36.7, 36.7, 40.8, 40.8, 40.8,
                        25.9, 25.9, 25.9, rep(4.2, 12), rep(0.9, 12))/100
p_inpatient_shi_dev <- c(15.9, 26.1, 26.1, 20.7, 20.7, 20.7, 12.0, 12.0, 12.0,
                         11.3, 11.3, 11.3, rep(5.0, 12), rep(1.0, 12))/100
p_inpatient_shi_ind <- c(31.0, 54.6, 54.6, 27.5, 27.5, seq(27.5, 3.9, length.out = 8),
                         rep(3.9, 11), rep(0.8, 12))/100

CFR_inpatient <- 0.016  # PERCH PIA
CFR_inpatient_u <- rbeta(trials, 0.05*48, 0.552*259 - 0.05*48)

Li_CFR <- read_csv("cfr_lmic_ts_n5000.csv", 
                              col_types = cols(X1 = col_skip()))
CFR_age_mat <- matrix(Li_CFR$pred, ncol = 60, byrow = TRUE)

# subset to infants in first two years of life
# sample values based on number of trials with replacement
CFR_sub <- sample(CFR_age_mat[ , 1:24], trials, replace = TRUE, prob = NULL)  
CFR_nr_care <- CFR_inpatient/0.51 * 0.49  # 49% of infants in LMIC with RSV-LRTI die outside of inpatient care setting
CFR_nr_care_u <- CFR_inpatient_u/ 0.51 * 0.49

#####