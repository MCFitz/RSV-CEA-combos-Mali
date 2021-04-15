# health outcome parameters

ILI_positivity <- rbeta(trials, 110, 379-110)
p_pneum_u <- rbeta(trials, 43, 110 + ILI_positivity*839)
p_pneum <- 43/(110+839*(110/379))

p_seek_care <- 0.47
p_inpatient_u <- rbeta(trials, 13, 30)
p_inpatient <- 13/43

CFR_inpatient <- 0.016  # PERCH PIA
CFR_inpatient_u <- rbeta(trials, 0.05*48, 0.552*259 - 0.05*48)

Li_CFR <- read_csv("cfr_lmic_ts_n5000.csv", 
                              col_types = cols(X1 = col_skip()))
CFR_age_mat <- matrix(Li_CFR$pred, ncol = 60, byrow = TRUE)
CFR_sub <- CFR_age_mat[ , 1:24] # subset to infants in first two years of life
  
CFR_nr_care <- CFR_inpatient/0.51 * 0.49  # 49% of infants in LMIC with RSV-LRTI die outside of inpatient care setting
CFR_nr_care_u <- CFR_inpatient_u/ 0.51 * 0.49

#####