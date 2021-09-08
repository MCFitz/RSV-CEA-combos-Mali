# health outcome parameters

ILI_positivity <- rbeta(trials, 110, 379-110)
p_pneum_u <- rbeta(trials, 43, 110 + ILI_positivity*839)
p_pneum <- 43/(110+839*(110/379))

p_wheeze <- 0.0646
U5_mort <- 101/1000
  
p_seek_care <- 0.47
p_inpatient_u <- rbeta(trials, 13, 30)
p_inpatient <- 13/43

p_inpatient_shi_li <- c(5, 5.2, 5.2, 6.8, 6.8, 6.8, 2.5, 2.5, 2.5, 1.8, 1.8, 1.8,
                        rep(1.3, 12), rep(0.4, 12))/1000

p_inpatient_shi_li_LC <- c(0.4, 1.4, 1.4, 1.7, 1.7, 1.7, 0.5, 0.5, 0.5, 0.9, 0.9, 0.9,
                        rep(0.4, 12), rep(0.1, 12))/1000
p_inpatient_shi_li_UC <- c(35.3, 29.5, 39.5, 35.4, 35.4, 35.4, 25.4, 25.4, 25.4, 21.7, 21.7, 21.7,
                        rep(9.7, 12), rep(1.9, 12))/1000

CFR_inpatient <- 0.016  # PERCH PIA
CFR_inpatient_u <- rbeta(trials, 0.05*48, 0.552*259 - 0.05*48)

Li_CFR <- read_csv("cfr_lmic_ts_n5000.csv", 
                              col_types = cols(X1 = col_skip()))
CFR_age_mat <- matrix(Li_CFR$pred, ncol = 60, byrow = TRUE)

# subset to infants in first three years of life
# sample values based on number of trials with replacement
# CFR_sub <- sample(CFR_age_mat[ , 1:36], trials, replace = TRUE, prob = NULL)

CFR_by_age <- apply(CFR_age_mat[ , 1:36], 2, mean)

CFR_age_mat_3y <- CFR_age_mat[ , 1:36] # subset to first three years
CFR_by_age_u <- CFR_age_mat_3y[sample(nrow(CFR_age_mat_3y), size = trials, replace = TRUE),]

CFR_nr_care <- CFR_by_age/0.51 * 0.49  # 49% of infants in LMIC with RSV-LRTI die outside of inpatient care setting
CFR_nr_care_u <- CFR_by_age_u/ 0.51 * 0.49

# CFR_nr_care_u <- CFR_sub/ 0.51 * 0.49
#####