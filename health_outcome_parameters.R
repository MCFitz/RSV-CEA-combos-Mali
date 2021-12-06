# health outcome parameters

ILI_positivity <- rbeta(trials, 110, 379-110)
p_pneum_u <- rbeta(trials, 43, 110 + ILI_positivity*839)
p_pneum <- 43/(110+839*(110/379))

p_wheeze <- 0.0646

# mortality rates by month, World Bank, Mali, 2019
# https://data.worldbank.org/indicator/SP.DYN.IMRT.IN?locations=ML
U1_mort <- 60.2/12/1000
U5_mort <- (94- 60.2)/48/1000

mort_vec <- c(rep(U1_mort, 12), rep(U5_mort, 24))

mort_mat <- empty_year_cohort
for (m in 1:12) {
  mort_mat[m,] <- c(rep(0, times = m-1), mort_vec, rep(0, times = 12-m))
}

p_seek_care <- 0.47
p_inpatient_u <- rbeta(trials, 13, 30) # from Buchwald for 0-6 months
p_inpatient <- 13/43 # from Buchwald for 0-6 months

p_inpatient_shi_li <- c(5, 5.2, 5.2, 6.8, 6.8, 6.8, 2.5, 2.5, 2.5, 1.8, 1.8, 1.8,
                        rep(1.3, 12), rep(0.4, 12))/1000
p_inpatient_shi_li_LC <- c(0.4, 1.4, 1.4, 1.7, 1.7, 1.7, 0.5, 0.5, 0.5, 0.9, 0.9, 0.9,
                        rep(0.4, 12), rep(0.1, 12))/1000
p_inpatient_shi_li_UC <- c(69.8, 19.5, 19.5, 28.1, 28.1, 28.1, 12.9, 12.9, 12.9, 3.7, 3.7, 3.7,
                        rep(5.0, 12), rep(1.2, 12))/1000

# incidence
i_inpatient_gambia <- c(rep(32.2, 6), rep(17.4, 6), rep(6.3, 12), rep(1.6, 12))/1000 

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
old_CFR <- sample(CFR_age_mat_3y, size = trials, replace = TRUE)
old_CFR_mat <- matrix(rep(old_CFR, times = 36), nrow = trials)

CFR_by_age_u <- CFR_age_mat_3y[sample(nrow(CFR_age_mat_3y), size = trials, replace = TRUE),]
# CFR_by_age_u <- old_CFR_mat

CFR_nr_care <- CFR_by_age/0.51 * 0.49  # 49% of infants in LMIC with RSV-LRTI die outside of inpatient care setting
CFR_nr_care_u <- CFR_by_age_u/ 0.51 * 0.49

#####