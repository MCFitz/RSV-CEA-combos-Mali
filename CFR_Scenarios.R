################################################################################
############################# CFR Scenarios ####################################


# REMOVE CHAMPS, BUCHWALD OUT FOR NOW

CFR_scenario_names <- c("Li 2020, age-gradient", "Li 2020, without age-gradient",
                        "PERCH PIA, Mali", "Li 2022, CFR by age from LICs",
                        "PERCH conventional, Mali")

# "Buchwald for 0-6 m, Mali", "CHAMPS w/ RSV in causal chain", "CHAMPS RSV detected"

no_scenarios <- length(CFR_scenario_names)

# Scenarios 
CFR_S1 <- CFR_by_age

CFR_S2 <- rep(mean(CFR_by_age), length(months))

CFR_S3 <- c(rep(0.001718095, 3), rep(0.01158412, 3), rep(0.001309821, 6),
            rep(0.01687563, 12), rep(0.1507355, 12))

CFR_S4 <- c(rep(2.6, 3), rep(2.2, 3), rep(1.8, 6), rep(1.6, 24))/100

# CFR_S5 <- rep(1/13, length(months))

# CFR_S6 <- rep(0, length(months))

# CFR_S7 <- rep(0, length(months))

CFR_S8 <- c(rep(1/64, 3), rep(5/52, 3), rep(2/(13+8), 6), rep(1/(11+5), 12), rep(1/7, 12))

CFR_scenarios <- cbind(CFR_S1, CFR_S2, CFR_S3, CFR_S4, CFR_S8)

###########################
# Uncertainty distributions
CFR_S1_u <- CFR_by_age_u

CFR_S2_u <- rep.col(rowMeans(CFR_by_age_u), length(months))

# age cats = 1-2M, 3-5M, 6-11M, 12-35M (no 0-1 so have to assume = 1-2)
# also these are 15000 iterations, so will need to sample
PERCH_PIA_raw <- read_csv("CFR_iterations.csv", 
                   col_types = cols(X1 = col_skip()))

PIA_1 <- sample(PERCH_PIA_raw$cfr.data.age1, size = trials, replace = TRUE)
PIA_2 <- sample(PERCH_PIA_raw$cfr.data.age2, size = trials, replace = TRUE)
PIA_3 <-sample(PERCH_PIA_raw$cfr.data.age3, size = trials, replace = TRUE)
PIA_4 <- sample(PERCH_PIA_raw$cfr.data.age4, size = trials, replace = TRUE)

CFR_S3_u <- cbind(rep.col(PIA_1, 3),
                  rep.col(PIA_2, 3),
                  rep.col(PIA_3, 6),
                  rep.col(PIA_4, 24))

# distribution to use for CFR estimates from Li??
CFR_S4_u <- cbind(rep.col(rgamma(trials, 2.6), 3),
                  rep.col(rgamma(trials, 2.2), 3),
                  rep.col(rgamma(trials, 1.8), 6),
                  rep.col(rgamma(trials, 1.6), 24))/100

# CFR_S5_u <- rep.col(rbeta(trials, 1, 12), 36)

CFR_S8_u <- cbind(rep.col(rbeta(trials, 1, 63), 3),
                  rep.col(rbeta(trials, 5, 47), 3),
                  rep.col(rbeta(trials, 2, 19), 6),
                  rep.col(rbeta(trials, 1, 15), 12),
                  rep.col(rbeta(trials, 1, 6), 12))

CFR_scenarios_u <- array(c(CFR_S1_u, CFR_S2_u, CFR_S3_u, CFR_S4_u, CFR_S8_u),
                         dim = c(trials, length(months), no_scenarios))

# Confidence Intervals
CI_S1 <- apply(CFR_by_age_u, 2, CI_func)

CI_S2 <- rep.col(rbind(mean(CI_S1[1,]), mean(CI_S1[2,])), 36)

CI_S3 <- rbind(c(rep(0, 36)),
               c(rep(0.014285714, 3), rep(0.08823529, 3),
                 rep(0, 6), rep(0.13333333, 12), rep(1, 12)))

CI_S4 <- rbind(c(rep(1.8, 3), rep(1.5, 3), rep(0.9, 6), rep(0.4, 24))/100,
               c(rep(3.6, 3), rep(3.3, 3), rep(3.4, 6), rep(5.7, 24))/100)

# CI_S5 <- apply(CFR_S5_u, 2, CI_func)
  
# CI_S6 <- apply(rep.col(rep(0, trials), length(months)), 2, CI_func)

# CI_S7 <- apply(rep.col(rep(0, trials), length(months)), 2, CI_func)

CI_S8 <- apply(CFR_S8_u, 2, CI_func)

# Make data frame
CFR_scenario_df <- data.frame(Scenario = rep(CFR_scenario_names, each = length(months)),
                              Month = rep(months, no_scenarios),
                              CFR = c(CFR_S1, CFR_S2, CFR_S3, CFR_S4, CFR_S8),
                              Lower = c(CI_S1[1,], CI_S2[1,], CI_S3[1,], CI_S4[1,],
                                        CI_S8[1,]),
                              Upper = c(CI_S1[2,], CI_S2[2,], CI_S3[2,], CI_S4[2,],
                                        CI_S8[2,]))

# plot each CFR scenario over time
quartz("CFR scenarios by age", 8, 6)
ggplot(CFR_scenario_df, aes(x = Month, y = CFR, group = Scenario)) +
  geom_line(aes(color = Scenario)) +
  geom_point(aes(color = Scenario)) +
  geom_point(aes(x= 1, y= CFR_S3[1]), shape = 8, colour="grey34") +
  geom_point(aes(x= 1, y= CFR_S8[1]), shape = 8, colour="grey34") +
  geom_ribbon(aes(ymin= Lower, ymax= Upper, fill = Scenario), alpha=0.25,
              color = "black", linetype = "dotted")
quartz.save(file = "Figures/CFR_scenarios_by_age_w_confidence_intervals.pdf", type = "pdf")


