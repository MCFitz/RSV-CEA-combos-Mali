################################################################################
############################# CFR Scenarios ####################################

CFR_scenario_names <- c("Li age-gradient", "Li without age-gradient",
                        "PERCH PIA, Mali", "New CFR by age from LICs",
                        "Buchwald for 0-6 m, Mali", "CHAMPS w/ RSV in causal chain",
                        "CHAMPS RSV detected", "PERCH conventional, Mali")

no_scenarios <- length(CFR_scenario_names)

# Scenarios 
CFR_S1 <- CFR_by_age

CFR_S2 <- rep(mean(CFR_by_age), length(months))

CFR_S3 <- rep(0, length(months))

CFR_S4 <- c(rep(2.6, 3), rep(2.2, 3), rep(1.8, 6), rep(1.6, 24))/100

CFR_S5 <- rep(1/13, length(months))

CFR_S6 <- rep(0, length(months))

CFR_S7 <- rep(0, length(months))

CFR_S8 <- c(rep(1/64, 3), rep(5/52, 3), rep(2/(13+8), 6), rep(1/(11+5), 12), rep(1/7, 12))

# Confidence Intervals
CI_S1 <- apply(CFR_by_age_u, 2, CI_func)

CI_S2 <- c(mean(CI_S1[1,]), mean(CI_S1[2,]))

CI_S3 <- rep(0, length(months))

CI_S4 <- rbind(c(rep(1.8, 3), rep(1.5, 3), rep(0.9, 6), rep(0.4, 24))/100,
               c(rep(3.6, 3), rep(3.3, 3), rep(3.4, 6), rep(5.7, 24))/100)

CI_S5 <- CI_func(rbeta(trials, 1, 12))
  
CI_S6 <- rep(0, length(months))

CI_S7 <- rep(0, length(months))

CI_S8 <- 

# Make data frame
CFR_scenario_df <- data.frame(Scenario = rep(CFR_scenario_names, each = length(months)),
                              Month = rep(months, no_scenarios),
                              CFR = c(CFR_S1, CFR_S2, CFR_S3, CFR_S4,
                                        CFR_S5, CFR_S6, CFR_S7, CFR_S8))

# plot each CFR scenario over time
ggplot(CFR_scenario_df, aes(x = Month, y = CFR, group = Scenario)) +
  geom_line(aes(color = Scenario)) +
  geom_point(aes(color = Scenario)) +
  geom_ribbon(aes(ymin= , ymax= ), alpha=0.1, fill = Scenario, # group = group
              color = "black", linetype = "dotted")

