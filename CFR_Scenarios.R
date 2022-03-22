################################################################################
############################# CFR Scenarios ####################################

CFR_scenario_names <- c("Li age-gradient", "Li without age-gradient",
                        "PERCH PIA, Mali", "New CFR by age from LICs",
                        "Buchwald for 0-6 m, Mali", "CHAMPS w/ RSV in causal chain",
                        "CHAMPS RSV detected")

no_scenarios <- length(CFR_scenario_names)

# Scenarios 
CFR_S1 <- CFR_by_age

CFR_S2 <- rep(mean(CFR_by_age), length(months))

CFR_S3 <- rep(0, length(months))

CFR_S4 <- c(rep(2.6, 3), rep(2.2, 3), rep(1.8, 6), rep(1.6, 24))/100

CFR_S5 <- rep(1/13, length(months))

CFR_S6 <- rep(0, length(months))

CFR_S7 <- rep(0, length(months))

# Make data frame
CFR_scenario_df <- data.frame(Scenario = rep(CFR_scenario_names, each = length(months)),
                              Month = rep(months, no_scenarios),
                              CFR = c(CFR_S1, CFR_S2, CFR_S3, CFR_S4,
                                        CFR_S5, CFR_S6, CFR_S7))

# plot each CFR scenario over time
ggplot(CFR_scenario_df, aes(x = Month, y = CFR, group = Scenario)) +
  geom_line(aes(color = Scenario)) +
  geom_point(aes(color = Scenario))

