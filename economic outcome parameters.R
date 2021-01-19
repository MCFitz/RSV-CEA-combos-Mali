# economic outcome parameters

# Medical costs in 2019 USD (based on data collected from Orenstein et al.)
cost_hosp <- 118.57
cost_outpatient <- 6.56

# # sampling distribution for medical costs
# # using SE of costs
cost_hosp_u <- rnorm(trials, cost_hosp, 15.90)
cost_outpatient_u <- rnorm(trials, cost_outpatient, 0.67)

