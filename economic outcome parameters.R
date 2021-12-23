# economic outcome parameters

# Medical costs in 2019 USD (based on data collected from Orenstein et al.)
cost_hosp <- 118.57
cost_outpatient <- 6.56

# sampling distribution for medical costs
# using SE of costs
cost_hosp_u <- rnorm(trials, cost_hosp, 15.90)
cost_outpatient_u <- rnorm(trials, cost_outpatient, 0.67)

# program costs
# cost_nd <- 1.35 # cost of adding new dose to existing EPI visit
cost_nd <- 0.63  # cost of adding new dose to existing EPI visit
cost_EPI <- 3.73 # fully loaded EPI visit intervention costs
cost_prod <- 1.0276 # product cost 
# cost_prod <- 0.721 # product cost

#####