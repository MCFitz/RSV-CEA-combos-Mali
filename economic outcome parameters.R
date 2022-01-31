# economic outcome parameters

# Medical costs in 2019 USD (based on data collected from Orenstein et al.)
cost_hosp <- 118.57
cost_outpatient <- 6.56

# sampling distribution for medical costs
# using SE of costs
cost_hosp_u <- rnorm(trials, cost_hosp, 15.90)
cost_outpatient_u <- rnorm(trials, cost_outpatient, 0.67)

# program costs
cost_nd_2016 <- 0.63  # cost of adding new dose to existing EPI visit, 2016 USD
cost_nd <- cost_nd_2016*(1+0.021)^3  # cost of adding new dose to existing EPI visit, 2019 USD = 0.67
cost_prod_2021 <- 1.0276 # product cost, penta, 2021 USD
cost_prod <- cost_prod_2021*(1-(mean(1.32,4.70)/100))^2 # product cost, penta, 2019 USD

# similar costs of vaccines in LMICs
MR_cost_2021 <- 0.721 # product cost, MR, 2021 USD
MR_cost <- MR_cost_2021*(1-(mean(1.32,4.70)/100))^2

#####