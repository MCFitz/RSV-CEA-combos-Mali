# economic functions

medcost_func <- function(c_hosp, num_inpat, c_out, num_outpat){
  (c_hosp * num_inpat) + (c_out * num_outpat)
}

# discounting function
# net present value (NPV) is a function of time in years (yrs), the discount rate (dr), and the original outcome (oo)
# discount YLL at 3% per year
NPV <- function(yrs, dr, oo){
  oo/(1+ dr)^(yrs-1)
}

yrs_vec <- c(1:58)
YLL_pc <- sum(NPV(yrs_vec, 0.03, 1))

# calculate YLL for each intervention 
# use life expectancy at birth for Mali (World Bank, 2017)
# subtract 7 months from first year of life to account for average age illness and subsequent death
YLL_func <- function(num_mort) {
  num_mort * (YLL_pc - 7/12)
}

##
# Duration of illness
di_bc <- 8.5   
di_yrs <- di_bc/365
di <- rnorm(trials, 8.5, (1.5/1.96)) # uncertainty, 7-10 days (Mathers et al.)
di_yrs_u <- di/365

# Life Expectancy at birth, males and females combined
le_mali <- 58 # life expectancy in Mali (World Bank, 2017)

# DALY disability weight for acute lower respiratory infections (LRTI)
dw_LRTI_severe <- 0.133 # severe LRTI (IHME GBD, 2017), for RSV-LRTI inpatient
dw_LRTI_severe_u <- rtriangle(trials, a = 0.088, b = 0.190 , c = 0.133)
dw_LRTI_mod <- 0.051 # moderate LRTI (IHME GBD, 2017), for RSV-LRTI outpatient
dw_LRTI_mod_u <- rtriangle(trials, a = 0.032, b = 0.074, c = 0.051)

# calculate YLD for each intervention
YLD_func <- function(num_inpat, num_mort, di, dw_LRTI_s, num_pneum, dw_LRTI_m) {
  ((num_inpat - num_mort) * di * dw_LRTI_s) +
    ((num_pneum - num_inpat) * di * dw_LRTI_m)
}

# Net Health Benefits function
NHB_func <- function (inputs, WTP) {
DALY_lost_no <- inputs[,1]
DALY_lost_int <- inputs[,2]
total_cost <- inputs[,3]
total_cost_no <- inputs[,4]
NHB <- (DALY_lost_no - DALY_lost_int) - (total_cost - totalcost_no) / WTP
NHB
}

# NHB function for 2-dimmensional inputs
NHB_func_er <- function(inputs, WTP) {
  DALY_lost_no <- inputs[, ,1]
  DALY_lost_int <- inputs[, ,2]
  total_cost <- inputs[, ,3]
  total_cost_no <- inputs[, ,4]
  NHB_er <- (DALY_lost_no - DALY_lost_int) - (total_cost - totalcost_no) / WTP
  NHB_er
}  

# WTP
CET_Mali_GDP <- 891
GDP3 <- 3* CET_Mali_GDP
WTP_5k <- 5000
WTP_sp <- c(0.01, seq(10, 20*CET_Mali_GDP, by = 5))

#####