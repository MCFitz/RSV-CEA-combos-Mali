# Code to produce plot for one-way sensitvity anlysis
# on the probability optimal across multiplier on CFR by age

# First specify range of multiplier on CFR values
CFR_mult <- seq(0,5, by = 0.05)

# write a function that can be applied to every strategy,
# which will return a matrix for each strategy with DALYs
# for every trial across the CFR multiplier
# dims = c(trials, length(CFR_mult))

# only DALYs will change, medcosts and totcosts will remain the same across
# changing values for the CFRs

cfr_mult_func <- function(admin, covrg, effcy){
  for (t in 1:trials){
  LRTI <- adj_func(LRTI_func(effcy, covrg, admin, p_pneum_u[t], cases_no_u_bic[,,t]))
  inpat <- inpat_func(p_hosp_u[t,], LRTI)
  outpat <- outpat_func(LRTI, inpat)
  nrcare <- nr_care_func(inpat)
  for(cfm in 1: length(CFR_mult)){
  death <- mort_inpat_func(CFR_by_age_u[t,] * CFR_mult[cfm], inpat, CFR_nr_care_u[t,], nrcare)
  YLL <- YLL_func(death)
  YLD <- YLD_func(inpat, death, di_yrs_u[t], dw_LRTI_severe_u[t], LRTI, dw_LRTI_mod_u[t])
  DALYS[t, cfm] <- sum(YLD + YLL)
}}}

# test it out
testing <- cfr_mult_func(llAb_admin, coverage[1], efficacy[1])




# write second function for strategies that have intervention combos


# then for every strategy I will have a matrix of equal size for DALYS
# I will then compute the NHBs and compate them 
# ask: for every trial, and value of CFR_mult,
# what is the probability that strategy X wins (fraction of times it wins * 100)