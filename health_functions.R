# Functions for calculating health outcomes

# functions for calculating the probability of disease and the number of cases while removing the babies who get sick in each cohort
pd_calc <- function (Ve, cov, AR, ad) {
  mat_out <- AR* (1-ad) + AR* ad* cov* (1-Ve) + AR* ad* (1-cov)
}

RSVcases <- function (pd, babies) {
  lim <- dim(pd)[2]
  cases <- matrix(0, nrow = dim(pd)[1], ncol = lim)
  bb <- babies
  for (m in 1:lim) {
    cases[,m] <- pd[,m]*bb
    bb <- bb - cases[,m]
  }
  sum(cases)
}

# calculate number of infants who develop RSV-LRTI (pneumonia)
pneum_func <- function(prob_pneum, num_cases) {
  prob_pneum * num_cases
}

# calculate number of infants receiving inpatient care
inpat_func <- function(p_inpat, num_pneum){
  p_inpat* num_pneum * p_seek_care
}

# calculate number of intants receiving outpatient care
outpat_func <- function(p_inpat, num_pneum){
  ((1-p_inpat) * num_pneum)
}

# calculate number of infants not receiving appropriate level of care
# 53% of infants in LMIC with RSV-LRTI do not receive appropriate level of care
nr_care_func <- function(p_inpat, num_pneum){
  p_inpat* num_pneum * (1-p_seek_care)
}

# number of deaths
mort_inpat_func <- function(CFR_inpat, num_inpat, CFR_nr, num_nr_care){
  (CFR_inpat * num_inpat) + (CFR_nr * num_nr_care)
}

