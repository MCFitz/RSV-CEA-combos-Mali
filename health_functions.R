# Functions for calculating health outcomes

# functions for calculating the probability of disease and the number of cases,
# while removing the babies who die due to all-cause mortality and the babies
# who get sick from RSV in each cohort

pd_calc <- function (Ve, cov, AR, ad) {
  mat_out <- AR* (1-ad) + AR* ad* cov* (1-Ve) + AR* ad* (1-cov)
  mat_out
}

pd_joint <- function (Ve1, Ve2, cov1, cov2, AR, ad1, ad2, intf){
  pd1 <- AR* (1-ad1) + AR* ad1* cov1* (1-Ve1) + AR* ad1* (1-cov1)
  pd2 <- pd1* (1-ad2) + pd1* ad2* cov2* (1-Ve2) + pd1* ad2* (1-cov2)
  pd2
}

# this function returns a vector of cases by age in month 1:12
RSVcases <- function (pd, babies, mort) {
  lim <- dim(pd)[2]
  cases <- matrix(0, nrow = dim(pd)[1], ncol = lim)
  bb <- babies
  for (m in 1:lim) {
    cases[,m] <- pd[,m] * bb
    bb <- bb - cases[,m] - (bb * mort[,m])
  }
  cases
adj <- matrix(NA, nrow = 12, ncol = 36)
for(i in 1:12){
  adj[i,] <- cases[i ,i:(i+35)]
}
adj2 <- colSums(adj)
# list(cases, adj2)
}

# this function returns a matrix of cases by monthly birth cohort and month of age
bic_cases <- function (pd, babies, mort) {
  lim <- dim(pd)[2]
  cases <- matrix(0, nrow = dim(pd)[1], ncol = lim)
  bb <- babies
  for (m in 1:lim) {
    cases[,m] <- pd[,m] * bb
    bb <- bb - cases[,m] - (bb * mort[,m])
  }
  cases
}

# calculate number of infants who develop RSV-LRTI (pneumonia)
pneum_func <- function(prob_pneum, num_cases) {
  num_pneum <- prob_pneum * num_cases
  num_pneum
}

# number of infants who develop RSV-LRTI (pneumonia) under scenario where
# prevention products don't prevent disease, but do prevent RSV-LRTI
LRTI_func <-  function (Ve, cov, ad, prob_pneum, cases) {
  LRTI_out <- cases * (1-ad) * prob_pneum + cases * cov * ad * (1-Ve) * prob_pneum + cases * (1-cov) * ad * prob_pneum
  LRTI_out
}
# prob of LRTI when intervention is NOT acting +
# prob of LRTI when intervention IS acting and babies ARE covered +
# prob of LRTI when intervention IS acting and babies ARE NOT covered

LRTI_func_joint <- function (Ve1, Ve2, cov1, cov2, ad1, ad2, prob_pneum, cases){
  LRTI1 <- cases * (1-ad1) * prob_pneum + cases * cov1 * ad1 * (1-Ve1) * prob_pneum + cases * (1-cov1) * ad1 * prob_pneum
  LRTI2 <- LRTI1 * (1-ad2) + LRTI1 * cov2 * ad2 * (1-Ve2) + LRTI1 * (1-cov2) * ad2
  LRTI2
}

# calculate number of infants receiving inpatient care
inpat_func <- function(p_inpat, num_pneum){
  num_inpat <- p_inpat* num_pneum * p_seek_care
  num_inpat
}

# calculate number of infants receiving outpatient care
outpat_func <- function(num_pneum, num_hosp, num_nr_care){
  num_outpat <- num_pneum - num_hosp - num_nr_care
  num_outpat
}

# calculate number of infants not receiving appropriate level of care
# 53% of infants in LMIC with RSV-LRTI do not receive appropriate level of care
nr_care_func <- function(num_hosp){
  num_nr_care <- num_hosp * (1-p_seek_care)
  num_nr_care
}

# number of deaths
mort_inpat_func <- function(CFR_inpat, num_inpat, CFR_nr, num_nr_care){
  num_mort <- (CFR_inpat * num_inpat) + (CFR_nr * num_nr_care)
  num_mort
}

# number of children who develop wheeze/asthma
wheeze_func <-function(p_wz, num_pneum){
  num_wheeze <- num_pneum * (1- U5_mort) * p_wz
  num_wheeze
}

#####