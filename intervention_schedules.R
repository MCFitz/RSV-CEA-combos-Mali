# create matrices for intervention administration and efficacy for birth cohorts

#################
# number of people born in Mali 
pop_mali <- 18540000 # 2017 population in Mali (World Bank)
br_crude <- 42 # crude birth rate per 1000 people in Mali, 2017 (World Bank)
pop_bc <- pop_mali * (br_crude/1000) # total number of infants born in one year in Mali

# create a matrix for the number of infants in birth cohort 
days_per_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
days_per_year <- days_per_month/365
num_infants <- days_per_year* pop_bc
#################

mat_year_cohort <- empty_year_cohort
for (i in 1:ft) {
  for(h in 1:12){
    if( h-i<=0){
      mat_year_cohort[h,i]<-1
      if(i-h >=12){
        mat_year_cohort[h,i] <-0
      }
    }}}

mat_year_infants <- (matrix(num_infants, 12, ncol = ft)) * mat_year_cohort
#################

# long-acting mAb, birth dose
llAb_admin <- empty_year_cohort
for (i in 1:ft) {
  if(i >2 & i < 11)
    llAb_admin[i, i] <- 1
}

# long-acting mAb catch-up campaign
ssllAb_admin_pt2 <- empty_year_cohort
for (i in 1:ft){
  for (h in 1:12){
    if ((h<3 & i-h == 3) | h == 12 & i == 20 | h == 11 & i == 19) {
      ssllAb_admin_pt2[h,i] <- 1
    }}}

# long-acting mAb birth dose and catch-up campaign combined
ssllAb_admin <- llAb_admin + ssllAb_admin_pt2

# calculate number eligible to receive llAb for each intervention
num_eligible_llAb <- sum(llAb_admin * mat_year_infants)
num_eligible_ssllAb <- sum(ssllAb_admin * mat_year_infants)

# create matrix for when protection is provided for each llAb intervention
mat_eff_llAb <- empty_year_cohort
for (i in 1:ft) {
  if(i >2 & i <11)
    mat_eff_llAb[i, i:(i+4)] <- 1
}

mat_eff_ssllAb_pt2 <- empty_year_cohort
for (i in 1:ft) {
  for(h in 1:12){
    if (h<3 & i-h == 3) {
      mat_eff_ssllAb_pt2[h,i:(i+4)] <- 1}
    if (h == 12 & i == 20 | h == 11 & i == 19) {
      mat_eff_ssllAb_pt2[h,i:(i+3)] <- 1
    }}}

mat_eff_ssllAb <- mat_eff_llAb + mat_eff_ssllAb_pt2
#################

# mVax, year round admin to mothers
mVax_admin <- empty_year_cohort
for(i in 1:12) {
  mVax_admin[i, i] <-1
}

# create matrix for mVax duration of efficacy
# assume mVax is given to all preg. women and is effective only 1st 4 months of infant life
mat_eff_mVax <- empty_year_cohort
mVax_months <- 4 # mVax has 4 months duration
for (i in 1:12) {
  mat_eff_mVax[i, i:(i+(mVax_months-1))] <- 1
}

# pVax, pre-seasonal, two doses admin at weeks 10 & 14, protection delayed (2 wks)
pVax_admin <- empty_year_cohort
for (i in 1:12){
  pVax_admin[i,i+2] <-2
}

# create matrix for pVax duration of efficacy
mat_eff_pVax <- empty_year_cohort
for (i in 1:12){
  endpoint <- min((i+15), 35)
  mat_eff_pVax[i,(i+4):endpoint] <-1
}

# pVax, pre-seasonal, two doses admin at 8 & 9 months, protection delayed (2 wks)
pVax_older_admin <- empty_year_cohort
for (i in 1:12){
  pVax_older_admin[i,(i+7):(i+8)] <-1
}

# create matrix for pVax duration of efficacy
mat_eff_older_pVax <- empty_year_cohort
for (i in 1:12){
  endpoint_ol <- min((i+20), 35)
  mat_eff_older_pVax[i,(i+9):endpoint_ol] <-1
}

######