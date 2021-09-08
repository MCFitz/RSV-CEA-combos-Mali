#########################################
## build AR matrix in R
# ft <- 17  # 6 months follow-up time
ft <- 47  # 3 years follow-up time
empty_year_cohort <-  matrix (0, 12, ncol = ft) # extrapolate age-based attack rates out to 36 months,
# for 6-24 months project linear decline starting with AR at 6mo. and ending with AR at 3mo.
# then for 24-36 months project linear decline starting with AR at 24 mo. (or 3mo.) and ending with AR at 1.5 mo.(average of months 1 and 2)
# based on Li et. al supplement Fig. 2A

# incidence rate infant RSV cases in Mali per 1000 person-years, divided by 12 for rate in a single month
mali_inc <- 536.8/12

# raw data for incidence by calendar month
cal_dat <- data.frame(month = seq(1,12, by =1), RSV_pos = c(7,6,4,2,2,1,6,36,78,5,5,5), num_samples = c(56,63,71,95,109,91,68,100,122,56,47,39))

# calculate the proportion of samples that were positive in each month
prop_pos <- cal_dat$RSV_pos / cal_dat$num_samples

# calculate monthly attack rate estimates spanning 1 calendar year
cal_AR <- prop_pos / mean(prop_pos) * mali_inc / 1000

# raw data for incidence by infant age in months
# follow-up time is reported in person-years
age_dat <- data.frame(age = seq(1,6, by =1), cases = c(8,11,28,27,47,32), follow_up = c(56.5,52.7,51.9,46.9,44.9,32.1))

# calculate age-based attack rate estimates, 0 to 6 months
age_AR <- age_dat$cases / (age_dat$follow_up*12)

### create AR matrices for birth cohorts followed for three years
age_AR_y <- c(age_AR, seq(age_AR[6], age_AR[3], length = 18), seq(age_AR[3], mean(age_AR[1:2]), length  = 12))
# AR_age_weights <- age_AR/ (sum(age_AR))
AR_age_weights <- age_AR_y/ (sum(age_AR_y))

# temp3 <- empty_year_cohort
# for (a in 1:12) {
#   temp3[a,] <- c(rep(0, times = a-1), age_AR, rep(0, times = 12-a))
# }

temp3 <- empty_year_cohort
for (a in 1:12) {
  temp3[a,] <- c(rep(0, times = a-1), age_AR_y, rep(0, times = 12-a))
}

# cal_AR_y <- c(cal_AR, cal_AR[1:5])
cal_AR_y <- c(cal_AR, cal_AR, cal_AR, cal_AR[1:11])
temp4 <- matrix(rep(cal_AR_y, each = 12), 12, ft)

AR_y_bc <- temp4/(mali_inc/1000)*temp3

# add uncertainty
AR_age_sample <- function (cases, fu, num_tri) {
  ARvec <- rbeta(num_tri, cases, fu*12 - cases)
}

AR_cases_year <- c(age_dat$cases, seq(age_dat$cases[6], age_dat$cases[3], length = 18), seq(age_dat$cases[3], mean(age_dat$cases[1:2]), length = 12))
AR_ft_year <- c(age_dat$follow_up, seq(age_dat$follow_up[6], age_dat$follow_up[3], length =18), seq(age_dat$follow_up[3], mean(age_dat$follow_up[1:2]), length = 12))
AR_age_y_u <- mapply(AR_age_sample, AR_cases_year, AR_ft_year, trials)

temp3_u <- array(0, dim = c(12, ft, trials))
for (a in 1:12) {
  temp3_u[a,a:(a+(ft-12)),] <- t(AR_age_y_u)
}

temp4_u <- array(rep(rep(cal_AR_y, each = 12), trials), dim = c(12, ft, trials))
AR_y_u <- temp4_u/(mali_inc/1000)*temp3_u

#####
