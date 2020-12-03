#########################################
## build AR matrix in R

ft <- 23   # 1 year follow-up time
empty_year_cohort <-  matrix (0, 12, ncol = ft)# extrapolate age-based attack rates out to 12 months, for 6-12 months project linear decline starting with AR at 6mo. and ending with AR at3mo.

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

### create AR matrices for birth cohorts followed for one-year (scenario analysis)
age_AR_y <- c(age_AR, seq(age_AR[6], age_AR[3], length = 6))

temp3 <- empty_year_cohort
for (a in 1:12) {
  temp3[a,] <- c(rep(0, times = a-1), age_AR_y, rep(0, times = 12-a))
}

cal_AR_y <- c(cal_AR, cal_AR[1:11])
temp4 <- matrix(rep(cal_AR_y, each = 12), 12, 23)

AR_y_bc <- temp4/(mali_inc/1000)*temp3


