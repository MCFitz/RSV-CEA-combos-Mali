

# the proportion of hosps through 6 mo. from Buchwald : hosps through 6 mo. from low-income countries in Shi table
prop_h <- p_inpatient/ mean(rsv_lrti_hosps[1:6])

# create data frame with hospitalization rates (population is denominator) by age, including lower and upper 95% CIs
# from low-income countries Shi table
months <- c(1:36)
hosp_rates <- p_inpatient_shi_li
hosp_LB <- p_inpatient_shi_li_LC
hosp_UB <- p_inpatient_shi_li_UC

DT_hosps <- data.frame(months, hosp_rates, hosp_LB, hosp_UB)

ggplot(DT_hosps, aes(x = months, y = hosp_rates)) +
  geom_ribbon(aes(ymin = hosp_LB, ymax = hosp_UB), alpha = 0.2) +
  geom_smooth(method = "gam", formula = y ~ s(x), size = 1, se = FALSE) +
  geom_point()

###########
months_new <- c(1, 2.5, 4, 8, 11, mean(13:24), mean(25:36))
hosp_lo <- c(rsv_lrti_hosps[1], mean(rsv_lrti_hosps[2:3]), mean(rsv_lrti_hosps[4:6]),
             mean(rsv_lrti_hosps[7:9]), mean(rsv_lrti_hosps[10:12]), mean(rsv_lrti_hosps[13:24]),
             mean(rsv_lrti_hosps[25:36]))

df_hosp_lo <- data.frame(months_new, hosp_lo)
ggplot(df_hosp_lo, aes(x = months_new, y = hosp_lo)) +
  geom_smooth(method = "gam", formula = y ~ s(x), size = 1, se = FALSE) +
  # geom_smooth(method = lm, formula = y ~ log(x), size = 1, se = FALSE) +
  geom_point()

############

# Use a generalized additive model (GAM) fitted to the hospitalization rates by age from Shi low-income countries
# to estimate hospitalizations for Mali by age
  x <- months
  y <- rsv_lrti_hosps
  gam_y <- gam(y ~ s(x), method = "REML")
  x_new <- seq(0, max(x), length.out = length(y))
  y_pred <- predict(gam_y, data.frame(x = x_new)) * prop_h
  
  # par(mfrow = c(2,2))
  # gam.check(gam_y)
  # summary(gam_y)
  
p_hosp_new <- as.vector(y_pred)

df_newhosp <- data.frame(months, p_hosp_new)
ggplot(df_newhosp, aes(x = months, y = p_hosp_new)) +
  geom_point()

