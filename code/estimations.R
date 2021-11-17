# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Running individual regressions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

reg1 <- lm(clphsg_all ~ t, data = CLGHSwage_logsupply_data)
summary(reg1)

reg2 <- lm(eu_lnclg ~ t, data = CLGHSwage_logsupply_data)
summary(reg2)

wagediff_until88 <- lm(clphsg_all ~ t + eu_lnclg, data = subset(CLGHSwage_logsupply_data, year<=1987))
summary(wagediff_until88)

wagediff <- lm(clphsg_all ~ t + eu_lnclg, data = CLGHSwage_logsupply_data)
summary(wagediff)

wagediff_92trendbreak <- lm(clphsg_all ~ t + t92 + eu_lnclg, data = CLGHSwage_logsupply_data)
summary(wagediff_92trendbreak)

wagediff_quad_time_trend <-  lm(clphsg_all ~ t + t_sq + eu_lnclg, data = CLGHSwage_logsupply_data)
summary(wagediff_quad_time_trend)

wagediff_cube_time_trend <-  lm(clphsg_all ~ t + t_sq + t_cube + eu_lnclg, data = CLGHSwage_logsupply_data)
summary(wagediff_cube_time_trend)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating Table 2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

table_2 <- stargazer(wagediff_until88, wagediff, wagediff_92trendbreak, wagediff_quad_time_trend, wagediff_cube_time_trend, 
          type = "text", 
          dep.var.labels = c("College/High School Log Wage Gap, 1963-2008"), 
          covariate.labels = c("Time", "Time x post 1992", "Time Square by 100", "Time Cube by 1000", "CLG HS Relative Supply", "Constant"),
          report = ("vc*p*s"))

write.csv(table_2, str_interp("${output_dir}/Regression_Table 2.csv"), row.names = FALSE)
summary(table_2)
