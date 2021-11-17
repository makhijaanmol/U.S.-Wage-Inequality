# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating required residual variables for panel A of Figure 4
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CLGHSwage_logsupply_data <- CLGHSwage_logsupply_data %>%
  mutate(gapdt = resid(reg1)) %>% 
  mutate(supdt = resid(reg2)) %>% 
  mutate(wage_gap_till88 = predict(wagediff_until88, newdata = CLGHSwage_logsupply_data))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Recreating Figure 4A
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fig4A_extension <- ggplot(data = CLGHSwage_logsupply_data, aes(x = year)) +
  geom_line(aes(y = gapdt, color = "Detrended Wage Differential")) +
  geom_line(aes(y = supdt, color = "Detrended Relative Supply")) +
  geom_point(aes(y = gapdt), size = 1.5, alpha = 0.35) +
  geom_point(aes(y = supdt), size = 1.5, alpha = 0.35) +
  scale_x_continuous(name = "Year", limits = c(1963,2008), breaks = c(1963, 1972, 1981, 1990, 1999, 2008)) +
  scale_y_continuous(name = "Log Points") +
  scale_color_manual(name="",
                     values = c("Detrended Relative Supply"="red", "Detrended Wage Differential"="skyblue")) +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(size = 9.75)) +
  labs(title = "A. Detrended College-High School Wage Differential and Relative Supply, 1963-2008") +
  geom_hline(yintercept = 0) 

ggsave("Fig4A_extended.pdf", fig4A_extension, path = (str_interp("${output_dir}")), width = 7, height = 5)
while (!is.null(dev.list()))  dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Recreating Figure 4B
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fig4B_extension <- ggplot(data = CLGHSwage_logsupply_data, aes(x = year)) +
  geom_line(aes(y = clphsg_all, color = "Observed CLG/HS Gap")) +
  geom_line(aes(y = wage_gap_till88, color = "Katz-Murphy Predicted Wage Gap; 1963-1987 Trend")) +
  geom_point(aes(y = clphsg_all), size = 1.5, alpha = 0.35) +
  geom_point(aes(y = wage_gap_till88), size = 1.5, alpha = 0.35) +
  scale_x_continuous(name = "Year", limits = c(1963,2008), breaks = c(1963, 1972, 1981, 1990, 1999, 2008)) +
  scale_y_continuous(name = "Log Wage Gap", limits = c(0.35,0.75), breaks = c(0.35, 0.45, 00.55, 0.65, 0.75)) +
  scale_color_manual(name="",
                     values = c("Katz-Murphy Predicted Wage Gap; 1963-1987 Trend"="red", "Observed CLG/HS Gap"="skyblue")) +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(size = 9.75)) +
  labs(title = "B. Katz-Murphy Prediction Model for the College-High School Wage Gap") +
  geom_vline(xintercept = c(1987,1992))
fig4B_extension

ggsave("Fig4B_extended.pdf", fig4B_extension, path = (str_interp("${output_dir}")), width = 7, height = 5)
while (!is.null(dev.list()))  dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Saving Figure 4
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fig4_extension <- grid.arrange(fig4A_extension, fig4B_extension)

ggsave("Fig4_extended.pdf", fig4_extension, path = (str_interp("${output_dir}")), width = 10, height = 10)

while (!is.null(dev.list()))  dev.off()
