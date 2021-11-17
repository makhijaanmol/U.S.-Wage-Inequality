
# To do this portion of the analysis I created separate trend break year variables for each year between 1964-2007. For each year if the year is greater than the
# assumed trend break year then a difference of the two years is observed if not a value of 0 is assigned. 

CLGHSwage_logsupply_data_yeartrend <- CLGHSwage_logsupply_data %>% 
  select(year, clphsg_all, eu_lnclg, t) %>% 
  mutate(t64 = case_when(year > 1964 ~ (year - 1964))) %>%
  mutate(t65 = case_when(year > 1965 ~ (year - 1965))) %>% 
  mutate(t66 = case_when(year > 1966 ~ (year - 1966))) %>% 
  mutate(t67 = case_when(year > 1967 ~ (year - 1967))) %>% 
  mutate(t68 = case_when(year > 1968 ~ (year - 1968))) %>% 
  mutate(t69 = case_when(year > 1969 ~ (year - 1969))) %>% 
  mutate(t70 = case_when(year > 1970 ~ (year - 1970))) %>% 
  mutate(t71 = case_when(year > 1971 ~ (year - 1971))) %>% 
  mutate(t72 = case_when(year > 1972 ~ (year - 1972))) %>% 
  mutate(t73 = case_when(year > 1973 ~ (year - 1973))) %>% 
  mutate(t74 = case_when(year > 1974 ~ (year - 1974))) %>% 
  mutate(t75 = case_when(year > 1975 ~ (year - 1975))) %>% 
  mutate(t76 = case_when(year > 1976 ~ (year - 1976))) %>% 
  mutate(t77 = case_when(year > 1977 ~ (year - 1977))) %>% 
  mutate(t78 = case_when(year > 1978 ~ (year - 1978))) %>% 
  mutate(t79 = case_when(year > 1979 ~ (year - 1979))) %>% 
  mutate(t80 = case_when(year > 1980 ~ (year - 1980))) %>% 
  mutate(t81 = case_when(year > 1981 ~ (year - 1981))) %>% 
  mutate(t82 = case_when(year > 1982 ~ (year - 1982))) %>% 
  mutate(t83 = case_when(year > 1983 ~ (year - 1983))) %>%
  mutate(t84 = case_when(year > 1984 ~ (year - 1984))) %>% 
  mutate(t85 = case_when(year > 1985 ~ (year - 1985))) %>% 
  mutate(t86 = case_when(year > 1986 ~ (year - 1986))) %>% 
  mutate(t87 = case_when(year > 1987 ~ (year - 1987))) %>% 
  mutate(t88 = case_when(year > 1988 ~ (year - 1988))) %>% 
  mutate(t89 = case_when(year > 1988 ~ (year - 1988))) %>% 
  mutate(t90 = case_when(year > 1990 ~ (year - 1990))) %>% 
  mutate(t91 = case_when(year > 1991 ~ (year - 1991))) %>% 
  mutate(t92 = case_when(year > 1992 ~ (year - 1992))) %>% 
  mutate(t93 = case_when(year > 1993 ~ (year - 1993))) %>% 
  mutate(t94 = case_when(year > 1994 ~ (year - 1994))) %>% 
  mutate(t95 = case_when(year > 1995 ~ (year - 1995))) %>% 
  mutate(t96 = case_when(year > 1996 ~ (year - 1996))) %>% 
  mutate(t97 = case_when(year > 1997 ~ (year - 1997))) %>% 
  mutate(t98 = case_when(year > 1998 ~ (year - 1998))) %>% 
  mutate(t99 = case_when(year > 1999 ~ (year - 1999))) %>% 
  mutate(t00 = case_when(year > 2000 ~ (year - 2000))) %>% 
  mutate(t01 = case_when(year > 2001 ~ (year - 2001))) %>% 
  mutate(t02 = case_when(year > 2002 ~ (year - 2002))) %>% 
  mutate(t03 = case_when(year > 2003 ~ (year - 2003))) %>% 
  mutate(t04 = case_when(year > 2004 ~ (year - 2004))) %>% 
  mutate(t05 = case_when(year > 2005 ~ (year - 2005))) %>% 
  mutate(t06 = case_when(year > 2006 ~ (year - 2006))) %>% 
  mutate(t07 = case_when(year > 2007 ~ (year - 2007))) %>% 
  mutate(t08 = case_when(year > 2008 ~ (year - 2008))) %>% 
  
  # For each year when the year value was higher than the assumed trend break year the difference was calculated now we can use the coalesce function to assign
  # a value of 0 elswehere
  mutate_all(coalesce, 0)
  
# Iterating regressions for each year (indicated by columns 5:49) and storing results in the my_lms object
my_lms <- lapply(5:49, function(x) lm(clphsg_all ~ t + eu_lnclg + CLGHSwage_logsupply_data_yeartrend[,x], data = CLGHSwage_logsupply_data_yeartrend))

summaries <- lapply(my_lms, summary)
r2 <- sapply(summaries, function(x) c(r_sq = x$r.squared, # Saving just the residuals in a list
                                adj_r_sq = x$adj.r.squared))
models_r2 <- as.data.frame(r2) # Converting list to dataframe to analyze later
years_6408 <- data.frame(1964:2008) # Making the dataframe more presentable in code lines 64-67
years_6408 <- transpose(years_6408)
models_r2 <- rbind(years_6408, models_r2)
rownames(models_r2)[rownames(models_r2) == "1"] = "year"

models_r2 <- models_r2[-c(2), ] 
models_r2 <- t(models_r2)
models_r2 <- as.data.frame(models_r2)

plot(models_r2$year, models_r2$adj_r_sq)

# 1994 is the trend break year with r^2 = 0.964344 and adj_r^2 = 0.9617971

wagediff_94trendbreak <- lm(clphsg_all ~ t + t94 + eu_lnclg, data = CLGHSwage_logsupply_data_yeartrend) # Calculating a final model with 1994 as trend break year
summary(wagediff_94trendbreak)
