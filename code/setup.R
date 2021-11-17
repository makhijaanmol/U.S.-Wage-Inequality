# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Install Required Packages (Run only during the first time on your R setup)
install.packages("tidyverse")
install.packages("skimr")
install.packages("zoo")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("stargazer")
install.packages("moderndive")
install.packages("kableExtra")

# Load packages
library(tidyverse)
library(skimr)
library(zoo)
library(ggplot2)
library(gridExtra)
library(stargazer)
library(moderndive)
library(kableExtra)

# Read in raw data
raw_data <- read.csv(str_interp("${input_dir}/raw_data.csv"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating Time Variables
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CLGHSwage_logsupply_data <- raw_data %>% 
  mutate(t = year - 1962) %>% 
  mutate(t_sq = t^2/100) %>% 
  mutate(t_cube = t^3/1000) %>% 
  mutate(t92 = case_when(year > 1992 ~ (year - 1992), 
                         year <= 1992 ~ 0))