# This script runs the Data cleaning, summarizing, visualization, and estimation scripts.

# Setting up working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Defining globals
input_dir = "input"
code_dir = "code"
output_dir = "output"

source("code/setup.R")
source("code/estimations.R")
source("code/visualization.R")
source("code/iterated_regs.R")
