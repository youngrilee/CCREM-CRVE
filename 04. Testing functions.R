library(tidyverse)
library(lme4)
library(sandwich)
library(lmtest)
library(lmerTest)
library(lfe)
library(knitr)
library(kableExtra)
library(truncnorm)
library(purrr)
library(simhelpers) # citation("simhelpers")
library(psych)
library(future)
library(furrr)

rm(list = ls())
source("01. simulation functions.R")

# Data Generating Model----------------------------------------------------
set.seed(20201216)
dat <- generate_dat(
  # model parameters
  gamma000 = 0,        # overall average
  gamma100 = 0.03,     # coefficient for X (student)
  gamma010 = 0.3,      # coefficient for W (school)
  gamma002 = 0.3,      # coefficient for Z (neighborhood)
  G = 70,              # neighbor j = {1..g}
  H = 20,              # school k = {1..h}
  ICC = 0.15,          # ICC
  rho = .4,            # correlation between dimensions
  sparse = .1,         # sparcity 
  J = 30,              # average number of students per school
  L1cov_m = 0,
  L1cov_sd = 10,
  L2cov_m = 0,
  L2cov_sd = 1, 
  assumption = "exogeneity")  # homosced, exogeneity or met

dat %>% head()

# Model-fitting/Estimation-------------------------------------------------
results <- estimate(dat = dat, gamma100 = 0.01, gamma010 = 0.01, gamma002 = 0.01)

results

# Performance calculations ------------------------------------------------
calc_performance(results)

# Simulation driver -------------------------------------------------------
run_sim(iterations = 1, 
        gamma000 = 0, gamma100 = 0.03, gamma010 = 0.3, gamma002 = 0.3,  
        G = 70, H = 20, ICC = 0.15, rho = .4, sparse = .1, J = 30,
        L1cov_m = 0, L1cov_sd = 10,  L2cov_m = 0, L2cov_sd = 1, 
        assumption = "met")
